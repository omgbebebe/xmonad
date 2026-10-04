{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The dbus service a panel client talks to — the Wayland replacement
-- for EWMH root properties and client messages.
--
-- Under X11 a bar learned the workspace list from
-- @_NET_DESKTOP_NAMES@, the window list from
-- @_NET_CLIENT_LIST_STACKING@, and switched workspaces by sending
-- @_NET_CURRENT_DESKTOP@ client messages to the root.  river offers
-- none of that: the window manager is this process, and nothing else
-- can see its state.  So this module is that channel: it pushes the
-- window set as signals (after every manage sequence, plus a 1s
-- fallback that catches title and app_id changes arriving outside
-- sequences) and accepts commands as method calls, which are posted
-- back into the manage sequence through 'postAction'.
--
-- The consumer is homgb's Wayland backend
-- (design_docs/wayland.md there); the wire shapes below are the
-- contract.  Start it from the config's startupHook:
--
-- > startupHook = dbusService defaultDBusConfig
--
-- Everything here is best-effort: a missing session bus disables the
-- service with a warning, never a crash.
module XMonad.River.DBus
  ( DBusConfig(..)
  , defaultDBusConfig
  , dbusService
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, handle)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (gets)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (sortBy, (\\))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing, Down(..))
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import System.IO (hPutStrLn, stderr)

import qualified DBus
import DBus (toVariant)
import DBus.Client
  (Client, autoMethod, connectSession, emit, export, requestName
  , defaultInterface, interfaceMethods, interfaceName
  , nameAllowReplacement, nameReplaceExisting)

import XMonad.Core
import XMonad.Layout (ChangeLayout(..))
import XMonad.Operations (float, sendMessage, windows)
import XMonad.River (afterLayout, moveResizeWindow, postAction, restackWindows)
import XMonad.River.State (RiverState(..))
import XMonad.River.Types (RiverWindow(..), Rectangle(..))
import qualified XMonad.StackSet as W

-- | Knobs on the service.  @dcSetGroup@ is deliberately not defaulted
-- to anything clever: which backend applies an xkb layout group is a
-- session policy (riverctl keyboard-layout, a patched compositor, a
-- virtual keymap), and a silent wrong guess is worse than a log line.
data DBusConfig = DBusConfig
  { dcBusName :: String
    -- ^ well-known name to own; default @org.xmonad.WM@
  , dcLayouts :: [String]
    -- ^ layout rotation list reported in LayoutChanged, in group
    -- order; default empty (no layout reporting)
  , dcSetGroup :: Int -> IO ()
    -- ^ apply a layout group; default logs and does nothing
  }

defaultDBusConfig :: DBusConfig
defaultDBusConfig = DBusConfig
  { dcBusName = "org.xmonad.WM"
  , dcLayouts = []
  , dcSetGroup = \n -> hPutStrLn stderr
      ("xmonad-river: dbus: SetLayoutGroup requested but no backend \
       \configured (DBusConfig dcSetGroup); group was " ++ show n)
  }

data Svc = Svc
  { sXConf    :: XConf
  , sSignals  :: Chan Signal
  , sSnapshot :: IORef (Maybe Snapshot)
  , sSurfaces :: IORef (M.Map String (Rectangle, Int32))
    -- ^ panels' requested placements, by app_id; PERSISTENT — a
    -- placement may arrive before the compositor has reported the
    -- surface's window to us (pre-map), so it is retried on every
    -- manage sequence until a window with that app_id exists
  , sApplied  :: IORef (M.Map String Rectangle)
    -- ^ last rectangle actually floated, per app_id (change-suppress)
  , sNoMatchWarned :: IORef [String]
    -- ^ app_ids already logged as unmatched (warn once)
  , sStacked  :: IORef [Window]
    -- ^ last restack order applied (change-suppress; restackWindows
    -- forces a manage sequence, so an unconditional call would spin)
  , sGroup    :: IORef Int
  , sLayouts  :: [String]
  , sLastFocus :: IORef (Maybe Window)
    -- ^ last focused non-panel window; a clicked panel must not
    -- keep the keyboard
  }

-- | One full picture of what a bar renders.  Diffs between snapshots
-- become signals; a signal carries the WHOLE new value (consumers
-- diff, exactly like an EWMH property re-read).
data Snapshot = Snapshot
  { snapWorkspaces :: [(String, Bool, Bool)]
    -- ^ (name, is current, has windows) in workspace order — an
    -- association list, not a Map, because a panel renders the order
    -- and "10" must not sort between "1" and "2"
  , snapWindows :: [(String, String, String, String, Bool)]
    -- ^ (identifier, title, app_id, workspace, focused); identifier is
    -- river's stable window identifier, never the recycled object id
  , snapFocus :: (String, String)
    -- ^ (title, app_id) of the focused window, ("", "") when none
  , snapLayout :: (Int, [String])
  } deriving (Eq, Show)

data Signal
  = SigWorkspaces [(String, Bool, Bool)]
  | SigWindows [(String, String, String, String, Bool)]
  | SigFocus (String, String)
  | SigLayout (Int, [String])

-- | Own the bus name, export the interface, and stream signals.
--
-- Runs from the startup hook: forks the dbus threads (a client
-- connection of their own, none of it on the event loop) and
-- registers the snapshot emitter, which re-queues itself through
-- 'afterLayout' after every manage sequence.
dbusService :: DBusConfig -> X ()
dbusService cfg = do
  conf <- ask
  sigs <- liftIO newChan
  snapRef <- liftIO (newIORef Nothing)
  surfRef <- liftIO (newIORef M.empty)
  appliedRef <- liftIO (newIORef M.empty)
  stackedRef <- liftIO (newIORef [])
  warnedRef <- liftIO (newIORef [])
  groupRef <- liftIO (newIORef 0)
  focusRef <- liftIO (newIORef Nothing)
  let s = Svc conf sigs snapRef surfRef appliedRef stackedRef warnedRef
            groupRef (dcLayouts cfg) focusRef
  liftIO $ do
    void $ forkIO (dbusThread cfg s)
    -- 1s fallback emission: title and app_id changes arrive outside
    -- manage sequences, and the afterLayout hook only fires inside
    -- one.  Cheap: the snapshot diff suppresses no-change sends.
    void $ forkIO $ forever $ do
      threadDelay 1000000
      postAction conf (emitSignals s)
  emitter s

-- | The recurring emitter: apply any pending surface placements (a
-- panel's window may only now have been reported by the compositor),
-- diff-and-send, re-arm for after the next layout.
emitter :: Svc -> X ()
emitter s = do
  applySurfaces s
  guardPanelFocus s
  emitSignals s
  afterLayout (emitter s)

-- | Panels never hold focus: clicking one (a workspace button, a
-- tray icon) must not leave the user typing into a bar. Runs every
-- manage sequence; remembers the last non-panel focus and restores
-- it when a panel ends up focused.
guardPanelFocus :: Svc -> X ()
guardPanelFocus s = do
  ws <- gets windowset
  surfaces <- liftIO (readIORef (sSurfaces s))
  known <- liftIO . readIORef =<< asks (riverWindows . riverState)
  let isPanel w = case M.lookup w known of
        Just rw -> any (matches rw) (M.keys surfaces)
        Nothing -> False
      matches rw k = rwAppId rw == Just (BC.pack k)
        || rwTitle rw == Just (BC.pack k)
  case W.peek ws of
    Just w | isPanel w -> do
      lastGood <- liftIO (readIORef (sLastFocus s))
      case lastGood of
        Just lw | Just _ <- W.findTag lw ws -> windows (W.focusWindow lw)
        _ -> return ()
    foc -> liftIO (writeIORef (sLastFocus s) foc)

forever :: IO () -> IO ()
forever act = act >> forever act

emitSignals :: Svc -> X ()
emitSignals s = do
  snap <- takeSnapshot s
  old <- liftIO (readIORef (sSnapshot s))
  liftIO $ writeIORef (sSnapshot s) (Just snap)
  mapM_ (liftIO . writeChan (sSignals s)) (diffSignals old snap)

-- | Build the current picture from the windowset and the accumulated
-- compositor state.
takeSnapshot :: Svc -> X Snapshot
takeSnapshot s = do
  ws <- gets windowset
  known <- liftIO . readIORef =<< asks (riverWindows . riverState)
  let rwList = [ rw | rw <- M.elems known, not (rwClosed rw) ]
      ident rw = fromMaybe (show (rwObject rw)) (BC.unpack <$> rwIdentifier rw)
      title rw = maybe "" BC.unpack (rwTitle rw)
      appId rw = maybe "" BC.unpack (rwAppId rw)
      workspaceOf w = fromMaybe "" (W.findTag w ws)
      focused = W.peek ws
      wins = [ (ident rw, title rw, appId rw, workspaceOf (rwObject rw)
               , Just (rwObject rw) == focused)
             | rw <- rwList ]
      cur = W.currentTag ws
      wsspaces =
        [ (W.tag wk, W.tag wk == cur
          , not . null . W.integrate' $ W.stack wk)
        | wk <- W.workspaces ws ]
      foc = case [ rw | rw <- rwList, Just (rwObject rw) == focused ] of
        (rw:_) -> (title rw, appId rw)
        []     -> ("", "")
  group <- liftIO (readIORef (sGroup s))
  pure Snapshot
    { snapWorkspaces = wsspaces
    , snapWindows = wins
    , snapFocus = foc
    , snapLayout = (group, sLayouts s)
    }

diffSignals :: Maybe Snapshot -> Snapshot -> [Signal]
diffSignals old new =
  [ SigWorkspaces (snapWorkspaces new)
    | fmap snapWorkspaces old /= Just (snapWorkspaces new) ]
    ++
  [ SigWindows (snapWindows new)
    | fmap snapWindows old /= Just (snapWindows new) ]
    ++
  [ SigFocus (snapFocus new)
    | fmap snapFocus old /= Just (snapFocus new) ]
    ++
  [ SigLayout (snapLayout new)
    | fmap snapLayout old /= Just (snapLayout new) ]

dbusThread :: DBusConfig -> Svc -> IO ()
dbusThread cfg s = handle warn $ do
  client <- connectSession
  let name = fromMaybe (error "xmonad-river: dbus: invalid dcBusName")
        (DBus.parseBusName (dcBusName cfg))
  _ <- requestName client name [nameAllowReplacement, nameReplaceExisting]
  export client "/org/xmonad/WM" defaultInterface
    { interfaceName = "org.xmonad.WM"
    , interfaceMethods =
        [ autoMethod "SwitchWorkspace" (switchWorkspace s)
        , autoMethod "FocusWindow" (focusWindowById s)
        , autoMethod "NextLayout" (nextLayout cfg s)
        , autoMethod "SetLayoutGroup" (setLayoutGroup cfg s)
        , autoMethod "PlaceSurface"
            (\appId x y w h o -> placeSurface s appId x y w h o)
        ]
    }
  forever $ emitSignal client =<< readChan (sSignals s)
  where
    warn (e :: SomeException) = hPutStrLn stderr
      ("xmonad-river: dbus service failed: " ++ show e)

emitSignal :: Client -> Signal -> IO ()
emitSignal client sig = emit client $ case sig of
  SigWorkspaces m -> base "WorkspacesChanged"
    `withBody` [toVariant m]
  SigWindows ws -> base "WindowsChanged"
    `withBody` [toVariant ws]
  SigFocus f -> base "FocusChanged"
    `withBody` [toVariant f]
  SigLayout l -> base "LayoutChanged"
    `withBody` [toVariant (fromIntegral (fst l) :: Int32, snd l)]
  where
    base member = DBus.signal "/org/xmonad/WM" "org.xmonad.WM"
      (DBus.memberName_ member)
    withBody s body = s { DBus.signalBody = body }

switchWorkspace :: Svc -> String -> IO ()
switchWorkspace s name =
  postAction (sXConf s) (windows (W.greedyView name))

focusWindowById :: Svc -> String -> IO ()
focusWindowById s ident =
  postAction (sXConf s) $ do
    mw <- findWindowByIdent s ident
    forM_ mw (windows . W.focusWindow)

-- | Rotate to the next keyboard layout group. This is NOT xmonad's
-- NextLayout (that rotates the layout algorithm): panel layouts are
-- xkb groups, applied by the config's dcSetGroup backend, and the
-- new group is tracked here so the indicator follows it.
nextLayout :: DBusConfig -> Svc -> IO ()
nextLayout cfg s = do
  g <- readIORef (sGroup s)
  let n = length (sLayouts s)
  when (n > 0) $ setGroup cfg s ((g + 1) `mod` n)

setLayoutGroup :: DBusConfig -> Svc -> Int32 -> IO ()
setLayoutGroup cfg s n = setGroup cfg s (fromIntegral n)

setGroup :: DBusConfig -> Svc -> Int -> IO ()
setGroup cfg s g = do
  writeIORef (sGroup s) g
  dcSetGroup cfg g
  writeChan (sSignals s) (SigLayout (g, sLayouts s))

-- | Place (or move) a client's surface: float the window at the
-- rectangle and keep every placed surface stacked by its stackOrder,
-- highest on top — the standing equivalent of a raise, re-applied by
-- the render sequence every frame.
placeSurface :: Svc -> String -> Int32 -> Int32 -> Int32 -> Int32 -> Int32
             -> IO ()
placeSurface s appId x y w h stackOrder = do
  hPutStrLn stderr ("xmonad-river: dbus: PlaceSurface " ++ appId)
  modifyIORef' (sSurfaces s)
    (M.insert appId (Rectangle x y (fromIntegral w) (fromIntegral h)
                    , stackOrder))
  postAction (sXConf s) (applySurfaces s)

-- | Apply persistent placements: float every known surface window at
-- its requested rectangle and keep them stacked by stackOrder. Runs
-- on every manage sequence (via the emitter) AND right after a
-- PlaceSurface call, because either side may come first — a panel
-- asks before its window exists, or its window appears before the
-- panel's first placement. Everything is change-suppressed: float
-- and restack only fire when something actually moved, so an idle
-- loop costs two compares, not window management.
applySurfaces :: Svc -> X ()
applySurfaces s = do
  surfaces <- liftIO (readIORef (sSurfaces s))
  known <- liftIO . readIORef =<< asks (riverWindows . riverState)
  floated <- gets (W.floating . windowset)
  applied <- liftIO (readIORef (sApplied s))
  let byAppId = M.fromListWith (\a _ -> a)
        [ (a, rw) | rw <- M.elems known, Just a <- [rwAppId rw] ]
      -- panels identify themselves by app_id, but not every toolkit
      -- lets a client set one per window (SDL only honors the title
      -- at creation), so fall back to the window title — a panel's
      -- title is its identity and does not change
      byTitle = M.fromListWith (\a _ -> a)
        [ (t, rw) | rw <- M.elems known, Just t <- [rwTitle rw] ]
      lookupSurface appId = case M.lookup (BC.pack appId) byAppId of
        Just rw -> Just rw
        Nothing -> M.lookup (BC.pack appId) byTitle
      placed =
        [ (appId, rw, rect, o)
        | (appId, (rect, o)) <- M.toList surfaces
        , Just rw <- [lookupSurface appId]
        ]
  case (M.keys surfaces, placed) of
    ([], _) -> return ()
    (want, []) -> do
      -- warn once per app_id (a panel that exited would otherwise
      -- log this every sequence forever)
      warned <- liftIO (readIORef (sNoMatchWarned s))
      let fresh = want \\ warned
      unless (null fresh) $ do
        liftIO $ hPutStrLn stderr
          ("xmonad-river: dbus: no window yet for " ++ show fresh)
        liftIO (writeIORef (sNoMatchWarned s) (warned ++ fresh))
    _ -> return ()
  forM_ placed $ \(appId, rw, rect, _) -> do
    let w = rwObject rw
        needsFloat = M.notMember w floated
        moved = M.lookup appId applied /= Just rect
    when (needsFloat || moved) $ do
      moveResizeWindow w rect
      float w
      liftIO $ hPutStrLn stderr
        ("xmonad-river: dbus: " ++ (if needsFloat then "floated " else "moved ")
          ++ appId ++ " to " ++ show (rect_x rect, rect_y rect)
          ++ " " ++ show (rect_width rect, rect_height rect))
  liftIO $ modifyIORef' (sApplied s) $ \m ->
    foldl (\acc (appId, _, rect, _) -> M.insert appId rect acc) m placed
  let ordered = map (\(_, rw, _, _) -> rwObject rw)
        (sortBy (comparing (Down . (\(_, _, _, o) -> o))) placed)
  stacked <- liftIO (readIORef (sStacked s))
  unless (null ordered || ordered == stacked) $ do
    liftIO (writeIORef (sStacked s) ordered)
    restackWindows ordered

findWindowByIdent :: Svc -> String -> X (Maybe Window)
findWindowByIdent s ident = do
  known <- liftIO . readIORef =<< asks (riverWindows . riverState)
  pure $ case [ rwObject rw
              | rw <- M.elems known
              , Just i <- [rwIdentifier rw]
              , BC.unpack i == ident ] of
    (w:_) -> Just w
    []    -> Nothing
