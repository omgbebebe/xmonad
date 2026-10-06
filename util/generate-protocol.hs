#!/usr/bin/env stack
-- stack script --snapshot lts-24.53 --package xml-conduit --package text --package containers --package directory --package filepath --package process

-- | Generates Haskell bindings from Wayland protocol XML.
--
-- The source XML is not checked in. It is downloaded on demand into
-- @protocol/@, which is gitignored, from the pinned upstream revisions in
-- 'waylandCommit' and 'riverCommit'.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Char (isAlpha, toLower, toUpper)
import Control.Monad (unless)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>))
import System.Process (callProcess)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.XML as X

-- | The wayland release @wayland.xml@ is taken from.
waylandCommit :: String
waylandCommit = "1.24.0"

-- | The river commit the river protocols are taken from. A version tag would be
-- preferred, but these are not yet tagged.
riverCommit :: String
riverCommit = "bfab9ea75985e048ca31b919ccd6dfc676da6dd5"

-- | Where the wayland XML is downloaded to.
protocolDir :: FilePath
protocolDir = "protocol"

-- | Each protocol file: its name here, and where to get it.
sources :: [(FilePath, String)]
sources =
  [ ( "wayland.xml"
    , wayland "protocol/wayland.xml" )
  , ( "river-window-management-v1.xml"
    , river "protocol/river-window-management-v1.xml" )
  , ( "river-xkb-bindings-v1.xml"
    , river "protocol/river-xkb-bindings-v1.xml" )
  , ( "river-xkb-config-v1.xml"
    , river "protocol/river-xkb-config-v1.xml" )
  , ( "river-layer-shell-v1.xml"
    , river "protocol/river-layer-shell-v1.xml" )
  , ( "wlr-layer-shell-unstable-v1.xml"
    , river "protocol/upstream/wlr-layer-shell-unstable-v1.xml" )
  , ( "virtual-keyboard-unstable-v1.xml"
    , river "protocol/upstream/virtual-keyboard-unstable-v1.xml" )
  ]
  where
    wayland p =
      "https://gitlab.freedesktop.org/wayland/wayland/-/raw/" ++ waylandCommit ++ "/" ++ p
    river p =
      "https://codeberg.org/river/river/raw/commit/" ++ riverCommit ++ "/" ++ p

-- | Which protocol files to generate, what to call the resulting modules, and
-- which interfaces to take from each.
--
-- 'Nothing' means every interface in the file.  @wayland.xml@ needs a
-- selection: it defines the whole core protocol, most of which a window
-- manager never binds, and two of its interfaces -- @wl_keyboard.keymap@ and
-- @wl_data_source.send@ -- carry descriptors in *events*, which the generator
-- does not support.  Taking only what is needed keeps that limitation from
-- mattering.
--
-- @wl_display@ and @wl_registry@ are deliberately absent: "XMonad.River.Connection"
-- implements them by hand, because id allocation, error reporting and the
-- registry dance are the connection rather than ordinary protocol traffic.
targets :: [(FilePath, String, Maybe [String])]
targets =
  [ ("river-window-management-v1.xml", "XMonad.River.Protocol.WindowManagement", Nothing)
  , ("river-xkb-bindings-v1.xml",      "XMonad.River.Protocol.XkbBindings", Nothing)
  , ("river-xkb-config-v1.xml",        "XMonad.River.Protocol.XkbConfig", Nothing)
  , ("river-layer-shell-v1.xml",       "XMonad.River.Protocol.LayerShell", Nothing)
  , ("wayland.xml",                    "XMonad.River.Protocol.Core",
      Just [ "wl_compositor", "wl_shm", "wl_shm_pool", "wl_surface"
              , "wl_buffer", "wl_region", "wl_callback"
              -- For prompts, which run as an ordinary Wayland client on a second
              -- connection so that they get real keyboard input.  See
              -- XMonad.River.Client.
              , "wl_seat", "wl_keyboard", "wl_output" ])
  , ("wlr-layer-shell-unstable-v1.xml", "XMonad.River.Protocol.LayerShellClient", Nothing)
  -- Only the tests use this: a headless seat has no keyboard, so a spec that
  -- wants to press a key has to give the seat one.  Generated here rather than
  -- hand-written for the same reason as everything else in Protocol/.
  , ("virtual-keyboard-unstable-v1.xml", "XMonad.River.Protocol.VirtualKeyboard", Nothing)
  ]

--------------------------------------------------------------------------------
-- Protocol model

data Interface = Interface
  { ifaceName     :: String
  , ifaceVersion  :: Int
  , ifaceRequests :: [Message]
  , ifaceEvents   :: [Message]
  , ifaceEnums    :: [Enum']
  }

data Message = Message
  { msgName       :: String
  , msgOpcode     :: Int
  , msgArgs       :: [Argument]
  , msgDestructor :: Bool
  , msgSince      :: Maybe Int
  , msgSummary    :: String
  }

data Argument = Argument
  { argName      :: String
  , argType      :: ArgType
  , argNullable  :: Bool
  , argInterface :: Maybe String
  }

data ArgType = TInt | TUInt | TFixed | TString | TObject | TNewId | TArray | TFd
  deriving (Eq)

data Enum' = Enum'
  { enumName    :: String
  , enumEntries :: [(String, Integer)]
  }

--------------------------------------------------------------------------------
-- Parsing

parseProtocol :: FilePath -> IO [Interface]
parseProtocol path = do
  doc <- X.readFile X.def path
  pure $ map parseInterface (childrenNamed "interface" (X.documentRoot doc))

parseInterface :: X.Element -> Interface
parseInterface el = Interface
  { ifaceName     = attr "name" el
  , ifaceVersion  = read (attr "version" el)
  , ifaceRequests = zipWith parseMessage [0..] (childrenNamed "request" el)
  , ifaceEvents   = zipWith parseMessage [0..] (childrenNamed "event" el)
  , ifaceEnums    = map parseEnum (childrenNamed "enum" el)
  }

parseMessage :: Int -> X.Element -> Message
parseMessage opcode el = Message
  { msgName       = attr "name" el
  , msgOpcode     = opcode
  , msgArgs       = map parseArg (childrenNamed "arg" el)
  , msgDestructor = attrMaybe "type" el == Just "destructor"
  , msgSince      = read <$> attrMaybe "since" el
  , msgSummary    = maybe "" (attr "summary")
                      (listToMaybe' (childrenNamed "description" el))
  }

parseArg :: X.Element -> Argument
parseArg el = Argument
  { argName      = attr "name" el
  , argType      = case attr "type" el of
      "int"     -> TInt
      "uint"    -> TUInt
      "fixed"   -> TFixed
      "string"  -> TString
      "object"  -> TObject
      "new_id"  -> TNewId
      "array"   -> TArray
      "fd"      -> TFd
      t         -> error ("unknown arg type: " ++ t)
  , argNullable  = attrMaybe "allow-null" el == Just "true"
  , argInterface = attrMaybe "interface" el
  }

parseEnum :: X.Element -> Enum'
parseEnum el = Enum'
  { enumName    = attr "name" el
  , enumEntries =
      [ (attr "name" e, readValue (attr "value" e))
      | e <- childrenNamed "entry" el
      ]
  }
  where
    readValue s
      | "0x" `isPrefixOf` s = read s
      | otherwise           = read s

--------------------------------------------------------------------------------
-- XML helpers

childrenNamed :: T.Text -> X.Element -> [X.Element]
childrenNamed n el =
  [ e | X.NodeElement e <- X.elementNodes el, X.nameLocalName (X.elementName e) == n ]

attrMaybe :: T.Text -> X.Element -> Maybe String
attrMaybe n el = T.unpack <$> M.lookup (X.Name n Nothing Nothing) (X.elementAttributes el)

attr :: T.Text -> X.Element -> String
attr n el = fromMaybe (error ("missing attribute " ++ T.unpack n)) (attrMaybe n el)

listToMaybe' :: [a] -> Maybe a
listToMaybe' = \case { (x:_) -> Just x; [] -> Nothing }

--------------------------------------------------------------------------------
-- Naming

-- | @river_window_v1@ becomes @RiverWindowV1@.
typeName :: String -> String
typeName = concatMap capitalise . splitOn '_'
  where capitalise = \case { (c:cs) -> toUpper c : cs; [] -> [] }

-- | @river_window_v1@ becomes @riverWindowV1@.
funName :: String -> String
funName s = case typeName s of
  (c:cs) -> toLower c : cs
  []     -> []

-- | Avoids colliding with Haskell keywords and Prelude names.
safeVar :: String -> String
safeVar n
  | n `elem` reserved = n ++ "_"
  | otherwise         = lowerFirst (concatMap capitalise (zip [0 :: Int ..] (splitOn '_' n)))
  where
    reserved = ["type", "class", "data", "where", "id", "min", "max", "then", "else"]
    capitalise (0, w) = w
    capitalise (_, w) = case w of { (c:cs) -> toUpper c : cs; [] -> [] }
    lowerFirst = \case { (c:cs) -> toLower c : cs; [] -> [] }

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (w, [])      -> [w]
  (w, _:rest)  -> w : splitOn c rest

--------------------------------------------------------------------------------
-- Type and codec mapping

haskellType :: Argument -> String
haskellType a = case argType a of
  TInt    -> "Int32"
  TUInt   -> "Word32"
  TFixed  -> "Fixed"
  TString -> if argNullable a then "(Maybe ByteString)" else "ByteString"
  TObject -> "ObjectId"
  TNewId  -> "ObjectId"
  TArray  -> "ByteString"
  TFd     -> "Fd"

-- | How a request argument is encoded.
argEncoder :: Argument -> String
argEncoder a = case argType a of
  TInt    -> "argInt " ++ v
  TUInt   -> "argUInt " ++ v
  TFixed  -> "argFixed " ++ v
  TString -> if argNullable a then "argString " ++ v else "argString (Just " ++ v ++ ")"
  TObject -> "argObject " ++ v
  TNewId  -> "argObject " ++ v
  TArray  -> "argArray " ++ v
  -- An fd argument contributes nothing to the message body: it travels
  -- entirely as ancillary data, and the server pairs it with the request by
  -- position.  So it encodes as the empty Args and is passed separately.
  TFd     -> "mempty"
  where v = safeVar (argName a)

argDecoder :: Argument -> String
argDecoder a = case argType a of
  TInt    -> "getInt"
  TUInt   -> "getWord32"
  TFixed  -> "getFixed"
  TString -> if argNullable a then "getStringMaybe" else "getString"
  TObject -> "getObject"
  TNewId  -> "getObject"
  TArray  -> "getArray"
  -- A descriptor occupies no bytes in the body: it is claimed from the
  -- connection before decoding and spliced in with pure, which keeps the
  -- applicative in argument order without consuming input.  See renderListener.
  TFd     -> "pure " ++ fdVar a

fdVar :: Argument -> String
fdVar a = "fd_" ++ safeVar (argName a)

--------------------------------------------------------------------------------
-- Rendering

renderModule :: String -> String -> [Interface] -> String
renderModule modName source ifaces = unlines $
  [ "-- | Bindings for @" ++ source ++ "@."
  , "--"
  , "-- Generated by @util/generate-protocol.hs@. Do not edit by hand."
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module " ++ modName
  , "  ("
  ] ++
  prefixFirst "    " "  , " (concatMap ifaceExports ifaces) ++
  [ "  ) where"
  , ""
  ] ++ imports ++
  [ ""
  , "import XMonad.River.Connection"
  , "import XMonad.River.Wire"
  , ""
  ] ++
  concatMap renderInterface ifaces
  where
    allArgs = [ a | i <- ifaces, m <- ifaceRequests i ++ ifaceEvents i, a <- msgArgs m ]
    uses t = any ((== t) . argType) allArgs
    imports = concat
      [ [ "import Data.ByteString (ByteString)" ]
      , [ "import Data.Int (Int32)" | uses TInt ]
      , [ "import Data.Word (Word16, Word32)" ]
      , [ "import System.Posix.Types (Fd)" | uses TFd ]
      ]

-- | Adds a prefix to the first element and another to the rest.
prefixFirst :: String -> String -> [String] -> [String]
prefixFirst _ _ [] = []
prefixFirst p1 pn (x:xs) = (p1 ++ x) : map (pn ++) xs

ifaceExports :: Interface -> [String]
ifaceExports i =
  [ typeName (ifaceName i) ++ "Event(..)"
  , funName (ifaceName i) ++ "Interface"
  , funName (ifaceName i) ++ "Version"
  , funName (ifaceName i) ++ "Listen"
  ] ++
  [ funName (ifaceName i) ++ typeName (msgName m)
  | m <- ifaceRequests i
  ] ++
  [ enumConstName i e n | e <- ifaceEnums i, (n, _) <- enumEntries e ]

enumConstName :: Interface -> Enum' -> String -> String
enumConstName i e n =
  funName (ifaceName i) ++ typeName (enumName e) ++ typeName n

renderInterface :: Interface -> [String]
renderInterface i =
  [ sectionRule
  , "-- " ++ ifaceName i ++ " (version " ++ show (ifaceVersion i) ++ ")"
  , sectionRule
  , ""
  , "-- | The interface name, as advertised by @wl_registry@."
  , funName (ifaceName i) ++ "Interface :: ByteString"
  , funName (ifaceName i) ++ "Interface = \"" ++ ifaceName i ++ "\""
  , ""
  , "-- | The highest version these bindings were generated against."
  , funName (ifaceName i) ++ "Version :: Word32"
  , funName (ifaceName i) ++ "Version = " ++ show (ifaceVersion i)
  , ""
  ] ++
  concatMap (renderEnum i) (ifaceEnums i) ++
  concatMap (renderRequest i) (ifaceRequests i) ++
  renderEventType i ++
  renderListener i

sectionRule :: String
sectionRule = "--------------------------------------------------------------------------------"

renderEnum :: Interface -> Enum' -> [String]
renderEnum i e = concat
  [ [ "-- | @" ++ ifaceName i ++ "." ++ enumName e ++ "." ++ n ++ "@"
    , nm ++ " :: Word32"
    , nm ++ " = " ++ show v
    , ""
    ]
  | (n, v) <- enumEntries e
  , let nm = enumConstName i e n
  ]

renderRequest :: Interface -> Message -> [String]
renderRequest i m =
      [ "-- | @" ++ ifaceName i ++ "." ++ msgName m ++ "@"
      ] ++
      sinceComment ++
      [ name ++ " :: Connection -> ObjectId" ++ concatMap ((" -> " ++) . haskellType) plainArgs
          ++ " -> IO " ++ resultType
      , name ++ " conn self" ++ concatMap ((' ' :) . safeVar . argName) plainArgs ++ " ="
      ] ++
      body ++
      [ "" ]
  where
    name = funName (ifaceName i) ++ typeName (msgName m)
    -- A new_id argument in a request is allocated by us and returned, rather
    -- than being taken as a parameter.
    newIdArgs = [ a | a <- msgArgs m, argType a == TNewId ]
    plainArgs = [ a | a <- msgArgs m, argType a /= TNewId ]
    resultType = case newIdArgs of
      []  -> "()"
      _   -> "ObjectId"
    sinceComment = case msgSince m of
      Nothing -> []
      Just s  -> [ "-- Since version " ++ show s ++ "." ]
    encoded = case msgArgs m of
      [] -> "mempty"
      as -> intercalate " <> " (map argEncoder as)
    fdArgs = [ a | a <- msgArgs m, argType a == TFd ]
    send = case fdArgs of
      [] -> "request conn self " ++ show (msgOpcode m) ++ " (" ++ encoded ++ ")"
      as -> "requestWithFds conn self " ++ show (msgOpcode m) ++ " (" ++ encoded
              ++ ") [" ++ intercalate ", " (map (safeVar . argName) as) ++ "]"
    body = case newIdArgs of
      [] ->
        [ "  " ++ send ]
        ++ [ "    >> freeObject conn self" | msgDestructor m ]
      (nid:_) ->
        [ "  do"
        , "    " ++ safeVar (argName nid) ++ " <- newObject conn"
        , "    " ++ send
        , "    pure " ++ safeVar (argName nid)
        ]

renderEventType :: Interface -> [String]
renderEventType i =
  [ "-- | Events delivered to a @" ++ ifaceName i ++ "@."
  , "data " ++ tn ++ "Event"
  ] ++
  zipWith renderCon [0 :: Int ..] (ifaceEvents i) ++
  [ sep (length (ifaceEvents i)) ++ tn ++ "Unknown !Word16 !ByteString"
  , "    -- ^ An event this build does not know about, from a server speaking"
  , "    -- a newer version of the protocol. Ignoring these is what keeps a"
  , "    -- client forward compatible."
  , "  deriving (Eq, Show)"
  , ""
  ]
  where
    tn = typeName (ifaceName i)
    sep 0 = "  = "
    sep _ = "  | "
    renderCon n m =
      sep n ++ tn ++ typeName (msgName m)
        ++ concatMap ((" !" ++) . haskellType) (msgArgs m)

renderListener :: Interface -> [String]
renderListener i =
  [ "-- | Attach an event handler to a @" ++ ifaceName i ++ "@ object."
  , name ++ " :: Connection -> ObjectId -> (" ++ tn ++ "Event -> IO ()) -> IO ()"
  , name ++ " conn self handler ="
  , "  setListener conn self $ \\opcode body -> case opcode of"
  ] ++
  concatMap renderCase (ifaceEvents i) ++
  [ "    _ -> handler (" ++ tn ++ "Unknown opcode body)"
  , ""
  ]
  where
    tn = typeName (ifaceName i)
    name = funName (ifaceName i) ++ "Listen"
    renderCase m = case msgArgs m of
      [] ->
        [ "    " ++ show (msgOpcode m) ++ " -> handler " ++ tn ++ typeName (msgName m) ]
      as | null (fdClaims as) ->
        [ "    " ++ show (msgOpcode m) ++ " ->"
        , "      handler =<< decode (" ++ applicative as ++ ") body"
        ]
      as ->
        [ "    " ++ show (msgOpcode m) ++ " -> do" ]
        ++ fdClaims as ++
        [ "      handler =<< decode (" ++ applicative as ++ ") body" ]
      where
        applicative as' =
          tn ++ typeName (msgName m) ++ " <$> "
            ++ intercalate " <*> " (map argDecoder as')
        -- Descriptors arriving with an event are claimed in the order their
        -- arguments appear, which is the order the server sent them.
        fdClaims as' =
          [ "      " ++ fdVar a ++ " <- takeFdOrFail conn " ++ show (argName a)
          | a <- as', argType a == TFd ]

--------------------------------------------------------------------------------
-- Protocol sources

fetchSources :: IO ()
fetchSources = do
  createDirectoryIfMissing True protocolDir
  mapM_ fetch sources
  where
    fetch (name, url) = do
      let path = protocolDir </> name
      have <- doesFileExist path
      unless have $ do
        putStrLn ("fetching " ++ name)
        callProcess "curl"
          [ "-fsSL", "--proto", "=https", "--tlsv1.2"
          , "--retry", "3", "--retry-all-errors", "-o", path, url ]

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = fetchSources >> mapM_ generate targets
  where
    generate (xmlFile, modName, only) = do
      allIfaces <- parseProtocol (protocolDir </> xmlFile)
      let ifaces = case only of
            Nothing    -> allIfaces
            Just names -> [ i | i <- allIfaces, ifaceName i `elem` names ]
          missing = case only of
            Nothing    -> []
            Just names -> [ n | n <- names, n `notElem` map ifaceName allIfaces ]
      unless (null missing) $
        error (xmlFile ++ ": no such interface(s): " ++ intercalate ", " missing)
      let out = "src" </> map (\c -> if c == '.' then '/' else c) modName <.> "hs"
      writeFile out (renderModule modName xmlFile ifaces)
      putStrLn ("wrote " ++ out ++ " (" ++ show (length ifaces) ++ " interfaces)")
