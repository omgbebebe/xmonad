-- | What the window manager decided, as a value.
--
-- The manage and render sequences used to compute and transmit in one pass:
-- 'XMonad.River.WM.applyLayout' ran the layout and sent @propose_dimensions@
-- from inside the same @X@ action.  That works only while the thing computing
-- and the thing holding the connection are the same thread, and they are about
-- to stop being -- see @DESIGN.md@.  So the decision is separated from its
-- delivery: everything above computes a 'Plan', and one place below transmits
-- it.
--
-- The shape follows river's own.  Most of what a sequence sends is a /total
-- restatement/ -- river keeps no memory of rendering state, so every frame
-- states position, borders, stacking and visibility again from scratch -- and
-- a restatement is naturally a value that may be re-sent verbatim.  A few
-- requests are not restatements but one-shot effects, and re-sending one of
-- those is a bug rather than a no-op: @close@ twice kills a second window.
-- Hence 'Plan' and 'Op' rather than one list of things to do.
module XMonad.River.Plan
  ( Plan(..)
  , emptyPlan
  , FocusTarget(..)
  , Op(..)
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.River.Types (Dimension, KeyMask, KeySym, Position, Rectangle, Window)
import XMonad.River.Wire (ObjectId)

-- | Where a seat's keyboard should go.
data FocusTarget
  = FocusWindow !Window
  | ClearFocus
  deriving (Eq, Show)

-- | Everything restated on each sequence.
--
-- Safe to transmit again unchanged, which is what lets the loop answer a
-- @manage_start@ it has nothing new for: re-affirming the current plan is a
-- valid, cheap, consistent sequence.
data Plan = Plan
  { planSerial     :: !Int
    -- ^ Monotonic.  Lets whoever published a plan tell when it has landed.
  , planPlacements :: ![(Window, Rectangle)]
    -- ^ In bottom-to-top order, floats last, as the layout produced them.
    -- The render sequence @place_top@s this list in order, so later is higher.
  , planBorders    :: !(M.Map Window (Dimension, (Word32, Word32, Word32, Word32)))
    -- ^ Width and RGBA per placed window, already resolved against any
    -- per-window override.  Borders are rendering state, so river forgets them
    -- between frames and they have to be restated rather than set once.
  , planVisible    :: !(S.Set Window)
    -- ^ What the layout placed.  Anything river knows about and this does not
    -- contain belongs to a workspace that is off screen, and is hidden --
    -- which is how workspaces exist at all, river having no such concept.
  , planRaised     :: ![Window]
    -- ^ Standing "keep this above the layout" requests, re-applied every frame
    -- because the render sequence would otherwise have just undone them.
  , planFocus      :: !FocusTarget
    -- ^ Applied to every seat whose keyboard has not gone to a layer surface.
    -- One target rather than one per seat: xmonad has a single focus, and a
    -- second seat pointed somewhere else would have no way to be described.
  , planLayerDefault :: !(Maybe (ObjectId, Maybe ObjectId))
    -- ^ The output that should host layer surfaces which name none, and its
    -- @river_layer_shell_output_v1@.  Held as the choice rather than as a
    -- request because the request is only reissued when the choice changes,
    -- and what has been sent is the transmitting side's business.
  , planMustLand   :: !Bool
    -- ^ Whether this plan is only correct if it reaches the compositor in the
    -- sequence that provoked it.  Arming a submap is the case: it has to be
    -- atomic with the key press that opened it, or the config's own bindings
    -- are still live when the next key arrives.
  }

-- | The plan of a window manager that has decided nothing yet.
emptyPlan :: Plan
emptyPlan = Plan
  { planSerial     = 0
  , planPlacements = []
  , planBorders    = M.empty
  , planVisible    = S.empty
  , planRaised     = []
  , planFocus      = ClearFocus
  , planLayerDefault = Nothing
  , planMustLand   = False
  }

-- | Effects that happen once and are then forgotten.
--
-- Everything here is a request river treats as an event rather than as state,
-- so it must be delivered exactly once.  They are drained as they are
-- transmitted, where a 'Plan' is kept.
data Op
  = OpClose !Window
    -- ^ @river_window_v1.close@, from 'XMonad.Operations.kill'.
  | OpWarpPointer !ObjectId !Position !Position
    -- ^ @river_seat_v1.pointer_warp@ on a named seat.
  | OpPointerOpStart !ObjectId
    -- ^ @river_seat_v1.op_start_pointer@, which begins an interactive drag.
  | OpUseDecorations !Window !Bool
    -- ^ @use_ssd@ when 'True', @use_csd@ when 'False'.
  | OpSetPosition !Window !Position !Position
    -- ^ Where an interactive drag has put a window.  Rendering state, so legal
    -- in either sequence, but it arrives from user code and so takes the same
    -- route as the rest.
  | OpProposeDimensions !Window !Dimension !Dimension
    -- ^ Likewise from an interactive resize.
  | OpExitSession
    -- ^ End the Wayland session.
  | OpFullscreen !Window !ObjectId
    -- ^ @river_window_v1.fullscreen@ on the given output, followed by
    -- @inform_fullscreen@. Honors a client's fullscreen request; the
    -- compositor takes over the window's geometry while fullscreen.
  | OpExitFullscreen !Window
    -- ^ @river_window_v1.exit_fullscreen@ followed by
    -- @inform_not_fullscreen@, returning the window to layout control.
  | OpStop
    -- ^ @river_window_manager_v1.stop@: ask river to release this window
    -- manager so a successor may connect.
  | OpSetXcursorTheme !ObjectId !ByteString !Word32
    -- ^ The cursor theme and size, on one seat.
  | OpSetLayoutGroup !Int
    -- ^ @river_xkb_keyboard_v1.set_layout_by_index@ on every known xkb
    -- keyboard — the layout group half of the panel's NextLayout. Input
    -- config rather than window management state, so it drains with the
    -- now-ops, outside any sequence.
  | OpGrabKeys ![(KeyMask, KeySym)]
    -- ^ Bind these for as long as the config wants them, reporting presses and
    -- releases by index.  Unlike 'OpCaptureInput' this disables nothing and
    -- ends only when 'OpUngrabKeys' says so -- it is "XMonad.River.grabKeys",
    -- which is a standing capture rather than an interaction.
  | OpUngrabKeys
    -- ^ Release everything 'OpGrabKeys' bound.
  | OpCaptureInput ![(KeyMask, KeySym)] !KeyMask !Bool !Int
    -- ^ Take the keyboard: disable the config's bindings and listen for these
    -- keys instead, ending when the modifier mask goes up (zero to watch none)
    -- or, if the flag is set, after the first key.  Tagged with the generation
    -- identifying it.
    --
    -- The keys rather than the actions: what to run is the config's business,
    -- and this says only what to listen for.  Drained inside a manage
    -- sequence, which is what makes arming atomic with the key press that
    -- asked for it.
  deriving (Eq, Show)
