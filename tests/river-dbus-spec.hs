module Main (main) where

import XMonad
import XMonad.River.DBus

-- A window manager whose only distinguishing feature is the dbus
-- panel service.  tests/headless-dbus.sh runs this inside a headless
-- river and a private session bus, and asserts on the signals a panel
-- client would see.
main :: IO ()
main = xmonad def
  { workspaces = ["alpha", "beta", "gamma"]
  , startupHook = dbusService defaultDBusConfig
  }
