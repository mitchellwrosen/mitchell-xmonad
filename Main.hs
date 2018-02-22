{-# language PartialTypeSignatures #-}
{-# language RecordWildCards       #-}

import Data.Bits ((.|.))
import Data.Map (Map)
import Data.Monoid (All, Endo)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO (Handle, hPutStrLn)
import Graphics.X11.Types
  (enterWindowMask, propertyChangeMask, structureNotifyMask)
import XMonad
  (Button, ButtonMask, Event, EventMask, Full(Full), IncMasterN(IncMasterN),
    KeyMask, KeySym, Layout, Query, Resize(Expand, Shrink), Window, WindowSet,
    WindowSpace, WorkspaceId, X, XConfig(XConfig), def, io, launch, mod4Mask,
    sendMessage, withFocused, xC_top_left_arrow)
import XMonad.Actions.CycleWS
  (Direction1D(Next, Prev), WSType(AnyWS), moveTo, shiftTo)
import XMonad.Hooks.DynamicLog
  (PP(..), dynamicLogWithPP, dzenEscape, shorten, wrap, xmobarColor,
    xmobarStrip)
import XMonad.Hooks.InsertPosition
  (Focus(Newer), Position(Below), insertPosition)
import XMonad.Hooks.ManageDocks
  (avoidStruts, docksEventHook, docksStartupHook, manageDocks)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.FixedColumn (FixedColumn(FixedColumn))
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Operations (kill, restart, windows)
import XMonad.StackSet
  (focusDown, focusUp, sink, swapDown, swapMaster, swapUp)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified XMonad as X

-- Main entrypoint: spawn xmobar, then launch xmonad.
main :: IO ()
main = do
  xmobar <- spawnPipe "xmobar /home/mitchell/.xmobarrc"

  launch XConfig
    { -- How many pixels wide should the border of the currently-selected
      -- window be?
      X.borderWidth = 4

      -- When manually clicking on a window to focus it, should that click also
      -- be delivered to the window? 'True' means no, just focus it.
    , X.clickJustFocuses = True

    , X.clientMask = clientMask

      -- Should hovering over a window with the mouse select it?
    , X.focusFollowsMouse = False

      -- Focused window border color.
    , X.focusedBorderColor = "#444444"

      -- Rebind Mod to the Windows key.
    , X.modMask = mod4Mask

      -- Unfocused window border color.
    , X.normalBorderColor = "#000000"

      -- The list of workspace names.
    , X.workspaces = ["1", "2", "3", "4", "5", "6"]

    , X.handleEventHook = handleEventHook
    , X.handleExtraArgs = handleExtraArgs
    , X.keys = keys
    , X.layoutHook = layoutHook
    , X.logHook = logHook xmobar
    , X.manageHook = manageHook
    , X.mouseBindings = mouseBindings
    , X.rootMask = rootMask
    , X.startupHook = startupHook
    , X.terminal = terminal
    }

-- Which client events is XMonad interested in? (I use the default value).
clientMask :: EventMask
clientMask =
  enterWindowMask .|. propertyChangeMask .|. structureNotifyMask

-- The layouts.
layoutHook :: _
layoutHook =
  avoidStruts
    (toggleLayouts
      -- Fullscreen mode, without wasting any pixels drawing a border.
      (noBorders Full)
      -- Normal mode (80-column-wide window), with a border around the
      -- currently-focused window if there's more than one window).
      (smartBorders (FixedColumn 1 1 80 1)))

-- Our preferred terminal application.
terminal :: String
terminal =
  "urxvt"

keys :: XConfig l -> Map (KeyMask, KeySym) (X ())
keys cfg = mkKeymap cfg myKeymap

-- The action to run when a new new window is created.
manageHook :: Query (Endo WindowSet)
manageHook = mconcat
  [ -- Don't manage DOCK-type programs like xmobar.
    manageDocks

    -- Create new windows *below* the current window, to avoid annoyingly
    -- replacing the master window.
    --
    -- The default behavior is 'insertPosition Above Never'.
  , insertPosition Below Newer
  ]

-- How to handle X events.
handleEventHook :: Event -> X All
handleEventHook event =
  -- Whenever a new dock appears, refresh the layout immediately.
  docksEventHook event

mouseBindings :: XConfig Layout -> Map (ButtonMask, Button) (Window -> X ())
mouseBindings =
  X.mouseBindings def

logHook :: Handle -> X ()
logHook xmobar = dynamicLogWithPP PP{..}
 where
  ppCurrent :: String -> String
  ppCurrent = xmobarColor "black" "gray"

  ppExtras :: [X (Maybe String)]
  ppExtras = []

  ppHidden :: String -> String
  ppHidden = xmobarColor "orange" ""

  ppHiddenNoWindows :: WorkspaceId -> String
  ppHiddenNoWindows = id

  ppLayout :: String -> String
  ppLayout = id

  ppOutput :: String -> IO ()
  ppOutput = hPutStrLn xmobar

  ppOrder :: [String] -> [String]
  ppOrder = id

  ppSep :: String
  ppSep = xmobarColor "orange" "" " | "

  ppSort :: X ([WindowSpace] -> [WindowSpace])
  ppSort = getSortByIndex

  ppTitle :: String -> String
  ppTitle = xmobarColor "lightblue" "" . shorten 120

  ppTitleSanitize :: String -> String
  ppTitleSanitize = xmobarStrip . dzenEscape

  ppUrgent :: String -> String
  ppUrgent = xmobarColor "red" "yellow"

  ppVisible :: String -> String
  ppVisible = wrap "(" ")"

  ppWsSep :: String
  ppWsSep = " "

rootMask :: EventMask
rootMask = 
  X.rootMask def

startupHook :: X ()
startupHook = do
  -- TODO: check keymap

  -- Some Java GUI compatibility nonsense
  setWMName "LG3D"

  -- Normal looking cursor, not ugly X
  setDefaultCursor xC_top_left_arrow

  -- Shrug, undocumented function in XMonad.Hooks.ManageDocks
  docksStartupHook

-- How should we handle extra command-line arguments? We don't pass any
-- command-line arguments, so the default behavior is fine, which just blows up
-- if there are any unrecognized arguments.
handleExtraArgs :: [String] -> XConfig Layout -> IO (XConfig Layout)
handleExtraArgs =
  X.handleExtraArgs def

myKeymap :: [(String, X ())]
myKeymap =
  [ -- Mod-minus and Mod-plus: increase/decrease the amount of master windows.
    ("M--", sendMessage (IncMasterN (-1)))
  , ("M-=", sendMessage (IncMasterN 1))

  -- Mod-enter: spawn a terminal.
  , ("M-<Return>", safeSpawn terminal [])

  -- Mod-i: spawn a web browser.
  , ("M-i", safeSpawn "google-chrome-stable" [])

  -- Mod-p: spawn dmenu to run any program by name.
  , ("M-p", safeSpawn "dmenu_run" [])

  -- Mod-c: kill the current window.
  , ("M-c", kill)

  -- Mod-f: toggle fullscreen.
  , ("M-f", sendMessage ToggleLayout)

  -- Mod-h and Mod-l: move left and right through workspaces.
  , ("M-h", moveTo Prev AnyWS)
  , ("M-l", moveTo Next AnyWS)

  -- Mod-j and Mod-k: move up and down through windows in the current workspace.
  , ("M-j", windows focusDown)
  , ("M-k", windows focusUp)

  -- Mod-m: swap the currently-selected window with the master window.
  , ("M-m", windows swapMaster)

  -- Mod-t: snap a floating window back into place.
  , ("M-t", withFocused (\w -> windows (sink w)))

  -- Mod-Shift-h and Mod-Shift-l: move left and right through workspaces,
  -- dragging the currently-selected window with us.
  , ("M-S-h", shiftTo Prev AnyWS >> moveTo Prev AnyWS)
  , ("M-S-l", shiftTo Next AnyWS >> moveTo Next AnyWS)

  -- Mod-Shift-j and Mod-Shift-k: move up and down through windows in the
  -- current workspace, dragging the currently-selected window with us.
  , ("M-S-j", windows swapDown)
  , ("M-S-k", windows swapUp)

  -- Mod-Alt-h and Mod-Alt-l: grow or shrink the master pane by a little bit.
  -- This doesn't seem very useful for me since I only used fixed-width and
  -- fullscreen layouts.
  , ("M-M1-h", sendMessage Shrink)
  , ("M-M1-l", sendMessage Expand)

  -- Mod-Shift-r: restart xmonad
  , ("M-S-r", restart "/home/mitchell/.local/bin/mitchell-xmonad" True)

  -- Mod-Shift-q: quit xmonad.
  , ("M-S-q", io (exitWith ExitSuccess))
  ]
