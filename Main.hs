{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures,
             RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Bloop
-- import Debug
import MitchellLayout

import Control.Monad.IO.Class       (MonadIO(..))
import Control.Monad.State          (gets)
import Control.Monad.Trans.Maybe    (MaybeT(..))
import Data.Bits                    ((.|.))
import Data.ByteString              (ByteString)
import Data.Map                     (Map)
import Data.Monoid                  (All, Endo)
import Graphics.X11.Types           (enterWindowMask, propertyChangeMask,
                                     structureNotifyMask)
import System.Exit                  (ExitCode(ExitSuccess), exitWith)
import System.IO                    (Handle, hPutStrLn)
import System.Posix.Files           (readSymbolicLink)
import System.Posix.Types           (ProcessID)
import XMonad                       (Button, ButtonMask,
                                     ChangeLayout(NextLayout), Event, EventMask,
                                     Full(Full), IncMasterN(IncMasterN),
                                     KeyMask, KeySym, Layout, Query,
                                     Resize(Expand, Shrink), Tall(Tall), Window,
                                     WindowSet, WindowSpace, WorkspaceId, X,
                                     XConfig(XConfig), def, io, launch,
                                     mod4Mask, sendMessage, withFocused,
                                     xC_top_left_arrow, (|||))
import XMonad.Actions.CycleWS       (Direction1D(Next, Prev), WSType(AnyWS),
                                     moveTo, shiftTo)
import XMonad.Hooks.DynamicLog      (PP(..), dynamicLogWithPP, dzenEscape,
                                     shorten, wrap, xmobarColor, xmobarStrip)
import XMonad.Hooks.InsertPosition  (Focus(Newer), Position(Below),
                                     insertPosition)
import XMonad.Hooks.ManageDocks     (avoidStruts, docksEventHook,
                                     docksStartupHook, manageDocks)
import XMonad.Hooks.SetWMName       (setWMName)
import XMonad.Layout.NoBorders      (noBorders)
import XMonad.Layout.ToggleLayouts  (ToggleLayout(ToggleLayout), toggleLayouts)
import XMonad.Operations            (kill, restart, windows)
import XMonad.StackSet              (focusDown, focusUp, sink, swapDown,
                                     swapMaster, swapUp)
import XMonad.Util.Cursor           (setDefaultCursor)
import XMonad.Util.EZConfig         (mkKeymap)
import XMonad.Util.Run              (safeSpawn, spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified Data.ByteString            as ByteString
import qualified XMonad                     as X
import qualified XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet
import qualified XMonad.Util.Run

(∘) = (.)
(⨾) = flip (.)

δ :: Functor f => (a -> b) -> (f a -> f b)
δ = fmap

-- δ² :: (Functor f, Functor g) => (a -> b) -> (f (g a)) -> (f (g b))
-- δ² = fmap ∘ fmap

-- Main entrypoint: spawn xmobar, then launch xmonad.
main :: IO ()
main = do
  xmobar :: Handle <-
    spawnPipe "xmobar /home/mitchell/.xmobarrc"

  safeSpawn "xset" ["r", "rate", "160", "120"]
  safeSpawn "xrdb" ["-merge", "~/.Xresources"]
  safeSpawn "xmodmap" ["~/.Xmodmap"]
  safeSpawn "xbindkeys" []

  launch XConfig
    { -- How many pixels wide should the border of the currently-selected
      -- window be?
      X.borderWidth = 2

      -- When manually clicking on a window to focus it, should that click also
      -- be delivered to the window? 'True' means no, just focus it.
    , X.clickJustFocuses = True

    , X.clientMask = clientMask

      -- Should hovering over a window with the mouse select it?
    , X.focusFollowsMouse = False

      -- Focused window border color.
    , X.focusedBorderColor = "#3F607F"

      -- Rebind Mod to the Windows key.
    , X.modMask = mod4Mask

      -- Unfocused window border color.
    , X.normalBorderColor = "#000000"

      -- The list of workspace names.
    , X.workspaces = ["α", "β", "γ", "δ", "ε", "ζ"]

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
layoutHook =
  avoidStruts (toggleLayouts x1 x2)
 where
  -- Fullscreen mode, without wasting any pixels drawing a border.
  x1 = noBorders Full

  x2 = y1 ||| y2
  y1 = mitchellLayout (7/10) (8/10) 0 0 0
  y2 = Tall 1 (3/100) (1/2)

-- Our preferred terminal application.
terminal   = "alacritty" :: String
terminalBS = "alacritty" :: ByteString

keys :: XConfig l -> Map (KeyMask, KeySym) (X ())
keys cfg =
  mkKeymap cfg myKeymap

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

  -- Mod-enter: spawn a terminal in the current directory.
  , ("M-<Return>", do
      runMaybeT (do
        pid     <- MaybeT currentWindowPid
        True    <- isTerminal pid
        [child] <- getChildrenPids pid
        cwd     <- liftIO (readSymbolicLink ("/proc/" ++ child ++ "/cwd"))
        safeSpawn terminal ["--working-directory", cwd])
      >>= maybe (safeSpawn terminal []) pure)

  -- Mod-i: spawn a web browser.
  , ("M-i", safeSpawn "google-chrome-stable" [])

  -- Mod-p: spawn dmenu to run any program by name.
  , ("M-p", safeSpawn "dmenu_run" ["-b", "-fn", "PragmataPro Mono-34"])

  -- Mod-c: kill the current window.
  , ("M-c", kill)

  -- Mod-f: toggle fullscreen.
  , ("M-f", sendMessage ToggleLayout)

  -- Mod-space: next layout
  , ("M-<Space>", sendMessage NextLayout)

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
  , ("M-M1-h", sendMessage Shrink)
  , ("M-M1-l", sendMessage Expand)

  , ("M-M1-S-h", sendMessage (Bloop 'H'))
  , ("M-M1-S-j", sendMessage (Bloop 'J'))
  , ("M-M1-S-k", sendMessage (Bloop 'K'))
  , ("M-M1-S-l", sendMessage (Bloop 'L'))

  , ("M-M1-j", sendMessage (Bloop 'j'))
  , ("M-M1-k", sendMessage (Bloop 'k'))

  , ("M-M1-m", sendMessage (Bloop 'm'))
  , ("M-M1-0", sendMessage (Bloop '0'))

  -- Mod-Shift-r: restart xmonad
  , ("M-S-r", restart "mitchell-xmonad" True)

  -- Mod-Shift-q: quit xmonad.
  , ("M-S-q", io (exitWith ExitSuccess))
  ]

currentWindowPid :: X (Maybe ProcessID)
currentWindowPid =
  runMaybeT $ do
    windowset :: WindowSet <-
      gets X.windowset

    window :: Window <-
      (MaybeT ∘ pure ∘ XMonad.StackSet.peek) windowset

    (MaybeT ∘ X.runQuery XMonad.Hooks.ManageHelpers.pid) window

getChildrenPids :: MonadIO m => ProcessID -> m [String]
getChildrenPids pid =
  lines <$>
    XMonad.Util.Run.runProcessWithInput "pgrep" ["-P", show pid] ""

isTerminal :: MonadIO m => ProcessID -> m Bool
isTerminal =
  cmdlinePath⨾
  ByteString.readFile⨾
  δ (ByteString.takeWhile (/= 0))⨾
  δ (== terminalBS)⨾
  liftIO

cmdlinePath :: ProcessID -> FilePath
cmdlinePath pid =
  "/proc/" ++ show pid ++ "/cmdline"
