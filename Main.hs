{-# LANGUAGE DeriveFunctor, DerivingStrategies, GeneralizedNewtypeDeriving,
             LambdaCase, OverloadedStrings, PartialTypeSignatures,
             PatternSynonyms, RankNTypes, RecordWildCards,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Bloop
-- import Debug
import MitchellLayout

import           Control.Applicative
import           Control.Monad.Cont
import           Control.Monad.IO.Class       (MonadIO(..))
import           Control.Monad.State          (gets)
import           Control.Monad.Trans.Maybe    (MaybeT(..))
import           Data.Bits                    ((.|.))
import           Data.ByteString              (ByteString)
import           Data.Default.Class           (def)
import           Data.Map                     (Map)
import           Data.Monoid                  (All, Endo)
import           Graphics.X11.Types           (enterWindowMask,
                                               propertyChangeMask,
                                               structureNotifyMask)
import           System.Exit                  (ExitCode(ExitSuccess), exitWith)
import           System.IO                    (Handle, hPutStrLn)
import           System.Posix.Files           (createNamedPipe, namedPipeMode,
                                               ownerReadMode, ownerWriteMode,
                                               readSymbolicLink)
import           System.Posix.Types           (ProcessID)
import           XMonad                       (X, XConfig)
import           XMonad.Actions.CycleWS       (Direction1D(Next, Prev),
                                               WSType(AnyWS), moveTo, shiftTo)
import           XMonad.Hooks.DynamicLog      (PP(..), dynamicLogWithPP,
                                               dzenEscape, shorten, wrap,
                                               xmobarColor, xmobarStrip)
import           XMonad.Hooks.InsertPosition  (Focus(Newer), Position(Below),
                                               insertPosition)
import           XMonad.Hooks.ManageDocks     (avoidStruts, docksEventHook,
                                               docksStartupHook, manageDocks)
import           XMonad.Hooks.SetWMName       (setWMName)
import           XMonad.Layout.NoBorders      (noBorders)
import           XMonad.Layout.ToggleLayouts  (ToggleLayout(ToggleLayout),
                                               toggleLayouts)
import           XMonad.Operations            (kill, restart, windows)
import           XMonad.StackSet              (focusDown, focusUp, sink,
                                               swapDown, swapMaster, swapUp)
import           XMonad.Util.Cursor           (setDefaultCursor)
import qualified XMonad.Util.ExtensibleState
import           XMonad.Util.EZConfig         (mkKeymap)
import           XMonad.Util.Run              (safeSpawn, spawnPipe)
import           XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified Data.ByteString as ByteString
-- import qualified Data.Typeable              as Typeable
import qualified XMonad                     as X
import qualified XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet            as X (Stack)
import qualified XMonad.StackSet            as X.Stack
import qualified XMonad.Util.Run

-- Main entrypoint: spawn xmobar, then launch xmonad.
main :: IO ()
main = do
  createNamedPipe
    "/tmp/xmobar-wireguard.fifo"
    (ownerReadMode .|. ownerWriteMode .|. namedPipeMode)
    <|> pure ()
  safeSpawn "wg-ping-xmobar-fifo" []

  xmobar :: Handle <-
    spawnPipe "xmobar /home/mitchell/.xmobarrc"

  safeSpawn "xset" ["r", "rate", "160", "120"]
  safeSpawn "xrdb" ["-merge", "~/.Xresources"]
  safeSpawn "xmodmap" ["~/.Xmodmap"]
  safeSpawn "xbindkeys" []

  X.launch X.XConfig
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
    , X.modMask = X.mod4Mask

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
clientMask :: X.EventMask
clientMask =
  enterWindowMask .|. propertyChangeMask .|. structureNotifyMask

-- The layouts.
layoutHook =
  avoidStruts (toggleLayouts x1 x2)
 where
  -- Fullscreen mode, without wasting any pixels drawing a border.
  x1 = noBorders X.Full

  x2 = y1 X.||| y2
  y1 = mitchellLayout (7/10) (8/10) 0 0 0
  y2 = X.Tall 1 (3/100) (1/2)

-- Our preferred terminal application.
terminal   = "alacritty" :: String
terminalBS = "alacritty" :: ByteString

keys :: XConfig l -> Map (X.KeyMask, X.KeySym) (X ())
keys cfg =
  mkKeymap cfg myKeymap

-- The action to run when a new new window is created.
manageHook :: X.Query (Endo X.WindowSet)
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
handleEventHook :: X.Event -> X All
handleEventHook event =
  -- Whenever a new dock appears, refresh the layout immediately.
  docksEventHook event

mouseBindings
  :: XConfig X.Layout
  -> Map (X.ButtonMask, X.Button) (X.Window -> X ())
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

  ppHiddenNoWindows :: X.WorkspaceId -> String
  ppHiddenNoWindows = id

  ppLayout :: String -> String
  ppLayout = id

  ppOutput :: String -> IO ()
  ppOutput = hPutStrLn xmobar

  ppOrder :: [String] -> [String]
  ppOrder = id

  ppSep :: String
  ppSep = xmobarColor "orange" "" " | "

  ppSort :: X ([X.WindowSpace] -> [X.WindowSpace])
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

rootMask :: X.EventMask
rootMask =
  X.rootMask def

startupHook :: X ()
startupHook = do
  -- TODO: check keymap

  -- Some Java GUI compatibility nonsense
  setWMName "LG3D"

  -- Normal looking cursor, not ugly X
  setDefaultCursor X.xC_top_left_arrow

  -- Shrug, undocumented function in XMonad.Hooks.ManageDocks
  docksStartupHook

-- How should we handle extra command-line arguments? We don't pass any
-- command-line arguments, so the default behavior is fine, which just blows up
-- if there are any unrecognized arguments.
handleExtraArgs :: [String] -> XConfig X.Layout -> IO (XConfig X.Layout)
handleExtraArgs =
  X.handleExtraArgs def

myKeymap :: [(String, X ())]
myKeymap =
  [ -- Mod-minus and Mod-plus: increase/decrease the amount of master windows.
    -- ("M--", sendMessage (IncMasterN (-1)))
  -- , ("M-=", sendMessage (IncMasterN 1))

  -- Mod-enter: spawn a terminal in the current directory.
    ("M-<Return>", spawnTerminalInCurrentDirectory)

  -- Mod-i: spawn a web browser.
  , ("M-i", safeSpawn "google-chrome-stable" [])

  -- Mod-p: spawn dmenu to run any program by name.
  , ("M-p", safeSpawn "dmenu_run" ["-b", "-fn", "PragmataPro Mono-18"])

  -- Mod-c: kill the current window.
  , ("M-c", kill)

  -- Mod-f: toggle fullscreen.
  , ("M-f", X.sendMessage ToggleLayout)

  -- Mod-space: next layout
  , ("M-<Space>", X.sendMessage X.NextLayout)

  -- Mod-h and Mod-l: move left and right through workspaces.
  , ("M-h", moveTo Prev AnyWS)
  , ("M-l", moveTo Next AnyWS)

  -- Mod-j and Mod-k: move up and down through windows in the current workspace.
  -- In MitchellLayout with the second master above the first, we pretend the
  -- master and second master have swapped positions in the stack, so j/k
  -- movement feels more natural.
  , ("M-j",
      runKM
        (handleModj
          <$> getCurrentStack
          <*> kX XMonad.Util.ExtensibleState.get))

  , ("M-k", do
      runKM
        (handleModk
          <$> getCurrentStack
          <*> kX XMonad.Util.ExtensibleState.get))

  -- Mod-m: swap the currently-selected window with the master window.
  , ("M-m", windows swapMaster)

  -- Mod-t: snap a floating window back into place.
  , ("M-t", X.withFocused (sink⨾ windows))

  -- Mod-Shift-h and Mod-Shift-l: move left and right through workspaces,
  -- dragging the currently-selected window with us.
  , ("M-S-h", shiftTo Prev AnyWS >> moveTo Prev AnyWS)
  , ("M-S-l", shiftTo Next AnyWS >> moveTo Next AnyWS)

  -- Mod-Shift-j and Mod-Shift-k: move up and down through windows in the
  -- current workspace, dragging the currently-selected window with us.
  , ("M-S-j", windows swapDown)
  , ("M-S-k", windows swapUp)

  -- Mod-Alt-h and Mod-Alt-l: grow or shrink the master pane by a little bit.
  , ("M-M1-h", X.sendMessage X.Shrink)
  , ("M-M1-l", X.sendMessage X.Expand)

  , ("M-M1-S-h", X.sendMessage (Bloop 'H'))
  , ("M-M1-S-j", X.sendMessage (Bloop 'J'))
  , ("M-M1-S-k", X.sendMessage (Bloop 'K'))
  , ("M-M1-S-l", X.sendMessage (Bloop 'L'))

  , ("M-M1-j", X.sendMessage (Bloop 'j'))
  , ("M-M1-k", X.sendMessage (Bloop 'k'))

  , ("M-M1-m", X.sendMessage (Bloop 'm'))
  , ("M-M1-n", X.sendMessage (Bloop 'n'))
  , ("M-M1-0", X.sendMessage (Bloop '0'))

  -- Mod-Shift-r: restart xmonad
  , ("M-S-r", restart "mitchell-xmonad" True)

  -- Mod-Shift-q: quit xmonad.
  , ("M-S-q", liftIO (exitWith ExitSuccess))
  ]

getCurrentStack :: K () (X.Stack X.Window)
getCurrentStack =
  (ContT⨾ K)
    (\k ->
      gets (X.windowset⨾ X.Stack.current⨾ X.Stack.workspace⨾ X.Stack.stack) >>=
        maybe (pure ()) k)

handleModj :: X.Stack X.Window -> MitchState -> X ()
handleModj stack = \case
  MitchStateDoSwapJK ->
    case stack of
      StackSelecting1of1 -> pure ()
      StackSelecting1of2 -> windows focusDown
      StackSelecting1ofN -> windows (focusDown ∘ focusDown)
      StackSelecting2ofN -> windows focusUp
      StackSelectingNofN -> windows (focusDown ∘ focusDown)
      _                  -> windows focusDown

  MitchStateDontSwapJK ->
    windows focusDown

handleModk :: X.Stack X.Window -> MitchState -> X ()
handleModk stack = \case
  MitchStateDoSwapJK ->
    case stack of
      StackSelecting1of1 -> pure ()
      StackSelecting1ofN -> windows focusDown
      StackSelecting2of2 -> windows focusDown
      StackSelecting2ofN -> windows (focusUp ∘ focusUp)
      StackSelecting3ofN -> windows (focusUp ∘ focusUp)
      _                  -> windows focusUp

  MitchStateDontSwapJK ->
    windows focusUp

pattern StackSelecting1of1 <- X.Stack.Stack _ []    []
pattern StackSelecting1of2 <- X.Stack.Stack _ []    [_]
pattern StackSelecting1ofN <- X.Stack.Stack _ []    _
pattern StackSelecting2of2 <- X.Stack.Stack _ [_]   []
pattern StackSelecting2ofN <- X.Stack.Stack _ [_]   _
pattern StackSelecting3ofN <- X.Stack.Stack _ [_,_] _
pattern StackSelectingNofN <- X.Stack.Stack _ _     []

spawnTerminalInCurrentDirectory :: X ()
spawnTerminalInCurrentDirectory =
  runMaybeT (do
    pid     <- MaybeT currentWindowPid
    True    <- isTerminal pid
    [child] <- getChildrenPids pid
    cwd     <- liftIO (readSymbolicLink ("/proc/" ++ child ++ "/cwd"))
    safeSpawn terminal ["--working-directory", cwd])
  >>= maybe (safeSpawn terminal []) pure

currentWindowPid :: X (Maybe ProcessID)
currentWindowPid =
  runMaybeT $ do
    windowset :: X.WindowSet <-
      gets X.windowset

    window :: X.Window <-
      (MaybeT ∘ pure ∘ X.Stack.peek) windowset

    (MaybeT ∘ X.runQuery XMonad.Hooks.ManageHelpers.pid) window

getChildrenPids :: MonadIO m => ProcessID -> m [String]
getChildrenPids pid =
  δ lines
    (XMonad.Util.Run.runProcessWithInput "pgrep" ["-P", show pid] "")

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

--------------------------------------------------------------------------------

(∘) = (.)
(⨾) = flip (.)

δ :: Functor f => (a -> b) -> (f a -> f b)
δ = fmap

-- δ² :: (Functor f, Functor g) => (a -> b) -> (f (g a)) -> (f (g b))
-- δ² = fmap ∘ fmap

newtype K r a
  = K (ContT r X a)
  deriving stock (Functor)
  deriving newtype (Applicative, Monad)

runK :: K r r -> X r
runK (K x) =
  runContT x pure

runKM :: K r (X r) -> X r
runKM (K x) =
  runContT x id

kX :: X a -> K r a
kX =
  lift⨾ K

-- instance Applicative K where
--   pure x = K (pure x)
--   K f <*> K x = K (f <*> x)

-- instance Monad K where
--   return = pure
--   K x >>= f = K (x >>= f⨾ unK)
