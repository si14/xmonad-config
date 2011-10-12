-- Imports.
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import System.IO

import XMonad

import XMonad.Config.Gnome

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowNavigation
import XMonad.Actions.Promote
import XMonad.Actions.WindowGo

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Themes
import XMonad.Util.NamedScratchpad

import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.Grid

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh

import qualified XMonad.StackSet as W

modm = mod4Mask
baseConfig = gnomeConfig
myTerminal = "sakura"

-- The main function.
main = do
        config <- withWindowNavigationKeys myWinNavKeys
                  $ baseConfig { modMask = mod4Mask,
                                 terminal = myTerminal,
                                 focusFollowsMouse = False,
                                 focusedBorderColor = "yellow",
                                 layoutHook = myLayoutHook,
                                 manageHook = myManageHook,
                                 workspaces = myWorkspaces
                                }
                  `additionalKeysP` myKeys
        xmonad config

promptConfig = defaultXPConfig {
	font = "xft:Ubuntu:size=10",
	position = Bottom
}

myWorkspaces = ["im", "web", "dev", "files", "other"]

myManageHook = manageDocks
             <+> composeAll [
               isFullscreen --> doFullFloat
             ]
             <+> namedScratchpadManageHook myScratchpads
             <+> manageHook baseConfig

myDWConfig = (theme smallClean) { fontName   = "xft:Ubuntu:size=8",
                                  decoHeight = 16 }

myLayoutHook = smartBorders
               $ showWName' defaultSWNConfig { swn_font = "xft:Ubuntu:size=20" }
               $ avoidStruts
               $ dwmStyle shrinkText myDWConfig
               $ tiled ||| reflectTiled ||| Mirror tiled ||| Grid ||| Full

tiled = ResizableTall 1 (2/100) (1/2) []
reflectTiled = (reflectHoriz tiled)

myWinNavKeys =
  [ ((modm              , xK_Up),    WNGo   U),
    ((modm              , xK_Left),  WNGo   L),
    ((modm              , xK_Down),  WNGo   D),
    ((modm              , xK_Right), WNGo   R),
    ((modm .|. shiftMask, xK_Up),    WNSwap U),
    ((modm .|. shiftMask, xK_Left),  WNSwap L),
    ((modm .|. shiftMask, xK_Down),  WNSwap D),
    ((modm .|. shiftMask, xK_Right), WNSwap R) ]

myKeys = [--("C-M1-l", spawn "slock"),

          ("C-M1-<Right>",   nextWS),
          ("C-M1-<Left>",    prevWS),
          ("C-M1-S-<Right>", shiftToNext >> nextWS),
          ("C-M1-S-<Left>",  shiftToPrev >> prevWS),
          ("C-M-S-<Left>",   shiftPrevScreen),
          ("C-M-S-<Right>",  shiftNextScreen),

          ("M-<Return>", promote),
          ("M-j", sendMessage MirrorShrink),
          ("M-k", sendMessage MirrorExpand),

          ("M-x", shellPrompt promptConfig),
          ("M-S-x", sshPrompt promptConfig),

          ("M-z", namedScratchpadAction myScratchpads "urxvt")] ++
         myApps

myApps = appShortcuts[("b", "google-chrome", "Google-chrome"),
                      ("f", "nautilus", "Nautilus")]

myScratchpads = [NS "urxvt" "urxvt -title urxvt_scratch " (title =? "urxvt_scratch") defaultFloating]

--utility functions

appShortcuts = concat . map appShortcut

appShortcut (key, app, name) =
    [("M-M1-"   ++ key, runOrRaise app (className =? name)),
     ("M-S-M1-" ++ key, spawn app)]
