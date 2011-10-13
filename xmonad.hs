{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import Data.Ratio ((%))
import System.IO
import Control.Monad

import XMonad

import XMonad.Config.Gnome

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive

import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowNavigation
import XMonad.Actions.Promote
import XMonad.Actions.WindowGo
import XMonad.Actions.NoBorders
import XMonad.Actions.GridSelect

import XMonad.Util.EZConfig
import XMonad.Util.Themes
import XMonad.Util.WindowProperties

import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutModifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh

import qualified XMonad.StackSet as W

modm = mod4Mask
baseConfig = gnomeConfig
myTerminal = "gnome-terminal"

-- The main function.
main = do
        spawn "xcompmgr -C"
        xmonad $ baseConfig { modMask = modm,
                              terminal = myTerminal,
                              focusFollowsMouse = False,
                              focusedBorderColor = "yellow",
                              borderWidth = 0,
                              layoutHook = myLayoutHook,
                              logHook    = myLogHook,
                              manageHook = myManageHook,
                              workspaces = myWorkspaces
                            }
                  `additionalKeysP` myKeys

promptConfig = defaultXPConfig {
	font = "xft:Ubuntu:size=10",
	position = Bottom
}

myWorkspaces = ["jabber", "skype", "web", "dev1", "dev2", "music", "other"]

myManageHook = manageDocks
             <+> composeAll [
               className =? "Unity-2d-panel" --> doIgnore
             , isFullscreen                  --> doFullFloat
             , className =? "Skype"          --> (doF $ W.shift "skype")
             , className =? "Gajim.py"         --> (doF $ W.shift "jabber")
             ] <+> manageHook baseConfig

myDWConfig = (theme smallClean) { fontName   = "xft:Ubuntu:size=8",
                                  decoHeight = 16 }

myLayoutHook = gaps [(U, 24)]
               $ showWName' defaultSWNConfig { swn_font = "xft:Ubuntu:size=20" }
               $ onWorkspace "jabber" (reflectHoriz (withIM (1%7) (Role "roster") Grid))
               $ onWorkspace "skype" (reflectHoriz tiled)
               $ dwmStyle shrinkText myDWConfig
               $ layouts
               where
                 layouts = tiled 
                           ||| (reflectHoriz tiled) 
                           ||| Mirror tiled 
                           ||| Grid 
                           ||| tabbed shrinkText myDWConfig
                           ||| Full

tiled = ResizableTall 1 (2/100) (1/2) []
myLogHook = fadeInactiveLogHook 0.7 >> logHook baseConfig

myGSConfig = defaultGSConfig { gs_cellwidth = 250,
                               gs_cellheight = 50,
                               gs_font = "xft:Ubuntu:size=8" }

myKeys = [("C-M1-<Right>",   nextWS),
          ("C-M1-<Left>",    prevWS),
          ("C-M1-S-<Right>", shiftToNext >> nextWS),
          ("C-M1-S-<Left>",  shiftToPrev >> prevWS),
          ("C-M-S-<Left>",   shiftPrevScreen),
          ("C-M-S-<Right>",  shiftNextScreen),

          ("M-<Right>",   windows W.focusDown),
          ("M-<Left>",    windows W.focusUp),
          ("M-S-<Right>", windows W.swapDown),
          ("M-S-<Left>",  windows W.swapUp),

          ("M-a",   goToSelected myGSConfig),
          ("M-S-a", bringSelected myGSConfig),

          ("M-p",   spawn "~/.xmonad/screenshot"),
          ("M-S-p", spawn "~/.xmonad/select-screenshot"),

          ("M-<Return>", promote),
          ("M-j", sendMessage MirrorShrink),
          ("M-k", sendMessage MirrorExpand),

          ("M-x", shellPrompt promptConfig),
          ("M-S-x", sshPrompt promptConfig)] ++
         myApps

myApps = appShortcuts[("b", "google-chrome", "Google-chrome"),
                      ("f", "nautilus", "Nautilus")]

--utility functions

appShortcuts = concat . map appShortcut

appShortcut (key, app, name) =
    [("M-M1-"   ++ key, runOrRaise app (className =? name)),
     ("M-S-M1-" ++ key, spawn app)]

