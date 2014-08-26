-- ~/.xmonad/xmonad.hs
-- Imports {{{
import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations

import System.IO
import System.Exit

import XMonad.Util.Run


import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid

import Data.Ratio ((%))

import qualified XMonad.StackSet as W
import qualified Data.Map as M

--}}}

-- Config {{{
-- Define Terminal
myTerminal      = "gnome-terminal"
-- Define workspaces
myWorkspaces    = ["1:shell", "2:mail", "3:web", "4:chat", "5:gimp", "6:vm", "7:qgis", "8:alt1", "9:alt2"]
-- Dzen/Conky
myXmonadBar = "dzen2 -y '0' -h '24' -w '1024' -ta 'l' -fg '#FFFFFF' -bg '#000000'"
myStatusBar = "conky -c /home/jjaques/.xmonad/.conky_dzen | dzen2 -x '1024' -h '24' -ta 'r' -bg '#000000' -fg '#FFFFFF' -y '0'"
myBitmapsDir = "/home/jjaques/.xmonad/dzen2"
--}}}
-- Main {{{
main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "red", "fg", "black", "-xs", "1", "-y", "25"] } urgencyConfig { remindWhen = Every 15 } $ defaultConfig {
        terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = \c -> myKeys c `M.union` keys defaultConfig c
      , modMask             = mod4Mask
      , logHook             = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
      , manageHook          = manageDocks <+> manageHook defaultConfig
      , layoutHook          = avoidStruts $ layoutHook defaultConfig
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      --, borderWidth         = 2
}
--}}}


-- Hooks {{{
--LogHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#000000" . pad
      , ppVisible           =   dzenColor "white" "#000000" . pad
      , ppHidden            =   dzenColor "white" "#000000" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#000000" . pad
      , ppUrgent            =   dzenColor "black" "red" . pad
      , ppWsSep             =   ""
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#ebac54" "#000000" .
                                (\x -> case x of
                                    "Tall"              ->      "Vert"
                                    "Mirror Tall"       ->      "Horiz"
                                    _                   ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#000000" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

--}}}
-- Theme {{{
-- Color names are easier to remember:
colorNormalBorder   = "#CCCCC6"
colorFocusedBorder  = "#fd971f"


barFont  = "inconsolata"
barXFont = "inconsolata:size=12"
xftFont = "xft: inconsolata-14"
--}}}

myRestart :: String
myRestart = "pkill dzen2 ; xmonad --recompile && xmonad --restart"

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = m, terminal = term}) = M.fromList [
             ((m, xK_f),     spawn "nautilus")
            ,((m, xK_q),     spawn myRestart)
            ,((m, xK_l),     spawn "gnome-screensaver-command -l")
            ,((0, xK_F11),   spawn "amixer -q sset Master 5%-")
            ,((0, xK_F12),   spawn "amixer -q sset Master 5%+")
            ,((0, xK_Print), spawn "gnome-screenshot -i")
            ]

-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
