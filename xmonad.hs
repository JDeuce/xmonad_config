-- ~/.xmonad/xmonad.hs

-- Imports {{{

import XMonad
import XMonad.Util.Run -- spawnPipe
import XMonad.Util.SpawnOnce -- spawnOnce
import XMonad.Hooks.ManageHelpers -- doCenterFloat isFullscreen

import XMonad.Hooks.ManageDocks -- avoidStruts
import XMonad.Hooks.DynamicLog -- dzenColor, etc
import XMonad.Hooks.UrgencyHook -- noUrgencyHook
import XMonad.Hooks.FadeInactive -- fadeInactiveLogHook

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.SimpleFloat

import Data.Ratio ((%))

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Text.Printf

-- End imports }}}

-- Config {{{

-- Define Terminal
myTerminal      = "gnome-terminal"
-- Define workspaces
myWorkspaces    = ["1:shell", "2:mail", "3:web", "4:chat", "5:gimp", "6:vm", "7:qgis", "8:alt1", "9:alt2"]
-- Dzen/Conky
myDzenHeight    = 16 :: Int
myDzenSplit     = 1750 :: Int -- where the left bar split occurs (resolution dependent)
myXmonadBar     = printf "dzen2 -y '0' -h '%d' -w '%d' -ta 'l' -xs 1" myDzenHeight myDzenSplit
myLeftRightBar  = printf "conky -c /home/jjaques/.xmonad/conky-left-tr  | dzen2 -xs 1 -h '%d' -x ' %d' -ta 'r' -y '0'" myDzenHeight myDzenSplit
myRightRightBar = printf "conky -c /home/jjaques/.xmonad/conky-right-tr | dzen2 -xs 2 -h '%d' -ta 'r' -y '0'" myDzenHeight

-- End Config }}}

-- Main {{{

main = do
    dzenLeftLeftBar   <- spawnPipe myXmonadBar
    dzenLeftRightBar  <- spawnPipe myLeftRightBar
    dzenRightRightBar <- spawnPipe myRightRightBar
    autolock <- spawnPipe "xautolock -time 5 -locker 'gnome-screensaver-command -l'"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = \c -> myKeys c `M.union` keys defaultConfig c
      , modMask             = mod4Mask
      , logHook             = myLogHook dzenLeftLeftBar >> fadeInactiveLogHook 0xdddddddd
      , manageHook          = myManageHook
      , layoutHook          = myLayoutHook
      , startupHook         = myStartupHook
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
}

-- End Main }}}

-- Hooks {{{

-- ManageHook {{{

myManageHook :: ManageHook
myManageHook = manageDocks <+> (composeAll . concat $
    [ [resource     =? r  --> doIgnore            | r <- myIgnores]
    , [className    =? c  --> doShift  "1:shell"  | c <- myShell  ]
    , [className    =? c  --> doShift  "2:mail"   | c <- myMail   ]
    , [className    =? c  --> doShift  "3:web"    | c <- myWebs   ]
    , [className    =? c  --> doShift  "4:chat"   | c <- myChat   ]
    , [className    =? c  --> doShift  "5:gimp"   | c <- myGimp   ]
    , [className    =? c  --> doShift  "6:vm"     | c <- myVM     ]
    , [className    =? c  --> doCenterFloat       | c <- myFloats ]
    , [isFullscreen       --> myDoFullFloat                           ]
    ])
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        -- classnames
        myShell   = ["gnome-terminal"]
        myMail    = ["Thunderbird"]
        myWebs    = ["Firefox","Google-chrome"]
        myChat    = ["Pidgin","Buddy List"]
        myGimp    = ["Gimp"]
        myVM      = ["VirtualBox"]
        -- resources
        myIgnores = []
        myFloats  = ["gnome-calculator"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

-- End ManageHook }}}

-- LayoutHook {{{

defaultLayout = avoidStruts $ layoutHook defaultConfig
imLayout      = avoidStruts $ withIM(1%11) (Role "buddy_list") Grid ||| defaultLayout
--floatLayout   = avoidStruts $ simpleFloat
myLayoutHook  = onWorkspaces["4:chat"] imLayout $
                defaultLayout

-- End LayoutHook }}}

-- StartupHook {{{

myStartupHook = do
    spawnOnce "gnome-terminal"
    spawnOnce "thunderbird"
    spawnOnce "google-chrome"
    --spawnOnce "pidgin"

-- End StartupHook }}}

-- LogHook {{{

myLogHook h = dynamicLogWithPP $ dzenPP {
        ppCurrent           =   dzenColor colorStatusFGActive colorStatusBG . pad
        , ppVisible           =   dzenColor colorStatusFG colorStatusBG . pad
        , ppHidden            =   dzenColor colorStatusFG colorStatusBG . pad
        , ppHiddenNoWindows   =   dzenColor colorStatusFGInactive colorStatusBG . pad
        , ppUrgent            =   dzenColor colorStatusFGUrgent colorStatusBGUrgent . pad . dzenStrip
        , ppWsSep             =   ""
        , ppSep               =   "  |  "
        , ppLayout            =   dzenColor colorStatusFGActive colorStatusBG .
            (\x -> case x of
                "Tall"              ->      "Vert"
                "Mirror Tall"       ->      "Horiz"
                "IM Tall"           ->      "Vert"
                "IM Mirror Tall"    ->      "Horiz"
                "IM Grid"           ->      "Grid"
                _                   ->      x
            )
        , ppTitle             =   (" " ++) . dzenColor colorStatusFG colorStatusBG . dzenEscape
        , ppOutput            =   hPutStrLn h
}

-- End LogHook }}}

-- End Hooks }}}

-- Theme {{{

colorStatusFG         = "#ffffff"
colorStatusFGActive   = "#00FF00"
colorStatusFGUrgent   = "black"
colorStatusFGInactive = "#aaaaaa"
colorStatusBGUrgent   = "red"
colorStatusBG         = "#000000"
colorNormalBorder     = "#CCCCC6"
colorFocusedBorder    = "#FF0000"

-- }}}

-- Keys {{{

myRestart :: String
myRestart = "pkill dzen2 ; pkill xautolock; xmonad --recompile && xmonad --restart"

myMenu :: String
myMenu = printf "dmenu_run -fn 'xft:inconsolata:size=10' -p 'Run:' -h %d" myDzenHeight

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = m, terminal = term}) = M.fromList [
             ((m, xK_f),             spawn "nautilus")
            ,((m, xK_p),             spawn myMenu)
            ,((m, xK_q),             spawn myRestart)
            ,((mod1Mask, xK_l),             spawn "gnome-screensaver-command -l")
            ,((mod1Mask, xK_Tab),    windows W.focusUp >> windows W.shiftMaster)
            ,((0, xK_F11),           spawn "amixer -q sset Master 5%-")
            ,((0, xK_F12),           spawn "amixer -q sset Master 5%+")
            ,((0, xK_Print),         spawn "gnome-screenshot -i")
            ]

-- End Keys }}}

-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
