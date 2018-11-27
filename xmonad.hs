{-# OPTIONS_GHC-fno-warn-missing-signatures -fno-warn-type-defaults #-}
import XMonad hiding ((|||))
import System.Exit
import System.IO

import Control.Monad (when, join, liftM2)
import Data.Maybe (maybeToList,fromJust,isJust)
import Data.List
import Data.Char (toLower)
import Data.Function (on)

import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap
import XMonad.Actions.NoBorders
import XMonad.Actions.WithAll hiding (killAll)
import XMonad.Actions.Promote
import XMonad.Actions.CopyWindow
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.OnScreen
import XMonad.Actions.CycleWorkspaceByScreen

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ToggleHook
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.HintedGrid
import XMonad.Layout.Cross
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Drawer

import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Layout
import XMonad.Prompt.FuzzyMatch

import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Font

import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W


myProjects =
  [ Project { projectName = "MSG"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "urxvtc -geometry 130x26-500+200 -e cmus"
            }
  , Project { projectName = "GEN"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                runInTerm "urxvtc" "zsh -c 'neofetch; zsh'"
            }

  , Project { projectName = "WEB"
            , projectDirectory = "~/Downloads"
            , projectStartHook = Just $ do
                spawn "firefox"
            }
  , Project { projectName = "TOR"
            , projectDirectory = "~/data/torrent"
            , projectStartHook = Just $ do
                spawn "qbittorrent"
            }

  , Project { projectName = "GAME"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName = "ML"
            , projectDirectory = "~/Sync/project/yolov3_on_respberrypi/yolo"
            , projectStartHook = Just $ do
                runInTerm "-title nixos-env-fun" "load-env-ml emacs --eval \"(call-interactively 'ein:jupyter-server-start)\""
            }
  , Project { projectName = "ORG"
            , projectDirectory = "~/Sync/sync/org"
            , projectStartHook = Just $ do
                spawn "emacsclient -nc -e '(progn (org-todo-list)(delete-other-windows)(org-agenda-redo-all))'"
            }
  , Project { projectName = "WRK"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "emacsclient -nc"
            }

  , Project { projectName = "DOC"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  ]


myPromptTheme = def
  { position = Bottom
  , font = "xft:Sarasa UI SC:size=9"
  , height = 22
  , bgColor = "#2d3436"
  , promptBorderWidth = 0
  , searchPredicate = fuzzyMatch
  }


systemPromptCmds =
  [ ("Shutdown", spawn "$HOME/.bin/shutdown.sh poweroff")
  , ("Reboot", spawn "$HOME/.bin/shutdown.sh reboot")
  , ("Exit", io exitSuccess)
  , ("Hibernate", spawn "sudo systemctl hibernate")
  , ("Restart", restart "xmonad" True)
  ]

myScratchPads =
  [ NS "fileManager" "thunar" (className =? "Thunar")
    (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  , NS "htop" "urxvtc -title htop -e htop"
      (title =? "htop")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "nm" "urxvtc -title nmtui -e nmtui"
      (title =? "nmtui")
      (customFloating $ W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3))
  , NS "arandr" "arandr"
      (className =? "Arandr")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "pamix" "urxvtc -title pamix -e ncpamixer"
      (title =? "pamix")
      (customFloating $ W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3))
  , NS "ranger" "urxvtc -title rangerfloat -e ranger"
      (title =? "rangerfloat")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "org"
       "emacsclient -c -F '((name . \"org-agenda\") (alpha . (85 . 85)))' -e '(org-capture nil \"i\")'"
       (title =? "org-agenda")
       (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  , NS "dropdown"
    " urxvtc -title dropdown -e zsh -c 'tmux has -t dropdown && exec tmux attach-session -d -t dropdown || exec tmux new-session -s dropdown'"
      (title =? "dropdown")
      (customFloating $ W.RationalRect 0 0.02 1 0.5)
  ]

myTerminal = "urxvtc"

myFocusFollowsMouse = True

myBorderWidth = 3

xmobarTitleColor = "#ababab"

myNormalBorderColor = "#282A36"

myFocusedBorderColor = "#7f8c8d"

myModMask = mod4Mask

myOrgCmd = "emacsclient -nc"

myWorkspaces = ["GEN","WEB","WRK","ORG","DOC","MSG","VOD","GAME","TOR"]

killAll = withAll (\w -> do (focus w) >> kill1)

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
     -- Basic
  [ ((modm, xK_q), kill1)
  , ((modm, xK_BackSpace), killAll)
  , ((modm .|. controlMask, xK_s), sinkAll)
  , ((modm .|. shiftMask, xK_e), io exitSuccess)
  , ((modm, xK_n), refresh)
  , ((modm, xK_r), spawn "rofi -show run")
  , ((modm, xK_Tab), cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_Tab xK_grave)
  , ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_m), windows W.focusMaster)
  , ((modm, xK_Return), promote)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_t), withFocused $ windows . W.sink)
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
  -- Workspace Bindings
  , ((modm .|. shiftMask, xK_Return), bindOn [("WEB", spawn "firefox")
                                             ,("ORG", spawn myOrgCmd)
                                             ,("DOC", spawn "XMind")
                                             ,("GAME", spawn "steam")
                                             ,("", spawn "urxvtc")])
  , ((modm, xK_w), selectWorkspace myPromptTheme)
  , ((modm .|. shiftMask, xK_w), withWorkspace myPromptTheme (windows . W.shift))
  , ((modm .|. controlMask, xK_w), withWorkspace myPromptTheme (windows . copy))
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
  , ((modm, xK_Left ), DO.moveTo Prev HiddenNonEmptyWS)
  , ((modm, xK_Right), DO.moveTo Next HiddenNonEmptyWS)
  , ((modm, xK_u), focusUrgent)
  -- Layout Management
  , ((modm, xK_p), sendMessage NextLayout)
  , ((modm .|. shiftMask, xK_p), layoutPrompt myPromptTheme)
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  , ((modm, xK_b), withFocused toggleBorder)
  , ((modm, xK_y), sendMessage $ Toggle REFLECTX)
  , ((modm .|. shiftMask, xK_f), sendMessage $ Toggle FULL)
  , ((modm, xK_v), sendMessage ToggleStruts)
  -- Manage Hooks
  , ((modm .|. controlMask, xK_t), toggleHookAllNew "sink" >> runLogHook)
  , ((modm .|. controlMask, xK_f), toggleHookAllNew "float" >> runLogHook)
  , ((modm, xK_backslash), toggleHookNext "float" >> runLogHook)
  , ((modm .|. shiftMask, xK_t), sinkAll)
  -- Submap
  , ((modm, xK_e), submap . M.fromList $
      [ ((0, xK_e), spawn "emacsclient -nc")
      , ((modm, xK_e), spawn "emacsclient -nc")
      , ((0, xK_f), spawn "firefox")
      , ((0, xK_h), namedScratchpadAction myScratchPads "htop")
      , ((0, xK_n), namedScratchpadAction myScratchPads "nm")
      , ((0, xK_v), namedScratchpadAction myScratchPads "pamix")
      , ((0, xK_a), namedScratchpadAction myScratchPads "music")
      , ((0, xK_r), namedScratchpadAction myScratchPads "arandr")
      ])
   -- Applications
  , ((modm .|. shiftMask, xK_r) , spawn "pkill xmobar; xmonad --recompile; xmonad --restart")
  , ((modm, xK_f), namedScratchpadAction myScratchPads "ranger")
  , ((modm, xK_space), namedScratchpadAction myScratchPads "dropdown")
  , ((modm, xK_c), namedScratchpadAction myScratchPads "org")
  , ((modm, xK_g), spawn "$HOME/.bin/rofi-surfraw.sh")
   -- Volume control
  , ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
  , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
  , ((0, 0x1008FF12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ((modm, xK_Home), spawn "~/.bin/AudioSwitch.sh speaker")
  , ((modm, xK_End), spawn "~/.bin/AudioSwitch.sh headphone")
  , ((modm, xK_equal), spawn "playerctl next")
  , ((modm, xK_minus), spawn "playerctl previous")
  , ((modm, xK_0), spawn "playerctl play-pause")
  , ((modm .|. shiftMask, xK_0), spawn "playerctl -a play-pause")
  , ((modm .|. controlMask, xK_0), spawn "playerctl -a stop")
   -- Screenshots
  , ((0, xK_Print), spawn "$HOME/.bin/ScreenShot.sh")
   -- System Prompt
   ,((0, xK_Pause), spawn "i3lock-fancy")
   ,((modm .|. shiftMask, xK_q), xmonadPromptC systemPromptCmds myPromptTheme)

  ] ++
    -- Workspaces
  zip (zip (repeat (modm)) [xK_1..xK_9]) (map (DO.withNthWorkspace W.greedyView) [0..])
    ++
  zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (DO.withNthWorkspace W.shift) [0..])
    ++
  zip (zip (repeat (modm .|. controlMask)) [xK_1..xK_9]) (map (DO.withNthWorkspace copy) [0..])
    ++
    -- Monitors
  [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_s, xK_a, xK_d] [0 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- set mouse side button to float and resize
    , ((0, 8), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((0, 9), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myTheme = def
   {  activeColor         = myFocusedBorderColor
    , inactiveColor       = "#111111"
    , activeBorderColor   = myFocusedBorderColor
    , inactiveBorderColor = "#111111"
    , activeTextColor     = "black"
    , inactiveTextColor   = myFocusedBorderColor
    , fontName            = "xft:Sarasa UI SC:size=10"
    , decoHeight          = 16
    }

-- Layouts

mySpacing x = spacingRaw True (Border 0 x x x) True (Border x x x x) True

myTiled = renamed [Replace "T"]
    . mySpacing 4
    $ mkToggle (single REFLECTX)
    $ drawer `onBottom` (Tall 1 (3 / 100) (1 / 2))
    where
      drawer = simpleDrawer 0 0.3 (Title "nixos-env-fun")

myMirror = renamed [Replace "M"]
    $ Mirror myTiled

myFloat = renamed [Replace "L"]
    $ floatSimple shrinkText myTheme

mySimpleFloat = renamed [Replace "SFlot"]
    $ simplestFloat

myGrid = renamed [Replace "Grid"]
    $ mkToggle (single REFLECTX)
    $ Grid False

myCross = renamed [Replace "Cross"]
    . noBorders
    $ simpleCross

myPane = renamed [Replace "Pane"]
    . mySpacing 4
    $ mkToggle (single REFLECTX)
    $ TwoPane (3/100) (1/2)

myTab = renamed [Replace "Tab"]
    $ tabbed shrinkText myTheme

myBig = renamed [Replace "Big"]
    . mySpacing 4
    $ OneBig (3/4) (3/4)

myLayout = id
   . smartBorders
   . mkToggle (single FULL)
   . avoidStruts
   $ onWorkspace "WRK" (myTiled ||| myPane ||| myMirror)
   $ onWorkspace "WEB" (myTab ||| myPane |||myCross ||| myBig)
   $ onWorkspace "VOD" myGrid
   $ onWorkspace "MSG" (mySimpleFloat ||| myFloat ||| myCross ||| myGrid)
   $ onWorkspace "GAME" mySimpleFloat
   $ onWorkspace "TOR" mySimpleFloat
   $ myTiled |||  myMirror  ||| myGrid ||| myCross
   ||| myPane ||| myTab ||| myBig ||| myFloat ||| mySimpleFloat

myManageHook =
  composeAll . concat $
  [ [manageDocks]
  , [isFullscreen --> doFullFloat]
  , [isDialog --> doCenterFloat]
  , [className =? c --> doCenterFloat | c <- myCFloats]
  , [title =? t --> doCenterFloat | t <- myTFloats]
  , [role =? t --> doFloat | t <- myRole]
  , [resource =? r --> doFloat | r <- myRFloats]
  , [fmap (pt `isInfixOf`) title --> doFloat | pt <- myPTFloats]
  , [fmap (pc `isInfixOf`) className --> doFloat | pc <- myPCFloats]
  , [namedScratchpadManageHook myScratchPads]
  , [className =? "mpv" --> doShift "VOD" ]
  , [className =? ".zathura-wrapped_" --> doShiftAndGo "DOC"]
  , [className =? "XMind ZEN" --> doShiftAndGo "DOC" ]
  , [className =? "Zeal" --> doShiftAndGo "DOC" ]
  , [title =? "XMind" --> doFloat <+> doShiftAndGo "DOC" ]
  , [className =? "qBittorrent" --> doFloat <+> doShift "TOR"]
  , [className =? "Steam" --> doFloat <+> doShift "GAME"]
  ]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    role = stringProperty "WM_WINDOW_ROLE"
    myCFloats =
      [ -- "mpv"
        "Lxappearance"
      , "File-roller"
      , "Gimp"
      , "VirtualBox"
      , "Gpicview"
      , "octave-gui"
      , "Gnuplot"
      , "Wine"
      , "obs"
      , "Xmessage"
      , "Octave"
      , "feh"
      , "Anki"
      ]
    myTFloats = ["Add Downloads", "Library","emacs-capture"]
    myRFloats = ["desktop_window"]
    myPTFloats = ["DownThemAll!", "AutoProxy", "Install user style","Ediff"]
    myPCFloats = []
    myRole = ["pop-up"]

-- xmobar
clickable ws =
  let index = elemIndex ws myWorkspaces
  in
    if isJust index
    then "<action=xdotool key Super_L+" ++ show(fromJust index + 1) ++ ">" ++ ws ++ "</action>"
    else ws


myPP  =
  namedScratchpadFilterOutWorkspacePP $
  xmobarPP
  {
    ppSep    = "  "
  , ppCurrent = xmobarColor "#74b9ff" "" . clickable
  , ppVisible = xmobarColor "#dfe6e9" "" . clickable
  , ppUrgent = xmobarColor "#ff7675" "" . clickable
  , ppHidden = clickable
  , ppWsSep  = "  "
  , ppTitle  = xmobarColor xmobarTitleColor "" . shorten 50
  , ppOrder  = \(ws:m:t:e) -> [ws,m] ++ e ++ [t]
  , ppSort   = DO.getSortByOrder
  , ppLayout = xmobarColor "#a29bfe" "" . wrap "| " " |"
  , ppExtras = [ willHookNextPP "float" $ xmobarColor "green" ""
               , willHookNextPP "sink" $ xmobarColor "red" ""
               , willHookAllNewPP "sink" $ xmobarColor "red" ""
               , willHookAllNewPP "float" $ xmobarColor "green" ""]
  }

myLogHook = multiPP myPP myPP
  >> workspaceHistoryHook
  >> updatePointer (0.9, 0.9) (0.9, 0.9)

barCreate (S 0) = spawnPipe $ "xmobar -x 0 ~/.xmonad/xmobar.hs"
barCreate (S sid) = spawnPipe $ "xmobar -x " ++ show sid ++ " ~/.xmonad/xmoside.hs"

barDestroy = return ()

-- fix firefox fullscreen
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

myStartupHook = setWMName "LG3D"
  <+> addEWMHFullscreen
  <+> setDefaultCursor xC_left_ptr
  <+> spawn "source $HOME/.fehbg"
  <+> spawn "tmux new-session -s dropdown -d"
  <+> spawn "$HOME/.xmonad/startup.sh"
  <+> dynStatusBarStartup barCreate barDestroy

myToggleHook = toggleHook "float" doFloat
               <+> toggleHook "sink" doSink

doSink :: ManageHook
doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)

myEventHook = handleEventHook def
          <+> fullscreenEventHook
          <+> docksEventHook
          <+> dynStatusBarEventHook barCreate barDestroy

main = do
  xmonad
    $ ewmh
    $ withUrgencyHook NoUrgencyHook
    $ dynamicProjects myProjects
      def
      { terminal = myTerminal
      , focusFollowsMouse = myFocusFollowsMouse
      , borderWidth = myBorderWidth
      , modMask = myModMask
      , workspaces = myWorkspaces
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , keys = myKeys
      , mouseBindings = myMouseBindings
      , layoutHook =  myLayout
      , handleEventHook = myEventHook
      , startupHook = myStartupHook
      , manageHook = myToggleHook <+> myManageHook
      , logHook = myLogHook
      }
