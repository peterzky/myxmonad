{-# OPTIONS_GHC-fno-warn-missing-signatures -fno-warn-type-defaults #-}
import           XMonad

import           System.Exit
import           System.IO

import           Data.List

import           XMonad.Config.Xfce
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Util.Cursor
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                  (spawnPipe)
import           XMonad.Util.WorkspaceCompare

import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.UpdatePointer

import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing

import           XMonad.Prompt.XMonad

import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W

projects =
  [ Project { projectName = "haskell"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "emt ~/playground/haskell/hello.hs"
                                           spawn "zathura ~/Nextcloud/books/Christopher Allen, Julie Moronuki-Haskell Programming from first principles (2016).pdf"
            }
  ]

systemPromptCmds =
  [ ("Shutdown", spawn "sudo systemctl poweroff")
  , ("Reboot", spawn "sudo systemctl reboot")
  , ("Exit", io exitSuccess)
  , ("Hibernate", spawn "sudo systemctl hibernate")
  , ("Restart", restart "xmonad" True)
  ]

myScratchPads =
  [ NS "fileManager" "thunar" (className =? "Thunar")
    (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  , NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol") defaultFloating
  , NS "cloud" "nextcloud" (className =? "Nextcloud") defaultFloating
  , NS "mpv" "" (className =? "mpv") defaultFloating
  , NS "org"
       "emacsclient -c -F '((name . \"org-agenda\") (alpha . (85 . 85)))' -e '(progn (org-todo-list)(delete-other-windows))'"
       (title =? "org-agenda")
       (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  , NS "music" "urxvtc -title musicbox -e musicbox"
      (title =? "musicbox")
      (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  , NS "htop" "urxvtc -title htop -e htop"
      (title =? "htop")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "nm" "urxvtc -title nmtui -e nmtui"
      (title =? "nmtui")
      (customFloating $ W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3))
  , NS "pamix" "urxvtc -title pamix -e pamix"
      (title =? "pamix")
      (customFloating $ W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3))
  , NS "term" "urxvtc -title term"
      (title =? "term")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "ncmpcpp" "urxvtc -title ncmpcpp -e ncmpcpp"
      (title =? "ncmpcpp")
      (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  , NS "weechat" "urxvtc -title weechat -e weechat"
      (title =? "weechat")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "mutt" "urxvtc -title mutt -e mutt"
      (title =? "mutt")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "ranger" "urxvtc -title rangerfloat -e ranger"
      (title =? "rangerfloat")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]

myTerminal = "urxvtc"

myFocusFollowsMouse = True

myBorderWidth = 2

xmobarTitleColor = "#ababab"

myNormalBorderColor = "#000000"

myFocusedBorderColor = "#90C695"

mySpeaker = "alsa_output.usb-Harman_Multimedia_JBL_Pebbles_1.0.0-00.analog-stereo"

myHeadset = "alsa_output.usb-Creative_Technology_Ltd_SB_X-Fi_Surround_5.1_Pro_000003XO-00.analog-stereo"

myModMask = mod4Mask

myWorkspaces = withScreens 3 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myLauncher = "rofi -show run"

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
     -- Basic
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((modm, xK_space), spawn myLauncher)
  , ((modm, xK_q), kill)
  , ((modm .|. shiftMask, xK_q), io exitSuccess)
  , ((modm, xK_grave), sendMessage NextLayout)
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  , ((modm, xK_n), refresh)
  , ((modm, xK_Tab), toggleWS)
  , ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_m), windows W.focusMaster)
  , ((modm, xK_Return), windows W.swapMaster)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_t), withFocused $ windows . W.sink)
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Applications
  , ((modm .|. shiftMask, xK_r) , spawn "killall xmobar; xmonad --recompile; xmonad --restart")
  , ((modm, xK_f), namedScratchpadAction myScratchPads "ranger")
  , ((modm, xK_e), namedScratchpadAction myScratchPads "music")
  , ((modm .|. shiftMask, xK_h), namedScratchpadAction myScratchPads "htop")
  , ((modm .|. shiftMask, xK_n), namedScratchpadAction myScratchPads "nm")
  , ((modm .|. shiftMask, xK_e), namedScratchpadAction myScratchPads "ncmpcpp")
  , ((modm .|. shiftMask, xK_i), namedScratchpadAction myScratchPads "mutt")
  , ((modm .|. shiftMask, xK_f), namedScratchpadAction myScratchPads "fileManager")
  , ((modm, xK_w), spawn "emacsclient -nc")
  , ((modm, xK_c), spawn "conkeror")
  , ((modm, xK_z), namedScratchpadAction myScratchPads "org")
  , ((modm .|. shiftMask, xK_v), namedScratchpadAction myScratchPads "mpv")
  , ((modm, xK_v), namedScratchpadAction myScratchPads "pamix")
    -- Volume control
  , ((0, xK_F12), spawn $ "pactl set-sink-volume " ++ mySpeaker ++ " +3%")
  , ((0, xK_F11) , spawn $ "pactl set-sink-volume " ++ mySpeaker ++ " -3%")
  , ((modm, xK_F12), spawn $ "pactl set-sink-volume " ++ myHeadset ++ " +3%")
  , ((modm, xK_F11), spawn $ "pactl set-sink-volume " ++ myHeadset ++ " -3%")
   -- Screenshots
  , ((0, xK_Print), spawn "scrot -s ~/Nextcloud/Screenshots/Screenshot%Y-%m-%d%H:%M:%S.png")
  , ((modm, xK_Print), spawn "scrot -u ~/Nextcloud/Screenshots/Screenshot%Y-%m-%d%H:%M:%S.png")
   -- System Prompt
   ,((0, xK_Pause), xmonadPromptC systemPromptCmds def)
   -- ,((modm, xK_slash), shiftToProjectPrompt)

  ] ++
    -- Workspaces
  [ ((m .|. modm, k), windows $ onCurrentScreen f i)
       | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
    -- Monitors
  [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_s, xK_a, xK_d] [0 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ( (modm, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- set mouse side button to float and resize
    , ((0, 8), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((0, 9), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myLayout =
   myTiled ||| myMirror ||| myFull
  where
    myTiled = renamed [XMonad.Layout.Renamed.Replace "T"]
      . smartSpacing 2 $ Tall 1 (3 / 100) (1 / 2)
    myMirror = renamed [XMonad.Layout.Renamed.Replace "M"]
       $ Mirror myTiled

    myFull = renamed [XMonad.Layout.Renamed.Replace "F"]
       $ Full

myManageHook =
  composeAll . concat $
  [ [manageDocks]
  , [isFullscreen --> doFullFloat]
  , [className =? "Xfce4-notifyd" --> doIgnore]
  , [className =? "Nextcloud" --> doShift "NSP"]
  , [isDialog --> doCenterFloat]
  , [className =? c --> doCenterFloat | c <- myCFloats]
  , [title =? t --> doFloat | t <- myTFloats]
  , [role =? t --> doFloat | t <- myRole]
  , [resource =? r --> doFloat | r <- myRFloats]
  , [fmap (pt `isInfixOf`) title --> doFloat | pt <- myPTFloats]
  , [fmap (pc `isInfixOf`) className --> doFloat | pc <- myPCFloats]
  , [namedScratchpadManageHook myScratchPads]
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    myCFloats =
      [ "mpv"
      , "Lxappearance"
      , "GoldenDict"
      , "Pavucontrol"
      , "File-roller"
      , "Gimp"
      , "VirtualBox"
      , "Gpicview"
      , "octave-gui"
      , "Gnuplot"
      , "Wine"
      , "Zeal"
      , "obs"
      , "Thunderbird"
      , "Octave"
      , "feh"
      , "Anki"
      ]
    myTFloats = ["Add Downloads", "Library","emacs-capture"]
    myRFloats = ["desktop_window"]
    myPTFloats = ["DownThemAll!", "AutoProxy", "Install user style","Ediff"]
    myPCFloats = []
    myRole = ["pop-up"]

myPP h =
  xmobarPP
  { ppOutput = hPutStrLn h
  , ppSep = "  "
  , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
  , ppSort = getSortByXineramaRule
  , ppLayout = xmobarColor "#CEFFAC" ""
  }

myLogHook h0 h1 h2 =
  let bar screen =
        dynamicLogWithPP .
        namedScratchpadFilterOutWorkspacePP . marshallPP screen . myPP
  in bar 0 h0 >> bar 1 h1 >> bar 2 h2 >> updatePointer (0.9, 0.9) (0.9, 0.9)

myStartupHook =
  setWMName "LG3D" <+>
  -- spawn "compton -fcC -t-3 -l-5 -r4 --config /dev/null --backend xrender --unredir-if-possible" <+>
  -- spawn "urxvtd" <+>
  -- spawn "emacs --daemon" <+>
  spawn "source ~/.fehbg"

main = do
  h0 <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobar.hs"
  h1 <- spawnPipe "xmobar -x 1 ~/.xmonad/xmoside.hs"
  h2 <- spawnPipe "xmobar -x 2 ~/.xmonad/xmoside.hs"
  xmonad $ ewmh def
      { terminal = myTerminal
      , focusFollowsMouse = myFocusFollowsMouse
      , borderWidth = myBorderWidth
      , modMask = myModMask
      , workspaces = myWorkspaces
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , keys = myKeys
      , mouseBindings = myMouseBindings
      , layoutHook = smartBorders $ avoidStruts $ myLayout
      , handleEventHook = mempty <+> docksEventHook <+> fullscreenEventHook
      , startupHook = myStartupHook
      , manageHook = myManageHook <+> manageHook xfceConfig
      , logHook = myLogHook h0 h1 h2
      }
