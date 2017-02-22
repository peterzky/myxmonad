
import           XMonad

import           System.Exit
import           System.IO

import           Data.List
-- import           Data.Monoid

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Util.Cursor
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                  (spawnPipe)
-- import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare

import           XMonad.Actions.UpdatePointer
-- import           XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS

import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
-- import XMonad.Layout.Fullscreen         -- fix fullscreen issue

-- import           XMonad.Prompt
-- import           XMonad.Prompt.Shell
import           XMonad.Prompt.XMonad


import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W



systemPromptCmds = [
        ("Shutdown"  , spawn "sudo systemctl poweroff"),
        ("Reboot"    , spawn "sudo systemctl reboot"),
        ("Exit"      , io $ exitWith ExitSuccess),
        ("Hibernate" , spawn "sudo systemctl hibernate"),
        ("Restart"   , restart "xmonad" True)
                   ]

myScratchPads = [ NS "fileManager" "nautilus"
                  (className =? "Nautilus")
                  defaultFloating
                , NS "cloud" "nextcloud"
                  (className =? "Nextcloud")
                  defaultFloating
                , NS "music" "xfce4-terminal -T musicbox -x musicbox"
                  (title =? "musicbox")
                  (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
                , NS "htop" "xfce4-terminal -T htop -x htop"
                  (title =? "htop")
                  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                , NS "nm" "xfce4-terminal -T nmtui -x nmtui"
                  (title =? "nmtui")
                  (customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3))
                , NS "term" "xfce4-terminal -T term"
                  (title =? "term")
                  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                , NS "ncmpcpp" "xfce4-terminal -T ncmpcpp -x ncmpcpp"
                  (title =? "ncmpcpp")
                  (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
                , NS "weechat" "xfce4-terminal -T weechat -x weechat"
                  (title =? "weechat")
                  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                ]
  -- where
  --   spawnFM = "nautilus"
  --   findFM  = className =? "Nautilus"
  --   manageFM = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

  --   spawnMU = "xfce4-terminal -T musicbox -x musicbox"
  --   findMU = title =? "musicbox"
  --   manageMU = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)



myTerminal :: [Char]
myTerminal          = "urxvtc"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth       = 0
myModMask           = mod4Mask
myWorkspaces        = withScreens 3 ["1","2","3","4","5","6","7","8","9"]
myLauncher          = "rofi -run-command \"/bin/zsh -i -c '{cmd}'\" -hide-scrollbar -font \"Bitstream Vera Sans Mono 12\" -show run"

-- Border colors for unfocused and focused windows, respectively.
--

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

     -- Basic

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_space ), spawn myLauncher)
    , ((modm              , xK_q     ), kill)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm,               xK_o     ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), toggleWS)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Applications
    -- , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    -- , ((modm,               xK_r     ), spawn "urxvtc -e ranger")
    , ((modm .|. shiftMask, xK_r      ), spawn "killall xmobar; xmonad --recompile; xmonad --restart")
    , ((0                 , xK_Pause  ), xmonadPromptC systemPromptCmds def)
    , ((modm              , xK_f      ), namedScratchpadAction myScratchPads "fileManager")
    , ((modm              , xK_e      ), namedScratchpadAction myScratchPads "music")
    , ((modm .|. shiftMask, xK_u      ), namedScratchpadAction myScratchPads "cloud")
    , ((modm .|. shiftMask, xK_h      ), namedScratchpadAction myScratchPads "htop")
    , ((modm .|. shiftMask, xK_n      ), namedScratchpadAction myScratchPads "nm")
    , ((modm              , xK_F1     ), namedScratchpadAction myScratchPads "term")
    , ((modm .|. shiftMask, xK_e      ), namedScratchpadAction myScratchPads "ncmpcpp")
    , ((modm .|. shiftMask, xK_i      ), namedScratchpadAction myScratchPads "weechat")
    , ((modm .|. shiftMask, xK_f      ), spawn "firefox")
    , ((modm              , xK_w      ), spawn "emacsclient -nc")
    -- , ((modm .|. shiftMask, xK_u      ), raise (className =? "Nextcloud"))

    -- , ((modm              , xK_z      ), namedScratchpadAction myScratchPads "dict")

    -- Volume control
    , ((0                 , xK_F12   )
      , spawn "pactl set-sink-volume alsa_output.usb-Harman_Multimedia_JBL_Pebbles_1.0.0-00.analog-stereo +3%")
    , ((0                 , xK_F11   )
      , spawn "pactl set-sink-volume alsa_output.usb-Harman_Multimedia_JBL_Pebbles_1.0.0-00.analog-stereo -3%")
    , ((modm              , xK_F12   )
      , spawn "pactl set-sink-volume alsa_output.usb-Creative_Technology_Ltd_SB_X-Fi_Surround_5.1_Pro_000003XO-00.analog-stereo +3%")
    , ((modm              , xK_F11   )
      , spawn "pactl set-sink-volume alsa_output.usb-Creative_Technology_Ltd_SB_X-Fi_Surround_5.1_Pro_000003XO-00.analog-stereo -3%")

    -- Screenshots
    , ((0                 , xK_Print )
      , spawn "scrot -s ~/Sync/Screenshots/Screenshot%Y-%m-%d%H:%M:%S.png")
    , ((modm              , xK_Print )
      , spawn "scrot -u ~/Sync/Screenshots/Screenshot%Y-%m-%d%H:%M:%S.png")


    ]

    ++
    -- Workspaces
    [((m .|. modm, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- Monitors
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_s, xK_a, xK_d] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- set mouse side button to float and resize
    , ((0,    8), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    , ((0,    9), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    ]

------------------------------------------------------------------------
-- Layouts:

myLayout = myTiled ||| myMirror ||| Full

  where
    myTiled = renamed [Replace "Tiled"] $ smartSpacing 2 $ Tall 1 (3/100) (1/2)
    myMirror = renamed [Replace "Mirror"] $ Mirror myTiled


------------------------------------------------------------------------
-- Window rules:

myManageHook = composeAll . concat $
   [ [manageDocks                                                         ]
   , [isFullscreen                     --> doFullFloat                    ]
   , [isDialog                         --> doFloat                        ]
   , [className =? c                   --> doFloat | c  <- myCFloats      ]
   , [title     =? t                   --> doFloat | t  <- myTFloats      ]
   , [resource  =? r                   --> doFloat | r  <- myRFloats      ]
   , [fmap ( pt `isInfixOf`) title     --> doFloat | pt <- myPTFloats     ]
   , [fmap ( pc `isInfixOf`) className --> doFloat | pc <- myPCFloats     ]
   , [namedScratchpadManageHook myScratchPads                             ]
   ]

  where
    myCFloats  = ["mpv","Lxappearance","GoldenDict","Pavucontrol","File-roller","Gimp"]
    myTFloats  = ["Add Downloads","Library"]
    myRFloats  = ["desktop_window"]
    myPTFloats = ["DownThemAll!","AutoProxy","Install user style"]
    myPCFloats = []

------------------------------------------------------------------------
-- Event handling


------------------------------------------------------------------------
-- Status bars and logging

xmobarTitleColor :: [Char]
xmobarTitleColor = "#3399ff"
-- xmobarCurrentWorkspaceColor :: [Char]
-- xmobarCurrentWorkspaceColor = "#CEFFAC"
myNormalBorderColor :: [Char]
myNormalBorderColor  = "#7c7c7c"

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#ffb6b0"

myPP h = xmobarPP {
              ppOutput = hPutStrLn h
            , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
            , ppSort = getSortByXineramaRule
            , ppLayout = xmobarColor "red" ""
                  }

myLogHook h0 h1 h2 =
  let log screen handle = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP . marshallPP screen . myPP $ handle
  in log 0 h0 >> log 1 h1 >> log 2 h2

------------------------------------------------------------------------
-- Startup hook

myStartupHook =
      spawn "source ~/.fehbg"
          <+> spawn "compton -fcC"
          <+> setDefaultCursor xC_left_ptr
          -- <+> spawn "sleep 1;xcape -e 'Control_L=Escape'"
          <+> spawn "fcitx"
          <+> spawn "urxvtd"
          <+> spawn "sogou-qimpanel"
          <+> spawn "goldendict"
          <+> spawn "dunst"
          <+> namedScratchpadAction myScratchPads "cloud"
          <+> spawn "systemctl --user start emacs.service"

------------------------------------------------------------------------


main = do
  h0 <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobar.hs"
  h1 <- spawnPipe "xmobar -x 1 ~/.xmonad/xmobar.hs"
  h2 <- spawnPipe "xmobar -x 2 ~/.xmonad/xmobar.hs"

  xmonad $ ewmh def {
      -- simple stuff
          terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

      -- key bindings
        , keys               = myKeys
        , mouseBindings      = myMouseBindings

      -- hooks, layouts
        , layoutHook         = avoidStruts $  myLayout
        , handleEventHook    = mempty <+> docksEventHook <+> fullscreenEventHook
        , startupHook        = myStartupHook
        , manageHook         = myManageHook
        , logHook            = myLogHook h0 h1 h2
                             >> updatePointer (0.9,0.9) (0.9,0.9)
          }


--
