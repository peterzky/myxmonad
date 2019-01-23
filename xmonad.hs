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
import XMonad.Actions.OnScreen
import XMonad.Actions.CycleWorkspaceByScreen
import XMonad.Actions.DynWG

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ToggleHook
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Hooks.PositionStoreHooks

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize
import XMonad.Layout.StateFull (focusTracking)
import XMonad.Layout.Maximize
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows hiding (Replace)
import XMonad.Layout.LayoutHints
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Cross
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace

import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Layout
import XMonad.Prompt.FuzzyMatch

import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Dmenu (menuArgs)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Font

import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W


rofiWithWorkspace prompt job = do
  ws <- gets (W.workspaces . windowset)
  sort <- getSortByIndex
  t <- menuArgs "rofi" ["-dmenu", "-i","-p", prompt]
    $ filter (\w -> w /= "NSP") $ map W.tag $ sort ws
  when (t /= "") $ job t

rofiGoto = do
  ws <- gets (W.workspaces . windowset)
  sort <- getSortByIndex
  s <- gets windowset
  t <- menuArgs "rofi" ["-dmenu", "-i","-p","switch"]
    $ filter (\w -> w /= "NSP") $ map W.tag $ sort ws
  when (t /= "") $ if W.tagMember t s
    then windows $ W.greedyView t
    else addWorkspace t

myProjects =
  [ Project { projectName = "MSG"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "$HOME/.bin/rofi-msg.sh"
            }
  , Project { projectName = "GEN"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                runInTerm "urxvtc" "zsh -c 'neofetch; zsh'"
            }
  , Project { projectName = "ENV"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "$HOME/.bin/rofi-env.sh"
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
            , projectStartHook = Nothing
            -- , projectStartHook = Just $ do
            --     spawn "$HOME/.bin/rofi-org.sh"
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

myScratchPads =
  [ NS "htop" "urxvtc -title htop -e htop"
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
  , NS "note"
       "emacsclient -c -F '((name . \"org-agenda\") (alpha . (85 . 85)))' -e '(org-capture nil \"c\")'"
       (title =? "org-agenda")
       (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  , NS "dropdown"
    " urxvtc -title dropdown -e zsh -c 'tmux has -t dropdown && exec tmux attach-session -d -t dropdown || exec tmux new-session -s dropdown'"
      (title =? "dropdown")
      (customFloating $ W.RationalRect 0 0.02 1 0.5)
  ]

myTerminal = "urxvtc"

myBorderWidth = 3

xmobarTitleColor = "#ababab"

myNormalBorderColor = "#282A36"

-- myFocusedBorderColor = "#ecf0f1"
myFocusedBorderColor = "#3498db"

myModMask = mod4Mask

myOrgCmd = "emacsclient -nc"

myWorkspaces = ["GEN","WEB","WRK","ORG","DOC","MSG","VOD","ENV","TOR","GAME"]

killAll = withAll (\w -> do (focus w) >> kill1)

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  -- Basic
  [ ((modm, xK_q), kill1)
  , ((modm, xK_n), refresh)
  , ((modm, xK_r), bindOn [("MSG", spawn "$HOME/.bin/rofi-msg.sh")
                          ,("WEB", spawn "$HOME/.bin/rofi-surfraw.sh")
                          ,("DOC", spawn "$HOME/.bin/rofi-doc.sh")
                          ,("ORG", spawn "$HOME/.bin/rofi-org.sh")
                          ,("ENV", spawn "$HOME/.bin/rofi-env.sh")
                          ,("", spawn "rofi -show run")])
  -- Window Bindings
  , ((modm, xK_j), focusDown)
  , ((modm, xK_k), focusUp)
  , ((modm, xK_m), focusMaster)
  , ((modm, xK_Return), promote)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_t), withFocused $ windows . W.sink)
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
  , ((modm, xK_grave), windows W.focusDown)
  , ((modm, xK_BackSpace), killAll)
  , ((modm .|. controlMask, xK_s), sinkAll)
  -- Max/Minimize
  , ((modm, xK_x), withFocused $ sendMessage . maximizeRestore)
  -- Sublayouts
  , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
  , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
  , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
  , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)

  , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
  , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

  , ((modm, xK_apostrophe), onGroup W.focusUp')
  , ((modm, xK_semicolon), onGroup W.focusDown')
  -- , ((modm .|. shiftMask, xK_semicolon), onGroup W.swapUp')
  -- Workspace Groups
  , ((modm, xK_Up), promptWSGroupAdd myPromptTheme "Name this group: ")
  , ((modm, xK_Right), promptWSGroupView myPromptTheme "Goto Group: ")
  , ((modm, xK_Left), promptWSGroupForget myPromptTheme "Delete Group: ")
  -- Workspace Bindings
  , ((modm, xK_Tab), cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_Tab xK_grave)
  , ((modm .|. shiftMask, xK_Return), bindOn [("WEB", spawn "firefox")
                                             ,("ORG", spawn myOrgCmd)
                                             ,("DOC", spawn myOrgCmd)
                                             ,("GAME", spawn "steam")
                                             ,("", spawn "urxvtc")])
  -- , ((modm, xK_w), selectWorkspace myPromptTheme)
  , ((modm, xK_w), rofiGoto)

  , ((modm .|. shiftMask, xK_w), rofiWithWorkspace "shift" (windows . W.shift))
  , ((modm .|. controlMask, xK_w), rofiWithWorkspace "copy" (windows . copy))
  , ((modm .|. mod1Mask, xK_w), rofiWithWorkspace "remove" removeEmptyWorkspaceByTag)
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
  -- , ((modm, xK_Left ), DO.moveTo Prev HiddenNonEmptyWS)
  -- , ((modm, xK_Right), DO.moveTo Next HiddenNonEmptyWS)
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
  , ((modm .|. controlMask, xK_v), toggleHookAllNew "mpv" >> runLogHook)
  , ((modm, xK_backslash), toggleHookNext "float" >> runLogHook)
  -- Submap
  , ((modm, xK_e), submap . M.fromList $
      [ ((0, xK_e), spawn "emacsclient -nc")
      , ((modm, xK_e), spawn "emacsclient -nc")
      , ((0, xK_f), spawn "firefox")
      , ((0, xK_c), spawn "chromium")
      , ((0, xK_h), namedScratchpadAction myScratchPads "htop")
      , ((0, xK_n), namedScratchpadAction myScratchPads "nm")
      , ((0, xK_r), namedScratchpadAction myScratchPads "arandr")
      , ((0, xK_v), spawn "$HOME/.bin/AudioSwitch.sh")
      , ((0, xK_w), spawn "$HOME/.bin/wacom.sh")
      ])
  -- Org Mode
  , ((modm, xK_o), submap . M.fromList $
      [ ((0, xK_i), spawn "emacsclient -e '(org-clock-in-last)'")
      , ((0, xK_o), spawn "emacsclient -e '(org-clock-out)'")
      , ((0, xK_q), spawn "emacsclient -e '(org-clock-cancel)'")

      , ((modm, xK_i), spawn "emacsclient -e '(org-clock-in-last)'")
      , ((modm, xK_o), spawn "emacsclient -e '(org-clock-out)'")
      , ((modm, xK_q), spawn "emacsclient -e '(org-clock-cancel)'")

      -- timers
      , ((0, xK_p), spawn "timer 25")
      , ((0, xK_5), spawn "timer 5")
      , ((0, xK_1), spawn "timer 10")
      , ((0, xK_2), spawn "timer 20")
      , ((0, xK_k), spawn "pkill timer")

      , ((modm, xK_p), spawn "timer 25")
      , ((modm, xK_5), spawn "timer 5")
      , ((modm, xK_1), spawn "timer 10")
      , ((modm, xK_2), spawn "timer 20")
      , ((modm, xK_k), spawn "pkill timer")

      ])
  -- Applications
  , ((modm .|. shiftMask, xK_r) , spawn "rofi -show-icons -combi-modi drun,run -show combi -modi combi")
  , ((modm, xK_f), namedScratchpadAction myScratchPads "ranger")
  , ((modm, xK_space), namedScratchpadAction myScratchPads "dropdown")
  , ((modm, xK_c), namedScratchpadAction myScratchPads "org")
  , ((modm, xK_g), spawn "rofi -show-icons -show window")
  , ((modm, xK_z), namedScratchpadAction myScratchPads "note")
  , ((modm .|. controlMask, xK_d), spawn "$HOME/.bin/dict.sh")
  , ((modm .|. controlMask, xK_r), spawn "tts -sel")
  -- Volume control
  , ((0, 0x1008FF13), spawn "$HOME/.bin/volume.sh up")
  , ((0, 0x1008FF11), spawn "$HOME/.bin/volume.sh down")
  , ((0, 0x1008FF12), spawn "$HOME/.bin/volume.sh mute")
  , ((modm, xK_Home), spawn "$HOME/.bin/AudioSwitch.sh speaker")
  , ((modm, xK_End), spawn "$HOME/.bin/AudioSwitch.sh headphone")
  , ((modm, xK_equal), spawn "$HOME/.bin/player_control.sh next")
  , ((modm, xK_minus), spawn "$HOME/.bin/player_control.sh prev")
  , ((modm, xK_0), spawn "$HOME/.bin/player_control.sh toggle")
  , ((modm .|. shiftMask, xK_0), spawn "playerctl -a play-pause")
  , ((modm .|. controlMask, xK_0), spawn "playerctl -a stop")
  -- Screenshots
  , ((0, xK_Print), spawn "$HOME/.bin/ScreenShot.sh")
  -- System Prompt
  , ((modm, xK_Pause), spawn "i3lock-fancy")
  , ((modm .|. shiftMask, xK_q), spawn "$HOME/.bin/rofi-system.sh")

  ] ++
  -- Workspaces
  zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
  zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
    ++
  zip (zip (repeat (modm .|. controlMask)) [xK_1..xK_9]) (map (withNthWorkspace copy) [0..])
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
    $ avoidStruts
    $ mkToggle (single REFLECTX)
    $ windowNavigation
    $ addTabs shrinkText myTheme
    $ subLayout [] Simplest
    $ mySpacing 4
    $ focusTracking
    $ maximize
    $ Tall 1 (3/100) (1/2)

myMirror = renamed [Replace "M"]
    $ avoidStruts
    $ mkToggle (single REFLECTX)
    $ windowNavigation
    $ addTabs shrinkText myTheme
    $ subLayout [] Simplest
    $ mySpacing 4
    $ focusTracking
    $ maximize
    $ Mirror
    $ Tall 1 (3/100) (1/2)

myFloat = renamed [Replace "F"]
    $ avoidStruts
    $ noBorders
    $ maximize
    $ floatingDeco
    $ borderResize
    $ positionStoreFloat
  where
    floatingDeco = noFrillsDeco shrinkText myTheme

myGame = renamed [Replace "G"]
    $ noBorders
    $ maximizeWithPadding 0
    $ floatingDeco
    $ borderResize
    $ positionStoreFloat
  where
    floatingDeco = noFrillsDeco shrinkText myTheme

myVideo = renamed [Replace "V"]
    $ avoidStruts
    $ layoutHintsWithPlacement (0.5,0.5)
    $ MosaicAlt M.empty

myCross = renamed [Replace "C"]
    $ avoidStruts
    $ noBorders
    $ simpleCross

myTab = renamed [Replace "B"]
    $ avoidStruts
    $ tabbed shrinkText myTheme

myLayout = smartBorders
   $ boringWindows
   $ onWorkspace "WRK" (myTiled ||| myMirror)
   $ onWorkspace "WEB" (myTab ||| myTiled)
   $ onWorkspace "VOD" myVideo
   $ onWorkspace "MSG" (myCross ||| myFloat)
   $ onWorkspace "GAME" myGame
   $ onWorkspace "ENV" myFloat
   $ onWorkspace "TOR" myFloat
   $ myTiled |||  myMirror  ||| myCross ||| myVideo
    ||| myTab ||| myFloat

myManageHook =
  composeAll . concat $
  [ [manageDocks]
  , [namedScratchpadManageHook myScratchPads]
  , [isFullscreen --> doFullFloat]
  , [isDialog --> doCenterFloat]
  , [className =? c --> doCenterFloat | c <- myCFloats]
  , [title =? t --> doCenterFloat | t <- myTFloats]
  , [className =? "Zathura" --> doShiftAndGo "DOC"]
  , [className =? "XMind ZEN" --> doShiftAndGo "DOC" ]
  , [className =? "Zeal" --> doShiftAndGo "DOC" ]
  , [className =? "qBittorrent" --> doShift "TOR"]
  -- , [className =? "VirtualBox Manager" --> doShift "ENV"]
  -- , [className =? "VirtualBox Machine" --> doShift "ENV"]
  , [className =? "Steam" --> doShift "GAME"]
  , [className =? "ieaseMusic" --> doSink]
  ]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    role = stringProperty "WM_WINDOW_ROLE"
    myCFloats =
      [ "File-roller"
      , "Dragon"
      , "octave-gui"
      , "Gnuplot"
      , "Wine"
      -- , "obs"
      , "Xmessage"
      , "Octave"
      , "feh"
      -- , "Anki"
      , "Pcmanfm"
      ]
    myTFloats = ["Add Downloads", "Library","emacs-capture"]

-- xmobar
clickable ws = "<action=$HOME/.bin/switch-ws.sh " ++ show(ws) ++ ">" ++ ws ++ "</action>"

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
  , ppSort   = getSortByIndex
  , ppLayout = xmobarColor "#a29bfe" "" . wrap "| " " |"
  , ppExtras = [ willHookNextPP "float" $ xmobarColor "green" ""
               , willHookNextPP "sink" $ xmobarColor "red" ""
               , willHookAllNewPP "sink" $ xmobarColor "red" ""
               , willHookAllNewPP "float" $ xmobarColor "green" ""
               , willHookAllNewPP "mpv" $ xmobarColor "yellow" ""]
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
  <+> spawn "dunst &"
  <+> spawn "$HOME/.xmonad/startup.sh"
  <+> dynStatusBarStartup barCreate barDestroy
  <+> addRawWSGroup "default" [(0,"WEB"),(1,"GEN"),(2,"WRK")]

myToggleHook = toggleHook "float" doFloat
               <+> toggleHook "sink" doSink
               <+> toggleHook' "mpv" myMpvFloat myMpvSink

myMpvSink :: ManageHook
myMpvSink = className =? "mpv" --> doShift "VOD"

myMpvFloat :: ManageHook
myMpvFloat = className =? "mpv" --> doFloat

doSink :: ManageHook
doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)

myEventHook = handleEventHook def
          <+> positionStoreEventHook
          <+> fullscreenEventHook
          <+> docksEventHook
          <+> dynStatusBarEventHook barCreate barDestroy

main = do
  xmonad
    $ ewmh
    $ withUrgencyHook NoUrgencyHook
    $ dynamicProjects myProjects
      def
      { terminal           = myTerminal
      , focusFollowsMouse  = True
      , borderWidth        = myBorderWidth
      , modMask            = myModMask
      , workspaces         = myWorkspaces
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , keys               = myKeys
      , mouseBindings      = myMouseBindings
      , layoutHook         = myLayout
      , handleEventHook    = myEventHook
      , startupHook        = myStartupHook
      , manageHook         = myToggleHook
                             <+> myManageHook
                             <+> positionStoreManageHook Nothing
      , logHook            = myLogHook
      }
