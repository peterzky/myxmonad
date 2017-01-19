--
-- xmonad example config file for xmonad-0.9
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- NOTE: Those updating from earlier xmonad versions, who use
-- EwmhDesktops, safeSpawn, WindowGo, or the simple-status-bar
-- setup functions (dzen, xmobar) probably need to change
-- xmonad.hs, please see the notes below, or the following
-- link for more details:
--
-- http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8
-- hell
import System.IO 
import XMonad
import Data.Monoid
import Data.List
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Util.SpawnOnce
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.UpdatePointer 
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell

  
import qualified XMonad.StackSet as W
import qualified Data.Map        as M



systemPromptCmds = [
        ("Shutdown", spawn "sudo systemctl poweroff"),
        ("Reboot", spawn "sudo systemctl reboot"),
        ("Exit", io $ exitWith ExitSuccess),
        ("Hibernate", spawn "sudo systemctl hibernate"),
        ("Restart", restart "xmonad" True)
    ]
                   



-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
-- Width of the window border in pixels.
--
myBorderWidth   = 0
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------
 
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = withScreens 3 ["1","2","3","4","5","6","7","8","9"]
 
-- Border colors for unfocused and focused windows, respectively.
--
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modm,               xK_space ), spawn "rofi -run-command \"/bin/zsh -i -c '{cmd}'\" -hide-scrollbar -font \"Bitstream Vera Sans Mono 12\" -show run")
    -- launch nautilus
    , ((modm,               xK_f     ), spawn "nautilus")
 
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window
    , ((modm .|. shiftMask, xK_r     ), spawn "killall xmobar; xmonad --recompile; xmonad --restart")
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_p     ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Volume control
    , ((0                 , xK_F12   ) , spawn "pactl set-sink-volume 1 +5%")
    , ((0                 , xK_F11   ) , spawn "pactl set-sink-volume 1 -5%")
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), kill)



    , ((0                 , xK_Pause  ),
                    xmonadPromptC systemPromptCmds defaultXPConfig)
    -- Copy and Paste
    -- , ((modm              , xK_v     ), pasteSelection)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
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
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = 
  Tall 1 (3/100) (1/2) |||
  Mirror (Tall 1 (3/100) (1/2)) |||
  Full
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

myManageHook = composeAll . concat $
   [[ className =? "mpv"        --> doFloat
    , className =? "Lxappearance" --> doFloat
    , className =? "GoldenDict" --> doFloat
    , title =? "Library" --> doFloat
    , title =? "Add Downloads" --> doFloat
    , className =? "Nautilus" --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat]
   , [ fmap ( c `isInfixOf`) title     --> doFloat | c <- ["DownThemAll!","AutoProxy"]]
   ]
 
------------------------------------------------------------------------
-- Event handling
 
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = mempty
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook =
      spawn "source ~/.fehbg"
          <+> spawnOnce "compton -fcC"
          <+> setDefaultCursor xC_left_ptr
          <+> spawnOnce "sleep 1;xcape -e 'Control_L=Escape'"
          <+> spawnOnce "goldendict"
          <+> spawn "fcitx"
          -- <+> spawn "xbindkeys"
 
------------------------------------------------------------------------
  -- Xmobar config
xmobarTitleColor = "#3399ff"
xmobarCurrentWorkspaceColor = "#CEFFAC"
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

myLogHook h = dynamicLogWithPP $ xmobarPP
            {
              ppOutput = hPutStrLn h
            , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
            } 

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
-- main = xmonad =<< statusBar myBar myPP toggleStrutsKey  defaults
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"

  xmonad $ ewmh defaults {
            logHook = myLogHook xmproc
                      >> updatePointer (0.9,0.9) (0.9,0.9)
          , manageHook = manageDocks <+> myManageHook 
          }
    
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = avoidStruts $ smartSpacing 2 $ myLayout,
        handleEventHook    = myEventHook <+> fullscreenEventHook <+> docksEventHook,
        startupHook        = myStartupHook
    }
