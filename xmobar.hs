Config {
   -- appearance
       font            = "xft:Sarasa UI SC:size=10"
     , additionalFonts = ["xft:FontAwesome:size=10"]
     , alpha           = 180
     , textOffset      = -1
     , iconOffset      = -1
     , iconRoot        = "."
     , bgColor         = "black"
     , fgColor         = "#ABABAB"
     , position        = Top
   -- , border =       BottomB
   -- , borderColor =  "#646464"

   -- layout
   , sepChar  = "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "   %UnsafeStdinReader% } %pomodoro%  %org%  {%dynnetwork%    %cpu%   %coretemp%   %disku%    %date%   %default:Master%   %battery%   "

   -- general behavior
   , lowerOnStart     = True    -- send to bottom of window stack on start
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , allDesktops      = True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest     = False   -- choose widest display (multi-monitor)
   , persistent       = True    -- enable/disable hiding (True = disabled)
   , commands =
        [ Run UnsafeStdinReader
        , Run Com ".bin/xmobar-org.sh" [] "org" 20
        , Run Com "timer" ["-c"] "pomodoro" 10
        , Run Volume "default" "Master" [
                "-t", "<action=`pactl set-sink-volume @DEFAULT_SINK@ -2%` button=5><action=`pactl set-sink-volume @DEFAULT_SINK@ +2%` button=4><action=`pactl set-sink-mute @DEFAULT_SINK@ toggle` button=1><status> <volume>%</action></action></action>",
                "--",
                "-o", "<fn=1>\xf026</fn>",
                "-O", "<fn=1>\xf028</fn>",
                "-c", "#ababab",
                "-C", "#ababab"
                                        ] 20
        , Run DiskU [("/", "<fn=1>\xf0a0</fn>  <free>")]
         ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20

        -- , Run Mpris2 "cmus" ["-t", "<fn=1>\xf001</fn>  <artist> - <title>" ] 10

        , Run DynNetwork ["-t", "<fn=1>\xf102</fn> <tx>  <fn=1>\xf103</fn> <rx>"
                         ,"-S", "True"
                         ] 10

        , Run Cpu ["-t", "<fn=1>\xf108</fn> <total>%"
                  , "--Low"      , "50"
                  , "--High"     , "85"
                  , "--normal"   , "#f4b350"
                  , "--high"     , "#ec644b"
                  , "--width"    , "4"
                  ] 10


        -- cpu core temperature monitor
        , Run CoreTemp       [ "--template" , "<fn=1>\xf2c8</fn> <core0>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             -- , "--low"      , "#87d37c"
                             , "--normal"   , "#f4b350"
                             , "--high"     , "#ec644b"
                             , "--width"    , "4"
                             ] 50


        -- battery monitor
        , Run Battery        [ "--template" , "<acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#ec644b"
                             , "--normal"   , "#f4b350"
                             , "--high"     , "#87d37c"

                             , "--" -- battery specific options
                              -- discharging status
                             , "-o"	, "<fn=1>\xf240</fn>  <left>% (<timeleft>)"
                              -- AC "on" status
                             , "-O"	, "<fc=#dAA520><fn=1>\xf376</fn> <left></fc>"
                              -- charged status
                             , "-i"	, "<fc=#006000><fn=1>\xf1e6</fn></fc>"
                             ] 50

        -- time and date indicator
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run Date           "<fn=1>\xf073</fn> %_m-%_d (%a)   <fn=1>\xf017</fn>  %H:%M" "date" 30

        ]
   }
