#/run/current-system/sw/bin/env bash

check_process() {
    if ! pgrep $1 > /dev/null ; then
        $2
        echo $1 started...
    fi
}

if [[ $(hostname) == 'nixos' ]]; then
    xrandr --output DVI-D-0 --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-0 --off --output DP-5 --off --output DP-4 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-3 --mode 1920x1080 --pos 3840x0 --rotate normal --output DP-2 --off --output DP-1 --off --output DP-0 --off
    # sync_api='q4q6mjnPfKsgE79ASx43bbAxtYKeCTgK'
    # polybar main  &
    # polybar left  &
    # polybar right &
    xset s off -dpms
else
    # sync_api='TpD3z5RWDsoDPjyQDzLkZHHQw3TLjUGw'
    # MONITOR=eDP-1 polybar thinkpad &
    echo "laptop..."
fi

check_process "unclutter" "unclutter -b"
check_process "compton" "compton -b"
check_process "urxvtd" "urxvtd -f"
check_process "emacs" "emacs --daemon "
check_process "xkeysnail" "sudo xkeysnail -q /home/peterzky/.config/xkeysnail/config.py"

tmux new-session -s dropdown -d
