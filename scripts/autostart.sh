#!/bin/bash

sleep 1

xrandr --listactivemonitors | grep DP1 && xrandr --output DP1 --scale 1.33
xrandr --listactivemonitors | grep DP-2 && xrandr --output DP-2 --scale 1.33
# xrandr --listactivemonitors | grep DP-2 && xrandr --output DP-2 --scale 1.6

# setxkbmap -device $(xinput list --id-only keyboard:'ITE Tech. Inc. ITE Device(8910) Keyboard') -option ctrl:swapcaps -option altwin:swap_alt_win


# feh --bg-fill /home/shane/OneDrive/Wallpapers/1009895.jpg
# feh --bg-fill /home/shane/OneDrive/Wallpapers/jack-zhong-OzD_vkpMM5Y-unsplash.jpg


# killall xcompmgr
# xcompmgr &

killall polybar
polybar -r &

