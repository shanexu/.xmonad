#!/bin/bash

pkill polybar
pkill stalonetray

autorandr -c

while true; do
    output=$(xrandr --current)
    if echo "$output" | grep -q 'DP-4 connected primary 3413x2133+0+0 (normal left inverted right x axis y axis) 345mm x 215mm'
    then
        command=$'polybar -r topbar &\nstalonetray --geometry 1x1+2850+0 &\n'
        break
    elif echo "$output"  | grep -q 'DP-2 connected primary 5120x2880+0+0 (normal left inverted right x axis y axis) 609mm x 349mm'
    then
        command=$'polybar -r topbar &\nstalonetray --geometry 1x1-535+0 &\n'
        break
    else
        sleep 1
    fi
done

sleep 1

eval $command
