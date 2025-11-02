#!/bin/bash

pkill polybar
pkill xfce4-panel

# autorandr -c
# xrandr --output HDMI-0 --scale 1.3333

while true; do
    output=$(xrandr --current)
    if echo "$output" | grep -q 'DP-4 connected primary 3413x2133+0+0 (normal left inverted right x axis y axis) 345mm x 215mm'
    then
        export POLYBAR_DPI=192
        # command=$'polybar -r topbar &\nxfce4-panel --disable-wm-check &\n'
        command=$'xfce4-panel --disable-wm-check &\n'
        break
    elif echo "$output"  | grep -q 'DP-2 connected primary 5120x2880+0+0 (normal left inverted right x axis y axis) 609mm x 349mm'
    then
        export POLYBAR_DPI=192
        # command=$'polybar -r topbar &\nxfce4-panel --disable-wm-check &\n'
        command=$'xfce4-panel --disable-wm-check &\n'
        break
    elif echo "$output"  | grep -q 'HDMI-0 connected primary 5120x2880+0+0 (normal left inverted right x axis y axis) 597mm x 335mm'
    then
        export POLYBAR_DPI=192
        # command=$'polybar -r topbar &\nxfce4-panel --disable-wm-check &\n'
        command=$'xfce4-panel --disable-wm-check &\n'
        break
    elif echo "$output"  | grep -q 'DP-2 connected primary 5120x2880+0+0 (normal left inverted right x axis y axis) 597mm x 335mm'
    then
        export POLYBAR_DPI=192
        # command=$'polybar -r topbar &\nxfce4-panel --disable-wm-check &\n'
        command=$'xfce4-panel --disable-wm-check &\n'
        break
    elif echo "$output" | grep -q 'eDP-1 connected primary 1920x1080+0+0 (normal left inverted right x axis y axis) 309mm x 174mm'
    then
        export POLYBAR_DPI=96
        # command=$'polybar -r topbar &\nxfce4-panel --disable-wm-check &\n'
        command=$'xfce4-panel --disable-wm-check &\n'
        break
    else
        export POLYBAR_DPI=192
        # command=$'polybar -r topbar &\nxfce4-panel --disable-wm-check &\n'
        command=$'xfce4-panel --disable-wm-check &\n'
        break
    fi
done

sleep 1

eval $command
