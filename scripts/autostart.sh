#!/bin/bash


PID=$(pgrep polybar)

if [ -z $PID ]; then
    sleep 3
    polybar -r topbar -r &
else
    polybar-msg cmd restart
fi

$HOME/.xmonad/scripts/adjust_scale.sh
