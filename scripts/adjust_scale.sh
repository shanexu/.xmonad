#!/bin/bash

HOST=$(hostname)

if [ "$HOST" = "shanes-archlaptop" ]; then
	(xrandr --listactivemonitors | grep DP-4) && xrandr --output DP-4 --scale 1.5
else
	(xrandr --listactivemonitors | grep DP1) && xrandr --output DP1 --scale 1.33
	(xrandr --listactivemonitors | grep DP-2) && xrandr --output DP-2 --scale 1.33
fi

