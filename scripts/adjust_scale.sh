#!/bin/bash

(xrandr --listactivemonitors | grep DP1) && xrandr --output DP1 --scale 1.33
(xrandr --listactivemonitors | grep DP-2) && xrandr --output DP-2 --scale 1.33

