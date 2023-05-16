#!/bin/bash

pkill polybar
pkill stalonetray

while true; do
    xrandr --current | grep '5120x2880' && break
    sleep 1
done

sleep 1
polybar -r topbar &
stalonetray &
sleep 1
copyq&
