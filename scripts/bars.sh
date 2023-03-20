#!/bin/bash

pkill polybar
ps aux | grep '[g]et_spotify' | awk '{print $2}' | xargs kill -9
ps aux | grep '[p]olywins' | awk '{print $2}' | xargs kill -9

sleep 5
polybar -r topbar &

sleep 1

pkill stalonetray
stalonetray &
