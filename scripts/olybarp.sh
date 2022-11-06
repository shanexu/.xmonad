#!/bin/bash

pgrep polybar | xargs kill -9
ps aux | grep '[g]et_spotify' | awk '{print $2}' | xargs kill -9
ps aux | grep '[p]olywins' | awk '{print $2}' | xargs kill

polybar -r topbar &
polybar -r topright &
