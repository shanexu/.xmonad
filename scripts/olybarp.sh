#!/bin/bash

HOST=$(hostname)

if [ "$HOST" = "shanes-archlaptop" ]; then
    monitor_layout=$(autorandr --detected)
    if [ "$monitor_layout" = "home_dell" ]; then
        export TOPRIGHT_WIDTH='11.3%'
        export TOPRIGHT_OFFSET_X='88.7%'
        export TOPBAR_WIDTH='88.7%'
    elif [ "$monitor_layout" = "office_redmi_dp" ]; then
        export TOPRIGHT_WIDTH='11.3%'
        export TOPRIGHT_OFFSET_X='88.7%'
        export TOPBAR_WIDTH='88.7%'
    else
        export TOPRIGHT_WIDTH='15%'
        export TOPRIGHT_OFFSET_X='85%'
        export TOPBAR_WIDTH='85%'
    fi
else
    export TOPRIGHT_WIDTH='11.3%'
    export TOPRIGHT_OFFSET_X='88.7%'
    export TOPBAR_WIDTH='88.7%'
fi

pgrep polybar | xargs kill -9
ps aux | grep '[g]et_spotify' | awk '{print $2}' | xargs kill -9
ps aux | grep '[p]olywins' | awk '{print $2}' | xargs kill

polybar -r topbar &
polybar -r topright &
