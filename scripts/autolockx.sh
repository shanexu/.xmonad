#!/bin/bash

pgrep xautolock | xargs kill -9
xautolock -time 5 -locker 'xlock -mode rain' -detectsleep &

