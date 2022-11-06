#!/bin/bash

pgrep -f day-night-switcher | xargs kill -9

~/src/github.com/shanexu/day-night-switcher/day-night-switcher &
