#!/usr/bin/env sh

padded_day=$(date '+%d')
day=$((10#$padded_day))
session=$(<.session)
curl -s --cookie "session=${session}" https://adventofcode.com/2020/day/${day}/input -o input/day${padded_day}.txt && echo DONE
