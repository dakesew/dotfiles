#!/bin/sh

while :
do
    wallpaper=$(shuf -n1 -e ~/usr/img/wallpapers/X*/*)
    feh --no-fehbg --bg-fill "$wallpaper"
    echo "$wallpaper" > /tmp/wallpaper
    sleep $1
done
