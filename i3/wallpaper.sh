#!/bin/sh
OLD_BG=$(pidof swaybg)
while :
do
    wallpaper=$(shuf -n1 -e ~/usr/img/wallpapers/X*/*)
    # Adapted from https://github.com/swaywm/sway/issues/3659#issuecomment-463427809
    swaybg -i "$wallpaper" -m fill &
    NEXT_BG=$!
    sleep 0.1
    kill $OLD_BG
    echo "$wallpaper" > /tmp/wallpaper
    OLD_BG=$NEXT_BG
    sleep $1
done
