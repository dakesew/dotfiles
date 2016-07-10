#!/bin/sh

while :
do
    #wallpaper=$(shuf -n1 -e $(shuf -n1 -e ~/.i3/Wallpapers/X*/ ~/.i3/Wallpapers/XX*/)*)
    wallpaper=$(shuf -n1 -e ~/.i3/Wallpapers/X*/*)
    feh --no-fehbg --bg-fill "$wallpaper"
    echo "$wallpaper" > /tmp/wallpaper
    sleep $1
done
