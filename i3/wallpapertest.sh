#!/bin/sh

while :
do
feh --bg-fill "$(shuf -n1 -e ~/.i3/Wallpapers/X*/* $(shuf -n1 -e ~/.i3/Wallpapers/X*/*)"
sleep $1
done