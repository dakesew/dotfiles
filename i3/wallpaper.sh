#!/bin/sh

while :
do
    sleep $1
    feh --bg-fill "$(shuf -n1 -e $(shuf -n1 -e ~/.i3/Wallpapers/X*/ ~/.i3/Wallpapers/XX*/)*)"
done
