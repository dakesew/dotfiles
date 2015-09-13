#!/bin/sh

while :
do
    feh --bg-fill "$(shuf -n1 -e $(shuf -n1 -e ~/.i3/Wallpapers/X*/ ~/.i3/Wallpapers/XX*/)*)"
    sleep $1
done
