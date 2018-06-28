#!/usr/bin/env bash
#Displays a menu and runs xrandr to provide the selection
selection=$(rofi -p "Display mode:" -dmenu <<EOF
internal_only
HDMI1_only
internal_HDMI1
clone_HDMI1
EOF
)
case $selection in
internal_only)
	xrandr --output eDP-1 --auto
	xrandr --output HDMI-1 --off
	;;
HDMI1_only)
	xrandr --output eDP-1 --off
	xrandr --output HDMI-1 --auto
	;;
internal_HDMI1)
	xrandr --output eDP-1 --auto --primary
	xrandr --output HDMI-1 --auto --right-of eDP-1
	;;
clone_HDMI1)
	# 1024x768 is the lowest common denominator
	xrandr --output eDP-1 --primary --mode 1024x768
	xrandr --output HDMI-1 --same-as eDP-1 --mode 1024x768
	;;
*)
	echo error
	return 1
	;;
esac
# Apply background again
feh --no-fehbg --bg-fill "$(cat /tmp/wallpaper)"
