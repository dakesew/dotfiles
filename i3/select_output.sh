#!/usr/bin/env bash
#Displays a menu and runs xrandr to provide the selection
selection=$(rofi -p "Display mode:" -dmenu <<EOF
internal_only
home
HDMI2_only
internal_HDMI2
clone_HDMI2
EOF
)
case $selection in
internal_only)
	swaymsg output eDP-1 enable
	swaymsg output DP-3 disable
	swaymsg output DP-4 disable
	swaymsg output DP-5 disable
	swaymsg output DP-6 disable
	;;
home)
	swaymsg output DP-3  enable mode 1280x1024 pos 1280 0   transform normal
	swaymsg output DP-4  enable mode 1280x1024 pos 0 0      transform normal
	# Sometimes They change names 
	swaymsg output DP-5  enable mode 1280x1024 pos 1280 0   transform normal
	swaymsg output DP-6  enable mode 1280x1024 pos 0 0      transform normal
	swaymsg output eDP-1 enable mode 1920x1080 pos 320 1024 transform normal
	;;
HDMI2_only)
	xrandr --output eDP-1 --off
	xrandr --output HDMI2 --auto
	xrandr --output DP-1-1 --off
	xrandr --output DP-1-2 --off
	xrandr --output DP-1-3 --off
	;;
internal_HDMI2)
	xrandr --output eDP-1 --auto --primary
	xrandr --output HDMI2 --auto --right-of eDP-1
	xrandr --output DP-1-1 --off
	xrandr --output DP-1-2 --off
	xrandr --output DP-1-3 --off
	;;
clone_HDMI2)
	# 1024x768 is the lowest common denominator
	xrandr --output eDP-1 --primary --mode 1024x768
	xrandr --output HDMI2 --same-as eDP-1 --mode 1024x768
	xrandr --output DP-1-1 --off
	xrandr --output DP-1-2 --off
	xrandr --output DP-1-3 --off
	;;
*)
	echo error
	return 1
	;;
esac
# Apply background again
# feh --no-fehbg --bg-fill "$(cat /tmp/wallpaper)"
# Reload polybar so the tray position matches reality
# xrandr --dpi 96
