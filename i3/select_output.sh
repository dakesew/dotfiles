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
	xrandr --output eDP-1 --auto --output DP-1-3 --off --output DP-1-2 --off --output DP-1-1 --off --output DP-1 --off
	;;
home)
	xrandr --output DP-1-2 --mode 1280x1024 --pos 1280x0 --rotate normal
	xrandr --output DP-1-3 --mode 1280x1024 --pos 0x0 --rotate normal
	xrandr --output eDP-1 --primary --mode 1920x1080 --pos 320x1024 --rotate normal
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
feh --no-fehbg --bg-fill "$(cat /tmp/wallpaper)"
# Reload polybar so the tray position matches reality
killall -USR1 polybar
xrandr --dpi 96
xinput map-to-output "Wacom ISDv4 EC Pen stylus" eDP-1
xinput map-to-output "Wacom ISDv4 EC Pen eraser" eDP-1
xinput map-to-output "ELAN Touchscreen" eDP-1
