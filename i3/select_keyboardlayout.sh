#!/usr/bin/env bash
#Displays a menu and runs xrandr to provide the selection
selection=$(rofi -p "Keyboard Layout:" -dmenu <<EOF
de
de neo
en
EOF
	 )
setxkbmap $selection
