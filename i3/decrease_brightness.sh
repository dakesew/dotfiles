#! /bin/sh
if [[ "xbacklight -get" < 8 ]]; then
    echo "Under 8"
else
    echo "Not under 8"
fi
