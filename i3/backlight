#!/bin/sh
brightness=$(light -rG)
echo $brightness
if [ $brightness = 3125 ]
then
    light -rS 0
elif [ $brightness = 2343 ]
then
    light -rS 3125
elif [ $brightness = 1562 ]
then
     light -rS 2343
elif [ $brightness = 781 ]
then
    light -rS 1562
else
    light -rS 781
fi
