#!/bin/sh
brightness=$(light -rG)
#max_brightness=$(light -rm)
max_brightness=204
brightness_step=$(expr $max_brightness / 10)
echo $brightness
if [ $brightness -eq $max_brightness ]
then
    light -rS 0
elif [ $(expr $brightness_step '*' 6) -eq $brightness ]
then
    light -rS $max_brightness
elif [ $(expr $brightness_step '*' 3) -eq $brightness ]
then
     light -rS $(expr $brightness_step '*' 6)
elif [ $(expr $brightness_step '*' 2) -eq $brightness ]
then
     light -rS $(expr $brightness_step '*' 3)
elif [ $brightness = $brightness_step ]
then
    light -rS $(expr $brightness_step '*' 2)
elif [ $brightness -eq 0 ]
then
    light -rS 1
else
    light -rS $brightness_step
fi
