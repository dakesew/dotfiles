#!/bin/sh
brightness=$(light -rG)
max_brightness=$(light -rm)
brightness_step=$(expr $max_brightness / 5)
echo $brightness
if [ $brightness -eq $max_brightness ]
then
    light -rS 1
elif [ $(expr $brightness_step '*' 4) -eq $brightness ]
then
    light -rS $max_brightness
elif [ $(expr $brightness_step '*' 3) -eq $brightness ]
then
     light -rS $(expr $brightness_step '*' 4)
elif [ $(expr $brightness_step '*' 2) -eq $brightness ]
then
     light -rS $(expr $brightness_step '*' 3)
elif [ $brightness = $brightness_step ]
then
    light -rS $(expr $brightness_step '*' 2)
else
    light -rS $brightness_step
fi
