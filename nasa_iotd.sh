#!/bin/bash
 
# grabs the nasa image of the day by RSS feed and updates the gnome
# background. add this to your cron jobs to have this happen daily.  this is,
# obviously, a hack, that is likely to break at the slightest change of NASA's
# RSS implementation. yay standards!

dldir=$HOME/Downloads/iotd/

rss=`wget -q -O - http://www.nasa.gov/rss/lg_image_of_the_day.rss`
img_url=`echo $rss | grep -o '<enclosure [^>]*>' | grep -o 'http://[^\"]*'`
img_name=`echo $img_url | grep -o [^/]*\.\w*$`

# this command is benign if the directory already exists.
mkdir -p $dldir
     
# this command will overwrite the image if it already exists
wget -q -O $dldir/$img_name $img_url

# and then use awsetbg to set the background
feh --bg-fill $dldir/$img_name
