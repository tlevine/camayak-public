#!/bin/sh
set -e

if ls /tmp/camayak-*.png 2> /dev/null; then
  rm -f /tmp/frame*.png
  i=1
  for old in /tmp/camayak-*.png; do
    new=$(printf /tmp/frame%04d.png $i)
    mv $old $new
    i=$(($i + 1))
  done
fi

mkdir -p /tmp/web
#avconv=./ffmpeg-*/ffmpeg
avconv=ffmpeg
$avconv -r 2 -i /tmp/frame%04d.png -i /tmp/camayak.wav \
        -y -pix_fmt yuv420p -r 2 \
        -strict -2 \
        /tmp/web/camayak.webm
