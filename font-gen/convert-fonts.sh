#!/bin/sh

ALPHABET="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
for (( i=0; i<${#ALPHABET}; i++ )); do
  CHAR=${ALPHABET:${i}:1}
  convert -font ./kenpixel_square.ttf -background transparent -fill white -size 200x200 -pointsize 200 -gravity Center label:"$CHAR" font_${CHAR}.png
done

