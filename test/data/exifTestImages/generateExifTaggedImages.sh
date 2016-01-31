#!/bin/bash
rm latLonOnly.jpg
rm dateTimeOnly.jpg
rm gpsDirectionOnly.jpg
cp -T untagged.jpg latLonOnly.jpg
cp -T untagged.jpg dateTimeOnly.jpg
cp -T untagged.jpg gpsDirectionOnly.jpg
exiv2 -m latLonOnly.metadata.txt latLonOnly.jpg
exiv2 -m dateTimeOnly.metadata.txt dateTimeOnly.jpg
exiv2 -m gpsDirectionOnly.metadata.txt gpsDirectionOnly.jpg
