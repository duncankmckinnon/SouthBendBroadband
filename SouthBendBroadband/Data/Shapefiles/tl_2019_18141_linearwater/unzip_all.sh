#!/bin/bash
files=$(find . -name "*.zip")
for i in $files; do 
  dir=../Temp/$(echo $i | awk '{gsub(/.zip/,"")}1') #remove .zip from from the filename for directory
  mkdir -p "$dir"
  unzip $i -d "$dir"; done