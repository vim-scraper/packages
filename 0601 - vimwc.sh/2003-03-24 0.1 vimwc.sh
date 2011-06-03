#!/bin/sh
#
# vimwc.sh -  a small script that writes a vim :so file that :echos the number
#             of words and lines to the status bar.
#
# (C) <thsutton@utas.edu.au> 20030306
#

# Find the file names.
IN="$1"
#OUT=`tempfile`		# someone should fix this for which(1)
OUT=/tmp/vimwc.$$

# Get the numbers.
words=`cat "$IN" | wc -w`
words=`expr $words`
lines=`cat "$IN" | wc -l`
lines=`expr $lines`
lineslabel=` [ "$lines" -lt 2 ] && echo Line || echo Lines`

# Write the VIM script.
{
  echo "\""
  echo "\"Dont read other peoples temp files."
  echo "\"" 
} > $OUT
echo -e ":echo \"$words Words on $lines $lineslabel\"" >> $OUT
echo ":silent !rm $OUT" >> $OUT

# Tell VIM where the script is.
echo $OUT
