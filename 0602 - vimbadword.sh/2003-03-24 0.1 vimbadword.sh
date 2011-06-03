#!/bin/sh
#
# vimbadword.sh -  a small script that writes a vim :so file that :echos the 
#                  first misspelled word that aspell reports to the status bar.
#                  There is not real reason to use this over vimspell.sh and it
#                  has highlighting, so go use that.
#
# (c) <thsutton@utas.edu.au> 20030306
#

IN="$1"
#OUT=`tempfile`		# someone should fix this for which(1)
OUT=/tmp/vimbadword.$$

badword=`cat "$IN" | aspell -l --mode=none | head -1`	# spell(1) sans sort(1)
# you could probably put stuff in here to put the bad word in a register
# and then map a key to go to it/highlight it or what ever.
{
  echo "\""
  echo "\"Dont read other peoples temp files."
  echo "\"" 
} > $OUT
echo -e ":echo \"Bad word: $badword\"" >> $OUT
echo ":silent !rm $OUT" >> $OUT
echo $OUT
