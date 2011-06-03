#!/bin/bash

SERVERNAME=$1
FILE=$2
LINENR=$3
EX=/usr/bin/gvim


servers=$($EX --serverlist)
echo $servers | egrep -qw $SERVERNAME 
if [ 0 -ne $? ] ; then
	# start server with file
	$EX --servername $SERVERNAME $FILE
else
	# server already running
	vimcmd="bufwinnr(\"$(basename $FILE)\")"

	buffer=`$EX --servername $SERVERNAME --remote-expr $vimcmd`
	if [ "$buffer" != "-1" ] ; then
		# buffer already loaded. Select buffer and pick line
		$EX --servername $SERVERNAME --remote-send "<ESC>:${buffer}winc w<CR>"

	else
		# split and
		$EX --servername $SERVERNAME --remote-send "<ESC>:sp $FILE<CR>"
	fi
fi
if [ "$LINENR" != "" ] ; then 
	$EX --servername $SERVERNAME --remote-send ":$LINENR<CR>"
fi

# if this fails in KDE try kde controlcenter -> Desktop -> Window Behavior ->  Advanced
# and set focus stealing policy to none
$EX --servername $SERVERNAME --remote-expr "foreground()"


