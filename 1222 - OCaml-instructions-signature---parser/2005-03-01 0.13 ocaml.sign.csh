#!/bin/csh
#written by Grzegorz Dymarek (gdymarek@idea.net.pl)
#script for OCaml instructions signature (ver 0.13)

set phrase = `head --lines=$2 $1 | tail --lines=1`
set i = 0
set num = `echo $phrase | grep -c ";;"`
set end = 0
while ( ( $num < 1 ) && ( ! $end ) )
    set i = `expr $i + 1`
    set a = `expr $2 + $i`
    set b = `expr $i + 1`
    set phrase1 = `head --lines=$a $1 | tail --lines=$b`
    if ( "$phrase" == "$phrase1" ) set end = 1
    
    
    set phrase = "$phrase1"
    set num = `echo $phrase | grep -c ";;"`
end 

if ($end) then
    echo "Can't find the end of instruction (;;)"
else
    echo $phrase | ocaml > "$1.~tmp"
    set phrase1 = `head --lines=3 "$1.~tmp" | tail --lines=1`
    echo $phrase1;
    rm "$1.~tmp"
endif
