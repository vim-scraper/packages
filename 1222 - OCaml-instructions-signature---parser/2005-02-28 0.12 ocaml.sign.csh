#!/bin/csh
#written by Grzegorz Dymarek (gdymarek@idea.net.pl)
#script for OCaml instructions signature (ver 0.12)

set phrase = `head --lines=$2 $1 | tail --lines=1`

set i = 0
set num = `echo $phrase | grep -c ";;"`
while ($num < 1)
set i = `expr $i + 1`
set a = `expr $2 + $i`
set b = `expr $i + 1`
set phrase = `head --lines=$a $1 | tail --lines=$b`
set num = `echo $phrase | grep -c ";;"`
end 

echo $phrase | ocaml > "$1.~tmp"

set phrase1 = `head --lines=3 "$1.~tmp" | tail --lines=1`
echo $phrase1;

rm "$1.~tmp"
