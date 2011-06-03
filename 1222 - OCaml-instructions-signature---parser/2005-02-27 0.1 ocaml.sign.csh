#!/bin/csh
#Written by Grzegorz Dymarek (gdymarek@idea.net.pl)
#Script for OCaml instructions signature

set phrase = `head --lines=$2 $1 | tail --lines=1`
echo $phrase | ocaml > "$1.~tmp"

set phrase1 = `head --lines=3 "$1.~tmp" | tail --lines=1`
echo $phrase1;

rm "$1.~tmp"
