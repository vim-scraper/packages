#!/bin/sh

# Script for using ViM as a PAGER.
# Based on Bram's less.sh.
# Version 0.8

files=$@
if [ -z "$files" ]; then files="-"; fi

if uname -s | grep -iq cygwin; then
    cygwin=1
elif uname -s | grep -iq Linux; then
    linux=1
else
    bsd=1
fi

less_vim() {
    vim -R \
    -c 'let no_plugin_maps = 1' \
    -c 'runtime! macros/less.vim' \
    -c 'set scrolloff=999' \
    -c 'set foldlevel=999' \
    -c 'set mouse=h' "$@"
}

do_ps() {
    if [ $bsd ]; then
        ps -u `id -u` -o pid,comm=
    else
        ps fuxw
    fi
}

pproc() {
    if [ $linux ]; then
        ps -p $1 -o comm=
    elif [ $cygwin ]; then
        ps -p $1 | sed -e 's/^I/ /' | grep -v PID
    else
        ps -p $1 -o comm= | grep -v PID
    fi
}

ppid() {
    if [ $linux ]; then
        ps -p $1 -o ppid=
    elif [ $cygwin ]; then
        ps -p $1 | sed -e 's/^I/ /' | grep -v PID | awk '{print $2}'
    else
        ps -p $1 -o ppid= | grep -v PID
    fi
}

# Check if called from man or perldoc
if do_ps | grep -q '\(py\(thon\|doc\)\|man\|perl\(doc\|$\)\)\>'; then
    proc=$$
    while next_parent=`ppid $proc` && [ $next_parent != 1 ]; do
        if pproc $next_parent | grep -q 'man\>'; then
            cat $files | sed -e 's/\[[^m]*m//g' | sed -e 's/.//g' | less_vim -c 'set ft=man' -; exit
        elif pproc $next_parent | grep -q 'py\(thon\|doc\)\>'; then
            cat $files | sed -e 's/\[[^m]*m//g' | sed -e 's/.//g' | less_vim -c 'set ft=man' -; exit
        elif pproc $next_parent | grep -q 'perl\(doc\|$\)\>'; then
            cat $files | sed -e 's/.//g' | less_vim -c 'set ft=man' -; exit
        fi
        proc=$next_parent
    done
fi

less_vim $files

# CONTRIBUTORS:
#
# Rafael Kitover
# Antonio Ospite
# Jean-Marie Gaillourdet
# Perry Hargrave
