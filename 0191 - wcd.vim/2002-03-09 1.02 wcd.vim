"  wcd.vim: (global plugin) Wherever Change Directory
"  Last Change: March 9, 2002
"  Maintainer:  Pavol Juhas   <juhas@seas.upenn.edu>
"  Version:     1.02
"
"  Usage:  if you have wcd installed, just drop this file into your
"  	   plugin directory and you should be able to use :Wcd command
"  	   similarly as in the shell
"
"  Requirements:     wcd
"  Wcd home page:    http://www.xs4all.nl/~waterlan/
"

if exists("loaded_wcd") || &cp
  finish
endif
let loaded_wcd = 1

" Settings:
"
" wcd executable: (may need a full path)
let s:wcd = 'wcd.exe'
let s:wcdgo = '~/wcd.go'
"
" cygwin wcd.exe uses paths in the form of `/cygdrive/c/bin',
" this would confuse win32 gvim, so we can convert paths to the windows form
let s:filter_cygdrive = !has("unix")
"
" abbreviate for faster typing
cnoreabbrev wcd Wcd
" End of settings

com! -nargs=+ Wcd call WcdFun("<args>")
function! WcdFun(wcdargs)
    exe 'silent !' . s:wcd . ' ' . a:wcdargs
    exe "1new " . s:wcdgo
    if s:filter_cygdrive
	g/^cd /s#/cygdrive/\(\a\)#\1:#ei | s#^cd \(/.*\)#cd c:/cygwin\1#ei
    endif
    update
    exe "source " . s:wcdgo
    g/^cd /let wdir = getline(".")
    redraw!
    if exists("wdir")
	echo "-> " . strpart(wdir, 3)
    else
	echo "Directory not found"
    endif
    bwipeout
endfunction
