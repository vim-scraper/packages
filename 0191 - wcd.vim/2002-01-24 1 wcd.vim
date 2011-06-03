"  wcd.vim: (global plugin) Wherever Change Directory
"  Last Change: January 25, 2002
"  Maintainer:  Pavol Juhas   <juhas@seas.upenn.edu>
"  Version:     1
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
" wcd executable: (may need a full path)
let s:wcd = 'wcd.exe'
" cygwin wcd.exe uses paths in the form of `/cygdrive/c/bin',
" this would confuse gvim, let's convert to the normal form
let s:filter_cygrive = 1
" abbreviate for faster typing
cnoreabbrev wcd Wcd
" End of settings

com! -nargs=+ Wcd call WcdFun("<args>")
function! WcdFun(wcdargs)
    1new 
    setl bt=nofile
    exe '0r !' . s:wcd . ' ' . a:wcdargs
    0/./
    if s:filter_cygrive
	s#/cygrive/\(\a\)#\1:#ei
    endif
    " redraw so that echo works
    redraw
    let ln1 = getline(".")
    echo ln1
    if ln1 =~ '-> '
	let ln1 = strpart(ln1,3)
	exe "cd " . substitute(ln1, ' ', '\ ', "g")
    endif
    bwipeout
endfunction
