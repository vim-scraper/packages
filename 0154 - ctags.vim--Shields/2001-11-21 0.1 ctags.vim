"
" ctags.vim:    Vim menu for manipulating ctag files
" Author:       Daniel Shields <vim@aegis.mailshell.com>
" License:      LGPL
"
" Tested with Vim 6.0, Unix only, requires ctags, egrep
" and find
"
" TODO: Allow changes to file suffixes
" TODO: Dialog for setting root directory
" TODO: Headers should come before source in the tag file

if exists("loaded_ctags")
    finish
endif
let loaded_ctags = 1

let s:suffixes='"\.c|\.cc|\.cpp|\.cxx|\.h|\.hpp|\.hxx"'
let s:tagfile=$HOME."/.tags"
let s:rootdir=getcwd()

let &tags=s:tagfile

amenu CTa&gs.\ &Add			    :call CTagsAdd()<CR>
amenu CTa&gs.\ &Replacell 	    :call CTagsReplace()<CR>
amenu CTa&gs.\ &Set\ Root\ Dir 	:call CTagsSetRoot()<CR>

" append tagged contents of this directory to the tag file
function! CTagsAdd()
	exec '!ctags --c-types=+C+x+p -a -f '.s:tagfile.' $(find '.s:rootdir.' | egrep '.s:suffixes.')'
endfunction

" replace tag file with tagged contents of this directory
function! CTagsReplace()
	exec '!ctags --c-types=+C+x+p -f '.s:tagfile.' $(find '.s:rootdir.' | egrep '.s:suffixes.')'
endfunction

" move the starting directory to pwd
function! CTagsSetRoot()
    let s:rootdir=getcwd()
    echo s:rootdir
endfunction

nmap ,ta :call CTagsAdd()<CR>
nmap ,tr :call CTagsReplace()<CR>
nmap ,ts :call CTagsSetRoot()<CR>
