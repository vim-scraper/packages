scriptencoding utf-8
if &cp || exists('g:loaded_rargs')
    finish
endif
let g:loaded_rargs = 1

" escape user configuration
let s:save_cpo = &cpo
set cpo&vim

" read file at cursor position
function! s:RArgs(insertat,...)
    let l:filelist = []
    for l:argv in a:000
        call extend(filelist, split(expand(l:argv), '\n'))
    endfor

    call reverse(l:filelist)

    for l:f in l:filelist
        if filereadable(l:f)
            execute ':' . a:insertat . 'r ' . l:f
        endif
    endfor
endfunction

command! -narg=* -range=1 -complete=file RArgs :call s:RArgs(<line1>,<f-args>)

" recover user configuration
let &cpo = s:save_cpo
finish

==============================================================================
rargs.vim : read the contents of many files at one time.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/rargs.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2009/09/16 13:30:00
==============================================================================
This plugin defined ':RArgs' command.
':RArgs' can read selected some files at one time.
( like :read command )


------------------------------------------------------------------------------
[command format]

" read {filename1}, {filename2}, and {filename3},
" contents are inserted into N.
:[N]RArgs {filename1} {filename2} {filename3}...

[N]
    contents read are inserted into the specified line,
    or not specify line, contents are inserted into cursor position.

{filename1},{filename2},{filename3}...
    filepath list.

TODO
    ++opt parameter is now not supported.


------------------------------------------------------------------------------
[filepath parameters]

" read sample1.txt, sample2.txt, and sample3.txt
:RArgs sample1.txt sample2.txt sample3.txt

" wildcards are allowed
:RArgs sample*

" Vim special keywords are allowed
:RArgs #2 #4 #6<.bak

" reading the file at multiple times are allowed
:RArgs sample1.txt sample1.txt sample1.txt


------------------------------------------------------------------------------
[inserting file position]

If you do not specify line number,
the contents of selected files are inserted into cursor line.

" contents are inserted into cursor line.
:RArgs sample1.txt sample2.txt sample3.txt

" contents are inserted into specified line.
:200RArgs sample1.txt sample2.txt sample3.txt

" contents are inserted into first line of buffer.
:0RArgs sample1.txt sample2.txt sample3.txt


==============================================================================
" vim: set ff=unix et ft=vim nowrap :
