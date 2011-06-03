scriptencoding utf-8
if &cp || exists('g:loaded_cmdline_increment')
    finish
endif
let g:loaded_cmdline_increment = 1

" escape user configuration
let s:save_cpo = &cpo
set cpo&vim

cnoremap <m-a> <c-b>"<cr>:let @l=@:<cr>:call IncrementCommandLineNumbering(1)<cr>:<c-r>l
cnoremap <m-x> <c-b>"<cr>:let @l=@:<cr>:call IncrementCommandLineNumbering(-1)<cr>:<c-r>l

function! IncrementCommandLineNumbering(plus)
    " escape continuous increment
    let l:lastcommand = histget(':', -1)
    let l:top = strpart(l:lastcommand, 0, 1)

    if l:top ==# '"'
        let l:command = @l
    else
        let l:command = '"' . l:lastcommand
    endif

    " get current input
    let l:matchtest = match(l:command, '^".\{-\}\d\+\D*$')
    if l:matchtest < 0
        " remove first '"'
        let @l = substitute(l:command, '^"\(.*\)$', '\1', '')
        return
    endif

    " update numbering
    let l:numpattern = substitute(l:command, '^".\{-\}\(\d\+\)\D*$', '\1', '')
    let l:numpattern = l:numpattern + a:plus

    let l:p1 = substitute(l:command, '^"\(.\{-\}\)\d\+\D*$', '\1', '')
    let l:p2 = l:numpattern
    let l:p3 = substitute(l:command, '^".\{-\}\d\+\(\D*\)$', '\1', '')

    " set l register
    let @l = l:p1 . l:p2 . l:p3

    " delete '"' command (dummy command) history
    call histdel(':', -1)
    " TODO last command is wrong history
    call histadd(':', l:p1 . l:p2 . l:p3)
endfunction

" recover user configuration
let &cpo = s:save_cpo
finish

==============================================================================
cmdline-increment.vim : increment, decrement for commandline mode.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/cmdline-increment.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2009/09/18 03:00:00
==============================================================================
Increment number in commandline-mode command with Alt-a ,
and decrement number in commandline-mode command with Alt-x .

    <M-a> increment
    <M-x> increment

This plugin can only increment (, or decrement) last decimal value in command.


------------------------------------------------------------------------------
[Tutorial]

1.  Enter commandline mode.
2.  Enter next command.

    :edit workfile_1.txt

3.  Press Alt-a , or Alt-x .


==============================================================================
" vim: set ff=unix et ft=vim nowrap :
