scriptencoding utf-8
if &cp || exists('g:loaded_cmdline_increment')
    finish
endif
let g:loaded_cmdline_increment = 1

" escape user configuration
let s:save_cpo = &cpo
set cpo&vim

" mapping
if !hasmapto('<Plug>IncrementCommandLineNumber', 'c')
    cmap <c-a> <Plug>IncrementCommandLineNumber
endif
cnoremap <Plug>IncrementCommandLineNumber <c-b>"<cr>:call g:IncrementCommandLineNumbering(1)<cr>:<c-r>=g:IncrementedCommandLine()<cr>
if !hasmapto('<Plug>DecrementCommandLineNumber', 'c')
    cmap <c-x> <Plug>DecrementCommandLineNumber
endif
cnoremap <Plug>DecrementCommandLineNumber <c-b>"<cr>:call g:IncrementCommandLineNumbering(-1)<cr>:<c-r>=g:IncrementedCommandLine()<cr>

" script sharing variables.
" updated command line will be stored in g:IncrementCommandLineNumbering().
let s:updatedcommandline = ''

" increment, or decrement last appearing number.
function! g:IncrementCommandLineNumbering(plus)
    " when continuous increment is done, because @: is not updated,
    " plugin can not increment correctly.
    " so add one entry to command history, and use there flag.
    " last command is start with '"' is first try,
    " last command is not start with '"' is second try.
    let l:lastcommand = histget(':', -1)
    let l:firstcommandchar = strpart(l:lastcommand, 0, 1)

    " l:command is '"' starting text.
    if l:firstcommandchar ==# '"'
        let l:command = l:lastcommand
    else
        let l:command = '"' . l:lastcommand
    endif

    " check input
    let l:matchtest = match(l:command, '^".\{-\}-\?\d\+\D*$')
    " command do not contain number
    if l:matchtest < 0
        " remove first char '"'
        let s:updatedcommandline = substitute(l:command, '^"\(.*\)$', '\1', '')
        return
    endif

    " update numbering
    let l:numpattern = substitute(l:command, '^".\{-\}\(\d\+\)\D*$', '\1', '')
    let l:updatednumberpattern = s:IncrementedText(l:numpattern, a:plus)

    " create new command line strings
    let l:p1 = substitute(l:command, '^"\(.\{-\}\)\d\+\D*$', '\1', '')
    let l:p2 = l:updatednumberpattern
    let l:p3 = substitute(l:command, '^".\{-\}\d\+\(\D*\)$', '\1', '')
    " set l register
    let s:updatedcommandline = l:p1 . l:p2 . l:p3

    " delete '"' command (dummy command) history
    call histdel(':', -1)
    " wrong command history is added. limitation.
    call histadd(':', s:updatedcommandline)
endfunction

" return incremented command line text.
function! g:IncrementedCommandLine()
    return s:updatedcommandline
endfunction

" return incremented pattern text.
"
" number   +      -
" 0     -> 1,     0
" 0000  -> 0001,  0000
" 127   -> 128,   126
" 0127  -> 0128,  0126
" 00127 -> 00128, 00126
function! s:IncrementedText(pattern, plus)
    " 0
    if match(a:pattern, '^0$') >= 0
        if a:plus > 0
            return a:pattern + a:plus
        else
            " not supported
            return a:pattern
        endif
    endif

    " 123
    if match(a:pattern, '^[^0]\d*$') >= 0
        return a:pattern + a:plus
    endif

    " 00000
    if match(a:pattern, '^0\+$') >= 0
        if a:plus > 0
            let l:numlength = strlen(a:pattern)
            return printf('%0' .l:numlength. 'd', a:plus)
        else
            " not supported
            return a:pattern
        endif
    endif

    " 00123
    if match(a:pattern, '^0\d*$') >= 0
        echo a:pattern + a:plus
        let l:numlength = strlen(a:pattern)
        let l:number = substitute(a:pattern, '^0\+\(\d\+\)$', '\1', '')
        return printf('%0' .l:numlength. 'd', l:number + a:plus)
    endif

    throw 'unknow numbering pattern is found.'
endfunction

" recover user configuration
let &cpo = s:save_cpo
finish

==============================================================================
cmdline-increment.vim : increment, decrement for commandline number.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/cmdline-increment.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2009/09/19 03:00:00
==============================================================================
Increment last appearing number in commandline-mode command with Control-a ,
and decrement with Control-x .

    <C-a> increment commandline last appearing number.
    <C-x> decrement commandline last appearing number.


------------------------------------------------------------------------------
[Usage]

1.  Enter commandline mode.
2.  Enter next command.

    :edit workfile_1.txt

3.  And Press Control-a , or press Control-x .


------------------------------------------------------------------------------
[Customized Mapping]

If you will customize increment, decrement mapping,
add put these code to your vimrc .

    " (for example)
    " increment with Shift-Up
    cmap <S-Up> <Plug>IncrementCommandLineNumber
    " decrement with Shift-Down
    cmap <S-Down> <Plug>DecrementCommandLineNumber


------------------------------------------------------------------------------
[history]
2009/09/17
    - initial version.

2009/09/18
    - plugin is now not using 'l' register.
    - below 0 number decrement is newly not supported.
      minial value is 0.
    - default mapping is switched to <c-a>, <c-x>.
    - custom mapping is supported.


==============================================================================
" vim: set ff=unix et ft=vim nowrap :
