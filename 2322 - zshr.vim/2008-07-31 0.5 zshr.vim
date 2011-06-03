if &cp || exists("g:loaded_zshr")
    finish
endif
let g:loaded_zshr = 1

let s:save_cpo = &cpo
set cpo&vim

" command
command! -nargs=+ R :call s:R(<f-args>)

function! s:R(...)
    let l:length = len(a:000)
    let l:index = 0

    " get last command
    let l:pre_cmd = histget(":", -2)

    while l:index < l:length
        let l:pre_cmd = s:ReplaceCmd(l:pre_cmd, a:000[l:index])
        let l:index += 1
    endwhile

    " execute command
    try
        execute l:pre_cmd
    catch
        " command failure.
        echohl ErrorMsg | echo v:exception | echohl None
    endtry

    " add command history.
    call histadd(":", l:pre_cmd)
endfunction

" replace command, and return
function! s:ReplaceCmd(cmd, param)
    let l:cmd = a:cmd

    let l:matched = match(a:param, '[^=]\+=[^=]\+')
    if l:matched > -1
        " find and replace.
        let l:pre  = substitute(a:param, '\([^=]\+\)=\([^=]\+\)', '\1', "")
        let l:post = substitute(a:param, '\([^=]\+\)=\([^=]\+\)', '\2', "")
        let l:cmd = substitute(l:cmd, l:pre, l:post, "g")
    else
        " command syntax error is found.
        echohl WarningMsg | echo ":R command syntax error is found." | echohl None
    endif

    return l:cmd
endfunction

let &cpo = s:save_cpo
finish

==============================================================================
zshr.vim : substitute and execute previous command.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/zshr.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2008/05/08 20:00:00
==============================================================================

Substitute previous command string, and execute substituted command.
This plugin add ":R" command to your vim environment.

------------------------------------------------------------------------------
[command syntax]

:R {old}={new}
:R {old1}={new1} [{old2}={new2} {old3}={new3} ...]

------------------------------------------------------------------------------
[command example - 1]
1. Execute a command.

    :%s/aaabbbccc/xxxyyyzzz/g


2. Execute ":R" command with replacement parameters.

    :R a=m

   (find "a" in last command, and replace "a" to "m".)


3. Vim editor execute this command.

    :%s/mmmbbbccc/xxxyyyzzz/g


------------------------------------------------------------------------------
[command example - 2]
4. You want to use multiple replacement parameters,
   you execute command this example.

    :R a=m ccc=xyz bb=123

   (find "a" in last command, and replace "a" to "m".
    find "ccc" in last command, and replace "ccc" to "xyz".
    find "bb" in last command, and replace "bb" to "123".)

==============================================================================

1. Copy the zshr.vim script to
   $HOME/vimfiles/plugin or $HOME/.vim/plugin directory.
   Refer to ':help add-plugin', ':help add-global-plugin' and
   ':help runtimepath' for more details about Vim plugins.

2. Restart Vim.

3. "zshr.vim" plugin add ":R" command to your vim environment.

==============================================================================
" vim: set ff=unix et ft=vim nowrap :
