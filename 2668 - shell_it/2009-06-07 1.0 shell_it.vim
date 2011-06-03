"    Author:  Vande (vimtexhappy@gmail.com)
"             a easy shell buf for vim
"   Version:  v01
"   Created:  2009-04-02
"   License:  Copyright (c) 2001-2009, Vande
"             This program is free software; you can redistribute it and/or
"             modify it under the terms of the GNU General Public License as
"             published by the Free Software Foundation, version 2 of the
"             License.
"             This program is distributed in the hope that it will be
"             useful, but WITHOUT ANY WARRANTY; without even the implied
"             warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
"             PURPOSE.
"             See the GNU General Public License version 2 for more details.
"     Usage:  Put this file in your VIM plugin dir  
"shell_it.vim: {{{1
if exists('loaded_shell_it') || &cp
    finish
endif
let loaded_shell_it = 1
let s:shell_prompt = "@vim>"
let s:shell_prompt_len = strlen(s:shell_prompt)
let s:shell_history = []
let s:shell_history_s = 0
let s:shell_history_l = 0

"ShowPrompt {{{1
function! ShowPrompt()
    exec 'normal! S'
    exec 'normal! 0i'.s:shell_prompt
    exec 'normal! l'
endfunction

"ShellExe {{{1
function! ShellExe(cmd)
    let old_winnr = winnr()
    call ShellOpen()
    exec 'normal! a'.a:cmd
    call ShellEnter()
    silent! exec old_winnr.'wincmd w'
endfunction

"ShellEnter {{{1
function! ShellEnter()
    let line = getline('.')
    let cmd = matchstr(line, '\m^'.s:shell_prompt.'\zs.*$')
    if cmd =~ '\s*clear\s*'
        normal ggdG
    elseif cmd != ""
        exec "silent! r !".cmd
        let found = 0
        for i in s:shell_history
            if i == cmd
                let found = 1
                break
            endif
        endfor
        if !found
            call add(s:shell_history, cmd)
            let s:shell_history_l += 1
        endif
        exec 'normal! o'
    else 
        exec 'normal! o'
    endif
    let s:shell_history_s = s:shell_history_l
    call ShowPrompt()
endfunction

"CheckInput {{{1
function! CheckInput()
    if (col('.') <= s:shell_prompt_len) && (line('.') == line('$'))
        call ShowPrompt()
    end
endfunction

"GetHistory {{{1
function! GetHistory(up)
    if a:up 
        let s:shell_history_s -= 1
    else 
        let s:shell_history_s += 1
    endif
    if s:shell_history_s < 0
        let s:shell_history_s = 0
        return
    elseif s:shell_history_s >= s:shell_history_l
        let s:shell_history_s = s:shell_history_l
        exec 'normal! S'
        call ShowPrompt()
        return
    endif
    call ShowPrompt()
    exec 'normal! i'.s:shell_history[s:shell_history_s]
    exec 'normal! l'
endfunction

"ShellOpen {{{1
function! ShellOpen()
    let shell_winnr = bufwinnr("vshell")
    if shell_winnr == -1
        silent bot 10new vshell
        exec 'syntax match prompt "^'.
                    \ s:shell_prompt.'.*$" contains=command'
        exec 'syntax match command "^'.s:shell_prompt.'\zs.*$" contained'
        hi def prompt ctermfg=green guifg=green
        hi def command ctermfg=red guifg=red
        setlocal buftype=nofile
        silent! autocmd CursorMoved,CursorMovedI <buffer> call CheckInput()
        inoremap <buffer> <cr> <c-o>:call ShellEnter()<cr>
        inoremap <buffer> <up> <c-o>:call GetHistory(1)<cr>
        inoremap <buffer> <down> <c-o>:call GetHistory(0)<cr>
        inoremap <buffer> <c-p> <c-o>:call GetHistory(1)<cr>
        inoremap <buffer> <c-n> <c-o>:call GetHistory(0)<cr>
    elseif shell_winnr != winnr()
        silent! exec shell_winnr.'wincmd w'
    endif
    normal G
    call ShowPrompt()
endfun
