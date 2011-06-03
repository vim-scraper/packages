" Jump Next Long Line
" ===================
"
" Vim plugin to jump to the next line that exceed the textwidth setting
" Last Change: 2010 Dec 24
" Maintainer: Caio Romão <caioromao@gmail.com>
" License: This file is placed in the public domain
"
" Mappings:
"   <Leader>l or <Plug>JumpNextLongLine
"       Jumps to the next (too) long line
"
" This plugin doesn't have any settings.

if exists("g:loaded_JumpNextLongLine") || &cp
  finish
endif

let g:loaded_JumpNextLongLine= 1
let s:save_cpo = &cpo
set cpo&vim

if !hasmapto('<Plug>JumpNextLongLine')
    nmap <silent> <unique> <Leader>l <Plug>JumpNextLongLine
endif

noremap <unique> <script> <Plug>JumpNextLongLine :call <SID>JumpNext()<CR>

function! s:JumpNext()
    let nline = s:NextLongLine()
    execute "normal! " . nline . "gg"
endfunction

function! s:ListLongLines()
    let treshold = (&tw ? &tw : 80)
    let spaces = repeat(" ", &ts)
    let longlines = []

    let i = 1
    while i <= line("$")
        " Respect user's &ts setting
        let len = strlen(substitute(getline(i), '\t', spaces, 'g'))
        if len > treshold
            call add(longlines, i)
        endif
        let i += 1
    endwhile

    return longlines
endfunction

function! s:NextLongLine()
    if !exists("b:long_lines_list")
        let b:long_lines_list = s:ListLongLines()
    endif

    let curline = line('.')
    let listsize = len(b:long_lines_list)

    let i = 0
    while i < listsize
        let nextline = get(b:long_lines_list, i)
        if nextline > curline
            return nextline
        endif
        let i += 1
    endwhile
    " allow wrapping to the beginning of the buffer
    return get(b:long_lines_list, 0, curline)
endfunction

" Delete list of long lines when idle and when writing
" to trigger it's re-creation
autocmd cursorhold,bufwritepost * unlet! b:long_lines_list

let &cpo = s:save_cpo
