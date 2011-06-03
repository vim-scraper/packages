" vim:foldmethod=marker:fen:
scriptencoding utf-8

" DOCUMENT {{{1
"==================================================
" Name: WinMove
" Version: 0.0.0
" Author:  tyru <tyru.takuya+vim@gmail.com>
" Last Change: 2009-03-09.
"
" Change Log: {{{2
"   1.0.0: Initial upload.
" }}}2
"
" Description:
"   Move your gVim around.
"
" Usage:
"   MAPPING:
"       <Up>        move your gVim up.
"       <Right>     move your gVim right.
"       <Down>      move your gVim down.
"       <Left>      move your gVim left.
"
"   GLOBAL VARIABLES:
"       g:wm_move_up (default:'<Up>')
"           if empty string, no mapping is defined.
"
"       g:wm_move_right (default:'<Right>')
"           if empty string, no mapping is defined.
"
"       g:wm_move_down (default:'<Down>')
"           if empty string, no mapping is defined.
"
"       g:wm_move_left (default:'<Left>')
"           if empty string, no mapping is defined.
"
"       g:wm_move_x (default:20)
"           gVim use this when move left or right.
"
"       g:wm_move_y (default:15)
"           gVim use this when move up or down.
"
"
"==================================================
" }}}1

" INCLUDE GUARD {{{1
if exists('g:loaded_winmove') && g:loaded_winmove != 0
    finish
endif
let g:loaded_winmove = 1
" }}}1

" NOTE: THIS PLUGIN CAN'T WORK IN TERMINAL.
if ! has('gui_running')
    finish
endif

" SAVING CPO {{{1
let s:save_cpo = &cpo
set cpo&vim
" }}}1

" GLOBAL VARIABLES {{{1
if ! exists('g:wm_move_up')
    let g:wm_move_up = '<Up>'
endif
if ! exists('g:wm_move_right')
    let g:wm_move_right = '<Right>'
endif
if ! exists('g:wm_move_down')
    let g:wm_move_down = '<Down>'
endif
if ! exists('g:wm_move_left')
    let g:wm_move_left = '<Left>'
endif
if ! exists('g:wm_move_x')
    let g:wm_move_x = 20
endif
if ! exists('g:wm_move_y')
    let g:wm_move_y = 15
endif
" }}}1

" FUNCTION DEFINITION {{{1

func! s:MoveTo( dest ) range
    let winpos = { 'x':getwinposx(), 'y':getwinposy() }
    let repeat = a:lastline - a:firstline + 1

    if a:dest == '>'
        let winpos['x'] = winpos['x'] + g:wm_move_x * repeat
    elseif a:dest == '<'
        let winpos['x'] = winpos['x'] - g:wm_move_x * repeat
    elseif a:dest == '^'
        let winpos['y'] = winpos['y'] - g:wm_move_y * repeat
    elseif a:dest == 'v'
        let winpos['y'] = winpos['y'] + g:wm_move_y * repeat
    endif

    execute 'winpos ' . winpos['x'] . ' ' . winpos['y']
endfunc

" }}}1

" MAPPING {{{1
let s:mappings = {
    \ '^': g:wm_move_up,
    \ '>': g:wm_move_right,
    \ 'v': g:wm_move_down,
    \ '<': g:wm_move_left
\ }
for key in keys(s:mappings)
    if s:mappings[key] != ""
        let fmt = 'nnoremap <silent> %s    :<C-u>call <SID>MoveTo("%s")<CR>'
        execute printf(fmt, s:mappings[key], key)
    endif
endfor
unlet s:mappings
" }}}1

" RESTORE CPO {{{1
let &cpo = s:save_cpo
" }}}1

