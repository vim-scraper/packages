" Title: undobranche_viewer.vim : Undo Branches Explorer
" File: undobranche_viewer.vim
" Author: Omi Taku (mail@nanasi.jp)
" URL: http://nanasi.jp/
" Version: 0.4
" Last Modified: February 04, 2008
"
" Overview
" --------
" Vim7 Undo Branches Explorer.
" See ':help undo-branches', also.
"
" Installation
" ------------
" 1. Copy the undobranch_viewer.vim script to the $HOME/.vim/plugin or
"    the $HOME/vimfiles/plugin. Refer to the ':help add-plugin',
"    ':help add-global-plugin' and ':help runtimepath' topics for
"    more details about Vim plugins.
"
" 2. Restart Vim.
"
" 3. This script requires tlib (vimscript #1863) to be installed. 
"
" Usage
" -----
" 1. Open Undo Branches Explorer with command ':UndoBranchViewer'.
" 2.
"   <c-p> undo selected branche.
"   <c-u> undo selected branche, and close undo branches list.
"
:if exists('loaded_undobranch_viewer')
    :finish
:endif
:let loaded_undobranch_viewer = 1

" command
:command! -nargs=0 UndoBranchViewer :call s:UndoBranchViewer()

:function! s:UndoBranchViewer()
    :let g:undolistHandlers = [
        \ {'key': 21, 'agent': s:SNR().'AgentUndoAndClose', 'key_name': '<c-u>', 'help': 'Undo and close'},
        \ {'key': 16, 'agent': s:SNR().'AgentUndoPreview', 'key_name': '<c-p>', 'help': 'Undo preview'},
        \ {'scratch_vertical': 1},
        \ {'pick_last_item': 0},
        \ ]
    :let s:undolists = tlib#cmd#OutputAsList('undolist')
    :call tlib#input#List('s', 'UndoBranch', s:undolists, g:undolistHandlers)
:endfunction

:function! s:AgentUndoAndClose(world, selected)
    :let l:entry = a:selected[0]
    :if l:entry !~ "^ *\\d"
        :return a:world
    :endif
    :let l:undonumber = matchstr(l:entry, "\\d\\+")
    :let sb = a:world.SwitchWindow('win')
    :try
        :execute "undo " . l:undonumber
    :finally
        :exec sb
    :endtry
    :let a:world.state = 'redisplay'
    :silent call a:world.CloseScratch()
    :return a:world
:endfunction

:function! s:AgentUndoPreview(world, selected)
    :let l:entry = a:selected[0]
    :if l:entry !~ "^ *\\d"
        :return a:world
    :endif
    :let l:undonumber = matchstr(l:entry, "\\d\\+")
    :let sb = a:world.SwitchWindow('win')
    :try
        :execute "undo " . l:undonumber
    :finally
        :exec sb
    :endtry
    :let a:world.state = 'redisplay'
    :return a:world
:endfunction

:function! s:SNR()
    :return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
:endfunction

" vim: set ff=unix et :
