"
" Mappings that let you undelete the previously deleted text using ^W or ^U
"  in insert mode. To use just source this file and to undo the previous
"  deletion, just type ^X^U.
"
" Author: Hari <hari_vim@yahoo.com>
" Last Modified: 16-Nov-2001 @ 14:57
" Requires: Vim-6.0
" Version: 1.0

" This would have happened to everyone at least once. You start typing a new
"   line and press ^U and kill the line, and then realize you can't get that
"   line back.  It is painful that you can't undo in insert mode isn't it? This
"   little script aims at adding a rudimentary backup mechanism by which you can
"   get back previously killed text through ^U or ^W. To undo what you have just
"   done (through ^W or ^U) use ^X^U or ^X^W.
"
" TODO: Currently the script doesn't keep track of the start column where the
"   editing started, so it might undo a little too much text, but essentially
"   you are not loosing anything. You just have to manually edit the text that
"   is put by undo.

if exists("loaded_undoins")
  finish
endif
let loaded_undoins=1

inoremap <silent> <C-W> <C-R>=<SID>SaveWordToBeDeleted()<CR><C-W>
inoremap <silent> <C-U> <C-R>=<SID>SaveLineToBeDeleted()<CR><C-U>

"
" Define a default mapping if the user hasn't defined one.
"
if !hasmapto('<Plug>UndoInInsModeUndoKey')
  imap <unique> <silent> <C-X><C-U> <Plug>UndoInInsModeUndoKey
endif

" The main plugin mapping.
inoremap <script> <silent> <Plug>UndoInInsModeUndoKey <C-R>=<SID>ReturnLastDeletion()<CR>

" Saving one deletion is more than enough. This is only to provide an emergency
"   backup, in case the user presses ^W by mistake.
function! s:SaveWordToBeDeleted()
  let b:lastDeletion = s:GetPreviousWordLikeCTRL_W()
  return ""
endfunction


function! s:SaveLineToBeDeleted()
  let lin = getline(line('.'))
  let b:lastDeletion = strpart(lin, 0, col('.') - 1)
  return ""
endfunction


function! s:ReturnLastDeletion()
  if exists("b:lastDeletion")
    return b:lastDeletion
  else
    return ""
  endif
endfunction


" With the help of Luc Hermitte <hermitte@laas.fr> from his words_tools.vim
"   script.
" This function tries to return exactly what is going to be deleted by ^W.
function! s:GetPreviousWordLikeCTRL_W()
  let lin = getline(line('.'))
  let lin = strpart(lin, 0, col('.') - 1)
  let w = matchstr(lin, '\<\k\+\>\s*$')
  if strlen(w) == 0
    " can't make this work ... hence the substitute which is fine.
    "let w = matchtr(lin, '\k\.\{-}$')
    let w = substitute(lin, '.*\k', '', 'g')
  endif
  return w
endfunction
