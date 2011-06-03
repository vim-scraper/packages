" undoins.vim - Undeleted the immediate previous <C-W> or <C-U> action.
" Author: Hari Krishna Dara (hari_vim at yahoo dot com)
" Last Modified: 16-Nov-2001 @ 14:57
" Created: Sometime before 08-Aug-2001
" Requires: Vim-6.0
" Version: 1.1
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Acknowledgements:
"   - Luc Hermitte (hermitte at laas dot fr) for his words_tools.vim and for
"     the help on constructing s:GetPreviousWordLikeCTRL_W() function.
" Download From:
"     http://www.vim.org/script.php?script_id=150
" Description: 
" - Vim provides a number or ways to recover some text that you have deleted
"   (multiple undos, numbered registers, unnamed register etc.), however,
"   there is no way to recover a line or word that you killed either
"   accidentally (e.g., pressing <C-W> one extra time) or deliberately if you
"   are already in insert mode.
" - This little script tries to compensate for this missing functionality for
"   the most commonly used two commands, <C-W> and <C-U> such that the text
"   that is going to be killed is first backed up in an internal script
"   variable before letting Vim continue to do what you asked it to do.
" - The script also provides a mapping (by default <C-X><C-U> which you can
"   customize by using the <Plug>UndoInInsModeUndoKey mapping) that you can
"   execute to put back the text at the current cursor position. A <C-X><C-U>
"   immediately followed by a <C-U> or <C-W> is effectively works like an undo
"   of the previous action.
" - The script is only going to "estimate" what is going to be deleted by ^W,
"   but for certain filetypes it may not exactly match the word that Vim is
"   going to delete, in which case the undo might undo too much or too little.
"
" TODO: 
"   - Cosider consecutive word deletions. 

if exists("loaded_undoins")
  finish
endif
let loaded_undoins=1

inoremap <silent> <C-W> <C-R>=<SID>SaveWordToBeDeleted()<CR><C-W>
inoremap <silent> <C-U> <C-R>=<SID>SaveLineToBeDeleted()<CR><C-U>

"
" Define a default mapping if the user hasn't defined one.
"
if !hasmapto('<Plug>UndoInInsModeUndoKey', 'i')
  imap <unique> <silent> <C-X><C-U> <Plug>UndoInInsModeUndoKey
endif

" The main plugin mapping.
inoremap <script> <silent> <Plug>UndoInInsModeUndoKey <C-R>=<SID>ReturnLastDeletion()<CR>

" Saving one deletion is more than enough. This is only to provide an emergency
"   backup, in case the user presses ^W by mistake.
function! s:SaveWordToBeDeleted()
  let lastWord = s:GetPreviousWordLikeCTRL_W()
  let wordStartCol = match(strpart(getline('.'), 0, col('.')), lastWord.'\s*')
	\ + 1 " string index to column number
  let b:lastDeletion = strpart(lastWord,
	\ (line("'^") == line('.') && col("'^") > wordStartCol) ?
	    \     (col("'^") - wordStartCol) : 0)
  return ""
endfunction


function! s:SaveLineToBeDeleted()
  let lin = getline(line('.'))
  let b:lastDeletion = strpart(lin, line("'^") == line('.') ? col("'^") - 1: 0,
	\ col('.') - 1)
  return ""
endfunction


function! s:ReturnLastDeletion()
  if exists("b:lastDeletion")
    return b:lastDeletion
  else
    return ""
  endif
endfunction


" With the help of Luc Hermitte (hermitte at laas dot fr) from his
"   words_tools.vim script.
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
