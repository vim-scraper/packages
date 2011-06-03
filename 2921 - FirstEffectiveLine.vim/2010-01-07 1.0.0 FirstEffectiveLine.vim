"=============================================================================
" FirstEffectiveLine.vim - move to the first effective(non-comment) line
"=============================================================================
"
" Author:  Takahiro SUZUKI <takahiro.suzuki.ja@gmDELETEMEail.com>
" Version: 1.0.0 (Vim 7.1)
" Licence: MIT Licence
"
"=============================================================================
" Document: {{{1
"
"-----------------------------------------------------------------------------
" Description:
"   This plugin provides a normal mode command 'gG', which moves to the first
"   effective line of the current file.
"   In this script, "effective line" means line whose first character is
"   not a comment nor a preprocessor nor empty.
"
"   If you want to map them to another keys than 'gG', add a line like below
"   in your vimrc.
"     nmap YOURKEY <Plug>GotoFirstEffectiveLine
"
"-----------------------------------------------------------------------------
" Installation:
"   Place this file in /usr/share/vim/vim*/plugin or ~/.vim/plugin/
"
"-----------------------------------------------------------------------------
" Examples:
"   in normal mode:
"     gG    " move to the first effective line
"
"-----------------------------------------------------------------------------
" ChangeLog:
"   1.0.0:
"     - released in vim.org
"   0.0.0:
"     - Initial version
"
" }}}1
"=============================================================================

function! s:GotoFirstEffectiveLine()
  let l:c = 0
  while l:c<line("$") && (
        \ getline(l:c) =~ '^\s*$'
        \ || synIDattr(synID(l:c, 1, 0), "name") =~ ".*Comment.*"
        \ || synIDattr(synID(l:c, 1, 0), "name") =~ ".*PreProc$"
        \ )
    let l:c = l:c+1
  endwhile
  exe "normal ".l:c."Gz\<CR>"
endfunction

" default mapping
if !hasmapto('<Plug>GotoFirstEffectiveLine', 'n')
  nmap <silent> gG <Plug>GotoFirstEffectiveLine
endif

" export the plugin mapping
nmap <script> <Plug>GotoFirstEffectiveLine 
              \:<C-U>call <SID>GotoFirstEffectiveLine()<CR>

" vim: set foldmethod=marker et ts=2 sts=2 sw=2:
