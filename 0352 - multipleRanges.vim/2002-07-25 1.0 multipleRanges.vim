" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/multipleRanges.vim" {{{
" LAST MODIFICATION: "Thu, 25 Jul 2002 09:56:35 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version history:
" 1.0:  Initial upload

" Usage:
"
" Maps:
" <Leader>ar (visual mode):  add selected lines as a new range
" <Leader>ar (normal mode):  add current line as a new range
" <Leader>cr (normal mode):  clear all ranges
"
" Commands:
" Rangecommand:  executes the arguments as an ex command across all the ranges
" specified.

" TODO:
" Allow for range inversion.
"
" Possible options (if people actually ask for them):
"
" Consolidate ranges  (overlapped ranges become  one range) -- only  works for
" sorted  ranges --  not  consolidating allows  for  recursive action  (easily
" avoided by not specifying overlapped ranges)
"
" Sort ranges (whether  commands should be applied to the  ranges in the order
" they were specified or whether the range list should be sorted)
"
" Initially, don't consolidate or sort ranges

let s:numRanges = 0

vmap <Leader>ar :call AddRange( line( "'<" ), line ( "'>" ) )<CR>
nmap <Leader>ar :call AddRange( line( "." ), line ( "." ) )<CR>
nmap <Leader>cr :Clearranges<CR>

function! AddRange( l1, l2 ) range
  let s:range{s:numRanges} = a:l1 . ',' . a:l2
  let s:numRanges = s:numRanges + 1

  let i = a:l1
  while ( i <= a:l2 )
    execute "syn match Ranges '\\%" . i . "l.*' containedin=ALL"
    let i = i + 1
  endwhile
endfunction

function! ShowRanges()
  let i = 0
  while ( i < s:numRanges )
    echo s:range{i}
    let i = i + 1
  endwhile
endfunction

function! RangeCommand( theCommand )
  let i = 0
  while ( i < s:numRanges )
    execute s:range{i} . ' ' . a:theCommand
    let i = i + 1
  endwhile
endfunction
com! -nargs=+ -complete=command Rangecommand call RangeCommand( <q-args> )

function! ClearRanges()
  syn clear Ranges
  let s:numRanges = 0
endfunction
com! Clearranges call ClearRanges()

hi Ranges guifg=grey90 guibg=black gui=reverse
