" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/multipleRanges.vim" {{{
" LAST MODIFICATION: "Thu, 25 Jul 2002 10:19:44 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version history:
"
" 1.1: Added the Showranges command to spew the currently added ranges.
"
" Cleaned it up to be more plugin-aware  (checks to make sure it isn't already
" loaded and  allows for mappings  to be changed  by the user);  also, changed
" method signatures to be contained within the script to avoid possible naming
" conflicts elsewhere.

" 1.0:  Initial upload
"
" Usage:
"
" Maps (default):
" <Leader>ar (visual mode):  add selected lines as a new range
" <Leader>ar (normal mode):  add current line as a new range
" <Leader>cr (normal mode):  clear all ranges
"
" Commands:
" Rangecommand:  executes the arguments as an ex command across all the ranges
" specified.
" Showranges: spews the  list of ranges in the list  (in case they've scrolled
" of the screen or something); just  a convenience feature. Not a particularly
" pretty display or anything.
"
" Note  that substitute  operations (such  as through  the :s  command) should
" probably be ended with  the 'e' flag (no error if  nothing found) since each
" range is acted  upon individually and it's possible that  a particular range
" will  not have  any  matches. The  script  will not  abort  but each  failed
" substitute will generate an error which may not be what the user wants.

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


if exists("loaded_multipleRanges")
  finish
endif
let loaded_multipleRanges = 1

if ( !hasmapto( '<Plug>AddRange', 'v' ) )
  vmap <unique> <Leader>ar <Plug>AddRange
endif
vmap <silent> <unique> <script> <Plug>AddRange :call <SID>AddRange( line( "'<" ), line ( "'>" ) )<CR>

if ( !hasmapto( '<Plug>AddRange', 'n' ) )
  nmap <unique> <Leader>ar <Plug>AddRange
endif
nmap <silent> <unique> <script> <Plug>AddRange :call <SID>AddRange( line( "." ), line ( "." ) )<CR>

if ( !hasmapto( '<Plug>ClearRanges', 'n' ) )
  nmap <unique> <Leader>cr <Plug>ClearRanges
endif
nmap <silent> <unique> <script> <Plug>ClearRanges :Clearranges<CR>


let s:numRanges = 0


function! <SID>AddRange( l1, l2 ) range
  let s:range{s:numRanges} = a:l1 . ',' . a:l2
  let s:numRanges = s:numRanges + 1

  let i = a:l1
  while ( i <= a:l2 )
    execute "syn match Ranges '\\%" . i . "l.*' containedin=ALL"
    let i = i + 1
  endwhile
endfunction

function! <SID>ShowRanges()
  let i = 0
  while ( i < s:numRanges )
    echo s:range{i}
    let i = i + 1
  endwhile
endfunction
com! Showranges call s:ShowRanges()

function! <SID>RangeCommand( theCommand )
  let i = 0
  while ( i < s:numRanges )
    execute s:range{i} . ' ' . a:theCommand
    let i = i + 1
  endwhile
endfunction
com! -nargs=+ -complete=command Rangecommand call s:RangeCommand( <q-args> )

function! <SID>ClearRanges()
  syn clear Ranges
  let s:numRanges = 0
endfunction
com! Clearranges call s:ClearRanges()

hi Ranges guifg=grey90 guibg=black gui=reverse
