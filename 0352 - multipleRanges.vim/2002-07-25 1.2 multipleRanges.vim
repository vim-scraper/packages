" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/multipleRanges.vim" {{{
" LAST MODIFICATION: "Thu, 25 Jul 2002 17:10:05 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version history:
"
" 1.2: Cleans  up better once  a set of ranges  is cleared (the  variables are
" unlet).
"
" Added  an  option (buffer  or  global  variable called  'consolidateRanges')
" which,  if  set  to 1,  will  cause  Rangecommand  to  first sort  and  then
" consolidate  overlapping ranges  into a  single range  before executing  the
" command. The ranges will remain consolidated once the command is done.
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
"
" Range  highlighting  is  done  through  the Ranges  syntax  item;  this  can
" obviously be  changed to anything  the user desires. Suggestion:  a slightly
" lighter background than used in Visual mode.

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


function! <SID>AddRange( l1, l2 ) range
  " first range for this window
  if ( !exists( 'w:numRanges' ) )
    let w:numRanges = 0
  endif

  let w:rangeS{w:numRanges} = a:l1
  let w:rangeE{w:numRanges} = a:l2
  let w:numRanges = w:numRanges + 1

  let i = a:l1
  while ( i <= a:l2 )
    execute "syn match Ranges '\\%" . i . "l.*' containedin=ALL"
    let i = i + 1
  endwhile
endfunction

" the swap function used by the variableSort plugin
function! MultipleRangeSwap( sub1, sub2 )
  let temp = w:rangeS{a:sub1}
  let w:rangeS{a:sub1} = w:rangeS{a:sub2}
  let w:rangeS{a:sub2} = temp

  let temp = w:rangeE{a:sub1}
  let w:rangeE{a:sub1} = w:rangeE{a:sub2}
  let w:rangeE{a:sub2} = temp
endfunction

" Used to sort the  ranges -- just calls the sorting  routine specified in the
" variableSort plugin.
function! SortRanges()
  call SortArray( "w:rangeS", 0, w:numRanges - 1, 1, "MultipleRangeSwap" )
endfunction

function! ConsolidateRanges()
  call SortRanges()

  " Showranges

  let numConsolidations = 0
  let i = 0
  let j = i + 1
  while ( j < w:numRanges )
    if ( w:rangeE{i} >= ( w:rangeS{j} - 1 ) )
      " echo "Consolidating " . i . " and " . j
      let w:rangeE{i} = s:Max( w:rangeE{i}, w:rangeE{j} )
      let w:rangeS{j} = -1
      let numConsolidations = numConsolidations + 1
    else
      let i = j
    endif
    let j = j + 1
  endwhile

  " Showranges

  let i = 0
  let j = -1
  while ( i < w:numRanges )
    if ( w:rangeS{i} == -1 )
      if ( j == -1 )
        let j = i
        " echo "Found a blank at " . j
      endif
    elseif ( j != -1 )
      " echo "Found a candidate at " . i
      let w:rangeS{j} = w:rangeS{i}
      let w:rangeE{j} = w:rangeE{i}
      let w:rangeS{i} = -1
      let i = j
      let j = -1
    endif

    let i = i + 1
  endwhile

  " Showranges

  let i = w:numRanges - numConsolidations
  while ( i < w:numRanges )
    unlet w:rangeS{i}
    unlet w:rangeE{i}
    " echo w:rangeS{i}
    let i = i + 1
  endwhile
  let w:numRanges = w:numRanges - numConsolidations

  " Showranges
endfunction

" Returns the higher of two numbers
function! <SID>Max( n1, n2 )
  if ( a:n1 > a:n2 )
    return a:n1
  else
    return a:n2
  endif
endfunction

function! <SID>ShowRanges()
  let i = 0
  while ( i < w:numRanges )
    let numLines = w:rangeE{i} - w:rangeS{i} + 1
    echo w:rangeS{i} . "-" . w:rangeE{i} . " (" . numLines . " line" . (numLines != 1 ? "s" : "") . ")"
    let i = i + 1
  endwhile
endfunction
com! Showranges call s:ShowRanges()

function! <SID>RangeCommand( theCommand )
  if ( GetVar( 'consolidateRanges', 0 ) == 1 )
    call ConsolidateRanges()
  endif

  let i = 0
  while ( i < w:numRanges )
    execute w:rangeS{i} . ',' . w:rangeE{i} . ' ' . a:theCommand
    let i = i + 1
  endwhile
endfunction
com! -nargs=+ -complete=command Rangecommand call s:RangeCommand( <q-args> )

function! <SID>ClearRanges()
  syn clear Ranges

  let i = 0
  while ( i < w:numRanges )
    unlet w:rangeS{i}
    unlet w:rangeE{i}

    let i = i + 1
  endwhile

  let w:numRanges = 0
endfunction
com! Clearranges call s:ClearRanges()

hi Ranges guifg=grey90 guibg=black gui=reverse
