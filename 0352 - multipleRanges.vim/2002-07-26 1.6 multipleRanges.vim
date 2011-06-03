" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/multipleRanges.vim" {{{
" LAST MODIFICATION: "Fri, 26 Jul 2002 14:51:49 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version history:

"
" 1.6: Added  support for  user-defined highlights for  the ranges  (GUI only;
" will do cterm if asked).  Suggested by Vikas Agnihotri.
"
" 1.5:  Changed  the  variables  within   to  be  buffer-specific  instead  of
" window-specific. This was causing problems when another buffer was opened in
" the same window.
"
" Forgot to  add dependency  information; depends  on my  variableSort.vim and
" getVar.vim scripts.
"
" Main feature addition: New mapping to  invert the ranges (all bits that were
" in a range  become deselected and everything else becomes  part of a range).
" Warning: can be  slow in a large  file because of all  the highlighting that
" has to be done.  Suggested by Dan Sharp.
"
" 1.2: Cleans  up better once  a set of ranges  is cleared (the  variables are
" unlet).
"
" Added  an  option (buffer  or  global  variable called  'consolidateRanges')
" which,  if  set  to 1,  will  cause  Rangecommand  to  first sort  and  then
" consolidate  overlapping ranges  into a  single range  before executing  the
" command. The ranges will remain consolidated once the command is done.  (The
" default is NOT to do the consolidation.)
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
" <Leader>ir (normal mode):  invert ranges
"
" Commands:
" Rangecommand:  executes the arguments as an ex command across all the ranges
" specified.
" Showranges: spews the  list of ranges in the list  (in case they've scrolled
" of the screen or something); just  a convenience feature. Not a particularly
" pretty display or anything.
"
" Options (can  be set globally  as g:<variable> or  on a per-buffer  basis as
" b:<variable>):
" consolidateRanges (default 0):  whether or not to sort and consolidate
" overlapping ranges before executing commands
"
" multipleRangesGuifg (default  'grey90'): default highlight  background color
" for range
" multipleRangesGuibg  (default 'black'):  default highlight  foreground color
" for range
" multipleRangesGui (default 'reverse'):  default highlight gui for range
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

if ( !hasmapto( '<Plug>InvertRanges', 'n' ) )
  nmap <unique> <Leader>ir <Plug>InvertRanges
endif
nmap <silent> <unique> <script> <Plug>InvertRanges :Invertranges<CR>


function! <SID>AddRange( l1, l2 ) range
  " first range for this window
  if ( !exists( 'b:numRanges' ) )
    let b:numRanges = 0
  endif

  let b:rangeS{b:numRanges} = a:l1
  let b:rangeE{b:numRanges} = a:l2
  let b:numRanges = b:numRanges + 1

  let i = a:l1
  while ( i <= a:l2 )
    execute "syn match Ranges '\\%" . i . "l.*' containedin=ALL"
    let i = i + 1
  endwhile
endfunction

" the swap function used by the variableSort plugin
function! MultipleRangeSwap( sub1, sub2 )
  let temp = b:rangeS{a:sub1}
  let b:rangeS{a:sub1} = b:rangeS{a:sub2}
  let b:rangeS{a:sub2} = temp

  let temp = b:rangeE{a:sub1}
  let b:rangeE{a:sub1} = b:rangeE{a:sub2}
  let b:rangeE{a:sub2} = temp
endfunction

" Used to sort the  ranges -- just calls the sorting  routine specified in the
" variableSort plugin.
function! <SID>SortRanges()
  call SortArray( "b:rangeS", 0, b:numRanges - 1, 1, "MultipleRangeSwap" )
endfunction

function! <SID>ConsolidateRanges()
  call s:SortRanges()

  " Showranges

  let numConsolidations = 0
  let i = 0
  let j = i + 1
  while ( j < b:numRanges )
    if ( b:rangeE{i} >= ( b:rangeS{j} - 1 ) )
      " echo "Consolidating " . i . " and " . j
      let b:rangeE{i} = s:Max( b:rangeE{i}, b:rangeE{j} )
      let b:rangeS{j} = -1
      let numConsolidations = numConsolidations + 1
    else
      let i = j
    endif
    let j = j + 1
  endwhile

  " Showranges

  let i = 0
  let j = -1
  while ( i < b:numRanges )
    if ( b:rangeS{i} == -1 )
      if ( j == -1 )
        let j = i
        " echo "Found a blank at " . j
      endif
    elseif ( j != -1 )
      " echo "Found a candidate at " . i
      let b:rangeS{j} = b:rangeS{i}
      let b:rangeE{j} = b:rangeE{i}
      let b:rangeS{i} = -1
      let i = j
      let j = -1
    endif

    let i = i + 1
  endwhile

  " Showranges

  let i = b:numRanges - numConsolidations
  while ( i < b:numRanges )
    unlet b:rangeS{i}
    unlet b:rangeE{i}
    " echo b:rangeS{i}
    let i = i + 1
  endwhile
  let b:numRanges = b:numRanges - numConsolidations

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
  while ( i < b:numRanges )
    let numLines = b:rangeE{i} - b:rangeS{i} + 1
    echo b:rangeS{i} . "-" . b:rangeE{i} . " (" . numLines . " line" . (numLines != 1 ? "s" : "") . ")"
    let i = i + 1
  endwhile
endfunction
com! Showranges call s:ShowRanges()

function! <SID>RangeCommand( theCommand )
  if ( GetVar( 'consolidateRanges', 0 ) == 1 )
    call s:ConsolidateRanges()
  endif

  let i = 0
  while ( i < b:numRanges )
    execute b:rangeS{i} . ',' . b:rangeE{i} . ' ' . a:theCommand
    let i = i + 1
  endwhile
endfunction
com! -nargs=+ -complete=command Rangecommand call s:RangeCommand( <q-args> )

function! <SID>ClearRanges()
  syn clear Ranges

  let i = 0
  while ( i < b:numRanges )
    unlet b:rangeS{i}
    unlet b:rangeE{i}

    let i = i + 1
  endwhile

  let b:numRanges = 0
endfunction
com! Clearranges call s:ClearRanges()

function! <SID>InvertRanges()
  call s:ConsolidateRanges()

  let i = 0
  let j = 0
  let newS{i} = 1

  while ( j < b:numRanges )
    let newE{i} = b:rangeS{j} - 1
    let i = i + 1
    let newS{i} = b:rangeE{j} + 1
    let j = j + 1
  endwhile

  let newE{i} = line( '$' )

  Clearranges

  let j = 0
  while ( j <= i )
    " echo newS{j} . ", " . newE{j}
    if ( newE{j} >= newS{j} )
      " echo "Yes"
      call s:AddRange( newS{j}, newE{j} )
    endif

    let j = j + 1
  endwhile
endfunction
com! Invertranges call s:InvertRanges()

" the highlight colors to use for the range
execute "hi Ranges guifg=" . GetVar( "multipleRangesGuifg", "grey90" ) . " guibg=" . GetVar( "multipleRangesGuibg", "black" ) . " gui=" . GetVar( "multipleRangesGui", "reverse" )
