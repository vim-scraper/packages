" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/multipleRanges.vim" {{{
" LAST MODIFICATION: "Tue, 20 Aug 2002 11:20:55 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version history:
"
" 1.8: No new functionality; brought the plugin up to version 7.2, using Lists and removing the dependence on variableSort.vim (getVar.vim is still required).
"
" 1.7: Added a new command (Rangecommandnormal) which allows for a normal-mode
" command to be  executed on the ranges. Also added  mappings to quickly enter
" the Rangecommand and  Rangecommandnormal command-lines. Added error-checking
" in  case there  are  no  ranges currently  selected  when  the commands  are
" executed.
"
" 1.65: Removed the <unique> keyword  from the internal script mappings; Vikas
" Agnihotri  pointed  out that  this  was  causing  a problem  with  potential
" reloading of the script while not adding anything to the ease of use.
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
" <Leader>rc (normal mode):  Go   to   the   command-line   with   the    word
"                            'Rangecommand' typed  in, waiting for  the actual
"                            command.
" <Leader>rn (normal mode):  Go   to   the   command-line   with   the    word
"                            'Rangecommandnormal'  typed in,  waiting for  the
"                            actual command.
"
" Commands:
" Rangecommand:  executes the arguments as an ex command across all the ranges
" specified.
" Rangecommandnormal:  executes a  normal-mode sequence  of keystrokes  on the
" ranges.  For  example,  to  convert  a set  of  ranges  to  upper-case,  use
" :Rangecommandnormal gU -- this will behave as if each of the ranges had been
" selected visually (line-wise) and then gU hit.
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


if exists("loaded_multipleRanges")
  finish
endif
let loaded_multipleRanges = 1

if ( !hasmapto( '<Plug>AddRange', 'v' ) )
  vmap <unique> <Leader>ar <Plug>AddRange
endif
vmap <silent> <script> <Plug>AddRange :call <SID>AddRange( line( "'<" ), line ( "'>" ) )<CR>

if ( !hasmapto( '<Plug>AddRange', 'n' ) )
  nmap <unique> <Leader>ar <Plug>AddRange
endif
nmap <silent> <script> <Plug>AddRange :call <SID>AddRange( line( "." ), line ( "." ) )<CR>

if ( !hasmapto( '<Plug>ClearRanges', 'n' ) )
  nmap <unique> <Leader>cr <Plug>ClearRanges
endif
nmap <silent> <script> <Plug>ClearRanges :Clearranges<CR>

if ( !hasmapto( '<Plug>InvertRanges', 'n' ) )
  nmap <unique> <Leader>ir <Plug>InvertRanges
endif
nmap <silent> <script> <Plug>InvertRanges :Invertranges<CR>

if ( !hasmapto( '<Plug>RangeCommand', 'n' ) )
  nmap <unique> <Leader>rc <Plug>RangeCommand
endif
nmap <script> <Plug>RangeCommand :Rangecommand<Space>

" This  isn't called  <Plug>RangeCommandNormal because  of the  ambiguity with
" <Plug>RangeCommand which causes Vim to pause until the input times out.
if ( !hasmapto( '<Plug>NormalangeCommand', 'n' ) )
  nmap <unique> <Leader>rn <Plug>NormalRangeCommand
endif
nmap <script> <Plug>NormalRangeCommand :Rangecommandnormal<Space>


function! <SID>AddRange( l1, l2 ) range
  " SALMAN: Replace with len( b:multipleRanges_ranges )
  call Allocate( 'b:multipleRanges_numRanges', 0 )
  call Allocate( 'b:multipleRanges_ranges', [] )

  let b:multipleRanges_ranges += [ [ a:l1, a:l2 ] ]

  let b:multipleRanges_numRanges += 1

  let i = a:l1
  while ( i <= a:l2 )
    execute "syn match Ranges '\\%" . i . "l.*' containedin=ALL"
    let i = i + 1
  endwhile
endfunction

function! CompareRanges( range1, range2 )
  return a:range1[ 0 ] == a:range2[ 0 ] ? 0 : a:range1[ 0 ] > a:range2[ 0 ] ? 1 : -1
endfunction
let s:CompareRangeReference = function( "CompareRanges" )

" Used to sort the  ranges -- just calls the sorting  routine specified in the
" variableSort plugin.
function! <SID>SortRanges()
  call sort( b:multipleRanges_ranges, s:CompareRangeReference )
endfunction
com! Sortranges call s:SortRanges()

function! <SID>ConsolidateRanges()
  call s:SortRanges()

  " Showranges

  let numConsolidations = 0
  let i = 0
  let j = i + 1
  while ( j < b:multipleRanges_numRanges )
    if ( b:multipleRanges_ranges[ i ][ 1 ] >= ( b:multipleRanges_ranges[ j ][ 0 ] - 1 ) )
      " echo "Consolidating " . i . " and " . j
      let b:multipleRanges_ranges[ i ][ 1 ] = max( [ b:multipleRanges_ranges[ i ][ 1 ], b:multipleRanges_ranges[ j ][ 1 ] ] )
      let b:multipleRanges_ranges[ j ][ 0 ] = -1
      let numConsolidations = numConsolidations + 1
    else
      let i = j
    endif
    let j = j + 1
  endwhile

  " Showranges

  let i = 0
  let j = -1
  while ( i < b:multipleRanges_numRanges )
    if ( b:multipleRanges_ranges[ i ][ 0 ] == -1 )
      if ( j == -1 )
        let j = i
        " echo "Found a blank at " . j
      endif
    elseif ( j != -1 )
      " echo "Found a candidate at " . i
      let b:multipleRanges_ranges[ j ][ 0 ] = b:multipleRanges_ranges[ i ][ 0 ]
      let b:multipleRanges_ranges[ j ][ 1 ] = b:multipleRanges_ranges[ i ][ 1 ]
      let b:multipleRanges_ranges[ i ][ 0 ] = -1
      let i = j
      let j = -1
    endif

    let i = i + 1
  endwhile

  " Showranges

  let i = b:multipleRanges_numRanges - numConsolidations
  while ( i < b:multipleRanges_numRanges )
    unlet b:multipleRanges_ranges[ i ][ 0 ]
    unlet b:multipleRanges_ranges[ i ][ 1 ]
    " echo b:multipleRanges_ranges[ i ][ 0 ]
    let i = i + 1
  endwhile
  let b:multipleRanges_numRanges = b:multipleRanges_numRanges - numConsolidations

  " Showranges
endfunction
com! Consolidateranges call s:ConsolidateRanges()

function! <SID>ShowRanges()
  let i = 0
  while ( i < b:multipleRanges_numRanges )
    let numLines = b:multipleRanges_ranges[ i ][ 1 ] - b:multipleRanges_ranges[ i ][ 0 ] + 1
    echo b:multipleRanges_ranges[ i ][ 0 ] . "-" . b:multipleRanges_ranges[ i ][ 1 ] . " (" . numLines . " line" . (numLines != 1 ? "s" : "") . ")"
    let i = i + 1
  endwhile
endfunction
com! Showranges call s:ShowRanges()

function! <SID>RangeCommand( theCommand )
  if ( !exists( 'b:multipleRanges_ranges' ) )
    return
  endif

  if ( GetVar( 'consolidateRanges', 0 ) == 1 )
    call s:ConsolidateRanges()
  endif

  let i = 0
  while ( i < b:multipleRanges_numRanges )
    execute b:multipleRanges_ranges[ i ][ 0 ] . ',' . b:multipleRanges_ranges[ i ][ 1 ] . ' ' . a:theCommand

    let i = i + 1
  endwhile
endfunction
com! -nargs=+ -complete=command Rangecommand call s:RangeCommand( <q-args> )

function! <SID>RangeCommandNormal( theCommand )
  if ( !exists( 'b:multipleRanges_ranges' ) )
    return
  endif

  if ( GetVar( 'consolidateRanges', 0 ) == 1 )
    call s:ConsolidateRanges()
  endif

  let i = 0
  while ( i < b:multipleRanges_numRanges )
    execute "normal! " . b:multipleRanges_ranges[ i ][ 0 ] . "GV" . b:multipleRanges_ranges[ i ][ 1 ] . "G"
    execute "normal " . a:theCommand

    let i = i + 1
  endwhile
endfunction
com! -nargs=+ -complete=command Rangecommandnormal call s:RangeCommandNormal( <q-args> )

function! <SID>ClearRanges()
  syn clear Ranges

  let b:multipleRanges_numRanges = 0
  let b:multipleRanges_ranges    = []
endfunction
com! Clearranges call s:ClearRanges()

function! <SID>InvertRanges()
  call s:ConsolidateRanges()

  " Showranges

  let i    = 0
  let j    = 0
  let newS = [ 1 ]
  let newE = []

  while ( j < b:multipleRanges_numRanges )
    let newE += [ b:multipleRanges_ranges[ j ][ 0 ] - 1 ]
    let i = i + 1
    let newS += [ b:multipleRanges_ranges[ j ][ 1 ] + 1 ]
    let j = j + 1
  endwhile

  let newE += [ line( '$' ) ]

  Clearranges

  let j = 0
  while ( j <= i )
    " echo newS[ j ] . ", " . newE[ j ]
    if ( newE[ j ] >= newS[ j ] )
      " echo "Yes"
      call s:AddRange( newS[ j ], newE[ j ] )
    endif

    let j = j + 1
  endwhile

  " Showranges
endfunction
com! Invertranges call s:InvertRanges()

" the highlight colors to use for the range
execute "hi Ranges guifg=" . GetVar( "multipleRangesGuifg", "grey90" ) . " guibg=" . GetVar( "multipleRangesGuibg", "black" ) . " gui=" . GetVar( "multipleRangesGui", "reverse" )
