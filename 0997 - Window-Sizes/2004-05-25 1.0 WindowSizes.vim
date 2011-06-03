" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/WindowSizes.vim" {{{
" LAST MODIFICATION: "Tue, 25 May 2004 14:14:43 Eastern Daylight Time"
" (c) Copyright 2004 Hewlett-Packard Development Company, L.P.
" $Id:$ }}}
"
" Version 1.0

" Plugin to keep the vertical sizes of windows within specifications.  For
" example, if you're using the Decho.vim plugin, you could force the DBG
" window to always be 10 lines high and for all other windows to automatically
" maximize upon entry.  Or, you could specify explicit sizes for two windows
" and let all the remaining space be shared equally among the other windows.
"
" This is a bit different from using equalalways and winheight=999 because it
" will always honor the vertical height settings of individual windows, using
" the maximizing or equalization only on those windows that don't have
" anything set explicitly.
"
" Caveat (and it's a big one):  No support for vertical splits.  If you have a
" vertical split, the behavior is undefined (well, it's not undefined, but
" it's ugly).
"
" Usage (default mappings):
"
" <leader>max:  Toggle whether windows without an explicit fixed size will
" automatically maximize upon entry or whether all un-fixed-size windows will
" simply share the vertical space left over once the fixed windows have taken
" their share of the space.
"
" <leader>fix:  If the current window has a preferred fixed vertical size,
" remove it, allowing this window to expand and contract according to the
" g:WindowSizes_windowMax setting.
"
" <s-up>:  Increases the current windows's size by the value of the
" WindowSizes_increment variables (see below); using this mapping results in
" fixing the window height.
"
" <s-down>:  Decreases the current windows's size by the value of the
" WindowSizes_increment variables (see below); using this mapping results in
" fixing the window height.
"
" Variables (configuration):
" g:WindowSizes_windowMax:  If 1, then windows are maximized (the <leader>max
" mapping merely toggles this value between 1 and 0).
"
" b:preferredWindowSize:  If defined, the fixed vertical size of the current
" window -- the window will be this size vertically (tweaked by the
" <leader>fix, <s-up> and <s-down> mappings).  If not defined, a prompt is
" displayed for the user to specify a fixed vertical height for the current
" window where the current height is used as the default value (hit <ESC> to
" get out of it).
"
" g:WindowSizes_increment:  If defined, this is the increment by which to
" increase or decrease the current window's size if using the <s-up> or
" <s-down> mappings.  If not defined, 5 is used.
"
" b:WindowSizes_increment:  If defined in a given window, this is the
" increment by which to increase or decrease the current window's size when
" using the <s-up> or <s-down> mappings.  If not defined, the global version
" (g:WindowSizes_increment) is used (if even THAT's not defined, 5 is used).
"
" Tips:
"
" - Always have at least one window without a specified size so it can take
"   over the remaining vertical space.
"
" - Always make sure that the total size of the fixed windows is less than the
"   actual number of lines visible on-screen!  (The behavior otherwise is
"   undefined -- that is, not tested by me.)
"
" - To restore a window to not having a preferred size, unlet
"   b:preferredWindowSize (just use the <leader>fix mapping).
"
" TODO:
"
" - Observe the 'laststatus' value.
"
" - Figure out how to change all the global variables to script or some such
"   so they don't pollute the global variable pool.
"
" This relies on my getVar.vim (vimscript #353).

if exists( "g:loaded_WindowSizes" )
  finish
endif
let g:loaded_WindowSizes=1

if ( !hasmapto( '<Plug>Max', 'n' ) )
  nmap <unique> <leader>max <Plug>Max
endif

if ( !hasmapto( '<Plug>Fix', 'n' ) )
  nmap <unique> <leader>fix <Plug>Fix
endif

if ( !hasmapto( '<Plug>SizeUp', 'n' ) )
  nmap <unique> <s-up> <Plug>SizeUp
endif

if ( !hasmapto( '<Plug>SizeDown', 'n' ) )
  nmap <unique> <s-down> <Plug>SizeDown
endif

nnoremap <silent> <Plug>Max :let g:WindowSizes_windowMax=1 - GetVar( "WindowSizes_windowMax", 0 )<cr>:call SetPreferredSize()<cr>
nnoremap <silent> <Plug>Fix :call TogglePreferredSize()<cr>

nnoremap <silent> <Plug>SizeUp :let b:preferredWindowSize=winheight( 0 ) + GetVar( "WindowSizes_increment", 5 )<cr>:call SetPreferredSize()<cr>
nnoremap <silent> <Plug>SizeDown :let b:preferredWindowSize=winheight( 0 ) - GetVar( "WindowSizes_increment", 5 )<cr>:call SetPreferredSize()<cr>

augroup WindowSizing
  au!
  au WinEnter * call SetPreferredSize()
augroup END

function! SetPreferredSize()
  " No sense in having all the events triggered as we execute the Windo
  " functionality below
  let savedEventIgnore=SaveOpts('ei')
  set ei=all

  let g:WindowSizes_currentWindow=winnr()

  " If current window has a preferred height, temporarily disable WindowSizes_windowMax so
  " the non-preferred windows can be treated accordingly.
  if ( VarExists( "preferredWindowSize" ) )
    let savedVar=SaveVar( "g:WindowSizes_windowMax", 0 )
  endif

  " Figure out total preferred heights and total number of non-preferred
  " windows
  let g:WindowSizes_preferredHeights=0
  let g:WindowSizes_numNonPreferred=0
  Windo if ( VarExists( "preferredWindowSize" ) ) |
        \   let g:WindowSizes_preferredHeights = g:WindowSizes_preferredHeights + GetVar( "preferredWindowSize" ) + 1 |
        \ else |
        \   let g:WindowSizes_numNonPreferred = g:WindowSizes_numNonPreferred + 1 |
        \ endif
  let s:numLinesOnScreen=&lines - &cmdheight - g:WindowSizes_preferredHeights
  let s:hasWindowMax=GetVar( "WindowSizes_windowMax", 0 )

  " If the division isn't even, things get fun -- probably do something
  " like add the modulus to the first modulus windows when returning the
  " value -- so, if dividing by 3 and we get 2, the first 2 windows get 1+
  " the actual division and the third does not. . .
  if ( !s:hasWindowMax )
    let s:individualHeights=s:numLinesOnScreen / g:WindowSizes_numNonPreferred
    let s:individualMod=s:numLinesOnScreen % g:WindowSizes_numNonPreferred
  endif

  " Resize all windows
  Windo call ResizeCurrentWindow( g:WindowSizes_currentWindow )

  execute savedEventIgnore

  " Restore WindowSizes_windowMax if it was taken away
  if ( VarExists( "preferredWindowSize" ) )
    execute savedVar
  endif
endfunction

" Count the total number of windows that don't have a preferred size.  Also,
" count the total number of 'preferred' lines per window + 1 per window
" (status line).  Then, subtract this from ( &lines - &cmdheight ) to get the
" total number of lines available for the non-specified windows.  Then, if max
" is NOT set, divide evenly and set each of the non-specified windows to that
" height.  If max IS set, set each window to &winminheight and subtract, for
" each window, 1 (for status line) + &winminheight from the total and set
" current window to that number - 1 (again, for status line).
function! HeightOfCurrentWindow( cursorWindow )
  let windowHeight=GetVar( "preferredWindowSize" )

  if ( windowHeight == -1 )
    " If maximizing the non-preferred windows, maximize the current window and
    " make the others as small as allowed by winminheight
    if ( s:hasWindowMax )
      if ( winnr() == a:cursorWindow )
        let windowHeight = s:numLinesOnScreen - ( &winminheight + 1 ) * ( g:WindowSizes_numNonPreferred - 1 )
      else
        let windowHeight = &winminheight + 1
      endif
    else
      let windowHeight = s:individualHeights

      if ( NonPreferredWindowNumber( winnr() ) <= s:individualMod )
        let windowHeight = windowHeight + 1
      endif
    endif
  else
    " The + 1 is for the status line
    let windowHeight = windowHeight + 1
  endif

  return windowHeight
endfunction

" Given the current window number, determines the cardinality of the current
" window in the non-preferred window list
function! NonPreferredWindowNumber( windowNumber )
  " This is a preferred window
  let g:WindowSizes_windowNumber=a:windowNumber
  let g:WindowSizes_result=-1
  let g:WindowSizes_counts=0

  Windo if ( !VarExists( "preferredWindowSize" ) ) |
        \ let g:WindowSizes_counts = g:WindowSizes_counts + 1 |
        \ if ( winnr() == g:WindowSizes_windowNumber ) |
        \ let g:WindowSizes_result=g:WindowSizes_counts |
        \ endif |
        \ endif

  return g:WindowSizes_result
endfunction

" Resizes the current window, given the window that contained the cursor when
" the command was issued
function! ResizeCurrentWindow( cursorWindow )
  execute "normal " . ( HeightOfCurrentWindow( a:cursorWindow ) - 1 ) . "\<c-w>_"
endfunction

function! TogglePreferredSize()
  if ( exists( "b:preferredWindowSize" ) )
    unlet b:preferredWindowSize
  else
    let b:preferredWindowSize=input( "Enter preferred size:  ", winheight( 0 ) )

    " The either entered nothing or hit escape (more likely)
    if ( b:preferredWindowSize == '' )
      unlet b:preferredWindowSize
    endif
  endif

  call SetPreferredSize()
endfunction
