" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/bisect.txt	[[[1
80
*bisect.txt*  Plugin for navigation via bisection.

This plugin will allow you to reach a desired location on the visible screen by
performing a sequence of bisections to the left, right, up, and down.  By
default, these are performed using the <C-h>, <C-l>, <C-j>, and <C-k> commands,
respectively.

For example, suppose your cursor is above the desired location on the screen.
Call the BisectDown command, and your cursor will move halfway between its
current location and the bottom of the screen.  Now suppose your cursor is
below the desired position.  Call the BisectUp command, and your cursor will
move to halfway between its current position and its original position.  Repeat
as necessary.  BisectLeft and BisectRight work analagously.  Horizontal and
vertical commands can be interleaved. 

The VisualBisect commands work exactly like the normal Bisect commands in
visual mode.

You do not need to completely finish a bisection before resuming normal
editing.  A hybrid approach is often faster.

Please note that a bisection does not currently end until the cursor moves away
from the last position that resulted from a bisection.  Even if the cursor
moves away and comes back, the previous bisection will not terminate.  You can
manually terminate a bisection by invoking <Plug>StopBisect and
<Plug>VisualStopBisect, in normal and visual modes, respectively.  There
default key mapping for stop bisect is <C-n>.

Also note that this script uses the 's and 'p registers and that this behavior
is not currently configurable.  The `p command can be used to return to the
last bisection result in order to continue the previous bisection, assuming one
of the StopBisect commands has not been called.

Here's an example .vimrc snippet that should use <C-m> for StopBisect as
opposed to the default, <C-n>:

nmap <C-m> <Plug>StopBisect
xmap <C-m> <Plug>VisualStopBisect

And here's another example for using the arrow keys instead of h, j, k, and l.

nmap <C-Left>  <Plug>BisectLeft
nmap <C-Right> <Plug>BisectRight
nmap <C-Up>    <Plug>BisectUp
nmap <C-Down>  <Plug>BisectDown
xmap <C-Left>  <Plug>VisualBisectLeft
xmap <C-Right> <Plug>VisualBisectRight
xmap <C-Up>    <Plug>VisualBisectUp
xmap <C-Down>  <Plug>VisualBisectDown


Normal Mode Mappings:
<C-j>  or  <Plug>BisectDown
    Perform a downward bisection.
<C-k>  or  <Plug>BisectUp
    Perform an upward bisection.
<C-h>  or  <Plug>BisectLeft
    Perform a leftward bisection.
<C-l>  or  <Plug>BisectRight
    Perform a rightward bisection.
<C-n>  or  <Plug>StopBisect
    Manually terminate the last bisection.

Visual Mode Mappings:
<C-j>  or  <Plug>VisualBisectDown
    Perform a downward visual bisection.
<C-k>  or  <Plug>VisualBisectUp
    Perform an upward visual bisection.
<C-h>  or  <Plug>VisualBisectLeft
    Perform a leftward visual bisection.
<C-l>  or  <Plug>VisualBisectRight
    Perform a rightward visual bisection.
<C-n>  or  <Plug>VisualStopBisect
    Manually terminate the last bisection from visual mode.

Registers Used:
's
    Point where selection started before last visual bisection
'p
    Result of last bisection.
plugin/bisect.vim	[[[1
159
" Vim global plugin which allows navigation via bisection
" Last Change: 2010 Feb 3
" Author:      Zachary Michaels |mikezackles
"                               |    @t
"                               |gmail.com
" License:     This file is released under the Vim license.
" Version:     0.0.1

if exists("loaded_bisect")
  finish
endif
let g:loaded_bisect = 1

function! s:StartBisect(invoking_mode)
  let s:running = 1
  let s:current_mode = a:invoking_mode
  let s:top_mark = line('w0') - 1
  let s:bottom_mark = line('w$') + 1
  let s:left_mark = 0
  let s:right_mark = col('$')
  let s:current_col = col('.')
  call setpos("'p", getpos('.')) "Save current position
endfunction

function! s:BisectIsRunning(invoking_mode)
  "We use non-bisect movement as a way of ending a bisect
  "Note that moving away from a location and then coming back
  "will fool this mechanism.
  "Bisects are also terminated when the editing mode has changed.
  "Bind the StopBisect function if you wish the ability to manually stop bisects.
  "NOTE - exists("s:running") implies exists("s:current_mode")
  return exists("s:running") && s:running && getpos("'p") == getpos('.') && a:invoking_mode == s:current_mode
endfunction

function! s:NarrowBoundaries(direction)
  "Notice that we update the value of s:right_mark every time the line changes, in
  "order to account for varying line length
  if a:direction == "up"
    let s:bottom_mark = line('.')
    let l:new_line = s:top_mark + float2nr(ceil((s:bottom_mark - s:top_mark)/2.0))
    let l:extend = (s:right_mark == col('$')) ? 1 : 0 "should we extend right_mark or not?
    call cursor(l:new_line, s:current_col)
    if l:extend
      let s:right_mark = col('$')
    endif
  elseif a:direction == "down"
    let s:top_mark = line('.')
    let l:new_line = s:top_mark + float2nr(floor((s:bottom_mark - s:top_mark)/2.0))
    let l:extend = (s:right_mark == col('$')) ? 1 : 0 "should we extend right_mark or not?
    call cursor(l:new_line, s:current_col)
    if l:extend
      let s:right_mark = col('$')
    endif
  elseif a:direction == "left" && col('.') > s:left_mark && col('.') != 0 "Corner case because of varying line length
    let s:right_mark = col('.')
    let s:current_col = s:left_mark + float2nr(ceil((s:right_mark - s:left_mark)/2.0))
    call cursor(line('.'), s:current_col)
  elseif a:direction == "right" && col('.') > s:left_mark && col('.') != col('$') - 1 && col('.') != col('$')
    let s:left_mark = col('.')
    let l:tmp_right = min([s:right_mark, col('$')]) "This column could be shorter
    let s:current_col = s:left_mark + float2nr(floor((l:tmp_right - s:left_mark)/2.0))
    call cursor(line('.'), s:current_col)
  endif
endfunction

function! s:StopBisect()
  let s:running = 0
endfunction

function! s:Bisect(direction, invoking_mode)
  if !s:BisectIsRunning(a:invoking_mode)
    call s:StartBisect(a:invoking_mode)
  endif

  call s:NarrowBoundaries(a:direction)

  " Start a new bisection if the cursor hasn't moved.
  " This allows the user to 'break out' of a bisection.
  " We only do this once to avoid an infinite loop at the
  " top and bottom of the screen
  if getpos("'p") == getpos('.')
    call s:StartBisect(a:invoking_mode)
    call s:NarrowBoundaries(a:direction)
  endif

  call setpos("'p", getpos('.')) "Save current position
endfunction

" Wrappers for s:Bisect
function! s:NormalBisect(direction)
  call s:Bisect(a:direction, 'n')
endfunction

function! s:VisualBisect(direction)
  if getpos(".") == getpos("'<")
    call setpos("'s", getpos("'>")) "'s for start - saves the position where the visual select started
  elseif getpos(".") == getpos("'>")
    call setpos("'s", getpos("'<"))
  elseif line('.') == line("'<")    "for visual line mode
    call setpos("'s", getpos("'>"))
  else
    call setpos("'s", getpos("'<"))
  endif
  call s:Bisect(a:direction, visualmode())
endfunction

" Normal mode mappings
if !hasmapto('<Plug>BisectDown', 'n')
  nmap <C-j> <Plug>BisectDown
endif
if !hasmapto('<Plug>BisectUp', 'n')
  nmap <C-k> <Plug>BisectUp
endif
if !hasmapto('<Plug>BisectLeft', 'n')
  nmap <C-h> <Plug>BisectLeft
endif
if !hasmapto('<Plug>BisectRight', 'n')
  nmap <C-l> <Plug>BisectRight
endif
if !hasmapto('<Plug>StopBisect', 'n')
  nmap <C-i> <Plug>StopBisect
endif
nnoremap <unique> <script> <Plug>BisectDown <SID>BisectDown
nnoremap <unique> <script> <Plug>BisectUp <SID>BisectUp
nnoremap <unique> <script> <Plug>BisectLeft <SID>BisectLeft
nnoremap <unique> <script> <Plug>BisectRight <SID>BisectRight
nnoremap <unique> <script> <Plug>StopBisect <SID>StopBisect
nnoremap <silent> <SID>BisectDown :call <SID>NormalBisect("down")<CR>
nnoremap <silent> <SID>BisectUp :call <SID>NormalBisect("up")<CR>
nnoremap <silent> <SID>BisectLeft :call <SID>NormalBisect("left")<CR>
nnoremap <silent> <SID>BisectRight :call <SID>NormalBisect("right")<CR>
nnoremap <silent> <SID>StopBisect :call <SID>StopBisect()<CR>

" Visual mode mappings
if !hasmapto('<Plug>VisualBisectDown', 'v')
  xmap <C-j> <Plug>VisualBisectDown
endif
if !hasmapto('<Plug>VisualBisectUp', 'v')
  xmap <C-k> <Plug>VisualBisectUp
endif
if !hasmapto('<Plug>VisualBisectLeft', 'v')
  xmap <C-h> <Plug>VisualBisectLeft
endif
if !hasmapto('<Plug>VisualBisectRight', 'v')
  xmap <C-l> <Plug>VisualBisectRight
endif
if !hasmapto('<Plug>VisualStopBisect', 'v')
  xmap <C-i> <Plug>VisualStopBisect
endif
xnoremap <unique> <script> <Plug>VisualBisectDown <SID>VisualBisectDown
xnoremap <unique> <script> <Plug>VisualBisectUp <SID>VisualBisectUp
xnoremap <unique> <script> <Plug>VisualBisectLeft <SID>VisualBisectLeft
xnoremap <unique> <script> <Plug>VisualBisectRight <SID>VisualBisectRight
xnoremap <unique> <script> <Plug>VisualStopBisect <SID>VisualStopBisect
xnoremap <silent> <SID>VisualBisectDown <ESC>:call <SID>VisualBisect("down")<CR>:exe "normal! `s".visualmode()."`p"<CR>
xnoremap <silent> <SID>VisualBisectUp <ESC>:call <SID>VisualBisect("up")<CR>:exe "normal! `s".visualmode()."`p"<CR>
xnoremap <silent> <SID>VisualBisectLeft <ESC>:call <SID>VisualBisect("left")<CR>:exe "normal! `s".visualmode()."`p"<CR>
xnoremap <silent> <SID>VisualBisectRight <ESC>:call <SID>VisualBisect("right")<CR>:exe "normal! `s".visualmode()."`p"<CR>
xnoremap <silent> <SID>VisualStopBisect :call <SID>StopBisect()<CR>gv
