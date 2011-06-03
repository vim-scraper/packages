" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
" | FRI 17TH SEP 2010:   1.7                                                    |
" |                      Made improvements to scroll till cursor is on top and  |
" |                      cursor is at bottom cases.  These work very reliably   |
" |                      when in folded mode.  The center case is not quite     |
" |                      stable but kind of works, there are still some         |
" |                      glitches in folded mode and when the buffer is         |
" |                      scrolled near the bottom.                              |
" | MON 16TH AUG 2010:   1.6                                                    |
" |                      Had a slight glitch that happened but not always when  |
" |                      you did Shift-ESC, the scrolling would just continue.  |
" |                      Not sure why but fixed it and checked it so that this  |
" |                      does not happen.  Had a bug that caused the continuing |
" |                      of scrolling when line wrapping was on. This gave      |
" |                      incorrect readings for the window height so that       |
" |                      scrolling would continue.  Fixed this by putting in a  |
" |                      check that warns you if wrap is on, and doesn't scroll.|
" | MON 16TH AUG 2010:   1.5                                                    |
" |                      Sorted out some funky messy logic where 'zb' equivalent|
" |                      in normal vim in Smooth Center did not work, if the    |
" |                      cursor was parked on the last line. (S-ESC).  Nothing  |
" |                      happened.  Obviously fixed it now so that it does.     |
" | SAT 8TH AUG 2010:    1.4                                                    |
" |                      Fixed a subtle bug that meant that if scrolloff>0 the  |
" |                      display would keep on centering but not immediately.   |
" |                      If you were at the top of bottom having done S-ESC or  |
" |                      S-F1 with scrolloff>0 then the display would stop      |
" |                      scrolling then, but then later on when you moved off   |
" |                      that line the display would keep on trying to scroll.  |
" |                      This was to do with where I had attempted to account   |
" |                      for the 'scrolloff' value in the centering bit of my   |
" |                      code.  Made the command line clear so that you don't   |
" |                      see a silly 'call StartSmoothCentering()' at the bottom|
" |                      of the screen each time.                               |
" |                      1.3                                                    |
" |                      Fixed the glitch where if you had a scrolloff>0 set    |
" |                      it would carry on scrolling if you were at the top     |
" |                      and bottom and you did a S-ESC or S-F1.  Now correctly |
" |                      identifies the scrolloff setting and accounts for this |
" |                       value, not just the cursors position in the window.   |
" |                      Changed key mappings to a more natural S-ESC gets you  |
" |                      'out of the code', whereas S-F1 scrolls down the file. |
" |                      Feels more natural because you think of ESC as getting |
" |                      you 'out' of something, rather than go deeper into it  |
" |                      the way it was previously.                             |
" +-----------------------------------------------------------------------------+

autocmd CursorHold * call DoSmoothCenter()

" This variable controls whether smooth center should be 'centering'
let g:centermode = 0

" this maps Escape key to start centering
map <Esc> :call StartSmoothCentering(1)<CR>:<BS>
map <S-F1> :call StartSmoothCentering(2)<CR>:<BS>
map <S-Esc> :call StartSmoothCentering(3)<CR>:<BS>

function StartSmoothCentering(centermode)
:	if g:centermode==0
:		let g:centermode = a:centermode
:		return
:	endif
:	let g:centermode = 0
endfunction

" do centering if g:centermode set, this acts as the 'control' flag 
function DoSmoothCenter()
:	if g:centermode == 0
:		return
:	endif
" -- centering mode
:	if g:centermode == 1
:		let hiddenlines = line("w$")-line("w0")-winheight(0)
:		let targetline = line("w0")+hiddenlines+line("w$")
:		let endshortfall = winheight(0)-(line("w$")-line("w0"))
:		if endshortfall<0
:			let endshortfall = 0
:		endif
:		let targetline = targetline/2
:		if line(".")>targetline
:			exe "normal \<C-e>"
:		elseif line(".")<targetline
:			exe "normal \<C-y>"
:		else
:			let g:centermode = 0
:		endif
:	endif
" -- scroll up screen such that pov moves down file
:	if g:centermode == 2
:		if line("w0")<line(".")
:			exe "normal \<C-e>"
:		else
:			let g:centermode = 0
:		endif
:	endif
" -- scroll down screen such that pov moves up file
:	if g:centermode == 3
:		if line("w$")>line(".")
:			if line("w0")==1
:				let g:centermode = 0
:			else
:				exe "normal \<C-y>"
:			endif
:		else
:			let g:centermode = 0
:		endif
:	endif
endfunction

" +-----------------------------------------------------------------------------+
" | END                                                                         |
" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+

