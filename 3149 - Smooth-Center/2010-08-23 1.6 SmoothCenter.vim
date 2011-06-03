" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
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
" | WED 5TH AUG 2010:    1.2                                                    |
" |                      Had a bug where the end cases were not being detected  |
" |                      properly, the top of the file and the end of the file  |
" |                      respectively. I got them confused, so it looked like   |
" |                      the scrolling wouldn't start if the first line was     |
" |                      showing when you press S-Esc.                          |
" +-----------------------------------------------------------------------------+

autocmd CursorHold * call DoSmoothCenter()

" This variable controls whether smooth center should be 'centering'
let g:smoothcentering = 0

" this maps Escape key to start centering
map <Esc> :call StartSmoothCentering(1)<CR>:<BS>
map <S-F1> :call StartSmoothCentering(2)<CR>:<BS>
map <S-Esc> :call StartSmoothCentering(3)<CR>:<BS>

function StartSmoothCentering(centermode)
:	if g:smoothcentering==0
:		if eval("&wrap")==1
:			echo input("smooth centering not supported with wrap!!")
:		else
:			let g:smoothcentering = a:centermode
:			if a:centermode==1
:				let g:movinglinevector = (line("w0")+line("w0")+winheight(0))/2
:			endif
:		endif
:		return
:	endif
:	let g:smoothcentering = 0
endfunction

" do centering if g:smoothcentering set, this acts as the 'control' flag 
function DoSmoothCenter()
:	if g:smoothcentering == 0
:		return
:	endif
" -- centering case
:	if g:smoothcentering == 1
:		let movinglinevector = (line("w0")+line("w0")+winheight(0))/2
:		if movinglinevector>line("$")
:			let g:smoothcentering = 0
:			return
:		endif
:		if line("w0")==1 && line(".")<movinglinevector
:			let g:smoothcentering = 0
:			return
:		endif
:	endif
" -- i scroll up screen such that user pov move down file?
:	if g:smoothcentering == 2
:		let movinglinevector = line("w0")+eval("&scrolloff")
:		if line("w0")==line("$")
:			let g:smoothcentering = 0
:			return
:		endif
:	endif
" -- i scroll down screen such that pov moves up file?
:	if g:smoothcentering == 3
:		let movinglinevector = line("w0")+winheight(0)-1-eval("&scrolloff")
:		if line("w0")==1
:			let g:smoothcentering = 0
:			return
:		endif
:	endif
:	let changeflag = 0
" -- set scroll up screen such that pov moves down file
:	if line(".")>movinglinevector
:		let changeflag = 1 
:	endif
" -- set scroll down screen such that pov moves up file
:	if line(".")<movinglinevector
:		let changeflag = 2
:	endif
:	if line(".")==movinglinevector
:		let g:smoothcentering = 0
:	endif
" -- do scroll up screen such that user pov move down file 
:	if changeflag==1
:       	exe "normal \<C-e>"
:	endif
" -- do scroll down screen such that user pov move up file
:	if changeflag==2
:		exe "normal \<C-y>"
:	endif
endfunction

" +-----------------------------------------------------------------------------+
" | END                                                                         |
" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+

