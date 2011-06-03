" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
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
" | SAT 1ST AUG 2010:    1.1                                                    |
" |                      Cured the glitch that the centering is not centering   |
" |                      when the end of the file is not as far as the bottom   |
" |                      of the window.  Added a couple of new modes, these act |
" |                      just like zt and zb in normal mode. They are shift-ESC |
" |                      and shift-F1 respectively.  So now you've got all zz,  |
" |                      zt & zb as smooth-equivalents: ESC, S-ESC and S-F1     |
" |                      respectively.                                          |
" | WED 8TH JUL 2010:    1.0                                                    |
" |                      Initial revision. There is a small glitch when file is |
" |                      is less then the end of the window, the centering will |
" |                      continue until the cursor is in the center of the      |
" |                      visible area of the file, not the true 'center'.       |
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
:		let g:smoothcentering = a:centermode
:		return
:	endif
:	let g:smoothcentering = 0
endfunction

" Do the centering --if-- flag is set
function DoSmoothCenter()
:	if g:smoothcentering == 0
:		return
:	endif
:	if g:smoothcentering == 1
:		let centerline = (line("w0")+line("w0")+winheight(0))/2
:		if centerline>line("$")
:			let g:smoothcentering = 0
:			return
:		endif
:		if line("w0")==1 && line(".")<centerline
:			let g:smoothcentering = 0
:			return
:		endif
:	endif
:	if g:smoothcentering == 2
:		let centerline = line("w0")+eval("&scrolloff")
:		if line("w0")==line("$")
:			let g:smoothcentering = 0
:			return
:		endif
:	endif
:	if g:smoothcentering == 3
:		let centerline = line("w$")-eval("&scrolloff")
:		if line("w0")==1+eval("&scrolloff")
:			let g:smoothcentering = 0
:			return
:		endif
:	endif
:	let changeflag = 0
:	if line(".")>centerline
:		let changeflag = 1 
:	endif
:	if line(".")<centerline
:		let changeflag = 2
:	endif
:	if line(".")==centerline
:		let g:smoothcentering = 0
:	endif
:	if changeflag==1
:       	exe "normal \<C-e>"
:	endif
:	if changeflag==2
:		exe "normal \<C-y>"
:	endif
endfunction

" +-----------------------------------------------------------------------------+
" | END                                                                         |
" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+

