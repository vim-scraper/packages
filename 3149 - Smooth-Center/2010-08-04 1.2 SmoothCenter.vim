" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
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
map <Esc> :call StartSmoothCentering(1)<CR>
map <S-Esc> :call StartSmoothCentering(2)<CR>
map <S-F1> :call StartSmoothCentering(3)<CR>

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
:		let centerline = line("w0")
:		if line("w0")==line("$")
:			let g:smoothcentering = 0
:			return
:		endif
:	endif
:	if g:smoothcentering == 3
:		let centerline = line("w$")
:		if line("w0")==1
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

