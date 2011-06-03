" +-----------------------------------------------------------------------------+
" | SMOOTH CENTER                                                               |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
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
map <Esc> :call StartSmoothCentering()<CR>

function StartSmoothCentering()
:	if g:smoothcentering==0
:		let g:smoothcentering = 1
:		return
:	endif
:	let g:smoothcentering = 0
endfunction

" Do the centering --if-- flag is set
function DoSmoothCenter()
:	if g:smoothcentering == 0
:		return
:	endif
:	let centerline = (line("w0")+line("w$"))/2
:	if line("w0")==1 && line(".")<centerline
:		let g:smoothcentering = 0
:		return
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

