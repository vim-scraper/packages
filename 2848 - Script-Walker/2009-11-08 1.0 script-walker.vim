" +-----------------------------------------------------------------------------+
" | SCRIPT WALKER                                                               |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
" | SUN 8TH OCT 2009:    1.0                                                    |
" |                      Initial revision.                                      |
" +-----------------------------------------------------------------------------+

" These two variables control whether script walker should be 'walking' or not, 
" and what direction to be walking in respectively (initially down: -1)
let g:walking = 0
let g:walkdir = -1

" Call DoWalk() every CursorHold event regarless
autocmd CursorHold * call DoWalk()

" Toggle walking 'on/off' 
function ToggleWalk()
:	if g:walking == 1
:		let g:walking = 0
:		echo "Walking switched --off--"
:	else
:		let g:walking = 1
:		echo "Walking switched --on--"
:	endif
endfunction

" Turn walking 'off'
function StopWalk()
:	let g:walking = 0
endfunction

" Do the walk --if-- flag is set
function DoWalk()
:	if g:walking == 0
:		return
:	endif
:	if line(".") == line("$")
:		let g:walkdir = 1
:	endif
:	if line (".") == 1
:		let g:walkdir = -1
:	endif
:	if g:walkdir == 1
:		exe "normal k"
:	endif
:	if g:walkdir == -1
:		exe "normal j"
:	endif
endfunction

" Map F12 to toggle walking on/off state flag for ease of use
map <F12> :call ToggleWalk()<CR>

" +-----------------------------------------------------------------------------+
" | END                                                                         |
" +-----------------------------------------------------------------------------+
" | SCRIPT WALKER                                                               |
" +-----------------------------------------------------------------------------+

