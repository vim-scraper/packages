" $Id$
" Name Of File: motion.vim
"
"  Description: Vim plugin
"
"       Author: Ivan Carlos S. Lopes <lopesivan (at) poli (dot) com (dot) br>
"   Maintainer: Ivan Carlos S. Lopes <lopesivan (at) poli (dot) com (dot) br>
"
"  Last Change: $Date:$
"      Version: $Revision:$
"
"    Copyright: This script is released under the Vim License.
"
"  Description: motion text in command mode.
"
" use:
" **line**
" CTRL+up     - move line
" CTRL+down   - move line
" 
" **char**
" CTRL+right  - move char
" CTRL+left   - move char
" 
" **screen**
" SHIFT+up    - move screen
" SHIFT+down  - move screen
" SHIFT+right - move screen
" SHIFT+left  - move screen

if &cp || exists("g:loaded_motion") || exists("g:loaded_screen")
	finish
endif

let g:loaded_motion = "v01"
let g:loaded_screen  = "v01"
let s:keepcpo  = &cpo
set cpo&vim

" ----------------------------------------------------------------------------

if ( !hasmapto('<Plug>ScreenToLeft' )  &&
\    !hasmapto('<Plug>ScreenToRight')  &&
\    !hasmapto('<Plug>ScreenToUp'   )  &&
\    !hasmapto('<Plug>ScreenToDown' )  &&
\    ( maparg ('<S-Up>'   )   == '' )  &&
\    ( maparg ('<S-Down>' )   == '' )  &&
\    ( maparg ('<S-Left>' )   == '' )  &&
\    ( maparg ('<S-Right>')   == '' )  &&
\    ( has("gui_running"  )         )
\)

	map  <unique> <S-Left>  <Plug>ScreenToLeft
	map  <unique> <S-Right> <Plug>ScreenToRight
	map  <unique> <S-Up>    <Plug>ScreenToUp
	map  <unique> <S-Down>  <Plug>ScreenToDown

elseif  ( !hasmapto('<Plug>ScreenToLeft' ) &&
\         !hasmapto('<Plug>ScreenToRight') &&
\         !hasmapto('<Plug>ScreenToUp'   ) &&
\         !hasmapto('<Plug>ScreenToDown' )
\ )

	if ( !has("gui_running") || has("win32") )

		echo "Error: No Key mapped."
		echo printf( "%8s\t%s", "<S-Up>",    "shift-cursor-up"   )
		echo printf( "%8s\t%s", "<S-Down>",  "shift-cursor-down" )
		echo printf( "%8s\t%s", "<S-Left>",  "shift-cursor-left" )
		echo printf( "%8s\t%s", "<S-Right>", "shift-cursor-right")
		echo "takens and a replacement were not assigned."

	endif

	let g:loaded_screen = 0
endif

"
" MoveScreenToLeft
"
function <SID>MoveScreenToLeft()
	normal zl
endfunction
"
" MoveScreenToRight
"
function <SID>MoveScreenToRight()
		normal zh
		echo wincol() - virtcol('.')
endfunction
"
" MoveScreenToUp
"
function <SID>MoveScreenToUp()
	if winline() == 1
		echohl WarningMsg | echo 'Wuff  ----  Wuff!!'| echohl None
	else
		execute "normal \<C-e>"
	endif
endfunction
"
" MoveScreenToDown
"
function <SID>MoveScreenToDown()
	if winline() == winheight(0)
		echohl WarningMsg | echo 'Wuff  ----  Wuff!!'| echohl None
	else
		execute "normal \<C-y>"
	endif
endfunction
"
" Map
"
map <Plug>ScreenToLeft  :call <SID>MoveScreenToLeft()<CR>
map <Plug>ScreenToRight :call <SID>MoveScreenToRight()<CR>
map <Plug>ScreenToUp    :call <SID>MoveScreenToUp()<CR>
map <Plug>ScreenToDown  :call <SID>MoveScreenToDown()<CR>

" ----------------------------------------------------------------------------

if ( !hasmapto('<Plug>LineToUp'   ) &&
\    !hasmapto('<Plug>LineToDown' ) &&
\    !hasmapto('<Plug>CharToRight') &&
\    !hasmapto('<Plug>CharToLeft' ) &&
\    (maparg  ('<C-Up>'   ) == '' ) &&
\    (maparg  ('<C-Down>' ) == '' ) &&
\    (maparg  ('<C-Left>' ) == '' ) &&
\    (maparg  ('<C-Right>') == '' ) &&
\    (maparg  ('<C-r>'    ) == '' ) &&
\    (maparg  ('<C-l>'    ) == '' )
\)

	map  <unique> <C-Down>  <Plug>LineToDown
	map  <unique> <C-Up>    <Plug>LineToUp
	map  <unique> <C-Right> <Plug>CharToRight
	map  <unique> <C-Left>  <Plug>CharToLeft

	" A map for swapping paragraphs
	map _sp {dap}p{

	" A pair of maps for swapping a word to-the-left and to-the-right:
	"
	nnoremap <unique> <silent>
	\ ( "_yiw?\w\+\_W\+\%#<CR>:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR>:silent! /Wuff  ----  Wuff!!<CR><c-o><c-l>

	nnoremap <unique> <silent>
	\ ) "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR><c-o>/\w\+\_W\+<CR>:silent! /Wuff  ----  Wuff!!<CR><c-l>

elseif ( !hasmapto('<Plug>MoveLineToUp'   ) &&
\        !hasmapto('<Plug>MoveLineToDown' ) &&
\        !hasmapto('<Plug>MoveCharToRight') &&
\        !hasmapto('<Plug>MoveCharToLeft' ))

	if ( !has("gui_running") || has("win32") )

		echo "Error: No Key mapped."
		echo printf("%8s\t%s", "<C-Up>",    "control-cursor-up"   )
		echo printf("%8s\t%s", "<C-Down>",  "control-cursor-down" )
		echo printf("%8s\t%s", "<C-Left>",  "control-cursor-left" )
		echo printf("%8s\t%s", "<C-Right>", "control-cursor-right")
		echo "is taken and a replacement was not assigned."

	endif

	let g:loaded_motion = 0

endif
"
" MoveCharToRight
"
function <SID>MoveCharToRight()

	let cursor_position_x = col('.')
	let last_cursor_position = (col('$') -1)

	if cursor_position_x == last_cursor_position
		echohl WarningMsg | echo '-- End of line --'| echohl None
	else
		normal xp
		echohl NonText | echo '-- Move to right --'| echohl None
	endif

endfunction
"
" MoveCharToLeft
"
function <SID>MoveCharToLeft()

	let cursor_position_x = col('.')

	if cursor_position_x == 1
		echohl WarningMsg | echo '-- Begin of line --'| echohl None
	else
		normal xhP
		echohl NonText | echo '-- Move to left --'| echohl None
	endif

endfunction
"
" MoveLineToDown
"
"function <SID>MoveLineToDown()
"	let cursor_position_y = line('.')
"	let last_line_nuber   = line('$')
"
"	if cursor_position_y == last_line_nuber
"		echohl WarningMsg | echo '-- Last line --'| echohl None
"	else
"		silent normal ddp
"		echohl NonText | echo '-- Move to down --'| echohl None
"	endif
"
"endfunction
function <SID>MoveLineToDown()
	if line('.') < line('$')
		let cur_col = virtcol('.')
		silent! normal ddp
		execute 'normal ' . cur_col . '|'
		echohl NonText | echo '-- Move to down --'| echohl None
	else
		echohl WarningMsg | echo '-- Last line --'| echohl None
	endif
endfunction
"
" MoveLineToUp
"
"function <SID>MoveToLineUp()
"	let cursor_position_y = line('.')
"
"	if cursor_position_y == 1
"		echohl WarningMsg | echo '-- First line --'| echohl None
"	else
"		silent normal ddkP
"		echohl NonText | echo '-- Move to up --'| echohl None
"	endif
"
"endfunction
function <SID>MoveToLineUp()
	if line('.') > 1
		let cur_col = virtcol('.')
		if line('.') == line('$')
			silent! normal ddP
		else
			silent! normal ddkP
		endif

		execute 'normal ' . cur_col . '|'
		echohl NonText | echo '-- Move to up --'| echohl None
	else
		echohl WarningMsg | echo '-- First line --'| echohl None
	endif
endfunction

"
" map
"
map <Plug>LineToDown  :call <SID>MoveLineToDown()<CR>
map <Plug>LineToUp    :call <SID>MoveToLineUp()<CR>
map <Plug>CharToRight :call <SID>MoveCharToRight()<CR>
map <Plug>CharToLeft  :call <SID>MoveCharToLeft()<CR>

" ----------------------------------------------------------------------------

let &cpo= s:keepcpo
unlet s:keepcpo

" vim: ts=8
