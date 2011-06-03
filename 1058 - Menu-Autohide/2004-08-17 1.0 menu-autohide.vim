" Vim plugin to hide the menu bar when the window gets too small.
"
" Last Change:   Tuesday August 17, 2004
" Maintainer:    Peter Wilson <pwilson+vim@cs.hmc.edu>
" License:       This file is placed in the public domain.

" Documentation:
" Adjustable Parameters:
"    g:menu_hide_threshold -- Menu is hidden when number of lines goes below
"                             this. Default: 12
"    g:menu_show_threshold -- Menu is shown when number of lines goes above
"                             this. Default: 15
"    g:menu_height_in_lines -- An approximation of the height of the menu.
"    			       The number of lines displayed is adjusted by
"    			       this amount when the menu is toggled.
"                              Default: 2
"

let g:menu_hide_threshold=12
let g:menu_show_threshold=15
let s:menu_last_state="start"
function! s:check_window_size ()
	if &lines < g:menu_hide_threshold && s:menu_last_state != "hide"
		set guioptions-=m
		let &lines = &lines + g:menu_height_in_lines
		let s:menu_last_state = "hide"
	elseif &lines > g:menu_show_threshold && s:menu_last_state != "show"
		set guioptions+=m
		let &lines = &lines - g:menu_height_in_lines
		let s:menu_last_state = "show"
	endif
endfunction
let g:menu_height_in_lines=0
call s:check_window_size()
let g:menu_height_in_lines=2

augroup MenuAutoHide
autocmd FocusGained * :call s:check_window_size()
augroup END
