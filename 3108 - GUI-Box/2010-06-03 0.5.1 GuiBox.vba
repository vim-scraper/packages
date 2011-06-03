" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/gui-box.vim	[[[1
122
" GUI Box
" Maintainer: David Munger
" Email: mungerd@gmail.com
" Version: 0.5.1


" TODO: doc color categories


if !has("gui_running")
    finish
endif

" Load Only Once {{{
if exists("g:gui_box_loaded")
	finish
endif

let g:gui_box_loaded = 1
" }}}

" Settings {{{
if !exists("g:gui_fonts")
    let g:gui_fonts = [&guifont]
endif

if !exists("g:gui_colors")
	let g:gui_colors = ['=LIGHT=', 'default']
endif
" }}}

" Menu Bar {{{
function! s:ToggleMenuBar()
    if &guioptions =~# 'm'
        set guioptions-=m
    else
        set guioptions+=m
    endif
endfunction

noremap <silent> <script> <Plug>ToggleMenuBar :call <SID>ToggleMenuBar()<CR>
" }}}

" Color Menu {{{
function! s:color_menu()
	20vnew +setlocal\ buftype=nofile Color\ Menu

    call append('$', g:gui_colors)
	call append('$', ["", "<Esc>: close", "<Space>: activate", "<Enter>: act+close"])
	0delete
	syntax match Comment /^<.*/
	syntax match Title /^=.*=$/

	map <buffer> <silent> <Esc> 	:bdelete<CR>
	map <buffer> <silent> <Space> 	:call <SID>color_menu_activate(0)<CR>
	map <buffer> <silent> <CR> 		:call <SID>color_menu_activate(1)<CR>
    call search('^' . g:colors_name . '$', 'w')

    setlocal cursorline nomodifiable
endfunction

function! s:color_menu_activate(close)

	if getpos('.')[1] > len(g:gui_colors)
		return
	endif

    let color = getline('.')

	if (empty(color) || color[0] == '=')
		return
	endif

	if (a:close)
		bdelete
	endif
	execute 'colorscheme ' . color
endfunction

map <silent> <Plug>ColorMenu :call <SID>color_menu()<CR>
" }}}

" Font Menu {{{
function! s:font_menu()
	20vnew +setlocal\ buftype=nofile Font\ Menu

    call append('$', g:gui_fonts)
	call append('$', ["", "<Esc>: close", "<Space>: activate", "<Enter>: act+close"])
	0delete
	syntax match Comment /^<.*/
	syntax match Title /^=.*=$/

	map <buffer> <silent> <Esc> 	:bdelete<CR>
	map <buffer> <silent> <Space> 	:call <SID>font_menu_activate(0)<CR>
	map <buffer> <silent> <CR> 		:call <SID>font_menu_activate(1)<CR>
    call search('^' . &guifont . '$', 'w')

    setlocal cursorline nomodifiable
endfunction

function! s:font_menu_activate(close)

	if getpos('.')[1] > len(g:gui_fonts)
		return
	endif

    let font = getline('.')

	if (empty(font) || font[0] == '=')
		return
	endif

	if (a:close)
		bdelete
	endif
	execute 'set guifont=' . escape(font, ' ')
endfunction

map <silent> <Plug>FontMenu :call <SID>font_menu()<CR>
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
doc/gui-box.txt	[[[1
63
*gui-box.txt*		GUI Tool Box
*gui-box*

This plugin provides:
- A color scheme menu to choose among your favorite color schemes;
- A font menu to choose among your favorite fonts;
- A command to toggle the GUI menu.

Configuration ~

Set the |g:gui_colors| and |g:gui_fonts| variables to lists (of strings) of
your favorite color scheme names and font names, add mappings to
|<Plug>ColorMenu|, |<Plug>FontMenu| and |<Plug>ToggleMenuBar|.


Usage ~

Once the color scheme or font menu is open, use j/k to highlight the desired
entry and press:

	<Enter>	to activate the colors/font and close the menu window
	<Space>	to activate the colors/font without closing the menu window
	<Esc>	to close the menu window

Variables ~

*g:gui_fonts*
	List of font names, e.g. >
	let g:gui_fonts = ['Monospace 10', 'Lucida Console 10', 'Courier New 10']
<
	The font list can also comprise blank lines, and separators can be
	added by enclosing them in a pair of '=', e.g. >
	let g:gui_fonts = ['=Free=', 'Monospace 10', '', '=Non-Free=', 'Lucida Console 10']

*g:gui_colors*
	List of color scheme names, e.g. >
	let g:gui_colors = ['darkbone', 'corporation', 'sienna', 'satori']
<
	The color list can also comprise blank lines, and separators can be
	added by enclosing them in a pair of '=', e.g. >
	let g:gui_colors = ['=DARK=', 'darkbone', 'corporation', '', '=LIGHT=', 'sienna', 'satori']


Mappings ~

*<Plug>ColorMenu*
	Opens the color scheme selection menu.

*<Plug>FontMenu*
	Opens the font selection menu.

*<Plug>ToggleMenuBar*
	Toggles the GUI menu bar.


Here is an example of mappings: >

	nmap <Leader>gc <Plug>ColorMenu
	nmap <Leader>gf <Plug>FontMenu
	nmap <Leader>gm <Plug>ToggleMenuBar


vim:tw=78:ts=8:sw=8:ft=help:norl:noet:
