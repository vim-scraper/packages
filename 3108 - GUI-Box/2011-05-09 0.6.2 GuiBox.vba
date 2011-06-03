" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/gui-box.vim	[[[1
185
" GUI Box
" Maintainer: David Munger
" Email: mungerd@gmail.com
" Version: 0.6.2


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
if !exists("g:gui_box_width")
    let g:gui_box_width = 22
endif

if !exists("g:gui_fonts")
    let g:gui_fonts = [&guifont]
endif

if !exists("g:gui_colors")
	let g:gui_colors = ['=LIGHT=', 'default']
endif

if !exists("g:gui_default_font_size")
	let g:gui_default_font_size = 9
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

	" check if window already exists
	let winnr = bufwinnr(bufnr('Color Menu'))
	if winnr >= 0
		silent execute winnr . 'wincmd w'
		return
	endif

	execute g:gui_box_width . 'vnew Color\ Menu'
	setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap cursorline nospell

    call append('$', g:gui_colors)
	call append('$', ["", "<Esc>: close", "<Space>: activate", "<Enter>: act+close"])
	0delete
	syntax match PreProc	/^<.*/
	syntax match Title		/^=.*=$/

	map <buffer> <silent> <Esc> 	:bwipeout<CR>
	map <buffer> <silent> <Space> 	:call <SID>color_menu_activate(0)<CR>
	map <buffer> <silent> <CR> 		:call <SID>color_menu_activate(1)<CR>
	nnoremap <buffer> <silent> G	G4k
    call search('^' . g:colors_name . '$', 'w')

    setlocal nomodifiable
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
		bwipeout
	endif
	execute 'colorscheme ' . color
endfunction

map <silent> <Plug>ColorMenu :call <SID>color_menu()<CR>
" }}}

" Font Menu {{{
function! s:font_menu()

	" check if window already exists
	let winnr = bufwinnr(bufnr('Font Menu'))
	if winnr >= 0
		silent execute winnr . 'wincmd w'
		return
	endif

	execute g:gui_box_width . 'vnew Font\ Menu'
	setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap cursorline nospell

    call append('$', g:gui_fonts)
	call append('$', ["", "<Esc>: close", "<Space>: activate", "<Enter>: act+close",
				\ "K/+: bigger", "J/-: smaller"])
	0delete
	syntax match PreProc	/.*:.*/
	syntax match Title		/^=.*=$/

	map <buffer> <silent> <Esc> 	:bwipeout<CR>
	map <buffer> <silent> <Space> 	:call <SID>font_menu_activate(0)<CR>
	map <buffer> <silent> <CR> 		:call <SID>font_menu_activate(1)<CR>
	map <buffer> <silent> K 		:call <SID>font_menu_resize(0.5)<CR><Space>
	map <buffer> <silent> J 		:call <SID>font_menu_resize(-0.5)<CR><Space>
	map <buffer> <silent> + 		K
	map <buffer> <silent> - 		J
	nnoremap <buffer> <silent> G	G6k
    
	if search('^' . &guifont . '$', 'w')
		let s:inserted_fonts = 0
	else
		" if font not found, try to match without size and insert line
		let font_str = &guifont
		while !search('^' . font_str, 'w') && font_str =~ '[ 0-9\.]$'
			let font_str = font_str[:-2]
		endwhile
		call append(line('.') - 1, &guifont)
		normal k
		let s:inserted_fonts = 1
	endif

    setlocal nomodifiable
endfunction

function! s:font_menu_activate(close)

	if getpos('.')[1] > len(g:gui_fonts) + s:inserted_fonts
		return
	endif

    let font = getline('.')

	if (empty(font) || font[0] == '=')
		return
	endif

	if (a:close)
		bwipeout
	endif
	execute 'set guifont=' . escape(font, ' ')
endfunction

function! s:font_menu_resize(increment)

	let font = getline('.')

	let size = matchstr(font, '\zs[0-9\.]*\ze\s*$')

	if empty(size)
		" if no size, start with default size
		let newfont = font . ' ' . printf('%g', g:gui_default_font_size)
	else
		" change size
		let newsize = printf("%g", str2float(size) + a:increment)
		let newfont = substitute(font, escape(size, '.') . '\s*$', escape(newsize, '.'), '')
	endif

	setlocal modifiable
	call setline('.', newfont)
	setlocal nomodifiable

endfunction

map <silent> <Plug>FontMenu :call <SID>font_menu()<CR>
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
doc/gui-box.txt	[[[1
80
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

In the font menu, you can use K/+ and J/- to increase or decrease the size of
fonts for entries like 'Monospace 10'.


Settings ~

*g:gui_fonts*
	List of font names, e.g. >
	let g:gui_fonts = ['Monospace 10', 'Lucida Console 10', 'Courier New 10']
<
	The font list can also comprise blank lines, and separators can be
	added by enclosing them in a pair of '=', e.g. >
	let g:gui_fonts = ['=Free=', 'Monospace 10', '', '=Non-Free=', 'Lucida Console 10']
<
	I found my favorite programming fonts here:
	http://hivelogic.com/articles/top-10-programming-fonts

*g:gui_colors*
	List of color scheme names, e.g. >
	let g:gui_colors = ['darkbone', 'corporation', 'sienna', 'satori']
<
	The color list can also comprise blank lines, and separators can be
	added by enclosing them in a pair of '=', e.g. >
	let g:gui_colors = ['=DARK=', 'darkbone', 'corporation', '', '=LIGHT=', 'sienna', 'satori']

*g:gui_box_width*
	Width of the font and color menu windows. Default is 22.

*g:gui_default_font_size*
	Default font size to use when resizing fonts with no size specified.
	Default is 9.


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

I also like this one to maximize my Vim window: >
	noremap <Leader>gw :winsize 999 999<CR>
<

vim:tw=78:ts=8:sw=8:ft=help:norl:noet:
