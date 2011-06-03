" - ToggleOptions.vim
"
" vim6:fdm=marker:foldenable:ts=4:sw=4
"
" Author:		Eric Arnold ( eric_p_arnold in_the_vicinity_of yahoo.com )
" Created:		Mon May 09, 05/09/2005 05:04:17
"
map <leader>o :ToggleOptions<CR>
command! ToggleOptions call ToggleOptionsMenu()

function! ToggleOptionsMenu()

	let l:char = s:Char_menu( "{w}rap, {W}rapscan, {l}ist, {b}inary, {m}odifiable, {d}iff, {v}irtualedit, uhe{x}" )

	if l:char ==# 'w'
		set wrap!
		set wrap?
	elseif l:char ==# 'W'
		set wrapscan!
		set wrapscan?
	elseif l:char ==# 'b'
		set binary!
		set binary?
	elseif l:char ==# 'l'
		set list!
		set list?
	elseif l:char ==# 'm'
		if &modifiable && !&readonly
			set nomodifiable
			set readonly
		else
			set modifiable
			set noreadonly
		endif
		set readonly?
		set modifiable?
	elseif l:char ==# 'v'
		if &virtualedit == 'all'
			set virtualedit=block
		else
			set virtualedit=all
		endif
		set virtualedit?
	elseif l:char ==# 'd'
		if &diff
			set nodiff foldcolumn=0
		else
			diffthis
		endif
		set diff?
	elseif l:char ==# 'x'
		if &display =~ 'uhex'
			set display-=uhex
		else
			set display+=uhex
		endif
		set display?
	endif

endfunction

function! s:Char_menu( stuff, ... )
	let l:stuff = a:stuff
	let l:match_col = 0
	let l:hi = "None"

	echohl Question

	while l:match_col >= 0
		let l:match_col = match( l:stuff, '^\(\w\+\|\W\+\|{\|}\)' )

		let l:tok = strpart( l:stuff, 0, l:match_col + 1 )
		let l:stuff = strpart( l:stuff, l:match_col + 1 )

		if l:tok == '{'
			echohl warningmsg " Error
			let l:hi = "standout"
		elseif l:tok == '}'
			echohl Directory " Statusline " Question
			let l:hi = "standout"
		elseif l:tok != ''
			if l:tok =~ '\w\+' && l:hi == "whitespace"
				echohl Directory " Statusline " Question
				let l:hi = "text"
			elseif l:tok =~ '[ ]\+'
				let l:hi = "whitespace"
				echohl None
			endif
			echon l:tok
		endif
	endwhile

	echohl None
	echon "\r"
	return nr2char( getchar() )

endfunction

