" CharSort -- Functions and mappings to sort a string, matching or ignoring
"             case.
"
" Copyright July 16, 2007 Christian J. Robinson <infynity@onewest.net>
"
" Distributed under the terms of the Vim license.  See ":help license".
"
" Mappings:
"  <leader>so  --  sort, case sensitive
"  <leader>si  --  sort, case insensitive
"
"  These mappings work in visual and normal mode--in normal mode they take
"  a motion operator.
"
" Function:
"  CharSort#Sort({string} [, {i/c}]) -- Sort {string}
"     {i/c} == c  -- match case (default)
"     {i/c} == i  -- ignore case

if v:version < 700
	finish
endif

" Either source this file directly or put it in <runtimepath>/autoload and
" copy the following maps to your vimrc or a file in <runtimepath>/plugin:
if !hasmapto("CharSort", 'nv') && !exists('g:did_CharSort_mappings')
	nnoremap <silent> <leader>so :set opfunc=CharSort#SM<CR>g@
	nnoremap <silent> <leader>si :set opfunc=CharSort#SM<CR>:let b:tmpsorttype='i'<CR>g@
	vnoremap <silent> <leader>so s<C-r>=CharSort#Sort(@")<CR><ESC>
	vnoremap <silent> <leader>si s<C-r>=CharSort#Sort(@",'i')<CR><ESC>

	let g:did_CharSort_mappings = 1
endif

function! CharSort#Sort(str, ...)  " {{{1
	let chars = split(a:str, '\zs')

	if a:0 > 0
		let s:whichsort = a:1
	else
		let s:whichsort = 'c'
	endif
	
	let chars = sort(chars,'CharSort#Cmp')
	return join(chars, '')
endfunction

function! CharSort#Cmp(a, b)  " {{{1
	if s:whichsort ==? 'i'
		if a:a ==? a:b
			" Even though case is being ignored,
			" "A" should come before "a", etc.
			return a:a ==# a:b ? 0 : a:a ># a:b ? 1 : -1
		else
			return a:a ==? a:b ? 0 : a:a >? a:b ? 1 : -1
		endif
	else
		return a:a ==# a:b ? 0 : a:a ># a:b ? 1 : -1
	endif
endfunc

function! CharSort#SM(type, ...)  " {{{1
	let sel_save = &selection
	let &selection = "inclusive"

	if exists('b:tmpsorttype')
		let sorttype = b:tmpsorttype
		unlet b:tmpsorttype
	else
		let sorttype = 'c'
	endif

	let what = "s\<C-r>=\CharSort#Sort(@\",'" . sorttype . "')\<CR>\<ESC>"

	if a:0
		silent exe "normal! `<" . a:type . "`>" . what
	elseif a:type == 'line'
		silent exe "normal! '[V']" . what
	elseif a:type == 'block'
		silent exe "normal! `[\<C-V>`]" . what
	else
		silent exe "normal! `[v`]" . what
	endif

	let &selection = sel_save
endfunction
" }}}1

" vim:fdm=marker:fdc=2:
