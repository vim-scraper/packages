" GreedyBackspace.vim  -  Make backspace eat multiple whitespace characters.
"
" Copyrright October 2005 by Christian J. Robinson <infynity@onewest.net>
"
" Distributed under the terms of the Vim license.  See ":help license".
"
" This script isn't very useful if you don't put
"   set backspace=indent,eol,start
" or at least
"   set backsapce=indent,start
" in your vimrc.

function! s:GreedyBackspace()
	let bs = ''

	let offset = s:ByteOffset() - 1
	let c = s:GetChar(offset)

	if c !~ '\s'
		return "\<BS>"
	endif

	while c =~ '\s'
		let bs = bs . "\<BS>"
		let offset = offset - 1
		if (offset <= 1)
			break
		endif
		let c = s:GetChar(offset)
	endwhile

	return bs
endfunction

function! s:ByteOffset()
        return line2byte(line(".")) + col(".") - 1
endfunction

function! s:GetChar(offset)
	let line = byte2line(a:offset)
	let char = a:offset - line2byte(line)
	let c = getline(line)[char]
	return c
endfunction

inoremap <silent> <BS> <C-R>=<SID>GreedyBackspace()<CR>
