function! GreedyBackspace()
	let bs = ''

	let offset = s:ByteOffset()
	let c = s:GetChar(offset)

    if (c !~ '\s' )
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
	let c = getline(line)[char - 1]
	return c
endfunction

inoremap <silent> <BS> <C-R>=GreedyBackspace()<CR>
