let g:aliases = []
let g:aliases_file = glob('~/.mutt/aliases')

function! muttaliasescomplete#Init()
	if filereadable(g:aliases_file)
		for line in readfile(g:aliases_file)
			let fields = split(line)
			call remove(fields, 0, 1)
			call add(g:aliases, join(fields, " "))
		endfor
	else
		echoerr 'Aliases file not readable'
	endif
endfunction


function! muttaliasescomplete#Complete(findstart, base)
	" cache aliases
	if g:aliases == []
		call muttaliasescomplete#Init()
	endif

	
	" find beginning of the current address
	if a:findstart
		let line = getline('.')
		let start = col('.') -1
		while line[start -2] != ',' && line[start -2] != ':' && start > 0
			let start -= 1
		endwhile

		return start
	endif


	" TODO check if an address is required in this line (To:, Cc:, ...)


	" complete an empty start, return all aliases
	if a:base == ''
		return g:aliases
	endif


	let matches = []
	let needle = '\c' . a:base
	for item in g:aliases
		if match(item, needle) != -1
			call add(matches, item)
		endif
	endfor

	return matches
endfunction

