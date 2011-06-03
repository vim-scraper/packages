function! CompileLustre()
	let cc = "lustre "

	execute ":w!"

	"execute '$'
	execute '?\<node\>'
	
	let line = getline('.')
	let b:node = substitute(line, '^node\s*', '', '')
	let b:node = substitute(b:node, '(.*$','','')
	execute "!".cc." % ".b:node
	unlet line
	unlet cc
endfunction

function! RunLustre()
	exe "!luciole % ".b:node
endfunction


map <m-c> :call CompileLustre()<CR>
map <m-r> :call RunLustre()<CR>
map <c-s> I--<ESC>
