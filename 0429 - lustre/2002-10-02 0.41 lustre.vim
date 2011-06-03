function! CompileLustre()
	let cc = "lustre "
	execute ":w!"

	:normal mz
	if (!stridx(getline(".") , "node ") == 0)
		execute '?^\<node\>'
	endif
	
	let line = getline('.')
	let b:node = substitute(line, '^node\s*', '', '')
	let b:node = substitute(b:node, '(.*$','','')
	execute "!".cc." % ".b:node
	unlet line
	unlet cc
	:normal `z
endfunction

function! RunLustre()
	"call CompileLustre()
	exe "!luciole % ".b:node
endfunction



map <m-c> :call CompileLustre()<CR>
map <m-r> :call RunLustre()<CR>
