
function! s:hfile()
	let file = expand("%:t")

	let macro = toupper(substitute(file, '\.', "_", "g"))

	exe ("normal a#ifndef " . macro . "_INCLUDED\n")
	exe ("normal a#define " . macro . "_INCLUDED\n")
	exe ("normal a\n\n\n#endif")
	normal kk

endfunction

autocmd! BufNewFile *.h nested call s:hfile()
autocmd! BufNewFile *.hpp nested call s:hfile()

