" Vim indent file
" Language:	vhdl
" Author:
" URL:
" Last Change:

if exists("b:did_indent")
	finish
endif
let b:did_indent = 1

setlocal indentexpr=VhdlGetIndent(v:lnum)
setlocal indentkeys&
setlocal indentkeys+==~end,=~);,=~elsif,=~else,=~begin

" Only define the function once.
if exists("*VhdlGetIndent")
	finish
endif

fun! VhdlGetIndent(lnum)
	" preprocessor get zero indent immediately
	let this_line = getline(a:lnum)
	let PREPROC = '^\s*\(\<library\>\|\<use\>\).*'
	if this_line =~? PREPROC
		return 0
	endif

	" Find a non-blank line above the current line.
	" Skip over labels and preprocessor directives.
	let lnum = a:lnum
	while lnum > 0
		let lnum = prevnonblank(lnum - 1)
		let previous_line = getline(lnum)
		if previous_line !~? PREPROC
			break
		endif
	endwhile

	" Hit the start of the file, use zero indent.
	if lnum == 0
		return 0
	endif

	let ind = indent(lnum)

	" Add
	if previous_line =~? '.*\<begin\>\|.*\s*\<is\>$\|.*($\|^\s*\<else\>\|^\s*\<elsif\>.*\<then\>$\|^\s*\<if\>.*\<then\>$'
		let ind = ind + &sw
	endif

	" Subtract
	if this_line =~? '^\s*end\|^\s*);\|^\s*else\|^\s*elsif' || (previous_line =~? '.*;$' && this_line =~? '^\s*begin')
		let ind = ind - &sw
	endif
endif

return ind
endfun
