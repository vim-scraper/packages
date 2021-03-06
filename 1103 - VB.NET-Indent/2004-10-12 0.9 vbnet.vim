" Vim indent file

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal indentexpr=VbGetIndent(v:lnum)
setlocal indentkeys&
setlocal indentkeys+==~else,=~elseif,=~end,=~wend,=~case,=~next,=~select,~=loop,<:>

" Only define the function once.
if exists("*VbGetIndent")
    finish
endif

fun! VbGetIndent(lnum)
    " labels and preprocessor get zero indent immediately
    let this_line = getline(a:lnum)
    let LABELS_OR_PREPROC = '^\s*\(\<\k\+\>:\s*$\|#.*\)'
    if this_line =~? LABELS_OR_PREPROC
	return 0
    endif

    " Find a non-blank line above the current line.
    " Skip over labels and preprocessor directives.
    let lnum = a:lnum
    while lnum > 0
	let lnum = prevnonblank(lnum - 1)
	let previous_line = getline(lnum)
	if previous_line !~? LABELS_OR_PREPROC
	    break
	endif
    endwhile

    " Hit the start of the file, use zero indent.
    if lnum == 0
	return 0
    endif

    let ind = indent(lnum)

    " Add
    if previous_line =~? '^\s*\<\(begin\|\%(\%(private\|public\|protected\|friend\)\s\+\)\=\%(function\|sub\|class\|property\)\|select\|case\|default\|if\>.\{-}\<then\>\s*$\|else\|elseif\|do\|for\|while\|enum\|with\)\>'
	let ind = ind + &sw
    endif

    " Subtract
    if this_line =~? '^\s*\<end\>\s\+\<select\>'
	if previous_line !~? '^\s*\<select\>'
	    let ind = ind - 2 * &sw
	else
	    " this case is for an empty 'select' -- 'end select'
	    " (w/o any case statements) like:
	    "
	    " select case readwrite
	    " end select
	    let ind = ind - &sw
	endif
    elseif this_line =~? '^\s*\<\(end\|else\|until\|loop\|next\|wend\)\>'
	let ind = ind - &sw
    elseif this_line =~? '^\s*\<\(case\|default\)\>'
	if previous_line !~? '^\s*\<select\>'
	    let ind = ind - &sw
	endif
    endif

    return ind
endfun
