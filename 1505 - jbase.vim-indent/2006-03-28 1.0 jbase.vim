" Vim indent file
" Language:	jBASE jBASIC (ft=jbase)
" Author:	Gary Calvin <gcalvin@kenwoodusa.com>
" URL:		<TBD>
" Last Change:	Mon, 12 Apr 2004

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal indentexpr=JBaseGetIndent(v:lnum)
setlocal indentkeys+==~\*,o,<:>,=CASE,=END,=NEXT,=REPEAT,=UNTIL,*<Return>
setlocal sw=4

" Only define the function once.
if exists("*JBaseGetIndent")
    finish
endif

fun! JBaseGetIndent(lnum)
    " labels and comments get zero indent
    let this_line = getline(a:lnum)
    let LABELS_OR_COMMENT = '^\s*\(\*\|\d\+\s\+\|\I\i*:\).*'
    if this_line =~? LABELS_OR_COMMENT
	return 0
    endif

    " Find a non-blank line above the current line.
    " Skip over labels and preprocessor directives.
    let lnum = a:lnum
    while lnum > 0
	let lnum = prevnonblank(lnum - 1)
	let previous_line = getline(lnum)
	if previous_line !~? LABELS_OR_COMMENT
	    break
	endif
    endwhile

    " Hit the start of the file, use one shiftwidth
    if lnum == 0
	return &sw
    endif

    let ind = indent(lnum)

    " If previous line begins with CASE or FOR, indent
    if previous_line =~? '^\s*\<\(CASE\|FOR\)\>.*'
	let ind = ind + &sw
    endif

    " If previous line ends with DO, ELSE, LOOP, or THEN (followed by end-of-line or semicolon), indent
    if previous_line =~? '^.*\<\(DO\|ELSE\|LOOP\|THEN\)\>\s*\(;\|$\)'
	let ind = ind + &sw
    endif

    " If current line starts with END, NEXT, REPEAT or UNTIL, outdent
    if this_line =~? '^\s*\<\(END\|NEXT\|REPEAT\|UNTIL\)\>.*'
        let ind = ind - &sw
    end

    " If current line starts with CASE, outdent...
    if this_line =~? '^\s*\<CASE\>.*'
        " unless previous line is a BEGIN CASE
        if previous_line !~? '^\s*\<BEGIN\>\s*\<CASE\>.*'
            let ind = ind - &sw
        end
    end

    return ind
endfun
