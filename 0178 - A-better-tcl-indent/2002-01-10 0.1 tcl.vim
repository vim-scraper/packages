"  vim: set sw=4 sts=4:
"  Maintainer	: Gergely Kontra <kgergely@mcl.hu
"  Revised on	: 2001.12.08. 17:27:52
"  Language	: Tcl/tk

" Improvements:
"   Correct, when you reindent a line
"   Detects opening brackets
"   Detects continuations lines
"   Eg.
"   pack [
"     button .b] [
"     entry .e
"   ]
"   pack\
"     button .b2
"   wm title "Tk example"

" TODO:
"   checking with respect to syntax highlighting
"   ignoring comments
"   detecting multiline strings

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
    finish
endif

"let b:did_indent = 1

setlocal indentexpr=GetTclIndent()
setlocal indentkeys-=:,0#
setlocal indentkeys+=0]

" Only define the function once.
if exists("*GetTclIndent")
    finish
endif

function GetTclIndent()
    " Find a non-blank line above the current line.
    let pnum = prevnonblank(v:lnum - 1)
    " Hit the start of the file, use zero indent.
    if pnum == 0
       return 0
    endif
    let line = getline(v:lnum)
    let pline = getline(pnum)

    let ind = indent(pnum)

    " Check for opening brace on previous line
    if pline =~ '{[^}]*$'
	let ind = ind + &sw
    endif
    " Check for opening bracket on previous line
    if pline =~ '\[[^[]*$'
	let ind = ind + &sw
    endif
    " Check for continuation line
    if pline =~ '\\$'
	let ind = ind + &sw
    endif
    " Check for closing brace on current line
    if line =~ '^[^{]*}'
	let ind	= ind - &sw
    endif
    " Check for closing bracket on current line
    if line =~ '^\s*]'
	let ind	= ind - &sw
    " Check for closing an unclosed bracket
    elseif pline =~ '^[^]]*]'
	let ind	= ind - &sw
    endif
    " Check whether previous line was an end of a continuation line
    let ppnum = prevnonblank(pnum - 1)
    if ppnum != 0
	let ppline = getline(ppnum)
	if ppline =~'\\$'
	    let ind = ind - &sw
	endif
    endif
    return ind
endfunction
