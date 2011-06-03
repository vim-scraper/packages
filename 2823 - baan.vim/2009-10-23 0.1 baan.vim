"Vim indent file
"Language:	    Baan 3GL/4GL
"Author:	    Rajesh Kannan (rajesh dot kannan at infor dot com)
"Created:	    Thu, 22 Oct 2009, 1600hrs IST
"Last Modified:	    Fri, 23 Oct 2009
"Last Modified By:  Rajesh Kannan

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal autoindent
setlocal indentexpr=Get_Baan_Indent()
setlocal indentkeys&
setlocal indentkeys+==~end,=else,=until,=default

" stop if function already defined
if exists("*Get_Baan_Indent")
    finish
endif

function Get_Baan_Indent()
    let lnum = prevnonblank(v:lnum - 1)

    if lnum == 0  
	return 0 "no indent
    endif

    "check if the previous line has something that warrants the 
    "current line to be indented to the right
    let line_indent = indent(lnum)
    let previous_line = getline(lnum)

    if previous_line =~ '\c^\s*\(if\|then\|else\|while\|for\|case\|on case\|repeat\)'
	\ || previous_line =~ '\c^\s*select\(do\|empty\|eos\|error\|bind\)'
	\ || previous_line =~ '\c^\s*{' 
	\ || previous_line =~ '\c^\s*function.*)\n*{'
	\ || previous_line =~ '\c^\s*\(dllusage\|functionusage\)'
	\ || previous_line =~ '\c^\s*default'
	let line_indent = line_indent + &sw
    endif

    " deduct indent for syntax that signifies end of a block sdfgjsdfjksdf
    let current_line = getline(v:lnum)

    if current_line =~ '\c^\s*end.*'
	" ideally would like to specify the following but I cannot make it
	" work :-(
	" '\c^\s*\(end\)\(select\|if\|case\|....\)'
	let line_indent = line_indent - &sw
    elseif current_line =~ '\c^\s*else'
	\ || current_line =~ '^\s*}'
	\ || current_line =~ '\c^\s*until'
	let line_indent = line_indent - &sw
    endif

    return line_indent
endfunction

