"Vim indent file
"Language:	    Baan 3GL/4GL
"Author:	    Rajesh Kannan (rajesh dot kannan at infor dot com)
"Created:	    Thu, 22 Oct 2009, 1600hrs IST
"Last Modified:	    Fri, 26 Oct 2009
"Last Modified By:  Rajesh Kannan

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal autoindent
setlocal indentexpr=Get_Baan_Indent()
" entries in indentkeys forces vim to reindent current line
setlocal indentkeys&
setlocal indentkeys+==~end,=else,=until,=default,=endcase

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
	\ || previous_line =~ '\c^\s*\(declaration\|before\.program\|after\.form\.read\|on.error\|after\.program\):'
	\ || previous_line =~ '\c^\s*\(after\.update\.db\.commit\|before\.display\.object\|before\.new\.object\|functions\):'
	\ || previous_line =~ '\c^\s*on\.\(entry\|exit\):'
	\ || previous_line =~ '\c^\s*\(init\|before\|after\)\.\(form\|group\):'
	\ || previous_line =~ '\c^\s*\(before\|on\|after\)\.choice:'
	\ || previous_line =~ '\c^\s*\(init\.field\|before\.field\|before\.input\|before\.display\|selection\.filter\|before\.zoom\|before\.checks\|domain\.error\|ref\.input\|ref\.display\|check\.input\|on\.input\|when\.field\.changes\|after\.zoom\|after\.input\|after\.display\|after\.field\):'
	\ || previous_line =~ '\c^\s*\(read\.view\|before\.read\|after\.read\|before\.write\|before\.rewrite\|after\.write\|after\.skip\.write\|after\.skip\.rewrite\|before\.delete\|after\.delete\|after\.skip\.delete\)'
	let line_indent = line_indent + &sw
    endif

    " deduct indent for syntax that signifies end of a block 
    let current_line = getline(v:lnum)

    if current_line =~ '^\s*\(\#\)'
	" #defines, #includes etc get zero indent
	let line_indent = 0
    elseif current_line =~ '\c^\s*end.*'
	" ideally would like to specify the following but I cannot make it
	" work :-(
	" '\c^\s*\(end\)\(select\|if\|case\|....\)'
	let line_indent = line_indent - &sw
	if current_line =~ '\c^\s*endcase' 
	    "we're two indents to the right; one for 'on case' and another for
	    "'case'. thus we have to deduct twice
	    let line_indent = line_indent - &sw
	endif
    elseif current_line =~ '\c^\s*else'
	\ || current_line =~ '^\s*}'
	\ || current_line =~ '\c^\s*until'
	\ || (current_line =~ '\c^\s*\(case.*\|default\):' && !(previous_line =~ '\c^\s*on case'))
	let line_indent = line_indent - &sw
    endif

    return line_indent
endfunction

