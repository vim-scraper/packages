" Vim autoload script - complete a variable's value after "let" in the
"			command line
" File:		complval.vim
" Last Change:	2007 Dec 13
" Version:	0.1
"
" Installation:
" - copy file into your autoload folder
" - create a mapping in your vimrc:
"	cno <C-G> <c-r>=complval#VarComplValue()<cr>
"   This maps Ctrl-G for completion.  Tab should not be used, it already
"   completes other things.
"
" Usage Example:
"   :let foo = "bar"
"   :let foo = <C-G>

func! complval#VarComplValue()

    " remove the extra line caused by auto-loading:
    redraw

    if getcmdtype() != ":"
	" wrong mode
	return ""
    endif
    " cut cmdline below and right of cursor
    let cmdline = strpart(getcmdline(), 0, getcmdpos()-1)
    " " remove preceding commands (separated with <bar>)
    " let cmdline = substitute(cmdline, '^.*|', '', '')

    " where is the "let" command
    let letend = matchend(cmdline, '^\C\s*let\s\+')
    if letend < 0
	" not a let command
	return ""
    endif
    let varname = matchstr(cmdline, '^\%(\h:\)\=\h\w*\ze\s*=\s*$', letend)
    if varname == ""
	" not a possible variable name
	" or missing "="-sign or "=" followed by non-blanks
	return ""
    endif
    if varname[1] != ':'
	" within a function (this function!) global vars need the 'g:'
	" prefix (else they were local)
	let varname = "g:" . varname
    endif
    if !exists(varname)
	" variable not defined
	return ""
    endif
    return string({varname})
endfunc

" vim:set ts=8 sts=4 noet:
