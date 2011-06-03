" Language:  		Verilog HDL or SystemVerilog
" Maintainer:	  ljh575@gmail.com
" Last Change:  Thu Nov  5 19:42:43 CST 2009
"
" Descripetion: automatically insert begin-end after 'if/else/...'
" and end-func after 'function/task/...'
"
" Drag it to ~/.vim/ftplugin/


set cpoptions-=C
abbr func function

imap <CR>	<C-R>=VerilogAutoCompletion()<CR>

let s:if_patt = '\<\(if\|else\|while\|initial\|always\|forever\)\>'
let s:func_patt = '\<\(function\|class\|task\|module\|program|interface\)\>'

function! VerilogAutoCompletion()

	let b:cur_line = substitute(getline(line('.')),"//.*$","","")

	" match 's:if_patt' ; and ignore if already has begin-end 
	if b:cur_line =~ '^\s*\(end\)\?\s*'.s:if_patt &&  b:cur_line !~ '\(\<begin\>\|;\)' 
		if	searchpair(s:if_patt,'','\<begin\>','nW') <= 0 
			return " begin\<NL>end\<C-O>O"
		endif

		" match 's:func_patt'; and ignore if already has end-func
	elseif b:cur_line =~ s:func_patt.'\s\+\w.*[;(]\s*$' && b:cur_line !~ '^\s*\<extern\>'
		let b:match_func = matchstr(b:cur_line,s:func_patt)
		if searchpair(b:match_func,'','\<end'.b:match_func.'\>','nW') <= 0
			return "\<NL>end".b:match_func."\<C-O>O"
		endif
	endif

	return "\<NL>"

endfunction

