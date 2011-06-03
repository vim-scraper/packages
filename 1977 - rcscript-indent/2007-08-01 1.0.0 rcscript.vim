" Vim indent file
" Language: RcScript
" Maintainer: Hans Winderix <fa356928@skynet.be>
" Last Change: 2007 March 28
" Filenames: *.rcs
" Version: 1.0.0

if exists("b:did_indent")
	finish
endif
let b:did_indent = 1

setlocal nosmartindent

" Now, set up our indentation expression and keys that trigger it.
setlocal indentexpr=GetRcScriptIndent(v:lnum)
setlocal indentkeys=0{,0},0),0],!^F,o,O,e
setlocal indentkeys+==END,=BEGIN,=ELSE,=ELSIF,=DO,=THEN,=VAR

if exists("*GetRcScriptIndent")
	finish
endif

function! s:GetPrevNonCommentLineNum( line_num )

	" Skip lines starting with a comment
	let SKIP_LINES = '^\s*--'

	let nline = a:line_num
	while nline > 0
		let nline = prevnonblank(nline-1)
		if getline(nline) !~? SKIP_LINES
			break
		endif
	endwhile

	return nline
endfunction

function! GetRcScriptIndent( line_num )
	" Line 0 always goes at column 0
	if a:line_num == 0
		return 0
	endif

	let this_codeline = getline( a:line_num )

	" The following keywords go on column 0
  if this_codeline =~ '^\s*\<\(MODULE\|USE\|PROCEDURE\|BEGIN\)\>'
		return 0
	endif

  if this_codeline =~ '^\s*\<\(CONST\|VAR\)\>'
		return 0
	endif

	let prev_codeline_num = s:GetPrevNonCommentLineNum( a:line_num )
	let prev_codeline = getline( prev_codeline_num )
	let indnt = indent( prev_codeline_num )

	" If the previous line starts indentation
  if prev_codeline =~ '^\s*\<\(PROCEDURE\|BEGIN\|FOR\|WHILE\|DO\|IF\|ELSIF\|THEN\|ELSE\)\>'
    " then indent
    let indnt = indnt + &shiftwidth
	endif

  if this_codeline =~ '^\s*\<\(DO\|THEN\)\>'
    let indnt = indnt - &shiftwidth
  endif

	" If the previous line starts indentation through a variable declaration
  " TODO: how to end this ?
  if prev_codeline =~ '^\s*\<\(CONST\|VAR\)\>'
    "let indnt = indnt + &shiftwidth
  endif

	" At the end of a block, we have to unindent both the current line
  " (the "END" for instance) and the newly-created line.
  if this_codeline =~ '^\s*\<\(END\|ELSE\|ELSIF\)\>'
		let indnt = indnt - &shiftwidth
	endif

	return indnt
endfunction
