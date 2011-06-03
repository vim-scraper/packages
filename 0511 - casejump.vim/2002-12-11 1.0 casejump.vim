" Vim filetype plugin for jumping to cases in switch statements
" Language:     C
" File:		casejump.vim
" Maintainer:	Gary Johnson <garyjohn@spk.agilent.com>
" Last Change:	2002-12-11 22:14:36

" By default, this plugin implements two jump or go-to macros:
"
"   ]s	Jump forward to the next case/default in this switch.
"   [s	Jump backward to the previous case/default in this switch.
"
" These mappings may be changed by mapping some other key sequence to
" <Plug>CasejumpForward and/or <Plug>CasejumpBackward, respectively.
"
" Installation:
"
" This file is intended to be installed as a filetype plugin for C files.
" Put it in your ~/.vim/ftplugin/c directory or rename it c_casejump.vim
" and put it in your ~/.vim/ftplugin directory.

" ----------------------------------------------------------------------

" Only load this plugin once for this buffer.
" (Don't use b:did_ftplugin because this is not meant to override any
" other filetype plugin.)
"
if exists("b:loaded_casejump")
    finish
endif
let b:loaded_casejump = 1

" Allow the use of line-continuation, even if user has 'compatible' set.
"
let s:save_cpo = &cpo
set cpo&vim

" Define mappings to the CaseJump() function only if mappings to this
" function do not already exist (the "!hasmapto" test) and issue an
" error message if the lhs of either mapping already exists (the
" "<unique>" qualifier).
"
if !hasmapto('<Plug>CasejumpForward')
    map <buffer> <unique> ]s <Plug>CasejumpForward
endif
if !hasmapto('<Plug>CasejumpBackward')
    map <buffer> <unique> [s <Plug>CasejumpBackward
endif

noremap <script> <silent> <Plug>CasejumpForward :call <SID>CaseJump('')<CR>
noremap <script> <silent> <Plug>CasejumpBackward :call <SID>CaseJump('b')<CR>

" Jump to the next or previous case or default label in the current
" switch statement.  Direction is determined by the 'flag' argument:
" '' means forward or next; 'b' means backward or previous.
"
" This function uses indentation rather than syntax to distinguish among
" the labels of nested switches.
"
" Don't redefine this function if it has already been defined by being
" loaded in some other buffer.
"
if !exists("*s:CaseJump")
    function! s:CaseJump(flag)

	" Save original cursor and screen location.
	"
	let curr_line = line(".")
	normal H
	let restoretop = line(".") . "normal!zt"

	" Find limits of enclosing block, assumed to be the desired switch.
	"
	execute curr_line . 'normal [{'
	let top = line(".")		" First line of the switch.
	normal %
	let bottom = line(".")		" Last line of the switch.

	" If the current line is a case or default label, use its indent
	" level.  Otherwise, find the indent level of the first label in the
	" enclosing block and use that instead.
	"
	if match(getline(curr_line), '^\s*\(\<case\>\|\<default\>\)') >= 0
	    let col = indent(curr_line) + 1
	else
	    execute top
	    let next_line = search('^\s*\(\<case\>\|\<default\>\)', 'W')
	    if (next_line == 0) || (next_line > bottom)
		" No case or default label was found within this block.

		" Restore screen and cursor to original positions.  (Cursor
		" column may be different.)
		"
		execute restoretop
		execute curr_line . 'normal ^'

		" Display error message and exit.
		"
		echohl ErrorMsg
		echo "No case or default label found within this block"
		echohl None
		return
	    endif
	    let col = indent(next_line) + 1
	endif

	" Restore screen to original position to avoid annoying jumping of
	" the display.
	"
	execute restoretop

	" Restore cursor to original line and begin search for next label.
	"
	execute curr_line . 'normal ^'
	let next_line = search('\%'.col.'v\(case\>\|default\>\)', 'w'.a:flag)
	if (next_line > bottom) || (next_line < top)
	    if a:flag == 'b'
		execute bottom
	    else
		execute top
	    endif
	    let next_line = search('\%'.col.'v\(case\>\|default\>\)', 'w'.a:flag)
	    execute restoretop
	    execute next_line . 'normal ^'
	endif
    endfunction
endif

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: ts=8 sw=4
