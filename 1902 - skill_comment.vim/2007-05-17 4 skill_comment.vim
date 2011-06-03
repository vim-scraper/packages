" Vim filetype plugin code snippet
" Language: skill (cadence extension language)
" File:	    skill_comment.vim
" Version:  4
" Created:  2007 May 14
" Author:   Andy Wokula <anwoku@yahoo.de>
" Description: 
"   After inserting ");", insert the word located before the matching
"   opening paren; for selected keywords also add their argument.  To work
"   properly, all must be found on the same line.
"
" Changes:
"   05/16/07 simon@methodics-eda.com
"   - added missing keywords
"
" Installation: (suggestion)
" 1. make sure filetype "skill" is detected by Vim
" 2. copy this script to ~/.vim/
"    (i.e. to the first entry of 'runtimepath')
" 3. :edit
"	~/.vim/after/ftplugin/skill.vim
"    (create path and file if it doesn't exist)
"    add the following line:
"	runtime skill_comment.vim
"
" TODO:
" + extend s:keywords
"
" Limits: (rare cases)
" - does not ignore ");" in comments
" - does not ignore (possibly unbalanced) parens in comments

if v:version < 700
    " sorry, Vim7 required
    finish
endif

" Mapping for ";" in Insert mode
ino <buffer><expr> ; <sid>CloseKeyword()

if exists("s:keywords")
    " sourcing once is enough
    finish
endif

" search for arguments to these keywords:
let s:keywords = ['procedure', 'mprocedure', 'nprocedure', 'lambda', 'defun', 'while', 'if', 'when', 'unless', 'cond', 'for', 'foreach', 'forall', 'case', 'caseq', 'decode']


function! <sid>CloseKeyword()
    if getline(".")[col(".")-2] != ')'
	" if char just before ";" is not ")", do nothing
	return ";"
	" (Note: first getline index is 0, first col is 1)
    endif
    " move cursor to closing paren (this is allowed)
    call cursor(".", col(".")-1)
    " get position of matching paren:
    let pomp = searchpairpos('(','',')','bW')
    " [line, column]
    if !pomp[0]
	return ";"
    endif
    " getline of matching paren
    let glineomp = getline(pomp[0])
    " get matching keyword:
    let word = matchstr(glineomp[: pomp[1]-2], '\k\+\ze\s*$')
    if word == ""
	return ";"
    elseif match(s:keywords, '\<'.word.'\>') >= 0
	" search for keyword argument
	let kwarg = matchstr(glineomp, '\k\+', pomp[1])
	" omit trailing space if no argument found
	return "; " . word . (kwarg!="" ? " ".kwarg : "")
    else
	return "; " . word
    endif
endfunction

" vim:set ts=8 sts=4 noet:
