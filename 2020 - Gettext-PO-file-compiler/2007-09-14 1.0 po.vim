" Vim compiler file
" Compiler:     msgfmt (for compiling po files)
" Maintainer:   KARASZI Istvan <vim@spam.raszi.hu>
" Last Change:  Fri Sep 14 12:17:21 CEST 2007
" Based on:     perl.vim by Christian J. Robinson <infynity@onewest.net>

" Changelog:
"
" 1.0: initial version

if exists("current_compiler")
	finish
endif
let current_compiler = "po"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
	command -nargs=* CompilerSet setlocal <args>
endif

" there is no pipes under windows, vi use temp file
" and as perl outputs to stderr this have to be handled corectly
if has("win32")
	setlocal shellpipe=1>&2\ 2>
endif

setlocal makeprg=msgfmt\ -v\ %

let s:savecpo = &cpo
set cpo&vim

" Sample errors:
" test.po:991: duplicate message definition...
" test.po:989: ...this is the location of the first definition
" msgfmt: found 1 fatal error
CompilerSet errorformat=
	\%f:%l:\ %m,
	\%-G%.%#\ translated\ messages.,
	\%-G%.%#\ fatal\ error
