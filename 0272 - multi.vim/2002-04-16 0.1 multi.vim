" Vim compiler file
" Compiler:	Greenhills MULTI
" Maintainer:	Thomas Ramming <Thomas.Ramming@Grundig.com>
" Last Change:	2002 Mar 20

if exists("current_compiler")
  finish
endif
let current_compiler = "multi"

" doesnot work fine yet, missing warnings
setlocal errorformat=%EError:%m,
	\%Z,
	\%E\"%f\"\\,%*[^0-9]%l:\ error:\ %m,
	\%C\%m,
	\%C\%m,
	\%Z\%m,
	\%W\"%f\"\\,%*[^0-9]%l:\ warning:\ %m,
	\%C\%m,
	\%C\%m,
	\%C\%m,
	\%Z\%m,
	\%W[elxr]\ %m,
	\%Z,
	\%-A%.%#
" stderr must be catched !
setlocal makeprg=catch\ build
