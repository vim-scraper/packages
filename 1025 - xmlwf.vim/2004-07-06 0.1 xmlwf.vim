" Vim Compiler File
" Compiler:	xmlwf
" Maintainer:	Robert Rowsome <rrowsome@gmail.com>
" Last Change:	Tue, 06 Jul 2004 


if exists("current_compiler")
  finish
endif
let current_compiler = "xmlwf"

let s:cpo_save = &cpo
set cpo-=C

setlocal makeprg=xmlwf\ %

setlocal errorformat=%f:%l:%c:%m

let &cpo = s:cpo_save
unlet s:cpo_save

