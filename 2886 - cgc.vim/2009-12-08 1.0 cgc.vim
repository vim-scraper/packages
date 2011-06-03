" vim compiler file
" Compiler:		cgc (nVidia cg compiler)
" Maintainer:   Vincent B. (twinside@free.fr)
" Last Change:  2009 déc. 08

if exists("cgc")
  finish
endif
let current_compiler = "cgc"

let s:cpo_save = &cpo
set cpo-=C

setlocal errorformat=%f(%l)\ :\ %t%s\ c%n:\ %m

let &cpo = s:cpo_save
unlet s:cpo_save

"vim: ft=vim

