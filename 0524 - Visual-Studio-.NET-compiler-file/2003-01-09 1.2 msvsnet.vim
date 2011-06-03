" Vim compiler file
" Compiler:	Miscrosoft Visual C 7
" Maintainer:	John Connors <johnc@yagc.demon.co.uk>
" Last Change:	2003 Jan 09 

if exists("current_compiler")
  finish
endif
let current_compiler = "msvsnet"

setlocal errorformat=\ %#%f(%l)\ :\ %#%t%[A-z]%#\ %m
setlocal makeprg=devenv
