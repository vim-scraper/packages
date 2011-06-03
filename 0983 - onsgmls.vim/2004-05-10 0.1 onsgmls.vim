" Vim compiler file
" Compiler:	onsgmls
" Maintainer:	Robert Rowsome <rowsome@wam.umd.edu>
" Last Change:	2004 Mar 13

if exists("current_compiler")
  finish
endif

let current_compiler = "onsgmls"

let s:cpo_save = &cpo
set cpo-=C

setlocal makeprg=onsgmls\ -s\ %

setlocal errorformat=onsgmls:%f:%l:%c:%t:%m,
		    \onsgmls:%f:%l:%c:%m

let &cpo = s:cpo_save
unlet s:cpo_save

