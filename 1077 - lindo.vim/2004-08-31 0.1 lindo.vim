" Vim syntax file
" Language:	LINDO
" Maintainer:	Juan M. Cataldo <jcataldo@inf.utfsm.cl>
" Last change:	2004 Aug 30

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn match	lindoComment	"^\s*!.*$"
syn match	lindoNumber	"\<\d\+\>"
syn match	lindoIdentifier "^\s*[a-zA-z_0-9]*)"
syn keyword	lindoStatement	MIN Min min
syn keyword	lindoStatement	ST St st
syn keyword	lindoStatement	END End end

hi def link lindoComment Comment
hi def link lindoNumber Number
hi def link lindoIdentifier Identifier
hi def link lindoStatement Statement

let b:current_syntax = "lindo"

" vim: ts=8: noexpandtab
