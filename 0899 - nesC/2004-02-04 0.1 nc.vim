" Vim syntax file
" Language:	nesC
" Maintainer:	Joao Girao <joao.girao@netlab.nec.de>
" Last change:	2004 Feb 04
" based on the C++ syntax file by Ken Shan

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the C syntax to start with
if version < 600
  so <sfile>:p:h/c.vim
else
  runtime! syntax/c.vim
  unlet b:current_syntax
endif

" nesC extentions
syn keyword ncAttribute		command event task
syn keyword ncType		components inline bool result_t
syn keyword ncOperator		signal post
syn keyword ncStructure		provides uses interface
syn keyword ncNumber		NPOS
syn keyword ncBoolean		true false SUCCESS FAIL
syn keyword ncFileTypes		configuration module implementation
syn keyword ncInclude		includes

if version >= 508 || !exists("did_nc_syntax_inits")
  if version < 508
    let did_nc_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink ncInclude		Include
  HiLink ncAttribute		StorageClass
  HiLink ncType			Type
  HiLink ncOperator		Statement
  HiLink ncStructure		Structure
  HiLink ncNumber		Number
  HiLink ncBoolean		Boolean
  HiLink ncFileTypes		Statement
  delcommand HiLink
endif

let b:current_syntax = "nc"

" vim: ts=8
