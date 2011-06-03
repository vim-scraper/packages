" Vim syntax file
" Language: DAKOTA
" Maintainer: Jacobo Díaz (jadiga00@gmail.com)
" Last change: February 27, 2008
" 
" Thanks to the authors and maintainers of fortran.vim and nastran.vim.
"
"----------------------------------------------------------------------
" Remove any old syntax stuff hanging around
"syn clear
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
"
"--------------------DAKOTA SYNTAX---------------------------------------
"
syn region DAKOTAString  start=+"+ end=+"+ oneline
syn region DAKOTAString  start=+'+ end=+'+ oneline
" Any integer
syn match DAKOTANumber  "-\=\<[0-9]\+\>"
" floating point number, with dot, optional exponent
syn match DAKOTAFloat  "\<[0-9]\+\.[0-9]*\([edED][-+]\=[0-9]\+\)\=\>"
" floating point number, starting with a dot, optional exponent
syn match DAKOTAFloat  "\.[0-9]\+\([edED][-+]\=[0-9]\+\)\=\>"
" floating point number, without dot, with exponent
syn match DAKOTAFloat  "\<[0-9]\+[edED][-+]\=[0-9]\+\>"
syn match DAKOTALogical "\(true\|false\)"

" -------Comments
syn match DAKOTAComment "^\s*\zs#.*$"	contains=@shCommentGroup
syn match DAKOTAComment  "#.*$"	contains=@shCommentGroup

" -------Implicit Functions
syn keyword DAKOTAImplicit strategy method model variables interface responses

if version >= 508 || !exists("did_dakota_syntax_inits")
  if version < 508
     let did_dakota_syntax_inits = 1
     command -nargs=+ HiLink hi link <args>
  else
     command -nargs=+ HiLink hi link <args>
  endif
  " The default methods for highlighting.  Can be overridden later
  HiLink DAKOTAString	   String
  HiLink DAKOTANumber	   Constant
  HiLink DAKOTAFloat	   Constant
  HiLink DAKOTAComment	   Comment
  HiLink DAKOTAImplicit	   Identifier
  HiLink DAKOTALogical	   Statement
  delcommand HiLink
endif

let b:current_syntax = "dakota"

"EOF vim: ts=8 noet tw=120 sw=8 sts=0
