" Vim syntax file
" Language: idp - knowledge representation
" Maintainer: Adriaan Larmuseau
" Latest Revision: Oct 25 2010

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" comments
syn match idp_todo "[tT][oO][dD][oO]" contained
syn match idp_linecomment "//.*$"
syn region idp_comment start="/\*" end="\*/" contains=idp_todo

" matches
syn match idp_discl ':'

" keywords
syn match idp_symbol '!'
syn match idp_symbol '?'
syn match idp_symbol "=>"
syn match idp_symbol "<=>"
syn match idp_symbol "<-"
syn keyword idp_symbol isa
syn keyword idp_key Given Declare Satisfying Find Data Minimize Partial nextgroup=idp_discl skipwhite
syn keyword idp_small type declare partial domain

syn keyword idp_builtin int sum
syn keyword idp_function MIN MAX SUCC abs

" numbers & literals
" only simpel numbers as far as I know of taken from prolog 
syn match idp_number "\<[0123456789]*\>'\@!"

" ------------------------
" higlighting
" ------------------------
let b:current_syntax = "idp"

hi def link idp_todo Todo
hi def link idp_comment Comment
hi def link idp_linecomment Comment
hi def link idp_key  PreProc
hi def link idp_small Type
hi def link idp_symbol Statement
hi def link idp_builtin Identifier
hi def link idp_function Function
hi def link idp_number Constant




