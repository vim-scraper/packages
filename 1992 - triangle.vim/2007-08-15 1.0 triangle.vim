" Vim syntax file
" Language: Triangle (from "Programming Language Processors in Java")
" Version: 1.0
" Last Change: 2006/05/03
" Maintainer: Nicolas Weber <nicolasweber@gmx.de>


if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif



syn keyword triangleConditional if then else
syn keyword triangleStatement   let begin end const var type
syn keyword triangleStatement   proc func
syn keyword triangleType Integer Char Boolean array
syn keyword triangleRepeat do while
syn keyword triangleStruct record
syn keyword triangleConstant false true maxint
syn keyword triangleFunction eof eol get getint geteol put putint puteol
syn keyword triangleFunction chr ord
syn keyword triangleOperator of in
syn region triangleComment start="!" end="$"
syn match triangleNumber "\<\d\+\>"
syn match triangleCharacter "'.'"

" TODO: operators \ /\ \/ + - * / // < <= > >=    = \=   :=

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_triangle_syn_inits")
  if version < 508
    let did_triangle_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink triangleConditional Conditional
  HiLink triangleStatement Statement
  HiLink triangleType Type
  HiLink triangleRepeat Repeat
  HiLink triangleStruct Struct
  HiLink triangleConstant Constant
  HiLink triangleFunction Function
  HiLink triangleOperator Operator
  HiLink triangleComment Comment
  HiLink triangleNumber Number
  HiLink triangleCharacter Character

  delcommand HiLink
endif

let b:current_syntax = "triangle"
