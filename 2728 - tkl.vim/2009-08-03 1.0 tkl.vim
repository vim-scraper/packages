" Vim syntax file
" Language: Tikle script
" Maintainer: Higor Eurípedes
" Latest Revision: 13 July 2009

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore


syn keyword tklTodo contained TODO FIXME XXX NOTE
syn match tklComment "#.*$" contains=tklTodo

syn keyword tklLabel       start stop
syn keyword tklStatement   do after
syn match   tklStatement   "end\s*$"
syn keyword tklRepeat      while for
syn keyword tklOperator    is equal not
syn keyword tklConditional if else then
syn match   tklConditional "end\s\+if"
syn keyword tklAction      drop duplicate delay
syn keyword tklProperties  progressive
syn keyword tklProtocol    sctp tcp udp

syn match tklDelimiter     "[():;]"
syn match tklIpNumber      "\(\d\{1,3}\.\)\d\{1,3}"
syn match tklIpNumber      "\([0-9a-f]\+:\)[0-9a-f]\+"
syn match tklValue         "\d\+%"
syn match tklValue         "\d\+ms"
syn match tklValue         "\d\+s"
syn match tklIdentifier "^@.*" nextgroup=tklIpNumber
syn match tklIdentifier "%ip"

hi def link tklTodo        Todo
hi def link tklComment     Comment
hi def link tklLabel       Label
hi def link tklStatement   Statement
hi def link tklRepeat      Repeat
hi def link tklOperator    Operator
hi def link tklConditional Conditional
hi def link tklAction      Type
hi def link tklDelimiter   Delimiter
hi def link tklIpNumber    Special
hi def link tklValue       Number
hi def link tklIdentifier  Identifier
hi def link tklProperties  PreProc
hi def link tklProtocol    PreProc

let b:current_syntax = "tkl"
