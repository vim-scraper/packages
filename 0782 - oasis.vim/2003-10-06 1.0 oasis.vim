" Vim syntax file
" Language:     OASIS-CC Syntax File
" Maintainer:   David Morris <dmorris@ball.com>
" Last Change:  2002-12-16

if exists("b:current_syntax")
  finish
endif

syn match       kwCont          /&$/

syn keyword     kwOperations    contained update insert delete update lock restore unlock commit
syn match       kwTables        "\(update\|insert\|delete\|update\|lock\|restore\|unlock\|commit\) \+\w\+" contains=kwOperations
syn keyword     kwEEN           EXTERNAL_ELEMENT ITEM_NAME NAME COMMAND_NAME 
syn keyword     kwEEN           COMMAND_NAME SUBFIELD_NAME TAE_BASE TASK_NAME
syn match       kwColumns       /\<\w\+\>\ze\s*=\s*\S*/
syn match       kwColumns       "\$\w\+" nextgroup=kwValues,kwString,kwNums
syn match       kwValues        "=\s*\zs\w\+"
syn match       kwValues        "=\s*\zs\w\+\s\+\(\w\+,\)*\w\+" contains=kwValAvail
syn match       kwValAvail      "\w\s\+\zs\(\w\+,\)*\w\+" contained
syn match       kwNums          "\d\+\(\.\d\+\)\="
syn keyword     kwAction        write begin endmac
syn keyword     kwAction        new_proc compile start decompile new_mac
syn match       kwAction        "cstol off"
syn match       kwAction        "declare variable"
syn keyword     kwControl       if end proc endproc ask clear where endif
syn keyword     kwTodo          contained TODO FIXME XXX
syn match       kwComment       "^;.*" contains=kwTodo
syn region      kwString        start=+L\="+ skip=+\\\\\|\\"+ end=+"+
syn keyword     kwOther         variable

command -nargs=+ HiLink hi def link <args>
HiLink kwOperations     Statement
HiLink kwAction         Statement
HiLink kwTables         Type
HiLink kwControl        Conditional
HiLink kwTodo           Todo
HiLink kwComment        Comment
HiLink kwString         String
HiLink kwColumns        Tag
HiLink kwEEN            PreProc
HiLink kwOther          Statement
HiLink kwCont           ModeMsg
HiLink kwValues         Identifier
HiLink kwNums           LineNr
HiLink kwValAvail       PreProc
delcommand HiLink

let b:current_syntax = "oasis"

" vim: ts=8 sw=2
