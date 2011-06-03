" Vim syntax file
" Language:     Polyhedra configuration file
" Maintainer:   Olivier Mengué <dolmen@users.sourceforge.net>
" Last Change:  16 Feb 2006
" License:      Same as Vim

" Configuration file for the Polyhedra database
" http://www.enea.com/


if exists("b:current_syntax")
   finish
endif

syn clear

syn keyword polycfgTodo contained    TODO FIXME XXX
syn cluster polycfgCommentGroup contains=polycfgTodo
syn region polycfgCommentL matchgroup=polycfgCommentStart start="--\|;\|#" end="$" keepend contains=@polycfgCommentGroup

syn match   polycfgVar contained "\$([^)]*)"
syn keyword polycfgBoolean contained true false on off yes no
syn match   polycfgValue "\S.*\ze\s*$" contained contains=polycfgVar,polycfgBoolean
syn match   polycfgEqual "=" contained nextgroup=polycfgValue
syn region  polycfgSetting matchgroup=polycfgKey start=/^\w\+/ end=/$/ oneline keepend contains=polycfgEqual,polycfgValue
syn region  polycfgSection matchgroup=polycfgSectionHeader start="^[a-zA-Z0-1:]\+\s*$" end="^[a-zA-Z0-1:]\+\s*$"me=s-1,he=s-1 contains=polycfgSetting,polycfgCommentL keepend


hi def link  polycfgError          Error
hi def link  polycfgTodo           Todo
hi def link  polycfgComment        Comment
hi def link  polycfgCommentL       polycfgComment
hi def link  polycfgCommentStart   polycfgComment
hi def link  polycfgVar            PreProc
hi def link  polycfgKey            Identifier
hi def link  polycfgBoolean        Constant
hi def link  polycfgSectionHeader  Special
hi def link  polycfgEqual          Operator
"hi def link  polycfgValue          String

let b:current_syntax = expand('<sfile>:t:r')

" vim: set ts=8:
