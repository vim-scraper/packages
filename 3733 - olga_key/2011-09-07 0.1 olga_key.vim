" Vim syntax file
" Language: OLGA .key file
" Maintainer: Anders Schau Knatten (I work for SPT Group, but this is not officially supported)
" Latest Revision: 0.1 07 September 2011

let b:current_syntax = "key"

syn keyword keyEnd ENDCASE ENDLIBRARY ENDNETWORKCOMPONENT

syn match keyComment "!.*$"

syn match keyKeyword "^ *[A-Z]* "
syn match keyTerminals "TERMINALS = "
syn match keyKey "[A-Z0-9_]*\ *="he=e-1
syn match keyValue "[A-Za-z0-9_\-\+\.\/ \"\(\)@:;]*,"
syn match keyValue2 "[A-Za-z0-9_\-\+\.\/ \"\(\)@:;]*$"

hi def link keyEnd Type

hi def link keyKeyword StorageClass

hi def link keyKey Identifier
hi def link keyTerminals Identifier
hi def link keyValue Constant
hi def link keyValue2 Constant
hi def link keyComment Comment
