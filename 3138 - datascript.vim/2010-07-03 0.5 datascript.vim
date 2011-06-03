" Vim syntax file
" Language:	Datascript
" Maintainer:	Dominique Pelle <dominique.pelle@gmail.com>
" Last Change:	2010-07-02
"
" See http://datascript.berlios.de/DataScriptLanguageOverview.html for
" a description of the Datascript language.

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword dsInclude       import
syn keyword dsType          uint8 int8 uint16 int16 uint32 int32 uint64 int64 bit
syn keyword dsKeyword       align enum bitmask union choice on case default subtype if
syn keyword dsKeyword       function return package
syn keyword dsSql           sql sql_table sql_database sql_integer sql_pragma
syn keyword dsOperator      sizeof bitsizeof lengthof is sum forall in
syn keyword dsTodo          contained TODO FIXME XXX

syn keyword dsStorageClass  const

" dsCommentGroup allows adding matches for special things in comments.
syn cluster dsCommentGroup  contains=dsTodo

syn match   dsNumber        display "\<\d\+\>"
syn match   dsNumberHex     display "\<0x\x\+\>"
syn match   dsNumberBin     display "\<[01]\+b\>" contains=dsBinaryB
syn match   dsBinaryB       display contained "b\>"
syn match   dsOctal         display "\<0\o\+\>" contains=dsOctalZero
syn match   dsOctalZero     display contained "\<0"

syn match   dsOctalError    display "\<0\o*[89]\d*\>"

syn region  dsString        start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell

syntax match dsCommentError display "\*/"
syntax match dsCommentStartError display "/\*"me=e-1 contained

syn region   dsCommentL     start="//" skip="\\$" end="$" keepend contains=@dsCommentGroup,@Spell
syn region   dsComment      matchgroup=dsCommentStart start="/\*" end="\*/" contains=@dsCommentGroup,dsCommentStartError,@Spell extend

" Define the default highlighting.
hi def link dsType          Type
hi def link dsStorageClass  StorageClass
hi def link dsSql           PreProc
hi def link dsKeyword       Statement
hi def link dsString        String
hi def link dsNumber        Number
hi def link dsNumberBin     Number
hi def link dsBinaryB       Special
hi def link dsOctal         Number
hi def link dsOctalZero     Special
hi def link dsOctalError    Error
hi def link dsNumberHex     Number
hi def link dsTodo          Todo
hi def link dsOperator      Operator
hi def link dsInclude       Include
hi def link dsCommentError  Error
hi def link dsCommentStart  dsComment
hi def link dsCommentL      dsComment
hi def link cCommentL       dsComment
hi def link dsComment       Comment

let b:current_syntax = "datascript"
