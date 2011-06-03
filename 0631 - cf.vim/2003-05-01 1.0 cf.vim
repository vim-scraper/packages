" Vim syntax file
" Copyright (c) 2003 Launchbird Design Systems, Inc.
" Language:     Confluence
" Maintainer:   Tom Hawkins
" Last Change:
" URL:          http://www.launchbird.com/misc/cf.vim

if exists("b:current_syntax")
  finish
endif

" Identifiers
syn match    cfIdentifier /\<\u\w*\>/

" Errors
syn match    cfBraceErr   "}"
syn match    cfBrackErr   "\]"
syn match    cfParenErr   ")"
syn match    cfCommentErr "\*)"

" Some convenient clusters
syn cluster  cfAllErrs contains=cfBraceErr,cfBrackErr,cfParenErr,cfCommentErr
syn cluster  cfContained contains=cfTodo

" Enclosing delimiters
syn region   cfEncl transparent matchgroup=cfKeyword start="(" matchgroup=cfKeyword end=")" contains=ALLBUT,@cfContained,cfParenErr
syn region   cfEncl transparent matchgroup=cfKeyword start="{" matchgroup=cfKeyword end="}"  contains=ALLBUT,@cfContained,cfBraceErr
syn region   cfEncl transparent matchgroup=cfKeyword start="\[" matchgroup=cfKeyword end="\]" contains=ALLBUT,@cfContained,cfBrackErr

" Comments
syn region   cfComment start="(\*" end="\*)" contains=cfComment,cfTodo
syn keyword  cfTodo contained TODO FIXME XXX

syn keyword  cfKeyword comp endcomp prim endprim if ef else endif when cond end
syn keyword  cfKeyword local endlocal
syn keyword  cfKeyword environment import

syn keyword  cfBoolean  true false

syn match cfOperator "'"

syn region   cfString   start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match    cfNil  "\[]"
syn match    cfInteger       "\<\=\~\?\d\+\>"
syn match    cfInteger       "\<0[x|X]\x\+\>"
syn match    cfFloat         "\<\=\~\?\d\+\.\d*\([eE][~]\=\d\+\)\>"
syn match    cfConst         "'[0|1]*'"

syn match cfSpecial "\$"
syn match cfSpecial "_"
syn match cfSpecial "`"
syn match cfSeperator ","

syn match cfOperator "?"
syn match cfOperator ":"
syn match cfOperator "||"
syn match cfOperator "&&"
syn match cfOperator "!"
syn match cfOperator "=="
syn match cfOperator "!="
syn match cfOperator "<"
syn match cfOperator ">"
syn match cfOperator "<="
syn match cfOperator ">="
syn match cfOperator "<\."
syn match cfOperator ">\."
syn match cfOperator "<=\."
syn match cfOperator ">=\."
syn match cfOperator "++"
syn match cfOperator "\\"
syn match cfOperator "::"
syn match cfOperator "#"
syn match cfOperator "head"
syn match cfOperator "tail"
syn match cfOperator "+"
syn match cfOperator "-"
syn match cfOperator "\*"
syn match cfOperator "/"
syn match cfOperator "%"
syn match cfOperator "\*\*"
syn match cfOperator "+\."
syn match cfOperator "-\."
syn match cfOperator "\*\."
syn match cfOperator "/\."
syn match cfOperator "\*\*\."
syn match cfOperator "'++'"
syn match cfOperator "or"
syn match cfOperator "nor"
syn match cfOperator "xor"
syn match cfOperator "xnor"
syn match cfOperator "and"
syn match cfOperator "nand"
syn match cfOperator "'=='"
syn match cfOperator "'!='"
syn match cfOperator "'<'"
syn match cfOperator "'>'"
syn match cfOperator "'<='"
syn match cfOperator "'>='"
syn match cfOperator "'<\.'"
syn match cfOperator "'>\.'"
syn match cfOperator "'<=\.'"
syn match cfOperator "'>=\.'"
syn match cfOperator "'+'"
syn match cfOperator "'-'"
syn match cfOperator "'\*'"
syn match cfOperator "'\*\.'"
syn match cfOperator "'#'"
syn match cfOperator "not"
syn match cfOperator "msb"
syn match cfOperator "msbs"
syn match cfOperator "lsb"
syn match cfOperator "lsbs"
syn match cfOperator "width"
syn match cfOperator "\."
syn match cfOperator "@"

syn match cfSpecial "<-"
syn match cfSpecial "->"

" Synchronization
syn sync minlines=50
syn sync maxlines=500

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_cf_syntax_inits")
  if version < 508
    let did_cf_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink cfBraceErr     Error
  HiLink cfBrackErr     Error
  HiLink cfParenErr     Error

  HiLink cfCommentErr   Error

  HiLink cfComment      Comment

  HiLink cfKeyword      Keyword
  HiLink cfOperator     Keyword

  HiLink cfNil          Constant
  HiLink cfBoolean      Boolean
  HiLink cfInteger      Number
  HiLink cfFloat        Float
  HiLink cfConst        Constant
  HiLink cfString       String

  HiLink cfTodo         Todo

  HiLink cfEncl         Keyword

  HiLink cfSpecial      Type

  HiLink cfSeperator     Special

  delcommand HiLink
endif

let b:current_syntax = "confluence"


