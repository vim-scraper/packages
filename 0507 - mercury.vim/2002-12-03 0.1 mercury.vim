" Vim syntax file
" Language:    Mercury
" Maintainers: Andy Hauser, andy@splashground.de
" Last Change: 2002 Dec 03

" TODO:
" group stuff, limit line to 80 Chars
" fix logical and recursive (and other) errors 
" submit filetype recognition

" Quit when a syntax file was already loaded
if version < 600
   syntax clear
elseif exists("b:current_syntax")
  finish
endif

" mercury is case sensitive.
syn case match

setlocal iskeyword+=-
setlocal iskeyword+=:
setlocal iskeyword+=>
setlocal iskeyword+=|

" Comments
syn region  mercuryCComment  start=!/\*! end=!\*/! contains=mercuryTodo
syn match   mercuryComment   /%.*/ contains=mercuryTodo
syn keyword	mercuryTodo		 TODO FIXME XXX contained 
syn match   mercuryCommentError      "\*/"

" Defines
syn region  mercuryDefine matchgroup=mercuryOperator  start=/^:- / end=/\./ contains=mercuryDefineKeyword,mercuryPredDefine,mercuryPredArg
syn keyword mercuryDefineKeyword  module import_module interface contained
syn keyword mercuryDefineKeyword  pred mode implementation contained
syn keyword mercuryDefineKeyword  is det semidet cc_multi  contained
syn match   mercuryPredDefine /pred|mode/    nextgroup=mercuryPred      skipwhite contained
syn match   mercuryPredDefine /type/         nextgroup=mercuryType      skipwhite contained
syn match   mercuryType       /[a-z][A-z_]*/ nextgroup=mercuryTypeArrow skipwhite skipnl contained
syn keyword mercuryTypeArrow  ---> contained

" Predicates
syn region  mercuryFunc matchgroup=mercuryPred start=/^[a-z][A-z0-9_]*/ end=/\./ contains=mercuryPredArg,mercuryComment,mercuryCComment,mercurySpecialCharacter,mercuryBeginDCG,mercuryBeginPred,mercuryBraces,mercuryOperator,mercuryString,mercuryList
syn region  mercuryPredArg matchgroup=mercuryArg  start=/(/ end=/)/ contains=mercuryTypeDefChar,mercuryPredArg,mercuryComment,mercuryCComment,mercuryBraces,mercuryOperator,mercuryString,mercuryList,mercuryOr contained
syn region  mercuryBraces matchgroup=mercuryPred start=/{/ end=/}/ contains=mercuryPredArg,mercuryComment,mercuryCComment,mercurySpecialCharacter,mercuryBraces,mercuryOperator,mercuryString,mercuryList contained
syn match   mercuryTypeDefChar  "::"  contained
syn keyword mercuryBeginDCG     --> contained
syn keyword mercuryBeginPred    :-  contained
syn match   mercuryOr           ";" contained

" Strings
syn region  mercuryString   start=/"/ skip=/\\\\\|\\"/ end=/"/ contained
syn region  mercuryAtom     start=/'/ skip=/\\\\\|\\'/ end=/'/ contained

" Lists
syn region  mercuryList matchgroup=mercuryListSep start=/\[/ end=/\]/ contains=mercuryListSep
syn match   mercuryListSep "|\|\^" contained

syn match   mercuryOperator "=\\=\|=:=\|\\==\|=<\|==\|>=\|\\=\|\\+\|<\|>\|:=\|=\|->\|\^"
syn match   mercuryAsIs     "===\|\\===\|<=\|=>"

syn match   mercurySpecialCharacter  ","
syn match   mercurySpecialCharacter  "!"

syn sync ccomment maxlines=100


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_mercury_syn_inits")
  if version < 508
    let did_mercury_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink mercuryComment          Comment
  HiLink mercuryCComment         Comment
  HiLink mercuryTodo             Todo
  HiLink mercuryCharCode         Special
  HiLink mercuryKeyword          Function
  HiLink mercuryClauseHead       Constant
  HiLink mercurySpecialCharacter Special
  HiLink mercuryNumber           Number
  HiLink mercuryAsIs             Normal
  HiLink mercuryCommentError     Error
  HiLink mercuryAtom             String
  HiLink mercuryString           String
  HiLink mercuryOperator         Operator
  HiLink mercuryBeginDCG         Operator
  HiLink mercuryBeginPred        Operator
  HiLink mercuryTypeDefChar      Operator
  HiLink mercuryDefine           Type
  HiLink mercuryDefineKeyword    Function
  HiLink mercuryType             Type
  HiLink mercuryTypeArrow        Function
  HiLink mercuryPredDefine       Function
  HiLink mercuryPred             Function
  HiLink mercuryArg              Special
  HiLink mercuryListSep          Special
  HiLink mercuryOr               Special

  delcommand HiLink
endif

let b:current_syntax = "mercury"

" vim: ts=4
