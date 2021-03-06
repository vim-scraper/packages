" Vim syntax file
" Language:     JavaScript
" Maintainer:   Yi Zhao <zzlinux AT hotmail DOT com>
" Last Change:  2006 March 10
" Version:      0.2
" Based On:     javascript.vim from Claudio Fleiner <claudio@fleiner.com>
" Changes:      Change file mode to UNIX. 
"
" TODO
"   - internal function hightlight
"   - code fold support
"

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'javascript'
endif

" Drop fold if it set but vim doesn't support it.
if version < 600 && exists("javaScript_fold")
  unlet javaScript_fold
endif

syntax case match

syntax match   jsSpecial        "\\\d\d\d\|\\."
syntax region  jsStringD        start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=jsSpecial,@htmlPreproc
syntax region  jsStringS        start=+'+  skip=+\\\\\|\\'+  end=+'+  contains=jsSpecial,@htmlPreproc

syntax keyword jsCommentTodo    TODO FIXME XXX TBD contained
syntax region  jsLineComment    start=+\/\/+ end=/$/ contains=jsCommentTodo oneline
syntax region  jsComment        start="/\*"  end="\*/" contains=jsCommentTodo,jsLineComment

syntax match   jsNumber         "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syntax region  jsRegexpString   start=+/[^/*]+me=e-1 skip=+\\\\\|\\/+ end=+/[gi]\?\s*$+ end=+/[gi]\?\s*[;.,)]+me=e-1 contains=@htmlPreproc oneline

"" Programm Keywords
syntax keyword jsConditional    if else
syntax keyword jsRepeat         while for
syntax keyword jsBranch         break continue switch case default
syntax keyword jsOperator       new in
syntax keyword jsType           this var const
syntax keyword jsStatement      return try catch throw with 
syntax keyword jsBoolean        true false
syntax keyword jsNull           null

if exists("javaScript_fold")
    syntax match  jsFunction            "\<function\>"
    syntax region jsFunctionFold        start="\<function\>.*[^};]$" end="^\z1}.*$" transparent fold keepend

    syntax sync match jsSync    grouphere jsFunctionFold "\<function\>"
    syntax sync match jsSync    grouphere NONE "^}"

    setlocal foldmethod=syntax
    setlocal foldtext=getline(v:foldstart)
else
    syntax keyword    jsFunction        function
endif

syntax sync fromstart
syntax sync maxlines=100

" Code blocks
syntax cluster jsAll       contains=jsComment,jsSpecial,jsStringD,jsStringS,jsNumber,jsRegexpString,jsBoolean,jsFunction,jsFunctionFold,jsConditional,jsRepeat,jsBranch,jsOperator,jsType,jsStatement,jsBoolean,jsConstant
syntax region  jsBracket   matchgroup=jsBracket transparent start="\[" end="\]" contains=@jsAll,jsBracket,jsParen
syntax region  jsParen     matchgroup=jsParen transparent start="(" end=")" contains=@jsAll,jsParen,jsBracket
syntax region  jsBlock     matchgroup=jsBlcok transparent start="{" end="}" contains=ALL 

" catch errors caused by wrong parenthesis
syntax match   jsParenError  ")\|}\|\]"

if main_syntax == "javascript"
  syntax sync ccomment jsComment
endif

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_javascript_syn_inits")
  if version < 508
    let did_javascript_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink jsComment              Comment
  HiLink jsLineComment          Comment
  HiLink jsCommentTodo          Todo
  HiLink jsSpecial              Special
  HiLink jsStringS              String
  HiLink jsStringD              String
  HiLink jsCharacter            Character
  HiLink jsNumber               jsValue
  HiLink jsConditional          Conditional
  HiLink jsRepeat               Repeat
  HiLink jsBranch               Conditional
  HiLink jsOperator             Operator
  HiLink jsType                 Type
  HiLink jsStatement            Statement
  HiLink jsFunction             Function
  HiLink jsBlock                Function
  HiLink jsError                Error
  HiLink jsParenError           jsError
  HiLink jsNull                 Keyword
  HiLink jsBoolean              Boolean
  HiLink jsRegexpString         String
  delcommand HiLink
endif

let b:current_syntax = "javascript"
if main_syntax == 'javascript'
  unlet main_syntax
endif

" vim: ts=4
