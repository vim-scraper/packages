" Vim syntax file
" Language: Falcon
" Maintainer: Steven Oliver <oliver.steven@gmail.com>
" Website: http://github.com/steveno/vim-files/blob/master/syntax/falcon.vim

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" When wanted, highlight the trailing whitespace.
if exists("c_space_errors")
  if !exists("c_no_trail_space_error")
    syn match falconSpaceError "\s\+$"
  endif

  if !exists("c_no_tab_space_error")
    syn match falconSpaceError " \+\t"me=e-1
  endif
endif

" Symbols.
syn match   falconSymbol "\(;\|,\|\.\)"

" Operators.
syn match   falconOperator "\(+\|-\|\*\|/\|=\|<\|>\|@\|\*\*\|!=\|\~=\)"
syn match   falconOperator "\(<=\|>=\|=>\|\.\.\|<<\|>>\|\"\)"

" Todo.
syn keyword falconTodo TODO FIXME XXX DEBUG NOTE

syn case match

" Keywords.
syn keyword falconKeyword break continue dropping return launch from global
syn keyword falconKeyword const self sender catch raise give 
syn keyword falconKeyword pass lambda def directive load export 
syn keyword falconKeyword function innerfunc init static attributes  
syn keyword falconKeyword enum try class object
syn keyword falconKeyword exit launch len
syn keyword falconKeyword print printl provides

" Other keywords
syn keyword falconKeyword and or not in notin to as has hasnt provides
syn keyword falconKeyword Error TraceStep SyntaxError CodeError 
syn keyword falconKeyword RangeError MathError IoError TypeError
syn keyword falconKeyword ParamError CloneError InterruprtedError
syn keyword falconKeyword List all any allp anyp eval choice xamp
syn keyword falconKeyword iff list cascade dolist eq

" Conditionals.
syn keyword falconConditional if elif else end
syn keyword falconConditional switch select case default
syn match   falconConditional "end\s\if"

" Loops.
syn keyword falconRepeat loop while for
syn keyword falconRepeat forlast formiddle forfirst

" Booleans
syn keyword falconBool true false nil

" Comments.
syntax match falconCommentSkip contained "^\s*\*\($\|\s\+\)"
syntax region falconComment start="/\*" end="\*/" contains=@falconCommentGroup,falconSpaceError
syntax region falconCommentL start="//" end="$" keepend contains=@falconCommentGroup,falconSpaceError
syn match falconSharpBang "\%^#!.*" display

syn sync ccomment falconComment

" Literals.
syn match falconNumbers transparent "\<[+-]\=\d\|[+-]\=\.\d" contains=falconIntLiteral,falconFloatLiteral
syn match falconNumbersCom contained transparent "\<[+-]\=\d\|[+-]\=\.\d" contains=falconIntLiteral,falconFloatLiteral
syn match falconIntLiteral contained "[+-]\=\d\+"
syn match falconFloatLiteral contained "[+-]\=\d\+\.\d*"
syn match falconFloatLiteral contained "[+-]\=\d*\.\d*"
syn match falconString  "'\([^']\|''\)*'"
syn region falconString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$'

" Syntax Synchronizing
syn sync minlines=10 maxlines=100

" Define the default highlighting.
if version >= 600 || !exists("did_falcon_syn_inits")
  if version < 600
    let did_falcon_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink falconKeyword		Keyword
  HiLink falconCommentString	String
  HiLink falconTodo		Todo
  HiLink falconConditional      Keyword
  HiLink falconRepeat           Repeat
  HiLink falconcommentSkip      Comment
  HiLink falconComment          Comment
  HiLink falconCommentL		Comment
  HiLink falconOperator         Operator
  HiLink falconSymbol           Normal
  HiLink falconSpaceError       Error
  HiLink falconIntLiteral       Number
  HiLink falconFloatLiteral     Number
  HiLink falconString           String
  HiLink falconBool		Constant
  HiLink falconSharpBang        PreProc

  delcommand HiLink
endif

let b:current_syntax = "falcon"
