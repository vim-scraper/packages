" Vim syntax file
" Language:     Falcon
" Maintainer:   Steven Oliver <oliver.steven@gmail.com>
" Website:      http://github.com/steveno/vim-files/blob/master/syntax/falcon.vim
" Credits:      Thanks the ruby.vim authors, I borrowed a lot!
" -------------------------------------------------------------------------------

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
syn match   falconSymbolOther "\(#\)" display

" Operators.
syn match   falconOperator "\(+\|-\|\*\|/\|=\|<\|>\|@\|\*\*\|!=\|\~=\)"
syn match   falconOperator "\(<=\|>=\|=>\|\.\.\|<<\|>>\|\"\)"

" Clusters
syn region falconSymbol start="[]})\"':]\@<!:\"" end="\"" skip="\\\\\|\\\"" contains=@falconStringSpecial fold

syn case match

" Keywords.
syn keyword falconKeyword break continue dropping return launch from global
syn keyword falconKeyword const self sender catch raise give 
syn keyword falconKeyword pass lambda def directive load export 
syn keyword falconKeyword function innerfunc init static attributes  
syn keyword falconKeyword enum try class object
syn keyword falconKeyword exit launch len
syn keyword falconKeyword print printl provides

" Todo.
syn keyword falconTodo TODO FIXME XXX DEBUG NOTE 

" Other keywords
syn keyword falconKeyword in notin to as has hasnt provides
syn keyword falconKeyword Error TraceStep SyntaxError CodeError 
syn keyword falconKeyword RangeError MathError IoError TypeError
syn keyword falconKeyword ParamError CloneError InterruprtedError
syn keyword falconKeyword List all any allp anyp eval choice xamp
syn keyword falconKeyword iff list cascade dolist eq

" Conditionals.
syn keyword falconConditional if elif else end and or not
syn keyword falconConditional switch select case default
syn match   falconConditional "end\s\if"

" Loops.
syn keyword falconRepeat loop while for
syn keyword falconRepeat forlast formiddle forfirst

" Booleans
syn keyword falconBool true false nil

" Comments.
syn match falconCommentSkip contained "^\s*\*\($\|\s\+\)"
syn region falconComment start="/\*" end="\*/" contains=@falconCommentGroup,falconSpaceError, falconTodo
syn region falconCommentL start="//" end="$" keepend contains=@falconCommentGroup,falconSpaceError, falconTodo
syn match falconSharpBang "\%^#!.*" display

syn sync ccomment falconComment

" Literals.
syn match falconNumbers transparent "\<[+-]\=\d\|[+-]\=\.\d" contains=falconIntLiteral,falconFloatLiteral
syn match falconNumbersCom contained transparent "\<[+-]\=\d\|[+-]\=\.\d" contains=falconIntLiteral,falconFloatLiteral
syn match falconIntLiteral contained "[+-]\=\d\+"
syn match falconFloatLiteral contained "[+-]\=\d\+\.\d*"
syn match falconFloatLiteral contained "[+-]\=\d*\.\d*"

" Includes
syn keyword falconInclude load

" Expression Substitution and Backslash Notation
syn match falconStringEscape "\\\\\|\\[abefnrstv]\|\\\o\{1,3}\|\\x\x\{1,2}" contained display
syn match falconStringEscape "\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)" contained display

syn region falconSymbol start="[]})\"':]\@<!:\"" end="\"" skip="\\\\\|\\\"" contains=falconStringEscape fold

" Normal String and Shell Command Output
syn region falconString matchgroup=falconStringDelimiter start="\"" end="\"" skip="\\\\\|\\\"" contains=falconStringEscape fold
syn region falconString matchgroup=falconStringDelimiter start="'" end="'" skip="\\\\\|\\'" fold
syn region falconString matchgroup=falconStringDelimiter start="`" end="`" skip="\\\\\|\\`" contains=falconStringEscape fold

" Generalized Single Quoted String, Symbol and Array of Strings
syn region falconString matchgroup=falconStringDelimiter start="%[qw]\z([~`!@#$%^&*_\-+=|\:;"',.?/]\)"  end="\z1" skip="\\\\\|\\\z1" fold
syn region falconString matchgroup=falconStringDelimiter start="%[qw]{" end="}" skip="\\\\\|\\}" fold contains=falconDelimEscape
syn region falconString matchgroup=falconStringDelimiter start="%[qw]<" end=">" skip="\\\\\|\\>" fold contains=falconDelimEscape
syn region falconString matchgroup=falconStringDelimiter start="%[qw]\[" end="\]" skip="\\\\\|\\\]" fold contains=falconDelimEscape
syn region falconString matchgroup=falconStringDelimiter start="%[qw](" end=")" skip="\\\\\|\\)" fold contains=falconDelimEscape
syn region falconSymbol matchgroup=falconSymbolDelimiter start="%[s]\z([~`!@#$%^&*_\-+=|\:;"',.?/]\)" end="\z1" skip="\\\\\|\\\z1" fold
syn region falconSymbol matchgroup=falconSymbolDelimiter start="%[s]{" end="}" skip="\\\\\|\\}" fold contains=falconDelimEscape
syn region falconSymbol matchgroup=falconSymbolDelimiter start="%[s]<" end=">" skip="\\\\\|\\>" fold contains=falconDelimEscape
syn region falconSymbol matchgroup=falconSymbolDelimiter start="%[s]\[" end="\]" skip="\\\\\|\\\]" fold contains=falconDelimEscape
syn region falconSymbol matchgroup=falconSymbolDelimiter start="%[s](" end=")" skip="\\\\\|\\)" fold contains=falconDelimEscape

" Generalized Double Quoted String and Array of Strings and Shell Command Output
syn region falconString matchgroup=falconStringDelimiter start="%\z([~`!@#$%^&*_\-+|\:;"',.?/]\)" end="\z1" skip="\\\\\|\\\z1" contains=falconStringEscape fold
syn region falconString matchgroup=falconStringDelimiter start="%[QWx]\z([~`!@#$%^&*_\-+=|\:;"',.?/]\)" end="\z1" skip="\\\\\|\\\z1" contains=falconStringEscape fold
syn region falconString matchgroup=falconStringDelimiter start="%[QWx]\={" end="}" skip="\\\\\|\\}" contains=falconStringEscape,falconDelimEscape fold
syn region falconString matchgroup=falconStringDelimiter start="%[QWx]\=<" end=">" skip="\\\\\|\\>" contains=falconStringEscape,falconDelimEscape fold
syn region falconString matchgroup=falconStringDelimiter start="%[QWx]\=\[" end="\]" skip="\\\\\|\\\]" contains=falconStringEscape,falconDelimEscape fold
syn region falconString matchgroup=falconStringDelimiter start="%[QWx]\=(" end=")" skip="\\\\\|\\)" contains=falconStringEscape,falconDelimEscape fold

syn region falconString start=+\%(\%(class\s*\|\%([]})"'.]\|::\)\)\_s*\|\w\)\@<!<<\z(\h\w*\)\ze+hs=s+2 matchgroup=falconStringDelimiter end=+^\z1$+ contains=falconStringEscape fold keepend
syn region falconString start=+\%(\%(class\s*\|\%([]})"'.]\|::\)\)\_s*\|\w\)\@<!<<"\z([^"]*\)"\ze+hs=s+2  matchgroup=falconStringDelimiter end=+^\z1$+ contains=falconStringEscape fold keepend
syn region falconString start=+\%(\%(class\s*\|\%([]})"'.]\|::\)\)\_s*\|\w\)\@<!<<'\z([^']*\)'\ze+hs=s+2  matchgroup=falconStringDelimiter end=+^\z1$+ fold keepend
syn region falconString start=+\%(\%(class\s*\|\%([]})"'.]\|::\)\)\_s*\|\w\)\@<!<<`\z([^`]*\)`\ze+hs=s+2  matchgroup=falconStringDelimiter end=+^\z1$+ contains=falconStringEscape fold keepend

syn region falconString start=+\%(\%(class\s*\|\%([]}).]\|::\)\)\_s*\|\w\)\@<!<<-\z(\h\w*\)\ze+hs=s+3 matchgroup=falconStringDelimiter end=+^\s*\zs\z1$+ contains=falconStringEscape fold keepend
syn region falconString start=+\%(\%(class\s*\|\%([]}).]\|::\)\)\_s*\|\w\)\@<!<<-"\z([^"]*\)"\ze+hs=s+3  matchgroup=falconStringDelimiter end=+^\s*\zs\z1$+ contains=falconStringEscape fold keepend
syn region falconString start=+\%(\%(class\s*\|\%([]}).]\|::\)\)\_s*\|\w\)\@<!<<-'\z([^']*\)'\ze+hs=s+3  matchgroup=falconStringDelimiter end=+^\s*\zs\z1$+ fold keepend
syn region falconString start=+\%(\%(class\s*\|\%([]}).]\|::\)\)\_s*\|\w\)\@<!<<-`\z([^`]*\)`\ze+hs=s+3  matchgroup=falconStringDelimiter end=+^\s*\zs\z1$+ contains=falconStringEscape fold keepend

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

  HiLink falconKeyword          Keyword
  HiLink falconCommentString    String
  HiLink falconTodo             Todo
  HiLink falconConditional      Keyword
  HiLink falconRepeat           Repeat
  HiLink falconcommentSkip      Comment
  HiLink falconComment          Comment
  HiLink falconCommentL         Comment
  HiLink falconOperator         Operator
  HiLink falconSymbol           Normal
  HiLink falconSpaceError       Error
  HiLink falconIntLiteral       Number
  HiLink falconFloatLiteral     Number
  HiLink falconStringEscape     Special
  HiLink falconStringDelimiter  Delimiter
  HiLink falconString           String
  HiLink falconBool             Constant
  HiLink falconSharpBang        PreProc
  HiLink falconInclude          Include
  HiLink falconSymbol           Constant
  HiLink falconSymbolOther      Delimiter

  delcommand HiLink
endif

let b:current_syntax = "falcon"

"---------------------------------------------
" vim: set sw=4 sts=4 et tw=80 :
"

