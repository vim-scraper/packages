" Vim syntax file
" Language:    ATS
" Filenames:   *.dats *.sats *.cats *.hats
" Maintainer:  Artyom Shalkhakov <artyom.shalkhakov@gmail.com>
" Last Change: Jun 27, 2009
" Version:     1
" URL:         http://artyoms.idhost.kz
"
" Huge thanks to maintainers of c.vim, haskell.vim, d.vim,
" html.vim.
"
" TODO:
" - make assignment of syntax groups more specific
"   (see sml.vim for let/local/begin/etc.)
" - highlight viewt@ype, etc. as keywords
" - report errors on mismatching braces/parens/brackets
" - highlight @ as a StorageClass?

if !exists("main_syntax")
  if version < 600
      syntax clear
  elseif exists("b:current_syntax")
      finish
  endif
  let main_syntax = 'ats'
endif

"
" lexical
"

" comment highlighting, mostly ripped from c.vim
syn keyword atsTodo TODO FIXME TEMP XXX HACKHACKHACK DEBUG NOTE contained
syn match   atscCommentError display "\*/"
syn match   atscCommentStartError display "/\*"me=e-1 contained
syn region  atscComment matchgroup=atscCommentStart start="/\*" end="\*/" contains=atsTodo,@Spell,atscCommentStartError
syn match   atsNestedCommentError display "\*)"
" mercilessly ripped from d.vim
syn region  atsNestedComment start="(\*" end="\*)" contains=atsNestedComment,atsTodo,@Spell
syn match   atsComment "//.*" contains=atsTodo
" this rule needs to be after // (above) because of priority!
syn region  atsComment start="////" end="\%$" contains=atsTodo

"
" denotation highlighting
"

" special characters, copied from c.vim
syn match   atsSpecial display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn match   atsSpecial display contained "\\\(u\x\{4}\|U\x\{8}\)"
syn match   atsFormat display "%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlL]\|ll\)\=\([bdiuoxXDOUfeEgGcCsSpn]\|\[\^\=.[^]]*\]\)" contained
syn region  atsStringDenot start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=atsSpecial,atsFormat,@Spell
syn match   atsCharDenot "'[^\\]'"
syn match   atsCharDenot "'[^']*'" contains=atsSpecial

syn keyword atsBoolDenot true false
syn case ignore
syn match   atsNumbers display transparent "\<\d\|\.\d" contains=atsNumber,atsFloat,atsOctalError,atsOctal
syn match   atsNumber display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
syn match   atsNumber display contained "0x\x\+\(u\=l\{0,2}\ll\=\u\)\>"
" flag the first zero of an octal number as something special
syn match   atsOctal display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=atsOctalZero
syn match   atsFloat display contained "\d\+\.\d*\(e[-+]\=\d\+\)\="
syn match   atsFloat display contained "\.\d\+\(e[-+]\=\d\+\)\>"
syn match   atsFloat display contained "\d\+e[-+]\=\d\+\>"

" flag an octal number with wrong digits
syn match   atsOctalError display contained "0\o*[89]\d*"
syn case match

syn match   atsIdent "[A-Za-z_][0-9A-Za-z_\']*"

" embedded C
" %{^ %} %{ %} %{$ %} %{# %}
if main_syntax != 'c' || exists('c')
    syn include @atsC syntax/c.vim
    unlet b:current_syntax
    syn region embC start=+%{[^#$]\=+ keepend end=+%}+ contains=@atsC
endif

"
" keyword highlighting
"

" keyword highlighting
syn keyword atsKeyword prefix postfix infix infixl infixr op nonfix
syn keyword atsKeyword staload stadef sta
syn keyword atsCond if then else case
syn keyword atsRepeat while
syn keyword atsException exception raise try
syn keyword atsStatement let in where local
syn keyword atsStatement val and fun fn lam fix rec var of
syn keyword atsStatement begin end
syn keyword atsTypedef typedef sortdef viewtypedef
syn keyword atsStructure datatype abstype dataviewtype dataprop dataview
syn keyword atsExternal extern implement

syn keyword atsKeyword symintr overload with
syn keyword atsKeyword prval praxi datasort

syn keyword atsSorts bool char int prop type view viewtype

syn keyword atsTypes string float double void

syn match   atsSym "[%&+-\./:=@~`^|*!$#?]+\|[%&+-\./:=@~`^|*<>]+"

" C preprocessor directives
syn match   atsError display "^\s*\(%:\|#\).*$"
syn region  atsPreCondit start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=atsComment,atsCommentError
syn match   atsPreCondit display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
syn region  atsCppOut start="^\s*\(%\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=atsCppOut2
syn region  atsCppOut2 contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=atsCppSkip
syn region  atsCppSkip contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=atsCppSkip
syn region  atsIncluded display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match   atsIncluded display contained "<[^>]*>"
syn match   atsInclude display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=atsIncluded
syn cluster atsPreProcGroup contains=atsPreCondit,atsIncluded,atsInclude,atsDefine,atsCppOut,atsCppOut2,atsCppSkip
syn region  atsDefine matchgroup=atsPreCondit start="\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$"
syn region  atsPreProc matchgroup=atsPreCondit start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend

"
" linking
"

" don't use standard HiLink, it won't work with included syntax files
command! -nargs=+ AtsHiLink hi def link <args>

" comments
AtsHiLink atsCommentStart atsComment
AtsHiLink atsNestedComment Comment
AtsHiLink atscCommentStart Comment
AtsHiLink atscComment Comment
AtsHiLink atsComment Comment
" delimiters
AtsHiLink atsEncl Keyword
" constants and denotations
AtsHiLink atsBoolDenot   Boolean
AtsHiLink atsCharsDenot  String
AtsHiLink atsCharDenot   Character
AtsHiLink atsStringDenot String
AtsHiLink atsNumber Number
AtsHiLink atsOctal Number
AtsHiLink atsOctalZero PreProc " link this to Error if you want
AtsHiLink atsFloat Float
AtsHiLink atsOctalError atsError
" identifiers
AtsHiLink atsIdent Identifier
" various keywords
AtsHiLink atsKey Keyword
AtsHiLink atsCond Conditional
AtsHiLink atsRepeat Repeat
AtsHiLink atsException Exception
AtsHiLink atsStatement Statement
AtsHiLink atsTypedef Typedef
AtsHiLink atsStructure Structure
AtsHiLink atsExternal StorageClass
AtsHiLink atsTypes Type
AtsHiLink atsSym Operator
AtsHiLink atsEncl Keyword
" embedded C
AtsHiLink embC Special

AtsHiLink atsPreProc PreProc

AtsHiLink atsCommentError atsError
AtsHiLink atscCommentStartError atsError
AtsHiLink atscCommentError atsError
AtsHiLink atsNestedCommentError atsError
AtsHiLink atsError Error
AtsHiLink atsAllErrs Error

delcommand AtsHiLink

let b:current_syntax = "ats"

if main_syntax == "ats"
    unlet main_syntax
endif

