" Vim syntax file
"
" Language:    D
" Maintainer:  Jason Mills<jmills@cs.mun.ca>
" URL:           
" Last Change: 2002 Aug 16
" Version:     0.1.1
"
" TODO - Highlighting strings, special chars, etc. in comments should be
" optional.
"
" TODO - Allow user to set sync minlines as.
"
" TODO Several keywords (e.g. synchronized, in, and out )are both storage
" class and statements, depending on their context. Must use some matching to 
" figure out which and highlight appropriately. For now I have made such
" keywords statements.


" Quit when a syntax file was already loaded
if exists("b:current_syntax")
   finish
endif

" We define it here so that included files can test for it
if !exists("main_syntax")
   let main_syntax='d'
endif

" Keyword definitions
" 
syn keyword dError           operator template inline friend sizeof bool typeof
syn keyword dError           boolean 
syn keyword dExternal        import module export extern
syn keyword dConditional     if else switch 
syn keyword dRepeat          while for do
syn keyword dBoolean         true false
syn keyword dConstant        null 
syn keyword dTypedef         class interface enum struct union alias typedef 
syn keyword dOperator        new delete assert delegate cast align this super
syn keyword dType            ushort int uint long ulong float 
syn keyword dType            void byte ubyte double bit char wchar ucent cent
syn keyword dType            short complex imaginary extended 
syn keyword dDebug           deprecated unittest 
syn keyword dExceptions      throw try catch finally 
syn keyword dBranch          goto break continue 
syn keyword dScopeDecl       public protected private
syn keyword dStatement       version debug return with invariant asm body
syn keyword dStatement       in out inout synchronized 
syn keyword dStorageClass    static override final const abstract

" Labels
"
" We contain dScopDecl so public: private: etc. are not highlighted like labels
syn match   dUserLabel "^\s*[_$a-zA-Z][_$a-zA-Z0-9_]*\s*:"he=e-1 contains=dLabel,dScopeDecl
syn keyword dLabel     case default

" Comments
"
syn keyword dTodo             contained TODO FIXME TEMP XXX
syn match   dCommentStar      contained "^\s*\*[^/]"me=e-1
syn match   dCommentStar      contained "^\s*\*$"
syn match   dCommentPlus      contained "^\s*+[^/]"me=e-1
syn match   dCommentPlus      contained "^\s*+$"
syn region  dBlockCommentString   contained  start=+"+ end=+"+ end=+\*/+me=s-1,he=s-1 contains=dCommentStar,dSpecial,dEscSequence,@Spell
syn region  dNestedCommentString  contained  start=+"+ end=+"+ end="+"me=s-1,he=s-1 contains=dCommentPlus,dSpecial,dEscSequence,@Spell
syn region  dLineCommentString    contained start=+"+  end=+$\|"+ contains=dSpecial,dEscSequence,@Spell
syn region  dBlockComment     start="/\*"  end="\*/" contains=dBlockCommentString,dTodo,@Spell
syn region  dNestedComment    start="/+"  end="+/" contains=dNestedComment,dNestedCommentString, dTodo,@Spell
syn match   dLineComment      "//.*" contains=dLineCommentString,dTodo,@Spell

hi link dLineCommentString dBlockCommentString
hi link dBlockCommentString d2QuoteString
hi link dNestedCommentString d2QuoteString
hi link dCommentStar  dBlockComment
hi link dCommentPlus  dNestedComment

"syn sync ccomment dBlockComment
syn sync minlines=20

" Escape sequences (oct,specal char,hex,wchar). These are not contained
" because they are considered string litterals
"
syn match  dEscSequence "\\\(\o\{1,3}\|[\"\\'\\?ntbrfva]\|u\x\{4}\|x\x\x\)"

" Quoted string 
"
syn region  d2QuoteString start=+"+ end=+"+ end=+$+ contains=dEscSequence,@Spell
syn region  d1QuoteString start=+'+ skip=+\\'+ end=+'+ end=+$+ contains=@Spell


" Numbers
"
syn case ignore
syn match dInt        display "\<\d\+\(u\=l\=\|l\=u\=\)\>"
" Hex number
syn match dHex        display "\<0x\x\+\(u\=l\=\|l\=u\=\)\>"
syn match dHex        display "\<\d\x\+h\(u\=l\=\|l\=u\=\)\>"
" Flag the first zero of an octal number as something special
syn match dOctal      display "\<0\o\+\(u\=l\=\|l\=u\=\)\>" contains=cOctalZero
syn match dOctalZero  display contained "\<0"

"floating point without dot, but with imaginary and/or float indicator
syn match dFloat      display "\<\d\+f\=i\=\>"
"floating point number, with dot, optional exponent
syn match dFloat      display "\<\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\=i\="
"floating point number, starting with a dot, optional exponent
syn match dFloat      display "\(\.\d\+\)\(e[-+]\=\d\+\)\=[fl]\=i\=\>"
"floating point number, without dot, with exponent
syn match dFloat      display "\<\d\+e[-+]\=\d\+[fl]\=\>"
" binary numbers
syn match dBinary     display "\<0b[01]\+"
" flag an octal number with wrong digits
syn match dOctalError display "0\o*[89]\d*"
syn case match

" Unicode characters
"
syn match   dSpecial "\\u\d\{4\}"

" The default highlighting.
" 
hi def link dBinary                  Number
hi def link dInt                     Number
hi def link dHex                     Number
hi def link dOctal                   Number
hi def link dFloat                   Number
hi def link dDebug                   Debug
hi def link dBranch                  Conditional
hi def link dConditional             Conditional
hi def link dLabel                   Label
hi def link dUserLabel               Label
hi def link dRepeat                  Repeat
hi def link dExceptions              Exception
hi def link dStatement               Statement
hi def link dScopeDecl               dStorageClass
hi def link dStorageClass            StorageClass
hi def link dBoolean                 Boolean
hi def link dSpecial                 Special
hi def link d1QuoteString            String
hi def link d2QuoteString            String
hi def link dEscSequence             SpecialChar
hi def link dOctalError              dError
hi def link dError                   Error
hi def link dOperator                Operator
hi def link dConstant                Constant
hi def link dTypedef                 Typedef
hi def link dTodo                    Todo
hi def link dType                    Type
hi def link dLineComment             Comment
hi def link dBlockComment            Comment
hi def link dNestedComment           Comment
hi def link dExternal                Include


let b:current_syntax = "d"

if main_syntax == 'd'
  unlet main_syntax
endif

" vim: ts=3
