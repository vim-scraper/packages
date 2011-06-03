" @(#) $Id: stol.vim,v 1.0 2003-05-14 17:03:41 EDT eralston exp $
" Vim syntax file
" Language:    STOL (Satellite Test and Operations Language)
" Maintainer:  R. Edward Ralston <eralston@techsan.org>
"
" created 2003-05-14 17:03:41 EDT eralston
"
"


" Remove any old syntax stuff hanging around
syn clear

" STOL is case insensitive
syn case ignore

"Error Codes
syn match  stolParenError ")"

" matched pairs
"==============
syn region stolGrp   transparent matchgroup=stolOperator start="("  end=")"

" Misc
"=====
syn match   stolOperator	 "\(&\|&&\||\|||\|<\|<=\|>\|>=\|!=\|<>\|==\|=\|<<\|>>\)"
syn match   stolWrapLineOperator "\\$"

syn keyword stolStatement       args continue do while else elseif enddo
syn keyword stolStatement       endif frame global goto if local noop
syn keyword stolStatement       pause popup prompt return setenv start
syn keyword stolStatement       stop stream system wait write while then
syn keyword stolBoolean         true false
syn keyword stolDeclarator      local global

syn keyword stolBuiltin         gmt int real time abs loge log10 mod sqrt
syn keyword stolBuiltin         sin cos tan asin acos atan dec hex lower
syn keyword stolBuiltin         oct upper string strstr substr env exec
syn keyword stolBuiltin         is_file nargs system clock ue is_normal
syn keyword stolBuiltin         is_warn is_error is_stale linear raw
syn keyword stolBuiltin         state volts

syn match   stolBoolean2        "\.\(T\(RUE\)\?\|F\(ALSE\)\?\)\."
syn match   stolOperatorErr     "\.[A-Z]\+[A-Z0-9_]*\>\.\?"
syn match   stolOperator2       "\.\(AND\|OR\|XOR\|NOT\|EQ\|NE\|LT\|GT\|LE\|GE\)\."


" Comments
"=========
syn keyword stolTodoi contained TODO
syn match   stolTodo ":[A-Z0-9_]*[A-Z_]\+[A-Z0-9_]*:" contained
syn match   stolComment "#.*$" contains=stolTodo

" Labels
" ======
syn match   stolLabel "^\s*[A-Z]\+[A-Z0-9_]*\s*:"

" String and Character constants
"===============================
syn match   stolNumber          "-\=\<\d\+\>"
syn match   stolHex             "\<0x[A-F0-9]\+\>"
syn match   stolOct             "\<0o[0-7]\+\>"
syn region  stolDoubleQuote     start=+"+ skip=+\\"+ end=+"+


" Dereferences
" ============
syn match  stolDeref "\$[a-zA-Z_][a-zA-Z0-9_]*\>"


" Highlighting
" ============
if !exists("did_stol_syntax_inits")
 let did_stol_syntax_inits = 1

 hi link stolDeref               Identifier
 hi link stolDoubleQuote         stolString
 hi link stolWrapLineOperator    stolOperator

 hi link stolBuiltin             Function
 hi link stolDeclarator          PreProc
 hi link stolBoolean             Number
 hi link stolBoolean2            stolBoolean
 hi link stolStatement           Statement
 hi link stolSpecialVariables    Identifier

 hi link stolParenError          Error

 hi link stolComment             Comment
 hi link stolNumber              Number
 hi link stolHex                 stolNumber
 hi link stolOct                 stolNumber
 hi link stolOperator            Operator
 hi link stolOperator2           stolOperator
 hi link stolOperatorErr         Error
 hi link stolString              String
 hi link stolTodo                Todo
 hi link stolLabel               Label

endif

" Current Syntax
" ==============
let b:current_syntax = "stol"

" vim:ts=8 et:
