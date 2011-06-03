" Vim syntax file
" Language: Rcscript
" Maintainer: Hans Winderix <fa356928@skynet.be>
" Last Change: 2007 Jul 27
" Filenames: *.rcs
" Version: 1.0.0

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

"syn case ignore
syn sync lines=250

syn keyword rcBoolean               TRUE FALSE
syn keyword rcConditional           IF ELSE THEN ELSIF
syn keyword rcOperator              TEST OR AND IN TO NOT MOD CAN IS
syn keyword rcConstant              VOID
syn keyword rcRepeat                WHILE FOR DO
syn keyword rcStatement             BEGIN END CONST VAR MODULE PERSISTENT
syn keyword rcStatement             PROCEDURE FUNCTION
syn keyword rcInclude               USE
syn keyword rcQuantifier            FOR_ALL THERE_IS
syn keyword rcQuantifier            COUNT FIRST LAST

syn region rcString matchgroup=rcString start=+'+ end=+'+
syn region rcString matchgroup=rcString start=+"+ end=+"+

syn match rcSymbolOperator          "[+\-/*=]"
syn match rcSymbolOperator          "[<>]=\="
syn match rcSymbolOperator          ":="
syn match rcSymbolOperator          ":-"
syn match rcSymbolOperator          "#"
syn match rcSymbolOperator          "&"
syn match rcSymbolOperator          "|"
syn match rcSymbolOperator          "[<>]@"
"syn match rcMatrixDelimiter         "[][]"
"syn match rcListDelimiter           "[}{]"

syn match rcNumber                  "-\=\<\d\+\>"
syn match rcFloat                   "-\=\<\d\+\.\d\+\>"

syn keyword rcTodo                  contained TODO TBD

syn region rcInterpolation          start="\(\s*`.*\)\@<=`[^`]*"  end="`" contained contains=rcNumber
syn region rcComment                start="--" end="$" contains=rcTodo
syn region rcListStringEl           start="`"  end="$" contains=rcInterpolation

syn keyword rcPredefined            RESULT INIT TERMINATE ASSERT TRACE
syn keyword rcEnvironment           ASSOC IO LEX LIST MATH
syn keyword rcEnvironment           PATCH STR SYS XML

" The default highlighting
hi def link rcBoolean               Boolean
hi def link rcComment               Comment
hi def link rcConditional           Conditional
hi def link rcConstant              Constant
"hi def link rcEnvironment           rcPredefined
hi def link rcInclude               Statement
hi def link rcFloat                 Float
hi def link rcFunction              Function
hi def link rcInterpolation         PreProc
hi def link rcListStringEl          Special
hi def link rcListDelimiter         Identifier
hi def link rcMatrixDelimiter       Identifier
hi def link rcNumber                Number
hi def link rcOperator              Operator
"hi def link rcPredefined            rcStatement
hi def link rcQuantifier            rcOperator
hi def link rcRepeat                Repeat
hi def link rcStatement             Statement
hi def link rcString                String
hi def link rcSymbolOperator        rcOperator
hi def link rcTodo                  Todo

let b:current_syntax = "rcscript" 
