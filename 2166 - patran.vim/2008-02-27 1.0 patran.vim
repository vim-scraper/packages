" Vim syntax file
" Language: Patran PCL
" Maintainer: Jacobo Díaz (jdiaz@udc.es)
" Last change: February 27, 2008
" 
" Thanks to the authors and maintainers of fortran.vim and nastran.vim.
"
"----------------------------------------------------------------------
" Remove any old syntax stuff hanging around
"syn clear
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
syn case ignore " PCL is not case dependent
"
"--------------------PCL SYNTAX---------------------------------------
"
syn region patranPCLString  start=+"+ end=+"+ oneline
syn region patranPCLString  start=+'+ end=+'+ oneline
" Any integer
syn match patranPCLNumber  "-\=\<[0-9]\+\>"
" floating point number, with dot, optional exponent
syn match patranPCLFloat  "\<[0-9]\+\.[0-9]*\([edED][-+]\=[0-9]\+\)\=\>"
" floating point number, starting with a dot, optional exponent
syn match patranPCLFloat  "\.[0-9]\+\([edED][-+]\=[0-9]\+\)\=\>"
" floating point number, without dot, with exponent
syn match patranPCLFloat  "\<[0-9]\+[edED][-+]\=[0-9]\+\>"
syn match patranPCLLogical "\(true\|false\)"

" -------Comments
syn match patranPCLComment "^[\$].*$"
syn match patranPCLComment "\$.*$"
syn match patranPCLComment "^[\$].*$" contained
syn match patranPCLComment "\$.*$"  contained
syn region patranPCLComment start="/\*" end="\*/"

" -------Implicit Functions
syn keyword patranPCLImplicit BREAK    BY      CASE        CLASS    CLASSWIDE
syn keyword patranPCLImplicit CONTINUE DEFAULT DUMP        ELSE     END
syn keyword patranPCLImplicit FALSE    FOR     FUNCTION    GLOBAL
syn keyword patranPCLImplicit IF       INFORMATIVE         INTEGER  LIST
syn keyword patranPCLImplicit LOCAL    LOGICAL ON          READONLY REAL
syn keyword patranPCLImplicit REPEAT   RETURN  STATIC      STRING   SWITCH
syn keyword patranPCLImplicit THEN     TO      TRUE        UNTIL    VIRTUAL
syn keyword patranPCLImplicit WHILE    WIDGET  WIDGET_NULL

if version >= 508 || !exists("did_patran_syntax_inits")
  if version < 508
     let did_patran_syntax_inits = 1
     command -nargs=+ HiLink hi link <args>
  else
     command -nargs=+ HiLink hi link <args>
  endif
  " The default methods for highlighting.  Can be overridden later
  HiLink patranPCLString	     String
  HiLink patranPCLNumber	     Constant
  HiLink patranPCLFloat	             Constant
  HiLink patranPCLComment	     Comment
  HiLink patranPCLImplicit	     Identifier
  HiLink patranPCLLogical	     Statement
  delcommand HiLink
endif

let b:current_syntax = "patran"

"EOF vim: ts=8 noet tw=120 sw=8 sts=0
