" Vim syntax file
" Language:	Small Lisp
" Maintainer:  	Michael M. Tung <michael.tung@uv.es>,
"              	Ismael Urraca Piedra <iurraca@yahoo.es>
" Last Change:	Wed Sep 18 19:08:40 CEST 2002

" First public release based on the syntax specifications in
" as defined in the reference book: Robert D. Cameron and
" Anthony H. Dixon, Symbolic Computing with Lisp, Prentice Hall,
" 1992.

" The Small Lisp (smlisp) Home Page is located at
" http://www.cs.sfu.ca/~cameron/smlisp/.

" This syntax file is still in development. Please feel free
" to send any suggestions to the maintainers. 

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

"Small Lisp functions
syn keyword SmLispFunction	cons divide endp eq eqp error explode
syn keyword SmLispFunction	first greaterp implode lessp listp minus
syn keyword SmLispFunction	numberp plus rem rest sym-lessp symbolp times

"Small Lisp variables
syn keyword SmLispVariable	T F	

"Small Lisp comments
syn match   SmLispComment	";;;.*$"

"Small Lisp delimiters
syn match   SmLispDelim		"[][{};:,]"

"catch errors caused by wrong parenthesis and brackets
syn cluster SmLispAll		contains=SmLispRegion,SmLispDelim,SmLispFunction,SmLispVariable,SmLispComment,SmLispParenError,SmLispSpecial,SmLispNumber,SmLispClause,SmLispCommand
syn region  SmLispRegion	matchgroup=Delimiter start="(" skip="|.\{-}|" matchgroup=Delimiter end=")" contains=@SmLispAll
syn region  SmLispRegion	matchgroup=Delimiter start="\[" skip="|.\{-}|" matchgroup=Delimiter end="\]" contains=@SmLispAll
syn region  SmLispRegion	matchgroup=Delimiter start="{" skip="|.\{-}|" matchgroup=Delimiter end="}" contains=@SmLispAll
syn match   SmLispParenError	"]"
syn match   SmLispParenError	")"
syn match   SmLispParenError	"}"

"Small Lisp specials
syn match   SmLispSpecial	"+"
syn match   SmLispSpecial	"-"
syn match   SmLispSpecial	"*"
syn match   SmLispSpecial	"/"
syn match   SmLispSpecial	"<"
syn match   SmLispSpecial	">"
syn match   SmLispSpecial	"="
syn match   SmLispSpecial	"&"
syn match   SmLispSpecial	"|"
syn match   SmLispSpecial	"!"
syn match   SmLispSpecial	"@"
syn match   SmLispSpecial	"#"
syn match   SmLispSpecial	"$"
syn match   SmLispSpecial	"%"
syn match   SmLispSpecial	"?"
syn match   SmLispSpecial	":"

"supporting integer numbers
syn match   SmLispNumber	"\<[0-9]\d*\>"
syn match   SmLispNumber	"-\d\+\>" contains=Number

"Small Lisp clause arrow
syn match   SmLispClause	"-->"

"Small Lisp commands
syn match   SmLispCommand	"/help"
syn match   SmLispCommand	"/info"
syn match   SmLispCommand	"/log"
syn match   SmLispCommand	"/read"
syn match   SmLispCommand	"/reset"
syn match   SmLispCommand	"/save"
syn match   SmLispCommand	"/show"
syn match   SmLispCommand	"/stop"

" synchronization
syn sync lines=100

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_SmLisp_syntax_inits")
  if version < 508
    let did_SmLisp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink SmLispComment		Comment
  HiLink SmLispFunction		Statement
  HiLink SmLispClause		Operator
  HiLink SmLispDelim		Delimiter
  HiLink SmLispNumber		Number
  HiLink SmLispSpecial		Type
  HiLink SmLispCommand		String
  HiLink SmLispVariable		Statement
  HiLink SmLispParenError	Error
  HiLink SmLispBracketError	Error
  delcommand HiLink
endif

let b:current_syntax = "smlisp"

" vim: ts=8 nowrap
