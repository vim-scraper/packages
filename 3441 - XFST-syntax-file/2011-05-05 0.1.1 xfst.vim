" Vim syntax file
" Language:	XFST Scripts
" Maintainer:	Ondrej Dusek
" Last Change: 2011-02-05

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn keyword     xfstTodo	contained TODO FIXME XXX
syn keyword     xfstKeyWord     define clear push apply up read regex source

" Special characters
syn match	xfstSpecChar	"\.#\." 
syn match	xfstSpecChar	"[?0:_]" 
syn match       xfstSpecChar    "\.\.\."

" Brackets
syn match	xfstBracket	"\["
syn match	xfstBracket	"\]"
syn match	xfstBracket	"("
syn match	xfstBracket	")"

" Operators
syn match	xfstOperator	"[+*~|&\\/,;<>]"
syn match       xfstOperator    "(\?@\?<\?->\?@\?)\?"
syn match       xfstOperator    "<\?=>\?"
syn match	xfstOperator	"\.x\."
syn match	xfstOperator	"\.o\."
syn match       xfstNorm        "%."

" Strings
syn match       xfstString      "{[^}]*}"
syn match       xfstNorm        "\"[^\"]*\""

" Comments
syn match	xfstComment	"^!.*" contains=xfstTodo
syn match	xfstComment	"\s!.*"ms=s+1 contains=xfstTodo

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link xfstComment		Comment
hi def link xfstTodo		Todo
hi def link xfstOperator	Special
hi def link xfstBracket		Type
hi def link xfstSpecChar        PreProc
hi def link xfstKeyword 	Keyword
hi def link xfstNorm            Identifier
hi def link xfstString          String

let b:current_syntax = "xfst"

" vim: ts=8 sw=2
