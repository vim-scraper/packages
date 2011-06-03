" Vim syntax file
" Language:	generic sofu file
" Maintainer:	Marc Lucksch
" Last Change:	2008 Jun 28


syn keyword	sofuTodo	contained TODO FIXME XXX
syn match	sofuComment	"^#.*" contains=sofuTodo
syn match	sofuComment	"\s#.*" contains=sofuTodo
syn region 	sofuString 	matchgroup=Quote start=+"+  skip=+\\"+	end=+"+
syn keyword	sofuUndef	UNDEF
syn region 	sofuMap	 	start="{" end="}" transparent fold
syn region 	sofuList 	start="(" end=")" transparent fold
syn match	sofuEqual	"="
syn match	sofuKeyEscape	"<[0123456789ABCDEFabcdef]*>"
"syn match	sofuNumber	"[0123456789\.]\+"
syn match	sofuRef		"@\S\+" contains=sofuKeyEscape


" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link sofuComment		Comment
hi def link sofuTodo		Todo
hi def link sofuString		String
hi def link sofuEqual		Operator
hi def link sofuKeyEscape	SpecialChar
"hi def link sofuNumber		Float
hi def link sofuUndef 		Keyword
hi def link sofuRef		Label

let b:current_syntax = "sofu"
