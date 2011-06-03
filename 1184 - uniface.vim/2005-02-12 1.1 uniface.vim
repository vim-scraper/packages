" Vim syntax file
" Language:	Uniface 7
" Maintainer:	Peter Storch <peter.storch@web.de>
" Last change:	2005 Feb 12
" Version: 	1.1

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
syntax clear
elseif exists("b:current_syntax")
finish
endif

" Uniface is a case insensitive language
syn case ignore

" Unfiace precompiler directives
syn region uniPreProc	start="^\s*#\s*include" skip="\\$" end="$"
syn region uniPreProc	start="^\s*#\s*ifdefined" skip="\\$" end="$"
syn region uniPreProc	start="^\s*#\s*endif" skip="\\$" end="$"
syn region uniPreProc	start="^\s*#\s*if" skip="\\$" end="$"
syn region uniPreProc	start="^\s*#\s*for" skip="\\$" end="$"
syn region uniPreProc	start="^\s*#\s*error" skip="\\$" end="$"
syn region uniPreProc	start="^\s*#\s*info" skip="\\$" end="$"
syn region uniPreProc	start="^\s*#\s*warning" skip="\\$" end="$"

syn keyword uniTodo contained	TODO FIXME XXX

" String
syn region  uniString		start=+"+  end=+"+

syn match   uniOperator		"[=\+\-\*\/\<\>|&@%\[\]]"
syn match   uniOperator		"!="

syn match   uniIdentifier	"\<[a-zA-Z_][a-zA-Z0-9_]*\>"

syn match   uniDelimiter	"[():;,]"

syn match   uniNumber		"-\=\<\d\+\>"
syn match   uniNumber		"\<[0-9a-fA-F]*[hH]*\>"

syn match	uniComment	";.*" contains=uniTodo
syntax match	uniCommentError	"\*/"

syn keyword uniConditional if endif while endwhile selectcase endselectcase else elseif case elsecase

syn keyword uniKeyword activate addmonths apexit apstart blockdata break call close clrmess commit
syn keyword uniKeyword compare creocc curocc_video debug delete delete_instance discard display done edit eject end
syn keyword uniKeyword endvariables endparams entry erase exit field_syntax field_video file_dump
syn keyword uniKeyword file_load filebox findkey goto help length lock lookup lowercase macro
syn keyword uniKeyword moveocc newinstance new_instance nodebug numgen numset open operation params
syn keyword uniKeyword perform postmessage pragma print print_break pulldown putmess read refresh release
syn keyword uniKeyword reload remocc repeat reset return
syn keyword uniKeyword rollback run scan selectdb sendmessage set setformfocus setocc show skip spawn
syn keyword uniKeyword sql u_condition u_where uppercase validatefield validatekey
syn keyword uniKeyword validateocc variables webgen webget write xmlload xmlsave
syn match uniKeyword "putitem\(\/id\)\?"
syn match uniKeyword "store\(\/e\|\/complete\)\?"
syn match uniKeyword "clear\(\/e\)\?"
syn match uniKeyword "validate\(\/e\)\?"
syn match uniKeyword "getitem\(\/id\)\?"
syn match uniKeyword "delitem\(\/id\)\?"
syn match uniKeyword "putlistitems\(\/id\|\/occ\)\?"
syn match uniKeyword "getlistitems\(\/occ\|\/id\)\?"
syn match uniKeyword "sort\(\/list\|\/e\)\?"
syn match uniKeyword "/init"
syn match uniKeyword "message\(\/nobeep\)\?\(\/info\|\/error\|\/warning\|\/hint\)\?"
syn match uniKeyword "message\(\/clear\|\/hint\)\?"
syn match uniKeyword "askmess\(\/nobeep\)\?\(\/info\|\/error\|\/warning\|\/question\)\?"
syn match uniKeyword "retrieve\(\/a\|\/o\|\/e\|\/reconnect\|\/x\)\?"

syn match uniVariable "\$\([a-zA-Z0-9_]\)*"
syn match uniVariable "\$\([a-zA-Z0-9_]\)*\$"

syn keyword uniFunction abs acos asin atan cos e exp fact frac idpart int item log log10 pi power sin sqrt tan valuepart

syn keyword uniType numeric string boolean date any
syn match uniType ":\s*in\(out\)\?"
syn match uniType ":\s*out"

syn sync lines=50

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_uniface_syntax_inits")
if version < 508
let did_uniface_syntax_inits = 1
command -nargs=+ HiLink hi link <args>
else
command -nargs=+ HiLink hi def link <args>
endif

" The default methods for highlighting.  Can be overridden later
HiLink uniConditional			Conditional
HiLink uniTodo			Todo
HiLink uniNumber			Number
HiLink uniDelimiter			Operator
HiLink uniOperator			Operator
HiLink uniPreProc			PreProc
HiLink uniKeyword			Statement
HiLink uniVariable			Special
HiLink uniFunction			Function
HiLink uniType				Type
HiLink uniString			String
HiLink uniComment			Comment

delcommand HiLink
endif

let b:current_syntax = "uniface"

" vim: ts=8 sw=2

