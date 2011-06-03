" Vim syntax file
" Language:     EDI
" Maintainer:   Jeremy Cowgar <jeremy@cowgar.com>
" Last Change:  Tue Oct 19 21:54:02 EDT 2004

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syn keyword ediKeywords    GS GE SE ST
syn match   ediSegments    "^[A-Z0-9]\+"
syn match   ediSeperators  "\*"
syn match   ediTerminators "\~"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_edi_syntax_inits")
	if version < 508
		let did_edi_syntax_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink ediKeywords			Comment
	HiLink ediSegments          String
	HiLink ediSeperators        Operator
	HiLink ediTerminators       Number
	
	delcommand HiLink
endif

let b:current_syntax = "edifile"

" vim: ts=4

