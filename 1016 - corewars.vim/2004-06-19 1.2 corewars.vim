" Vim syntax file
" Language: corewars assembler
" Maintainer: Lorenz Wegener <lorenzwegener@hotmail.com>
" Last Change: 2004 June 18
"
" $Revision: 1.2 $
" $Date: 2004/06/19 21:43:38 $

if exists("b:current_syntax")
	finish
endif

" keywords
syn match coreKeyword "data\>"
syn match coreKeyword "DATA\>"
syn match coreKeyword "loop\>"
syn match coreKeyword "LOOP\>"
syn match coreKeyword "jump\>"
syn match coreKeyword "JUMP\>"
syn match coreKeyword "less\>"
syn match coreKeyword "LESS\>"
syn match coreKeyword "equal\>"
syn match coreKeyword "EQUAL\>"
syn match coreKeyword "fork\>"
syn match coreKeyword "FORK\>"
syn match coreKeyword "add\>"
syn match coreKeyword "ADD\>"
syn match coreKeyword "mul\>"
syn match coreKeyword "MUL\>"
syn match coreKeyword "div\>"
syn match coreKeyword "DIV\>"
syn match coreKeyword "neg\>"
syn match coreKeyword "NEG\>"
syn match coreKeyword "mod\>"
syn match coreKeyword "MOD\>"
syn match coreKeyword "and\>"
syn match coreKeyword "AND\>"
syn match coreKeyword "move\>"
syn match coreKeyword "MOVE\>"
syn match coreKeyword "info\>"
syn match coreKeyword "INFO\>"
syn match coreKeyword "system\>"
syn match coreKeyword "SYSTEM\>"
syn match coreKeyword "own\>"
syn match coreKeyword "OWN\>"
syn match coreKeyword "movei\>"
syn match coreKeyword "MOVEI\>"

" labels
syn match coreLabel		"[a-zA-z0-9]*:"he=e-1

" comments
syn match coreComment	"#.*$"hs=s+1

" title + author
syn match coreAuthor		"author[ \t]*\".*\""
syn match coreTitle			"title[ \t]*\".*\""

if version >= 508 || !exists("did_corewars_syntax_inits")
	if version < 508
		let did_corewars_syntax_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink coreKeyword	Statement
	HiLink coreLabel	Label	
	HiLink coreComment	Comment
	HiLink coreAuthor	Special
	HiLink coreTitle	Special		

	delcommand HiLink

endif

let b:current_syntax = "corewars"
" vim: set tabstop=4: 

