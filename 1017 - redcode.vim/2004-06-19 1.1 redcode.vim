" Vim syntax file
" Language: redcode assembler
" Maintainer: Lorenz Wegener <lorenzwegener@hotmail.com>
" Last Change: 2004 June 18
"
" $Revision: 1.1 $
" $Date: 2004/06/19 21:43:38 $

if exists("b:current_syntax")
	finish
endif

" identifiers
syn match redcodeIdentifier "[a-zA-Z0-9_]*\>"

" keywords
syn match redcodeKeyword "DAT\>"
syn match redcodeKeyword "dat\>"
syn match redcodeKeyword "MOV\>"
syn match redcodeKeyword "mov\>"
syn match redcodeKeyword "ADD\>"
syn match redcodeKeyword "add\>"
syn match redcodeKeyword "SUB\>"
syn match redcodeKeyword "sub\>"
syn match redcodeKeyword "MUL\>"
syn match redcodeKeyword "mul\>"
syn match redcodeKeyword "DIV\>"
syn match redcodeKeyword "div\>"
syn match redcodeKeyword "MOD\>"
syn match redcodeKeyword "mod\>"
syn match redcodeKeyword "JMP\>"
syn match redcodeKeyword "jmp\>"
syn match redcodeKeyword "JMZ\>"
syn match redcodeKeyword "jmz\>"
syn match redcodeKeyword "JMN\>"
syn match redcodeKeyword "jmn\>"
syn match redcodeKeyword "DJN\>"
syn match redcodeKeyword "djn\>"
syn match redcodeKeyword "SPL\>"
syn match redcodeKeyword "spl\>"
syn match redcodeKeyword "CMP\>"
syn match redcodeKeyword "cmp\>"
syn match redcodeKeyword "SEQ\>"
syn match redcodeKeyword "seq\>"
syn match redcodeKeyword "SNE\>"
syn match redcodeKeyword "sne\>"
syn match redcodeKeyword "SLT\>"
syn match redcodeKeyword "slt\>"
syn match redcodeKeyword "LDP\>"
syn match redcodeKeyword "ldp\>"
syn match redcodeKeyword "STP\>"
syn match redcodeKeyword "stp\>"
syn match redcodeKeyword "NOP\>"
syn match redcodeKeyword "nop\>"
syn match redcodeKeyword "LDP\>"
syn match redcodeKeyword "ldp\>"
syn match redcodeKeyword "STP\>"
syn match redcodeKeyword "stp\>"

" pseudo keywords
syn match redcodeDirective		"EQU\>"
syn match redcodeDirective		"equ\>"
syn match redcodeDirective		"FOR\>"
syn match redcodeDirective		"for\>"
syn match redcodeDirective		"ROF\>"
syn match redcodeDirective		"rof\>"
syn match redcodeDirective		"PIN\>"
syn match redcodeDirective		"pin\>"

" labels
syn match redcodeLabel		"[a-zA-z0-9]*:"he=e-1

" addressing modes
syn match redcodeAddrOp		">"
syn match redcodeAddrOp		"<"
syn match redcodeAddrOp		"}"
syn match redcodeAddrOp		"{"
syn match redcodeAddrOp		"\*"
syn match redcodeAddrOp		"@"
syn match redcodeAddrOp		"\$"
syn match redcodeAddrOp		"\#"

" modifiers
syn match redcodeModifier	"\..."hs=e-2

" comments
syn match redcodeComment	";.*$"hs=s+1

" macros
syn match redcodeMacro		";[ \t]*assert\>"

" comments for name + author, etc
syn match redcodeAuthor		";[ \t]*author\>"
syn match redcodeStrategy	";[ \t]*strategy\>"
syn match redcodeName		";[ \t]*name\>"
syn match redcodeRedcode	";[ \t]*redcode.*\>"

" environment variables
syn match redcodeEnvVar		"CORESIZE\>"
syn match redcodeEnvVar		"PSPACESIZE\>"
syn match redcodeEnvVar		"MAXCYCLES\>"
syn match redcodeEnvVar		"MAXPROCESSES\>"
syn match redcodeEnvVar		"WARRIORS\>"
syn match redcodeEnvVar		"MAXLENGTH\>"
syn match redcodeEnvVar		"CURLINE\>"
syn match redcodeEnvVar		"MINDISTANCE\>"
syn match redcodeEnvVar		"VERSION\>"

if version >= 508 || !exists("did_redcode_syntax_inits")
	if version < 508
		let did_redcode_syntax_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

    HiLink redcodeKeyword		Comment
    HiLink redcodeLabel			Label
    HiLink redcodeDirective		Type
    HiLink redcodeIdentifier	Identifier
    HiLink redcodeModifier		Operator
    HiLink redcodeMacro			Macro
    HiLink redcodeAuthor		Special
    HiLink redcodeStrategy		Special
    HiLink redcodeName			Special
    HiLink redcodeRedcode		Special
    HiLink redcodeComment		Comment
    HiLink redcodeEnvVar		Special

    delcommand HiLink

endif

let b:current_syntax = "redcode"
" vim: set tabstop=4: 


