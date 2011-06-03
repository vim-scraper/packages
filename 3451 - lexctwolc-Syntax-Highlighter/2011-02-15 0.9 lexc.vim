" Vim syntax file
" Language: lexc/twolc
" Maintainer: Brendan Molloy <brendan@bbqsrc.net>
" Last Change: 2011-02-11
" Version: 0.9


if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword lexcLabel ALPHABET Alphabet Sets Rules Multichar_Symbols END 
syn keyword lexcLexicon LEXICON nextgroup=lexcLexiconB skipwhite
syn match   lexcLexiconB "[a-zA-Z_][a-zA-Z0-9_]*" contained
syn match	lexcLexiconB "[\#\_]"

" Identifiers
syn match lexcFlagDiacritic   "@[^@][^@]*@"

" Symbols
syn match lexcSymbol "<[^<>][^<>]*>"
syn match lexcSymbol "[<=|=>|/<=|<=>]"
syn region lexcString matchgroup=Normal start=+[uU]\="+ end=+"\|\n+ skip=+\\\\\|\\"+ contains=lexcSymbol
syn match   lexcSymbol +\\["'\\]+ contained
syn match 	lexcSymbol "[\:]"

" Comment
syn match lexcComment "\!.*$"

" Operators
syn match lexcOperator "[\.\*\+\?|\\\%\^]"

hi def link lexcLabel          	Label
hi def link lexcLexicon			Statement
hi def link lexcLexiconB		Function
hi def link lexcComment        	Comment
hi def link lexcOperator		Operator
hi def link lexcFlagDiacritic  	Identifier
hi def link lexcString			String
hi def link lexcSymbol			String

let b:current_syntax = "lexc"

