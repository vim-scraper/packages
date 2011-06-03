" Vim syntax file
" Language:         BNF
" Maintainer:       Michael Brailsford
" Last Change:      March 22, 2002
"
syn match bnfNonTerminal "<\a\w*>" contained
syn region bnfProduction start="^\a" end="::="me=e-3 contained
syn match bnfOr "|" contained
syn match bnfSeperator "::=" contained
syn match bnfComment "#.*$" contained
syn match bnfQuoted #".*"# contains=bnfNonTerminal,bnfProduction,bnfOr,bnfSeperator,bnfLiteral,bnfTerminalRangeDelim,bnfTerminalRange
syn match bnfQuoted #'.*'# contains=bnfNonTerminal,bnfProduction,bnfOr,bnfSeperator,bnfLiteral,bnfTerminalRangeDelim,bnfTerminalRange
"syn match bnfLiteral #"[ \w]*"# contained
syn match bnfTerminal "^.*$" contains=bnfNonTerminal,bnfProduction,bnfOr,bnfSeperator,bnfComment,bnfLiteral,bnfTerminalRangeDelim,bnfTerminalRange,bnfQuoted
syn region bnfTerminalRange matchgroup=bnfTerminalRangeDelim start=#\[# end=#\]#

hi link bnfNonTerminal 	Type
hi link bnfProduction 	Constant
hi bnfOr ctermfg=1 gui=bold guifg=red
hi link bnfSeperator 	PreProc
hi bnfTerminal 	guifg=cyan
hi link bnfComment 		Comment
hi bnfTerminalRangeDelim guifg=yellow gui=bold
hi link bnfTerminalRange bnfTerminal
hi link bnfQuoted bnfTerminal
"hi link bnfLiteral 	 	String
