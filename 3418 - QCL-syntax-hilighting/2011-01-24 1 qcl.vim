" Vim syntax file
" Language:		QCL
" Maintainer:	George Bargoud <george@bargoud.com>
" Last Change:	2010 Dec 30

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" built in keywords
	syn keyword qclKeyWords for while include measure print bit reset if const break to dump extern return load save list until set step input exit shell plot continue else 

" built in ops
	syn keyword qclOps mod not and or xor
	syn match qclOps "= *cond"hs=e-3

" data types - classic
	syn keyword qclClassicTypes int real boolean complex string

" data types - quantum
	syn keyword qclQuantumTypes qureg quconst quvoid quscratch

" different subroutine types, each has a different role
	syn keyword qclSubroutines qufunct operator procedure cond 

" built in operators
	" arbitrary unitary n by n matrix represented by Matrix${n}x${n}
	syn match qclOperators "Matrix\([0-9]\+\)x\1"
	" The rest of the built in operators
	syn keyword qclOperators Rot Mix CPhase Phase RotZ H

" built in quantum functions
	" permute a power of 2 items
	syn match qclQuFunctions "Perm\(2\|4\|8\|16\|32\|64\)"
	" the rest of the built in functions
	syn keyword qclQuFunctions Not CNot Fanout Swap -> <- 

" built in functions
	syn keyword qclFunctions ceil floor max min gcd lcm random
	
" built in mathematical functions
	syn keyword qclMath sin cos tan cot sinh cosh tanh coth Re Im abs conj exp log sqrt

" escape characters that may be found inside of strings
	syn match qclEscape contained "\\."

" numbers, strings, etc.
	" numbers: 
	" [\^ (\[=+\-\*/,] keeps it from highlighting numbers in variable names, 
	" hs=s+1 drops the first character from the highlighting
	syn match qclValue /[\^ (\[=+\-\*/,]-\=[0-9]\+\.\=[0-9]*/hs=s+1
	" match strings
	syn region qclValue start="\"" end="\"" contains=qclEscape
	" match boolean
	syn keyword qclValue true false
	" match complex values
	syn match qclValue "(-\=[0-9]\+\.\=[0-9]*,-\=[0-9]\+\.\=[0-9]*)"
	" match size of qureg operation
	syn match qclValue "#[a-zA-Z][a-zA-Z0-9]*"

" comment types, block and line
	" These are highlighted only when inside of comments.
	syn keyword qclTodo contained TODO FIXME XXX NOTE
	" Block comment
	syn region qclComment start="/\*" end="\*/" fold contains=qclTodo
	" Line comment
	syn match qclComment "//.*$" contains=qclTodo


let b:current_syntax = "qcl"

hi def link qclTodo			Todo
hi def link qclComment		Comment
hi def link qclValue		Constant
hi def link qclEscape		PreProc
hi def link qclClassicTypes	Type
hi def link qclQuantumTypes	Type
hi def link qclMath			PreProc
hi def link qclFunctions	PreProc
hi def link qclQuFunctions	PreProc
hi def link qclOperators	PreProc
hi def link qclKeyWords		Statement
hi def link qclSubroutines	Function
hi def link qclOps			PreProc
