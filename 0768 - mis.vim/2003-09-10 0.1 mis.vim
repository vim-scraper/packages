" Vim syntax file
" Language: Minimal Instruction Set (MIS)
" Maintainer: Dennis Craven <arker66@hotmail.com>
" Last Change: 09-Sep-2003 


syntax clear

syntax case ignore

syntax keyword misStatement ADDI CBI HALT
syntax match misIdentifier "[a-z0-9_]*_@"
syntax match misIdentifier2 "[a-z0-9_]*_@@"
syntax match misLabel    "[a-z_]*[0-9]*:"
syn match misComment		";.*"

syn match decNumber   "0\+[1-7]\=[\t\n$,; ]"
syn match decNumber   "[1-9]\d*"
syn match octNumber   "0[0-7][0-7]\+"
syn match hexNumber   "0[xX][0-9a-fA-F]\+"
syn match binNumber   "0[bB][0-1]*"

highlight link misStatement Statement
highlight link misLabel Label
highlight link misIdentifier Identifier
highlight link misIdentifier2 Include
highlight link misComment Comment
highlight link hexNumber  Number
highlight link decNumber  Number
highlight link octNumber  Number
highlight link binNumber  Number

