" LPL syntax file
" Language:		LPL (Linear/Logical Programming Language)
" Author:       Roberto Bonvallet <rbonvall@inf.utfsm.cl>
" Last Change:  20041106

" Reference: PDF manual shipped with the free version.

syntax case ignore

" keywords defined on the documentation
syntax keyword lplStatement abel_dqs abel_dpl abel_dsp abel_q$
syntax keyword lplStatement abel_pl abel_sp addm alias
syntax keyword lplStatement argmax argmin assumption atleast
syntax keyword lplStatement atmost begin check col
syntax keyword lplStatement constraint data default
syntax keyword lplStatement do else empty end exactly
syntax keyword lplStatement for freeze from
syntax keyword lplStatement max maximize minimize
syntax keyword lplStatement model observe option
syntax keyword lplStatement parameter priority procedure prob
syntax keyword lplStatement query read row
syntax keyword lplStatement section set subject
syntax keyword lplStatement then to unfreeze unit
syntax keyword lplStatement variable write

" more specific items
syntax keyword lplType        real free integer distinct binary string
syntax keyword lplFunction    abs arctan break ceil floor trunc sin
syntax keyword lplFunction    cos log exp sqrt rnd rndn if date sort
syntax keyword lplFunction    posstr substr while
syntax keyword lplExpression  sum prod forall exist

syntax match  lplNonPrintable display contained
	\ +\\\(a\|b\|f\|n\|r\|t\|\\\|"\)+
syntax region lplString	  start=+'+ skip=+\\'+ end=+'+ contains=lplNonPrintable

syntax match   lplOperator  "[|+-/*^&=()]"
syntax match   lplOperator  "[~#]"  " pasan cosas raras al ponerlos arriba
syntax match   lplOperator  "?[<>]"
syntax match   lplOperator  "[<>]=\="
syntax match   lplOperator  "[<][>]"
syntax keyword lplOperator  in and nand or nor xor
syntax match   lplOperator  "[<]-[>]\?"
syntax match   lplOperator  "->"
syntax match   lplOperator  ":="

syntax match  lplNumber  "\<\d\{-}\>"

syntax match   lplIndex           "\w*" contained contains=lplNumber
syntax match   lplIndexBrackets   "[{].\{-}[}]" contained contains=lplIndex
syntax match   lplIndexSquarePar  "\[.\{-}\]" contained contains=lplIndex
syntax match   lplIteration       "\h\w*{.\{-}}" contains=lplIndexBrackets
syntax match   lplIndexation      "\h\w*\[.\{-}\]" contains=lplIndexSquarePar

" TODO: improve these ones
"syntax region  lplSet		start="/" end="/"
"syntax region  lplData		start="\[" end="\]"


syntax match  lplComment "--.*$"
syntax region lplComment start=+(\*+ skip=+(\*.\{-}\*)+ end=+\*)+
syntax region lplComment start=+"+ end=+"+

highlight def link lplStatement      Statement
highlight def link lplType           Type
highlight def link lplFunction       Function
highlight def link lplExpression     Function
highlight def link lplString         String
highlight def link lplOperator       Operator
highlight def link lplSet            Constant
highlight def link lplData           Constant
"highlight def link lplIndex          Identifier
highlight def link lplIndexBrackets  Operator
highlight def link lplIndexSquarePar Operator
highlight def link lplIteration      Identifier
highlight def link lplIndexation     Identifier
highlight def link lplNumber         Constant
highlight def link lplComment        Comment

let b:current_syntax = "lpl"
