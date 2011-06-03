" Vim syntax file
" Language:	Miranda
" Maintainer:	xbtsw <xbtsw.chen@gmail.com>
" Last Change:	2008 Oct 21

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syntax case match

" Miranda keywords
syn keyword	mStatement	goto break return continue asm
syn keyword	mConditional	if otherwise
syn keyword	mType		num char bool
syn keyword	mSpecial	abstype div where mod readvals show type with sys_message False True Appendfile Closefile Exit Stderr Stdout System Tofile abs and arctan cjustify code concat const converse cos decode digit drop dropwhile entier error exp filemode filter foldl foldl1 foldr foldr1 force fst getenv hd hugenum id index init integer iterate last lay layn letter limit lines ljustify log log10 map map2 max max2 member merge min min2 mkset neg numval or pi postfix product read rep repeat reverse rjustify scan seq showfloat shownum showscaled sin snd sort spaces sqrt subtract sum system take takewhile tinynum tl transpose undef until zip2 zip3 zip4 zip5 zip6 zip

" Operators
syn match	mOperators	"[\(\)\[\]\.,;:&|<>!=/*%+-\~^]"

" Comments
syn match mComment "||.*" oneline

" Identifiers
syn match mIdentifier /[a-zA-Z_][a-zA-Z0-9_]*/

" String and Character constants
syn region mString start=/"/ skip=/\\"/ end=/"/
syn region mString start=/'/ skip=/\\'/ end=/'/

" num constants
syn match mNumber /[0-9]\+\.*[0-9]*e*[0-9]*/

" boolean
syn keyword mBoolean True False

" Highlights
highlight link mStatement Statment
highlight link mConditional Conditional
highlight link mType Type
highlight link mSpecial Special
highlight link mComment Comment
highlight link mOperators Operator
highlight link mIdentifier Identifier
highlight link mString String
highlight link mNumber Number
highlight link mBoolean Boolean


let b:current_syntax = "Miranda"

" vim: ts=8
