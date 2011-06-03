" Vim syntax file
" Language:	SR
" Version:	1.0
" Last Change:	2002/12/18 20:00
" Maintainer:	David Holm <david@realityrift.com>
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Keywords
syntax keyword srType		int real bool char ptr enum cap file const op
syntax keyword srStatement	if fi else co oc \/\/ \-\> \[\] in ni
syntax keyword srStatement	resource procedure process end return proc body
syntax keyword srStatement	var sem rec type returns skip cap create
syntax keyword srBoolean	true false
syntax keyword srRepeat		do od fa af next exit
syntax keyword srExternal	import
syntax keyword srOperator	and or not mod

syntax keyword srFunction	real abs max min "int
syntax keyword srFunction	low high
syntax keyword srFunction	lb ub length maxlength
syntax keyword srFunction	sqrt log exp sin ceil floor round
syntax keyword srFunction	random
syntax keyword srFunction	read write writes getarg numargs
syntax keyword srFunction	P V
syntax keyword srFunction	send receive call

" Match
syntax match srComment		/#.*/
syntax match srNumber		"-\=\<\d\+\>"
syntax match srReal		"-\=\<\d\+\.\d\+\>"
syntax match srSymbolOperator	"[+\-/*=]"
syntax match srSymbolOperator	"++"
syntax match srSymbolOperator	"--"
syntax match srSymbolOperator	"[<>]=\="
syntax match srSymbolOperator	"<>"
syntax match srSymbolOperator	"!="
syntax match srSymbolOperator	":="
syntax match srSymbolOperator	":=:"
syntax match srSymbolOperator	"[()]"
syntax match srSymbolOperator	"\.\."
syntax match srSymbolOperator	"[\^.]"

" Regions
syntax region srComment		start=/\/\*/ skip=/\/\*/ end=/\*\//
syntax region srString		start=/"/ skip=/\\"/ end=/"/
syntax region srBlock		start=/if/ end=/fi/ contains=ALL
syntax region srBlock		start=/if/ end=/\[\]/ contains=ALL
syntax region srBlock		start=/do/ end=/od/ contains=ALL
syntax region srBlock		start=/fa/ end=/af/ contains=ALL
" syntax region srBlock		start=/co/ end=/oc/ contains=

" Highlights
highlight link srType		Type
highlight link srStatement	Statement
highlight link srComment	Comment
highlight link srString		String
highlight link srBoolean	Boolean
highlight link srRepeat		Repeat
highlight link srOperator	Operator
highlight link srNumber		Number
highlight link srReal		Float
highlight link srSymbolOperator	srOperator
highlight link srFunction	Function
highlight link srExternal	Include
