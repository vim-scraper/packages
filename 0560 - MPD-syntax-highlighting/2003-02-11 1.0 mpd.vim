" Vim syntax file
" Language:	    MPD
" Version:	    1.0
" Last Change:	2003/01/12
" Maintainer:	Lukas Zapletal [lzap@root.cz]

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Keywords
syntax keyword srType		int real double bool char ptr enum cap file string const optype
syntax keyword srStatement	if else co oc \/\/ \-\> \[\] in by ni to op
syntax keyword srStatement	resource procedure process end return reply proc body global begin
syntax keyword srStatement	var sem rec type returns skip stop cap create destroy union external on
syntax keyword srBoolean	true false
syntax keyword srRepeat		while for next exit
syntax keyword srExternal	import extends
syntax keyword srOperator	and or not mod xor

" (built-in core functions)
syntax keyword srFunction	send receive call fork forward
syntax keyword srFunction	P V
" (predefined functions as taken from predefs.h)
syntax keyword srFunction	lb ub lb1 ub1 lb2 ub2 pred succ length maxlength 
syntax keyword srFunction	chars locate myresource mymachine myvm mypriority setpriority
syntax keyword srFunction	free abs min max sqrt log exp ceil floor round
syntax keyword srFunction	sin cos tan asin acos atan random seed 
syntax keyword srFunction	numargs getarg nap age open flush close remove 
syntax keyword srFunction	get put seek where read scanf sscanf 
syntax keyword srFunction	write writes printf sprintf

" Match
syntax match srComment		/#.*/
syntax match srNumber		"-\=\<\d\+\>"
syntax match srReal		"-\=\<\d\+\.\d\+\>"
syntax match srSymbolOperator	"[+\-/*=]"
syntax match srSymbolOperator	"++"
syntax match srSymbolOperator	"--"
syntax match srSymbolOperator	"@"
syntax match srSymbolOperator	"*"
syntax match srSymbolOperator	"?"
syntax match srSymbolOperator	"%"
syntax match srSymbolOperator	"^"
syntax match srSymbolOperator	"&"
syntax match srSymbolOperator	"|"
syntax match srSymbolOperator	"[<>]=\="
syntax match srSymbolOperator	"<<"
syntax match srSymbolOperator	">>"
syntax match srSymbolOperator	"!="
syntax match srSymbolOperator	":="
syntax match srSymbolOperator	":=:"
syntax match srSymbolOperator	"[()]"
syntax match srSymbolOperator	"\.\."
syntax match srSymbolOperator	"[\^.]"

" Regions
syntax region srComment		start=/\/\*/ skip=/\/\*/ end=/\*\//
syntax region srString		start=/"/ skip=/\\"/ end=/"/
syntax region srBlock		start=/in/ end=/ni/ contains=ALL
syntax region srBlock		start=/resource/ end=/end/ contains=ALL
syntax region srBlock		start=/global/ end=/end/ contains=ALL

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
