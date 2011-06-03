" Vim syntax file
" Language: Blue
" Maintainer: Higor Eurípedes
" Latest Revision: 16 August 2009

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syn keyword blTodo contained TODO FIXME XXX NOTE
syn match blComment "#.*$" contains=blTodo

syn keyword blException   raise
syn keyword blConditional ? :
syn keyword blRepeat      loop
syn keyword blBoolean     true false
syn keyword blStatement   return def func
syn match   blNumber      "-\=\<\d\+\>" 
syn match   blNumber      "\<0x\x\{1,8}\>" 
syn region  blString      matchgroup=None start=+"+ skip=+\\\\\|\\"+ end=+"+ 
syn region  blString      matchgroup=None start=+'+ skip=+\\\\\|\\'+ end=+'+ 
syn keyword blIdentifiers       print import library module attribs class this sys self del global lexical
syn keyword blDictionaryMethods length keys values
syn keyword blHashMethods       md5 
syn keyword blRandomMethods     max random unit
syn keyword blRegexMethods      pattern match matchAll exec
syn keyword blThreadMethods     async mutex join lock unlock
syn keyword blTimeMethods       time duration
syn keyword blSocketMethods     tcp udp bind listen recv accept sendto 
syn keyword blStreamMethods     exec shell memory file stdio socket 
syn keyword blSqlMethods        open 
syn keyword blXmlMethods        parser parse onError startElement endElement characters getTag getTags getData

hi def link blTodo        Todo
hi def link blComment     Comment
hi def link blStatement   Statement
hi def link blRepeat      Repeat
hi def link blOperator    Operator
hi def link blConditional Conditional
hi def link blException   Exception
hi def link blString      String
hi def link blNumber      Number
hi def link blBoolean     Boolean
hi def link blIdentifiers        Identifier
hi def link blDictionaryMethods  Function
hi def link blHashMethods        Function
hi def link blRandomMethods      Function
hi def link blRegexMethods       Function
hi def link blThreadMethods      Function
hi def link blTimeMethods        Function
hi def link blSocketMethods      Function
hi def link blStreamMethods      Function
hi def link blSqlMethodsi        Function
hi def link blXmlMethods         Function

