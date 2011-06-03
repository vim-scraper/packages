" Vim syntax file
" Language: Maxscript
" Maintainer: Jens Berlips


"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" comments
syn match msComment /--.*$/ contains=msTodo
syn match msString /".*"/ contains=msSpecial
syn keyword msStatement global fn return continue classOf
syn keyword msStruct	struct
syn keyword msLabel	case default at
syn keyword msConditional if else switch elseif
syn keyword msRepeat	while in for to do then
syn match   msOperator		"!\|>\|<\|+\|\*\|/\|=\|&\||"
syn keyword msOperator		and or
syn match   msOperator		"=="
syn keyword msOperator		not
syn region msFuncreg	start=/fn/ end=/)--fn/ fold contains=all
syn keyword msTodo		contained TODO FIXME XXX
syn case ignore
syn keyword msFunc		format writestring writelong writeshort writebyte 
syn match	msSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn match 	msDigit		"\d+"
syn region msBlock	transparent start='(' end=')' contains=ALL

hi def link msString	String
hi def link msComment	Comment
hi def link msStatement	Structure 
hi def link msLabel		Label
hi def link msStruct 	Structure
hi def link msFunc		Function
hi def link msDigit		Number
hi def link msConditional Conditional 
hi def link msRepeat	Repeat
hi def link msTodo		Todo
hi def link msOperator Operator 
hi def link msSpecial Special
