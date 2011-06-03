" Vim syntax file
" Language:    CVOPS EFSA
" Maintainer:  Jani Nurminen <jani.nurminen@intellitel.com>
" Last Change: Thu Sep 12 10:22:57 EEST 2002
" Remark:      CVOPS 6.3
"
" This is quite a quick hack, but works for me :)
" The definitions were taken from CVOPS 6.3 reference manual
" You might want to edit this file to suit your company preferences.
"
" To enable, put something like this e.g. in ~/.vimrc
" 
"   autocmd BufNewFile,BufRead *.aut    set syntax=cvopsefsa
"
" See also :help new-filetype

" Check if already loaded
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" comment, (* blah blah *)
syn region ceComment start=/(\*/ end=/\*)/

" macro
syn keyword ceMacro void

" interface specifiers
syn keyword ceKeyword UP DOWN PEER MGMT RES1 RES2
syn keyword ceKeyword RES3 RES4 RES5 RES6 RES7 RES8
syn keyword ceKeyword RES8 RES9 RES10 RES11 RES12 RES13
syn keyword ceKeyword RES14 RES15 RES16 RES17 RES18 RES19 RES20

" special reserved words
syn keyword ceKeyword all

" functions
syn keyword ceFunction start stop to encode decode combine combineFrame
syn keyword ceFunction delete deleteFrame moveAddress copyAddress delay seed 
syn keyword ceFunction copy copyFrame split splitFrame rand gamma

" state 
syn keyword ceType anystate ANYSTATE global GLOBAL otherstate OTHERSTATE
" state input
syn keyword ceType anyinput ANYINPUT other OTHER otherinput OTHERINPUT

" numbers
syn match ceConstant /[[:digit:]]/

syn keyword ceRepeat for while do
syn keyword ceConditional if else

if !exists("did_cvopsefsa_inits")
	let did_cvopsefsa_inits = 1
	hi link ceMacro			Macro	
	hi link ceKeyword		Keyword
	hi link ceComment		Comment
	hi link ceFunction		Function
	hi link ceType			Type
	hi link ceConditional	Conditional
	hi link ceRepeat		Repeat
	hi link ceConstant		Constant
endif

let b:current_syntax = "cvopsefsa"

" vim: ts=4
