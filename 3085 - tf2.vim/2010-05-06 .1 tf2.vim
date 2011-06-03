" Vim syntax file
" Language:         TF2 configuration file
" Maintainer:       Sumant Manne
" Latest Revision:  2010-5-5
" Credits:          Based on the original quake.vim by Nikolai Weibull

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

setlocal iskeyword+=-,+

syn keyword tf2Todo         contained TODO FIXME XXX NOTE

syn region  tf2Comment      display oneline start='//' end='$' end=';'
                              \ keepend contains=tf2Todo,@Spell

syn region  tf2String       display oneline start=+"+ skip=+\\\\\|\\"+
                              \ end=+"\|$+ contains=tf2Numbers,
                              \ @tf2Commands

syn case ignore

syn match tf2Numbers        display transparent '\<-\=\d\|\.\d'
                              \ contains=tf2Number,tf2Float
syn match tf2Number         contained display '\d\+\>'
syn match tf2Float          contained display '\d\+\.\d*'
syn match tf2Float          contained display '\.\d\+\>'

syn cluster tf2Commands		contains=tf2ToggleCommand,tf2Command

syn keyword tf2ToggleCommand	+attack -attack +attack2 -attack2
syn keyword tf2ToggleCommand	+left -left +right -right
syn keyword tf2ToggleCommand	+forward -forward +back -back
syn keyword tf2Command			cl_showfps cl_cmdrate cl_drawhud
syn keyword tf2Command			alias bind cc_emit closecaption exec
syn keyword tf2Command			jpeg_quality play zoom_sensitivity_ratio

hi def link tf2Comment      Comment
hi def link tf2Todo         Todo
hi def link tf2String       String
hi def link tf2Number       Number
hi def link tf2Float        Number
hi def link tf2Command      tf2Commands
hi def link tf2Commands     Keyword

let b:current_syntax = "tf2"

let &cpo = s:cpo_save
unlet s:cpo_save
