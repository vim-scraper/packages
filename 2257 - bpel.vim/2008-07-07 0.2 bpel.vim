" Vim syntax file
" Language:	BPEL, the Business Process Execution Language (xml)
" Maintainer:	Josef Spillner <spillner@rn.inf.tu-dresden.de>
" Last Change:	Mo 7. Jul 09:50:07 CEST 2008
" Filenames:	*.bpel

if exists("b:current_syntax")
    finish
endif

let s:bpel_cpo_save = &cpo
set cpo&vim

runtime! syntax/xml.vim

syn case ignore

syn cluster xmlTagHook add=bpelActivity,bpelSyntax,bpel20Syntax,bpel20Activity

syn keyword bpelActivity display invoke receive reply assign throw wait empty terminate
syn keyword bpelActivity display sequence while pick flow scope compensate switch link

syn keyword bpelActivity display copy from to case otherwise for until condition
syn keyword bpelActivity display links source fault target catch

syn keyword bpelSyntax display process variable variables partnerLink partnerLinks correlationSets
syn keyword bpelSyntax display compensationHandlers eventHandlers faultHandlers

syn keyword bpel20Syntax display import

syn keyword bpel20Activity display if else forEach startCounterValue finalCounterValue exit query literal

hi def link bpelActivity Statement
hi def link bpelSyntax Macro
hi def link bpel20Activity Statement
hi def link bpel20Syntax Macro

let b:current_syntax = "bpel"

let &cpo = s:bpel_cpo_save
unlet s:bpel_cpo_save

" vim: ts=8
