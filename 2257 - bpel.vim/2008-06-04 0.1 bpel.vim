" Vim syntax file
" Language:	BPEL, the Business Process Execution Language (xml)
" Maintainer:	Josef Spillner <spillner@rn.inf.tu-dresden.de>
" Last Change:	Mi 4. Jun 17:22:16 CEST 2008
" Filenames:	*.bpel

if exists("b:current_syntax")
    finish
endif

let s:bpel_cpo_save = &cpo
set cpo&vim

runtime! syntax/xml.vim

syn case ignore

syn cluster xmlTagHook add=bpelActivity,bpelSyntax

syn keyword bpelActivity display invoke receive reply assign throw wait empty
syn keyword bpelActivity display sequence while pick flow scope compensate switch link

syn keyword bpelActivity display copy from to case otherwise for until condition
syn keyword bpelActivity display links source fault target catch

syn keyword bpelSyntax display process variable variables partnerLink partnerLinks correlationSets
syn keyword bpelSyntax display compensationHandlers eventHandlers faultHandlers

hi def link bpelActivity Statement
hi def link bpelSyntax Macro

let b:current_syntax = "bpel"

let &cpo = s:bpel_cpo_save
unlet s:bpel_cpo_save

" vim: ts=8
