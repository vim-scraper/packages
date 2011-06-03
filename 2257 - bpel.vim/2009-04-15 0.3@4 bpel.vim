" Vim syntax file
" Language:	BPEL, the Business Process Execution Language (xml)
" Maintainer:	Josef Spillner <spillner@rn.inf.tu-dresden.de>
" Last Change:	Mi 15. Apr 10:27:59 CEST 2009
" Filenames:	*.bpel

if exists("b:current_syntax")
    finish
endif

let s:bpel_cpo_save = &cpo
set cpo&vim

runtime! syntax/xml.vim

syn case ignore

syn cluster xmlTagHook add=bpelActivity,bpelSyntax,bpel20Syntax,bpel20Activity,bpelExtension

syn keyword bpelActivity display invoke receive reply assign throw wait empty terminate
syn keyword bpelActivity display sequence while pick flow scope compensate switch link

syn keyword bpelActivity display copy from to case otherwise for until condition
syn keyword bpelActivity display links source sources fault catch

syn keyword bpelSyntax display process variable variables partnerLink partnerLinks
syn keyword bpelSyntax correlationSets correlationSet correlations correlation
syn keyword bpelSyntax display compensationHandlers eventHandlers faultHandlers compensationHandler
syn keyword bpelSyntax targets target joinCondition

syn keyword bpel20Syntax display import
syn keyword bpel20Syntax onMessage onAlarm completionCondition branches

syn keyword bpel20Activity display if then else elseif repeatUntil forEach startCounterValue finalCounterValue exit query literal compensateScope catchAll

syn keyword bpelExtension extensions extension extensionActivity extensionAssignOperation

hi def link bpelActivity Statement
hi def link bpelSyntax Macro
hi def link bpel20Activity Statement
hi def link bpel20Syntax Macro
hi def link bpelExtension Special

let b:current_syntax = "bpel"

let &cpo = s:bpel_cpo_save
unlet s:bpel_cpo_save

" vim: ts=8
