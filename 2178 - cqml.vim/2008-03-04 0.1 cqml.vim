" Vim syntax file
" Language:     CQML - Component Quality Modelling Language
" Maintainer:   Josef Spillner <spillner@rn.inf.tu-dresden.de>
" URL:          http://rn.inf.tu-dresden.de/
" Last Change:  Mon 2008-03-03 17:32:13 CET
" 
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

syn keyword cqml_keyword domain numeric real integer natural increasing decreasing with set enum order maximum minimum range mean variance standard_deviation percentile moment frequency distribution quality_characteristic

" other than quality_characteristic
syn keyword cqml_extra quality values and if then else endif profile provides uses for invariant invalid implies Undefined composition transition any precedence worth
syn keyword cqml_type Service Boolean Integer Flow
syn keyword cqml_num TRUE FALSE
syn keyword cqml_special SE SR initiate isEmpty time first
syn keyword cqml_comp sequential lhs rhs max min abs self
syn keyword cqml_statement compulsory threshold guaranteed limit

syn match cqml_comp_m "parallel\-or\|parallel\-and"
syn match cqml_statement_m "best\-effort"

syn match cqml_range "\[\d\+\.\.\d\+\]"

hi def link cqml_keyword Statement
hi def link cqml_extra String
hi def link cqml_type Macro
hi def link cqml_num Comment
hi def link cqml_range Comment
hi def link cqml_special Identifier
hi def link cqml_comp Type
hi def link cqml_comp_m Type
hi def      cqml_statement ctermfg=yellow
hi def      cqml_statement_m ctermfg=yellow

let b:current_syntax = "cqml"

" vim:sts=2 sw=2
