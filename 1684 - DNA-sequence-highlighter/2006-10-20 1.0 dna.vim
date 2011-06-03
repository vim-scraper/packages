" Vim syntax file
" Language:	DNA
" Maintainer:	Alexey Shipunov, plantago@herba.msu.ru
" Last Change:	February 13, 2003

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

set nowrap
set go=gmrLtTb

" DNA tokens
syn match dnaA	"[Aa]\+"
syn match dnaT	"[Tt]\+"
syn match dnaG	"[Gg]\+"
syn match dnaC	"[Cc]\+"

" Comments:
syn match dnaComment	"\[.\{-}\]"

" synchronizing
syn sync maxlines=50

" Define the default highlighting
highlight dnaA guifg=black guibg=yellow
highlight dnaT guifg=black guibg=magenta
highlight dnaC guifg=black guibg=red
highlight dnaG guifg=black guibg=lightgreen
highlight dnaComment guifg=darkgray guibg=white

let b:current_syntax = "dna"
" vim: ts=8
