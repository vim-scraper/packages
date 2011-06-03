" Vim syntax file
" Language:     byter
" Maintainer:   Zasenko Sergey
" Last Change:  2008 Jan 29

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'brainfuck'
endif

syn match bytMove   "[<>VA]"
syn match bytClear  "[0]"
syn match bytIO     "[{}+-]"
syn match bytTerm   "[#]"

if version >= 508 || !exists("did_byter_syn_inits")
  if version < 508
    let did_byter_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink bytMove   Identifier
  HiLink bytClear  Comment
  HiLink bytTerm   Special
  HiLink bytIO     Operator

  delcommand HiLink
endif

let b:current_syntax = "byter"

if main_syntax == 'byter'
  unlet main_syntax
endif

" vim: ts=8
