" Vim syntax file
" Language:	HP-41
" Version:	0.4
" Maintainer:	Geir Isene
" Last Change:	2005-09-17
" Filenames:    *.41
" URL:		http://www.geir.isene.com/

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax match  hp41LineNumber	"^[0-9]*"
syntax match  hp41LBL		".\=LBL [0-9]*"
syntax match  hp41GTO		"GTO.*"
syntax match  hp41XEQ		"XEQ.*"
syntax match  hp41RTN		"RTN.*"
syntax match  hp41Alpha		"\".*\""

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_crontab_syn_inits")
  if version < 508
    let did_crontab_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink hp41LineNumber		Number
  HiLink hp41LBL		Label
  HiLink hp41GTO		Function
  HiLink hp41XEQ		Function
  HiLink hp41RTN		Structure
  HiLink hp41Alpha		Comment

  delcommand HiLink
endif

let b:current_syntax = "hp41"

" vim: ts=8
