" Vim syntax file
" Language:	python coverage
" Maintainer: Dan Buch <dbuch@ag.com>

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

if has("syntax")
  highlight CoverCovered ctermfg=Green guifg=Green
  highlight CoverNotCovered ctermbg=LightRed ctermfg=White guibg=LightRed guifg=White
  highlight CoverIgnored ctermfg=Gray guifg=Gray
endif


syn match coverIsCovered "^+.*"
syn match coverNotCovered "^-.*"
syn match coverIgnored "^0.*"


if version >= 508 || !exists("did_cover_syn_inits")
  if version < 508
    let did_cover_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink coverIsCovered CoverCovered
  HiLink coverNotCovered CoverNotCovered
  HiLink coverIgnored CoverIgnored

  delcommand HiLink
endif

let b:current_syntax = "cover"
