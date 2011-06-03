" Vim color file
" Maintainer:	Ruda Moura <ruda.moura@gmail.com>
" Last Change:	$Date: 2005/11/06 12:59:38 $

let g:colors_name = "zen"

set background=light
highlight clear
  if exists("syntax on")
syntax reset
endif

highlight Normal     term=none ctermfg=black   cterm=none guifg=black   gui=none
highlight Comment    term=none ctermfg=cyan    cterm=none guifg=cyan    gui=none
highlight Constant   term=none ctermfg=red     cterm=none guifg=red     gui=none
highlight Special    term=none ctermfg=red     cterm=bold guifg=red     gui=bold
highlight Identifier term=none ctermfg=black   cterm=none guifg=black   gui=none
highlight Statement  term=bold ctermfg=black   cterm=bold guifg=black   gui=bold
highlight Operator   term=bold ctermfg=black   cterm=bold guifg=black   gui=bold
highlight PreProc    term=bold ctermfg=green   cterm=none guifg=green   gui=none
highlight Type       term=bold ctermfg=magenta cterm=none guifg=magenta gui=none
highlight String     term=none ctermfg=red     cterm=none guifg=red     gui=none
highlight Number     term=none ctermfg=red     cterm=none guifg=red     gui=none
