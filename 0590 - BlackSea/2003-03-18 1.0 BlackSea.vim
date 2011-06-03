" Vim color file
" Maintainer:   Gerald S. Williams
" Last Change:  2003 Mar 17

" This is a dark version/opposite of "seashell". The cterm version of this is
" very similar to "evening".
"
" Only values that differ from defaults are specified.

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "BlackSea"

hi Normal guibg=Black guifg=seashell ctermfg=White
hi NonText guifg=LavenderBlush ctermfg=LightMagenta
hi DiffDelete guibg=LightRed guifg=Black ctermbg=DarkRed ctermfg=White
hi DiffAdd guibg=LightGreen ctermbg=DarkGreen ctermfg=White
hi DiffChange guibg=Gray90 ctermbg=DarkCyan ctermfg=White
hi DiffText gui=NONE guibg=LightCyan2 ctermbg=DarkCyan ctermfg=Yellow
hi Comment guifg=LightBlue
hi Constant guifg=DeepPink
hi PreProc guifg=Magenta ctermfg=Magenta
hi StatusLine guibg=White guifg=DarkSeaGreen cterm=NONE ctermfg=White ctermbg=DarkGreen
hi StatusLineNC guifg=Gray
hi VertSplit guifg=Gray
hi Type gui=NONE
hi Identifier guifg=Cyan
hi Statement guifg=brown3 ctermfg=DarkRed
hi Search guibg=Gold3 ctermfg=White
