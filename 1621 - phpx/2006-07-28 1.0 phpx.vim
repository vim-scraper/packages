" Vim color file
"Version 1.0
" Last Change:	2006 July 28
"Look more like php.net and and work on xterm.
hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "phpx"

if exists("&t_Co") && &t_Co > 2 && &t_Co <= 8
    "Not much on the linux (text)console, unless anybody would document.
    hi Normal          ctermfg=4 ctermbg=7
    hi Comment         ctermfg=3
    hi Constant        ctermfg=1
    hi Special         ctermfg=3
    hi Identifier      ctermfg=4
    hi Statement       ctermfg=0
    hi PreProc         ctermfg=2
    hi Type            ctermfg=2
    finish
endif

hi Normal          ctermfg=19 ctermbg=254 guifg=#0000af guibg=#e4e4e4
hi Cursor          guifg=#ffffff guibg=#0000af
hi lCursor         guifg=#d7ffd7 guibg=#0000af
hi Comment         term=bold ctermfg=208 guifg=#ff8700
hi Constant        term=underline ctermfg=160 guifg=#d70000
hi Special         term=bold ctermfg=166 guifg=#d75f00
hi Identifier      term=underline cterm=NONE ctermfg=19 guifg=#0000af
hi Statement       term=bold ctermfg=28 cterm=NONE guifg=#008700 gui=NONE
hi PreProc         term=underline cterm=bold ctermfg=34 gui=bold guifg=#00af00
hi Type            term=underline ctermfg=34 guifg=#00af00
hi Underlined      term=underline cterm=underline ctermfg=9 gui=underline guifg=#80a0ff
hi Ignore          cterm=bold ctermfg=0 guifg=bg
hi Error           term=reverse cterm=bold ctermfg=15 ctermbg=12 guifg=White guibg=Red
hi Todo            term=standout ctermfg=0 ctermbg=14 guifg=Blue guibg=Yellow
hi Pmenu           ctermbg=13 guibg=Magenta
hi PmenuSel        ctermbg=8 guibg=DarkGrey
hi PmenuSbar       ctermbg=7 guibg=Grey
hi PmenuThumb      cterm=reverse gui=reverse
hi TabLine         term=underline cterm=underline ctermfg=15 ctermbg=8 gui=underline guibg=DarkGrey
hi TabLineSel      term=bold cterm=bold gui=bold
hi TabLineFill     term=reverse cterm=reverse gui=reverse
hi MatchParen      term=reverse ctermfg=3 ctermbg=229 guifg=#cecb00 guibg=#ffffaf 
hi SpecialKey      term=bold ctermfg=9 guifg=Cyan
hi NonText         term=bold ctermfg=9 gui=bold guifg=Blue
hi Directory       term=bold ctermfg=11 guifg=Cyan
hi ErrorMsg        term=standout cterm=bold ctermfg=15 ctermbg=4 guifg=White guibg=Red
hi IncSearch       term=reverse cterm=reverse ctermfg=11 ctermbg=10 gui=reverse guifg=slategrey guibg=khaki
hi Search          term=reverse ctermfg=18 ctermbg=122 guifg=#000087 guibg=#87ffd7
hi MoreMsg         term=bold ctermfg=10 gui=bold guifg=SeaGreen
hi ModeMsg         term=bold cterm=bold ctermfg=130 gui=bold guifg=goldenrod
hi LineNr          term=underline ctermfg=186 guifg=#d7d787
hi Question        term=standout ctermfg=10 gui=bold guifg=Cyan
hi StatusLine      term=bold,reverse cterm=bold,reverse ctermfg=18 ctermbg=15 gui=bold,reverse guifg=#000087 guibg=#ffffff
hi StatusLineNC    term=bold,reverse cterm=bold,reverse ctermfg=246 ctermbg=15 gui=bold,reverse guifg=#949494 guibg=#ffffff
hi VertSplit       term=reverse cterm=reverse gui=reverse guifg=grey50 guibg=#c2bfa5
hi Title           term=bold ctermfg=163 gui=bold guifg=#d700af
hi Visual          term=reverse cterm=reverse ctermfg=229 ctermbg=22 gui=reverse guifg=#ffffaf guibg=#005f00
hi VisualNOS       term=bold,underline cterm=bold,underline gui=bold,underline
hi WarningMsg      term=standout ctermfg=12 guifg=Red
hi WildMenu        term=standout ctermfg=0 ctermbg=14 guifg=Black guibg=Yellow
hi Folded          term=standout ctermfg=11 ctermbg=8 guifg=Cyan guibg=DarkGrey
hi FoldColumn      term=standout ctermfg=11 ctermbg=8 guifg=Cyan guibg=Grey
hi DiffAdd         term=bold ctermbg=1 guibg=DarkBlue
hi DiffChange      term=bold ctermbg=5 guibg=DarkMagenta
hi DiffDelete      term=bold cterm=bold ctermfg=9 ctermbg=3 gui=bold guifg=Blue guibg=DarkCyan
hi DiffText        term=reverse cterm=bold ctermbg=12 gui=bold guibg=Red
hi SignColumn      term=standout ctermfg=11 ctermbg=8 guifg=Cyan guibg=Grey


