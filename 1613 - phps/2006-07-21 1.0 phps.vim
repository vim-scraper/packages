" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
" Vim color file
" Maintainer:	Misha Strijov <MishaStrij@Mail.Ru>
" Last Change:	2003 May 02

hi clear
set background=dark
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "phps"

hi Normal		guibg=White		guifg=#0000BB
hi Comment		guifg=#FF8000
hi Constant		guifg=#DD0000	gui=NONE
hi Identifier	guifg=#0000BB
hi Ignore		guifg=bg
hi PreProc		guifg=#007700
hi Search		guifg=Black		guibg=Blue
hi Special		guifg=#007700
hi Statement	guifg=#007700	gui=NONE
hi Type			guifg=#007700	gui=none
hi Error		guibg=Red  		guifg=White
hi Todo			guifg=Blue 		guibg=Yellow
" From the source:
hi Cursor										  guifg=Orchid	guibg=fg
hi Directory	term=bold	   ctermfg=LightCyan  guifg=Cyan
hi ErrorMsg		term=standout  ctermbg=DarkRed	  ctermfg=White guibg=Red guifg=White
hi IncSearch	term=reverse   cterm=reverse	  gui=reverse
hi LineNr		term=underline ctermfg=Yellow					guifg=Yellow
hi ModeMsg		term=bold	   cterm=bold		  gui=bold
hi MoreMsg		term=bold	   ctermfg=LightGreen gui=bold		guifg=SeaGreen
hi NonText		term=bold	   ctermfg=Blue		  gui=bold		guifg=Blue
hi Question		term=standout  ctermfg=LightGreen gui=bold		guifg=Cyan
hi SpecialKey	term=bold	   ctermfg=LightBlue  guifg=Cyan
hi StatusLine	term=reverse,bold cterm=reverse   gui=NONE		guifg=White guibg=darkblue
hi StatusLineNC term=reverse   cterm=reverse	  gui=NONE		guifg=white guibg=#333333
hi Title		term=bold	   ctermfg=LightMagenta gui=bold	guifg=Pink
hi WarningMsg	term=standout  ctermfg=LightRed   guifg=Red
hi Visual		term=reverse   cterm=reverse	  gui=NONE		guifg=white guibg=darkgreen
