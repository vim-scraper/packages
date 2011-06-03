" Vim color file
" Maintainer:	Gergely Kontra <kgergely@mcl.hu>
" Last Change:	2002. 04. 09.
" Based on scite colors.
" I've changed my mind, and inverted the colors.
" After an hour, this is the result.
" Not resembles to scite's colors :-]
" I'm a bit red-blind, so if you have suggestions, don't hesitate :)
"           ^^^^^^^^^ Sorry, I cannot speak English well, just want to say,
"           that in some rare cases I cannot distinguish between some colors
"           (I've just realized it, when I see some special tests)

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
"colorscheme default
let g:colors_name = "cool"


" GUI
highlight Normal	guifg=#80A0C0	guibg=#223344
highlight Search	guifg=#ffffff	guibg=#335577
highlight IncSearch	guifg=#000000	guibg=#ffff00	gui=NONE
highlight Visual	guifg=#112233	guibg=#6688AA
highlight Folded	guifg=#000000	guibg=#999999
highlight Cursor	guifg=#999999	guibg=#FFFFFF
highlight Special	guifg=#80FF80	guibg=#333300	gui=bold
highlight Comment	guifg=#808080	guibg=#444444
highlight StatusLine	guifg=#FFFFFF	guibg=#553333
highlight StatusLineNC	guifg=#AA8888	guibg=#000000
highlight Statement	guifg=#FF8080			gui=bold
highlight Type		guifg=#FFFFff   gui=NONE
highlight Function	guifg=#FF8080	gui=bold
highlight LineNr	guifg=#FFFFFF	guibg=#444444
highlight FoldColumn	guifg=#FFFFFF	guibg=#222222
highlight Define	guifg=#FFFF80	guibg=#000099 gui=bold
highlight Number	guifg=#FFFFFF	guibg=#302030
highlight Subtitle	guifg=#FFFFFF	guibg=#994444 gui=bold,underline
highlight String	guifg=#80FF80	guibg=#004444
highlight Delimiter	guifg=#FFFFFF	guibg=#221F22	gui=bold
highlight PreProc	guifg=#ffff00	guibg=#000000	gui=bold

