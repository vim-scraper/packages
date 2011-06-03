" Vim color file
" Maintainer:	Thomas Schmall <ts_nowhere@yahoo.com>
" Last Change:	2004 Jun 14

" This color scheme uses a light grey background.

" Last Changes:
" *included more color definitions (operator (bold), identifier, error, Special and Todo)
" *changed identifier and type colors
" Todo: 

" First remove all existing highlighting.
set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "simpleandfriendly"

"Set nice colors #DC6210
"Cursor is Cyan when ":lmap" mappings are active
hi lCursor guibg=Cyan guifg=NONE

"Text below the last line is darker grey
hi NonText 	guibg=grey80
"Normal text is black background is grey
hi Normal 	guifg=black		guibg=grey89  ctermfg=Black  ctermbg=LightGrey 
hi Comment	guifg=Orange	guibg=grey94	ctermfg=DarkCyan	term=bold
"Constants are not underlined but have a slightly lighter background
hi Constant	guifg=#80a0ff 	gui=NONE guibg=grey92	term=underline	
"Words like _if_ or _else_  (Grey27)
hi Statement	guifg=#4A2B99	gui=NONE	ctermfg=Blue

"hi Identifier	term=underline	cterm=bold ctermfg=Cyan	guifg=#40ffff
"color for _NONE_ for instance:
hi PreProc	term=underline	ctermfg=LightBlue	guifg=#DC6210
"color for _guifg_ for instance: (SlateBlue works here nice too)
hi Type	  	guifg=#008080 ctermfg=LightGreen gui=None term=underline	
hi Function	guifg=#61577A	term=bold 	
"in lingo the defined functions
hi Identifier	guifg=Seagreen 

"hi Number	 ctermfg=Gray guifg=#808080
"hi Repeat	term=underline	ctermfg=White		guifg=white
"hi Ignore	guifg=bg	ctermfg=black
hi Error  	term=reverse 	ctermbg=Red ctermfg=White guibg=Red guifg=White
hi Todo	  	term=standout	 ctermbg=Yellow ctermfg=Black guifg=Blue guibg=Yellow
"Special Characters
hi Special 	guibg=grey90	guifg=Slateblue	gui=UNDERLINE

hi operator guifg=gray25 ctermfg=Black term=bold cterm=bold gui=bold
