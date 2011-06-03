" ----------------------------------------------------------------------------
" Filename:	 tabula.vim
" Last Modified: 2006-11-26
" Version:       0.4
" Maintainer:	 Bernd Pol (bernd.pol AT online DOT de)
" Copyright:	 2006 Bernd Pol
"                This script is free software; you can redistribute it and/or 
"                modify it under the terms of the GNU General Public License as 
"                published by the Free Software Foundation; either version 2 of 
"                the License, or (at your option) any later version. 
" Description:   Vim colorscheme based on marklar.vim by SM Smithfield,
" 		 slightly modified for harmonic, yet easily distinguishable
" 		 display on GUI and a 256 color xterm as well.
" Install:       Put this file in the users colors directory (~/.vim/colors)
"                then load it with :colorscheme tabula
" -----------------------------------------------------------------------------

hi clear
set background=dark
if exists("syntax_on")
    syntax reset
endif
let g:colors_name = "tabula"

"------------------------------------------------------------------------------
"			       Option Settings
"------------------------------------------------------------------------------
"
" Use these in your .vimrc file before the ':colorscheme tabula' line
"
" Alternatively set the options variable from the command line, e.g.
" 	:let Tabula_LNumUnderline=1
" and then call
" 	:colorscheme tabula
" again.

"------------------------------------------------------------------------------
" Display Statements In Bold:
" 	Tabula_BoldStatement = 0	statements display not bold
" 	Tabula_BoldStatement = 1	statements display bold
" Defaults to non-bold display.
"------------------------------------------------------------------------------
"
let s:BoldStatement = 0
if exists("g:Tabula_BoldStatement")
  let s:BoldStatement = g:Tabula_BoldStatement
endif

"------------------------------------------------------------------------------
" Set GUI Cursor Color:
"	Tabula_CurColor = 'blue'
"	Tabula_CurColor = 'red'
"	Tabula_CurColor = 'yellow'
"	Tabula_CurColor = 'white'
" Defaults to 'blue'.
"------------------------------------------------------------------------------
"
let s:CurColor = "blue"
if exists("g:Tabula_CurColor")
  let s:CurColor = g:Tabula_CurColor
endif

"------------------------------------------------------------------------------
" Set Color For Preprocessor Statements:
"	Tabula_ColorPre = 'blue'	purple-blue
"	Tabula_ColorPre = 'red'		orange-red
"	Tabula_ColorPre = 'yellow'	lightgreen-yellow
" Defaults to 'blue'.
"------------------------------------------------------------------------------
"
let s:ColorPre = "blue"
if exists("g:Tabula_ColorPre")
  if g:Tabula_ColorPre == "red" || g:Tabula_ColorPre == "yellow"
    let s:ColorPre = g:Tabula_ColorPre
  endif
endif

"------------------------------------------------------------------------------
" How To Display Ignore And NonText Characters:
" 	Tabula_InvisibleIgnore = 0	slightly visible
"	Tabula_InvisibleIgnore = 1	invisible on standard background
" Defaults to invisible display.
"------------------------------------------------------------------------------
"
let s:InvisibleIgnore = 1
if exists("g:Tabula_InvisibleIgnore")
  let s:InvisibleIgnore = g:Tabula_InvisibleIgnore
endif

"-----------------------------------------------------------------------------
" Show Line Numbers Underlined:
" Sometimes useful to spot a line more easily.
" 	Tabula_LNumUnderline = 0	not underlined
"	Tabula_LNumUnderline = 1	line numbers are underlined
" Defaults to not underlined.
"------------------------------------------------------------------------------
"
let s:LNumUnderline = 0
if exists("g:Tabula_LNumUnderline")
  let s:LNumUnderline = g:Tabula_LNumUnderline
endif

"------------------------------------------------------------------------------
" How To Display TODOs And Similar:
"	Tabula_TodoUnderline = 0	display on a blue background
"	Tabula_TodoUnderline = 1	display them underlined white
" Defaults to underlined display.
"------------------------------------------------------------------------------
"
let s:TodoUnderline=1
if exists("g:Tabula_TodoUnderline")
  let s:TodoUnderline = g:Tabula_TodoUnderline
endif

"------------------------------------------------------------------------------

if version >= 700
    hi SpellBad        	guisp=#FF0000
    hi SpellCap        	guisp=#0000FF
    hi SpellRare       	guisp=#ff4046
    hi SpellLocal     	guisp=#000000							ctermbg=0
    hi Pmenu		guifg=#00ffff	guibg=#000000			ctermfg=51	ctermbg=0
    hi PmenuSel       	guifg=#ffff00	guibg=#000000	gui=bold	ctermfg=226			cterm=bold
    hi PmenuSbar       			guibg=#204d40					ctermbg=6
    hi PmenuThumb      	guifg=#38ff56					ctermfg=3
    hi CursorColumn		    	guibg=#096354					ctermbg=29
    hi CursorLine		      	guibg=#096354					ctermbg=29
    hi Tabline         	guifg=bg	guibg=fg	gui=NONE	ctermfg=NONE	ctermbg=NONE	cterm=reverse,bold
    hi TablineSel      	guifg=#20012e	guibg=#00a675	gui=bold
    hi TablineFill     	guifg=#689C7C
    hi MatchParen      	guifg=#38ff56	guibg=#0000ff	gui=bold	ctermfg=14	ctermbg=21	cterm=bold
endif

hi Comment		guifg=#00BBBB	guibg=NONE			ctermfg=6 	cterm=none
hi Constant		guifg=#D0D0D0	guibg=NONE		 	ctermfg=254

if s:CurColor == "yellow"
  hi Cursor		guifg=#000000	guibg=#EFEF00
elseif s:CurColor == "red"
  " Note: Input cursor will be invisible on Error Group
  hi Cursor		guifg=#00007F	guibg=#F70000
elseif s:CurColor == "blue"
  hi Cursor		guifg=#00007F	guibg=#00EFEF
elseif s:CurColor == "white"
  hi Cursor		guifg=#000000	guibg=#FFFFFF
endif

hi DiffAdd		guifg=NONE	guibg=#136769 			ctermfg=4	ctermbg=7	cterm=none
hi DiffDelete		guifg=NONE	guibg=#50694A 			ctermfg=1 	ctermbg=7	cterm=none
hi DiffChange		guifg=fg	guibg=#00463c	gui=None	ctermfg=4 	ctermbg=2	cterm=none
hi DiffText		guifg=#7CFC94	guibg=#00463c	gui=bold 	ctermfg=4 	ctermbg=3	cterm=none
hi Directory		guifg=#25B9F8	guibg=NONE							ctermfg=2

if s:CurColor == "red"
  " Note: We need another background in this case to keep the cursor visible.
  hi Error		guifg=#FF0000	guibg=#FFFF00	gui=bold	ctermfg=11 	ctermbg=9	cterm=NONE
else
  hi Error		guifg=#FFFF00	guibg=#FF0000	gui=NONE	ctermfg=11 	ctermbg=9	cterm=NONE
endif

hi ErrorMsg		guifg=#8eff2e	guibg=#204d40
hi FoldColumn		guifg=#00BBBB	guibg=#4E4E4E			ctermfg=14 	ctermbg=240
hi Folded		guifg=#44DDDD	guibg=#4E4E4E			ctermfg=14 	ctermbg=240
hi Identifier		guifg=#FFAF00					ctermfg=214			cterm=none

" Ignore Group Variants:
"
if s:InvisibleIgnore
  " completely invisible
  hi Ignore		guifg=bg	guibg=NONE			ctermfg=23
  hi NonText		guifg=bg	guibg=NONE			ctermfg=23
else
  " nearly invisible
  hi Ignore		guifg=#005FAF	guibg=NONE			ctermfg=26
  hi NonText		guifg=#005FAF	guibg=NONE			ctermfg=26
endif
 
hi IncSearch				guibg=#52891f	gui=bold

" Line Number Variants:
" Lines can sometimes be more precisely identified if the line numbers are
" underlined.
if s:LNumUnderline
  hi LineNr		guifg=#00FF00	guibg=#005080	gui=underline	ctermfg=84	ctermbg=24	cterm=underline
else
  hi LineNr		guifg=#00FF00	guibg=#005080			ctermfg=84	ctermbg=24
endif

hi ModeMsg		guifg=#FFFFFF	guibg=#0000FF			ctermfg=7	ctermbg=4	cterm=bold
hi MoreMsg		guifg=#FFFFFF	guibg=#00A261			ctermfg=7	ctermbg=2	cterm=bold
hi Normal		guifg=#71C293	guibg=#06544a			ctermfg=84	ctermbg=23 

" Preprocessor Variants:
if s:ColorPre == "red"
  hi PreProc		guifg=#FF5F5F	guibg=bg			ctermfg=203
elseif s:ColorPre == "yellow"
  hi PreProc		guifg=#AFFF00	guibg=bg			ctermfg=154
elseif s:ColorPre == "blue"
  hi PreProc		guifg=#8787AF	guibg=bg			ctermfg=104
endif

hi Question		guifg=#FFFFFF	guibg=#00A261			ctermfg=15	ctermbg=35
hi Search		guifg=NONE	guibg=#0B7260			ctermfg=NONE	ctermbg=36
hi SignColumn		guifg=#00BBBB	guibg=#204d40
hi Special		guifg=#00FFFF	guibg=NONE	gui=none	ctermfg=51
hi SpecialKey		guifg=#00FFFF	guibg=#266955

" Statement Variants:
if s:BoldStatement
  hi Statement		guifg=#D7FF00			gui=bold	ctermfg=11			cterm=bold
else
  hi Statement		guifg=#D7FF00			gui=none	ctermfg=11
endif

hi StatusLine		guifg=#245748	guibg=#71C293	gui=none					cterm=reverse
hi StatusLineNC		guifg=#245748	guibg=#689C7C	gui=none
hi Title		guifg=#7CFC94	guibg=NONE	gui=none	ctermfg=2			cterm=bold

" Todo Variants:
if s:TodoUnderline
  " Underlined
  hi Todo		guifg=#E4E4E4	guibg=NONE	gui=underline	ctermfg=255	ctermbg=NONE	cterm=underline
else
  " Blue background
  hi Todo		guifg=#00FFFF	guibg=#0000FF			ctermfg=51	ctermbg=4
endif

hi Type			guifg=#FF80FF	guibg=bg	gui=none	ctermfg=212
"hi Underlined		guifg=#df820c	guibg=NONE	gui=underline	ctermfg=8			cterm=underline
hi Underlined						gui=underline					cterm=underline
hi Visual 				guibg=#0B7260	gui=none
hi WarningMsg		guifg=#FFFFFF	guibg=#FF0000			ctermfg=7	ctermbg=1	cterm=bold
hi WildMenu		guifg=#20012e	guibg=#00a675	gui=bold	ctermfg=none	ctermbg=none	cterm=bold
"
hi pythonPreCondit							ctermfg=2			cterm=none
hi tkWidget		guifg=#D5B11C	guibg=bg	gui=bold	ctermfg=7			cterm=bold
hi tclBookends		guifg=#7CFC94	guibg=NONE	gui=bold	ctermfg=2			cterm=bold

" ------------------------------------------------------------------------------------------------
" Custom HTML groups
" (see ':help html.vim' for more info)

let html_my_rendering=1

hi htmlBold		guifg=#87FFD7			gui=bold	ctermfg=122			cterm=bold
hi htmlBoldItalic	guifg=#87D7EF			gui=bold	ctermfg=117			cterm=bold
hi htmlBoldUnderline	guifg=#87FFD7			gui=bold,underline ctermfg=122			cterm=bold,underline
hi htmlBoldUnderlineItalic guifg=#87D7EF		gui=bold,underline ctermfg=117			cterm=bold,underline
hi htmlH1		guifg=#00FF00	guibg=NONE	gui=bold,underline ctermfg=2			cterm=bold,underline
hi htmlH2		guifg=#00FF00	guibg=NONE	gui=bold	ctermfg=2			cterm=bold
hi htmlH3		guifg=#00FF00	guibg=NONE	gui=NONE	ctermfg=2
hi htmlH4		guifg=#00C700	guibg=NONE	gui=underline	ctermfg=34			cterm=underline
hi htmlH5		guifg=#00C700	guibg=NONE	gui=NONE	ctermfg=34
hi htmlH6		guifg=#00A700	guibg=NONE	gui=underline	ctermfg=28			cterm=underline
hi htmlItalic		guifg=#87D7D7			gui=NONE	ctermfg=116
hi htmlLink		guifg=#8787D7			gui=underline   ctermfg=105			cterm=underline
hi htmlUnderline                			gui=underline					cterm=underline
hi htmlUnderlineItalic	guifg=#87D7D7			gui=underline	ctermfg=116			cterm=underline

