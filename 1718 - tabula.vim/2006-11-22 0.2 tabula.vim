" ----------------------------------------------------------------------------
" Filename:	 tabula.vim
" Last Modified: 2006-11-23
" Version:       0.2
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
" How No Display TODOs And Similar:
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
hi Cursor		guifg=NONE	guibg=#FF0000
hi DiffAdd		guifg=NONE	guibg=#136769 			ctermfg=4	ctermbg=7	cterm=none
hi DiffDelete		guifg=NONE	guibg=#50694A 			ctermfg=1 	ctermbg=7	cterm=none
hi DiffChange		guifg=fg	guibg=#00463c	gui=None	ctermfg=4 	ctermbg=2	cterm=none
hi DiffText		guifg=#7CFC94	guibg=#00463c	gui=bold 	ctermfg=4 	ctermbg=3	cterm=none
hi Directory		guifg=#25B9F8	guibg=NONE							ctermfg=2
hi Error		guifg=#FFFFFF	guibg=#000000			ctermfg=7 	ctermbg=0	cterm=bold
hi ErrorMsg		guifg=#8eff2e	guibg=#204d40
hi FoldColumn		guifg=#00BBBB	guibg=#4E4E4E			ctermfg=14 	ctermbg=240
hi Folded		guifg=#44DDDD	guibg=#4E4E4E			ctermfg=14 	ctermbg=240
hi Identifier		guifg=#FFAF00	ctermfg=214			cterm=none

" Ignore Highlight Group Variants:
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

hi Type			guifg=#FF80FF	guibg=bg	gui=none	ctermfg=2
hi Underlined		guifg=#df820c	guibg=NONE	gui=underline	ctermfg=8			cterm=underline
hi Visual 				guibg=#0B7260	gui=none
hi WarningMsg		guifg=#FFFFFF	guibg=#FF0000			ctermfg=7	ctermbg=1	cterm=bold
hi WildMenu		guifg=#20012e	guibg=#00a675	gui=bold	ctermfg=none	ctermbg=none	cterm=bold
"
hi pythonPreCondit							ctermfg=2	cterm=none
hi tkWidget		guifg=#D5B11C	guibg=bg	gui=bold	ctermfg=7	cterm=bold
hi tclBookends		guifg=#7CFC94	guibg=NONE	gui=bold	ctermfg=2	cterm=bold

" ------------------------------------------------------------------------------------------------
" Common groups that link to other highlight definitions.

highlight link Constant     Character
highlight link Constant     Number
highlight link Constant     Boolean
highlight link Constant     String

highlight link LineNr       Operator

highlight link Number       Float

highlight link PreProc      Define
highlight link PreProc      Include
highlight link PreProc      Macro
highlight link PreProc      PreCondit

highlight link Question     Repeat

highlight link Repeat       Conditional

highlight link Special      Delimiter
highlight link Special      SpecialChar
highlight link Special      SpecialComment
highlight link Special      Tag

highlight link Statement    Exception
highlight link Statement    Keyword
highlight link Statement    Label

highlight link Type         StorageClass
highlight link Type         Structure
highlight link Type         Typedef


