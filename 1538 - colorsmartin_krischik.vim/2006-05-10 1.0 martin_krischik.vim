"-------------------------------------------------------------------------------
"  Description: Set backup options to use a directrory
"   Maintainer:	Martin Krischik
"    Copyright: Copyright (C) 2006 Martin Krischik
" Name Of File: colors/martin_krischik.vim
" Last Changed: Monday, 09 May 2006
"      Version: 1.0
"        Usage: copy to plugin directory
"      History: 
"          URL: http://www.vim.org/account/profile.php?user=7818
"	  Note:	Tried and Tested for 'builtin_gui', 'xterm' (KDE Konsole)
"		'vt320'" (OpenVMS) and 'linux' (Linux Console).

" First remove all existing highlighting.

set background=light
highlight clear

if exists ("syntax_on")
    syntax reset
endif

let colors_name = "martin_krischik"

if (&term == "builtin_gui") || 
  \(&term == "xterm")	    ||
  \(&term == "vt320")	    ||
  \(&term == "linux")

    if &term=="vt320"
	set t_Co=8
    else
	set t_Co=16
    endif

    "
    " Normal Text Colors
    " 
    highlight Normal		term=none	    cterm=none		    ctermfg=Black	ctermbg=LightGray	gui=none		guifg=black	    guibg=white  
    highlight Visual		term=reverse	    cterm=reverse	    ctermfg=DarkRed	ctermbg=LightGray	gui=reverse		guifg=firebrick     guibg=white
    highlight VisualNOS		term=bold,underline cterm=bold,underline						gui=reverse		guifg=firebrick     guibg=black 
    highlight StatusLine	term=bold,reverse   cterm=bold,reverse							gui=bold,reverse	guifg=LightBlue2    guibg=black
    highlight VertSplit		term=reverse	    cterm=reverse							gui=reverse		guifg=LightBlue3    guibg=black
    highlight StatusLineNC	term=reverse	    cterm=reverse							gui=reverse		guifg=grey75	    guibg=black 
    highlight ErrorMsg		term=standout				    ctermfg=White	ctermbg=DarkRed					guifg=White	    guibg=Red
    highlight TabLine		term=reverse	    cterm=reverse							gui=reverse		guifg=grey75	    guibg=black 
    highlight TabLineFill       term=reverse	    cterm=reverse							gui=reverse
    highlight TabLineSel	term=bold,reverse   cterm=bold,reverse							gui=bold,reverse	guifg=LightBlue2    guibg=black
    "
    " Syntax Highlight Colors
    " 
    highlight Boolean		term=underline				    ctermfg=DarkRed	ctermbg=LightGray				guifg=DarkOrchid3   guibg=grey95
    highlight Character		term=underline				    ctermfg=DarkRed	ctermbg=LightGray				guifg=RoyalBlue3    guibg=grey95
    highlight Comment		term=bold				    ctermfg=DarkGray	ctermbg=LightGray				guifg=grey30
    highlight Conditional	term=bold				    ctermfg=DarkBlue	ctermbg=LightGray	gui=bold		guifg=DodgerBlue4
    highlight Constant		term=underline				    ctermfg=DarkRed	ctermbg=LightGray				guifg=MediumOrchid3 guibg=grey95    
    highlight Debug		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=maroon	    guibg=GhostWhite
    highlight Define		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=brown4	    guibg=snow	    
    highlight Delimiter		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=DarkOrchid    guibg=GhostWhite 
    highlight DiffAdd		term=bold							ctermbg=LightBlue						    guibg=LightBlue
    highlight DiffChange	term=bold							ctermbg=LightMagenta						    guibg=LightMagenta
    highlight DiffDelete	term=bold				    ctermfg=LightBlue	ctermbg=LightCyan	gui=bold		guifg=Blue	    guibg=LightCyan
    highlight DiffText		term=reverse	    cterm=bold					ctermbg=LightRed	gui=bold				    guibg=Red
    highlight Directory		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray				guifg=Blue
    highlight Error		term=reverse				    ctermfg=White	ctermbg=LightRed	gui=undercurl		guifg=Red	    guibg=MistyRose
    highlight Exception		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray	gui=none		guifg=SlateBlue4
    highlight Float		term=underline				    ctermfg=DarkRed	ctermbg=LightGray				guifg=MediumOrchid4 guibg=grey95
    highlight FoldColumn	term=standout				    ctermfg=DarkBlue	ctermbg=DarkGray				guifg=DarkBlue	    guibg=Grey
    highlight Folded		term=standout				    ctermfg=DarkBlue	ctermbg=DarkGray				guifg=DarkBlue	    guibg=LightGrey
    highlight Function		term=underline				    ctermfg=DarkCyan	ctermbg=LightGray				guifg=SteelBlue
    highlight Identifier	term=underline				    ctermfg=DarkCyan	ctermbg=LightGray				guifg=DarkCyan
    highlight Ignore							    ctermfg=White	ctermbg=grey		guifg=bg
    highlight Include		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=firebrick3    guibg=snow
    highlight IncSearch		term=reverse	    cterm=reverse							gui=reverse
    highlight Keyword		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray	gui=bold		guifg=RoyalBlue4
    highlight Label		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray	gui=none		guifg=SlateBlue3
    highlight LineNr		term=underline				    ctermfg=DarkRed	ctermbg=DarkGray				guifg=Brown	    guibg=grey80
    highlight Macro		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=brown3	    guibg=snow
    highlight ModeMsg		term=bold	    cterm=bold					ctermbg=LightGray	gui=bold
    highlight MoreMsg		term=bold				    ctermfg=DarkGreen	ctermbg=LightGray	gui=bold		guifg=SeaGreen
    highlight NonText		term=bold				    ctermfg=LightBlue	ctermbg=LightGray	gui=bold		guifg=Blue	    guibg=grey80
    highlight Number		term=underline				    ctermfg=DarkRed	ctermbg=LightGray				guifg=DarkOrchid4   guibg=grey95 
    highlight Operator		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray	gui=none		guifg=RoyalBlue3
    highlight PreCondit		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=red	    guibg=snow
    highlight PreProc		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=firebrick4    guibg=snow 
    highlight Question		term=standout				    ctermfg=DarkGreen	ctermbg=LightGray	gui=bold		guifg=SeaGreen
    highlight Repeat		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray	gui=bold		guifg=DodgerBlue3
    highlight Search		term=reverse							ctermbg=DarkYellow						    guibg=Yellow
    highlight SignColumn	term=standout				    ctermfg=DarkBlue	ctermbg=DarkGray				guifg=DarkBlue	    guibg=Grey
    highlight Special		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=SlateBlue     guibg=GhostWhite
    highlight SpecialChar	term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=DeepPink	    guibg=GhostWhite
    highlight SpecialComment	term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=VioletRed     guibg=GhostWhite
    highlight SpecialKey	term=bold				    ctermfg=DarkBlue	ctermbg=LightGray				guifg=Blue
    highlight SpellBad		term=reverse							ctermbg=LightRed	gui=undercurl							guisp=Red
    highlight SpellCap		term=reverse							ctermbg=LightBlue  	gui=undercurl 							guisp=Blue
    highlight SpellLocal	term=underline	 						ctermbg=LightCyan 	gui=undercurl 							guisp=DarkCyan
    highlight SpellRare		term=reverse	 						ctermbg=LightMagenta 	gui=undercurl 							guisp=Magenta
    highlight Statement		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray	gui=none		guifg=RoyalBlue4
    highlight StorageClass	term=underline				    ctermfg=DarkGreen	ctermbg=LightGray	gui=none		guifg=SeaGreen3
    highlight String		term=underline				    ctermfg=DarkRed	ctermbg=LightGray				guifg=RoyalBlue4    guibg=grey95 
    highlight Structure		term=underline				    ctermfg=DarkGreen	ctermbg=LightGray	gui=none		guifg=DarkSlateGray4
    highlight Tag		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray				guifg=DarkSlateBlue guibg=GhostWhite
    highlight Title		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray	gui=bold		guifg=Magenta
    highlight Todo		term=standout				    ctermfg=Black	ctermbg=Yellow					guifg=Blue	    guibg=Yellow
    highlight Type		term=underline				    ctermfg=DarkGreen	ctermbg=LightGray	gui=none		guifg=SeaGreen4
    highlight Typedef		term=underline				    ctermfg=DarkGreen	ctermbg=LightGray	gui=none		guifg=DarkSeaGreen4
    highlight Underlined	term=underline	    cterm=underline	    ctermfg=DarkMagenta	ctermbg=LightGray	gui=underline		guifg=SlateBlue
    highlight WarningMsg	term=standout				    ctermfg=DarkRed	ctermbg=LightGray				guifg=Red
    highlight WildMenu		term=standout				    ctermfg=Black	ctermbg=Yellow					guifg=Black	    guibg=Yellow
    "
    " Mouse Cursor
    " 
    highlight cCursor	     ctermfg=bg	    ctermbg=DarkRed     guifg=bg guibg=DarkRed
    highlight Cursor	     ctermfg=bg	    ctermbg=DarkGreen   guifg=bg guibg=DarkGreen
    highlight CursorColumn   term=reverse   ctermbg=7			 guibg=FloralWhite
    highlight CursorIM	     ctermfg=bg	    ctermbg=DarkGrey    guifg=bg guibg=DarkGrey
    highlight CursorLine     term=reverse   ctermbg=7			 guibg=cornsilk
    highlight lCursor	     ctermfg=bg	    ctermbg=DarkMagenta guifg=bg guibg=DarkMagenta
    highlight oCursor	     ctermfg=bg	    ctermbg=DarkCyan    guifg=bg guibg=DarkCyan
    highlight vCursor	     ctermfg=bg	    ctermbg=DarkYellow  guifg=bg guibg=DarkYellow

    set guicursor=n:block-lCursor,
		 \i:ver25-Cursor,
		 \r:hor25-Cursor,
		 \v:block-vCursor,
		\ve:ver35-vCursor,
		 \o:hor50-oCursor-blinkwait75-blinkoff50-blinkon75,
		 \c:block-cCursor,
		\ci:ver20-cCursor,
		\cr:hor20-cCursor,
		\sm:block-Cursor-blinkwait175-blinkoff150-blinkon175

    syntax enable
else

    set t_Co=8

endif

finish

" vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
