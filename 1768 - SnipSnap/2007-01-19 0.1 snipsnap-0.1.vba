" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
./colors/martin_krischik.vim	[[[1
418
"-------------------------------------------------------------------------------
"  Description: My personal colors
"          $Id: martin_krischik.vim 465 2006-11-22 18:01:07Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2006-11-22 19:01:07 +0100 (Mi, 22 Nov 2006) $
"      Version: 3.2
"    $Revision: 465 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/colors/martin_krischik.vim $
"	  Note:	Tried and Tested for 'builtin_gui', 'xterm' (KDE Konsole)
"		'vt320'" (OpenVMS) and 'linux' (Linux Console).
"      History: 16.05.2006 MK Check that all vim 7.0 colors are set
"		16.05.2006 MK Split GUI from terminal.
"		24.05.2006 MK Unified Headers
"		24.07.2006 MK Omni-Completion Colors.
"               15.10.2006 MK Bram's suggestion for runtime integration
"	 Usage: copy to colors directory
"------------------------------------------------------------------------------

" First remove all existing highlighting.

set background=light
highlight clear

if exists ("syntax_on")
    syntax reset
endif

let colors_name = "martin_krischik"

if version < 700
   " Section: works only with vim 7.0 use default otherwise {{{1
   "
   colorscheme default
   "
   " }}}1
   finish
elseif (&term == "builtin_gui")
    " Section: Set GUI colors. {{{1
    "
    " Subsection: User-Interface Colors {{{2
    "
    " Group: Normal Text Colors {{{3
    "
    highlight Normal		gui=none		guifg=black	    guibg=white
    highlight Search							    guibg=Yellow
    highlight SpecialKey				guifg=Blue
    highlight Title		gui=bold		guifg=Magenta
    highlight LineNr					guifg=Brown	    guibg=grey80
    highlight NonText		gui=bold		guifg=Blue	    guibg=grey80
    highlight MatchParen						    guibg=Cyan
    highlight IncSearch		gui=reverse
    "
    " Group: Messages {{{3
    "
    highlight WarningMsg				guifg=Red
    highlight ErrorMsg					guifg=White	    guibg=Red
    highlight ModeMsg		gui=bold
    highlight MoreMsg		gui=bold		guifg=SeaGreen
    highlight Question		gui=bold		guifg=SeaGreen
    "
    " Group: Spell Checker {{{3
    "
    highlight SpellBad		gui=undercurl							guisp=Red
    highlight SpellCap		gui=undercurl							guisp=Blue
    highlight SpellLocal	gui=undercurl							guisp=DarkCyan
    highlight SpellRare		gui=undercurl							guisp=Magenta
    "
    " Group: Status line {{{3
    "
    highlight StatusLine	gui=bold,reverse	guifg=LightBlue2    guibg=black
    highlight StatusLineNC	gui=reverse		guifg=grey75	    guibg=black
    highlight VertSplit		gui=reverse		guifg=LightBlue3    guibg=black
    "
    " Group: Visual selektio {{{3n
    "
    highlight Visual		gui=reverse		guifg=firebrick     guibg=white
    highlight VisualNOS		gui=reverse		guifg=firebrick     guibg=black
    "
    " Group: tab pages line {{{3
    "
    highlight TabLine		gui=reverse		guifg=grey75	    guibg=black
    highlight TabLineFill	gui=reverse
    highlight TabLineSel	gui=bold,reverse	guifg=LightBlue2    guibg=black
    "
    " Group: Competion (omni and otherwise) menu colors {{{3
    "
    highlight Pmenu							    guibg=Grey
    highlight PmenuSel					guifg=White	    guibg=firebrick
    highlight PmenuSbar					guibg=LightGrey	    guibg=DarkGrey
    highlight PmenuThumb	gui=reverse
    highlight WildMenu					guifg=White	    guibg=firebrick
    "
    " Group: Diff colors {{{3
    "
    highlight DiffAdd							    guibg=LightBlue
    highlight DiffChange						    guibg=LightMagenta
    highlight DiffDelete	gui=bold		guifg=Blue	    guibg=LightCyan
    highlight DiffText		gui=bold				    guibg=Red
    "
    " Group: Fold colors {{{3
    "
    highlight FoldColumn				guifg=DarkBlue	    guibg=Grey
    highlight Folded					guifg=DarkBlue	    guibg=LightGrey
    "
    " Group: Other Syntax Highlight Colors {{{3
    "
    highlight Directory		guifg=Blue
    highlight SignColumn	guifg=DarkBlue	    guibg=Grey
    "
    " Group: Motif and Athena widget colors. {{{3
    "
    highlight Menu		guifg=Black	    guibg=LightGrey
    highlight Scrollbar		guifg=LightGrey	    guibg=DarkGrey
    highlight Tooltip		guifg=Black	    guibg=LightGrey

    " Subsection: Syntax Colors  {{{2
    "
    " Group: Comment colors syntax-group
    "
    highlight Comment					guifg=grey30
    "
    " Group: Constant colors group {{{3
    "
    highlight Boolean					guifg=DarkOrchid3   guibg=grey95
    highlight Character					guifg=RoyalBlue3    guibg=grey95
    highlight Constant					guifg=MediumOrchid3 guibg=grey95
    highlight Float					guifg=MediumOrchid4 guibg=grey95
    highlight Number					guifg=DarkOrchid4   guibg=grey95
    highlight String					guifg=RoyalBlue4    guibg=grey95
    "
    " Group: Identifier colors group {{{3
    "
    highlight Function					guifg=SteelBlue
    highlight Identifier				guifg=DarkCyan
    "
    " Group: Statement colors group {{{3
    "
    highlight Conditional	gui=bold		guifg=DodgerBlue4
    highlight Exception		gui=none		guifg=SlateBlue4
    highlight Keyword		gui=bold		guifg=RoyalBlue4
    highlight Label		gui=none		guifg=SlateBlue3
    highlight Operator		gui=none		guifg=RoyalBlue3
    highlight Repeat		gui=bold		guifg=DodgerBlue3
    highlight Statement		gui=none		guifg=RoyalBlue4
    "
    " Group: Preprocessor colors group {{{3
    "
    highlight Define					guifg=brown4	    guibg=snow
    highlight Include					guifg=firebrick3    guibg=snow
    highlight Macro					guifg=brown3	    guibg=snow
    highlight PreCondit					guifg=red	    guibg=snow
    highlight PreProc					guifg=firebrick4    guibg=snow
    "
    " Group: type group {{{3
    "
    highlight StorageClass	gui=none		guifg=SeaGreen3
    highlight Structure		gui=none		guifg=DarkSlateGray4
    highlight Type		gui=none		guifg=SeaGreen4
    highlight Typedef		gui=none		guifg=DarkSeaGreen4
    "
    " Group: special symbol group {{{3
    "
    highlight Special					guifg=SlateBlue     guibg=GhostWhite
    highlight SpecialChar				guifg=DeepPink	    guibg=GhostWhite
    highlight Tag					guifg=DarkSlateBlue guibg=GhostWhite
    highlight Delimiter					guifg=DarkOrchid    guibg=GhostWhite
    highlight SpecialComment				guifg=VioletRed     guibg=GhostWhite
    highlight Debug					guifg=maroon	    guibg=GhostWhite
    "
    " Group: text that stands out {{{3
    "
    highlight Underlined	gui=underline		guifg=SlateBlue
    "
    " Group: left blank, hidden {{{3
    "
    highlight Ignore					guifg=bg
    "
    " Group: any erroneous construct {{{3
    "
    highlight Error		gui=undercurl		guifg=Red	    guibg=MistyRose
    "
    " Group: anything that needs extra attention {{{3
    "
    highlight Todo					guifg=Blue	    guibg=Yellow

    " Subsection: Cursor Colors {{{2
    "
    " Group: Mouse Cursor {{{3
    "
    highlight cCursor	     guifg=bg	 guibg=DarkRed
    highlight Cursor	     guifg=bg	 guibg=DarkGreen
    highlight CursorColumn		 guibg=FloralWhite
    highlight CursorIM	     guifg=bg	 guibg=DarkGrey
    highlight CursorLine		 guibg=cornsilk
    highlight lCursor	     guifg=bg	 guibg=DarkMagenta
    highlight oCursor	     guifg=bg	 guibg=DarkCyan
    highlight vCursor	     guifg=bg	 guibg=DarkYellow
    "
    " Group: Text Cursor {{{3
    "
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

    " Subsection: User Colors {{{2
    "
    " Group: Wikis {{{3
    "
    highlight Bold	gui=bold
    highlight Code					 guifg=SlateBlue      guibg=GhostWhite
    highlight Header1					 guifg=DarkOrchid3    guibg=grey95
    highlight Header2		       			 guifg=RoyalBlue3     guibg=grey95
    highlight Header3		       			 guifg=MediumOrchid3  guibg=grey95
    highlight Header4		       			 guifg=MediumOrchid4  guibg=grey95
    highlight Header5		       			 guifg=DarkOrchid4    guibg=grey95
    highlight Html					 guifg=DarkSlateBlue  guibg=GhostWhite
    highlight Italic	gui=Italic
    highlight Link					 guifg=brown4	      guibg=snow
    highlight OList					 guifg=DarkCyan
    highlight Strike	gui=undercurl
    highlight Table					 guifg=DarkCyan       guibg=GhostWhite
    highlight TableColumn                                guifg=VioletRed      guibg=GhostWhite
    highlight UList					 guifg=SteelBlue
    highlight Underline	gui=underline

    syntax enable

   " }}}1
   finish
elseif	(&term == "xterm")  ||
      \ (&term == "vt320")  ||
      \ (&term == "linux")
    " Section: Only set colors for terminals we actualy know of {{{1
    "
    if &term=="vt320"
	set t_Co=8
    else
	set t_Co=16
    endif

    " Subsection: User Interface Colors {{{2
    "
    " Group: Normal Text Colors {{{3
    "
    highlight Normal		term=none	    cterm=none		    ctermfg=Black	ctermbg=LightGray
    highlight Search		term=reverse							ctermbg=DarkYellow
    highlight SpecialKey	term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight Title		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight LineNr		term=underline				    ctermfg=DarkRed	ctermbg=DarkGray
    highlight NonText		term=bold				    ctermfg=LightBlue	ctermbg=LightGray
    highlight MatchParen	term=reverse				    ctermbg=DarkYellow
    highlight IncSearch		term=reverse	    cterm=reverse
    "
    " Group: Messages {{{3
    "
    highlight WarningMsg	term=standout				    ctermfg=DarkRed	ctermbg=LightGray
    highlight ErrorMsg		term=standout				    ctermfg=White	ctermbg=DarkRed
    highlight ModeMsg		term=bold	    cterm=bold					ctermbg=LightGray
    highlight MoreMsg		term=bold				    ctermfg=DarkGreen	ctermbg=LightGray
    highlight Question		term=standout				    ctermfg=DarkGreen	ctermbg=LightGray
    "
    " Group: Spell Checker {{{3
    "
    highlight SpellBad		term=reverse							ctermbg=LightRed
    highlight SpellCap		term=reverse							ctermbg=LightBlue
    highlight SpellLocal	term=underline							ctermbg=LightCyan
    highlight SpellRare		term=reverse							ctermbg=LightMagenta
    "
    " Group: Status line {{{3
    "
    highlight StatusLine	term=bold,reverse   cterm=bold,reverse
    highlight StatusLineNC	term=reverse	    cterm=reverse
    highlight VertSplit		term=reverse	    cterm=reverse
    "
    " Group: Visual selektion {{{3
    "
    highlight Visual		term=reverse	    cterm=reverse	    ctermfg=DarkRed	ctermbg=LightGray
    highlight VisualNOS		term=bold,underline cterm=bold,underline
    "
    " Group: tab pages line {{{3
    "
    highlight TabLine		term=reverse	    cterm=reverse
    highlight TabLineFill	term=reverse	    cterm=reverse
    highlight TabLineSel	term=bold,reverse   cterm=bold,reverse
    "
    " Group: Menu colors {{{3
    "
    highlight Pmenu										ctermbg=Grey
    highlight PmenuSel							    ctermfg=White	ctermbg=Red
    highlight PmenuSbar							    ctermfg=LightGrey	ctermbg=DarkGray
    highlight PmenuThumb			    cterm=reverse
    highlight WildMenu		term=standout				    ctermfg=White	ctermbg=Red
    "
    " Group: Diff colors {{{3
    "
    highlight DiffAdd		term=bold							ctermbg=LightBlue
    highlight DiffChange	term=bold							ctermbg=LightMagenta
    highlight DiffDelete	term=bold				    ctermfg=LightBlue	ctermbg=LightCyan
    highlight DiffText		term=reverse	    cterm=bold					ctermbg=LightRed
    "
    " Group: Fold colors {{{3
    "
    highlight FoldColumn	term=standout				    ctermfg=DarkBlue	ctermbg=DarkGray
    highlight Folded		term=standout				    ctermfg=DarkBlue	ctermbg=DarkGray
    "
    " Group: Other Syntax Highlight Colors {{{3
    "
    highlight Directory		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight SignColumn	term=standout				    ctermfg=DarkBlue	ctermbg=DarkGray

    " Subsection: Syntax Colors {{{2
    "
    " Group: Comment colors syntax-group {{{3
    "
    highlight Comment		term=bold				    ctermfg=DarkGray	ctermbg=LightGray
    "
    " Group: Constant colors group {{{3
    "
    highlight Boolean		term=underline				    ctermfg=DarkRed	ctermbg=LightGray
    highlight Character		term=underline				    ctermfg=DarkRed	ctermbg=LightGray
    highlight Constant		term=underline				    ctermfg=DarkRed	ctermbg=LightGray
    highlight Float		term=underline				    ctermfg=DarkRed	ctermbg=LightGray
    highlight Number		term=underline				    ctermfg=DarkRed	ctermbg=LightGray
    highlight String		term=underline				    ctermfg=DarkRed	ctermbg=LightGray
    "
    " Group: Identifier colors group {{{3
    "
    highlight Function		term=underline				    ctermfg=DarkCyan	ctermbg=LightGray
    highlight Identifier	term=underline				    ctermfg=DarkCyan	ctermbg=LightGray
    "
    " Group: Statement colors group {{{3
    "
    highlight Conditional	term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight Exception		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight Keyword		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight Label		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight Operator		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight Repeat		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    highlight Statement		term=bold				    ctermfg=DarkBlue	ctermbg=LightGray
    "
    " Group: Preprocessor colors group {{{3
    "
    highlight Define		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight Include		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight Macro		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight PreCondit		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight PreProc		term=underline				    ctermfg=DarkMagenta	ctermbg=LightGray
    "
    " Group: type group {{{3
    "
    highlight StorageClass	term=underline				    ctermfg=DarkGreen	ctermbg=LightGray
    highlight Structure		term=underline				    ctermfg=DarkGreen	ctermbg=LightGray
    highlight Type		term=underline				    ctermfg=DarkGreen	ctermbg=LightGray
    highlight Typedef		term=underline				    ctermfg=DarkGreen	ctermbg=LightGray
    "
    " Group: special symbol group {{{3
    "
    highlight Special		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight SpecialChar	term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight Tag		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight Delimiter		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight SpecialComment	term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray
    highlight Debug		term=bold				    ctermfg=DarkMagenta	ctermbg=LightGray
    "
    " Group: text that stands out {{{3
    "
    highlight Underlined	term=underline	    cterm=underline	    ctermfg=DarkMagenta	ctermbg=LightGray
    "
    " Group: left blank, hidden {{{3
    "
    highlight Ignore							    ctermfg=White	ctermbg=grey
    "
    " Group: any erroneous construct {{{3
    "
    highlight Error		term=reverse				    ctermfg=White	ctermbg=LightRed
    "
    " Group: anything that needs extra attention {{{3
    "
    highlight Todo		term=standout				    ctermfg=Black	ctermbg=Yellow

    " Subsection: Cursor Colors {{{2
    "
    " Group: Mouse Cursor {{{3
    "
    highlight Cursor				    ctermfg=bg		    ctermbg=DarkGreen
    highlight CursorColumn	term=reverse				    ctermbg=LightGray
    highlight CursorIM				    ctermfg=bg		    ctermbg=DarkGrey
    highlight CursorLine	term=reverse				    ctermbg=LightGray

    syntax enable

   " }}}1
    finish
else
   " Section: terminal is completely unknown - fallback to system default {{{1
   "
   set t_Co=8

   " }}}1
   finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: filetype=vim foldmethod=marker textwidth=0
./ftdetect/snipsnap.vim	[[[1
32
"- -----------------------------------------------------------------------------
"  Description: Snip Snap detection file
"     Language: Snip Snap Wiki
"          $Id: snipsnap.vim,v 1.2 2006/11/24 12:05:04 krischikm Exp $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer: Martin Krischik
"      $Author: krischikm $
"	 $Date: 2006/11/24 12:05:04 $
"      Version: 0.1
"    $Revision: 1.2 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ftdetect/ada.vim $
"      History: 20.11.2006 MK SnipSnap Wiki Syntax
"    Help Page: ft-snipnap-plugin
"------------------------------------------------------------------------------

if exists("s:loaded_ftdetect_snipsnap")
    finish
endif

let s:loaded_ftdetect_snipsnap=01

autocmd BufNewFile,BufRead *.snipsnap setfiletype snipsnap 

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
./ftplugin/snipsnap.vim	[[[1
75
"------------------------------------------------------------------------------
"  Description: Perform SnipSnap specific completion & tagging.
"     Language: Snip Snap Wiki
"	   $Id: snipsnap.vim,v 1.1 2006/11/22 14:13:23 krischikm Exp $
"   Maintainer: Martin Krischik
"      $Author: krischikm $
"	 $Date: 2006/11/22 14:13:23 $
"      Version: 0.1
"    $Revision: 1.1 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ftplugin/ada.vim $
"      History: 20.11.2006 MK SnipSnap Wiki Syntax
"    Help Page: ft-snipsnap-plugin
"------------------------------------------------------------------------------

" Only do this when not done yet for this buffer
if exists ("b:did_ftplugin") || version < 700
   finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 01

"
" Temporarily set cpoptions to ensure the script loads OK
"
let s:cpoptions = &cpoptions
set cpoptions-=C

" Section: File-Option {{{1
"
setlocal encoding=utf-8
setlocal wrap
setlocal smartcase
setlocal noignorecase

" Section: Comments {{{1
"
setlocal comments=O:{html}<!--
setlocal commentstring={html}<!--\ %s\ -->{html}
setlocal complete=.,w,b,u,t,i

" Section: Tagging {{{1
"

" Section: Completion {{{1
"

" Section: Matchit {{{1
"

" Section: Folding {{{1
"
set foldmethod=syntax

" Section: Abbrev {{{1
"

" Section: Commands, Mapping, Menus {{{1
"

" 1}}}
" Reset cpoptions
"
let &cpoptions = s:cpoptions
unlet s:cpoptions

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker nospell
./syntax/snipsnap.vim	[[[1
117
"----------------------------------------------------------------------------
"  Description: Snip Snap syntax file
"     Language: Snip Snap Wiki
"          $Id: snipsnap.vim,v 1.1 2006/11/22 14:13:23 krischikm Exp $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer: Martin Krischik
"      $Author: krischikm $
"        $Date: 2006/11/22 14:13:23 $
"      Version: 4.2
"    $Revision: 1.1 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/syntax/ada.vim $
"               http://www.dwheeler.com/vim
"      History: 20.11.2006 MK SnipSnap Wiki Syntax
"    Help Page: help ft-snipsnap-syntax
"------------------------------------------------------------------------------

if exists("b:current_syntax") || version < 700
    finish
endif

let b:current_syntax = "snipsnap"

" Section: SnipSnap is case-sensitive. {{{1
"
syntax case match

" Section: Simple Format {{{1
"
syntax region  SnipSnapBold   start=/__/    end=/__/    oneline
syntax region  SnipSnapItalic start=/\~\~/  end=/\~\~/  oneline
syntax region  SnipSnapStrike start=/--/    end=/--/    oneline

" Section: Header Format {{{1

syntax match   SnipSnapHeader1 /^1\s.\{1,}$/            contains=SnipSnapExtLink,SnipSnapIntLink
syntax match   SnipSnapHeader2 /^1.1\s.\{1,}$/          contains=SnipSnapExtLink,SnipSnapIntLink
syntax match   SnipSnapHeader3 /^1.1.1\s.\{1,}$/        contains=SnipSnapExtLink,SnipSnapIntLink
syntax match   SnipSnapHeader4 /^1.1.1.1\s.\{1,}$/      contains=SnipSnapExtLink,SnipSnapIntLink
syntax match   SnipSnapHeader5 /^1.1.1.1.1\s.\{1,}$/    contains=SnipSnapExtLink,SnipSnapIntLink

" Section: List Format {{{1

syntax match   SnipSnapDashList /^-\s.\{1,}$/
syntax match   SnipSnapDotList  /^\*\s.\{1,}$/
syntax match   SnipSnapNList    /^1\.\s.\{1,}$/
syntax match   SnipSnapLAList   /^a\.\s.\{1,}$/
syntax match   SnipSnapUAList   /^A\.-\s.\{1,}$/
syntax match   SnipSnapLAList   /^i\.\s.\{1,}$/
syntax match   SnipSnapUAList   /^I\.-\s.\{1,}$/

" Section: Link Format {{{1

syntax region SnipSnapExtLink start=/{link:/ end=/\(|none\)\{0,1}}/  oneline contains=@NoSpell
syntax region SnipSnapIntLink start=/\[/     end=/]/                 oneline contains=@NoSpell

" Section: Code Format {{{1

syntax region  SnipSnapCode start=/^{code\(:\a\{1,}\)\{0,1}}/ end=/^{code}/  contains=@NoSpell fold
syntax region  SnipSnapHtml start=/{html}/                    end=/{html}/   contains=@NoSpell fold

" Section: Table1 Format {{{1

syntax region  SnipSnapTable
    \   start=/{table}/
    \   end=/{table}/
    \   contains=SnipSnapTableColumn,SnipSnapExtLink,SnipSnapIntLink
    \   fold

syntax match SnipSnapTableColumn /|/ contained

" Section: Comments. {{{1
"
syntax region  SnipSnapComment start="{html}<!--" end="-->{html}" contains=@NoSpell fold

" Section: The default methods for highlighting. Can be overridden later. {{{1
"
highlight def link SnipSnapBold         Bold
highlight def link SnipSnapCode         Code
highlight def link SnipSnapComment      Comment
highlight def link SnipSnapDashList     UList
highlight def link SnipSnapDotList      UList
highlight def link SnipSnapExtLink      Link
highlight def link SnipSnapHeader1      Header1
highlight def link SnipSnapHeader2      Header2
highlight def link SnipSnapHeader3      Header3
highlight def link SnipSnapHeader4      Header4
highlight def link SnipSnapHeader5      Header5
highlight def link SnipSnapHtml         Html
highlight def link SnipSnapIntLink      Link
highlight def link SnipSnapItalic       Italic
highlight def link SnipSnapLAList       OList
highlight def link SnipSnapLRList       OList
highlight def link SnipSnapNList        OList
highlight def link SnipSnapStrike       Strike
highlight def link SnipSnapTable        Table
highlight def link SnipSnapTableColumn  TableColumn
highlight def link SnipSnapUAList       OList
highlight def link SnipSnapURList       OList
:
" Section: formatoptions {{{1
"
setlocal formatoptions+=ron

" Section: sync {{{1
"
" for {code} and {html} blocks we need to look backwards
syntax sync minlines=100 maxlines=200

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
"vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
"vim: foldmethod=marker
snipsnap_options.vim	[[[1
59
------------------------------------------------------------------------------
"  Description: Options setable by the wls plugin
"	   $Id: wls_options.vim,v 1.1 2006/11/24 11:48:59 krischikm Exp $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischikm $
"	 $Date: 2006/11/24 11:48:59 $
"      Version: 0.1
"    $Revision: 1.1 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/wls_options.vim $
"      History:	17.11.2006 MK wls_Options
"	 Usage: copy content into your .vimrc and change options to your
"		likeing.
"    Help Page: ft-ada-options
"------------------------------------------------------------------------------

echoerr 'It is suggested to copy the content of ada_options into .vimrc!'
finish " 1}}}

" Section: snipsnap options {{{1

   let g:mapleader		 = "<F12>"

   filetype plugin indent on
   syntax enable

" }}}1

" Section: Vimball options {{{1
:set expandtab fileformat=unix encoding=utf-8
:.+2,.+6 MkVimball snipsnap-0.1

./colors/martin_krischik.vim	    \
./ftdetect/snipsnap.vim
./ftplugin/snipsnap.vim
./syntax/snipsnap.vim
snipsnap_options.vim

" }}}1

" Section: Tar options {{{1

tar --create --bzip2		    \
   --file="snipsnap-0.1.tar.bz2"    \
   ./colors/martin_krischik.vim	    \
   ./ftdetect/snipsnap.vim	    \
   ./ftplugin/snipsnap.vim	    \
   ./syntax/snipsnap.vim	    \
   snipsnap_options.vim

" }}}1

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
