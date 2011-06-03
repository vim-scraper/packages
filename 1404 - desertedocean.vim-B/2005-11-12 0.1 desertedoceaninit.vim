" Vim color file
" Maintainer:	Shawn Axsom <axm285@gmail.com>
"               [http://axm285.50webs.com]

" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

set background=dark
if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
		syntax reset
    endif
endif
let g:colors_name="shawn"

" Version:	$Id: desert.vim,v 1.6 2003/06/02 19:40:21 fugalh Exp $
hi Normal	guifg=#FFE9E0 guibg=#283940

" highlight groups
"hi Cursor	guibg=#660000 guifg=#AA5522
hi Cursor	guibg=#007799 guifg=#00D6CC
"hi CursorIM
"hi Directory
"hi DiffAdd
"hi DiffChange
"hi DiffDelete
"hi DiffText
"hi ErrorMsg
hi VertSplit	guibg=#c2bfa5 guifg=grey50 gui=none
hi Folded	guibg=#337799 guifg=#BBDDCC
hi FoldColumn	guibg=#337799 guifg=#00CCFF
hi IncSearch	guifg=slategrey guibg=khaki """
hi LineNr   guifg=#EEEEFF guibg=#006688 
hi ModeMsg	guifg=#00AACC
hi MoreMsg	guifg=SeaGreen
hi NonText  guifg=#283940 guibg=#2C3F48 "same as background
hi Question	guifg=springgreen
hi Search	guibg=#677990 guifg=wheat
hi SpecialKey	guifg=#00CCBB " blue green
hi StatusLine	guibg=#c2bfa5 guifg=black gui=none
hi StatusLineNC	guibg=#c2bfa5 guifg=grey50 gui=none
hi Title	guifg=#33AAFF       guibg=#353035
hi Underlined      guifg=#3399FF guibg=#354045 gui=underline
hi Visual   guifg=#008FBF guibg=#33DFEF
"hi VisualNOS
hi WarningMsg	guifg=salmon
"hi WildMenu
"hi Menu
"hi Scrollbar  guibg=grey30 guifg=tan
"hi Tooltip

" syntax highlighting groups
hi Comment	  guifg=#77bbcc
hi Underlined guifg=#00DDEE
hi Statement  guifg=#FF8070
hi PreProc    guifg=#FFA090 gui=NONE
hi Constant	  guifg=#FF6959 " or #FF707A 
hi Identifier guifg=#FFDDEE
hi Type		  guifg=#FFB999
hi Special	  guifg=navajowhite
hi Ignore	guifg=grey40
"hi Error
hi Todo		guifg=orangered guibg=yellow2

" color terminal definitions
hi SpecialKey	ctermfg=darkgreen
hi NonText	cterm=bold ctermfg=darkblue
hi Directory	ctermfg=darkcyan
hi ErrorMsg	cterm=bold ctermfg=7 ctermbg=1
hi IncSearch	cterm=NONE ctermfg=yellow ctermbg=green
hi Search	cterm=NONE ctermfg=grey ctermbg=blue
hi MoreMsg	ctermfg=darkgreen
hi ModeMsg	cterm=NONE ctermfg=brown
hi LineNr	ctermfg=3
hi Question	ctermfg=green
hi StatusLine	cterm=bold,reverse
hi StatusLineNC cterm=reverse
hi VertSplit	cterm=reverse
hi Title	ctermfg=5
hi Visual	cterm=reverse
hi VisualNOS	cterm=bold,underline
hi WarningMsg	ctermfg=1
hi WildMenu	ctermfg=0 ctermbg=3
hi Folded	ctermfg=darkgrey ctermbg=NONE
hi FoldColumn	ctermfg=darkgrey ctermbg=NONE
hi DiffAdd	ctermbg=4
hi DiffChange	ctermbg=5
hi DiffDelete	cterm=bold ctermfg=4 ctermbg=6
hi DiffText	cterm=bold ctermbg=1
hi Comment	ctermfg=darkcyan
hi Constant	ctermfg=brown
hi Special	ctermfg=5
hi Identifier	ctermfg=6
hi Statement	ctermfg=3
hi PreProc	ctermfg=5
hi Type		ctermfg=2
hi Underlined	cterm=underline ctermfg=5
hi Ignore	cterm=bold ctermfg=7
hi Ignore	ctermfg=darkgrey
hi Error	cterm=bold ctermfg=7 ctermbg=1


"vim: sw=4
