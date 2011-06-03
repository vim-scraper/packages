" Vim color file
" carvedwood v0.1
" Maintainer:	Shawn Axsom <axs221@gmail.com>
"               [axs221.1l.com]

" carvedwood -
"     a color scheme modified from my desertocean scheme into a brown
" and green scheme, easier on the eyes and optimized for more important syntax
" to stand out the most (eg comments and values are dark and dull while
" statements are bright).

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

let g:colors_name="carvedwood"

hi Normal	guifg=#e0bacd guibg=#322c29

" syntax highlighting
hi Comment	  guifg=#5a6a60
hi Title	  guifg=#00BBFF
hi Underlined guifg=#00A0FF
hi Statement  guifg=#eac0ba
hi Type		  guifg=#c08d8b
hi PreProc    guifg=#b06a75 gui=none 
hi Constant	  guifg=#a0777D
hi Identifier guifg=#EE807a 
hi Special	  guifg=#6aba7f
hi Ignore     guifg=grey40
hi Todo		  guifg=orangered guibg=yellow2
"hi Error
"end syntax highlighting

" highlight groups
hi Cursor	guibg=#007799 guifg=#00D0D0
"hi CursorIM
hi Directory guifg=#bbd0df
"hi DiffAdd
"hi DiffChange
"hi DiffDelete
"hi DiffText
"hi ErrorMsg
hi VertSplit	guibg=#c2bfa5 guifg=grey50 gui=none
hi Folded	guibg=#4a4f4d guifg=#BBDDCC
hi FoldColumn	guibg=#4c404a guifg=#00CCFF
hi LineNr   guifg=#CCF0FF guibg=#405653
hi ModeMsg	guifg=#00AACC
hi MoreMsg	guifg=SeaGreen
hi NonText  guifg=#285960 guibg=#1A3A4A
hi Question	guifg=#AABBCC
hi Search	guibg=slategrey guifg=#FFDABB
hi IncSearch	guifg=slategrey guibg=#FFDFB0
hi SpecialKey	guifg=#906C3B
hi StatusLine	guibg=#cA9a9A guifg=#050709 gui=none
hi StatusLineNC	guibg=#907a80 guifg=#272334 gui=none
hi Visual   guifg=#008FBF guibg=#33DFEF
"hi VisualNOS
hi WarningMsg	guifg=salmon
"hi WildMenu
"hi Menu
"hi Scrollbar  guibg=grey30 guifg=tan
"hi Tooltip


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

