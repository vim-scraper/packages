" Vim color file
" carvedwood v0.4
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

hi Normal	guifg=#dac0cd guibg=#1e2221

" syntax highlighting
hi Comment	  guifg=#556a6d
hi Title	  guifg=#00BBFF
hi Underlined guifg=#00A0FF
hi Statement  guifg=#e5b3ad
hi Type		  guifg=#c58f8a
hi PreProc    guifg=#a06a7a gui=none 
hi Constant	  guifg=#9a7679
hi Identifier guifg=#c67a75 
hi Special	  guifg=#709a7a
hi Ignore     guifg=grey40
hi Todo		  guifg=orangered guibg=yellow2
"hi Error
"end syntax highlighting

" highlight groups
hi Cursor	guibg=#205a50 guifg=#80C0B0
"hi CursorIM
hi Directory guifg=#bbd0df
"hi DiffAdd
"hi DiffChange
"hi DiffDelete
"hi DiffText
"hi ErrorMsg
hi VertSplit	guibg=#c2bfa5 guifg=grey50 gui=none
hi Folded	guibg=#4a4f4d guifg=#BBDDCC
hi FoldColumn	guibg=#48403d guifg=#00CCFF
hi LineNr   guifg=#D0C5CA guibg=#4e4240
hi ModeMsg	guifg=#00AACC
hi MoreMsg	guifg=SeaGreen
hi NonText  guifg=#382920 guibg=#1b1f1f
hi Question	guifg=#AABBCC
hi Search	guibg=#50606d guifg=#c0caba
hi IncSearch	guifg=#50606d guibg=#e5efda
hi SpecialKey	guifg=#906C3B
hi StatusLine	guibg=#c99f93 guifg=#102015 gui=none
hi StatusLineNC	guibg=#937b7a guifg=#373334 gui=none
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

