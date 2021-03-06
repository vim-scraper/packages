" Vim color File
" Name:			BioGoo
" Maintainer:	Benjamin Esham <bdesham@iname.com>
" Last Change:	2002-09-28
" Version:		0.95
"
" A fairly simple gray-background scheme.  Feedback is greatly appreciated!
"
" Installation:
"   Copy to ~/.vim/colors; do :color biogoo
"
" Customization Options:
"   Use a 'normal' cursor color:
"     let g:biogoo_normal_cursor = 1
"
" Props:
"   Jani Nurminen's zenburn.vim as an example file.
"
" TODO: Possibly add some more groups

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "biogoo"

hi Comment			guifg=#0000c3
hi Delimiter		guifg=#00007f
hi DiffAdd			guifg=#007f00 guibg=#e5e5e5
hi DiffChange		guifg=#00007f guibg=#e5e5e5
hi DiffDelete		guifg=#7f0000 guibg=#e5e5e5
hi DiffText			guifg=#ee0000 guibg=#e5e5e5
hi Directory		guifg=#b85d00
hi Error			guifg=#d6d6d6 guibg=#7f0000
hi ErrorMsg			guifg=#ffffff guibg=#ff0000 gui=bold
hi Float			guifg=#b85d00
hi FoldColumn		guifg=#00007f guibg=#e5e5e5
hi Folded			guifg=#00007f guibg=#e5e5e5
hi Function			guifg=#7f0000
hi Identifier		guifg=#007f00
hi Include			guifg=#295498
hi IncSearch		guifg=#ffff00 guibg=#540054
hi LineNr			guifg=#303030 guibg=#e5e5e5 gui=underline
hi Keyword			guifg=#00007f
hi Macro			guifg=#295498
hi ModeMsg			guifg=#00007f
hi MoreMsg			guifg=#00007f
hi NonText			guifg=#007f00 guibg=#e5e5e5
hi Normal			guifg=#000000 guibg=#d6d6d6
hi Number			guifg=#b85d00
hi Operator			guifg=#00007f
hi PreCondit		guifg=#295498
hi PreProc			guifg=#295498 gui=bold
hi Question			guifg=#00007f
hi Search			guibg=#ffff00
hi SpecialKey		guifg=#00007f
hi Statement		guifg=#00007f gui=none
hi StatusLine		guifg=#00007f guibg=#ffffff
hi StatusLineNC		guifg=#676767 guibg=#ffffff
hi String			guifg=#d10000
hi Title			guifg=#404040 gui=underline
hi Todo				guifg=#00007f guibg=#e5e5e5 gui=underline
hi Type				guifg=#540054 gui=bold
hi VertSplit		guifg=#676767 guibg=#ffffff
hi Visual			guifg=#00007f guibg=#e5e5e5
hi VisualNOS		guifg=#007f00 guibg=#e5e5e5
hi WarningMsg		guifg=#500000
hi WildMenu			guifg=#540054

if !exists("g:biogoo_normal_cursor")
	" use a gray-on-blue cursor
	hi Cursor		guifg=#ffffff guibg=#00007f
endif
