
" Vim color file
" Maintainer:   tranquility@portugalmail.pt
" Last Change: 5 June 2002


" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="xemacs"

hi Normal				guibg=#cdcecd guifg=black
hi Cursor				guibg=red guifg=grey gui=bold
hi VertSplit		guifg=white guibg=navyblue gui=none
hi Folded				guibg=darkblue guifg=white
hi FoldColumn		guibg=lightgray guifg=navyblue
hi ModeMsg			guifg=#cdcecd guibg=black
hi MoreMsg			guifg=#cdcecd guibg=black
hi NonText			guifg=black guibg=#cdcecd gui=none
hi Question			guifg=black
hi Search				guibg=#aceeee
hi SpecialKey		guifg=navyblue
hi StatusLine		guibg=#b7b7b7 guifg=black gui=none
hi StatusLineNC	guibg=#a6b7b7 guifg=black gui=none
hi Title				guifg=bisque3
hi Subtitle			guifg=black
hi Visual				guibg=#a4a5a3 guifg=black gui=none
hi WarningMsg		guibg=#cdcecd guifg=black gui=bold
hi Comment			guifg=#00008b
hi Constant			guifg=#008900
hi Identifier		guibg=#cdcecd guifg=black
hi Statement		guifg=royalblue4
hi PreProc			guifg=#0000cd
hi Type					guifg=#4a81b4 gui=NONE
hi Special			guifg=navajowhite
hi Ignore				guifg=grey29
hi Todo					guibg=gold guifg=black
hi WildMenu			guibg=#b7b7b7 guibg=grey91
