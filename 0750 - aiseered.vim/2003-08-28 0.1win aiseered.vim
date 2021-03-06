" Vim color file for working with files in GDL/VCG format
" works nice in conjunction with gdl.vim 
" (see www.vim.org or www.aisee.com)

" Author : Alexander A. Evstiougov-Babaev <alex@absint.com>
" Version: 0.1win (not useful for Unix terminals)
"          tested with gVim 6.2 under Windows 2000
" Date   : 28 August 2003

set background=dark	
hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="aiseered"

hi Normal guifg=lightred guibg=#600000
hi Cursor guifg=bg guibg=fg
hi ErrorMsg guibg=red ctermfg=1

hi Comment guifg=white
hi Constant guifg=#88eeff
hi String guifg=#ffcc88
hi Character guifg=#ffaa00
hi Number guifg=#88eeff
hi Identifier guifg=#dddddd
hi Statement guifg=#eeff99 gui=bold
hi PreProc guifg=firebrick1 gui=italic
hi Type guifg=#88ffaa gui=none
hi Special guifg=#ffaa00
hi Error guifg=red guibg=yellow
