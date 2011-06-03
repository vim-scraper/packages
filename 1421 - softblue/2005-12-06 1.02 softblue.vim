" Vim color file
" Maintainer:   Zhang Jing
" Last Change:  %[% 2005年12月06日 星期二 10时21分08秒 %]%

set background=dark
if version > 580
    hi clear
    if exists("syntax_on")
	syntax reset
    endif
endif
let g:colors_name="softblue"

hi Normal	    guibg=#1c345c   guifg=#b0b0e0

hi Cursor	    guibg=#b3b2df   guifg=grey30    gui=bold
hi VertSplit	guibg=#466292   guifg=grey50    gui=none
hi Folded	    guibg=#0d2349   guifg=lightblue
hi FoldColumn	guibg=#0d2349   guifg=lightblue
hi IncSearch	guifg=slategrey guibg=khaki
hi LineNr		guifg=grey30
hi ModeMsg	    guifg=SkyBlue
hi MoreMsg	    guifg=SeaGreen
hi NonText	    guifg=LightBlue guibg=#0d2349
hi Question	    guifg=springgreen
hi Search	    guibg=#787878   guifg=wheat
hi SpecialKey	guifg=yellowgreen
hi StatusLine	guibg=#466292   guifg=black     gui=none
hi StatusLineNC	guibg=#466292   guifg=grey22    gui=none
hi Title	    guifg=#38d9ff
hi Visual	    guifg=lightblue guibg=darkblue  gui=none
hi WarningMsg	guifg=salmon

hi Comment	    guifg=#6279a0
hi Constant	    guifg=#9b60be
hi Identifier	guifg=#00ac55
hi Statement	guifg=SkyBlue2
hi PreProc	    guifg=#20a0d0
hi Type		    guifg=#8070ff
hi Special	    guifg=#a2b9e0
hi Ignore 	    guifg=grey40
hi Error        guifg=white     guibg=#a02020
hi Todo		    guifg=#54b900   guibg=#622098
"vim:ts=4:tw=4
