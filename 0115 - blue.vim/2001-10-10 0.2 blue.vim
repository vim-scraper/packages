" Vim color file
" Maintainer:   TaQ <taq@taq.cjb.net>
" Last Change:  10 October 2001 - 0.2
" URL: http://taq.cjb.net		

" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

set background=dark	" or light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="blue"

hi Normal	  ctermfg=Gray  ctermbg=DarkBlue guifg=#CCCCCC guibg=#000040
hi NonText    ctermfg=Gray  ctermbg=DarkBlue guifg=#FFFFFF guibg=#000040

hi Statement  ctermfg=White ctermbg=DarkBlue guifg=#00C0FF guibg=#000040
hi Comment    ctermfg=Blue  ctermbg=DarkBlue guifg=#00A0FF guibg=#000090
hi Constant	  ctermfg=White ctermbg=DarkBlue guifg=#FFFFFF guibg=#000040
hi Identifier ctermfg=Cyan  ctermbg=DarkBlue guifg=#0080E0 guibg=#000040
hi Type		  ctermfg=Cyan  ctermbg=DarkBlue guifg=#5050FF guibg=#000040
hi Folded	  ctermfg=White ctermbg=Blue     guifg=#00A0FF guibg=#000050
hi Special    ctermfg=Blue  ctermbg=DarkBlue guifg=#00A0FF guibg=#000090
hi PreProc	  ctermfg=Blue  ctermbg=DarkBlue guifg=#0090FF guibg=#000040
hi Scrollbar  ctermfg=Blue  ctermbg=DarkBlue guifg=#00C0FF guibg=#000040
hi Cursor     ctermfg=white ctermbg=DarkBlue guifg=#FFFFFF guibg=#5050FF
hi ErrorMsg   ctermfg=white ctermbg=Red      guifg=#FFFFFF guibg=#FF0000
hi WarningMsg ctermfg=Black ctermbg=Yellow   guifg=#000000 guibg=#FFFF00
hi VertSplit  ctermfg=White ctermbg=Blue     guifg=#000090 guibg=#000090
hi Directory  ctermfg=Cyan  ctermbg=DarkBlue guifg=#00A0F0 guibg=#000040
hi Visual	  ctermfg=Black ctermbg=Cyan     guifg=#5050FF guibg=#FFFFFF 

hi StatusLine   term=bold cterm=bold ctermfg=White ctermbg=LightBlue gui=bold guifg=#FFFFFF guibg=#0050EE
hi StatusLineNC term=bold cterm=bold ctermfg=Gray  ctermbg=Blue      gui=bold guifg=#AAAAAA guibg=#000090
hi LineNr		 term=bold cterm=bold ctermfg=Blue  ctermbg=DarkBlue  gui=bold guifg=#00A0FF guibg=#000050

