" Vim color scheme
"
" Name:         railscast.vim
" Maintainer:   Josh O'Rourke <jorourke23@gmail.com> 
" Last Change:  16 Aug 2007 
" License:      public domain
" Version:      1.0
"
" This theme is based on the Railscasts Textmate theme [1]. I used 
" Jo Vermeulen's "vibrantink" theme for Vim [2] as my template for 
" creating this theme.
"
" [1] http://railscasts.com/about 
" [2] http://www.vim.org/scripts/script.php?script_id=1794 

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let g:colors_name = "railscasts"

if has("gui_running")
    highlight Normal guifg=#F8F8F8   guibg=#0C1021
    highlight Cursor guifg=Black   guibg=White
    highlight Keyword guifg=#CC7833
    highlight Define guifg=#CC7833
    highlight Comment guifg=#BC9458
    highlight Type guifg=#DA4939 gui=NONE
    highlight rubySymbol guifg=#6E9CBE gui=NONE
    highlight Identifier guifg=#D0D0FF gui=NONE
    highlight rubyStringDelimiter guifg=#A5C261
    highlight rubyInterpolation guifg=#519F50
    highlight rubyPseudoVariable guifg=#6E9CBE
    highlight Constant guifg=#6D9CBE
    highlight Function guifg=#FFC66D gui=NONE
    highlight Include guifg=#CC7833 gui=NONE
    highlight Statement guifg=#CC7833 gui=NONE
    highlight String guifg=#A5C261
    highlight Search guibg=#FFFF00
    highlight CursorLine guibg=#323300
endif
