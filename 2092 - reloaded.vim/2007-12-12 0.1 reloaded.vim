" Vim color file
" Maintainer:   connorberry@yahoo.com   
" Last Change:  
" URL: www.narwhale.org

set background=dark     "or light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="reloaded"

        hi LineNr       term=bold gui=bold guifg=White guibg=DarkGray
        hi Normal       ctermfg=Green ctermbg=Black
        hi Normal       guifg=Green guibg=Black
        hi NonText      ctermfg=DarkGray  ctermbg=Black
        hi NonText      guifg=DarkGray  guibg=Black

        hi Statement    ctermfg=Green      ctermbg=Black
        hi Statement    guifg=Green      guibg=Black
        hi Comment      ctermfg=DarkGreen  ctermbg=Black cterm=bold term=bold
        hi Comment      guifg=DarkGreen  guibg=Black gui=bold term=bold
        hi Constant     ctermfg=Black  ctermbg=Green
        hi Constant     guifg=Black  guibg=Green
        hi Identifier   ctermfg=Green      ctermbg=Black
        hi Identifier   guifg=Green      guibg=Black
        hi Type         ctermfg=Green ctermbg=Black
        hi Type         guifg=Green guibg=Black
        hi String       ctermfg=Green ctermbg=DarkGreen
        hi String       guifg=Green guibg=DarkGreen
        hi Boolean      ctermfg=Green ctermbg=DarkGreen
        hi Boolean      guifg=Green guibg=DarkGreen
        hi Number       ctermfg=Green ctermbg=DarkGreen
        hi Number       guifg=Green guibg=DarkGreen
        hi Folded       ctermfg=DarkYellow ctermbg=Black cterm=underline term=none
        hi Folded       guifg=DarkYellow guibg=Black gui=underline term=none
        hi Special      ctermfg=Black      ctermbg=DarkGreen
        hi Special      guifg=Black      guibg=DarkGreen
        hi PreProc      ctermfg=DarkGreen ctermbg=Black cterm=bold term=bold
        hi PreProc      guifg=DarkGreen guibg=Black gui=bold term=bold
        hi Scrollbar    ctermfg=DarkYellow      ctermbg=Black
        hi Scrollbar    guifg=DarkYellow      guibg=Black
        hi Cursor       ctermfg=Black     ctermbg=Green
        hi Cursor       guifg=Black     guibg=Green
        hi ErrorMsg     ctermfg=Red       ctermbg=Black cterm=bold term=bold
        hi ErrorMsg     guifg=Red       guibg=Black gui=bold term=bold
        hi WarningMsg   ctermfg=Yellow    ctermbg=Black
        hi WarningMsg   guifg=Yellow    guibg=Black
        hi VertSplit    ctermfg=White     ctermbg=Black
        hi VertSplit    guifg=White     guibg=Black
        hi Directory    ctermfg=Green      ctermbg=DarkBlue
        hi Directory    guifg=Green      guibg=DarkBlue
        hi Visual       ctermfg=White     ctermbg=DarkGray cterm=underline term=none
        hi Visual       guifg=White     guibg=DarkGray gui=underline term=none
        hi Title        ctermfg=White     ctermbg=DarkBlue
        hi Title        guifg=White     guibg=DarkBlue

        hi StatusLine   term=bold cterm=bold,underline ctermfg=White ctermbg=Black
        hi StatusLine   term=bold gui=bold,underline guifg=White guibg=Black
        hi StatusLineNC term=bold cterm=bold,underline ctermfg=Gray  ctermbg=Black
        hi StatusLineNC term=bold gui=bold,underline guifg=Gray  guibg=Black
        hi LineNr       term=bold cterm=bold ctermfg=White ctermbg=DarkGray
        hi LineNr       term=bold gui=bold guifg=White guibg=DarkGray

        hi cursorline   ctermbg=White
        hi cursorline   guibg=DarkGray
