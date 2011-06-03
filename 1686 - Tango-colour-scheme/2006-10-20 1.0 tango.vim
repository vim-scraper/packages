"
" Tango Vim Color Scheme
" =======================
"
" For best results, set up your terminal with a Tango palette.
" Instructions for GNOME Terminal:
" http://uwstopia.nl/blog/2006/07/tango-terminal
"
" author:   Michele Campeotto <micampe@micampe.it>
"
set background=dark

hi clear
if exists("syntax_on")
    syntax reset
endif

let colors_name = "tango"

" Default Colors
hi Normal       guifg=#eeeeec guibg=#000000
hi NonText      guifg=#555753 guibg=#000000
hi NonText      ctermfg=darkgray
hi Cursor       guibg=#d3d7cf
hi lCursor      guibg=#d3d7cf

" Search
hi Search	guifg=#eeeeec guibg=#c4a000
hi Search	cterm=NONE ctermfg=grey ctermbg=blue
hi IncSearch	guibg=#eeeeec guifg=#729fcf
hi IncSearch	cterm=NONE ctermfg=yellow ctermbg=green

" Window Elements
hi StatusLine   guifg=#eeeeec guibg=#4e9a06 gui=bold
hi StatusLine   ctermfg=white ctermbg=green cterm=bold
hi StatusLineNC guifg=#d3d7df guibg=#4e9a06
hi StatusLineNC ctermfg=lightgray ctermbg=darkgreen
hi VertSplit    guifg=#eeeeec guibg=#eeeeec
hi Folded       guifg=#eeeeec guibg=#75507b
hi Folded       ctermfg=white ctermbg=magenta
hi Visual       guifg=#d3d7cf guibg=#4e9a06
hi Visual       ctermbg=white ctermfg=lightgreen cterm=reverse

" Specials
hi Todo         guifg=#8ae234 guibg=#4e9a06 gui=bold
hi Todo         ctermfg=white ctermbg=green
hi Title        guifg=#eeeeec gui=bold
hi Title        ctermfg=white cterm=bold

" Syntax
hi Constant     guifg=#c4a000
hi Constant     ctermfg=darkyellow
hi Number       guifg=#729fcf
hi Number       ctermfg=darkblue
hi Statement    guifg=#4e9a06 gui=bold
hi Statement    ctermfg=green
hi Identifier   guifg=#8ae234
hi Identifier   ctermfg=darkgreen
hi PreProc      guifg=#cc0000
hi PreProc      ctermfg=darkred
hi Comment      guifg=#06989a gui=italic
hi Comment      ctermfg=cyan cterm=NONE
hi Type         guifg=#d3d7cf gui=bold
hi Type         ctermfg=gray cterm=bold
hi Special      guifg=#75507b
hi Special      ctermfg=magenta cterm=NONE
hi Error        guifg=#eeeeec guibg=#ef2929
hi Error        ctermfg=white ctermbg=red

" Diff
hi DiffAdd      guifg=fg guibg=#3465a4 gui=NONE
hi DiffAdd	ctermfg=gray ctermbg=blue cterm=NONE
hi DiffChange	guifg=fg guibg=#555753 gui=NONE
hi DiffChange	ctermfg=gray ctermbg=darkgray cterm=NONE
hi DiffDelete   guibg=NONE
hi DiffDelete	ctermfg=gray ctermbg=NONE cterm=NONE
hi DiffText	guifg=fg guibg=#c4a000 gui=NONE
hi DiffText	ctermfg=gray ctermbg=yellow cterm=NONE
