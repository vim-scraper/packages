" Vim color file
" Maintainer:   Donald Ephraim Curtis <dcurtis@gmail.com>
" Last Change:  09. january 2007.
" URL:          http://milkbox.net
" Kate default color themes.

set background=light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="DevC++"

hi Comment      gui=italic      guifg=#808080		guibg=NONE
hi Identifier   gui=NONE		guifg=Black			guibg=NONE
hi Statement    gui=bold		guifg=DarkBlue		guibg=NONE
hi PreProc      gui=NONE		guifg=#008000		guibg=NONE	
hi link cIncluded PreProc
hi Statement    gui=bold        guifg=Black         guibg=NONE
hi cOctal       gui=NONE        guifg=#008080       guibg=NONE
hi Type         gui=bold		guifg=#800000		guibg=NONE
hi link Constant Type
hi Special      gui=NONE		guifg=#FF00FF	    guibg=NONE
hi Structure    gui=bold        guifg=Black         guibg=NONE
hi String       gui=NONE  	    guifg=#DD0000	    guibg=NONE
hi Number       gui=NONE		guifg=#0000FF       guibg=NONE
hi Float        gui=NONE        guifg=#800080       guibg=NONE
hi Boolean      gui=bold        guifg=Black	        guibg=NONE
hi Gutter       gui=NONE        guifg=Black		    guibg=Grey
hi Todo         gui=bold    	guifg=black		    guibg=#FFCCCC
hi LineNr       gui=NONE        guifg=Black         guibg=#EBE9ED
hi NonText      gui=bold 		guifg=black         guibg=#EBE9ED
hi Visual       gui=reverse     guifg=NONE          guibg=NONE
hi MatchParen   gui=NONE        guifg=Black         guibg=#FFFF99
hi Question     gui=NONE        guifg=DarkGreen     guibg=NONE
hi StatusLine   gui=bold        guifg=Black         guibg=#EBE9ED

