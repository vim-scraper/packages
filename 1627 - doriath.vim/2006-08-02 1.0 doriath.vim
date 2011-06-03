" Vim color file
" Maintainer:   Alex Esplin (alex.esplin@gmail.com)
" Last Change:  
" URL:		

" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

" your pick:
set background=dark	" or light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="doriath"

hi Normal               guifg=honeydew2 guibg=#0e2a1a

" OR

" highlight clear Normal
" set background&
" highlight clear
" if &background == "light"
"   highlight Error ...
"   ...
" else
"   highlight Error ...
"   ...
" endif

" A good way to see what your colorscheme does is to follow this procedure:
" :w 
" :so % 
"
" Then to see what the current setting is use the highlight command.  
" For example,
" 	:hi Cursor
" gives
"	Cursor         xxx guifg=bg guibg=fg 
 	
" Uncomment and complete the commands you want to change from the default.

hi Cursor	        guibg=#54ff9f guifg=#0e2a1a
"hi CursorIM	
"hi Directory	
"hi DiffAdd		
"hi DiffChange	
"hi DiffDelete	
"hi DiffText	
"hi ErrorMsg	
hi VertSplit	        guibg=#c2bfa5 guifg=grey50 gui=none
hi Folded		guibg=#4e9271 guifg=#0e2a1a
hi FoldColumn	        guibg=grey30 guifg=tan
hi IncSearch	        guibg=khaki guifg=slategrey
hi LineNr		guifg=green
hi ModeMsg		guifg=goldenrod
hi MoreMsg		guifg=SeaGreen
hi NonText		guibg=LightBlue guibg=SeaGreen
hi Question	        guifg=springgreen
hi Search		guibg=DarkRed guifg=DarkSeaGreen4
hi SpecialKey	        guifg=yellowgreen
hi StatusLine	        guibg=#c2dfa5 guifg=black gui=none
hi StatusLineNC	        guibg=#c2dfa5 guifg=grey50 gui=none
hi Title	        guifg=indianred	
hi Visual		guibg=olivedrab guifg=khaki gui=none
"hi VisualNOS	
hi WarningMsg	        guifg=salmon
"hi WildMenu	
"hi Menu		
"hi Scrollbar	
"hi Tooltip		

" syntax highlighting groups
hi Comment              guifg=SkyBlue
hi Constant	        guifg=#ffa0a0
hi Identifier	        guifg=palegreen
hi Statement	        guifg=khaki
hi PreProc	        guifg=indianred
hi Type		        guifg=#00ff62
hi Special	        guifg=navajowhite
"hi Underlined	
hi Ignore		guifg=grey40
"hi Error		
hi Todo		        guifg=orangered guibg=yellow2

