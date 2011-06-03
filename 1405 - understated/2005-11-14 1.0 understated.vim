" High Def:     Color file for sensitive eyes.
" Maintainer:   Adam Blinkinsop <blinks@acm.org>
" Last Change:  2005 Nov 14  
" URL:		    http://blinkinblogs.net

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="understated"

hi Normal       ctermfg=Grey ctermbg=Black guifg=Grey guibg=Black

" internal coloring
hi Cursor       ctermfg=Black ctermbg=White guifg=Black guibg=White
hi CursorIM	    ctermfg=Black ctermbg=White guifg=Black guibg=White
"hi Directory	
"hi DiffAdd		
"hi DiffChange	
"hi DiffDelete	
"hi DiffText	
hi ErrorMsg	    ctermbg=DarkRed guibg=DarkRed
"hi VertSplit	
"hi Folded		
"hi FoldColumn	
"hi IncSearch	
"hi LineNr		
"hi ModeMsg		
"hi MoreMsg		
"hi NonText		
"hi Question	
"hi Search		
"hi SpecialKey	
"hi StatusLine	
"hi StatusLineNC	
"hi Title		
hi Visual		ctermfg=DarkGrey guifg=DarkGrey ctermbg=White guibg=White
hi VisualNOS    ctermfg=DarkGrey guifg=DarkGrey ctermbg=White guibg=White	
"hi WarningMsg	
"hi WildMenu	
"hi Menu		
"hi Scrollbar	
"hi Tooltip		

" language general coloring
hi Comment      ctermfg=DarkGrey guifg=DarkGrey
hi Constant	    ctermfg=White guifg=White cterm=underline gui=underline
hi Identifier   ctermfg=DarkCyan guifg=DarkCyan cterm=bold gui=bold
hi Statement	ctermfg=DarkGreen guifg=DarkGreen cterm=bold gui=bold
hi PreProc	    ctermfg=DarkGreen guifg=DarkGreen cterm=underline gui=underline
hi Type	    	ctermfg=DarkCyan guifg=DarkCyan cterm=underline gui=underline
hi Special	    ctermfg=Blue guifg=Blue
hi Underlined	ctermfg=White guifg=White cterm=underline gui=underline
hi Ignore		ctermfg=DarkGrey guifg=DarkGrey cterm=bold gui=bold
hi Error		ctermbg=Red guibg=Red
hi Todo	    	ctermfg=Black guifg=Black ctermbg=DarkYellow guibg=DarkYellow

