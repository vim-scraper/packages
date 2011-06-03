" Vim color file
" @Author: Pascal Vasilii <jabberroid@gmail.com>	


hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "silent"

hi Cursor guifg=black guibg=grey gui=NONE
hi LineNr ctermbg=LightYellow ctermfg=LightGrey gui=bold,italic guifg=DarkGray guibg=#F1FFC1
hi StatusLineNC gui=bold,italic guifg=White guibg=DimGray
hi StatusLine   guifg=#DDDDDD guibg=#1D343B gui=italic
hi SpecialKey gui=none guifg=orange
hi Title gui=bold guifg=Black

hi FoldColumn gui=none guifg=Black guibg=#F1FFC1
hi VertSplit gui=none guifg=White guibg=DimGray
hi Wildmenu gui=bold guifg=Black guibg=White

hi Pmenu guibg=#DDDDDD guifg=Black gui=italic
hi PmenuSbar guibg=#DDDDDD guifg=fg gui=italic
hi PmenuSel guibg=#F1FFC1 guifg=Black gui=italic
hi PmenuThumb guibg=#DDDDDD guifg=fg gui=none

hi IncSearch gui=none guifg=White guibg=Black
hi Search gui=none guifg=Black guibg=Yellow

hi Normal	        ctermfg=DarkGrey guifg=#141312 guibg=White
hi Visual	        ctermfg=Blue guibg=#4485FF guifg=white gui=bold
hi Comment	        ctermfg=LightGrey guifg=#888786	gui=italic
hi PerlPOD	        ctermfg=Brown guifg=#B86A18	gui=NONE
hi Constant	        ctermfg=DarkBlue guifg=#141312 gui=NONE
hi Charachter	    ctermfg=Yellow guifg=#644A9B	gui=NONE
hi String           ctermfg=DarkRed guifg=#BF0303	gui=NONE
hi Number	        ctermfg=DarkRed  guifg=#B07E00 gui=NONE
hi Boolean	        ctermfg=Cyan guifg=#B07E00	gui=NONE
hi Special	        ctermfg=DarkMagenta	guifg=#9C0D0D gui=NONE
hi Define	        ctermfg=LightMagenta guifg=#006E26 gui=NONE
hi Identifier 	    ctermfg=DarkBlue guifg=#0057AE gui=NONE
hi Exception 	    ctermfg=DarkBlue guifg=blue gui=bold
hi Statement 	    ctermfg=DarkBlue guifg=#006E26 gui=NONE
hi Label 	        ctermfg=DarkBlue guifg=orange gui=NONE
hi Keyword 	        ctermfg=DarkBlue guifg=green gui=NONE
hi PreProc	        ctermfg=DarkBlue guifg=#141312 gui=NONE
hi Type		        ctermfg=DarkGreen guifg=#006E26 gui=bold
hi Function	        ctermfg=DarkBlue guifg=#644A9B gui=bold
hi Repeat	        ctermfg=DarkBlue guifg=#B07E00 gui=NONE
hi Operator	        ctermfg=DarkBlue guifg=#0057AE gui=NONE
hi Ignore	        ctermfg=DarkBlue guifg=bg
hi Folded           ctermbg=LightBlue ctermfg=Gray guibg=DarkBlue guifg=DarkGray gui=NONE
hi Error	        term=reverse ctermbg=White ctermfg=Red guibg=darkRed guifg=Red gui=NONE
hi Todo		        term=standout ctermbg=Yellow ctermfg=Black guifg=Grey guibg=#AD5500 gui=NONE
hi Done		        term=standout ctermbg=Gray ctermfg=White guifg=#CCEEFF guibg=Gray gui=NONE

hi SpellErrors      ctermfg=DarkRed guifg=#9C0D0D gui=NONE

hi MoreMsg          gui=NONE
hi ModeMsg          gui=NONE
hi Title            gui=bold
hi NonText          gui=NONE
hi DiffDelete       gui=NONE
hi DiffText         gui=NONE
hi Question         gui=NONE
"hi link String	Constant
"hi link Character	Constant
"hi link Number		Constant
"hi link Boolean	Constant
hi link Float		Number
hi link Conditional	Repeat
hi link Include		PreProc
hi link Structure	Define
hi link Macro		PreProc
hi link PreCondit	PreProc
hi link StorageClass	Type
hi link Structure	Type
hi link Typedef		Type
hi link Tag		Special
hi link SpecialChar	Special
hi link Delimiter	Normal
hi link SpecialComment 	Special
hi link Debug		Special

