" cschemerotate.vim: Colorscheme Rotator
" Version: 0.2
" Maintainer:  Rafal M. Sulejman (rms@poczta.onet.pl)
" To Enable: put this script into your ~/.vim/plugin directory
" Usage: ,b cycles thru 3 preconfigured color schemes
" History: 
" 17 Jun 2003 - version 0.2 - parametrized + uses users CS settings as default
" 17 Jun 2003 - initial version 0.1

" INITIALISATIONS
" start value for currentScheme
let s:currentScheme=0

" You can customize your alternate colorschemes here:
let s:alternateCS0 = 'murphy'
let s:alternateCS1 = 'zellner'

" Script uses the users colorscheme (if set) as default
if exists("colors_name")>0
	let s:alternateCS2=colors_name
else
	let s:alternateCS2='default'
endif

" Rotate ColorScheme
function RotateCS()
	exec "let s:s = s:alternateCS" . s:currentScheme
	exec "colorscheme " . s:s
	let s:currentScheme = (s:currentScheme + 1) % 3
endfun 

" Mapped to ,b (like 'background')
map ,b :call RotateCS()<Return>
