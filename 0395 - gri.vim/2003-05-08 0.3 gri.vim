" Vim syntax file
" Language: gri
" Maintainer: Patricio Toledo <patoledo@ing.uchile.cl>
" Last Change: mar jue may  8 20:57:09 CLT 2003
" Filenames: *.gri

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
 syntax clear
elseif exists("b:current_syntax")
 finish
endif

syn case ignore

syn region griComment start="#" end="$"
syn region griMatcher matchgroup=Special start="{" skip="[\#]\+" end="}" contains=griString,griVariable,griNumber fold
syn region griString start=+["']+ skip=+\\"+ end=+["']+ 
syn region griSystem start="system\s\+" skip="/\+" end="$" contains=griString,griVariable,griNumber
syn region griVariable start="\.\+" end="\.\+\(\s\+\|$\)" oneline
syn region griVariable start="\\\+" end="\s\+" oneline

syn match griError "}" 
syn match griNumber "\<\d\+\(\.\d*\)\=\([edED][-+]\=\d\+\)\=[ij]\=\>"
syn match griNumber "\.\d\+\([edED][-+]\=\d\+\)\=[ij]\=\>"
syn match griNumber "\<\d\+[ij]\=\>"

syn keyword griConditional if else end
syn keyword griFiles open close skip read rewind
syn keyword griFunction interpolate regress
syn keyword griFunction power abs differentiate 
syn keyword griRepeat while 
syn keyword griSet set convert draw create delete quit resize
"syn keyword griSetTo arc arrows arrow axes box circle contour curve 
"syn keyword griSetTo columns grid spline image 
"syn keyword griSetTo frame bottom top left right rapidograph
"syn keyword griSetTo grayscale graylevel
"syn keyword griSetTo histogram label line legend poligon name 
"syn keyword griSetTo page panel path postscript scale size margin
"syn keyword griSetTo symbol title axis type color clip colour font 
"syn keyword griSetTo width filled lines vertically horizontally
syn keyword griStatement break return continue
syn keyword griStatement flip smooth source
syn keyword griStatement help input insert query show window
syn keyword griStatement pwd cd ls 
syn keyword griTo to at from rgb hsb hsv

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_gri_syntax_inits")
 if version < 508
 let did_gri_syntax_inits = 1
 command -nargs=+ HiLink hi link <args>
 else
 command -nargs=+ HiLink hi def link <args>
 endif

 HiLink griComment Comment
 HiLink griConditional Conditional
 HiLink griCorchete Function
 HiLink griError Error
 HiLink griFiles Function
 HiLink griFunction Function
 HiLink griNumber Number
 HiLink griOctalError Error
 HiLink griPolacNotation Function
 HiLink griSystem String
 HiLink griRepeat Repeat
 HiLink griSet Function
 HiLink griSetTo Statement
 HiLink griStatement Function
 HiLink griString String
 HiLink griSynonims Statement
 HiLink griTo Special
 HiLink griVariable Statement

 delcommand HiLink
endif

let b:current_syntax = "gri"
