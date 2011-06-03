" Vim syntax file
" Language:	gri-2.10.1
" Maintainer:	Patricio Toledo patoledo@ing.puc.cl from gnuplot.vim
" Last Change:	lun jun 17 22:31:29 CLT 2002
" Filenames:    *.gri
" URL:		
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	griNumber	"\<[0-9]\+\(u\=l\=\|lu\|f\)\>"
"floating point number, with dot, optional exponent
syn match	griFloat	"\<[0-9]\+\.[0-9]*\(e[-+]\=[0-9]\+\)\=[fl]\=\>"
"floating point number, starting with a dot, optional exponent
syn match	griFloat	"\.[0-9]\+\(e[-+]\=[0-9]\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	griFloat	"\<[0-9]\+e[-+]\=[0-9]\+[fl]\=\>"
"hex number
syn match	griNumber	"\<0x[0-9a-f]\+\(u\=l\=\|lu\)\>"
syn case match
"flag an octal number with wrong digits by not hilighting
syn match	griOctalError	"\<0[0-7]*[89]"

" comments + strings
syn region	griComment	start="#" end="$"
syn region	griString	start=+"+ skip=+\\"+ end=+"+
syn region	griString	start=+'+	     end=+'+

" Programación
syn keyword     griStatement    break return continue
syn keyword     griConditional  if else end
syn keyword     griRepeat       while 

"set variables
syn region	griVariable     start=+\.+  end=+\.+    oneline
syn region	griVariable     start=+\.\.+ end=+\.\.+ oneline
syn region	griVariable     start=+\.\.\.+ end=+\.\.\.+ oneline

" set sinonimos
syn match 	griSynonims     "\\[a-zA-Z@]\+" 

" set delimitadores
syn region 	griMatcher	matchgroup=Delimiter start="{" skip="\\\\\|\\[{}]"	end="}"

" commands
syn keyword	griSet		set convert draw create
syn keyword	griTo		to at
syn keyword	griSetTo	columns grid spline image grayscale graylevel
syn keyword	griSetTo	arc arrows arrow axes box circle contour curve 
syn keyword	griSetTo  	histogram label line legend poligon name 
syn keyword	griSetTo  	symbol title axis type color clip colour font 
syn keyword	griSetTo  	page panel path postscript scale size margin
syn keyword     griSetTo        width filled lines vertically horizontally
syn keyword     griSetTo        frame bottom top left right rapidograph
syn keyword	griFiles        open close skip read rewind
syn keyword	griData         x y z u v 
syn keyword	griStatement 	flip resize smooth source
syn keyword	griStatement	help input insert query show quit 
syn keyword	griStatement	pwd cd ls 	
syn keyword	griFuncion      rpn power abs differentiate interpolate regress


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

  HiLink griSet		Function
  HiLink griTo		Special
  HiLink griSetTo	Statement
  HiLink griFiles       Function
  HiLink griSynonims	Statement
  HiLink griVariable	Statement
  HiLink griCorchete	Function
  HiLink griOctalError	Error
  HiLink griData	Type
  
  HiLink griFunction	Function
  HiLink griRepeat 	Repeat
  HiLink griStatement	Statement
  HiLink griConditional	Conditional
  HiLink griNumber	Number
  HiLink griFloat	Float
  HiLink griComment	Comment
  HiLink griString	String

  delcommand HiLink
endif

let b:current_syntax = "gri"

" vim: ts=8
