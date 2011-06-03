" Vim syntax file
" Language:	Eukleides
" Maintainer:	Robert Goulding <goulding@princeton.edu>
" Version:	0.3
" Last Change:  Wed Jan 30 10:33:28 EST 2002

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword eukFunction abs sqrt exp ln sin cos tan asin acos atan deg rad
syn keyword eukFunction abscissa ordinate distance length radius major minor eccentricity height arg
syn keyword eukFunction angle
syn keyword eukFunction vector
syn keyword eukFunction reflection rotation homothecy translation

syn keyword eukDefinition point barycenter abscissa ordinate midpoint begin end center orthocenter projection
" Matches `intersection', as in: Q = intersection(l1,l2)
syn match eukDefinition "\([^0-9a-zA-Z_ ]\s*\)\@<=intersection"
syn keyword eukDefinition line parallel perpendicular bisector altitude median
syn keyword eukDefinition segment
syn keyword eukDefinition circle incircle
syn keyword eukDefinition conic parabola ellipse hyperbola

syn keyword eukDeclaration foci vertices parallelogram rectangle square pentagon hexagon triangle isosceles equilateral
syn match eukDeclaration "right"
" Matches `intersection', as in: A B intersection(c1,c2)
syn match eukDeclaration "\([0-9a-zA-Z_]\s*\)\@<=intersection"
syn match eukVertices "[0-9a-zA-Z_ ]\+\(foci\|vertices\|intersection\|parallelogram\|rectangle\|square\|pentagon\|hexagon\|triangle\|right\|isosceles\|equilateral\|interactive\)\@="

syn keyword eukOperator = + - * / ^

syn keyword eukInteractive interactive

syn keyword eukSettings thickness frame
syn match eukSettings "box"
syn match eukXcolor "color\s*(.*)"me=s+5
syn match eukPScolor +color\s*".*"+me=s+5
syn keyword eukSpecialSettings tricks font
syn keyword eukFlag dot cross plus full dashed dotted entire halfline backhalfline 
syn keyword eukFlag noarrow arrow backarrow doublearrow simple double triple up
syn keyword eukFlag black darkgray gray lightgray white red green blue cyan magenta yellow

syn match eukFlag "\(,\s*\)\@<=right"
syn match eukFlag "\(,\s*\)\@<=box"

syn keyword eukDrawing draw mark trace
syn region eukString start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match eukTrailingsc ";\(\s*$\)\@="

"integer number, or floating point number without a dot.
syn match  eukNumber		"-\=\<\d\+\>"
"floating point number, with dot
syn match  eukNumber		"-\=\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match  eukNumber		"-\=\.\d\+\>"

syn keyword eukConstant pi

" Note: If you don't like having the whole of any angle-expression
" highlighted, change to the other definition of eukAngle

" syn match eukAngle "\w\+\(:\|<\)"
syn match eukAngle "[a-zA-Z0-9()_+\-*/]\+\(:\|<\)"
syn match eukComment "%.*$"

syn match eukTraceBraces "{\|}"
" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_basic_syntax_inits")
  if version < 508
    let did_basic_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

HiLink eukComment Comment

HiLink eukString String
HiLink eukNumber Number
HiLink eukConstant Constant

HiLink eukVertices Identifier
HiLink eukFunction Function


HiLink eukDeclaration Define

HiLink eukDrawing Statement
HiLink eukInteractive Statement
HiLink eukXcolor Statement
HiLink eukSettings Statement

HiLink eukDefinition Type

HiLink eukTrailingsc Error

HiLink eukOperator Operator

HiLink eukFlag Underlined
HiLink eukPScolor Special
HiLink eukSpecialSettings Special
HiLink eukAngle Special

  delcommand HiLink
endif

" In the font I use, braces and brackets are easily confused, so this helps to
" tell whether a trace command has been closed.  If you don't like it, comment
" it out.
hi eukTraceBraces cterm=bold gui=bold

let b:current_syntax = "eukleides"

