" Vim syntax file
" Language:	GMT3.4 (Generic Mapping Tools)
" Maintainer:	Patricio Toledo patoledo@ing.uchile.cl
" Last Change:	mar oct 15 21:47:16 CLST 2002
" Filenames:    *.gmt
" URL:		
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

" comments + strings
syn region	gmtComment	start="#" end="$"
syn region	gmtString	start=+"+ skip=+\\"+ end=+"+
syn region	gmtString	start=+'+	     end=+'+

" Options are followed by parameters, those could be secuences of letters
" or numbers. An interesting thing is that options are heredated, so 
" we could paint options alone.

" options, start with '-' between spaces. Upper or lower case as well.
syn match       gmtOptions	"\s-\a"

" J, G, L, D, W options require special treaatment, cause admites other
" letters, no more than 2.
syn match	gmtOptions      "\s-\(J\|G\|L\|W\|D\)\a\{1,2}"

" grd, cpt, eps, ps files
syn match	gmtFiles	"\w\{-}\.\(grd\|cpt\|eps\|ps\)"

" fits float or integer, positive or negative 
syn match	gmtParameters 	"\(-\d\|\d\|\d\.\d\)\+"

" redirection in sh, tcsh, bash, etc.
syn match	gmtRedir	"\d\=>\(&[-0-9]\)\="
syn match	gmtRedir	"\d\=>>-\="
syn match	gmtRedir	"\d\=|-\="
syn match	gmtRedir	"\d\=<\(&[-0-9]\)\="
syn match	gmtRedir	"\d<<-\="

" set synonims
syn match 	gmtSynonims     "\\[a-zA-Z@]\+" 

" GMT commands, mainly those of /usr/local/GMT/bin, ;-)
syn keyword	gmtSet	    gmtdefaults gmtset gmtpath

syn keyword	gmtTo       dat2gmt gmt2bin gmt2dat grd2cpt grd2xyz img2grd
syn keyword     gmtTo       img2grd img2mercgrd mgd77togmt xyz2grd gmtconvert

syn keyword     gmtSetTo    gmtinfo gmtlegs gmtlist gmtmath gmtselect gmttrack

syn keyword	gmtPlot     psbasemap psclip pscoast pscontour pscoupe
syn keyword     gmtPlot     pshistogram psimage psmask psmeca psmegaplot
syn keyword     gmtPlot     pspolar psrose psscale pssegy pssegyz pstext
syn keyword     gmtPlot     psvelo pswiggle psxy psxyz 

syn keyword	gmtGrid     grdclip grdcontour grdcut grdedit grdfft grdfilter
syn keyword     gmtGrid     grdgradient grdhisteq grdimage grdinfo grdlandmask
syn keyword     gmtGrid     grdmask grdmath grdpaste grdproject grdraster
syn keyword     gmtGrid     grdreformat grdsample grdtrack grdtrend grdvector
syn keyword     gmtGrid     grdview grdvolume

syn keyword	gmtFunction  triangulate makecpt

syn keyword	gmtText     pstext

" Little suport for sh, tcsh, bash. (from sh.vim)
syn keyword     gmtStatement    break return continue
syn keyword     gmtConditional  if else end then
syn keyword     gmtRepeat       for do done while 
syn keyword     gmtSh           cp mv rm rmdir cat EOF BEGIN END echo
syn keyword     gmtSh           cd eval exec pwd chdir kill alias pwd

" set matchers, mainly for sh scripting.
syn region 	gmtMatcher	matchgroup=Delimiter start="{" skip="\\\\\|\\[{}]"	end="}"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_gmt_syntax_inits")
  if version < 508
    let did_gmt_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink gmtSet		Function
  HiLink gmtTo		Function
  HiLink gmtSetTo	Function
  HiLink gmtPlot        Function
  HiLink gmtGrid        Function
  HiLink gmtFunction	Function
  HiLink gmtText        Function
  HiLink gmtSh          Statement
  HiLink gmtRedir       Statement
  HiLink gmtFiles       Ignore
  HiLink gmtOptions     Type
  HiLink gmtParameters  Special
  HiLink gmtComment	Comment
  HiLink gmtString	String
  HiLink gmtStatement	Statement
  HiLink gmtConditional	Conditional
  HiLink gmtRepeat 	Repeat
  HiLink gmtSynonims	Statement

  delcommand HiLink
endif

let b:current_syntax = "gmt"

" vim: ts=8
