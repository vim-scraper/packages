" Vim syntax file
" Language: GMT (Generic Mapping Tools)
" Maintainer: Patricio Toledo <patoledo@ing.uchile.cl>
" Last Change: jue may  8 20:57:47 CLT 2003
" Filenames: *.gmt

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
 syntax clear
elseif exists("b:current_syntax")
 finish
endif

syn case match

" comments + strings
syn region gmtComment start="#" end="$"
syn region gmtString start=+"+ skip=+\\"+ end=+"+
syn region gmtString start=+'+ end=+'+

" Options are followed by parameters, those could be secuences of letters
" or numbers. An interesting thing is that options are heredated, so 
" we could paint options alone.

" options, start with '-' between spaces. Upper and lower case as well. The
" option b is special.
syn match gmtOptions "\s-\a"
syn match gmtOptions "\s-\(bi\|bo\)"

" D, J, L, N, S, W options require special treatment, because admites other
" letters, no more than 2 anyway.
syn match gmtOptions "\s-\(D\|J\|L\|N\|S\|W\)\a\{1,2}"

" fits float or integer, positive or negative 
"syn match gmtParameters "\(-\d\|\d\|\d\.\d\)\+"

" redirection in bash, etc.
syn match gmtRedir "\s\(>\|>>\|&>\|>&\||\)\s"
syn match gmtRedir "\s\(<\|<<\)\s"

" GMT defaults. Used with gmtset.
syn keyword gmtDefaults ANOT_MIN_ANGLE ANOT_MIN_SPACING ANOT_FONT
syn keyword gmtDefaults ANOT_FONT_SIZE ANOT_OFFSET BASEMAP_AXES
syn keyword gmtDefaults BASEMAP_FRAME_RGB BASEMAP_TYPE
syn keyword gmtDefaults COLOR_BACKGROUND COLOR_FOREGROUND 
syn keyword gmtdefaults COLOR_NAN COLOR_IMAGE COLOR_MODEL D_FORMAT
syn keyword gmtDefaults DEGREE_FORMAT DOTS_PR_INCH ELLIPSOID 
syn keyword gmtDefaults FRAME_PEN FRAME_WIDTH GLOBAL_X_SCALE
syn keyword gmtDefaults GLOBAL_Y_SCALE GRID_CROSS_SIZE GRID_PEN
syn keyword gmtDefaults GRIDFILE_SHORTHAND HEADER_FONT HEADER_FONT_SIZE
syn keyword gmtDefaults HSV_MIN_SATURATION HSV_MAX_SATURATION 
syn keyword gmtDefaults HSV_MIN_VALUE HSV_MAX_VALUE INTERPOLANT
syn keyword gmtDefaults IO_HEADER N_HEADER_RECS LABEL_FONT
syn keyword gmtDefaults LABEL_FONT_SIZE LINE_STEP MAP_SCALE_FACTOR
syn keyword gmtDefaults MAP_SCALE_HEIGHT MEASURE_UNIT N_COPIES
syn keyword gmtDefaults OBLIQUE_ANOTATION PAGE_COLOR PAGE_ORIENTATION
syn keyword gmtDefaults PAPER_MEDIA PSIMAGE_FORMAT TICK_LENGTH TICK_PEN
syn keyword gmtDefaults UNIX_TIME UNIX_TIME_POS VECTOR_SHAPE VERBOSE
syn keyword gmtDefaults WANT_EURO_FONT X_AXIS_LENGTH Y_AXIS_LENGTH
syn keyword gmtDefaults X_ORIGIN Y_ORIGIN XY_TOGGLE Y_AXIS_TYPE

" GMT commands, mainly those of /usr/local/GMT/bin, ;-)
syn keyword gmtFunction project nearneighbor fitcircle sample1d filter1d
syn keyword gmtFunction spectrum1d blockmean blockmedian blockmode
syn keyword gmtFunction triangulate makecpt trend1d trend2d grdtrend
syn keyword gmtGrid grdclip grdcontour grdcut grdedit grdfft grdfilter
syn keyword gmtGrid grdgradient grdhisteq grdimage grdinfo grdlandmask
syn keyword gmtGrid grdmask grdmath grdpaste grdproject grdraster
syn keyword gmtGrid grdreformat grdsample grdtrack grdvector
syn keyword gmtGrid grdview grdvolume
syn keyword gmtPlot psbasemap psclip pscoast pscontour pscoupe
syn keyword gmtPlot pshistogram psimage psmask psmeca psmegaplot
syn keyword gmtPlot pspolar psrose psscale pssegy pssegyz pstext
syn keyword gmtPlot psvelo pswiggle psxy psxyz surface
syn keyword gmtSet gmtdefaults gmtset gmtpath
syn keyword gmtSetTo gmtinfo gmtlegs gmtlist gmtmath gmtselect gmttrack
syn keyword gmtText pstext
syn keyword gmtTo dat2gmt gmt2bin gmt2dat grd2cpt grd2xyz img2grd
syn keyword gmtTo img2grd img2mercgrd mgd77togmt xyz2grd gmtconvert

" Little suport for sh, tcsh, bash. (from sh.vim)
syn keyword gmtConditional if else fi then end
syn keyword gmtRepeat for do done while 
syn keyword gmtSh cd eval exec pwd chdir kill alias pwd
syn keyword gmtSh ls cp mv rm rmdir cat EOF BEGIN END echo
syn keyword gmtSh tail head paste cut
syn keyword gmtStatement break return continue

" set matchers, mainly for sh scripting.
syn region gmtMatcher matchgroup=Delimiter start="{" skip="\\\\\|\\[{}]" end="}"

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

 HiLink gmtComment Comment
 HiLink gmtConditional Conditional
 HiLink gmtDefaults Type
 HiLink gmtFiles Ignore
 HiLink gmtFunction Function
 HiLink gmtGrid Function
 HiLink gmtOptions Type
 HiLink gmtParameters Special
 HiLink gmtPlot Function
 HiLink gmtRedir Statement
 HiLink gmtRepeat Repeat
 HiLink gmtSet Function
 HiLink gmtSetTo Function
 HiLink gmtSh Statement
 HiLink gmtStatement Statement
 HiLink gmtString String
 HiLink gmtText Function
 HiLink gmtTo Function

 delcommand HiLink
endif

let b:current_syntax = "gmt"
