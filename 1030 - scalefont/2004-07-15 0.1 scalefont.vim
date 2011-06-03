" vim: ff=unix
" scalefont.vim -- Change the font size while maintaining the window geometry
" @Author:      Thomas Link (samul AT web.de)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     18-Mai-2004.
" @Last Change: 27-Jun-2004.
" @Revision:    0.1.177
" 
if &cp || exists("s:loaded_scalefont")
    finish
endif
let s:loaded_scalefont = 1

" if has("unix")
    " if !exists("g:scaleFontSize")
        " let g:scaleFontSize = 9
    " endif
    " if !exists("g:scaleFont")
        " let g:scaleFont     = "-misc-fixed-medium-r-semicondensed-*-#{SIZE}-120-*-*-c-*-iso8859-15"
    " endif
" elseif has("win16") || has("win32")
    " if !exists("g:scaleFontSize")
        " let g:scaleFontSize = 11
    " endif
    " if !exists("g:scaleFont")
        " let g:scaleFont     = "Lucida_Console:h#{SIZE}:cANSI""
    " endif
" else
if !exists("g:scaleFontSize") || !exists("g:scaleFont")
    echomsg "scalefont: Please set g:scaleFontSize and g:scaleFont"
    finish
endif

if !exists("g:scaleFontWidth")
    let g:scaleFontWidth = g:scaleFontSize
endif

let g:scaleFontSize_0      = g:scaleFontSize
let g:scaleFontWidth_0     = g:scaleFontWidth
let g:scaleFont_0          = g:scaleFont

let g:scaleFontSize_Large  = g:scaleFontSize  + 6
let g:scaleFontWidth_Large = g:scaleFontWidth + 5
let g:scaleFont_Large      = g:scaleFont

let g:scaleFontSize_large  = g:scaleFontSize  + 4
let g:scaleFontWidth_large = g:scaleFontWidth + 3
let g:scaleFont_large      = g:scaleFont

let g:scaleFontSize_big    = g:scaleFontSize  + 2
let g:scaleFontWidth_big   = g:scaleFontWidth + 1
let g:scaleFont_big        = g:scaleFont

let g:scaleFontSize_small  = g:scaleFontSize  - 2
let g:scaleFontWidth_small = g:scaleFontWidth - 1
let g:scaleFont_small      = g:scaleFont

let g:scaleFontSize_tiny   = g:scaleFontSize  - 4
let g:scaleFontWidth_tiny  = g:scaleFontWidth - 3
let g:scaleFont_tiny       = g:scaleFont

let s:scaleFontCols        = 0
let s:scaleFontLines       = 0
let s:scaleFontScaledSize  = 0
let s:scaleFontScaledWidth = 0

fun! ScaleFontSetLinesCols(setIt, lines, cols)
    let s:scaleFontCols  = a:cols  *  g:scaleFontWidth
    let s:scaleFontLines = a:lines * (g:scaleFontSize + &linespace)
    if a:setIt
        let &lines = a:lines
        let &co    = a:cols
    endif
endfun

command! -nargs=* ScaleFontSetLinesCols call ScaleFontSetLinesCols(1, <f-args>)

fun! ScaleFontSetSize(size, width, ...)
    let g:scaleFontSize  = a:size
    let g:scaleFontWidth = a:width
    let fnt = a:0 >= 1 ? a:1 : g:scaleFont
    let fnt = substitute(fnt, '\V#{SIZE}',  g:scaleFontSize,  "g")
    let fnt = substitute(fnt, '\V#{WIDTH}', g:scaleFontWidth, "g")
    
    let s:scaleFontScaledSize  = 0
    let s:scaleFontScaledWidth = 0
    
    if s:scaleFontCols == 0
        call ScaleFontSetLinesCols(0, &lines, &co)
    endif
    let nco  = s:scaleFontCols / a:width
    let nfsl = a:size + &linespace
    let nli  = s:scaleFontLines / nfsl
    if (s:scaleFontLines % nfsl) > 5
        let nli = nli + 1
    endif
    exe "set co=". nco
    exe "set lines=". nli
    exe "set guifont=".fnt
endfun

command! -nargs=1 ScaleFontMode 
            \ call ScaleFontSetSize(g:scaleFontSize_{<q-args>}, 
            \ g:scaleFontWidth_{<q-args>}, g:scaleFont_{<q-args>})

fun! ScaleFont(delta)
    if s:scaleFontScaledSize == 0 || s:scaleFontScaledWidth == 0
        let nfs = (g:scaleFontSize  + a:delta) * 100
        let nfw = (g:scaleFontWidth + a:delta) * 100
    else
        let nfs = s:scaleFontScaledSize + (a:delta * 100)
        let r   = 100 * s:scaleFontScaledWidth / s:scaleFontScaledSize
        let nfw = s:scaleFontScaledWidth + (a:delta * r)
    endif
    call ScaleFontSetSize(nfs/100, nfw/100)
    let s:scaleFontScaledSize  = nfs
    let s:scaleFontScaledWidth = nfw
    echomsg "Font: ". g:scaleFontWidth ."x". g:scaleFontSize 
                \ .", window: ". &co ."x". &lines
endfun

command! ScaleFontBigger  :call ScaleFont(1)
command! ScaleFontSmaller :call ScaleFont(-1)

if !exists("g:scaleFontDontSetOnStartup")
    call ScaleFontSetSize(g:scaleFontSize, g:scaleFontWidth)
endif

finish


*scalefont.txt* Change/Set the Font (Size) While Trying to Maintain Window Geometry
Thomas Link

Sometimes you want to change the font size without modifying the window 
geometry. This plugin tries to set the 'guifont', 'columns', and 'lines' to 
appropriate values for achieving this goal. You have to set g:scaleFontSize and 
g:scaleFont before using this plugin.

Commands~
:ScaleFontBigger
:ScaleFontSmaller
:ScaleFontMode MODE
:ScaleFontSetLinesCols LINES COLS

Functions~
ScaleFont(delta)
ScaleFontSetSize(size, width)
ScaleFontSetLinesCols(setIt, lines, cols)

Variables~
g:scaleFontSize
g:scaleFontWidth
g:scaleFont

Modes~
For each mode, you have to define these variables:
  - g:scaleFontSize_{MODE}
  - g:scaleFontWidth_{MODE}
  - g:scaleFont_{MODE}

Predefined modes (g:scaleFontWidth_{MODE} might require some modifications):
  0     :: standard mode
  large :: +4
  big   :: +2
  small :: -2
  tiny  :: -4

Install~
Place this file into your personal plugin directory. Set g:scaleFontSize and 
g:scaleFont according to your likings. "#{SIZE}" and "#{WIDTH}" will be 
replaced with the appropriate values. Examples:

Unix: >
  let g:scaleFont = "-misc-fixed-medium-r-semicondensed-*-#{SIZE}-120-*-*-c-*-iso8859-15"

Windows: >
  let g:scaleFont = "Lucida_Console:h#{SIZE}:cANSI""

You could also add something like this to your vimrc file: >

  amenu &View.-SepViewScaleFont- :
  amenu &View.Font\ Size:\ &+ :ScaleFontBigger<cr>
  amenu &View.Font\ Size:\ &- :ScaleFontSmaller<cr>

Change History~
  0.1 :: Initial release

