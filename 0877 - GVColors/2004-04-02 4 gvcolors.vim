" gvcolors.vim: fire up gvim on this file, then :so %
"
"   Authors: Charles Campbell <NdrOchipS@PcampbellAfamily.Mbiz> - NOSPAM
"   		 mosh at http://www.cs.albany.edu/~mosh  	
"   Date:	 Apr 02, 2004
"   Version:	4
"
" It will display all the colors available under X by setting up colors for
" just those named colors showing on the display (up to 192 of them).
" Pressing <shift-leftmouse> will toggle between dark and light colored
" background.
"
" If you attempt to show more than 192 colors, the additional colors will be
" set to Ignore highlighting.

" ---------------------------------------------------------------------
if !has("gui_running")
 echoerr "needs to run under gvim"
 finish
endif
"call Decho("setting up debugging window")

if !exists("g:rgbtxtfile")
 let g:rgbtxtfile= "/usr/X11R6/lib/X11/rgb.txt"
 if !filereadable(g:rgbtxtfile) && has("win32")
  let g:rgbtxtfile= "c:/cygwin/usr/X11R6/lib/X11/rgb.txt"
 endif
endif

if filereadable(g:rgbtxtfile)
 let g:do_rgb_stl= 1
 let g:color1  = ""
 let g:color1r = ""
 let g:color1g = ""
 let g:color1b = ""
 let g:color2  = ""
 let g:color2r = ""
 let g:color2g = ""
 let g:color2b = ""
 let g:color3  = ""
 let g:color3r = ""
 let g:color3g = ""
 let g:color3b = ""

 " LookUpColor:
 fun! s:LookUpColor(color,nmbr)
   set lz
   exe "silent! vsp ".g:rgbtxtfile
   1
   norm! 0
   setlocal hidden
   let g:color{a:nmbr}  = a:color
   let g:color{a:nmbr}r = "U"
   let g:color{a:nmbr}g = "U"
   let g:color{a:nmbr}b = "U"
   let srch             = search('\<'.a:color.'\>',"wW")
   if srch
    let colorline        = getline(".")
    let colorpat1        = '^\s*\(\d\+\).*$'
    let colorpat2        = '^\s*\(\d\+\)\s*\(\d\+\).*$'
    let colorpat3        = '^\s*\(\d\+\)\s*\(\d\+\)\s*\(\d\+\).*$'
    let g:color{a:nmbr}r = substitute(colorline,colorpat1,'\1','e')
    let g:color{a:nmbr}g = substitute(colorline,colorpat2,'\2','e')
    let g:color{a:nmbr}b = substitute(colorline,colorpat3,'\3','e')
"    call Decho("LookUpColor<".g:rgbtxtfile."> color<".a:color."> nmbr=".a:nmbr.": rgb=".g:color{a:nmbr}r.",".g:color{a:nmbr}g.",".g:color{a:nmbr}b)
   else
"    call Decho("LookUpColor<".g:rgbtxtfile."> color<".a:color."> nmbr=".a:nmbr.": search failed with ".srch)
   endif
   q
   set nolz
 endfun
endif

" DisplayColors: display a page of colors
fun! DisplayColors(linechg)
  let oldtopline= g:topline
  let g:topline = g:topline + a:linechg

  " sanity check
  if g:topline < g:firstline
   let g:topline= g:firstline
  elseif g:topline > line("$")
   let g:topline= line("$")
  endif
"  call Decho("topline=[".oldtopline."]+[linechg=".a:linechg."]=".g:topline." firstline=".g:firstline." lines/page=".g:linesperpage)

  hi  clear
  syn clear
  syn off
  syn on
  if &bg == "dark"
   hi Normal guibg=black
  else
   hi Normal guibg=white
  endif
  syn match CommentStart '^" '
  hi link CommentStart Ignore
  exe "norm! ".g:topline."G0"

  if exists("g:do_rgb_stl")
   let pat1       = '^"\s*\(\w\+\).*$'
   let pat2       = '^"\s*\(\w\+\)\s\+\(\w\+\).*$'
   let pat3       = '^"\s*\(\w\+\)\s\+\(\w\+\)\s\+\(\w\+\).*$'
   let pat4       = '^"\s*\(\w\+\)\s\+\(\w\+\)\s\+\(\w\+\)\s\+\(\w\+\)'
   let curline    = getline(".")
   let colorname1 = substitute(curline,pat1,'\1','e')
   let colorname2 = substitute(curline,pat2,'\2','e')
   let colorname3 = substitute(curline,pat3,'\3','e')
   let colorname4 = substitute(curline,pat4,'\4','e')
"   call Decho("do_rgb_stl=".g:do_rgb_stl.": c1<".colorname1."> c2<".colorname2."> c3<".colorname3."> c4<".colorname4.">")
   call s:LookUpColor(colorname1,"1")
   call s:LookUpColor(colorname2,"2")
   call s:LookUpColor(colorname3,"3")
   call s:LookUpColor(colorname4,"4")
   if g:color4r != 'U'
    set stl=\ \ %-16(%{g:color1r}:%{g:color1g}:%{g:color1b}%)\ %-21(%{g:color2r}:%{g:color2g}:%{g:color2b}%)\ %-13(%{g:color3r}:%{g:color3g}:%{g:color3b}%)\ %{g:color4r}:%{g:color4g}:%{g:color4b}
   elseif g:color3r != 'U'
    set stl=\ \ %-16(%{g:color1r}:%{g:color1g}:%{g:color1b}%)\ %-21(%{g:color2r}:%{g:color2g}:%{g:color2b}%)\ %-13(%{g:color3r}:%{g:color3g}:%{g:color3b}%)
   elseif g:color2r != 'U'
    set stl=\ \ %-16(%{g:color1r}:%{g:color1g}:%{g:color1b}%)\ %-21(%{g:color2r}:%{g:color2g}:%{g:color2b}%)
   else
    set stl=\ \ %-16(%{g:color1r}:%{g:color1g}:%{g:color1b}%)
   endif
  endif

  let wrdcnt= 0
  while search('\w\+','W') > 0
   if wrdcnt < 192
    exec 'hi col_'.expand("<cword>").' gui=NONE guifg='.expand("<cword>")
    exec 'syn keyword col_'.expand("<cword>")." ".expand("<cword>")
   else
    exec 'hi link col_'.expand("<cword>").' Ignore'
    exec 'syn keyword col_'.expand("<cword>")." ".expand("<cword>")
   endif
   let wrdcnt= wrdcnt + 1
   if wrdcnt%4 == 0 && (wrdcnt/4)%g:linesperpage == 0
    break
   endif
  endwhile

"  call Decho("displayed ".wrdcnt." colors")
  exe "norm! ".g:topline."G0z\<cr>"
endfun

" ---------------------------------------------------------------------
" CurHoldFix:
fun! CurHoldFix()
  let curline        = line(".")
  let s:chg          = line(".") - (winline() - 1) - g:topline
  norm! 1GL
  let g:linesperpage = line(".")
  exe curline
  exe "norm! z\<cr>"
  if line(".") > g:firstline
   call DisplayColors(s:chg)
  else
   let g:topline=g:firstline
   call DisplayColors(0)
  endif
endfun

" ---------------------------------------------------------------------

" Preparation
set ft=
norm! 1GL
let g:linesperpage = line(".")
let wrdcnt       = 0
norm! 1G
call search('^" --Start Colors--$')
let g:firstline = line(".") + 1
let g:topline   = g:firstline
"set nomodifiable
call DisplayColors(0)

" Maps:
nmap <buffer> <silent> <down>     j
nmap <buffer> <silent> <up>       k
nmap <buffer> <silent> <pageup>   <c-b>
nmap <buffer> <silent> <pagedown> <c-f>
nn   <buffer> <silent> j          :call DisplayColors(1)<cr>
nn   <buffer> <silent> k          :call DisplayColors(-1)<cr>
nn   <buffer> <silent> <c-d>      :call DisplayColors(&scroll)<cr>
nn   <buffer> <silent> <c-u>      :call DisplayColors(-&scroll)<cr>
nn   <buffer> <silent> <c-f>      :call DisplayColors(g:linesperpage)<cr>
nn   <buffer> <silent> <c-b>      :call DisplayColors(-g:linesperpage)<cr>
nn   <buffer> <silent> G          :let g:topline= line("$")-g:linesperpage+1<cr>:call DisplayColors(0)<cr>
nn   <buffer> <silent> gg         :let g:topline= g:firstline<cr>:call DisplayColors(0)<cr>
nn   <buffer> <silent> <c-l>      H0:let g:topline= line(".")<cr>:call DisplayColors(0)<cr>
nn   <buffer> <silent> <s-leftmouse> :call <SID>ToggleBackground()<cr>

" ---------------------------------------------------------------------
" ToggleBackground: changes background on a shift-leftmouse between
" dark and light (black and white)
fun! s:ToggleBackground()
  if &bg == "dark"
    hi Normal guibg=white
    let &bg="light"
  else
    hi Normal guibg=black
    let &bg="dark"
  endif
  hi  clear
  syn clear
  syn off
  syn on
endfun
" ---------------------------------------------------------------------

" Events:
au CursorHold,FocusGained * set lz|call CurHoldFix()|set nolz
let &updatetime= 1000

" --Start Colors--
" AliceBlue        HotPink4              peru          gray39
" AntiqueWhite     IndianRed             pink          gray40
" AntiqueWhite1    IndianRed1            pink1         gray41
" AntiqueWhite2    IndianRed2            pink2         gray42
" AntiqueWhite3    IndianRed3            pink3         gray43
" AntiqueWhite4    IndianRed4            pink4         gray44
" aquamarine       ivory                 plum          gray45
" aquamarine1      ivory1                plum1         gray46
" aquamarine2      ivory2                plum2         gray47
" aquamarine3      ivory3                plum3         gray48
" aquamarine4      ivory4                plum4         gray49
" azure            khaki                 PowderBlue    gray50
" azure1           khaki1                purple        gray51
" azure2           khaki2                purple1       gray52
" azure3           khaki3                purple2       gray53
" azure4           khaki4                purple3       gray54
" beige            lavender              purple4       gray55
" bisque           LavenderBlush         red           gray56
" bisque1          LavenderBlush1        red1          gray57
" bisque2          LavenderBlush2        red2          gray58
" bisque3          LavenderBlush3        red3          gray59
" bisque4          LavenderBlush4        red4          gray60
" black            LawnGreen             RosyBrown     gray61
" BlanchedAlmond   LemonChiffon          RosyBrown1    gray62
" blue             LemonChiffon1         RosyBrown2    gray63
" blue1            LemonChiffon2         RosyBrown3    gray64
" blue2            LemonChiffon3         RosyBrown4    gray65
" blue3            LemonChiffon4         RoyalBlue     gray66
" blue4            LightBlue             RoyalBlue1    gray67
" BlueViolet       LightBlue1            RoyalBlue2    gray68
" brown            LightBlue2            RoyalBlue3    gray69
" brown1           LightBlue3            RoyalBlue4    gray70
" brown2           LightBlue4            SaddleBrown   gray71
" brown3           LightCoral            salmon        gray72
" brown4           LightCyan             salmon1       gray73
" burlywood        LightCyan1            salmon2       gray74
" burlywood1       LightCyan2            salmon3       gray75
" burlywood2       LightCyan3            salmon4       gray76
" burlywood3       LightCyan4            SandyBrown    gray77
" burlywood4       LightGoldenrod        SeaGreen      gray78
" CadetBlue        LightGoldenrod1       SeaGreen1     gray79
" CadetBlue1       LightGoldenrod2       SeaGreen2     gray80
" CadetBlue2       LightGoldenrod3       SeaGreen3     gray81
" CadetBlue3       LightGoldenrod4       SeaGreen4     gray82
" CadetBlue4       LightGoldenrodYellow  seashell      gray83
" chartreuse       LightGray             seashell1     gray84
" chartreuse1      LightGreen            seashell2     gray85
" chartreuse2      LightGrey             seashell3     gray86
" chartreuse3      LightPink             seashell4     gray87
" chartreuse4      LightPink1            sienna        gray88
" chocolate        LightPink2            sienna1       gray89
" chocolate1       LightPink3            sienna2       gray90
" chocolate2       LightPink4            sienna3       gray91
" chocolate3       LightSalmon           sienna4       gray92
" chocolate4       LightSalmon1          SkyBlue       gray93
" coral            LightSalmon2          SkyBlue1      gray94
" coral1           LightSalmon3          SkyBlue2      gray95
" coral2           LightSalmon4          SkyBlue3      gray96
" coral3           LightSeaGreen         SkyBlue4      gray97
" coral4           LightSkyBlue          SlateBlue     gray98
" CornflowerBlue   LightSkyBlue1         SlateBlue1    gray99
" cornsilk         LightSkyBlue2         SlateBlue2    gray100
" cornsilk1        LightSkyBlue3         SlateBlue3    grey
" cornsilk2        LightSkyBlue4         SlateBlue4    grey0
" cornsilk3        LightSlateBlue        SlateGray     grey1
" cornsilk4        LightSlateGray        SlateGray1    grey2
" cyan             LightSlateGrey        SlateGray2    grey3
" cyan1            LightSteelBlue        SlateGray3    grey4
" cyan2            LightSteelBlue1       SlateGray4    grey5
" cyan3            LightSteelBlue2       SlateGrey     grey6
" cyan4            LightSteelBlue3       snow          grey7
" DarkBlue         LightSteelBlue4       snow1         grey8
" DarkCyan         LightYellow           snow2         grey9
" DarkGoldenrod    LightYellow1          snow3         grey10
" DarkGoldenrod1   LightYellow2          snow4         grey11
" DarkGoldenrod2   LightYellow3          SpringGreen   grey12
" DarkGoldenrod3   LightYellow4          SpringGreen1  grey13
" DarkGoldenrod4   LimeGreen             SpringGreen2  grey14
" DarkGray         linen                 SpringGreen3  grey15
" DarkGreen        magenta               SpringGreen4  grey16
" DarkGrey         magenta1              SteelBlue     grey17
" DarkKhaki        magenta2              SteelBlue1    grey18
" DarkMagenta      magenta3              SteelBlue2    grey19
" DarkOliveGreen   magenta4              SteelBlue3    grey20
" DarkOliveGreen1  maroon                SteelBlue4    grey21
" DarkOliveGreen2  maroon1               tan           grey22
" DarkOliveGreen3  maroon2               tan1          grey23
" DarkOliveGreen4  maroon3               tan2          grey24
" DarkOrange       maroon4               tan3          grey25
" DarkOrange1      MediumAquamarine      tan4          grey26
" DarkOrange2      MediumBlue            thistle       grey27
" DarkOrange3      MediumOrchid          thistle1      grey28
" DarkOrange4      MediumOrchid1         thistle2      grey29
" DarkOrchid       MediumOrchid2         thistle3      grey30
" DarkOrchid1      MediumOrchid3         thistle4      grey31
" DarkOrchid2      MediumOrchid4         tomato        grey32
" DarkOrchid3      MediumPurple          tomato1       grey33
" DarkOrchid4      MediumPurple1         tomato2       grey34
" DarkRed          MediumPurple2         tomato3       grey35
" DarkSalmon       MediumPurple3         tomato4       grey36
" DarkSeaGreen     MediumPurple4         turquoise     grey37
" DarkSeaGreen1    MediumSeaGreen        turquoise1    grey38
" DarkSeaGreen2    MediumSlateBlue       turquoise2    grey39
" DarkSeaGreen3    MediumSpringGreen     turquoise3    grey40
" DarkSeaGreen4    MediumTurquoise       turquoise4    grey41
" DarkSlateBlue    MediumVioletRed       violet        grey42
" DarkSlateGray    MidnightBlue          VioletRed     grey43
" DarkSlateGray1   MintCream             VioletRed1    grey44
" DarkSlateGray2   MistyRose             VioletRed2    grey45
" DarkSlateGray3   MistyRose1            VioletRed3    grey46
" DarkSlateGray4   MistyRose2            VioletRed4    grey47
" DarkSlateGrey    MistyRose3            wheat         grey48
" DarkTurquoise    MistyRose4            wheat1        grey49
" DarkViolet       moccasin              wheat2        grey50
" DeepPink         NavajoWhite           wheat3        grey51
" DeepPink1        NavajoWhite1          wheat4        grey52
" DeepPink2        NavajoWhite2          white         grey53
" DeepPink3        NavajoWhite3          WhiteSmoke    grey54
" DeepPink4        NavajoWhite4          yellow        grey55
" DeepSkyBlue      navy                  yellow1       grey56
" DeepSkyBlue1     NavyBlue              yellow2       grey57
" DeepSkyBlue2     OldLace               yellow3       grey58
" DeepSkyBlue3     OliveDrab             yellow4       grey59
" DeepSkyBlue4     OliveDrab1            YellowGreen   grey60
" DimGray          OliveDrab2            gray          grey61
" DimGrey          OliveDrab3            gray0         grey62
" DodgerBlue       OliveDrab4            gray1         grey63
" DodgerBlue1      orange                gray2         grey64
" DodgerBlue2      orange1               gray3         grey65
" DodgerBlue3      orange2               gray4         grey66
" DodgerBlue4      orange3               gray5         grey67
" firebrick        orange4               gray6         grey68
" firebrick1       OrangeRed             gray7         grey69
" firebrick2       OrangeRed1            gray8         grey70
" firebrick3       OrangeRed2            gray9         grey71
" firebrick4       OrangeRed3            gray10        grey72
" FloralWhite      OrangeRed4            gray11        grey73
" ForestGreen      orchid                gray12        grey74
" gainsboro        orchid1               gray13        grey75
" GhostWhite       orchid2               gray14        grey76
" gold             orchid3               gray15        grey77
" gold1            orchid4               gray16        grey78
" gold2            PaleGoldenrod         gray17        grey79
" gold3            PaleGreen             gray18        grey80
" gold4            PaleGreen1            gray19        grey81
" goldenrod        PaleGreen2            gray20        grey82
" goldenrod1       PaleGreen3            gray21        grey83
" goldenrod2       PaleGreen4            gray22        grey84
" goldenrod3       PaleTurquoise         gray23        grey85
" goldenrod4       PaleTurquoise1        gray24        grey86
" green            PaleTurquoise2        gray25        grey87
" green1           PaleTurquoise3        gray26        grey88
" green2           PaleTurquoise4        gray27        grey89
" green3           PaleVioletRed         gray28        grey90
" green4           PaleVioletRed1        gray29        grey91
" GreenYellow      PaleVioletRed2        gray30        grey92
" honeydew         PaleVioletRed3        gray31        grey93
" honeydew1        PaleVioletRed4        gray32        grey94
" honeydew2        PapayaWhip            gray33        grey95
" honeydew3        PeachPuff             gray34        grey96
" honeydew4        PeachPuff1            gray35        grey97
" HotPink          PeachPuff2            gray36        grey98
" HotPink1         PeachPuff3            gray37        grey99
" HotPink2         PeachPuff4            gray38        grey100
" HotPink3
