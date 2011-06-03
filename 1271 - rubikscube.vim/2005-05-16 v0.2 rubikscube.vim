"================================================================================
" Name:         rubikscube.vim
" Author:       Olivier Vermersch
" Version:      0.2
"--------------------------------------------------------------------------------
" Description:  a rubiks cube game
"--------------------------------------------------------------------------------
" Installation: Source the file in an empty buffer.
"               Use preferably the gui, for colors.
"               This should work with gvim version > 6.1 (apparently cursor()
"               fonction is not present on earlier versions)
"               Version used for development is 6.3.4
"
" **** SAVE EVERYTHING BEFORE DOING IT ****
"--------------------------------------------------------------------------------
" Commands:     
"       - u, d, l, r, f, b to turn up, down, left, right, front and back faces
"       clockwise (whatever cube orientation) 
"
"       - c, m, v to turm the three belts clockwise
"
"       - shift + letter for turning counterclockwise - ctrl + letter for half
"       turn 
"
"       - the whole cube can turn with the four arrows and Pgup/Pgdown keys
"
"       - sramble cube with s key, reset it with S
"--------------------------------------------------------------------------------
" TODO:
" - more configurable display
" - add some edit technique with a solver algorithm - in order to solve a real
"   cube :-) 
" - chrono + high score stuff 
" - support for 4x4 and 5x5 cubes 
" - write moves in letters when done
" - some optimization (seems to be slow on old or "slow networked display"
"   machines) 
" - etc.
"--------------------------------------------------------------------------------
" History: 
" v0.1: initial release
"
" v0.2:
" - scramble/reset key 
" - possibility to rotate the whole cube 
" - simplified display
" - now use the up/down/left/right/back/front convention for move keys
" - belt moves (c, m, v)
" - magenta color instead of lightred for orange face
" - use "normal!" instead of unset mappings for display
"
"================================================================================




"================================================================================
" Random numbers generation
"================================================================================
"
" We use a well known basic congruence algorithm giving pseudo random series:
" Xn = Xn-1 x A + B (mod M)
"
" where A=65539, B=0, M=2^31, and X0 odd. These values are those used by the
" long used "RANDU" generator of IBM 360. The period of serie is 2^29 (better
" algorithms exist but this simple one should be enough).
"
" The seed X0 is initialized with current time.
"

function! s:InitRandom()
  let s:X0 = localtime()
  if s:X0 % 2 == 0 
    let s:X0 = s:X0+1 
  endif
  let s:Xn = s:X0
endf


"
" return random int between 0 and maxValue-1
"
function! s:Random(maxValue)
  let s:Xn = s:Xn * 65539
  let s:Xn = s:Xn % 2147483648  " should be useless, vim uses 32 bits int

  " we want unsigned 32 bits
  if s:Xn <0 
    let s:Xn = s:Xn + 2147483648
  endif

  " vim soesn't handle float so we take the rest of the division by maxValue
  return s:Xn % a:maxValue
endf



"================================================================================
" Configuration of the cube
"================================================================================
"
" These variables are used for:
" - key mappings
" - "syntax highlighted" letter used for displaying colors
" - names of the facets variables containing the color
" - the value of the color
"
function! s:Init()
  set nowrap 
  call s:InitRandom()

  " colors 
  let s:blue   = "b"
  let s:white  = "w"
  let s:green  = "g"
  let s:yellow = "y"
  let s:red    = "r"
  let s:orange = "o"
  let s:black  = " "

  " mapping letters (english convention)
  let s:frontletter = "f"
  let s:backletter  = "b"
  let s:upletter    = "u"
  let s:downletter  = "d"
  let s:leftletter  = "l"
  let s:rightletter = "r"

  " mapping letters (french convention)
  "let s:frontletter = "a"
  "let s:backletter  = "p"
  "let s:upletter    = "h"
  "let s:downletter  = "i"
  "let s:leftletter  = "g"
  "let s:rightletter = "d"

  " 3 belts
  let s:belt1letter = "c"
  let s:belt2letter = "m"
  let s:belt3letter = "v"

  " configuration of the cube displayed
  let g:front = s:white
  let g:back  = s:yellow
  let g:up    = s:red
  let g:down  = s:orange
  let g:left  = s:blue
  let g:right = s:green

endf



"================================================================================
" Mappings 
"================================================================================
function! s:SetMappings ()
  let disp = "<BAR> call <SID>Display()<CR>"

  "
  " each of six moves
  "
  let cmd="(g:front, g:left, g:up, g:right, g:down)"
  exe "nmap <buffer>    ".s:frontletter. "  :call <SID>RotateFaceCw " .cmd.disp
  exe "nmap <buffer> <S-".s:frontletter. "> :call <SID>RotateFaceCcw ".cmd.disp
  exe "nmap <buffer> <C-".s:frontletter. "> :call <SID>RotateFace2 "  .cmd.disp

  let cmd="(g:back, g:left, g:down, g:right, g:up)"
  exe "nmap <buffer>    ".s:backletter.  "  :call <SID>RotateFaceCw " .cmd.disp
  exe "nmap <buffer> <S-".s:backletter.  "> :call <SID>RotateFaceCcw ".cmd.disp
  exe "nmap <buffer> <C-".s:backletter.  "> :call <SID>RotateFace2 "  .cmd.disp

  let cmd="(g:up, g:left, g:back, g:right, g:front)"
  exe "nmap <buffer>    ".s:upletter.    "  :call <SID>RotateFaceCw " .cmd.disp
  exe "nmap <buffer> <S-".s:upletter.    "> :call <SID>RotateFaceCcw ".cmd.disp
  exe "nmap <buffer> <C-".s:upletter.    "> :call <SID>RotateFace2 "  .cmd.disp

  let cmd="(g:down, g:left, g:front, g:right, g:back)"
  exe "nmap <buffer>    ".s:downletter.  "  :call <SID>RotateFaceCw " .cmd.disp
  exe "nmap <buffer> <S-".s:downletter.  "> :call <SID>RotateFaceCcw ".cmd.disp
  exe "nmap <buffer> <C-".s:downletter.  "> :call <SID>RotateFace2 "  .cmd.disp

  let cmd="(g:left, g:back, g:up, g:front, g:down)"
  exe "nmap <buffer>    ".s:leftletter.  "  :call <SID>RotateFaceCw " .cmd.disp
  exe "nmap <buffer> <S-".s:leftletter.  "> :call <SID>RotateFaceCcw ".cmd.disp
  exe "nmap <buffer> <C-".s:leftletter.  "> :call <SID>RotateFace2 "  .cmd.disp

  let cmd="(g:right, g:back, g:down, g:front, g:up)"
  exe "nmap <buffer>    ".s:rightletter. "  :call <SID>RotateFaceCw " .cmd.disp
  exe "nmap <buffer> <S-".s:rightletter. "> :call <SID>RotateFaceCcw ".cmd.disp
  exe "nmap <buffer> <C-".s:rightletter. "> :call <SID>RotateFace2 "  .cmd.disp

  "
  " three belts
  "
  let cmd = "(g:up, g:left, g:front, g:right, g:back, g:down)"
  exe "nmap <buffer>    ".s:belt1letter. "  :call <SID>RotateBeltCw  ".cmd." <BAR> call <SID>RotateCubeUp()".disp
  exe "nmap <buffer> <S-".s:belt1letter. "> :call <SID>RotateBeltCcw ".cmd." <BAR> call <SID>RotateCubeDown()".disp
  exe "nmap <buffer> <C-".s:belt1letter. "> :call <SID>RotateBelt2   ".cmd." <BAR> 1,2call <SID>RotateCubeDown()".disp

  let cmd = "(g:left, g:down, g:front, g:up, g:back, g:right)"
  exe "nmap <buffer>    ".s:belt2letter. "  :call <SID>RotateBeltCw  ".cmd." <BAR> call <SID>RotateCubeLeft()".disp
  exe "nmap <buffer> <S-".s:belt2letter. "> :call <SID>RotateBeltCcw ".cmd." <BAR> call <SID>RotateCubeRight()".disp
  exe "nmap <buffer> <C-".s:belt2letter. "> :call <SID>RotateBelt2   ".cmd." <BAR> 1,2call <SID>RotateCubeRight()".disp

  let cmd = "(g:up, g:front, g:right, g:back, g:left, g:down)"
  exe "nmap <buffer>    ".s:belt3letter. "  :call <SID>RotateBeltCw  ".cmd." <BAR> call <SID>RotateCubePgup()".disp
  exe "nmap <buffer> <S-".s:belt3letter. "> :call <SID>RotateBeltCcw ".cmd." <BAR> call <SID>RotateCubePgdwn()".disp
  exe "nmap <buffer> <C-".s:belt3letter. "> :call <SID>RotateBelt2   ".cmd." <BAR> 1,2call <SID>RotateCubePgdwn()".disp

  "
  " cube rotations
  "
  exe "nmap <buffer> <left>     :call <SID>RotateCubeLeft() ".disp
  exe "nmap <buffer> <right>    :call <SID>RotateCubeRight()".disp
  exe "nmap <buffer> <up>       :call <SID>RotateCubeUp()   ".disp
  exe "nmap <buffer> <down>     :call <SID>RotateCubeDown() ".disp
  exe "nmap <buffer> <PageUp>   :call <SID>RotateCubePgup() ".disp
  exe "nmap <buffer> <PageDown> :call <SID>RotateCubePgdwn()".disp

  "
  " scramble and reset
  "
  exe "nmap <buffer> s :call <SID>Scramble()<CR>"
  exe "nmap <buffer> S :call <SID>InitCube()".disp
endf


"================================================================================
" Scramble cube
"================================================================================
function! s:Scramble()
  let n=0
  while n<60
    let move = s:Random (6)

    if move == 0 | let l=s:frontletter | endif
    if move == 1 | let l=s:backletter  | endif
    if move == 2 | let l=s:upletter    | endif
    if move == 3 | let l=s:downletter  | endif
    if move == 4 | let l=s:leftletter  | endif
    if move == 5 | let l=s:rightletter | endif

    exe "normal ".l
    let n=n+1
  endw

endf


"================================================================================
" syntax highlighting stuff for displaying colored letters
"================================================================================
function! s:InitColors()
  hi Blue    ctermfg=blue       ctermbg=blue       guifg=blue     guibg=blue
  hi Yellow  ctermfg=yellow     ctermbg=yellow     guifg=yellow   guibg=yellow
  hi Orange  ctermfg=magenta    ctermbg=magenta    guifg=magenta  guibg=magenta
  hi Green   ctermfg=green      ctermbg=lightgreen guifg=green    guibg=green
  hi Red     ctermfg=red        ctermbg=red        guifg=red      guibg=red
  hi White   ctermfg=white      ctermbg=white      guifg=white    guibg=white
  hi Black   ctermfg=black      ctermbg=black      guifg=black    guibg=black

  exe "syn match Blue   /" . s:blue   ."/"     
  exe "syn match White  /" . s:white  ."/"      
  exe "syn match Green  /" . s:green  ."/"      
  exe "syn match Yellow /" . s:yellow ."/"       
  exe "syn match Red    /" . s:red    ."/"    
  exe "syn match Orange /" . s:orange ."/"       
  exe "syn match Black  /" . s:black  ."/"      
endf


"================================================================================
" facets variables
"================================================================================
"
" for example, s:oyg contains the color (at first: 'o') of the corner of the
" orange face, which is between green and yellow faces (variable name is
" chosen clockwise)
"
function! s:InitFace (face, left, up, right, down)
  let s:{a:face}{a:left}{a:up}    = a:face
  let s:{a:face}{a:up}1           = a:face
  let s:{a:face}{a:up}{a:right}   = a:face
  let s:{a:face}{a:right}1        = a:face
  let s:{a:face}{a:right}{a:down} = a:face
  let s:{a:face}{a:down}1         = a:face
  let s:{a:face}{a:down}{a:left}  = a:face
  let s:{a:face}{a:left}1         = a:face
endf


function! s:InitCube ()
  call s:InitFace (s:white,  s:blue,   s:red,   s:green,  s:orange) 
  call s:InitFace (s:yellow, s:green,  s:red,   s:blue,   s:orange) 
  call s:InitFace (s:blue,   s:yellow, s:red,   s:white,  s:orange) 
  call s:InitFace (s:green,  s:white,  s:red,   s:yellow, s:orange) 
  call s:InitFace (s:red,    s:white,  s:blue,  s:yellow, s:green) 
  call s:InitFace (s:orange, s:white,  s:green, s:yellow, s:blue) 
endf


"================================================================================
" Cube display functions
"================================================================================
function! s:InitDisplay()
  normal! ggdG50o
  call s:DisplayBlackBox (1,1, 105, 32)
endf


function! s:DisplayFrontFace (col, lig, face, left, up, right, down)

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}{a:up} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}{a:right}."l"
  let cmd = cmd."r".s:black

  call cursor (a:lig+0, a:col) | exe cmd
  call cursor (a:lig+1, a:col) | exe cmd


  let cmd = "normal! "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}1      ."l"
  let cmd = cmd."r".s:black."l4r".a:face                   ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}1     ."l"
  let cmd = cmd."r".s:black

  call cursor (a:lig+3, a:col) | exe cmd
  call cursor (a:lig+4, a:col) | exe cmd

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}{a:left} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}{a:down}."l"
  let cmd = cmd."r".s:black

  call cursor (a:lig+6, a:col) | exe cmd
  call cursor (a:lig+7, a:col) | exe cmd

endf


function! s:Display3dTop (col, lig, face, left, up, right, down)

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}{a:up} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}{a:right}."l"
  let cmd = cmd."r".s:black
  call cursor (a:lig+1, a:col+8) | exe cmd

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}1      ."l"
  let cmd = cmd."r".s:black."l4r".a:face                   ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}1     ."l"
  let cmd = cmd."r".s:black
  call cursor (a:lig+2, a:col+5) | exe cmd

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}{a:left} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}{a:down}."l"
  let cmd = cmd."r".s:black
  call cursor (a:lig+3, a:col+2) | exe cmd
endf

function! s:Display3dSide (col, lig, face, left, up, right, down)

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:left}{a:up} ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:up}1        ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:up}{a:right}."lk"
  let cmd = cmd."r".s:black
  call cursor (a:lig+4, a:col+15) | exe cmd
  call cursor (a:lig+5, a:col+15) | exe cmd

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:left}1      ."lk"
  let cmd = cmd."r".s:black."l2r".a:face                   ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:right}1     ."lk"
  let cmd = cmd."r".s:black
  call cursor (a:lig+7, a:col+15) | exe cmd
  call cursor (a:lig+8, a:col+15) | exe cmd

  let cmd = "normal! "
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:down}{a:left} ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:down}1        ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:right}{a:down}."lk"
  let cmd = cmd."r".s:black
  call cursor (a:lig+10, a:col+15) | exe cmd
  call cursor (a:lig+11, a:col+15) | exe cmd
endf



function! s:DisplayBurstView (col, lig, up, left, front, right, back, down)

  call s:DisplayFrontFace (a:col+22, a:lig+1, a:up, a:left, a:back, a:right, a:front)
  call s:DisplayFrontFace (a:col+3, a:lig+11, a:left, a:back, a:up, a:front, a:down)
  call s:DisplayFrontFace (a:col+22, a:lig+11, a:front, a:left, a:up, a:right, a:down)
  call s:DisplayFrontFace (a:col+41, a:lig+11, a:right, a:front, a:up, a:back, a:down)
  call s:DisplayFrontFace (a:col+60, a:lig+11, a:back, a:right, a:up, a:left, a:down)
  call s:DisplayFrontFace (a:col+22, a:lig+21, a:down, a:left, a:front, a:right, a:back)

endf


function! s:DisplayBlackBox (col, lig, width, height)

  let i=a:lig

  while i<a:lig+a:height
    call cursor (i, a:col)
    exe "normal! D".a:width."i".s:black
    normal! o
    let i=i + 1
  endw
endf


function! s:Display3dView (col, lig, up, left, front, right, back, down)

  call s:Display3dTop (a:col+1, a:lig, a:up, a:left, a:back, a:right, a:front)
  call s:Display3dSide (a:col+1, a:lig, a:right, a:front, a:up, a:back, a:down)
  call s:DisplayFrontFace (a:col+1, a:lig+4, a:front, a:left, a:up, a:right, a:down)

endf


function! s:Display()
  call s:Display3dView (1, 1, g:up, g:left, g:front, g:right, g:back, g:down)
  call s:Display3dView (1, 15, g:left, g:up, g:back, g:down, g:front, g:right)
  call s:DisplayBurstView (27, 1, g:up, g:left, g:front, g:right, g:back, g:down)
endf


"================================================================================
" face rotation functions
"================================================================================
function! s:RotateFaceCw (face, left, up, right, down)

  " corners front facet
  let tmp = s:{a:face}{a:left}{a:up}
  let s:{a:face}{a:left}{a:up}    = s:{a:face}{a:down}{a:left} 
  let s:{a:face}{a:down}{a:left}  = s:{a:face}{a:right}{a:down}
  let s:{a:face}{a:right}{a:down} = s:{a:face}{a:up}{a:right}  
  let s:{a:face}{a:up}{a:right}   = tmp

  " corners left facet
  let tmp = s:{a:left}{a:up}{a:face}
  let s:{a:left}{a:up}{a:face}    = s:{a:down}{a:left}{a:face} 
  let s:{a:down}{a:left}{a:face}  = s:{a:right}{a:down}{a:face}
  let s:{a:right}{a:down}{a:face} = s:{a:up}{a:right}{a:face}  
  let s:{a:up}{a:right}{a:face}   = tmp

  " corners up facet
  let tmp = s:{a:up}{a:face}{a:left}
  let s:{a:up}{a:face}{a:left}    = s:{a:left}{a:face}{a:down} 
  let s:{a:left}{a:face}{a:down}  = s:{a:down}{a:face}{a:right} 
  let s:{a:down}{a:face}{a:right} = s:{a:right}{a:face}{a:up} 
  let s:{a:right}{a:face}{a:up}   = tmp

  " edge front facets
  let tmp = s:{a:face}{a:left}1
  let s:{a:face}{a:left}1  = s:{a:face}{a:down}1 
  let s:{a:face}{a:down}1  = s:{a:face}{a:right}1
  let s:{a:face}{a:right}1 = s:{a:face}{a:up}1  
  let s:{a:face}{a:up}1    = tmp

  " edge up facets
  let tmp = s:{a:up}{a:face}1
  let s:{a:up}{a:face}1    = s:{a:left}{a:face}1 
  let s:{a:left}{a:face}1  = s:{a:down}{a:face}1 
  let s:{a:down}{a:face}1  = s:{a:right}{a:face}1 
  let s:{a:right}{a:face}1 = tmp
endf


function! s:RotateFaceCcw (face, left, up, right, down)

  " corners front facet
  let tmp = s:{a:face}{a:left}{a:up}
  let s:{a:face}{a:left}{a:up}    = s:{a:face}{a:up}{a:right} 
  let s:{a:face}{a:up}{a:right}   = s:{a:face}{a:right}{a:down}
  let s:{a:face}{a:right}{a:down} = s:{a:face}{a:down}{a:left}  
  let s:{a:face}{a:down}{a:left}  = tmp

  " corners left facet
  let tmp = s:{a:left}{a:up}{a:face}
  let s:{a:left}{a:up}{a:face}    = s:{a:up}{a:right}{a:face} 
  let s:{a:up}{a:right}{a:face}   = s:{a:right}{a:down}{a:face}
  let s:{a:right}{a:down}{a:face} = s:{a:down}{a:left}{a:face}  
  let s:{a:down}{a:left}{a:face}  = tmp

  " corners up facet
  let tmp = s:{a:up}{a:face}{a:left}
  let s:{a:up}{a:face}{a:left}    = s:{a:right}{a:face}{a:up} 
  let s:{a:right}{a:face}{a:up}   = s:{a:down}{a:face}{a:right} 
  let s:{a:down}{a:face}{a:right} = s:{a:left}{a:face}{a:down} 
  let s:{a:left}{a:face}{a:down}  = tmp

  " edge front facets
  let tmp = s:{a:face}{a:left}1
  let s:{a:face}{a:left}1  = s:{a:face}{a:up}1 
  let s:{a:face}{a:up}1  = s:{a:face}{a:right}1
  let s:{a:face}{a:right}1 = s:{a:face}{a:down}1  
  let s:{a:face}{a:down}1    = tmp

  " edge up facets
  let tmp = s:{a:up}{a:face}1
  let s:{a:up}{a:face}1    = s:{a:right}{a:face}1 
  let s:{a:right}{a:face}1  = s:{a:down}{a:face}1 
  let s:{a:down}{a:face}1  = s:{a:left}{a:face}1 
  let s:{a:left}{a:face}1 = tmp
endf


function! s:RotateFace2 (face, left, up, right, down)

  " corners front facet
  let tmp = s:{a:face}{a:left}{a:up}
  let s:{a:face}{a:left}{a:up}    = s:{a:face}{a:right}{a:down} 
  let s:{a:face}{a:right}{a:down} = tmp

  let tmp = s:{a:face}{a:up}{a:right}
  let s:{a:face}{a:up}{a:right}   = s:{a:face}{a:down}{a:left}
  let s:{a:face}{a:down}{a:left}  =  tmp

  " corners left facet
  let tmp = s:{a:left}{a:up}{a:face}
  let s:{a:left}{a:up}{a:face}    = s:{a:right}{a:down}{a:face} 
  let s:{a:right}{a:down}{a:face} = tmp

  let tmp = s:{a:up}{a:right}{a:face}
  let s:{a:up}{a:right}{a:face}   = s:{a:down}{a:left}{a:face}
  let s:{a:down}{a:left}{a:face}  = tmp

  " corners up facet
  let tmp = s:{a:up}{a:face}{a:left}
  let s:{a:up}{a:face}{a:left}    = s:{a:down}{a:face}{a:right} 
  let s:{a:down}{a:face}{a:right} = tmp

  let tmp = s:{a:left}{a:face}{a:down}
  let s:{a:left}{a:face}{a:down}  = s:{a:right}{a:face}{a:up} 
  let s:{a:right}{a:face}{a:up}   = tmp

  " edge front facets
  let tmp = s:{a:face}{a:left}1
  let s:{a:face}{a:left}1  = s:{a:face}{a:right}1 
  let s:{a:face}{a:right}1 = tmp

  let tmp = s:{a:face}{a:up}1
  let s:{a:face}{a:up}1   = s:{a:face}{a:down}1  
  let s:{a:face}{a:down}1 = tmp

  " edge up facets
  let tmp = s:{a:up}{a:face}1
  let s:{a:up}{a:face}1    = s:{a:down}{a:face}1 
  let s:{a:down}{a:face}1  = tmp

  let tmp = s:{a:left}{a:face}1
  let s:{a:left}{a:face}1  = s:{a:right}{a:face}1 
  let s:{a:right}{a:face}1 = tmp
endf


function! s:RotateBeltCw (up, left, front, right, back, down)
  call s:RotateFaceCw (a:left, a:back, a:up, a:front, a:down) 
  call s:RotateFaceCcw (a:right, a:back, a:down, a:front, a:up) 
endf

function! s:RotateBeltCcw (up, left, front, right, back, down)
  call s:RotateFaceCcw (a:left, a:back, a:up, a:front, a:down) 
  call s:RotateFaceCw (a:right, a:back, a:down, a:front, a:up) 
endf

function! s:RotateBelt2 (up, left, front, right, back, down)
  call s:RotateFace2 (a:left, a:back, a:up, a:front, a:down) 
  call s:RotateFace2 (a:right, a:back, a:down, a:front, a:up) 
endf




"================================================================================
" Rotate whole cube
"================================================================================
function! s:RotateCubeRight()
  let tmp = g:front
  let g:front = g:left
  let g:left  = g:back
  let g:back  = g:right
  let g:right = tmp
endf

function! s:RotateCubeLeft()
  let tmp = g:front
  let g:front = g:right
  let g:right = g:back
  let g:back  = g:left
  let g:left  = tmp
endf

function! s:RotateCubeUp()
  let tmp = g:front
  let g:front = g:down 
  let g:down  = g:back 
  let g:back  = g:up  
  let g:up    = tmp
endf

function! s:RotateCubeDown()
  let tmp = g:front
  let g:front = g:up 
  let g:up    = g:back  
  let g:back  = g:down  
  let g:down  = tmp
endf

function! s:RotateCubePgup()
  let tmp = g:right
  let g:right = g:down 
  let g:down  = g:left
  let g:left  = g:up  
  let g:up    = tmp
endf

function! s:RotateCubePgdwn()
  let tmp = g:right
  let g:right = g:up 
  let g:up    = g:left
  let g:left  = g:down  
  let g:down  = tmp
endf

"================================================================================
" Main 
"================================================================================
function! s:Main()
  call s:Init()
  call s:InitColors()
  call s:InitCube()
  call s:InitDisplay()

  call s:Display()
  call s:SetMappings()
endf

call s:Main()
