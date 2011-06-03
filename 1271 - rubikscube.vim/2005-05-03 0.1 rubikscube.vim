
function! s:Init()
  let s:blue   = "b"
  let s:white  = "w"
  let s:green  = "g"
  let s:yellow = "y"
  let s:red    = "r"
  let s:orange = "o"
  let s:black  = " "

  let s:front = s:white
  let s:back  = s:yellow
  let s:up    = s:red
  let s:down  = s:orange
  let s:left  = s:blue
  let s:right = s:green

endf



function! s:InitColors()
  hi Blue    ctermfg=blue       ctermbg=blue       guifg=blue     guibg=blue
  hi Yellow  ctermfg=yellow     ctermbg=yellow     guifg=yellow   guibg=yellow
  hi Orange  ctermfg=lightred   ctermbg=lightred   guifg=lightred guibg=lightred
  hi Green   ctermfg=green      ctermbg=lightgreen guifg=green    guibg=green
  hi Red     ctermfg=red        ctermbg=lightred   guifg=red      guibg=red
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


function! s:InitDisplay()
  call s:UnsetMappings()
  normal ggdG50o
  call s:DisplayBlackBox (1,1, 130, 44)
endf


function! s:DisplayFrontFace (col, lig, face, left, up, right, down)

  let cmd = "normal "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}{a:up} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}{a:right}."l"
  let cmd = cmd."r".s:black

  call cursor (a:lig+0, a:col) | exe cmd
  call cursor (a:lig+1, a:col) | exe cmd


  let cmd = "normal "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}1      ."l"
  let cmd = cmd."r".s:black."l4r".a:face                   ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}1     ."l"
  let cmd = cmd."r".s:black

  call cursor (a:lig+3, a:col) | exe cmd
  call cursor (a:lig+4, a:col) | exe cmd

  let cmd = "normal "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}{a:left} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}{a:down}."l"
  let cmd = cmd."r".s:black

  call cursor (a:lig+6, a:col) | exe cmd
  call cursor (a:lig+7, a:col) | exe cmd

endf


function! s:Display3dTop (col, lig, face, left, up, right, down)

  let cmd = "normal "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}{a:up} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:up}{a:right}."l"
  let cmd = cmd."r".s:black
  call cursor (a:lig+1, a:col+8) | exe cmd

  let cmd = "normal "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:left}1      ."l"
  let cmd = cmd."r".s:black."l4r".a:face                   ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}1     ."l"
  let cmd = cmd."r".s:black
  call cursor (a:lig+2, a:col+5) | exe cmd

  let cmd = "normal "
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}{a:left} ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:down}1        ."l"
  let cmd = cmd."r".s:black."l4r".s:{a:face}{a:right}{a:down}."l"
  let cmd = cmd."r".s:black
  call cursor (a:lig+3, a:col+2) | exe cmd
endf

function! s:Display3dSide (col, lig, face, left, up, right, down)

  let cmd = "normal "
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:left}{a:up} ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:up}1        ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:up}{a:right}."lk"
  let cmd = cmd."r".s:black
  call cursor (a:lig+4, a:col+15) | exe cmd
  call cursor (a:lig+5, a:col+15) | exe cmd

  let cmd = "normal "
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:left}1      ."lk"
  let cmd = cmd."r".s:black."l2r".a:face                   ."lk"
  let cmd = cmd."r".s:black."l2r".s:{a:face}{a:right}1     ."lk"
  let cmd = cmd."r".s:black
  call cursor (a:lig+7, a:col+15) | exe cmd
  call cursor (a:lig+8, a:col+15) | exe cmd

  let cmd = "normal "
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
    exe "normal D".a:width."i".s:black
    normal o
    let i=i + 1
  endw
endf


function! s:Display3dView (col, lig, up, left, front, right, back, down)

  call s:Display3dTop (a:col+1, a:lig, a:up, a:left, a:back, a:right, a:front)
  call s:Display3dSide (a:col+1, a:lig, a:right, a:front, a:up, a:back, a:down)
  call s:DisplayFrontFace (a:col+1, a:lig+4, a:front, a:left, a:up, a:right, a:down)

endf


function! s:Display()
  call s:UnsetMappings()
  call s:DisplayBurstView (27, 15, s:up, s:left, s:front, s:right, s:back, s:down)

  call s:Display3dView (1, 1, s:up, s:back, s:left, s:front, s:right, s:down)
  call s:Display3dView (36, 1, s:up, s:left, s:front, s:right, s:back, s:down)
  call s:Display3dView (68, 1, s:up, s:front, s:right, s:back, s:left, s:down)
  call s:Display3dView (104, 1, s:up, s:right, s:back, s:left, s:front, s:down)

  call s:Display3dView (1, 15, s:down, s:front, s:left, s:back, s:right, s:up)
  call s:Display3dView (1, 30, s:down, s:left, s:back, s:right, s:front, s:up)

  call s:Display3dView (104, 15, s:down, s:back, s:right, s:front, s:left, s:up)
  call s:Display3dView (104, 30, s:down, s:right, s:front, s:left, s:back, s:up)

  call s:SetMappings()
endf


function! s:Rotate (face, left, up, right, down)

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


function! s:SetMappings ()
  exe "nmap <buffer> ".s:white. " :call <SID>RotateWhite(1)<CR>"
  exe "nmap <buffer> ".s:yellow." :call <SID>RotateYellow(1)<CR>"
  exe "nmap <buffer> ".s:red.   " :call <SID>RotateRed(1)<CR>"
  exe "nmap <buffer> ".s:orange." :call <SID>RotateOrange(1)<CR>"
  exe "nmap <buffer> ".s:blue.  " :call <SID>RotateBlue(1)<CR>"
  exe "nmap <buffer> ".s:green. " :call <SID>RotateGreen(1)<CR>"

  exe "nmap <buffer> <S-".s:white. "> :call <SID>RotateWhite(3)<CR>"
  exe "nmap <buffer> <S-".s:yellow."> :call <SID>RotateYellow(3)<CR>"
  exe "nmap <buffer> <S-".s:red.   "> :call <SID>RotateRed(3)<CR>"
  exe "nmap <buffer> <S-".s:orange."> :call <SID>RotateOrange(3)<CR>"
  exe "nmap <buffer> <S-".s:blue.  "> :call <SID>RotateBlue(3)<CR>"
  exe "nmap <buffer> <S-".s:green. "> :call <SID>RotateGreen(3)<CR>"

  exe "nmap <buffer> <C-".s:white. "> :call <SID>RotateWhite(2)<CR>"
  exe "nmap <buffer> <C-".s:yellow."> :call <SID>RotateYellow(2)<CR>"
  exe "nmap <buffer> <C-".s:red.   "> :call <SID>RotateRed(2)<CR>"
  exe "nmap <buffer> <C-".s:orange."> :call <SID>RotateOrange(2)<CR>"
  exe "nmap <buffer> <C-".s:blue.  "> :call <SID>RotateBlue(2)<CR>"
  exe "nmap <buffer> <C-".s:green. "> :call <SID>RotateGreen(2)<CR>"
endf


function! s:UnsetMappings ()
  mapclear <buffer>
endf



function! s:RotateWhite(nb)
  let i=0 | while i<a:nb
    call s:Rotate (s:white, s:blue, s:red, s:green, s:orange) 
    let i = i+1
  endw

  call s:Display()
endf


function! s:RotateYellow(nb)
  let i=0 | while i<a:nb
  call s:Rotate (s:yellow, s:blue, s:orange, s:green, s:red) 
    let i = i+1
  endw

  call s:Display()
endf


function! s:RotateRed(nb)
  let i=0 | while i<a:nb
  call s:Rotate (s:red, s:yellow, s:green, s:white, s:blue)
    let i = i+1
  endw

  call s:Display()
endf


function! s:RotateOrange(nb)
  let i=0 | while i<a:nb
  call s:Rotate (s:orange, s:yellow, s:blue, s:white, s:green)
    let i = i+1
  endw

  call s:Display()
endf


function! s:RotateBlue(nb)
  let i=0 | while i<a:nb
  call s:Rotate (s:blue, s:yellow, s:red, s:white, s:orange)
    let i = i+1
  endw

  call s:Display()
endf


function! s:RotateGreen(nb)
  let i=0 | while i<a:nb
  call s:Rotate (s:green, s:yellow, s:orange, s:white, s:red)
    let i = i+1
  endw

  call s:Display()
endf


function! s:Main()
  call s:Init()
  call s:InitColors()
  call s:InitCube()
  call s:InitDisplay()

  call s:Display()
  "call s:SetMappings()
endf

call s:Main()
