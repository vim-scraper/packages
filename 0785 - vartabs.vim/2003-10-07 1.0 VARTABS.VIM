" VARTABS.VIM: (global plugin) Tabstops at arbitrary positions
" Last Change:	2003-10-07_17:57:31
" Maintainer:	Michael Fitz   <mfitz@aon.at>
" Version:	1.0
"
" Installation:
"   Just copy into the 'plugin'-folder
"
" Usage:
"   In a filetype-plugin enter this line:
"   :call VarTab_SetStops("a,b,c,...,n")
"   where a,b,... are ascending tabstop-positions you want
"    (eg. when dealing with /370-Assembler: "10,16,71"
"   
" Function:
"   Maps <TAB> and <S-TAB> to jump to the specified positions

let s:Pos_Count=0

:function! VarTab_SetStops(stoplist)
" Clearing old Positions
if (s:Pos_Count<0)
  let ii=-s:Pos_Count
else
  let ii=s:Pos_Count
endif
while(ii>1)
  unlet! s:Pos_{ii}
  let ii=ii-1 
endw
let s:Pos_Count=0
" Making a new Poslist
let z=a:stoplist.','  "will make parsing easier
let cnt=0 "Counter
let vg1=0
while(z!="")
  " echo z
  let i=matchend(z,'\s*\d\+\s*,')
  if(i<0)
    break
  endif
  let cnt=cnt+1
  let z1=strpart(z,0,i-1)
  let z1=matchstr(z1,'\d\+')
  let xpos=0+z1
  if(xpos<=vg1) "Must be ascending
    let msg="Tabstop #" . cnt . ' (' . z1 . ') is less than preceding stop (' . vg1 . ')'
    echohl WarningMsg
    echo msg
    echohl None
    let s:Pos_Count=-cnt "To remember and invalidate together
    return
  endif
  let vg1=xpos
  let s:Pos_{cnt}=xpos
  let z=strpart(z,i)
endwhile
let s:Pos_Count=cnt "Number of Positions
let ii=1
let z=s:Pos_Count . ' tabstops set at positions '
let zc=''
while(ii<=s:Pos_Count)                                       
  let z=z.zc.s:Pos_{ii}
  let ii=ii+1
  let zc=','
endwhile
echo z
:nmap <buffer> <TAB> :call <SID>VarTab_DoStops(1)<CR>
:imap <buffer> <TAB> <C-O>:call <SID>VarTab_DoStops(1)<CR>
:nmap <buffer> <S-TAB> :call <SID>VarTab_DoStops(-1)<CR>
:imap <buffer> <S-TAB> <C-O>:call <SID>VarTab_DoStops(-1)<CR>
:endfun

fun! s:VarTab_DoStops(Dir)
  if s:Pos_Count<=0 "No stops present
    return
  endif
  let l1=line(".")
  let c1=col(".")
  let c3=col("$")
  if(a:Dir>0)
    let c2=s:Pos_{s:Pos_Count}  "Last position if no other found
    let ii=1
    while(ii<s:Pos_Count)
      let ip=s:Pos_{ii}
      if(c1<ip)                                                                 
        let c2=ip
        break
      endif
      let ii=ii+1
    endwhile
  else
    let c2=1  "start of line
    let ii=s:Pos_Count
    while(ii>0)
      let ip=s:Pos_{ii}
      if(c1>ip)                                                                 
        let c2=ip
        break
      endif
      let ii=ii-1
    endwhile
  endif
  " echo "c1=" . c1 .", l1=" . l1 . ", c2=" . c2 . ", c3=" . c3             
  if(c2>c3)                                                                 
    let z1=getline(l1)                                                    
    while(strlen(z1)<c2)                                                    
      let z1=z1 . "             "
    endwhile
    call setline(l1,z1)
  endif                                                                 
  call cursor(l1,c2)
:endfun

" :call VarTab_SetStops("1,22, 33 , 44  , 60")                          
