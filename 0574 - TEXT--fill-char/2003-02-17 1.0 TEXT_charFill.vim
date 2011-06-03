" (c) Petr Mach <gsl@seznam.cz>, 2003
" http://iglu.cz/wraith/
" 
" Version 1.0
" 
" Script create:
"   function for fill space char by previous line
"   SAMPLE:
"
"   line 1: background-color:   #eeeeee
"   line 2: color:|
"
"   | is cursor, 
"   after call :FillUpSpace (you can map to TAB) is 
"   situation this:
"
"   line 1: background-color:   #eeeeee
"   line 2: color:              |
"
"   It's all.
"
"   You can use :FillDownSpace  for fill space char by after line
"   You can use :FillUpPrevious for fill any char (by char 
"   previous cursor) 
"
"   SAMPLE:
"
"   line1: Chapter1 ...................... 7
"   line2: Chapter2 .|
"
"   after use :FillUpPrevious is situation this:
"
"   line1: Chapter1 ...................... 7
"   line2: Chapter2 ......................|
"
"   You can use :FillDownPrevious for fill any char by after line
"
" ---------------------------------------------------------------------
" HINT: 
" uncoment this for hotkey F3 and Shift-F3
" 
" imap <F3>   <ESC>:FillUpSpace<CR>a
" imap <S-F3> <ESC>:FillDownSpace<CR>a
" 
" ---------------------------------------------------------------------

  function FillChar(shift, char)
    let nLi1=line('.')
    let nCo1=col('.')
    "
    if(nLi1+a:shift<1)
      return
    endif  
    if(nLi1+a:shift>line('$'))
      return
    endif
    "
    let nLi2=nLi1+a:shift
    let sLi2=getline(nLi2)
    let lLi2=strlen(sLi2)
    "
    let iCo2=nCo1
    let tSp=0
    "
    while(iCo2<lLi2)
      if(tSp==1)
        if(sLi2[iCo2]!=a:char)
          break
        endif
      else
        if(sLi2[iCo2]==a:char)
          let tSp=1
        endif
      endif
      let iCo2=iCo2+1
    endwhile  
    let nFill=iCo2-nCo1
    "
    let i=0
    let sFill=''
    while(i<nFill)
      let sFill=sFill.a:char
      let i=i+1
    endwhile
    "
    let x=@x
    let @x=sFill
    normal "xp
    let @x=x
    "
  endfunction

  function FillCharPrevious(shift)
    let  s=getline('.')
    let  c=col('.')
    let  char=s[c-1]
    normal x
    call FillChar(a:shift, char)
  endfunction
  
  command FillUpSpace      call FillChar(-1, ' ')
  command FillDownSpace    call FillChar(1,  ' ')
  command FillUpPrevious   call FillChar(-1, ' ')
  command FillDownPrevious call FillChar(1,  ' ')

