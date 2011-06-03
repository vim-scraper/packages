" DestPaste.vim :  Destructive Paste
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 0.1
"
" Destructively pastes a register.

fun! DestructivePaste(reverse)
  let rt=getregtype()
  
  if rt!~"^\<c-v>"
    exe "norm R\<c-r>".v:register
  else
    let reg=getreg()
    let lines=strlen(substitute(reg,"[^\n]",'','g'))
    let width=matchstr(rt,'\d\+$')
    let ve=&ve
    let sel=&sel
    set sel=inclusive
    set virtualedit=all
    let str=''
    if a:reverse
      let c=width-1 | while c > 0 | let c=c-1
        let str=str.'h'
      endwhile
    else
      let str='l'
    endif
    let str=str."\<c-v>"
    let c=width-1 | while c > 0 | let c=c-1
      let str=str.' '
    endwhile
    let c=lines | while c > 0 | let c=c-1
      let str=str.'j'
    endwhile
    exe "norm! ".str.'"_d"'.v:register.'P'
    let &sel=sel
    let &ve=ve
  endif

endfun
map <Leader>Dp :call DestructivePaste(0)<CR>
map <Leader>DP :call DestructivePaste(1)<CR>

