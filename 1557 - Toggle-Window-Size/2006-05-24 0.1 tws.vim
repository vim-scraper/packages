" Toggle Window Size
" Based on Tip #1110 by Jang Junyeong (less efficient, but works in Linux)
" This version works in Linux GTK under KDE
" has not been tested in any other environment afaik
" author: John C Kendall
" Version 0.1

let g:twsX = getwinposx()
let g:twsY = getwinposy()

" The idea was to change these values to whatever is appropriate for your display/resolution/font
"   setting to 999 should make it full screen
" however, I can't figure out how to do:  
"    set lines=g:twsMaxColumns
" as VIM apparently wants a constant... :(
" SO, the values are hard-coded within the function...
"
" let g:twsColumns = 90
" let g:twsMaxColumns=250
" let g:twsLines = 55
" let g:twsMaxLines=92

function ToggleWindowSize(act)
  if a:act == 2 || a:act == 0
    if &lines == 55
      if a:act != 0
        let g:twsX = getwinposx()
      endif
      let g:twsY = getwinposy()
      set lines=92
    else
      set lines=55
      execute "winpos " . g:twsX . " " . g:twsY
    endif
  endif
  if a:act == 1 || a:act == 0
    if &columns == 90
      if a:act != 0
        let g:twsY = getwinposy()
      endif
      let g:twsX = getwinposx()
      set columns=250
    else
      set columns=90
      execute "winpos " . g:twsX . " " . g:twsY
    endif
  endif
endfunction

map <S-F1> :call ToggleWindowSize(2)<CR>
map <S-F2> :call ToggleWindowSize(1)<CR>
map <S-F3> :call ToggleWindowSize(0)<CR>
