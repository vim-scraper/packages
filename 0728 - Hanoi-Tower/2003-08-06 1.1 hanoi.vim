" hanoi.vim 1.1
" Copyright (c) 2002,2003 by Samuel Yang <toklip@fetnet.net>
exe "normal 1Go\<CR>Hanoi_Tower\<CR>\<CR>Press 'g' to Start, 's' to Stop, 'q' to Quit\<Esc>"
set encoding=latin1
let g:stop_hanoi = 0
nnoremap g :let g:stop_hanoi=0<CR>:call Move(3,1,1)<CR>
nnoremap q :q!<CR>
nnoremap s :<ESC>
function Move( S, C, D)
  if( g:stop_hanoi == 0)
    let l:T = 6-(a:S)-(a:D)
    exe (a:S)
    if( getchar(0) == char2nr("s"))
      echo "Aborted"
      let g:stop_hanoi = 1
    elseif( col("$")-(a:C) == 0)
      exe "normal aHere_is_Nothing_to_Move\<Esc>"
    elseif( col("$")-(a:C) == 1)
      normal $x
      exe (a:D)
      normal $p
      redraw
    else
      exe (l:T)
      let l:TC = col("$")
      call Move( a:S, a:C+1, l:T)
      exe (a:S)
      normal $x
      exe (a:D)
      normal $p
      redraw
      call Move( l:T, l:TC, a:D)
    endif
  endif
endfunction
