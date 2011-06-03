" hanoi.vim 
" Copyright (c) 2002 by Samuel Yang <toklip@fetnet.net>
exe "normal 1Go\<CR>Hanoi_Tower\<CR>\<CR>Press 'g' to Start, 'q' to Quit\<Esc>"
nnoremap g :set encoding=latin1<CR>:call Move(3,1,1)<CR>
nnoremap q :q!<CR>
function Move( S, C, D)
  let l:T = 6-(a:S)-(a:D)
  exe (a:S)
  if( col("$")-(a:C) == 0)
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
endfunction
