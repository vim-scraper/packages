" TogFullScreen.vim
" Author: Andy Fischer
" Description: A simple script to toggle the Gvim window between normal and fullscreen modes
" in Windows. The F11 key has the same functionality in Internet Explorer.

let g:fullScreened = 0
function ToggleFullscreen()
  if g:fullScreened == 0
    let g:fullScreened = 1
    simalt ~x
  else
    let g:fullScreened = 0
    simalt ~r
  endif
endfunction

map <F11> :call ToggleFullscreen()<CR>