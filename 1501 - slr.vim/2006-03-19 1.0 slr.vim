" Copyright (c) 2006
" Michael Sharpe <feline@irendi.com>
"
" We grant permission to use, copy modify, distribute, and sell this
" software for any purpose without fee, provided that the above copyright
" notice and this text are not removed. We make no guarantee about the
" suitability of this software for any purpose and we are not liable
" for any damages resulting from its use. Further, we are under no
" obligation to maintain or extend this software. It is provided on an
" "as is" basis without any expressed or implied warranty.

" ---- Ensure only loaded once
if exists("loaded_slr")
    finish
endif
let loaded_slr = 1

" ---- Save the defaults ------------------------------------------------------
let g:rulerformat{'0'}=&rulerformat
let g:statusline{'0'}=&statusline

" ---- Some sample ruler/statusline formats -----------------------------------
" Either configure ruler/statusline formats here, or do it in the [._]vimrc
" file before/after this file is sourced. Only really have these samples to
" make it easy to see what this script does.
if !exists("g:rulerformat1")
   let g:rulerformat{'1'}='%40(%3*[b:%03b]%1*[B:%03B]%2*[o:%03o]%4*[S:%r%{synIDattr(synID(line(\".\"),col(\".\"),1),\"name\")}]%6*%m%r%)'
endif

if !exists("g:statusline1")
   let g:statusline{'1'}='%7*%f%=%3*[b:%03b]%1*[B:%03B]%2*[o:%03o]%4*[S:%r%{synIDattr(synID(line(\".\"),col(\".\"),1),\"name\")}]%6*%m%r%0*'
endif

if !exists("g:rulerformat2")
   let g:rulerformat{'2'}='%40(%3*[E:%r%{&encoding}]%1*[e:%r%{&fenc}]%2*[F:%r%{&fileformat}]%4*[T:%r%{&ft}]%6*%m%r%)'
endif

if !exists("g:statusline2")
   let g:statusline{'2'}='%7*%f%=%3*[E::%r%{&encoding}]%1*[e::%r%{&fenc}]%2*[F:%r%{&fileformat}]%4*[T:%r%{&ft}]%6*%m%r%0*'
endif
" -----------------------------------------------------------------------------

" ---- Useful function for working with rulers/status lines from VIM docs -----
function <SID>VarExists(var, val, err)
   if exists(a:var) | return a:val | else | return a:err | endif
endfunction


" Function : RotateStatusLine() (PRIVATE)
" Purpose  : Simply changes the current status line to the next configured
"            status line or falls back to the default if at the end of the list
" Args     : None
" Returns  : Nothing
" Notes    : Changes the current status line and ruler
function! <SID>RotateStatusLine()
   if !exists("g:slrCount") 
      let g:slrCount = 1
   else 
      let g:slrCount = g:slrCount + 1
   endif

   if !exists("g:rulerformat".g:slrCount)
      let g:slrCount = 0
   endif

   execute "set rulerformat=" . g:rulerformat{g:slrCount}
   execute "set statusline=" . g:statusline{g:slrCount}
endfunction

" ---- The public map which effects the rotation of the statusline/ruler
map <Leader>r :call <SID>RotateStatusLine()<CR>
