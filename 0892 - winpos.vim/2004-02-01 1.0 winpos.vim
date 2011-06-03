" winpos.vim - Don Yang (uguu.org)
"
" Save/restore window positions.
" <S-F1> <S-F2> <S-F3> <S-F4>    = save window positions.
" <F1> <F2> <F3> <F4>            = recall window positions.
"
" 02/01/04


let s:c = &cpoptions
set cpoptions-=<

" Default window positions
let winpos_x1 = 8
let winpos_y1 = 8
let winpos_x4 = 750
let winpos_y4 = 650
let winpos_x2 = winpos_x4
let winpos_y2 = winpos_y1
let winpos_x3 = winpos_x1
let winpos_y3 = winpos_y4

function! SaveWinPos1()
   let g:winpos_x1 = getwinposx()
   let g:winpos_y1 = getwinposy()
endfunction
function! SaveWinPos2()
   let g:winpos_x2 = getwinposx()
   let g:winpos_y2 = getwinposy()
endfunction
function! SaveWinPos3()
   let g:winpos_x3 = getwinposx()
   let g:winpos_y3 = getwinposy()
endfunction
function! SaveWinPos4()
   let g:winpos_x4 = getwinposx()
   let g:winpos_y4 = getwinposy()
endfunction

function! SetWinPos1()
   exec ':winpos ' . g:winpos_x1 . ' ' . g:winpos_y1
endfunction
function! SetWinPos2()
   exec ':winpos ' . g:winpos_x2 . ' ' . g:winpos_y2
endfunction
function! SetWinPos3()
   exec ':winpos ' . g:winpos_x3 . ' ' . g:winpos_y3
endfunction
function! SetWinPos4()
   exec ':winpos ' . g:winpos_x4 . ' ' . g:winpos_y4
endfunction

nnoremap <S-F1> :silent! call SaveWinPos1()<CR>
nnoremap <S-F2> :silent! call SaveWinPos2()<CR>
nnoremap <S-F3> :silent! call SaveWinPos3()<CR>
nnoremap <S-F4> :silent! call SaveWinPos4()<CR>

nnoremap <F1> :silent! call SetWinPos1()<CR>
nnoremap <F2> :silent! call SetWinPos2()<CR>
nnoremap <F3> :silent! call SetWinPos3()<CR>
nnoremap <F4> :silent! call SetWinPos4()<CR>

let &cpoptions = s:c
unlet s:c
