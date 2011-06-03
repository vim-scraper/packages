" Description: Simple script (hack ?) that closes opened parens
" Author:      Adrien Pierard <pierarda#iro.umontreal.ca>
" Modified:    04/05/07
" Version:     0.1
"
" Usage:       I mapped it to <Leader>p
"               So, just go to normal mode, and type the shortcut, or :call
"               RunScmCloseParens() yourself


let b:msg = ""
let b:bcpt = 0

function! SetCursorWhereItIsGoodToPutItEh()
  let line = substitute(getline("."), "\\s\\+$", "", "")
  call setline(line("."),line)
  norm $
  let charUnderCursor = strpart(line("."), col(".") - 1, 1)
  norm a)
  call CountAsMuchAsPossible()
endfunction

function! CountAsMuchAsPossible()
  let cpt = 0
  while (CanWeGoOn() > 0)
    let cpt = cpt + 1
    call OhGetBackAndSetAnotherOne()
  endwhile
  let line = substitute(getline("."), ")$", "", "")
  call setline(line("."),line)
  let b:cpt = cpt
endfunction

function! CanWeGoOn()
  return (searchpair('(', '', ')' , 'b' ))
endfunction  

function! OhGetBackAndSetAnotherOne()
  call searchpair('(', '', ')')
  norm a)

endfunction  

function! InitScmCloseParens()
  if ! exists("g:ScmCloseParens")
    let g:ScmCloseParens = "Scheme on you !"
    execute 'nmap <Leader>p :call RunScmCloseParens()<Cr>'
  endif
endfunction

fun! RunScmCloseParens()
  let b:bcpt = 0
  call SetCursorWhereItIsGoodToPutItEh()
  norm :echo b:bcpt
endfunction

call InitScmCloseParens()

