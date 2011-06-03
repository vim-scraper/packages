"============================================================================= 
" File:         Maxlen.vim
" Author:       Mutoh Yasuoki <mutoh@kb3.so-net.ne.jp>
" Requirements: Vim version 6.1 and later.
" Commands:     {range}Maxlen
" Description:  Find the long lines and mark them.
" Last Change:	2003 June 13
"============================================================================= 
"Has this already been loaded?
if exists('loaded_Maxlen')
    finish
endif
let loaded_Maxlen = 1

"Create command
command! -nargs=? -range=% Maxlen :<line1>,<line2>call <SID>Maxlen()

function! s:Maxlen() range
  let ln_cur = a:firstline
  "Line nrs being marked.
  let ln_a = 0
  let ln_b = 0
  let ln_c = 0
  let ln_d = 0
  let ln_e = 0
  "Line length.
  let len_a = 0
  let len_b = 0
  let len_c = 0
  let len_d = 0
  let len_e = 0
  "Loop each line.
  while ln_cur <= a:lastline
    let len_cur = strlen(getline(ln_cur))
    "If the line is longer than the saved ...
    if len_cur > len_a
      "Save the new line nr.
      let len_e = len_d
      let len_d = len_c
      let len_c = len_b
      let len_b = len_a
      "Save the new line length.
      let len_a = len_cur
      let ln_e = ln_d
      let ln_d = ln_c
      let ln_c = ln_b
      let ln_b = ln_a
      let ln_a = ln_cur
    elseif len_cur > len_b
      let len_e = len_d
      let len_d = len_c
      let len_c = len_b
      let len_b = len_cur
      let ln_e = ln_d
      let ln_d = ln_c
      let ln_c = ln_b
      let ln_b = ln_cur
    elseif len_cur > len_c
      let len_e = len_d
      let len_d = len_c
      let len_c = len_cur
      let ln_e = ln_d
      let ln_d = ln_c
      let ln_c = ln_cur
    elseif len_cur > len_d
      let len_e = len_d
      let len_d = len_cur
      let ln_e = ln_d
      let ln_d = ln_cur
    elseif len_cur > len_e
      let len_e = len_cur
      let ln_e = ln_cur
    endif
    let ln_cur = ln_cur + 1
  endwhile
  echo "Marked a=lin.".ln_a."(col.".len_a."), b=".ln_b."(".len_b."), c=".ln_c.
      \"(".len_c."), d=".ln_d."(".len_d."), e=".ln_e."(".len_e.")"
  "Mark the lines at its end.
  exe ln_a
  exe 'normal $ma'
  exe ln_b
  exe "normal $mb"
  exe ln_c
  exe 'normal $mc'
  exe ln_d
  exe 'normal $md'
  exe ln_e
  exe 'normal $me'
  exe ln_a
endfunction

