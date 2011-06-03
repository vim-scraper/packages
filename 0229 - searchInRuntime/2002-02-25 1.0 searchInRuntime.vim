" File:		searchInRuntime.vim 
" Author:	Luc Hermitte <EMAIL:hermitte@free.fr>
" 		<URL:http://hermitte.free.fr/vim>
" Last Update:  20th feb 2002
" Version:	1.0
"
" Purpose:	Search a file in the runtime path, and execute an Ex command on
" 		it.
"
" ========================================================================

if exists("g:searchInRuntime_vim") | finish | endif
let g:searchInRuntime_vim = 1

" ========================================================================
command! -nargs=+ -complete=file -bang
      \       SearchInRuntime call <SID>SearchInRuntime("<bang>",  <f-args>)

" ========================================================================
function! s:SearchInRuntime(bang, cmd, ...)
  let do_all = a:bang == "!"
  let f = ""
  " Loop on runtimepath
  let rp = &runtimepath
  while strlen(rp) != 0
    let r  = matchstr(rp, '^[^,]*' )."/"
    let rp = substitute(rp, '.\{-}\(,\|$\)', '', '')
      
    " Loop on arguments
    let i = 1
    while i <= a:0
      let f = f . glob(r.a:{i}). "\n"
      " echo a:{i}. "  --  " . f."\n"
      let i = i + 1
    endwhile
  endwhile
  " source the matching files
  while strlen(f) != 0
    let ff = matchstr(f, "^[^\n]*")
    let f  = substitute(f, '.\{-}\('."\n".'\|$\)', '', '')
    if filereadable(ff)
      exe a:cmd." ".ff
"      echo ff
      if !do_all | return | endif
    endif
  endwhile
endfunction

