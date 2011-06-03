" Vim filetype plugin file for adding getter/setter methods
" Language:	Java
" Maintainer:	Roman Wagner romanw@cs.tu-berlin.de
" Last Change:	Mon Sep 27 21:31:09 CEST 2004
"
" ToDo:
" refactoring!!! 
" new features (more than 1 catch-block)
" look for updates


if exists("b:did_InsertTry_ftplugin")
  finish
endif
let b:did_InsertTry_ftplugin = 1

fun! s:InsertTry() range
    " get lines and indent
    let l:first      = a:firstline
    let l:last       = a:lastline
    let l:indent     = matchstr(getline(l:first), "^\\s*")
    let l:shiftwidth = matchstr("                ", "^\\s\\{" . &sw . "}")
    
    " insert try { <CR>
    call append(l:first - 1, l:indent . "try {")
    
    " indent the block
    while l:first < l:last + 1
        let l:first = l:first + 1
        call setline(l:first, l:shift . getline(l:first))
    endwhile
    
    " insert }
    let l:last = l:last + 1
    call append(l:last, l:indent . "}")
    
    " insert catch () {
    "        }    
    let l:last = l:last + 1
    call append(l:last, l:indent . "catch () {")    
    let l:last = l:last + 1
    call append(l:last, l:indent . "}")

    " insert finally {
    "        }
    let l:last = l:last + 1
    call append(l:last, l:indent . "finally {")
    let l:last = l:last + 1
    call append(l:last, l:indent . "}")

    " goto catch ( ) {
    "            ^    
    execute l:last - 2  
    execute "normal ^w"
endfunction

if !exists("no_plugin_maps") && !exists("no_java_maps")
  if !hasmapto('<Plug>InsertTryInsertTry')
    map <unique> <buffer> ¬t <Plug>InsertTryInsertTry
  endif
  noremap <buffer> <script> 
    \ <Plug>InsertTryInsertTry 
    \ <SID>InsertTry
  noremap <buffer> 
    \ <SID>InsertTry 
    \ :call <SID>InsertTry()<CR>
endif
