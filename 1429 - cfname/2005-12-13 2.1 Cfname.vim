" File: cfname.vim
" Author: Fabio Visona' 
" Version: 2.0 (2005-12-12)
"
" This is a very simple script for C/C++ programmers; it probably has
" bugs/limits; maybe something similar already exists but I was not able to find it.
" When editing source files with very long functions, it may happen the
" programmer do not know which function the current line is in (e.g.
" when using tags and jumping with Ctrl-] or jumping after a search).
" Here are some shortcuts to be typed in normal mode:
"    ff: shows the function prototype on the command line
"    fb: jumps to the function beginning
"    fe: jumps to the function end
"    fz: folds the function
"    fo: unfolds the function
"    
" Also, the function name is visible between square brackets on the status
" line, with automatic update.   
" 
" Revision history:
" 1.0: first revision
" 2.0: Added "fz" and "fo" fold functions; shown the function name on the
" status bar
"

let g:CF_FunctionName = ""
let g:CF_FunctionEnd = 0 

function! CF_UpdateFunctionNameForStatusBar()
    let lastfunrow = searchpair('{', '', '}', 'rn')
    if lastfunrow != g:CF_FunctionEnd	    
      let g:CF_FunctionEnd = lastfunrow	     
      let g:CF_FunctionName = s:CF_GetPrototype(1)
    endif 
    return g:CF_FunctionName
endfunction

function! s:CF_GetPrototype(nameonly_or_fullprototype)
    "save current position
    let prevrow = line(".")
    let prevcol = col(".")
    let rownumber = searchpair('{', '', '}', 'r') 
    if searchpair('{', '', '}', 'b') == 0 
      return 
    endif
    if searchpair(')', '', '}', 'b') == 0
      return 
    endif
    if searchpair('(', '', ')', 'b') == 0 
      return 
    endif
    if searchpair('\<\i*\>', '', '(', 'b') == 0 
      return 
    endif
    if a:nameonly_or_fullprototype == 1 
      let funname = matchstr(getline(line(".")), '\<\i*\>', col(".") - 1)
     else
      let funname = getline(line("."))
      echo funname
    endif  
    "jump back to the previous position
    call cursor(prevrow, prevcol)
    return funname
endfunction

function! s:CF_JumpFunctionStart()
    let rownumber = searchpair('{', '', '}', 'r')            
    let rownumber = searchpair('{', '', '}', 'b')            
endfunction

function! s:CF_JumpFunctionEnd()
    let rownumber = searchpair('{', '', '}', 'r')        
endfunction

command! -nargs=0 CFunprototype :call s:CF_GetPrototype(0)
command! -nargs=0 CFunjumpstart :call s:CF_JumpFunctionStart()
command! -nargs=0 CFunjumpend :call s:CF_JumpFunctionEnd()

map ff :CFunprototype<CR>
map fb :CFunjumpstart<CR>
map fe :CFunjumpend<CR>
map fz :CFunjumpend<CR>zf%
map fo zo

set statusline=%<%f\ %h%m%r\ [%{CF_UpdateFunctionNameForStatusBar()}]%=%-14.(%l,%c%V%)\ %P

