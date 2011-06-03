" Vim plugin file
" Description: 
" Maintainer:  Torrin <torrin@torrin.net> 
" Last Change: 2002 JUN 21
" Version:     2

" If we have already loaded this file, don't load it again.
if exists("loaded_highlightline")
   finish
endif
let loaded_highlightline=1

" Save compatable options
let s:save_cpo = &cpo
set cpo&vim

let linehighlighted=0

map <F2> :call <SID>HighlightLine()<CR>
map <F3> :call <SID>UnHighlightLine()<CR>
imap <F2> <C-O>:call <SID>HighlightLine()<CR>
imap <F3> <C-O>:call <SID>UnHighlightLine()<CR>

function! s:HighlightLine()
"   execute "syntax keyword OneLine \"" . getline(".") . "\""
   if g:linehighlighted == 1
       call <SID>UnHighlightLine()
   endif
   execute "syntax match OneLine +" . substitute(getline("."),"[+\"]",".","g") . "+ oneline"
   execute "highlight default link OneLine Visual"
   let g:linehighlighted = 1
endfunction

function! s:UnHighlightLine()
   if g:linehighlighted == 1
      execute "syntax clear OneLine"
      let g:linehighlighted = 0
   endif
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
