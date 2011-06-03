" Vim plugin file
" Description: 
" Maintainer:  Torrin <torrin@torrin.net> 
" Last Change: 2003 JUN 06
" Version:     4

" If we have already loaded this file, don't load it again.
if exists("loaded_highlightline")
   finish
endif
let loaded_highlightline = 1

" Save compatable options
let s:save_cpo = &cpo
set cpo&vim

let s:linehighlighted = 0

nmap <F2> :call <SID>HighlightLine()<CR>
"map <F3> :call <SID>UnHighlightLine()<CR>
imap <F2> <C-O>:call <SID>HighlightLine()<CR>
"imap <F3> <C-O>:call <SID>UnHighlightLine()<CR>
"map <kUp> :call <SID>test()<CR>

"function! s:test()
"   echo "Hello world!"
"endfunction

function! s:HighlightLine()
   if s:linehighlighted == line(".")
      call <SID>UnHighlightLine()
      return
   endif
   if s:linehighlighted != 0
      call <SID>UnHighlightLine()
   endif
   let currentline = getline(".")
   let currentline = substitute(currentline, "\\", "\\\\\\\\", "g")
   let currentline = substitute(currentline, "*", "\\\\*", "g")
   let currentline = substitute(currentline, "\"", "\\\\\"", "g")
   let currentline = substitute(currentline, "[", "\\\\[", "g")
   let currentline = substitute(currentline, "]", "\\\\]", "g")
   let currentline = substitute(currentline, "-", "\\\\-", "g")
   let currentline = substitute(currentline, "\\~", "\\\\~", "g")

"This one doesn't work properly
"   let currentline = substitute(currentline, "+", "\\\\+", "g")

"I don't like these
   let currentline = substitute(currentline, "+", ".", "g")
   let currentline = substitute(currentline, "@", ".", "g")
   echo currentline
   execute "syntax match OneLine +" . currentline . "+ oneline"
"   execute "syntax match OneLine \"" . currentline . "\" oneline"
   execute "highlight default link OneLine Visual"
   let s:linehighlighted = line(".")
endfunction

function! s:UnHighlightLine()
   if s:linehighlighted != 0
      execute "syntax clear OneLine"
      let s:linehighlighted = 0
   endif
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
