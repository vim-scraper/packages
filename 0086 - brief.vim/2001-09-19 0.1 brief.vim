" -*- vim -*-
" @(#) $Id: brief.vim,v 1.0 2001-09-19 10:19:39 EDT eralston exp $
"
" Vim global plugin for doing brief-style <Home> and <End>
" Last Change: 2001-09-19 10:30:38
" Maintainer: Ed Ralston <eralston@techsan.org>
"
" created 2001-09-19 10:19:47 eralston@techsan.org
"
" Pressing <Home> successively puts you at the beginning of the
" current line, beginning of the screen, and the beginning of the file.
" Similarly, for <End>.
"
"

if exists("loaded_brief")
   finish
endif
let loaded_brief = 1


if !hasmapto('<Plug>BriefHome')
   nmap <unique> <Home> <Plug>BriefHome
endif
noremap <unique> <script> <Plug>BriefHome :call <SID>Home()<cr>

if !hasmapto('<Plug>BriefEnd')
   nmap <unique> <End> <Plug>BriefEnd
endif
noremap <unique> <script> <Plug>BriefEnd :call <SID>End()<cr>


function! s:Home()
   " if we are on the first char of the line, go to the top of the screen
   if (col(".") <= 1)
      " if on top of screen, go to top of file
      let l:a = line(".")
      normal H0
      if (line(".") == l:a)
         " we did not move! so ...
         normal 1G
      endif
   else
      " goto beginning of line
      normal 0
   endif

   " clear the command-line
   let l:ch_save = &l:ch | let &l:ch = 0 | let &l:ch = l:ch_save
endfunction

function! s:End()
   " if we are on the last char of the line, go to the bottom of the screen
   if (col(".") >= strlen(getline(".")))
      " if on bottom of screen, go to bottom of file
      let l:a = line(".")
      normal L$
      if (line(".") == l:a)
         " we did not move! so ...
         normal G$
      endif
   else
      " goto end of line
      normal $
   endif

   " clear the command-line
   let l:ch_save = &l:ch | let &l:ch = 0 | let &l:ch = l:ch_save
endfunction


" vim:syntax=vim:
" vim:set ai et ts=3 sw=3:
