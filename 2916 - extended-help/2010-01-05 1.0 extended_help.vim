" Plugin to open and close help without leaving current window
" Last Change:	2010 Jan 05
" Maintainer:	Sergey Khorev <sergey.khorev@gmail.com>
" License:	This file is placed in the public domain.

" USAGE
" :Help subject - get help using :help command staying in current window
" :Help! subject - help with :help! command
" :Help or :Help! - close help window

if exists("loaded_extended_help")
  finish
endif

let loaded_extended_help = 1
let s:save_cpo = &cpo
set cpo&vim

command! -nargs=? -bang Help call s:Help('<bang>', '<args>')

function! s:Help(bang, arg)
  if a:arg != ''
    exec 'help' . a:bang . ' ' . a:arg
    " return to prev window
    wincmd p
  else
    " open or switch to help window and close it
    help
    wincmd c
  endif
endfunction

let &cpo = s:save_cpo

" vim: set ft=vim ts=8 sts=2 sw=2:
