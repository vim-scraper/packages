" Copyright © 2007, Valyaeff Valentin <hhyperr AT gmail DOT com>
" Plugin for inserting snippets in new line
"     Version:    1.0 2007.07.14
"      Author:    Valyaeff Valentin <hhyperr AT gmail DOT com>
"     License:    GPL
"
" Example
" -------
" Perhaps you editing C file:
"   #include <stdlib.h*>
" (* - cursor position)
" If you hit Enter, new include directive will be inserted:
"   #include <stdlib.h>
"   #include <*>
"
" Configuration
" -------------
" You must create new hash g:smart_cr
"   let g:smart_cr = {}
" Hash must contain arrays for different filetypes:
"   let g:smart_cr.c = [['^#include <', '^>$', "#include \<@@@>"],
"                      \['^#include "', '^"$', "#include \"@@@\""]]
" When you hit Enter, line is divided on two parts - before and above
" the cursor. First two strings are regular expressions for left and
" right parts of line. If they matched, third string will be inserted in
" new line.
" "@@@" is marker where to place cursor. You may redefine it by setting
" global variable:
"   let g:smart_cr_marker = '<here>'
"
" Another example
" ---------------
" This for Embedded ruby:
"   let g:smart_cr.eruby = [['<%[^=]', '^\s*%>',  "<% @@@ %>"],
"                          \['<%[^=]', '^\s*-%>', "<% @@@ -%>"],
"                          \['<%=',    '^\s*%>',  "<%= @@@ %>"],
"                          \['<%=',    '^\s*-%>', "<%= @@@ -%>"]]

if exists("loaded_smart_cr")
    finish
endif
let loaded_smart_cr = 1

let s:save_cpo = &cpo
set cpo&vim


if !exists("g:smart_cr_marker")
  let g:smart_cr_marker = '@@@'
endif

function! NewLine()
  if has_key(g:smart_cr, &ft)
    let left_part = strpart(getline('.'), 0, col('.') - 1)
    let right_part = strpart(getline('.'), col('.') - 1, strlen(getline('.')))
    for item in g:smart_cr[&ft]
      if match(left_part, item[0]) >= 0 && match(right_part, item[1]) >= 0
        return "\<Esc>o".IMAP_PutTextWithMovement(item[2], g:smart_cr_marker, '')
      endif
    endfor
  endif
  return "\<CR>"
endfunction

if exists("g:smart_cr")
  inoremap <CR> <C-R>=NewLine()<CR>
endif

let &cpo = s:save_cpo
