" Copyright (c) 1998-2002
" Aaron Jensen <aj@mostafa.net>
"
" We grant permission to use, copy modify, distribute, and sell this
" software for any purpose without fee, provided that the above copyright
" notice and this text are not removed. We make no guarantee about the
" suitability of this software for any purpose and we are not liable
" for any damages resulting from its use. Ful:rther, we are under no
" obligation to maintain or extend this software. It is provided on an
" "as is" basis without any expressed or implied warranty.
"
" This script will generate the /// comment blocks for csharp for methods
" and constructors.  Simply place the cursor anywhere on the first line of
" the function prototype and hit <c-t>.
"
" Side effects: Overwrites marks starting at a, one mark for each parameter.
" Unmaps <cr>, <esc>, <tab>, <s-tab> in insert mode

function! CS_Comment()
  let l:a_back = @a
  let b:i = 0

  " yank from where we are to the end of the function header
  normal! 0"ay/{\|;

  " get the return type, or access modifier if its a constructor
  let l:rt = matchstr(@a, "\\h\\w*\\ze[ \t\n]\\+\\h\\w*[ \t\n]*([^)]*)")

  " get the params
  let l:params = matchstr(@a, "\\h\\w*[ \t\n]*([ \t\n]*\\zs[^)]*\\ze[ \t\n]*)")
  normal! O/// <summary>/// mai /// </summary>

  " go through the params and add their tags
  while strlen(l:params) > 0
    let b:i = b:i + 1
    let l:m = nr2char(b:i + 97)

    let l:param = matchstr(l:params, "\\h\\w*[ \t\n]\\+\\zs\\h\\w*")
    exec "normal! o/// <param name=\"" . l:param . "\">m" . l:m . "a</param>"

    let l:params = matchstr(l:params, ",\\zs[^)]*")
  endwhile

  " if we return something, add the returns tag
  if strlen(l:rt) > 0 && l:rt != "void" && l:rt != "public" && l:rt != "private" && l:rt != "protected"
    let b:i = b:i + 1
    let l:m = nr2char(b:i + 97)
    exec "normal! o/// <returns>m" . l:m . "a</returns>"
  endif

  " go back to the summary.
  normal! `a
  let b:n = b:i
  let b:i = 0
  let @a = l:a_back

  inoremap <CR> <CR>///<space>
  inoremap <esc> <esc>:call CS_Comment_End()
  inoremap <tab> <esc>:call CS_Comment_Next()a
  inoremap <s-tab> <esc>:call CS_Comment_Prev()a
endfunction

function! CS_Comment_End()
  iunmap <CR>
  iunmap <esc>
  iunmap <tab>
  iunmap <s-tab>
endfunction

function! CS_Comment_Next()
  if b:i < b:n
    let b:i = b:i + 1
  else
    let b:i = 0
  endif

  exec "normal! `" . nr2char(b:i + 97)
endfunction

function! CS_Comment_Prev()
  if b:i > 0
    let b:i = b:i - 1
  else
    let b:i = b:n
  endif

  exec "normal! `" . nr2char(b:i + 97)
endfunction

nmap <c-t> :call CS_Comment()A
