" Copyright (c) 1998-2002
" Aaron Jensen <aj@mostafa.net>
"
" We grant permission to use, copy modify, distribute, and sell this
" software for any purpose without fee, provided that the above copyright
" notice and this text are not removed. We make no guarantee about the
" suitability of this software for any purpose and we are not liable
" for any damages resulting from its use. Further, we are under no
" obligation to maintain or extend this software. It is provided on an
" "as is" basis without any expressed or implied warranty.
"
" This script will generate the /// comment blocks for csharp for methods
" and constructors.  Simply place the cursor anywhere on the first line of
" the function prototype and hit <c-t>.

function! CS_Comment()
  let a_back = @a
  " yank from where we are to the end of the function header
  normal! 0"ay/{\|;

  " get the return type, or access modifier if its a constructor
  let rt = matchstr(@a, "\\h\\w*\\ze[ \t\n]\\+\\h\\w*[ \t\n]*([^)]*)")

  " get the params
  let params = matchstr(@a, "\\h\\w*[ \t\n]*([ \t\n]*\\zs[^)]*\\ze[ \t\n]*)")
  normal! O/// <summary>/// mzo/// </summary>

  " go through the params and add their tags
  while strlen(params) > 0
    let param = matchstr(params, "\\h\\w*[ \t\n]\\+\\zs\\h\\w*")
    normal! o/// <param name="
    normal! "=paramp
    normal! A"></param>

    let params = matchstr(params, ",\\zs[^)]*")
  endwhile

  " if we return something, add the returns tag
  if strlen(rt) > 0 && rt != "void" && rt != "public" && rt != "private" && rt != "protected"
    normal! o/// <returns></returns>
  endif

  " go back to the summary.
  normal! `z
  let @a = a_back
endfunction

nmap <c-t> :call CS_Comment()A
