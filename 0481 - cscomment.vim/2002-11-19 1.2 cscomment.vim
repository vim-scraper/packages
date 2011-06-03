"=============================================================================
"    Copyright: Copyright (C) 2002 Aaron Jensen
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               cscomment.vim is provided *as is* and comes with no
"               warranty of any kind, either expressed or implied. In no
"               event will the copyright holder be liable for any damages
"               resulting from the use of this software.
" Name Of File: cscomment.vim
"  Description: C# Commenter Vim Plugin
"   Maintainer: Aaron Jensen (aj at mostafa dot net)
"          URL: none
"  Last Change: 19 November 2002
"      Version: 1.2
"        Usage: Normally, this file should reside in the plugins
"               directory and be automatically sourced. If not, you must
"               manually source this file using ':source cscomment.vim'.
"
"               You may use the default keymappings of
"
"               <c-t> - Start commenter
"
"               Once the commenter is active:
"
"               <tab> - Next tag's text
"               <s-tab> - Previous tag's text
"      History: Version 1.2
"               -Commenter now allows you to edit your comments and will
"                reshape them as necessary (add params, remove them, etc.)
"               -Now restores previous imappings
"               -Cleaned up a bit
"
"               Version 1.1
"               -<cr> now adds a new line with ///, useful for long summaries.
"               -<tab> and <s-tab> iterate through summary, params, and return
"                tags so you can fill everything in quickly.
"
"               Version 1.0
"               -Initial release.
"
"=============================================================================

if exists("loaded_cscommenter")
  finish
endif
let loaded_cscommenter=1

if (!hasmapto(":call <SID>CS_Comment()<cr>","n"))
  nmap <c-t> :call <SID>CS_Comment()<cr>
endif

function! <SID>CS_Comment()
  let l:a_back = @a
  let l:c_back = @c
  let b:i = 1
  let b:old_ve = &virtualedit
  " set virtual edit so we end up in the right place when we tab.
  set virtualedit=all

  " see if we're on a line with a /// comment already
  let l:oncomment = match(getline('.'), "[ \t]*///") != -1

  " go to first noncomment line
  if (l:oncomment)
    exe "normal! /^\\([ \t]*\\/\\/\\/\\)\\@!\<cr>"
    let l:hadcomment = 1
  else
    " see if there is a comment above this line
    let l:hadcomment = match(getline(line('.') - 1), "[ \t]*///") != -1
  endif

  if (l:hadcomment)
    let l:lastcomment = line('.') - 1
    normal! k0

    " goto first comment line
    exe "normal! ?^\\([ \t]*\\/\\/\\/\\)\\@!?1\<cr>"
    let l:firstcomment = line('.')

    " delete the comment into reg c
    exe l:firstcomment . "," . l:lastcomment . "d c"
  endif

  " yank from beg of line to the end of the function header into reg a
  exe "normal! 0\"ay/{\\|;\<cr>"

  " get the return type, or access modifier if its a constructor
  let l:rt = matchstr(@a, "\\h\\w*\\ze[ \t\n]\\+\\h\\w*[ \t\n]*([^)]*)")

  " get the params
  let l:params = matchstr(@a, "\\h\\w*[ \t\n]*([ \t\n]*\\zs[^)]*\\ze[ \t\n]*)")

  " insert summary info
  exe "normal! O/// <summary>"

  " if we have a comment and a summary
  if (l:hadcomment)
    set paste
    exe "normal! a" . matchstr(@c, "<summary>\\zs\\_.\\{-}\\ze\\(\n[ \t]*///[ \t]*\\)\\?</summary>")
    set nopaste
  endif

  exe "normal! mao/// </summary>"

  " insert remarks info
  exe "normal! o/// <remarks>"

  " if we have a comment and a remarks
  if (l:hadcomment)
    set paste
    exe "normal! a" . matchstr(@c, "<remarks>\\zs\\_.\\{-}\\ze\\(\n[ \t]*///[ \t]*\\)\\?</remarks>")
    set nopaste
  endif

  exe "normal! mbo/// </remarks>"

  " go through the params and add their tags
  while strlen(l:params) > 0
    let b:i = b:i + 1
    let l:m = nr2char(b:i + 97)

    let l:param = matchstr(l:params, "\\h\\w*[ \t\n]\\+\\zs\\h\\w*")
    exe "normal! o/// <param name=\"" . l:param . "\">"

    if (l:hadcomment)
      set paste
      exe "normal! a" . matchstr(@c, "<param name=\"" . l:param . "\">\\zs\\_.\\{-}\\ze</param>")
      set nopaste
    endif

    exe "normal! m" . l:m . "a</param>"

    let l:params = matchstr(l:params, ",\\zs[^)]*")
  endwhile

  " if we return something, add the returns tag
  if (strlen(l:rt) > 0 && l:rt != "void" && l:rt != "public" && l:rt != "private" && l:rt != "protected")
    let b:i = b:i + 1
    let l:m = nr2char(b:i + 97)
    exe "normal! o/// <returns>"
    if (l:hadcomment)
      set paste
      exe "normal! a" . matchstr(@c, "<returns>\\zs\\_.\\{-}\\ze\\(\n[ \t]*///[ \t]*\\)\\?</returns>")
      set nopaste
    endif
    exe "normal! m" . l:m . "a</returns>"
  endif

  " go back to the summary.
  normal! `al
  let b:n = b:i
  let b:i = 0
  let @a = l:a_back
  let @c = l:c_back

  " save old mappings
  let b:oldcr = maparg("<cr>","i")
  let b:oldesc = maparg("<esc>","i")
  let b:oldtab = maparg("<tab>","i")
  let b:oldstab = maparg("<s-tab>","i")

  " make new mappings
  inoremap <silent> <buffer> <cr> <cr>///<space>
  inoremap <silent> <buffer> <esc> <esc>:call <SID>CS_Comment_End()<cr>
  inoremap <silent> <buffer> <tab> <esc>:call <SID>CS_Comment_Next()<cr>
  inoremap <silent> <buffer> <s-tab> <esc>:call <SID>CS_Comment_Prev()<cr>

  startinsert
endfunction

function! <SID>CS_Comment_End()
  " fix everything
  let &virtualedit = b:old_ve
  if (strlen(b:oldcr))
    exec "inoremap <buffer> <cr> " . b:oldcr
  endif
  if (strlen(b:oldesc))
    exec "inoremap <buffer> <esc> " . b:oldesc
  endif
  if (strlen(b:oldtab))
    exec "inoremap <buffer> <tab> " . b:oldtab
  endif
  if (strlen(b:oldstab))
    exec "inoremap <buffer> <s-tab> " . b:oldstab
  endif
endfunction

function! <SID>CS_Comment_Next()
  exec "normal! m" . nr2char(b:i + 97)
  if b:i < b:n
    let b:i = b:i + 1
  else
    let b:i = 0
  endif

  exec "normal! `" . nr2char(b:i + 97) ."l"
  startinsert
endfunction

function! <SID>CS_Comment_Prev()
  exec "normal! m" . nr2char(b:i + 97)
  if b:i > 0
    let b:i = b:i - 1
  else
    let b:i = b:n
  endif

  exec "normal! `" . nr2char(b:i + 97) ."l"
  startinsert
endfunction
