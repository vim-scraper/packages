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
"  Last Change: 20 November 2002
"      Version: 1.3a
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
"      History: Version 1.3b
"               -Interfaces are no longer treated as properties
"               -Formal ref params are now handled properly
"               -Fixed backspace so it works on the last character.
"
"               Version 1.3a
"               -Fixed bug with insert point reworking
"               -Finished backspace support, it now backspaces the /// in one
"                backspace
"               Anyone have a suggestion for restoring key mappings (like when to
"               call the restore function... and autocmd or something?  <esc> seems
"               too unreliable.
"
"               Version 1.3
"               -Fixed bug with things not getting unmapped properly
"               -Made it so properties now get a value tag
"               -Reworked the way the insert point is found after a tab/s-tab
"               -All tags now start out on one line
"
"               Version 1.2
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
"         Todo:
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

  let l:isprop = match(@a, "\\%(\\<class\\>\\|\\<interface\\>\\|(\\_.*)\\)") == -1

  " get the params
  let l:params = matchstr(@a, "\\h\\w*[ \t\n]*([ \t\n]*\\zs[^)]*\\ze[ \t\n]*)")

  " insert summary info
  exe "normal! O/// \<esc>maa<summary>"

  " if we have a comment and a summary
  if (l:hadcomment)
    set paste
    exe "normal! a" . matchstr(@c, "<summary>\\zs\\_.\\{-}\\(\n[ \t]*///[ \t]*\\)\\?\\ze</summary>")
    set nopaste
  endif

  exe "normal! a</summary>"

  " insert remarks info
  exe "normal! o/// \<esc>mba<remarks>"

  " if we have a comment and a remarks
  if (l:hadcomment)
    set paste
    exe "normal! a" . matchstr(@c, "<remarks>\\zs\\_.\\{-}\\(\n[ \t]*///[ \t]*\\)\\?\\ze</remarks>")
    set nopaste
  endif

  exe "normal! a</remarks>"

  " if this is a property, add value, otherwise add params and returns
  if (l:isprop)
    " insert value info
    exe "normal! o/// \<esc>mca<value>"

    " if we have a comment and a value
    if (l:hadcomment)
      set paste
      exe "normal! a" . matchstr(@c, "<value>\\zs\\_.\\{-}\\(\n[ \t]*///[ \t]*\\)\\?\\ze</value>")
      set nopaste
    endif

    exe "normal! a</value>"
    let b:i = b:i + 1
  else
    " go through the params and add their tags
    while strlen(l:params) > 0
      let b:i = b:i + 1
      let l:m = nr2char(b:i + 97)

      let l:param = matchstr(l:params, "\\(ref \\)\\=\\h\\w*[ \t\n]\\+\\zs\\h\\w*")
      exe "normal! o/// \<esc>m" . l:m . "a<param name=\"" . l:param . "\">"

      if (l:hadcomment)
        set paste
        exe "normal! a" . matchstr(@c, "<param name=\"" . l:param . "\">\\zs\\_.\\{-}\\ze</param>")
        set nopaste
      endif

      exe "normal! a</param>"

      let l:params = matchstr(l:params, ",\\zs[^)]*")
    endwhile

    " if we return something, add the returns tag
    if (strlen(l:rt) > 0 && l:rt != "void" && l:rt != "public" && l:rt != "private" && l:rt != "protected")
      let b:i = b:i + 1
      let l:m = nr2char(b:i + 97)
      exe "normal! o/// \<esc>m" . l:m . "a<returns>"
      if (l:hadcomment)
        set paste
        exe "normal! a" . matchstr(@c, "<returns>\\zs\\_.\\{-}\\ze\\(\n[ \t]*///[ \t]*\\)\\?</returns>")
        set nopaste
      endif
      exe "normal! a</returns>"
    endif
  endif

  " go back to the summary.
  normal! `a
  exec "normal! h/<\\(.*\\)>\\_.\\{-}\\zs\\ze\\(\\n[ \t]*\\/\\/\\/[ \t]*\\)\\=<\\/\\1>\<cr>"

  " remember what the last mark was
  let b:n = b:i
  let b:i = 0

  " restore the registers
  let @a = l:a_back
  let @c = l:c_back

  " save old mappings
  let b:oldcr = maparg("<cr>","i")
  let b:oldesc = maparg("<esc>","i")
  let b:oldtab = maparg("<tab>","i")
  let b:oldstab = maparg("<s-tab>","i")
  let b:oldbs = maparg("<bs>","i")

  " make new mappings
  inoremap <silent> <buffer> <cr> <cr>///<space>
  inoremap <silent> <buffer> <esc> <esc>:silent call <SID>CS_Comment_End()<cr>
  inoremap <silent> <buffer> <tab> <esc>:silent call <SID>CS_Comment_Next()<cr>
  inoremap <silent> <buffer> <s-tab> <esc>:silent call <SID>CS_Comment_Prev()<cr>
  inoremap <silent> <buffer> <bs> <c-o>:silent call <SID>CS_Comment_BS()<cr><bs>

  startinsert
endfunction

function! <SID>CS_Comment_End()
  " fix everything
  let &virtualedit = b:old_ve
  iunmap <buffer> <cr>
  iunmap <buffer> <esc>
  iunmap <buffer> <tab>
  iunmap <buffer> <s-tab>
  iunmap <buffer> <bs>
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
  if (strlen(b:oldbs))
    exec "inoremap <buffer> <bs> " . b:oldbs
  endif
endfunction

function! <SID>CS_Comment_Next()
  if b:i < b:n
    let b:i = b:i + 1
  else
    let b:i = 0
  endif

  exec "normal! `" . nr2char(b:i + 97)
  exec "normal! h/<\\([^ ]\\{-}\\)\\%( .\\{-}\\)\\=>\\_.\\{-}\\zs\\ze\\%(\\n[ \\t]*\\/\\/\\/ \\)\\=<\\/\\1>\<cr>"
  startinsert
endfunction

"<\([^ ]\{-}\)\%( .\{-}\)\=>\_.\{-}\zs\ze\%(\n[ \t]*\/\/\/ \)\=<\/\1>"

function! <SID>CS_Comment_Prev()
  if b:i > 0
    let b:i = b:i - 1
  else
    let b:i = b:n
  endif

  exec "normal! `" . nr2char(b:i + 97)
  exec "normal! h/<\\([^ ]\\{-}\\)\\%( .\\{-}\\)\\=>\\_.\\{-}\\zs\\ze\\%(\\n[ \\t]*\\/\\/\\/ \\)\\=<\\/\\1>\<cr>"
  startinsert
endfunction

function! <SID>CS_Comment_BS()
  let l:a_back = @a

  " get the beginning of the line to this point
  y a
  let @a = strpart(@a, 0, col('.') - 1)
  " delete the whole line if we're backspacing over the ///
  if (@a =~ "[ \t]*/// $")
    exe "normal! d0"
  elseif (col('.') == col('$'))
    " hack to fix the can't backspace over last character if coming from ctrl-o bug (sigh)
    exe "normal! Xl"
  endif

  let @a = l:a_back
endfunction
