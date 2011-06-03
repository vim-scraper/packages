" vi:set ts=4 sts=2 sw=2 tw=0 nowrap et:
"=============================================================================
" What Is This: FtpComplete
" File: ftpcmpl.vim
" Author: Yasuhiro Matsumoto <mattn_jp@hotmail.com>
" Last Change: Mon, 12 Apr 2004
" Version: 0.1
" Usage:
"     :e ftp://ftp.vim.org/pub/vim/patches/
"     type <c-g> <tab>
"     :e ftp://ftp.vim.org/pub/vim/patches/5.7.001
"     type <c-g> <c-n>
"     :e ftp://ftp.vim.org/pub/vim/patches/5.7.002
"     type <c-g> <c-p>
"     :e ftp://ftp.vim.org/pub/vim/patches/5.7.001

" candidate for ftp directory or file
let s:ftp_complete_candidate = ''
" count of candidate
let s:ftp_complete_candcount = -1
" current index of candidate
let s:ftp_complete_candindex = -1
" previous input
let s:ftp_complete_previnput = ''

function! FtpCompleteList(index)
  " cmdline was changed
  let changed = 0

  " get current context
  let url = getcmdline()
  let before = strpart(url, 0, getcmdpos()-1)
  let after = strpart(url, getcmdpos()-1)
  let url = substitute(before, '^.*\s\(ftp://[^\/]\+\/.*\)$', '\1', '')

  " url is terminated with /
  if url =~ '^.*[^\/]$'
    let word = substitute(url, '^.*\/\([^\/]\+\)$', '\1', '')
    let preword = word
  else
    let preword = ''
  endif
  " if previous input is not current input, this treat 'changed!'
  if s:ftp_complete_previnput != preword || s:ftp_complete_candindex == -1
    let changed = 1
  endif

  if a:index == 0 || changed == 1
    " if first complete...
    let s:ftp_complete_candidate = ''
    let s:ftp_complete_candcount = 0
    let s:ftp_complete_candindex = 0
    if preword != ''
      echohl MoreMsg
      echo 'Now listing ' . url . '*'
      echohl None
      let s:ftp_complete_candidate = system("curl -s -l ".url." | grep ^".word)
      let s:ftp_complete_candcount = strlen(substitute(s:ftp_complete_candidate, "[^\<NL>]*\<NL>\\?", 'a', 'g'))
    else
      echohl MoreMsg
      echo 'Now listing ' . url
      echohl None
      let s:ftp_complete_candidate = system("curl -s -l ".url)
      let s:ftp_complete_candcount = strlen(substitute(s:ftp_complete_candidate, "[^\<NL>]*\<NL>\\?", 'a', 'g'))-2
    endif
    let s:ftp_complete_candindex = 0
    redraw
    let ret = matchstr(s:ftp_complete_candidate, "^[^\<NL>]*")
  elseif a:index == -1
    " previous complete
    let s:ftp_complete_candindex = s:ftp_complete_candindex - 1
    if s:ftp_complete_candindex < 0
      let s:ftp_complete_candindex = s:ftp_complete_candcount
    endif
    if s:ftp_complete_candindex == 0
      let ret = matchstr(s:ftp_complete_candidate, "^[^\<NL>]*")
    else
      let ret = substitute(s:ftp_complete_candidate, "^\\%([^\<NL>]*\<NL>\\)\\{" . s:ftp_complete_candindex . "}\\([^\<NL>]*\\).*", '\1', '')
    endif
  elseif a:index == 1
    " next complete
    let s:ftp_complete_candindex = s:ftp_complete_candindex + 1
    if s:ftp_complete_candindex >= s:ftp_complete_candcount
      let s:ftp_complete_candindex = 0
    endif
    if s:ftp_complete_candindex == 0
      let ret = matchstr(s:ftp_complete_candidate, "^[^\<NL>]*")
    else
      let ret = substitute(s:ftp_complete_candidate, "^\\%([^\<NL>]*\<NL>\\)\\{" . s:ftp_complete_candindex . "}\\([^\<NL>]*\\).*", '\1', '')
    endif
  endif

  if s:ftp_complete_candcount > 0
    let ret = substitute(ret, "\r", "", "g")
    let s:ftp_complete_previnput = ret
    let ret = substitute(preword, ".", "\<c-h>", "g") .ret
  else
    let ret = ''
  endif
  return ret
endfunction

cnoremap <c-g><tab> <c-r>=FtpCompleteList(0)<cr>
cnoremap <c-g><c-n> <c-r>=FtpCompleteList( 1)<cr>
cnoremap <c-g><c-p> <c-r>=FtpCompleteList(-1)<cr>
