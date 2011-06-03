" Vim global plugin for smart matching of trailing whitespace
" Last Change:	2009 May 15
" Maintainer:	Konstantin Lepa <konstantin.lepa@gmail.com>
" License:	This file is placed in the public domain.
" Version:      1.0.1
"
" Changes {{{
" 1.0.1 2009-05-16
"   Fixed bug in parsing g:sme_filelist.
"
"}}}

if exists("g:loaded_smartmatcheol")
  finish
endif
let g:loaded_smartmatcheol = 1

let s:save_cpo = &cpo
set cpo&vim

hi WhiteSpaceEOL ctermbg=darkred guibg=lightred

function s:SmartMatchWhiteSpaceEOL(fname, fext)
  if exists("g:sme_extlist")
    for ext in g:sme_extlist | if ext == a:fext | return 1 | endif | endfor
  endif
  if exists("g:sme_filelist")
    for fn in g:sme_filelist | if fn == a:fname | return 1 | endif | endfor
  endif
  return 0
endfunction

function s:MatchAdd(fname, fext)
  if s:SmartMatchWhiteSpaceEOL(a:fname, a:fext)
    let b:m1=matchadd('WhiteSpaceEOL', '\s\+$\| \+\ze\t', -1)
    if &textwidth > 0
      let b:m2=matchadd('ErrorMsg', '\%>' . &textwidth . 'v.\+', -1)
    endif
  endif
endfunction

function s:MatchDel(fname, fext)
  if s:SmartMatchWhiteSpaceEOL(a:fname, a:fext)
    if exists("b:m1") | call matchdelete(b:m1) | unlet b:m1 | endif
    if exists("b:m2") | call matchdelete(b:m2) | unlet b:m2 | endif
  endif
endfunction

au BufWinEnter * call s:MatchAdd(expand("<amatch>:t:r"), expand("<amatch>:e"))
au BufWinLeave * call s:MatchDel(expand("<amatch>:t:r"), expand("<amatch>:e"))

let &cpo = s:save_cpo

