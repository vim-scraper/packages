" File: phrases.vim
" Last Change: 2004 Jun 04
" Maintainer: Klaus Bosau <kbosau@web.de>
" Version: 0.9

if exists("loaded_phrases")
  finish
endif

let loaded_phrases = 1
let s:cpo = &cpo | set cpo&vim

""""""""""""""""""""""""""""""""""""" color """"""""""""""""""""""""""""""""""""
exe substitute('0lightgray1yellow2cyan3green4darkgray5magenta6darkgreen7darkmagenta8darkred9black', '\v(\d)(\l+)', 'let s:bgc\1 = "\2" | ', 'g')
exe substitute('0black1black2black3black4black5black6black7white8white9white', '\v(\d)(\l+)', 'let s:fgc\1 = "\2" | ', 'g')
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

fun! s:Scan() range
  fun! s:Save()
    if match(s:set, '\<' . submatch(0) . '\>') == -1
      let s:set = s:set . submatch(0) . ' '
      let s:match{s:cnt} = submatch(0)
      let s:cnt = s:cnt + 1
    endif
    return submatch(0)
  endfun

  let rest = 'norm! ' . (line('.') - winline() + 1) . 'zt'
  let s:cnt = 0
  let s:set = ''
  let mod = &l:mod
  sil $ put _
  exe 'sil ' . a:firstline . ',' . a:lastline . 'y z'
  exe (a:lastline > a:firstline ? 'sil! ' . a:firstline . ',' . (a:lastline - 1) . 's/\n/ /g' : a:firstline)
  """""""""""""""""""""""""""""" pattern, language """""""""""""""""""""""""""""
  "sil! s/\v(<\a{6,20}>)%(.+\1)@=|%(\2.+)@<=(<\a{6,20}>)/\=s:Save()/g
  " single words, english (fast)
  "sil! s/\v(<[-[:alpha:]]{6,20}>)%(.+\1)@=|%(\2.+)@<=(<[-[:alpha:]]{6,20}>)/\=s:Save()/g
  " single words, any language (still fast)
  sil! s/\v(<[-[:alpha:] ]{6,20}>)%(.+\1)@=|%(\2.+)@<=(<[-[:alpha:] ]{6,20}>)/\=s:Save()/g
  " phrases, any language (noticeably slower)
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  sil d _ | sil put! z
  sil $ d _
  let &l:mod = mod
  unlet s:set
  exe rest

  exe a:lastline . 'norm! V' . a:firstline . 'GV'

  if s:cnt > 0
    sil! syn clear phrases
    exe substitute('0123456789', '.', 'sil! syn clear match& | ', 'g')
    exe 'syn region phrases start="^\%' . a:firstline . 'l" end="^\%' . (a:lastline + 1) . 'l" containedin=ALL contains=match0,match1,match2,match3,match4,match5,match6,match7,match8,match9'
    hi phrases gui=none guifg=black guibg=white
    let i = (s:cnt > 10 ? 9 : s:cnt - 1)
    while i >= 0
      exe 'syn match match' . i . ' "\<[-' . "'" . '[:alpha:]]*' . substitute(s:match{i}, ' ', '\\_s', 'g') . '[-' . "'" . '[:alpha:]]*\>" contained'
      exe 'hi match' . i . ' guifg=' . s:fgc{i} . ' guibg=' . s:bgc{i} . ' ctermfg=' . s:fgc{i} . ' ctermbg=' . s:bgc{i}
      let i = i - 1
    endwhile
  endif
endfun

fun! s:Clear()
  redir @z | sil! let | redir END
  let vars = substitute(@z, "^\n\\| [^\n]*.", ' ', 'g')
  exe 'sil! unlet! ' . substitute(substitute(vars, ' \%(match\)\@!\S\+', '', 'g'), '\<.', 'g:&', 'g')
  sil! syn clear phrases
endfun

vn <silent> <Cr> :call <SID>Scan()<Cr>
nn <silent> <Cr> :call <SID>Clear()<Cr>

let &cpo = s:cpo

" vim600:fo=crq12:com=n\:\":nojs:so=5:siso=10:nowrap:inde=:sts=2:sm:
