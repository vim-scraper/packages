" vim: ts=2 sw=2 fdm=marker cms=\ "%s
" Plugin: OCamlHelp
" Version: 0.1
" $Id: ocamlhelp.vim,v 1.5 2003/09/12 20:02:44 andrew Exp $
" Author: Andrew Rodionoff (arnost AT mail DOT ru)
"
" Description:
" Objective Caml txt-format reference manual reader, with syntax coloring of
" examples. :edit ocaml-<current version>-refman.txt, press <Enter> on
" highlighted section numbers to jump.
"
" TODO: vimhelp-like keyword lookup.
"

augroup OCamlHelpTxt
  au!
  au BufNewFile,BufRead *caml*.txt call <SID>Setup()
augroup END

fun! s:Setup() "{{{
  set ft=ochelp
  syn match ochelpXref '\<[0-9]\+\(\.[0-9]\+\)\+\>'
  syn match ochelpSectionDelim '^\*=\(\*=\)\+\*\?\s*$'
  syn match ochelpSubSectionDelim '^===\+\s*$'
  syn match ochelpSubSubSectionDelim '^---\+\s*$'
  syn match ochelpHeadline '^\s*\*\*\+\s*$'
  syn include @OCaml syntax/ocaml.vim
  syn region ochelpExample matchgroup=ochelpExampleDelim start='^<<' end='^>>' contains=@OCaml keepend
  syn sync minlines=40
  hi def link  ochelpXref	PreProc
  hi def link  ochelpSectionDelim	PreProc
  hi def link  ochelpSubSectionDelim	Type
  hi def link  ochelpSubSubSectionDelim	Constant
  hi def link  ochelpHeadline		Statement
  hi def link  ochelpExample	String
  hi def link  ochelpExampleDelim	Ignore
  let b:current_syntax = 'ochelp'
  map <silent> <buffer> <CR> :call <SID>Enter()<CR>
endfun "}}}

fun! s:Enter() "{{{
  if synIDattr(synID(line('.'), col('.'), 1), 'name') =~ 'Xref$'
    let l:isk = &isk
    set isk=48-57,.
    let l:word = expand('<cword>')
    let &isk = l:isk
    if l:word != ''
      silent! exec 'ijump! /^' . escape(l:word, '.') . '/'
    endif
  endif
endfun "}}}
