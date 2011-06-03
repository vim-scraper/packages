" Vim indent file
" Language:	RPL/2
" Version:	0.3
" Last Change:	2005 October 22
" Maintainer:	BERTRAND Joël <joel.bertrand@systella.fr>

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentkeys+==~end,=~case,=~if,=~then,=~else,=~do,=~until,=~while,=~repeat,=~select,=~default,=~for,=~start,=~next,=~step,<<>,<>>

" Define the appropriate indent function but only once
setlocal indentexpr=RplGetFreeIndent()
if exists("*RplGetFreeIndent")
  finish
endif

function RplGetIndent(lnum)
  let ind = indent(a:lnum)
  let prevline=getline(a:lnum)
  " Strip tail comment
  let prevstat=substitute(prevline, '!.*$', '', '')

  " Add a shiftwidth to statements following if, iferr, then, else, elseif,
  " case, select, default, do, until, while, repeat, for, start
  if prevstat =~? '\<\(if\|iferr\|do\|while\)\>' && prevstat =~? '\<end\>'
  elseif prevstat =~? '\(^\|\s\+\)<<\($\|\s\+\)' && prevstat =~? '\s\+>>\($\|\s\+\)'
  elseif prevstat =~? '\<\(if\|iferr\|then\|else\|elseif\|select\|case\|do\|until\|while\|repeat\|for\|start\|default\)\>' || prevstat =~? '\(^\|\s\+\)<<\($\|\s\+\)'
    let ind = ind + &sw
  endif

  if prevstat =~? '/\*'
    let ind = ind + 1
  endif

  " Subtract a shiftwidth from then, else, elseif, end, until, repeat, next,
  " step
  let line = getline(v:lnum)
  if line =~? '^\s*\(then\|else\|elseif\|until\|repeat\|next\|step\|default\|end\)\>'
    let ind = ind - &sw
  elseif line =~? '^\s*>>\($\|\s\+\)'
    let ind = ind - &sw
  endif

  if prevstat =~? '\*/'
    let ind = ind - 1
  endif
  return ind
endfunction

function RplGetFreeIndent()
  " Find the previous non-blank line
  let lnum = prevnonblank(v:lnum - 1)

  " Use zero indent at the top of the file
  if lnum == 0
    return 0
  endif

  let ind=RplGetIndent(lnum)
  return ind
endfunction

" vim:sw=2 tw=130
