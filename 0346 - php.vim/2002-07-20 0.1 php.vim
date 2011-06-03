" Vim indent file
" Language:	Php
" Author:	Miles Lott <milosch@phpgroupware.org>
" URL:		http://milosch.dyndns.org/php.vim
" Last Change:	2001 Sep 08

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetPhpIndent()
setlocal indentkeys+=0=,0),=EO

" Only define the function once.
if exists("*GetPhpIndent")
  finish
endif

function GetPhpIndent()
  " Get the line to be indented
  let cline = getline(v:lnum)

  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)
  " Hit the start of the file, use zero indent.
  if lnum == 0
    return 0
  endif
  let line = getline(lnum)
  let ind = indent(lnum)

  " Indent after php open tags
  if line =~ '<?php'
    let ind = ind + &sw
  endif
  if cline =~ '\s*?>'
    let ind = ind - &sw
  endif

  " Indent blocks enclosed by {} or ()
  if line =~ '[{(]\s*\(#[^)}]*\)\=$'
    let ind = ind + &sw
  endif
  if cline =~ '^\s*[)}]'
    let ind = ind - &sw
  endif

  return ind
endfunction
