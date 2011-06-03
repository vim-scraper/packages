" Vim indent file
" Language: CMake (ft=cmake)
" Author:	Andy Cedilnik <andy.cedilnik@kitware.com>
" URL:		http://www.cmake.org
" Last Change:	Sun Mar 23 15:41:55 EST 2003

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=CMakeGetIndent(v:lnum)

" Only define the function once.
if exists("*CMakeGetIndent")
  finish
endif

fun! CMakeGetIndent(lnum)
  let this_line = getline(a:lnum)

  " Find a non-blank line above the current line.
  let lnum = a:lnum
  let lnum = prevnonblank(lnum - 1)
  let previous_line = getline(lnum)

  " Hit the start of the file, use zero indent.
  if lnum == 0
    return 0
  endif

  let ind = indent(lnum)

  " Add
  if previous_line =~? '^\s#'
    let ind = ind
  else
    if previous_line =~? '^\s*\(IF\|ELSE\|FOREACH\|MACRO\)\s*('
      let ind = ind + &sw
    endif
    if previous_line =~? '([^)]*\(#.*\)\=$'
      let ind = ind + &sw
    endif
  endif

  " Subtract
  if this_line =~? '^\s#'
    let ind = ind
  else
    if this_line =~? '^\s*\(ENDIF\|ELSE\|ENDFOREACH\|ENDMACRO\)\s*('
      let ind = ind - &sw
    endif
    if previous_line =~? '^[^(]*)\s*\(#.*\)\=$'
      let ind = ind - &sw
    endif
  endif

  return ind
endfun
