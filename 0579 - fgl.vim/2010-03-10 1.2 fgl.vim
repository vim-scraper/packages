" Description: Informix 4GL/4Js 4GL indent script  
" Author:      Timothy Kim (cool.msg@hotmail.com)
" Update:      Wed Mar 10 15:09:55 EST 2010

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal indentexpr=GetFglIndent(v:lnum)
setlocal indentkeys&
setlocal indentkeys+==~main,=~else,=~end,=~when,=~otherwise,=~on,=~command,=~before,=~after,=~input,=~construct,=~menu,=~case,=~globals,=~order,=~format,=~page,=~first,=~function,=~report,=~field,=~execute,=~call,=~prepare,=~declare,=~let,=~define,=~open,=~if,=~returning

" Only define the function once.
if exists("*GetFglIndent")
    finish
endif

function GetFglIndent(lnum)
    let this_line = getline(a:lnum)
    let lnum = prevnonblank(a:lnum - 1)
    let previous_line = getline(lnum)

    " Hit the start of the file
    " Let function/report/reprot sub-blocks starts on the first column always
    if lnum == 0 || 
       \ this_line =~# '^\s*\<\(main\|format\|output\)\>\s*$' ||
       \ this_line =~# '^\s*\<\(before\|after\)\>\s*group\s*of.*$' ||
       \ this_line =~# '^\s*\<\(function\|report\|first\|page\)\>' 
       return 0
    endif

    let ind = indent(lnum)

    if b:mult_line_end == 1
       let ind = ind - &sw
       let b:mult_line_end = 0
    endif   

    if b:mult_line == 1 && this_line !~# '^.*,\s*$'
       let b:mult_line = 0
       let b:mult_line_end = 1
    endif   

    " Add
    if  previous_line =~# '^\s*call .*,\s*$' ||
        \ previous_line =~# '^\s*define .*,\s*$' ||
        \ previous_line =~# '^\s*open .*using\s*$' ||
        \ previous_line =~# '^\s*put .*from\s*$' ||
        \ previous_line =~# '^\s*let .*,\s*$' ||
        \ previous_line =~# '^\s*let .*=\s*$' ||
        \ previous_line =~# '^\s*print column .*,\s*$' ||
        \ previous_line =~# '^\s*execute .*using\s*$' ||
        \ previous_line =~# '^\s*output\s*$' 

      let ind = ind + &sw
      let b:mult_line = 1

    elseif previous_line =~# '^\s*\<\(main\|function\|report\|case\|if\|else\|do\|for\|while\|otherwise\|command\|before\|after\|when\|on\|format\|page\s\+header\|first\|foreach\|menu\|input\|construct\|options\|globals\)\>' 
      let ind = ind + &sw

      if previous_line =~# '^\s*\<\(menu\|case\|input\|construct\)\>\s*.*$'
         let b:no_indent=1 
      endif

    elseif previous_line =~# '^.*record\s*$' && previous_line !~# '^\s*#'
          "================================================
          "variable record
          "   field like table.column
          "end record
          "================================================
         let ind = ind + &sw
    endif

    " Subtract
    if this_line =~# '^\s*\<end\>\s\+\<\(case\|menu\|input\|construct\|globals\)\>'
       if previous_line !~# '^\s*\<\(case\|menu\|input\|construct\|globals\)>'
          let ind = ind - 2 * &sw
       else
          let ind = ind - &sw
       endif
    elseif this_line =~# '^\s*\<\(else\|end\)\>\s*.*$'
          let ind = ind - &sw

    elseif this_line =~# '^\s*\<\(when\|otherwise\|command\|on\|before\s\+\<\(input\|menu\|construct\|field\)\>\|after\s\+\<\(input\|construct\|field\)\>\)\>\s*.*$'
       "================================================
       "unindent for sub command blocks
       "================================================
       if b:no_indent==1 
          let b:no_indent=0
       else
          let ind = ind - &sw
       endif
    elseif this_line =~# '^\s*\<then\>\s*.*$'
       "================================================
       "if expression
       "then
       "   statement ...
       "================================================
       if previous_line =~# '^\s*\<if\>\s\+.*$'
          let ind = ind - &sw
       endif
    elseif previous_line =~# '^\s*end record\s*$'
       let ind = ind - &sw
    endif

    return ind
endfunction
