" Description: Informix 4GL/4Js 4GL indent script	
" Author:      Timothy Kim	(timk@tecsys.com)
" URL:		   http://www.tecsys.com
" Update:      Mon Mar 10 16:19:39 EST 2003

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal indentexpr=GetFglIndent(v:lnum)
setlocal indentkeys&
setlocal indentkeys+==~main,=~else,=~end,=~when,=~otherwise,=~on,=~command,=~before,=~after,=~input,=~construct,=~menu,=~case,=~globals,=~then,=~select,=~from,=~where,=~into,=~order,=~format,=~page,=~first,=~function,=~report,=~field

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
       \ this_line =~# '^\s*\<\(function\|report\|first\|page\)\>'
       return 0
    endif

    let ind = indent(lnum)

    " Add
    if previous_line =~# '^\s*\<\(main\|function\|report\|case\|if\|then\|else\|do\|for\|while\|otherwise\|command\|before\|after\|when\|on\|format\|page\s\+header\|first\|foreach\|menu\|input\|construct\|output\|options\|globals\|define\)\>'
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
    elseif this_line =~# '^\s*\<\(from\|where\|into\|order\)\>\s*.*$'
       "================================================
       "select fiels ...
       "   [into ...]
       "   [from ...]
       "   [where ...]
       "   [order by ...]
       "================================================
       if previous_line =~# '^\s*\<select\>\s\+.*$'
          let ind = ind + &sw
       endif
    elseif this_line =~# '^\s*\<select\>\s*.*$'
       "================================================
       "declare cursor ..... for
       "   select ...
       "================================================
       if previous_line =~# '^\s*\<declare\>\s\+.*$'
          let ind = ind + &sw
          let b:acc_ind = b:acc_ind + 1
       endif
    endif

    return ind
endfunction


