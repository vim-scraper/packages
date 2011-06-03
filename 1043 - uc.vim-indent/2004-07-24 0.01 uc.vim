" Vim indent file
" Language:	Intel Network processor Microcode
" Maintainer:	Gui Wei <gawain_g@citiz.net>

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetUcIndent()
setlocal nolisp
setlocal nosmartindent
setlocal nocindent
setlocal autoindent
setlocal indentkeys+==.begin,=.end,=.if,=.if_unsigned,=.elif,=.elif_unsigned,=.else,=.endif
setlocal indentkeys+==#if,=#ifdef,=#ifundef,=#elif,=#else,=#endif,=#macro,=#endm
setlocal indentkeys+==.while,=.while_unsigned,=.endw,=.repeat,=.until,=.until_unsigned,=.local,=.endlocal
setlocal sw=4	"intel recommended table width
setlocal ts=4
setlocal softtabstop=4
setlocal tw=78

" Define some stuff
" keywords grouped by indenting
let s:relative_indent = '^\s*\(\.if\|\.if_unsigned\|\.begin\|\.local\|\.elif\|\.elif_unsigned\|\.else\|\.while\|\.while_unsigned\|\.repeat\|#\s*macro\|#\s*if\|#\s*elif\|#\s*else\)\>'
let s:outdent = '^\s*\(\.else\|\.elif\|\.endif\|\.endw\|\.until\|\.until_unsigned\|\.end\|\.endlocal\|#\s*endm\|#\s*elif\|#\s*else\|#\s*endif\)\>'


" Only define the function once.
if exists("*GetUcIndent")
  finish
endif

function GetUcIndent()

  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " At the start of the file use zero indent.
  if lnum == 0
    return 0
  endif

  " Add a 'shiftwidth' after lines that start with an indent word
  let ind = indent(lnum)
  if getline(lnum) =~ s:relative_indent
    let ind = ind + &sw
  endif

  " Subtract a 'shiftwidth'
  if getline(v:lnum) =~ s:outdent 
    let ind = ind - &sw
  endif

  return ind
endfunction

" vim:sw=2
