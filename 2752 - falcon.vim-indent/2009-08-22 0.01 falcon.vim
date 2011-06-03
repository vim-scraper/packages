" Vim indent file
" Language:	Falcon
" Version:	0.01
" Last Change:	2003 Feb 04
" Maintainer:	Brent A. Fulgham <bfulgham@debian.org>

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentkeys+==~case,=~catch,=~default,=~elif,=~else,=~end,=~\"

" Define the appropriate indent function but only once
setlocal indentexpr=FalconGetIndent()
if exists("*FalconGetIndent")
  finish
endif

function FalconGetIndent()
  " Get the line to be indented
  let cline = getline(v:lnum)

  " Don't reindent comments on first column
  if cline =~ '^/\[/\*]'
    return 0
  endif

  "Find the previous non-blank line
  let lnum = prevnonblank(v:lnum - 1)
  "Use zero indent at the top of the file
  if lnum == 0
    return 0
  endif

  let prevline=getline(lnum)
  let ind = indent(lnum)
  let chg = 0

  " If previous line was a comment, use its indent
  if prevline =~ '^\s*//'
   return ind
  endif

  " If the start of the line = ", then indent to the previous lines 1st "
  if cline =~? '^\s*"'
    let chg = chg + &sw
  endif

  " If previous line started with a " and this one doesn't, unindent
  if prevline =~? '^\s*"' && cline =~? '^\s*'
    let chg = chg - &sw
  endif

  " If previous line was a 'define', indent
  "if prevline =~? '^\s*\(case\|catch\|class\|enum\|default\|elif\|else\|function\|if \(\(:\)\@!.\)*$\|loop\|select\|switch\|while\)'
  if prevline =~? '^\s*\(case\|catch\|class\|enum\|default\|elif\|else\|function\|if.*"[^"]*:.*"\|if \(\(:\)\@!.\)*$\|loop\|select\|switch\|while\)'
    let chg = &sw
  " If previous line opened a parenthesis, and did not close it, indent
  elseif prevline =~ '^.*(\s*[^)]*\((.*)\)*[^)]*$'
    return = match( prevline, '(.*\((.*)\|[^)]\)*.*$') + 1
  "elseif prevline =~ '^.*(\s*[^)]*\((.*)\)*[^)]*$'
  elseif prevline =~ '^[^(]*)\s*$'
    " This line closes a parenthesis.  Find opening
    let curr_line = prevnonblank(lnum - 1)
    while curr_line >= 0
      let str = getline(curr_line)
      if str !~ '^.*(\s*[^)]*\((.*)\)*[^)]*$'
	let curr_line = prevnonblank(curr_line - 1)
      else
	break
      endif
    endwhile
    if curr_line < 0
      return -1
    endif
    let ind = indent(curr_line)
  endif

  " If previous line ended in a comma, indent again.
  if prevline =~? ',\s*$'
    let chg = chg + &sw
  endif


  " If a line starts with end, un-indent (even if we just indented!)
  if cline =~? '^\s*\(case\|catch\|default\|elif\|else\|end\)'
    let chg = chg - &sw
  endif

  return ind + chg
endfunction

" vim:sw=2 tw=130
