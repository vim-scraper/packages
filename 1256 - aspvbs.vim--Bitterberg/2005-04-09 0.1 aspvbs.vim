" aspvbs indent file
" Language:	ASPVBS script
" Maintainer:	Tilmann Bitterberg <transcode@tibit.org>
" Last Change:	2005 April 9

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetAspVbsIndent()
setlocal indentkeys=o,O,*<Return>

" Only define the function once.
if exists("*GetAspVbsIndent")
  finish
endif

function GetAspVbsIndent()
  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " If the current line doesn't start with '\' and below a line that starts
  " with '\', use the indent of the line above it.
  "  if getline(v:lnum) !~ '^\s*\\'
  "    while lnum > 0 && getline(lnum) =~ '^\s*\\'
  "      let lnum = lnum - 1
  "    endwhile
  "  endif

  " At the start of the file use zero indent.
  if lnum == 0
    return 0
  endif


  " Add a 'shiftwidth' after keywords
  " Add it additionally for a line whose previous line ended  with '_' 
  let ind = indent(lnum)

  if getline(lnum) =~ '_$' && getline(prevnonblank(lnum - 1)) !~ '_$' 
    let ind = ind + &sw 
  endif

  if getline(lnum) !~ '_$' && getline(prevnonblank(lnum - 1)) =~ '_$' 
    let ind = ind - &sw 
  endif

  if getline(lnum) =~ '^\s*\(select\s\+case\)\>'
    let ind = ind + &sw * 2
  endif

  let open_tags =             '^\s*\<\('
  let open_tags = open_tags . 'if\|elseif\|else\|while\|for\|do\|with\|case\|'
  let open_tags = open_tags . '\(private\s\+\|public\s\+\|\)\(sub\|function\|property\s\+\(get\|set\|let\)\)'
  let open_tags = open_tags . '\)\>'

  if getline(lnum) =~ open_tags
    let ind = ind + &sw
  endif

  " complete if. code after then -> no indent
  if getline(lnum) =~ '^\s*\<if\>.\+\<then\>\s*\S\+'
    let ind = ind - &sw 
  endif

  if getline(v:lnum) =~ '^\s*\(end\s\+select\)'
    let ind = ind - &sw * 2
  endif

  let close_tags =              '^\s*\<\('
  let close_tags = close_tags . 'end\s\+\(if\|function\|with\|sub\|property\|class\)\|'
  let close_tags = close_tags . 'wend\|else\|elseif\|next\|loop\|case'
  let close_tags = close_tags . '\)\>'

  " Subtract a 'shiftwidth' on a endif, endwhile, etc
  if getline(v:lnum) =~ close_tags
    let ind = ind - &sw
  endif

  return ind
endfunction

" vim:sw=2
