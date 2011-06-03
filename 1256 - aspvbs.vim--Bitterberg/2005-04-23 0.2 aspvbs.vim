" aspvbs indent file
" Language:	ASPVBS script
" Maintainer:	Tilmann Bitterberg (transcode at tibit dot org)
" Last Change:	2005 April 9
" History: 0.1 initial version
"          0.2 Incorporated case insensity fixes by Tavish Robinson
"              (trobinson at planetsoftware dot com)
"              added indent on ending keywords
"              added class keyword
"              buffer previous line (speed)

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetAspVbsIndent()
setlocal indentkeys=o,O,*<Return>,=wend,=next,=else,=case,=next,=loop,=end,=select

" Only define the function once.
if exists("*GetAspVbsIndent")
  finish
endif

function GetAspVbsIndent()
  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " At the start of the file use zero indent.
  if lnum == 0
    return 0
  endif

  let prev_line = getline(lnum)

  " Add a 'shiftwidth' after keywords
  " Add it additionally for a line whose previous line ended  with '_' 
  let ind = indent(lnum)

  if prev_line =~ '_$' && getline(prevnonblank(lnum - 1)) !~ '_$' 
    let ind = ind + &sw 
  endif

  if prev_line !~ '_$' && getline(prevnonblank(lnum - 1)) =~ '_$' 
    let ind = ind - &sw 
  endif

  if prev_line =~ '^\c\s*\(select\s\+case\)\>'
    let ind = ind + &sw * 2
  endif

  let open_tags =             '^\c\s*\<\('
  let open_tags = open_tags . 'if\|elseif\|else\|while\|for\|do\|with\|case\|'
  let open_tags = open_tags . '\(private\s\+\|public\s\+\|\)\(class\|sub\|function\|property\)'
  let open_tags = open_tags . '\)\>'

  if prev_line =~ open_tags
    let ind = ind + &sw
  endif

  " complete if. code after then -> no indent
  if prev_line =~ '^\c\s*\<if\>.\+\<then\>\s*\S\+'
    let ind = ind - &sw 
  endif

  if getline(v:lnum) =~ '^\c\s*\(end\s\+select\)'
    " because the end will already be indented by -1
    " let ind = ind - &sw * 2
    let ind = ind - &sw
  endif

  let close_tags =              '^\c\s*\<\('
  let close_tags = close_tags . 'end\(\s\+\(if\|function\|with\|sub\|property\|class\)\)\?\|'
  let close_tags = close_tags . 'wend\|else\|elseif\|next\|loop\|case'
  let close_tags = close_tags . '\)\>'

  " Subtract a 'shiftwidth' on a endif, endwhile, etc
  if getline(v:lnum) =~ close_tags
    let ind = ind - &sw
  endif

  return ind
endfunction

" vim:sw=2
