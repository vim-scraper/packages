" aspvbs indent file
" Language:	ASPVBS script
" Maintainer:	Tilmann Bitterberg (transcode at tibit dot org)
" Last Change:	2005 May 05
" History: 0.1 initial version
"          0.2 Incorporated case insensity fixes by Tavish Robinson
"              (trobinson at planetsoftware dot com)
"              added indent on ending keywords
"              added class keyword
"              buffer previous line (speed)
"          0.3 context based indenting -- the smart one --:
"              - if in an html context, the indent function for html will be
"                called to calculate the indent
"              - when you close an ASP block with %>,the indent of a new html
"                block depends on a previous html block
"              - when you open a new ASP block with <%, the indent of a
"                previous block is continued 
"              - lines starting with <% will have the same indent as a
"                previous line starting with <%
"              - for lines with <html><%..%></html> the html indenter wins
"                and these lines do not affect later ASP indent
"              - a single %> will be indented to match its opening <%
"              Problems:
"              - very likely to fail in most cases horribly
"              - <% and %> inside comments have odd effects
"          0.4 Bugfixes:
"              - make indent trigger on 'End' etc. and not just 'end'
"              - fix a bug with two consecutive ASP blocks with
"                no HTML in between

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif

runtime! indent/html.vim
" html.vim will define did_indent

setlocal indentexpr=GetAspVbsIndent()
setlocal indentkeys=o,O,*<Return>,=~wend,=~next,=~else,=~case,=~next,=~loop,=~end,=~select,<>>

" Only define the function once.
if exists("*GetAspVbsIndent")
  finish
endif

" something went wrong and we don't have the html indenter avialable
if !exists("*HtmlIndentGet")
  function! HtmlIndentGet(lnum)
    return indent(a:lnum)
  endfunction
endif

let s:mdebug = 0

if exists("g:aspvbs_debug")
  let s:mdebug = 1
endif

function! <SID>DEBUG(mstr)
  if s:mdebug == 1
    let tempvar = input(a:mstr)
  endif
endfunction

" Add or take away a 'shiftwidth' after keywords
function! <SID>GetCurrentIndent(prev_lnum)

  let lnum           = prevnonblank(a:prev_lnum - 1)
  let curr_line      = getline(v:lnum)
  let prev_line      = getline(lnum)
  let prev_prev_line = getline(prevnonblank(lnum - 1))

  let ind            = indent(lnum)

  " Add it additionally for a line whose previous line ended  with '_' 
  if prev_line =~ '_$' && prev_prev_line !~ '_$' 
    let ind = ind + &sw
  endif
  if prev_line !~ '_$' && prev_prev_line =~ '_$' 
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

  ""let ttt = input ("My prev line" . prev_line)
  " complete if. code after then -> no indent
  if prev_line =~ '^\c\s*\<if\>.\+\<then\>\s*\S\+' && prev_line !~ '%>\s*$'
    "let ttt = input (prev_line)
    let ind = ind - &sw 
  endif

  if curr_line =~ '^\c\s*\(end\s\+select\)'
    " because the end will already be indented by -1
    " let ind = ind - &sw * 2
    let ind = ind - &sw
  endif

  let close_tags =              '^\c\s*\<\('
  let close_tags = close_tags . 'end\(\s\+\(if\|function\|with\|sub\|property\|class\)\)\?\|'
  let close_tags = close_tags . 'wend\|else\|elseif\|next\|loop\|case'
  let close_tags = close_tags . '\)\>'

  " Subtract a 'shiftwidth' on a endif, endwhile, etc
  if curr_line =~ close_tags
    let ind = ind - &sw
  endif

  if ind < 0
    let ind = 0
  endif

  return ind
endfunction



" finds the indent of the previous block
function! <SID>GetPreviousIndent(lnum)
  let lnum = a:lnum

  let ind = 0

  while lnum > 0
    let mstr = substitute(getline(lnum), "<%.*%>", "", "")
    if mstr =~ '%>' 
      break
    endif
    let lnum = prevnonblank(lnum -1)
  endwhile

  let mline = getline(lnum)
  if mline =~ '^\s*%>\s*$' 
    call <SID>DEBUG("1) match:^\s*%>\s*$"." num:".lnum." line: ".mline)
    let ind = <SID>GetCurrentIndent(lnum)
  elseif mline =~ '%>\s*$'
    call <SID>DEBUG("2) match:%>\s*$"." num:".lnum." line: ".mline)
    let ind = <SID>GetCurrentIndent(lnum+1)
  elseif mline =~ '^\s*%>'
    call <SID>DEBUG("3) match:^\s*%>"." num:".lnum." line: ".mline)
    let ind = <SID>GetCurrentIndent(lnum)
  else
    call <SID>DEBUG("4) else "."line: ".mline)
  endif

  " no previous block -> no indent
  if lnum == 0 
    return 0
  endif

  return ind
endfunction



function! GetAspVbsIndent()

  let ind = 0

  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " At the start of the file use zero indent.
  if lnum == 0
    call <SID>DEBUG ("1. beginning of file.")
    return 0
  endif

  let curr_line = getline(v:lnum)

  " indent which start with a <% -- preserve previous indent
  if curr_line =~ '^\s*<%'
    call <SID>DEBUG ("2. Doing '^\s*<%' indent.")
    let mpair = search('%>\s*$', 'bW')
    " let mpair = searchpair('<%', '', '%>', 'nbW')
    "let tempvar = input("AAA: " .mpair." ")
    return indent(mpair)
  endif

  " a single %> will be placed at the same indent as the opening <%
  if curr_line =~ '^\s*%>'
    call <SID>DEBUG ("3. Doing '^\s*%>' indent.")
    return indent(search('<%', 'bW'))
  endif

  let prev_line      = getline(lnum)

  " if we are in a html block
  if 0 == searchpair('<%', '', '%>', 'nWb') " && 0 == searchpair('<%', '', '%>', 'nW')
    call <SID>DEBUG ("4. Doing HTML indent.")
    " try to pickup the previous HTML indentation
   "let a = input (searchpair('<%', '', '%>', 'nWb') ."  ".searchpair('<%', '', '%>', 'nW'))

    if prev_line =~ '%>' && (prev_line !~ '<%.*%>' || prev_line =~ '^\s*<%.*%>\s*$')
      let ind1 =  0 "search('%>', 'bW')
      let ind2 = searchpair('%>', '', '<%', 'bW')
      let ind3 = searchpair('<%', '', '%>', 'bW')
      "let tempvar = input ("fetching from ind1 ".ind1." ind2 " . ind2 . " ind3 ".ind3 )
      " found another opening block just above the current one
      if getline(prevnonblank(ind3-1)) =~ '%>\s*$'
	let ind3 = search('<%', 'bW')
      endif
      if getline(ind3) =~ '\S>\s*<%'
	let ind3 = ind3 + 1
      endif
      let ind = HtmlIndentGet(ind3)
      if curr_line =~ '^\s*</'
	let ind = ind - &sw
      endif
    else
      let ind = HtmlIndentGet(v:lnum)
    endif
    if ind < 0 
      ind = 0
    endif
    return ind
  endif

  " the user opened a new ASP block
  if prev_line =~ '<%'
    call <SID>DEBUG ("5. Doing new asp BLOCK indent.")
    return <SID>GetPreviousIndent(lnum)
  endif

  " let tempvar = input ("6. Doing current indent.")
  let ind = <SID>GetCurrentIndent(v:lnum)

  "call <SID>DEBUG("Final ind: " .ind)
  return ind

endfunction

" vim:sw=2
