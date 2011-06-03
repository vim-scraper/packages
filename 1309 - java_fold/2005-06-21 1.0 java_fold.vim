" Vim folding file
" Language:	Java
" Author:	Jorrit Wiersma
" Last Change:	2005 Jun 21
" Version:	1.0

setlocal foldmethod=expr
setlocal foldexpr=GetJavaFold(v:lnum)
setlocal foldtext=JavaFoldText()


function! JavaFoldText()
  let nnum = nextnonblank(v:foldstart + 1)
  let nline = getline(nnum)
  while nline =~ "\*"
      let nnum = nextnonblank(nnum + 1)
      let nline = getline(nnum)
  endwhile
  return nline
endfunction


function! GetJavaFold(lnum)
    " Determine folding level in Java source based on javadoc comments
    " and indentation
    let line = getline(a:lnum)
    let ind  = indent(a:lnum)

    " Ignore blank lines
    if line =~ '^\s*$'
	return "="
    endif

    " If a line starts a javadoc comment, start a new fold based on the
    " indentation level
    if line =~ '^\s*/\*\*'
	return ">" . (ind / &sw )
    endif

    let pnum = prevnonblank(a:lnum - 1)

    if pnum == 0
	" Hit start of file
	return 0
    endif

    " If the previous line has foldlevel zero, and we haven't increased
    " it, we should have foldlevel zero also
    if foldlevel(pnum) == 0
	return 0
    endif

    " If none of the above apply, keep the indentation
    return "="

endfunction

