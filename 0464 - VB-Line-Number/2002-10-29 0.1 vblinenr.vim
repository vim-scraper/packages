" Find the line-number in a VB File
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 0.1

fun! s:FindVBLine( n )
  let endline=line('$')
  let linenr=search('^\cBEGIN')
  if linenr>0
      let level = 1
      let linenr = linenr +1
      while level>0 && linenr <= endline
          let txt = getline(linenr)
          if txt =~ '^\c\s*BEGIN'
              let level=level+1
          elseif txt =~ '^\c\s*END'
              let level=level-1
          endif
          let linenr=linenr+1
      endwhile
  else
      let linenr=1
  endif
"  echo "..".linenr.".."
  let i = 0
  while i < a:n && linenr <= endline 
	if getline(linenr) !~ '^Attribute '
	  let i=i+1
	endif
	let linenr=linenr+1
  endwhile
  while linenr <= endline  && getline(linenr) =~ '^Attribute '
	let linenr=linenr+1
  endwhile
  exe linenr
endfun

map <buffer> gvb :<c-U>call <SID>FindVBLine(v:count)<cr>

com! -nargs=1 VGO :call s:FindVBLine( <f-args> )


