" Utilities for C/C++ syntax files
" 
" CimpTabulateCase -- tabulates switch statements 
"
" Last Change:  09 May 2007
" Maintainer:   Henrik Öhman (spiiph)
"

" script loaded
if exists("s:loaded_cimp")
  echo "Cimp already loaded."
  finish
endif
let s:loaded_cimp = 1

" script clean up -- for development purposes
func! CimpCleanUp()
  delfunction CimpTabulateCase
  delfunction s:CimpSetCol
  delfunction s:CimpIncTabForm
  delfunction s:CimpEchoTabForm
  delfunction s:CimpGetCompact
  delfunction s:CimpGetTabulated
  delfunction s:CimpCheckOverflow
  delfunction s:CimpGetColumns
  unlet s:leading_space
  unlet s:case
  unlet s:statement
  unlet s:tabform
  unlet s:range
  unlet s:loaded_cimp
  unlet g:CimpAlignSpecial
  unlet g:CimpAlignSpecialPattern
  unlet g:CimpPadding
endfunc

let s:leading_space = "^\\s\\+"
let s:case = "^case \\w\\+:"
let s:statement = "^\\((.*)\\|\".*\"\\|[^;]\\)\\+;"

" 0 = tabulate, 1 = smart, 2 = compact
let s:tabform = 0
let s:range = [0, 0]

" align breaks in the last column
let g:CimpAlignSpecial = 1
let g:CimpAlignSpecialPattern = "\^return\\|\^break"

" column padding
let g:CimpPadding = 1

" set the width of a column in a column width list 
func s:CimpSetCol(cols, col, val)
  let l:temp = a:cols
  if a:col >= len(l:temp)
    let l:temp = add(l:temp, a:val)
  elseif a:val > l:temp[a:col]
    let l:temp[a:col] = a:val
  endif
  return l:temp
endfunc

" get a list of column widths
func s:CimpGetColumns(lines, oflines, cols)
  let l:cols = a:cols
  let l:n = 0
  while l:n < len(a:lines) - 1
    " in smart tabulation format, skip overflow lines
    if (s:tabform == 1) && a:oflines[l:n]
      let l:n += 1
      continue
    endif 

    let l:k = 0
    let l:col = l:k
    let l:temp = 0
    let l:line = a:lines[l:n]
    while l:k < len(l:line) - 1
      if (len(l:cols) <= l:k) || (l:cols[l:k] != -1)
        let l:temp = 0
        let l:col = l:k
      endif

      let l:temp += len(l:line[l:k]) + g:CimpPadding
        
      let l:cols = s:CimpSetCol(l:cols, l:col, l:temp) 
      let l:k += 1
    endwhile

    let l:n += 1
  endwhile
  return l:cols
endfunc

" check if a line overflows textwidth
func s:CimpCheckOverflow(line, cols, indent)
  if &textwidth != 0
    let l:pos = a:indent
    let l:n = 0
    let l:col = 0
    let l:temp = 0
    while l:n < len(a:line) - 1
      if l:n >= len(a:cols) 
        let l:pos += max([l:temp, l:col])  " add previously accumulated width 
        let l:pos += len(a:line[l:n]) + 1
        let l:col = 0
        let l:temp = 0
      elseif a:cols[l:n] > 0
        let l:pos += max([l:temp, l:col])  " add previously accumulated width 
        let l:col = a:cols[l:n]            " reset the current column width 
        let l:temp = len(a:line[l:n]) + 1  " reset the current text width
      else
        let l:temp += len(a:line[l:n]) + 1 " accumulate text width
      endif
      let l:n += 1
    endwhile

    let l:pos += max([l:temp, l:col])      " add previously accumulated width 
    let l:pos += len(a:line[l:n])

    if l:pos > &textwidth
      return 1
    endif
  endif 
  return 0
endfunc

" cyclic increment of the tabulation format
func s:CimpIncTabForm()
  if s:tabform >= 2
    let s:tabform = 0
  else
    let s:tabform += 1
  endif
endfunc

" echo tabulation format to user
func s:CimpEchoTabForm()
  if s:tabform == 0
    echo "tabulation"
  elseif s:tabform == 1
    echo "smart tabulation"
  elseif s:tabform == 2
    echo "compact tabulation"
  endif
endfunc

" get a compact line
func s:CimpGetCompact(line, indent, col1)
  if len(a:line) == 0
    echoerr "s:CimpGetCompact called with a:line of length 0"
    return ""
  endif

  let l:text = repeat(' ', a:indent)
  let l:text .= a:line[0]

  if len(a:line) > 1
    let l:text .= repeat(' ', a:col1 - len(a:line[0]))
    let l:n = 1
    while l:n < len(a:line) - 1
      let l:text .= a:line[l:n] . " "
      let l:n += 1
    endwhile
    let l:text .= a:line[l:n]
  endif
  return l:text
endfunc

" get a tabulated line
func s:CimpGetTabulated(line, cols, indent)
  if len(a:line) == 0
    echoerr "s:CimpGetTabulated called with a:line of length 0"
    return ""
  endif

  let l:n = 0
  let l:pos = a:indent 
  let l:text = repeat(' ', a:indent)

  while l:n < len(a:line) - 1
    if a:cols[l:n] > 0
      let l:pos += a:cols[l:n]
    endif
    let l:text .= a:line[l:n] 
    if (l:n >= len(a:line) - 2) || (a:cols[l:n + 1] > 0)
      let l:text .= repeat(' ', l:pos - len(l:text))
    else
      let l:text .= ' '
    endif
    let l:n += 1
  endwhile
  
  if g:CimpAlignSpecial && (a:line[l:n] =~ g:CimpAlignSpecialPattern)
    for n in range(l:n, len(a:cols) - 1)
      let l:pos += a:cols[n]
    endfor
    let l:text .= repeat(' ', l:pos - len(l:text))
  endif
  let l:text .= a:line[l:n]

  return l:text
endfunc

"
" tabulate case statements
"
" argument: a:1
"   "s*", 1 -- smart tabulation format
"   "c*", 2 -- compact format 
"   else    -- normal format
" no argument:
"   if a new range is used, reset to using normal format
"   otherwise cycle normal -> smart -> compact -> normal -> ...
func CimpTabulateCase(...) range
  let l:n = a:firstline
  let l:cols = []
  let l:lines = []
  let l:oflines = []
  let l:lcspecial = g:CimpAlignSpecial
  
  if (a:0 > 0)
    " if the user specified a tabulation format, use that 
    if (a:1 =~ "\^s") || (a:1 == 1) " smart
      let s:tabform = 1
    elseif (a:1 =~ "\^c") || (a:1 == 2) " compact
      let s:tabform = 2
    else 
      let s:tabform = 0
    endif
  elseif s:range != [a:firstline, a:lastline]
    " reset tabulation format if range has changed
    let s:tabform = 0
  else 
    " cycle tab format
    call s:CimpIncTabForm()
  endif

  call s:CimpEchoTabForm()

  let s:range = [a:firstline, a:lastline]

  " PASS 1 - identify columns and first column alignment
  while l:n <= a:lastline 
    let l:str = substitute(getline(l:n), s:leading_space, "", "")
    let l:col = 0
    let l:line = []

    " match case XYZ:
    let l:idx = matchend(l:str, s:case)
    if l:idx == -1
      let l:lines = add(l:lines, [l:str])
      let l:n += 1
      continue
    endif

    let l:cols = s:CimpSetCol(l:cols, l:col, l:idx + g:CimpPadding)
    let l:col += 1
    let l:line = add(l:line, strpart(l:str, 0, l:idx))
    let l:str = substitute(strpart(l:str, l:idx), s:leading_space, "", "")

    " match statements
    let l:idx = matchend(l:str, s:statement)
    while l:idx != -1 
      let l:line = add(l:line, strpart(l:str, 0, l:idx))
      let l:str = substitute(strpart(l:str, l:idx), s:leading_space, "", "")
      let l:idx = matchend(l:str, s:statement)
    endwhile

    " check if last column is one of the special statements
    let l:lcspecial = l:lcspecial && (l:line[len(l:line)-1] =~ g:CimpAlignSpecialPattern)
    let l:lines = add(l:lines, l:line)
    let l:n += 1
  endwhile

  if len(l:cols) == 0 
    return
  endif

  " PASS 2 - identify overflow lines
  let l:n = a:firstline
  while l:n <= a:lastline
    let l:overflow = s:CimpCheckOverflow(l:lines[l:n - a:firstline], l:cols, cindent(l:n))
    let l:oflines = add(l:oflines, l:overflow) 
    let l:n += 1
  endwhile

"  echo "Moving onto PASS 3"
"  echo l:cols
"  echo l:oflines

  " PASS 3 - identify columns, excluding possible overflow lines
  let l:cols = s:CimpGetColumns(l:lines, l:oflines, l:cols)

  if len(l:cols) <= 1
    return
  endif

"  echo "Moving onto PASS 4"
"  echo l:cols
"  echo l:oflines

  " PASS 4 - strip columns until we have removed textwidth overflow
  if s:tabform == 1 " only in smart tabulate format
    let l:n = a:firstline
    let l:col = len(l:cols) - 1

    let l:overflow = 1
    while l:overflow && (l:col > l:lcspecial) " l:lcspecial is always 0 or 1
      let l:overflow = 0
      while l:n <= a:lastline
        if !l:oflines[l:n - a:firstline]
          if s:CimpCheckOverflow(l:lines[l:n - a:firstline], l:cols, cindent(l:n))
            let l:overflow = 1
            break
          endif
        endif
        let l:n += 1
      endwhile

      if l:overflow
        let l:cols[l:col] = -1
        " recalculate columns
        let l:cols = s:CimpGetColumns(l:lines, l:oflines, l:cols)
        let l:col -= 1
      endif
    endwhile

"    echo "Inside PASS 4, moving to check overflow"
"    echo l:cols
"    echo l:oflines

    " if we're using last column special, some lines might overflow anyway,  
    " so let's compactify them
    let l:n = a:firstline
    while l:n <= a:lastline
      let l:overflow = s:CimpCheckOverflow(l:lines[l:n - a:firstline], l:cols, cindent(l:n))
      let l:oflines[l:n - a:firstline] = l:overflow
      let l:n += 1
    endwhile
  endif

"  echo "Moving onto PASS 5"
"  echo l:cols
"  echo l:oflines

  " PASS 5 - do the alignment
  let l:n = a:firstline
  while l:n <= a:lastline
    let l:line = l:lines[l:n - a:firstline]
    let l:indent = cindent(l:n)

    if s:tabform == 2
      let l:text = s:CimpGetCompact(l:line, l:indent, l:cols[0])
    elseif (s:tabform == 1) && l:oflines[l:n - a:firstline] 
      let l:text = s:CimpGetCompact(l:line, l:indent, l:cols[0])
    else
      let l:text = s:CimpGetTabulated(l:line, l:cols, l:indent)
    endif

    call setline(l:n, l:text)
"    echo l:text
    let l:n += 1
  endwhile
endfunc

map <Leader>cc :call CimpTabulateCase()<CR>
map <Leader>c0 :call CimpTabulateCase(0)<CR>
map <Leader>c1 :call CimpTabulateCase(1)<CR>
map <Leader>c2 :call CimpTabulateCase(2)<CR>
