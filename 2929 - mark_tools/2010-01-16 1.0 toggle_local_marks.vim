" Toggle and navigate local bookmarks
" Last Change:	2010 Jan 15
" Maintainer:  Sergey Khorev <sergey.khorev@gmail.com>
" vim: set ft=vim ts=8 sts=2 sw=2:
"
" MAPPINGS DEFINED:
"<Plug>ToggleMarkAZ - mark/unmark current position
"		      if there are multiple marks on the line, remove the first
"<Plug>ToggleMarkAZ - mark/unmark current position
"		      if there are multiple marks on the line, remove the last
" <Plug>ForceMarkAZ - add an unused mark starting from a, even if the position is marked
" <Plug>ForceMarkZA - add an unused mark starting from z, even if the position is marked
" <Plug>NextMarkPos - go to next mark
" <Plug>PrevMarkPos - go to prev mark
" <Plug>NextMarkLexi - go to previous mark in lexicographical order
" <Plug>PrevMarkLexi - go to next mark in lexicographical order
"
" recommended mapping:
" nmap <Leader>a <Plug>ToggleMarkAZ
" nmap <Leader>z <Plug>ToggleMarkZA
" nmap <Leader>A <Plug>ForceMarkAZ
" nmap <Leader>Z <Plug>ForceMarkZA
" nmap <Leader>m <Plug>NextMarkPos
" nmap <Leader>M <Plug>PrevMarkPos
" nmap <Leader>l <Plug>NextMarkLexi
" nmap <Leader>L <Plug>PrevMarkLexi
" so
" \a and \z toggle a mark at current line
" \A and \Z force another mark
" \m and \M go to next/prev mark
" \l and \L go to next/prev mark alphabetically
"
" Also I recommend installation of a plugin to visualise marks
" e.g. quickfixsigns (http://www.vim.org/scripts/script.php?script_id=2584)

let s:save_cpo = &cpo
set cpo&vim

if exists("loaded_toggle_local_marks")
  finish
endif
let loaded_toggle_local_marks = 1

unlockvar s:marks_names
unlockvar s:marks_count
unlockvar s:marks_nlist
let s:marks_names = 'abcdefghijklmnopqrstuvwxyz'
let s:marks_count = strlen(s:marks_names)
let s:marks_nlist = split(s:marks_names, '\zs')
lockvar s:marks_names
lockvar s:marks_count
lockvar s:marks_nlist

function! s:LocalMarkList()
  return map(copy(s:marks_nlist), '[v:val, line("''" . v:val)]')
endfunction

function! s:MarksAt(pos)
  return join(map(filter(s:LocalMarkList(), 'v:val[1]==' . a:pos), 'v:val[0]'), '')
endfunction

function! s:UsedMarks()
  return join(map(s:LocalMarkList(), '(v:val[1]>0 ? v:val[0] : " ")'),'')
endfunction

function! s:NextMark(pos)
  let l:mark = ''
  let l:pos = 0
  let l:dist = 0
  for m in s:LocalMarkList()
    if m[1] > a:pos && (l:pos == 0 || m[1] - a:pos < l:dist)
      let l:mark = m[0]
      let l:pos = m[1]
      let l:dist = m[1] - a:pos
    endif
  endfor
  return l:mark
endfunction

function! s:PrevMark(pos)
  let l:mark = ''
  let l:pos = 0
  let l:dist = 0
  for m in s:LocalMarkList()
    if m[1] > 0 && m[1] < a:pos && (l:pos == 0 || a:pos - m[1] < l:dist)
      let l:mark = m[0]
      let l:pos = m[1]
      let l:dist = a:pos - m[1]
    endif
  endfor
  return l:mark
endfunction

function! s:NextMarkAlpha(mark)
  let l:index = char2nr(a:mark) - char2nr(s:marks_names[0])
  for m in s:LocalMarkList()[l:index + 1:]
    if m[1] > 0
      return m[0]
    endif
  endfor
  return ''
endfunction

function! s:PrevMarkAlpha(mark)
  let l:index = char2nr(s:marks_names[s:marks_count-1]) - char2nr(a:mark)
  for m in reverse(s:LocalMarkList())[l:index + 1:]
    if m[1] > 0
      return m[0]
    endif
  endfor
  return ''
endfunction

function! s:ToggleMarks(a2z, forceAdd)
  let l:marks_here = s:MarksAt(line('.'))

  if !a:forceAdd && !empty(l:marks_here)
    " delete one mark
    if a:a2z
      exec 'delma ' . l:marks_here[0]
    else
      exec 'delma ' . l:marks_here[strlen(l:marks_here)-1]
    endif
  else
    " no marks, add first available mark
    let l:used = s:UsedMarks()
    let l:len = strlen(l:used)
    if a:a2z
      for i in range(0, l:len-1)
	if l:used[i] == ' '
	  exec "normal m" . s:marks_names[i]
	  return
	endif
      endfor
    else
      for i in range(l:len-1, 0, -1)
	if l:used[i] == ' '
	  exec "normal m" . s:marks_names[i]
	  return
	endif
      endfor
    endif
  endif
endfunction

function! s:GetWrapSearch()
  let l:wrap = 1
  if exists('w:toggle_marks_wrap_search')
    let l:wrap = w:toggle_marks_wrap_search
  elseif exists('b:toggle_marks_wrap_search')
    let l:wrap = b:toggle_marks_wrap_search
  elseif exists('g:toggle_marks_wrap_search')
    let l:wrap = g:toggle_marks_wrap_search
  end

  if l:wrap < 0
    return &wrapscan
  elseif l:wrap == 0
    return 0
  else
    return 1
  endif
endfunction

function! s:NextByPos()
  let l:mark = s:NextMark(line('.'))
  if empty(l:mark) && s:GetWrapSearch()
    let l:mark = s:NextMark(0)
  endif
  if !empty(l:mark)
    exec ':''' . l:mark
  endif
endfunction

function! s:PrevByPos()
  let l:mark = s:PrevMark(line('.'))
  if empty(l:mark) && s:GetWrapSearch()
    let l:mark = s:PrevMark(line('$')+1)
  endif
  if !empty(l:mark)
    exec ':''' . l:mark
  endif
endfunction

function! s:NextByAlpha()
  let l:marks_here = s:MarksAt(line('.'))
  if !empty(l:marks_here)
    let l:mark = s:NextMarkAlpha(l:marks_here[strlen(l:marks_here)-1])
    if empty(l:mark) && s:GetWrapSearch()
      let l:mark = s:NextMarkAlpha(nr2char(char2nr(s:marks_names[0])-1))
    endif
    if !empty(l:mark)
      exec ':''' . l:mark
    endif
  endif
endfunction

function! s:PrevByAlpha()
  let l:marks_here = s:MarksAt(line('.'))
  if !empty(l:marks_here)
    let l:mark = s:PrevMarkAlpha(l:marks_here[0])
    if empty(l:mark) && s:GetWrapSearch()
      let l:mark = s:PrevMarkAlpha(nr2char(char2nr(s:marks_names[s:marks_count-1])+1))
    endif
    if !empty(l:mark)
      exec ':''' . l:mark
    endif
  endif
endfunction


" suggested mapping: <Leader>a and <Leader>z
nnoremap <silent> <Plug>ToggleMarkAZ :call <SID>ToggleMarks(1, 0)<CR>
nnoremap <silent> <Plug>ToggleMarkZA :call <SID>ToggleMarks(0, 0)<CR>
" suggested mapping: <Leader>A and <Leader>Z
nnoremap <silent> <Plug>ForceMarkAZ :call <SID>ToggleMarks(1, 1)<CR>
nnoremap <silent> <Plug>ForceMarkZA :call <SID>ToggleMarks(0, 1)<CR>
" suggested mapping: <Leader>m and <Leader>M
nnoremap <silent> <Plug>NextMarkPos :call <SID>NextByPos()<CR>
nnoremap <silent> <Plug>PrevMarkPos :call <SID>PrevByPos()<CR>
" suggested mapping: <Leader>l and <Leader>L (lexicographic)
nnoremap <silent> <Plug>NextMarkLexi :call <SID>NextByAlpha()<CR>
nnoremap <silent> <Plug>PrevMarkLexi :call <SID>PrevByAlpha()<CR>

let &cpo = s:save_cpo
