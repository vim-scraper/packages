" Vim global plugin for selecting C/C++ coding style (indent)
" Last Change:	2009 May 17
" Maintainer:	Konstantin Lepa <konstantin.lepa@gmail.com>
" Version:      1.0.1
"
" Changes {{{
" 1.0.2 2009-05-17
"   Renamed the folder cs_indent to coding_styles.
"   Removed styles.txt.
"   Added g:coding_style_ini (default ~/.coding_style.ini).
"   Added g:coding_style_dir (default ~/.vim/coding_styles).
"
" 1.0.1 2009-05-16
"   Added CodingStyle() for checking of current coding style.
"
"}}}

if exists("g:loaded_coding_style")
  finish
endif
let g:loaded_coding_style = 1

let s:save_cpo = &cpo
set cpo&vim

let s:style_list = {}
function s:ReadStyleList(filename)
  for l:line in readfile(a:filename)
    if l:line =~ '^\s*\[\(\S\+\)\]\s*$'
      let l:parsed = matchlist(l:line, '^\s*\[\(\S\+\)\]\s*$')
      let l:ft = l:parsed[1]
    endif
    if l:line =~ '^\s*\([^=]\)\+\s\+=\s\+\([^=]\)\+\s*$'
      let l:parsed = matchlist(l:line, '^\([^=]\+\)\s\+=\s\+\([^=]\+\)$')
      if !exists('s:style_list[l:ft]')
        let s:style_list[l:ft] = []
      endif
      call extend(s:style_list[l:ft],
\                 [{'name': l:parsed[1], 'path': l:parsed[2]}])
    endif
  endfor
endfunction

function s:ReverseCompare(i1, i2)
  return a:i1['path'] == a:i2['path'] ? 0 :
\        a:i1['path'] > a:i2['path'] ? -1 : 1
endfunction

function s:FindStyle(filetp, curpath)
  if !has_key(s:style_list, a:filetp) | return 'none' | endif
  for l:line in sort(s:style_list[a:filetp], 's:ReverseCompare')
    if l:line['path'] =~ '/\s*$' || l:line['path'] =~ '\\\s*$'
      let l:pathlen = strlen(l:line['path'])
      let l:line['path'] = strpart(l:line['path'], 0, l:pathlen - 1)
    endif
    if match(a:curpath, l:line['path']) == 0
      return l:line['name']
    endif
  endfor
  return 'none'
endfunction

function SelectStyle()
  if !exists('g:coding_style_ini')
    let g:coding_style_ini = expand('~/.coding_style.ini')
  endif
  if !exists('g:coding_style_dir')
    let g:coding_style_dir = expand('~/.vim/coding_styles')
  endif
  if !filereadable(g:coding_style_ini)
    return 'none'
  endif
  call s:ReadStyleList(g:coding_style_ini)
  let b:coding_style = s:FindStyle(&filetype, expand('%:p:h'))
  if b:coding_style == 'none'
    return
  endif
  let l:path = g:coding_style_dir . '/' . b:coding_style . '.vim'
  source `=l:path`
endfunction

function CodingStyle()
  if !exists("b:coding_style") | return 'none' | endif
  return b:coding_style
endfunction

au BufNewFile,BufRead * call SelectStyle()

let &cpo = s:save_cpo

