" Filename:    numlist.vim 
" Discirption: Add Re Add Remove id to a list items
" Last Change: 2011-08-29 
" Maintainer:  Tian Huixiong   <nedzqbear@gmail.com>
" Licence:     This script is released under the Vim License.
" Version:     1.1
" Install:     
"              Put this file in ~/.vim/plugin on *nux
"              Or put it in $vim/vimfiles/plugin on Windows
" Tutorial:
"              Only two commands:  NumberedList  and  NoNumberedList
"              Selected lines and call command.
"              Commands will guarantee the items with same indent.
"              If they are not so, they are changed. 
"              All rest items align with the first one.
"
"                        Numbered from 1
"               line1       :NumberedList      1. line1 
"               line2                          2. line2
"               line3                          3. line3
"               line4                          4. line4
"               line5                          5. line5
"
"                        Numbered from 8
"               line1      :NumberedList 8     8.  line1 
"               line2                          9.  line2
"               line3                          10. line3
"               line4                          11. line4
"               line5                          12. line5
"
"                        Re numbered
"               1. line1     :NumberedList     1. line1 
"               2. line2                       2. line2
"               5. line3                       3. line3
"               7. line4                       4. line4
"               8. line5                       5. line5
"
"                       Guarantee the same indent
"             1. line1     :NumberedList       1. line1 
"               2. line2                       2. line2
"               5. line3                       3. line3
"               7. line4                       4. line4
"               8. line5                       5. line5
"
"                         Remove number
"               1. line1     :NoNumberedList   line1 
"               2. line2                       line2
"               3. line3                       line3
"               4. line4                       line4
"               5. line5                       line5
"
"                       Guarantee the same indent
"             1. line1     :NoNumberedList     line1 
"               2. line2                       line2
"               3. line3                       line3
"               4. line4                       line4
"               5. line5                       line5
"
"                       Just the same, no error
"               line1     :NoNumberedList      line1 
"               line2                          line2
"               line3                          line3
"               line4                          line4
"               line5                          line5
"
"                     Guarantee the same indent
"             line1     :NoNumberedList        line1 
"               line2                          line2
"               line3                          line3
"               line4                          line4
"               line5                          line5

if exists("g:loaded_numlist")
    finish
endif
let g:loaded_numlist = 1

let s:save_cpo = &cpo
set cpo&vim

command! -range -nargs=? -complete=command NumberedList <line1>,<line2>call NumberLines(<f-args>)
command! -range -nargs=0 -complete=command NoNumberedList <line1>,<line2>call NoNumberLines()

function! NumberLines(...) range
    let start     = a:0 == 0 ? 1 : a:1
    let max_width = strlen(string(a:lastline-a:firstline+start))
    let indent    = matchstr(getline(a:firstline), '^\s*')

    for linenum in range(a:firstline, a:lastline)
        call NumberLine(linenum, linenum-a:firstline+start, indent, max_width)
    endfor

    normal gv 
endfunction

function! NumberLine(linenum, index, indent, max_width)
    let line    = getline(a:linenum)
    let width   = a:max_width - strlen(string(a:index)) + 1
    "let indent  = matchstr(line, '^\s*')
    let text    = substitute(line, '^\s*\(\d\+\.\{1}\)\?\s*', '', '')
    let newline = a:indent . string(a:index) . '.' . repeat(' ', width) . text
    call setline(a:linenum, newline)
endfunction

function! NoNumberLines() range
    let indent = matchstr(getline(a:firstline), '^\s*')
    for linenum in range(a:firstline, a:lastline)
        call NoNumberLine(linenum, indent)
    endfor
    normal gv 
endfunction

function! NoNumberLine(linenum, indent)
    let line    = getline(a:linenum)
    "let indent  = matchstr(line, '^\s*')
    let text    = substitute(line, '^\s*\(\d\+\.\{1}\)\?\s*', '', '')
    let newline = a:indent . text
    call setline(a:linenum, newline)
endfunction

let &cpo = s:save_cpo
 
"-----------------for test-----------------"
        "1. "1. line1
        "2. "2. line2
        "3. "3. line3
        "4. "4. line4
        "5. "5. line5
