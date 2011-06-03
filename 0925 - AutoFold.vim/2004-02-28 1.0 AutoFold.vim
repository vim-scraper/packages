" ------------------------------------------------------------------------------
" Filename:      ~/.vim/plugin/AutoFold.vim                                  {{{
" VimScript:
" Last Modified: 28 Feb 2004 10:30:55 PM by Dave Vehrs
" Maintainer:    Dave Vehrs (davev at ziplip.com)
" Description:   A script to automate folding based on markers and syntax with 
"                language specific support for Perl, Python, Shell, and 
"                Vim scripts. 
" Copyright:     (C) 2003 Dave Vehrs
"                This script is free software; you can redistribute it and/or 
"                modify it under the terms of the GNU General Public License as 
"                published by the Free Software Foundation; either version 2 of 
"                the License, or (at your option) any later version.
" Install:       Save this file in your .vim/plugins/ directory or load it
"                manually with :source VimNotes.vim.
"                                                                            }}}
" ------------------------------------------------------------------------------
" Exit if already loaded.                                                    {{{
if (exists("loaded_autofold") || &cp) | finish | endif
let g:loaded_autofold=1
"                                                                            }}}
" ------------------------------------------------------------------------------
" Configuration                                                              {{{

filetype on

set foldexpr=SF_SetFolds()
set foldmethod=expr
set foldtext=MyFoldText()

"                                                                            }}}
" ------------------------------------------------------------------------------
" Custom Fold Text                                                           {{{

" MyFoldText: Custom fold text.
function! MyFoldText() 
  let l:return_line = ""
  let l:linecount = v:foldend - v:foldstart + 1
  let l:line = getline(v:foldstart)
  " Record indent
  let l:line_start = SetFoldTextIndent(l:line)
  " Clean up line
  let l:line = SetFoldTextClean(l:line)
  if exists("*SFT_{&filetype}_clean")
    let l:line = SFT_{&filetype}_clean(l:line)
  endif
  " Set width of text.
  let l:line = SetFoldTextWidth(l:line,strlen(l:line_start))
  " Set text tail
  let l:line_end = SetFoldTextTail(l:linecount) 
  " Set return line
  let l:return_line = l:line_start . l:line . l:line_end 
  return l:return_line
endfunction  

" Cleanup for all filetypes
function! SetFoldTextClean(line)
  let l:line = a:line
  let l:line = substitute(l:line, '^\s*', '', 'g')
  let l:line = substitute(l:line, '/\*\|\*/\|{{{\d\=', '', 'g')
  let l:line = substitute(l:line, '\s*$', '', 'g')
  return l:line  
endfunction

function! SetFoldTextIndent(line)
  let l:inline= a:line
  let l:retline = ""
  let l:indent = 0
  while  ( strpart(l:inline,0,1) == " "  )
    let l:inline = strpart(l:inline,1)
    let l:indent = l:indent + 1
  endwhile
  while (strlen(l:retline) < l:indent)
    let l:retline = l:retline . " "
  endwhile
  let l:retline = l:retline . "+ "
  return l:retline
endfunction

" This is the tail of the fold text, with the line count.
function! SetFoldTextTail(count)
  let l:line = "[lines:"
  if (a:count <= 9)
    let l:line = l:line . "  " . a:count . "] "
  elseif (a:count <= 99)
    let l:line = l:line . " ". a:count . "] "
  else
    let l:line = l:line . a:count . "] "
  endif
  let l:line = l:line . "             "
  return l:line  
endfunction

" This is the part of the line that we will see, adjusted for width, etc.
function! SetFoldTextWidth(line,indent)
  let l:line = a:line
  let l:width = &textwidth -  14  
  let l:width = l:width - a:indent
  if (strlen(l:line) > l:width )
    let l:line = strpart(l:line,0,l:width)."..."
  else
    while (strlen(l:line) <= l:width + 2)
      let l:line = l:line . " "
    endwhile
  endif
  return l:line  
endfunction

" Cleanup for Perl scripts.
function! SFT_perl_clean(line) 
  let l:line = a:line
  let l:line = substitute(l:line, '^\#\s*', '', 'g')
  let l:line = substitute(l:line, '^sub\s', 'Subroutine: ', 'g')
  let l:line = substitute(l:line, '\s{\s*$', '', 'g')
  let l:line = substitute(l:line, '\s\+()\s\+{\s*$\|\s\+{\s*$', '', 'g')
  return l:line
endfunction

" Cleanup for Python files.
function! SFT_python_clean(line)
  let l:line = a:line
  let l:line = substitute(l:line, '^\"\s*', '', 'g')
  let l:line = substitute(l:line, '\s*\&\S*\*\s*$', '', 'g')
  let l:line = substitute(l:line, '^def\s', 'Function: ', 'g')
  let l:line = substitute(l:line, '^class\s', 'Class: ', 'g')
  return l:line
endfunction

" Cleanup for Shell scripts.
function! SFT_sh_clean(line) 
  let l:line = a:line
  let l:line = substitute(l:line, '^\#\s*', '', 'g')
  let l:line = substitute(l:line, '^function\s', 'Function: ', 'g')
  let l:line = substitute(l:line, '\s\+()\s\+{\s*$\|\s\+{\s*$', '', 'g')
  return l:line
endfunction

" Cleanup for Vim scripts.
function! SFT_vim_clean(line) 
  let l:line = a:line
  let l:line = substitute(l:line, '^\"\s*', '', 'g')
  let l:line = substitute(l:line, '\s*\&\S*\*\s*$', '', 'g')
  let l:line = substitute(l:line, '^\s*augroup', 'Autocommand Group:', 'g')
  let l:line = substitute(l:line, 'function\!*\s', 'Function: ', 'g')
  return l:line
endfunction

"                                                                             }}}
" ------------------------------------------------------------------------------
" Set Folds                                                                  {{{

function! SF_SetFolds()
  " let l:testline = getline(v:lnum)
  " echo "Testline = |" . l:testline . "|"
  " let l:test = SF_common_folds(l:testline)
  let l:test = SF_common_folds(v:lnum)
  " echo "Test1: " . l:test
  if ( l:test == "NF" )
    if exists("*SF_{&filetype}_folds") 
      let l:test = SF_{&filetype}_folds(v:lnum)
      " echo "Test2: " . l:test
    endif
  endif
  " echo "FINAL: " . l:test
  if ( l:test != "NF" )
    return l:test
  else
    return "="
  endif
endfunction   

" Folding rules for all files.
function! SF_common_folds(lnum)
  let l:line = getline(a:lnum)
  if l:line =~ '^\s*$'                                     | return "="     
  " For markers with Foldlevels
  elseif l:line =~ '\s\+{{{\d\+\s*$'
    let l:flvl = substitute(l:line, '^.*{{{', '', 'g')     | return ">" . l:flvl
  elseif l:line =~ '\s\+}}}\d\+\s*$'
    let  l:flvl = substitute(l:line, '^.*}}}', '', 'g')    | return "<" . l:flvl
  " For markers without foldlevels
  elseif l:line =~ '\s\+{{{\s*$'                           | return "a1"
  elseif l:line =~ '\s\+}}}\s*$'                           | return "s1"
  else                                                     | return "NF" | endif
endfunction 

" Perl folding rules.
function! SF_perl_folds(lnum)
  let l:line = getline(a:lnum)
  if     l:line =~ '^\s*sub\s\+\S\+\s\+{\s*$'              | return "a1"
  elseif l:line =~ '^\s*}\s*$'                             
    let l:eindent = indent(a:lnum)
    let l:pnum = a:lnum - 1
    while ( l:pnum > 0 )
      let l:pline = getline(l:pnum)
      if l:pline =~ '^\s*sub\s\+\S\+\s\+{\s*$'
        let l:pindent = indent(l:pnum)
        echo "ptestline: " . l:pline
        echo "eindent: " . l:eindent . " - pindent: " . l:pindent
        if l:eindent == l:pindent                          | return "s1" | endif
      endif
      let l:pnum = l:pnum - 1  
    endwhile
    let l:nline = getline(nextnonblank(a:lnum+1))
    if l:nline =~ '^\s*sub\s\+\S\+\s\+{\s*$'               | return "s1" | endif
  else                                                     | return "NF" | endif
endfunction 

" Python folding rules.
" NOTE: Inspired by Jorrit Wiersma and Max Ischenko's Python 
" folding script (VimScript#515)
function! SF_python_folds(lnum)
  let l:line = getline(a:lnum)
  let l:indent = indent(a:lnum)
  let l:nnum = nextnonblank(a:lnum + 1) 
  let l:nline = getline(l:nnum)
  let l:nindent = indent(l:nnum)
  let l:pnum = prevnonblank(a:lnum - 1)
  if     l:line =~ '^\s*\(class\|def\)\s\+\S*\s\+(.*):\s*$'| return "a1"
  elseif l:line =~ '^\s*\(\"\"\"\|'''\)'                   | return "="
  elseif l:line =~ '\\$'                                   | return "="  | endif 
  if     l:pnum == 0 || l:nnum == 0                        | return "=" 
  elseif foldlevel(l:pnum) == 0                            | return "=" 
  elseif l:nline =~ '^\s*\(except\|else\|elif\)'           | return "="
  elseif l:nnum == a:lnum + 1                              | return "="
  elseif l:nindent < l:indent                              | return "s1"
  else                                                     | return "NF" | endif
endfunction 

" Shell folding rules.
function! SF_sh_folds(lnum)
  let l:line = getline(a:lnum)
  if     l:line =~ '^\s*function\s\+\S\+\s\+{\s*$'         || 
       \ l:line =~ '^\s*function\s\+\S\+\s\+(.*)\s\+{\s*$' | return "a1"
  elseif l:line =~ '^\s*}\s*$'                             | return "s1"
  else                                                     | return "NF" | endif
endfunction 

" Vim script folding rules.
function! SF_vim_folds(lnum) 
  let l:line = getline(a:lnum)
  if     l:line =~ '^\s*func'                              | return "a1"
  elseif l:line =~ '^\s*endfunc'                           | return "s1"
  elseif l:line =~ '^\s*augroup end$'                      | return "s1"
  elseif l:line =~ '^\s*augroup'                           | return "a1"
  else                                                     | return "NF" | endif
endfunction  

" Insert fold markers
vmap zf  mz:<esc>'<O//{{{<esc>'>o// }}}<esc>`z?{{{<cr>A<space>

"                                                                            }}}
" ------------------------------------------------------------------------------
" Version History:                                                           {{{
" 1.0      02-28-2004  Initial Release.
"                                                                            }}}
" ------------------------------------------------------------------------------
" vim:tw=80:ts=2:sw=2:
