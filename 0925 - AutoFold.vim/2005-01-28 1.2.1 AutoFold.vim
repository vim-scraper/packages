" ------------------------------------------------------------------------------
" Filename:      AutoFold.vim                                                {{{
" VimScript:     925
"
" Maintainer:    Dave Vehrs <davev (at) ezrs.com>
" Last Modified: 28 Jan 2005 06:31:07 PM by Dave Vehrs
"
" Copyright:     © 2004-2005 Dave Vehrs
"
"                This program is free software; you can redistribute it and/or
"                modify it under the terms of the GNU General Public License as
"                published by the Free Software Foundation; either version 2 of
"                the License, or (at your option) any later version.
"
"                This program is distributed in the hope that it will be useful,
"                but WITHOUT ANY WARRANTY; without even the implied warranty of
"                MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"                GNU General Public License for more details.
"
"                You should have received a copy of the GNU General Public
"                License along with this program; if not, write to the Free
"                Software Foundation, Inc., 59 Temple Place, Suite 330,
"                Boston, MA 02111-1307 USA _OR_ download at copy at
"                http://www.gnu.org/licenses/licenses.html#TOCGPL
"
" Description:   A script to automate folding based on markers and syntax with
"                language specific support for Perl, Python, Shell, and
"                Vim scripts.
"
" Install:       Save this file in your .vim/plugins/ directory or load it
"                manually with :source AutoFold.vim.
"
"                                                                            }}}
" ------------------------------------------------------------------------------
" Configuration                                                              {{{

filetype on
set foldcolumn=0
set foldexpr=SF_SetFolds()
set foldmethod=expr
set foldminlines=1
set foldopen=block,hor,insert,mark,percent,quickfix,search,tag,undo
set foldtext=SFT_SetFoldText()

" Set fold text style
" ("solid" sets the fold to be a solid line of -, anything else leaves it open
" for textwidth + 14, then default fold character(-))
if !exists("g:AF_foldstyle")
	let g:AF_foldstyle = 'open'
endif

" Set fold width
" ("full" sets to window width, anything else sets it to text width)
if !exists("g:AF_foldwidth")
	let g:AF_foldwidth = 'text'
endif

"                                                                            }}}
" ------------------------------------------------------------------------------
" Exit if already loaded.                                                    {{{

if (exists("loaded_autofold") || &cp) | finish | endif
let g:loaded_autofold=1

"                                                                            }}}
" ------------------------------------------------------------------------------
" Autocommands:                                                              {{{

" Check/Fix Fold Marker Format
autocmd BufWritePre,FileWritePre * ks|call UpdateFoldMarkers()|'s

"                                                                            }}}
" ------------------------------------------------------------------------------
" Fold Text:                                                                {{{1

function! SFT_SetFoldText()
  let l:lcnt = v:foldend - v:foldstart + 1
  let l:line = getline(v:foldstart)
  " Record indent.
  let l:bline = substitute(l:line, '^\(\s*\)\S.*', '\1', 'g') . "+ "
  " Clean up line.
  if l:line =~ '^\s*\/\*[*]*\s*$'
    let l:line = getline(v:foldstart + 1)
  endif
  if exists("*s:SFT_{&filetype}_clean")
    let l:line = s:SFT_{&filetype}_clean(l:line)
  endif
  let l:line = substitute(l:line, '^\s*\|/\*\|\*\|\*/\|\s*[{{{]\s*\d*\|\s*$', '', 'g')
  " Set line width.
	if g:AF_foldwidth == 'full'
    let l:width = &columns -  (13 + &fdc + strlen(l:bline))
	else
    let l:width = &textwidth -  (13 + strlen(l:bline))
	endif
  if strlen(l:line) > l:width
    let l:line = strpart(l:line,0,l:width - 2) . "..."
  else
		if g:AF_foldstyle == 'solid'
			 while strlen(l:line) <= (l:width - 2) | let l:line = l:line . "-" | endwhile
       let l:line = l:line . "->"
		else
			while strlen(l:line) <= l:width | let l:line = l:line . " " | endwhile
		endif
  endif
  " Set tail (line count, etc.).
  if     l:lcnt <= 9   | let l:tline = "[lines:   " . l:lcnt . "]"
  elseif l:lcnt <= 99  | let l:tline = "[lines:  ". l:lcnt . "]"
  elseif l:lcnt <= 999 | let l:tline = "[lines: ". l:lcnt . "]"
  else                 | let l:tline = "[lines:". l:lcnt . "]"| endif
  " Set return line
	if ( g:AF_foldstyle == 'solid' && g:AF_foldwidth != 'full' )
		return l:bline . l:line . l:tline . "<"
	else
		return l:bline . l:line .  l:tline .  "              <"
	endif
endfunction

" Filetype specific cleanup functions.                                      {{{2

" Cleanup for C files.
function! s:SFT_c_clean(line)
  let l:line = substitute(a:line, '^\s*[/]*\*\+\s', 'Comment: ', 'g')
  "let l:line = substitute(l:line, '^\s*\/\*\s', 'Comment: ', 'g')
  if l:line =~ '^\s*\#if'
    let l:line = substitute(l:line, '^\s*\#', '', 'g')
    let l:line = substitute(l:line, '^ifdef\s\+', 'IFDEF: ', 'g')
    let l:line = substitute(l:line, '^if defined(', 'IF DEFINED: ', 'g')
    let l:line = substitute(l:line, '^if !defined(', 'IF NOT DEFINED: ', 'g')
    let l:line = substitute(l:line, '&& defined(', ', ', 'g')
    let l:line = substitute(l:line, ')\s*\(\/\*.*\*\/\s*\)*', '', 'g')
  endif
  return l:line
endfunction

" Cleanup for Perl scripts.
function! s:SFT_perl_clean(line)
  let l:line = substitute(a:line, '^\#\s*\|\s{\s*$\|\s\+(*)*\s*{\s*$', '', 'g')
  let l:line = substitute(l:line, '^sub\s', 'Subroutine: ', 'g')
  return l:line
endfunction

" Cleanup for Python files.
function! s:SFT_python_clean(line)
  let l:line = substitute(a:line, '^\"\s*\|\s*\&\S*\*\s*$', '', 'g')
  let l:line = substitute(l:line, '^def\s', 'Function: ', 'g')
  let l:line = substitute(l:line, '^class\s', 'Class: ', 'g')
  return l:line
endfunction

" Cleanup for Shell scripts.
function! s:SFT_sh_clean(line)
  let l:line = substitute(a:line, '^\#\s*\|\s\+(*)*\s*{\s*$', '', 'g')
  let l:line = substitute(l:line, '^function\s', 'Function: ', 'g')
  return l:line
endfunction

" Cleanup for Vim scripts.
function! s:SFT_vim_clean(line)
  let l:line = substitute(a:line, '^\"\s*\|\s*\&\S*\*\s*$\|()\s*$', '', 'g')
  let l:line = substitute(l:line, '^\s*augroup', 'Autocommand Group:', 'g')
  let l:line = substitute(l:line, 'function\!*\s\+\c\(s:\|<sid>\)*',
    \ 'Function: ', 'g')
  return l:line
endfunction

"                                                                           }}}2

"                                                                           }}}1
" ------------------------------------------------------------------------------
" Set Folds:                                                                {{{1

function! SF_SetFolds()
  let l:test = s:SF_common_folds(v:lnum)
  if l:test == "NF"
    if exists("*s:SF_{&filetype}_folds")
      let l:test = s:SF_{&filetype}_folds(v:lnum)
    endif
  endif
  if l:test != "NF"
    return l:test
  else
    return "="
  endif
endfunction

" Folding rules for all files (markers).
function! s:SF_common_folds(lnum)
  let l:line = getline(a:lnum)
	let l:comment_char = s:get_commentchar(&filetype)
  if l:line =~ '^\s*$'
    return "="
  endif
  " For markers with Foldlevels
  if l:line =~ '^\s*' . l:comment_char . '\s*{{{\d\+.*$'
    let l:flvl = substitute(l:line, '^.*{{{', '', 'g')
    return ">" . l:flvl
  endif
  if l:line =~ '\s\+{{{\d\+\s*$'
    let l:flvl = substitute(l:line, '^.*{{{', '', 'g')
    return ">" . l:flvl
  endif
  if l:line =~ '\s*}}}\d\+\s*$'
    let  l:flvl = substitute(l:line, '^.*}}}', '', 'g')
    return "<" . l:flvl
  endif
    " For markers without foldlevels
		if l:line =~ '^\s*' . l:comment_char . '.*{{{\s*.*$'
    return "a1"
	endif
  if l:line =~ '\s\+{{{\s*$'
    return "a1"
  endif
  if l:line =~ '\s*}}}\s*$'
    return "s1"
  endif
  return "NF"
endfunction

" Filetype specific folding functions (syntax)                              {{{2

" C Ideas
function! s:SF_c_folds(lnum)
  let l:line = getline(a:lnum)
  if l:line =~ '^{\s*$'
    return "a1"
  endif
  if l:line =~ '^\s*\/\*\+\s*[^*/]*$'
    return "a1"
  endif
  if l:line =~ '^\s*\#if\%[def]\s\+\(\S\+\s*\)*$'
    return "a1"
  endif
  if foldlevel(l:pnum) == 0
    return "0"
  endif
  if l:line =~ '^}\s*$'
    return "s1"
  endif
  if l:line =~ '^\s\+\(\*[^*/]\+\)*\*\/\s*$'
    return "s1"
  endif
  if l:line =~ '^\s*\#endif\s*\(\/\*.*\)*$'
    return "s1"
  endif
  if l:line =~ '^\s*\#.*'
    return "="
  endif
  let l:nline = getline(nextnonblank(a:lnum + 1))
  return "NF"
endfunction

" Perl folding rules (currently subroutines only).
function! s:SF_perl_folds(lnum)
  let l:line = getline(a:lnum)
  let l:pnum = a:lnum - 1
  if     l:line =~ '^\s*sub\s\+\S\+\s\+\(:\s\+\S\+\s\+\)*{\s*$'
    return "a1"
  endif
  if l:line =~ "^\s*\#.*"
    return "="
  endif
  if foldlevel(l:pnum) == 0
    return "0"
  endif
  if l:line =~ "^}\s*$"
    return "s1"
  endif
  let l:nline = getline(nextnonblank(a:lnum+1))
  if l:nline =~ '^\s*sub\s\+\S\+\s\+{\s*$'
    return "s1"
  endif
  return "NF"
endfunction

" Python folding rules.
function! s:SF_python_folds(lnum)
  " NOTE: Inspired by Jorrit Wiersma's foldexpr (VimScript#515)
  let l:line = getline(a:lnum)
  let l:indent = indent(a:lnum)
  if l:line =~ "^\s*\(\"\"\"\|'''\)"
    return "="
  endif
  if l:line =~ "\\$"
    return "="
  endif
  if l:line =~ '^\s*\(class\|def\)\s\+\S*\s\+(.*\(,\|):\)\s*$'
   return ">" . (l:indent / &shiftwidth + 1)
  endif
  let l:pnum = prevnonblank(a:lnum - 1)
  if  ( l:pnum == 0 || foldlevel(l:pnum) == 0 )
    return "0"
  endif
  let l:nnum = nextnonblank(a:lnum + 1)
  if  ( l:nnum == 0 || l:nnum  == a:lnum + 1 )
    return "="
  endif
  if getline(l:nnum) =~ "^\s*\(except\|else\|elif\)"
    return "="
  endif
  let l:nindent = indent(l:nnum)
  if l:nindent < l:indent
    return "<" . (l:nindent / &shiftwidth + 1)
  endif
  return "NF"
endfunction

" Shell folding rules.
function! s:SF_sh_folds(lnum)
  let l:line = getline(a:lnum)
  let l:pnum = a:lnum - 1
  if  l:line =~ '^\s*function\s\+\S\+\s\+[(.*)]*\s*{\s*$'
    return "a1"
  endif
  if foldlevel(l:pnum) == 0
    return "="
  endif
  if l:line =~ '^\s*\#.*'
    return "="
  endif
  if l:line =~ '^\s*}\s*$'
    return "s1"
  endif
  return "NF"
endfunction

" Vim script folding rules.
function! s:SF_vim_folds(lnum)
  let l:line = getline(a:lnum)
  let l:pnum = prevnonblank(a:lnum - 1)
  if l:line =~ '\c^\s*fu\%[nction]\%[!]\s\+[\(s:\|<sid>\)]*\S\+\s*(.*)\s*$'
    return "a1"
  endif
  if l:line =~ '\c^\s*au\%[group]\s\+\(end\)\@!\S\+\s*$'
    return "a1"
  endif
  if ( foldlevel(l:pnum) == 0 || l:pnum = 0 )
    return "0"
  endif
  if l:line =~ '^\s*\#.*'
    return "="
  endif
  if l:line =~ '^\s*endf\%[unction]\s*$'
    return "s1"
  endif
  if l:line =~ '\c^\s*au\%[group]\send\s*$'
    return "s1"
  endif
  return "NF"
endfunction

"                                                                           }}}2

"                                                                           }}}1
" ------------------------------------------------------------------------------
" Shared Functions:                                                          {{{

function! s:get_commentchar(ftype)
	if a:ftype == "c" || a:ftype == "cpp"
		return "\/\/"
	if a:ftype == "perl" || a:ftype == "python" || asie:ftype == "sh"
		return "\#"
	elseif a:ftype == "vim"
		return "\""
	endif
  " default return
	return "\#"
endfunction

" Seaches file for fold markers and checks/fixes their format.
function! UpdateFoldMarkers()
  if &modified && &foldexpr == "SF_SetFolds()"
    let l:line_current = 0
		let l:line_end = line("$")
		let l:line_width = &textwidth
		execute 'silent 1,'.l:line_end.' substitute/\({{{\d*\)\s*$/\1/'
 		execute 'silent 1,'.l:line_end.' substitute/\(}}}\d*\)\s*$/\1/'
 		execute 'silent 1,'.l:line_end.' substitute/\(--*\)\s*$/\1/'
    while l:line_current < l:line_end
	  	if ( match(getline(l:line_current),'\s{{{\d*\s*$')) > 1
				while strlen(getline(l:line_current)) < l:line_width
          execute 'silent '.l:line_current.','.l:line_current.' substitute/\(.*\)\(\s{{{\)/\1 \2/'
				endwhile
				while strlen(getline(l:line_current)) > l:line_width
          execute 'silent '.l:line_current.','.l:line_current.' substitute/\(.*\)\s\(\s{{{\)/\1\2/'
				endwhile
			endif
	  	if (match(getline(l:line_current),'\s}}}\d*\s*$')) > 1
				while strlen(getline(l:line_current)) < l:line_width
          execute 'silent '.l:line_current.','.l:line_current.' substitute/\(.*\)\(\s}}}\)/\1 \2/'
				endwhile
        while strlen(getline(l:line_current)) > l:line_width
          execute 'silent '.l:line_current.','.l:line_current.' substitute/\(.*\)\s\(}}}\)/\1\2/'
				endwhile
			endif
      let l:line_current = l:line_current + 1
    endwhile
  endif
endfunction 

"                                                                            }}}
" ------------------------------------------------------------------------------
" Mappings:                                                                  {{{

" Insert fold markers around marked area (visual mode)
"vmap zf  mz:<esc>'>O<esc>'>o_}_}_}_<esc>`z{_{_{_<cr>A<space>

"                                                                            }}}
" ------------------------------------------------------------------------------
" Version History:                                                           {{{
" 1.0    02-28-2004  Initial Release.
" 1.1    04-24-2004  Consolidated SFT_SetFoldText subfunctions to improve
"                    performance. Perl, Shell, and Vim folding pattern clean
"                    ups. Added initial C language support.
" 1.2    10-13-2004  Added Fold text style & width configuration items.  Thanks
"                    to Wolfgang H. for patches and ideas.
" 1.2.1  01-28-2005  Vim syntax fix. Configured FoldOpen options. Added function
"                    to check and fix the fold marker positioning with autocmd
"                    to run just before writing the file.  Improved Marker
"                    folding.
"                                                                            }}}
" ------------------------------------------------------------------------------
" vim:tw=80:ts=2:sw=2:
