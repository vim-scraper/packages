""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" substitute.vim -- mappings for the s/// command
" 
" Author: Anders Thøgersen
" Last Change: 13-Jan-2005
" Version:     1.03 
"
" Licence: This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License. See
" http://www.gnu.org/copyleft/gpl.txt 
"
" Download From:
" http://www.vim.org/scripts/script.php?script_id=1167
" 
" Description:
" Visual and normal mode mappings for easy access to substitutions with the
" s/// command.
" 
" This script will make it easy to replace the word under the cursor, or the
" visually selected text across the whole file. If more than one line is
" selected the substitution will only occur on the selected lines.
" 
" Key mappings:
"
"   ;;  Perform substitution across the whole file.
"
"   ;'  The same, but prompt for each substitution.
"
" Pressing <C-R><C-R> will insert the text being replaced into the command line
" window.
"
" When the search and replace has completed the cursor can be returned to where
" the search started by jumping to mark ' (pressing '' or `').
"
" BAD:
"
" - The z register is destroyed
"
" History:
" 
" 1.01 - Removed unuseful mapcheck.
" 
" 1.02 - Changed the meaning of the ;' mapping, and added <unique> to the map
"        definitions.  Added the Escape function so $ and ^ are only escaped when they
"        appear at the beginning, or at the end.  Also added cpoptions check and <SID>
"        stuff.
" 
" 1.03 - Simplified the mapping a bit. Marking register ' was not needed.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if exists('loaded_substitute')
  finish
endif
let loaded_substitute = 1

let s:savedCpo = &cpoptions
set cpoptions&vim

" define the mappings 
nnoremap <unique> ;; yiw:let @z=':%'.<SID>AltSubst(@", 'g')<Cr>@z
nnoremap <unique> ;' yiw:let @z=':%'.<SID>AltSubst(@", 'gc')<Cr>@z
vnoremap <unique> ;; <ESC>gvy:let @z=<SID>VisAltSubst(@", 'g')<Cr>@z
vnoremap <unique> ;' <ESC>gvy:let @z=<SID>VisAltSubst(@", 'gc')<Cr>@z
cnoremap <unique> <C-R><C-R> <C-R>"

fun! <SID>AltSubst(txt, flags)
	let d = s:GetSubstDelimiter(a:txt)
	let mv = '€kl€kl'
	if a:flags == 'gc'
		let mv = mv . '€kl' 
	endif
	if strlen(a:txt)==0
		let mv = mv . '€kl'
	endif
	let @" = s:Escape(a:txt) 
	return 's' .d . @" .d .d . a:flags . mv 
endfun

fun! <SID>VisAltSubst(txt, flags)
	let mv = '€kl€kl€kl'
	if a:flags == 'gc'
		let mv = mv . '€kl' 
	endif
	if line("'<")!=line("'>") || (line("'<")==line("'>") && col("'<")==1 && col("'>")==col("$"))
		let d = s:GetSubstDelimiter(a:txt)
		return ":'<,'>s" .d .d .d . a:flags . mv
	else
		return ':%' . <SID>AltSubst(a:txt, a:flags)	
	endif
endfun

" feel free to add more :-)
fun! s:GetSubstDelimiter(txt)
	if stridx(a:txt, '/') == -1
		return '/'
	elseif stridx(a:txt, ':') == -1
		return ':'
	elseif stridx(a:txt, '#') == -1
		return '#'
	elseif stridx(a:txt, ';') == -1
		return ';'
	elseif stridx(a:txt, '!') == -1
		return '!'
	else 
		return '*'
	endif
endfun

" escape as little as possible
fun! s:Escape(txt)
	let esc = '\\.~[]'
	if stridx(a:txt, '$') == (strlen(a:txt) -1)
		let esc = esc . '$'
	endif
	if stridx(a:txt, '^') == 0
		let esc = esc . '^'
	endif
	if stridx(a:txt, '*') > 0
		let esc = esc . '*'
	endif
	return escape(a:txt, esc)
endfun

let &cpoptions = s:savedCpo

