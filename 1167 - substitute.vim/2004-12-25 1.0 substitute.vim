""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" substitute.vim -- mappings for the s/// command
" 
" Author: Anders Th�gersen
" Last Change: 25-Dec-2004
" Created:     20-Dec-2004
" Version:     1.0
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
"
" Download From:
"     http://www.vim.org
" 
" Description:
"
" Visual and normal mode mappings for easier access to substitutions with the
" s/// command.
"
" Key mappings:
"
"   ;;  perform substitution using the word under the cursor, or the selected
"       text across the whole file.
"
"   ;'  perform replacement from this point till the end of the file.
"
" Pressing <C-V> will insert the text being replaced into the command line window.
"
" When the search and replace has completed the cursor can be returned to where
" the search started by jumping to mark ' (pressing '' or `').
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if exists('loaded_substitute')
  finish
endif

let loaded_substitute = 1

" Check the mappings
fun! CheckMapping(map, mode)
	if hasmapto(a:map, a:mode)
		echomsg "substitute.vim : keymap " .a:map . " has already been defined for mode " . a:mode . ". quitting..."
		finish
	endif
endfun

" check if mappings exist
call CheckMapping(';;', 'n')
call CheckMapping(";'", 'n')
call CheckMapping(';;', 'v')
call CheckMapping(";'", 'v')
call CheckMapping("<C-V>", 'c')

" define the mappings
nnoremap ;; m'yiw:let @z=':%'.AltSubst(@")<Cr>@z
nnoremap ;' m'yiw:let @z=':.,$'.AltSubst(@")<Cr>@z
vnoremap ;; <ESC>:0,'<ma '<CR>gvy:let @z=VisAltSubst('1' . @")<Cr>@z
vnoremap ;' <ESC>:0,'<ma '<CR>gvy:let @z=VisAltSubst('2' . @")<Cr>@z

if has("gui_running")
	cmap <C-V> <C-R>+
else
	cmap <C-V> <C-R>"
endif
	
fun! GetSubstDelimiter(txt)
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
	elseif stridx(a:txt, ',') == -1
		return ','
	else 
		return '^'
	endif
endfun
		
fun! VisAltSubst(txt)
	if line("'<")!=line("'>") || (line("'<")==line("'>") && col("'<")==1 && col("'>")==col("$"))
		let d = GetSubstDelimiter(a:txt)
		return ":'<,'>s" .d .d .d . 'gc�kl�kl�kl�kl'
	else
		let str = strpart(a:txt, 1)
		if stridx(a:txt, '1') == 0
			return ':%' . AltSubst(str)
		else
			return ':.,$' . AltSubst(str)	
		endif
	endif
endfun

fun! AltSubst(txt)
	let d = GetSubstDelimiter(a:txt)
	let mv = '�kl�kl�kl'
	if strlen(a:txt)==0
		let mv = mv . '�kl'
	endif
	return 's' .d .escape(a:txt,'\\.*$^~[]') .d .d .'gc' . mv 
endfun


