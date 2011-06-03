"=====================================================================
" cream-capitalization.vim
"
" Cream -- An easy-to-use configuration of the famous Vim text  editor
" [ http://cream.sourceforge.net ] Copyright (C) 2002-2003  Steve Hall
"
" License:
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of  the  License,  or
" (at your option) any later version.
" [ http://www.gnu.org/licenses/gpl.html ]
"
" This program is distributed in the hope that it will be useful,  but
" WITHOUT  ANY  WARRANTY;  without  even  the  implied   warranty   of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the  GNU
" General Public License for more details.
"
" You should have received a copy of the GNU  General  Public  License
" along with  this  program;  if  not,  write  to  the  Free  Software
" Foundation,  Inc.,  59  Temple  Place  -  Suite  330,   Boston,   MA
" 02111-1307, USA.
"
" Updated: 2003-12-06, 09:36am
" Source:  http://vim.sourceforge.net/scripts/script.php?script_id=242
" Author:  Steve Hall  [ digitect@mindspring.com ]
"
" Instructions:
" o Simply copy this file and paste it into your vimrc. Or you can
"   drop the entire file into your plugins directory.
" o As long as you don't already have keyboard mappings to the F5 key,
"   these keyboard shortcuts will now be available:
"   F5        Capitalize selection, title case
"   Shift+F5  Uppercase selection
"   Alt+F5    Lowercase selection
"   Ctrl+F5   Reverse case of selection
"
" Notes:
" o Restoration of selection is usually accurate, but not in some rare
"   instances where it is shifted one char.
"
" ChangeLog:
"
" 2003-12-06 -- 2.3
" o Fixed errant function name calls. ;)
"
" 2003-12-06 -- 2.2
" o Added mappings back in script for use outside of Cream.
" o Cleaned out obsolete algorithm and embedded new single line from
"   formerly called function.
" o Changed function name for title case to be consistent with
"   remainder.
"
" 2003-05-07 -- 2.1
" o Changed algorithm in Title Case to simple substitution. (Benji
"   Fisher)
"
" 2002-11-13 -- 2.0
" o Functionalized all mappings.
" o Destroyed standalone state with references to Cream libraries.
" o Made functions mode sensitive. From insertmode, each function
"   affects the current word.
"
" 2002-04-15 -- 1.3
" o Fixed positioning "anomalies" for title case. (Hopefully for good!)
"
" 2002-03-26 -- 1.2
" o Work around broken Vim paste back behavior at column 1
"
" 2002-03-25 -- 1.1
" o Modified title case function to lower case all text before
"   capitalizing initial characters.
"
" 2002-03-10
" o Initial Release
"


" mappings (if not used with Cream)
if !exists("$CREAM")

	" Title Case
	imap <silent> <F5>   <C-o>:call Cream_case_title("i")<CR>
	vmap <silent> <F5>   :<C-u>call Cream_case_title("v")<CR>
	" UPPERCASE
	imap <silent> <S-F5> <C-o>:call Cream_case_upper("i")<CR>
	vmap <silent> <S-F5> :<C-u>call Cream_case_upper("v")<CR>
	" lowercase
	imap <silent> <M-F5> <C-o>:call Cream_case_lower("i")<CR>
	vmap <silent> <M-F5> :<C-u>call Cream_case_lower("v")<CR>
	" rEVERSE CASE
	imap <silent> <C-F5> <C-o>:call Cream_case_reverse("i")<CR>
	vmap <silent> <C-F5> :<C-u>call Cream_case_reverse("v")<CR>

endif

function! Cream_case_title(mode)
" Title Case -- uppercase characters following whitespace

	if a:mode == "v"
		normal gv
        " Hack: fix Vim's gv proclivity to add a line when at line end
        if virtcol(".") == 1
			normal '>
			" line select
			normal gV
			" up one line
			normal k
			" back to char select
			normal gV
		endif
	else
		let mypos = Cream_pos()
		" select current word
		normal v
		normal aw
	endif
	" yank
	normal "xy

    " lower case entire string
    let @x = tolower(@x)
	" capitalize chars following these characters...
	let @x = substitute(@x, '\w\+', '\u&', 'g')

	" reselect
	normal gv
	" paste over selection (replacing it)
	normal "xP

	" return state
	if a:mode == "v"
		normal gv
	else
		execute mypos
	endif

endfunction

function! Cream_case_lower(mode)
" accepts "v" or "i" as mode
	if a:mode == "v"
		normal gv
	else
		let mypos = Cream_pos()
		" select current word
		normal v
		normal aw
	endif
	" lower case
	normal u
	" return state
	if a:mode == "v"
		normal gv
	else
		execute mypos
	endif
endfunction

function! Cream_case_upper(mode)
" accepts "v" or "i" as mode
	if a:mode == "v"
		normal gv
	else
		let mypos = Cream_pos()
		" select current word
		normal v
		normal aw
	endif
	" UPPER CASE
	normal U
	" return state
	if a:mode == "v"
		normal gv
	else
		execute mypos
	endif
endfunction

function! Cream_case_reverse(mode)
" accepts "v" or "i" as mode
	if a:mode == "v"
		normal gv
	else
		let mypos = Cream_pos()
		" select current word
		normal v
		normal aw
	endif
	" rEVERSE cASE
	normal ~
	" return state
	if a:mode == "v"
		normal gv
	else
		execute mypos
	endif
endfunction

