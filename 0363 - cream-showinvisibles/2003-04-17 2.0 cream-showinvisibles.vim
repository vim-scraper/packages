"======================================================================
" cream-showinvisibles.vim
" 
" Cream -- An easy-to-use configuration of the famous Vim text editor
" [ http://cream.sourceforge.net ] Copyright (C) 2002-2003  Steve Hall
"
" License:
"
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
" Description:
"
" Toggle view of invisible characters such as tabs, trailing spaces
" and hard returns. The script includes intuitive presets for these
" characters, a global environmental variable (g:LIST) that is
" retained and initialized across sessions if you use a viminfo, and a
" familiar (to some of us) keyboard shortcut mapping to F4.
"
" This configuration includes characters as nice looking as your
" specific setup will allow, determined by hardware, operating system
" and Vim version. (Vim version 6.1.469 supports multi-byte
" characters, used with UTF-8 encoding.)
"
" This is one of the many custom utilities and functions for gVim from
" the Cream project (http://cream.sourceforge.net), a configuration of
" Vim for those of us familiar with Apple and Windows software. 
"
" Updated: 2003 April 17
" Version: 2.0
" Source:  http://vim.sourceforge.net/scripts/script.php?script_id=363
" Author:  Steve Hall  [ digitect@mindspring.com ]
" License: GPL (http://www.gnu.org/licenses/gpl.html)
"
" Instructions:
"
" o Simply copy this file and paste it into your vimrc. Or you can
"   drop the entire file into your plugins directory.
" o As long as you don't already have keyboard mappings to the F4 key,
"   it will toggle invisible characters.
"
" Notes:
"
" For more information see Vim's ":help 'list", ":help 'listchars",
" and ":help viminfo".
"
" ChangeLog:
"
" 2003-04-17 -- v.2.0
" o New multi-byte sets, contingent upon Vim version 6.1.469+. Note
"   that your particular OS and Font capabilities impact the display
"   of multi-byte characters, your usage may vary.
" o Abstracted multi-byte characters to decimal values so the current
"   editing session doesn't affect them.
"
" 2002-10-06 -- v.1.2
" o Modified state variable types from string to numbers
" o Extracted autocommand and mappings for the sake of the project. ;)
"
" 2002-08-03 -- v.1.1
" o New normal mode mapping and slightly saner visual and insert mode
"   mappings.
"
" 2002-08-03 -- v.1.0
" o Initial Release


" characters used to represent invisibles
set listchars=

"*********************************************************************
" WARNING:
" Do not try to enter multi-byte characters below, use decimal
" abstractions only! It's the only way to guarantee that all encodings
" can edit this file.
"
if &encoding == "latin1"
	" decimal 187 followed by a space (032)
	execute "set listchars+=tab:" . nr2char(187) . '\ '
	" decimal 182
	execute "set listchars+=eol:" . nr2char(182)
	" decimal 183
	execute "set listchars+=trail:" . nr2char(183)
	" decimal 133 (ellipses )
	execute "set listchars+=precedes:" . nr2char(133)
	execute "set listchars+=extends:" . nr2char(133)

" patch 6.1.469 fixes list with multi-byte chars! (2003-04-16)
elseif &encoding == "utf-8"
\&& v:version >= 601
\&& has("patch469")
	" decimal 187 followed by a space (032)
	execute "set listchars+=tab:" . nr2char(187) . '\ '
	" decimal 182
	execute "set listchars+=eol:" . nr2char(182)
	" decimal 9642 (digraph sB ▪ )
	" decimal 9675 (digraph m0 ○ )
	" decimal 9679 (digraph M0 ● )
	" decimal 183
	execute "set listchars+=trail:" . nr2char(183)
	" decimal 8222 (digraph :9 „ )
	" decimal 8249 (digraph <1 ‹ )
	execute "set listchars+=precedes:" . nr2char(8249)
	" decimal 8250 (digraph >1 › )
	execute "set listchars+=extends:" . nr2char(8250)

else
	set listchars+=tab:>\ 		" decimal 62 followed by a space (032)
	set listchars+=eol:$		" decimal 36
	set listchars+=trail:.		" decimal 46
	set listchars+=precedes:_	" decimal 95
	set listchars+=extends:_	" decimal 95
endif
"*********************************************************************

" initialize environment
function! List_init()
	if !exists("g:LIST")
		" initially off
		set nolist
		let g:LIST = 0
	else
		if g:LIST == 1
			set list
		else
			set nolist
		endif
	endif
endfunction

" toggle on/off
function! List_toggle(mode)
	if exists("g:LIST")
		if g:LIST == 0
			set list
			let g:LIST = 1
		elseif g:LIST == 1
			set nolist
			let g:LIST = 0
		endif
	else
		call confirm(
		\"Error: global uninitialized in List_toggle()", "&Ok", 1, "Error")
	endif
	if a:mode == "v"
		normal gv
	endif
endfunction

"---------------------------------------------------------------------
" Note: we put this here so our beautiful little character
" representations aren't affected by encoding changes. ;)
"
" vim:fileencoding=utf-8
