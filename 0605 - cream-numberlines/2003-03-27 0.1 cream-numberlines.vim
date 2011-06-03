"=====================================================================
" cream-numberlines.vim -- Number lines of a selection with optional
"                          (dialog prompt) start number
"
" Cream -- An easy-to-use configuration of the famous Vim  text editor
" [ http://cream.sourceforge.net ]  Copyright (C)2002-2003  Steve Hall
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
" Updated: 2003-03-27
" Version: 0.1
" Source:  http://vim.sourceforge.net/scripts/script.php?script_id=538
" Author:  Steve Hall  [ digitect@mindspring.com ]
"
" Installation:
" o Drop this file into your plugins directory and (re)start Vim.
"
" Usage:
" o Must be called from visual mode (with a selection).
" o Required argument indicates mode, only "v" (visual) allowed.
" o Example mapping:
"	  vmap <silent> <F12> :<C-u>call Cream_number_lines("v")
" o Example menu:
"     vmenu <silent> 40.151 I&nsert.Line\ Numbers\ (selection) :<C-u>call Cream_number_lines("v")<CR>
"

function! Cream_number_lines(mode)

	" only possible with selection (visual mode)
	if a:mode !=? "v"
		return
	endif

	" prompt for line number (default current)
	let startno = inputdialog("Enter the beginning number...", line("'<"))
	" verify '0' means 0 and not quit
	if startno == 0
		let n = confirm("Ok to start with line number '0' or Cancel function...\n", "&Ok\n&Cancel", 1, "Info")
		if n != 1
			return
		endif
	endif
	" force to digit (theoretical)
	let startno = startno + 0

	" calculate difference in line and starting number
	if startno < line("'<")
		let startdiff = startno - line("'<")
	else
		let startdiff = startno - line("'<")
	endif

	" file's current number of lines
	let maxlinelen = strlen(line('$')) + 0

	" how large line numbers can be
	let maxsize = maxlinelen
	" require minimum of 3 columns
	if maxlinelen < 3
		let maxlinelen = 3 + 0
	endif
	" create spacer the number of spaces of maxlines
	let myspacer = "   "
	while strlen(myspacer) <= maxlinelen
		let myspacer = myspacer . " "
	endwhile

	" add line numbers (regexp help by Stefan Roemer <roemer@in.tum.de>, 2003-03-27)
	execute "'\<,'>s/^/\\=submatch(1) . " .
		\ "strpart('" . myspacer . "', 1, " . maxlinelen . " - strlen((line('.') + " . startdiff . "))) . " .
		\ "(line('.') + " . startdiff . ") . ' '"

endfunction

