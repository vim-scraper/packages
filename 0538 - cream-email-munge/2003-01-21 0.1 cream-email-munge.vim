"=====================================================================
" cream-email-munge.vim -- Munge an email address
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
" Date:    2003-01-21
" Version: 0.1
" Source:  http://vim.sourceforge.net/scripts/script.php?script_id=538
" Author:  Steve Hall  [ digitect@mindspring.com ]
"
" Description:
" Posted email addresses are regularly harvested and processed by
" automated web search tools. These addresses often end up added or
" sold to bulk email lists so that the unsuspecting address owner
" receives large quantities of unsolicited email, also known as Spam. 
"
" To avoid having one's email address harvested in this way, it can be
" "munged", or made unrecognizable by the automated system while still
" appearing as an email address to any human reader. Examples include:
" 
"   unmunged:  username@domain.com
"
"   munged:    <username>·<domain>·<com>
"              |username|at|domain|dot|com|
"              usNerOnameS@dPomAainM.com [remove NOSPAM to email]
"              userna____main.com [fill in the blank with "me@do"]
"
" This script provides a visual mode mapping Shift+F12 which will
" munge any selected email address per the first two examples above.
" (The second two methods are on the ToDo.) The script's main
" function:
" 
"   Cream_email_munge() 
"
" will also return a munged address if passed a valid email, like:
"
"   let mymunge = Cream_email_munge("username@domain.com")
"
" Installation:
" * Drop this file into your plugins directory and (re)start Vim.
"
" Todo:
" * More sophisticated munging like:  
"      usNerOnameS@dPomAainM.com [remove letters NOSPAM for address]
"

" list as an add-on if Cream project in use
if exists("$CREAM")
	call Cream_addon_list(
	\ 'Email Munge', 
	\ 'Make an email spam resistant', 
	\ 'Protect an email address from web bots by munging it into computer indecipherable form that is still human readable. Example: {creamforvim} * {mindspring} * {com}', 
	\ 'Email Munge', 
	\ '<Nil>', 
	\ "call Cream_email_munge(\"v\")"
	\ )
else
    vmap <silent> <S-F12> :call Cream_email_munge("v")
endif

function! Cream_email_munge(email)
" returns a randomly munged variation of any email address passed
" arguments: [any valid email address] returns munged 
"            "v" implies visual mode call, munges current selection

	" if visual mode, munge selection
	if a:email == "v"
		normal gv
		normal "xy
		normal gv
		let myemail = @x
	else
		let myemail = a:email
	endif

	" get last two digits of localtime(), 00-99
	let rnd = matchstr(localtime(), '..$') + 0

	" munge separators, switch every second
	if     rnd[1] % 5 < 1
		let sep1 = "|"
		let sep2 = "|"
	elseif rnd[1] % 5 < 2
		let sep1 = "["
		let sep2 = "]"
	elseif rnd[1] % 5 < 3
		let sep1 = "{"
		let sep2 = "}"
	elseif rnd[1] % 5 < 4
		let sep1 = "<"
		let sep2 = ">"
	else
		let sep1 = "("
		let sep2 = ")"
	endif

    " replace @ and . characters (most times), switch every 5 seconds
	if     rnd % 25 < 8
		let at = sep2 . "at" . sep1
		let dot = sep2 . "dot" . sep1
	elseif rnd % 25 < 12
		let at = sep2 . " " . sep1
		let dot = sep2 . " " . sep1
	elseif rnd % 25 < 15
		let at = sep2 . "·" . sep1
		let dot = sep2 . "·" . sep1
	elseif rnd % 25 < 18
		let at = sep2 . "*" . sep1
		let dot = sep2 . "*" . sep1
	else
		let at = sep2 . "@" . sep1
		let dot = sep2 . "." . sep1
	endif

	let myemail = substitute(myemail, "@", at, "g")
	let myemail = substitute(myemail, "\\.", dot, "g")

	let myemail = sep1 . myemail . sep2

	" if visual mode, paste over selection with munge
	if a:email == "v"
		let @x = myemail
		normal "xp
		normal gv
	else
		return myemail
	endif

endfunction


