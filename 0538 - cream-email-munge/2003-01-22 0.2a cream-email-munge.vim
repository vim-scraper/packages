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
" Date:    2003-01-22
" Version: 0.2a
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
"   unmunged:  
"        username@domain.com
"
"   munged, separator type:
"        <username>·<domain>·<com>
"        |username|at|domain|dot|com|
"
"   munged, substitution type:
"        useNrnaOme@SdomPainA.coMm (remove "NOSPAM" to email)
"        useSrnPamAe@MdoFmaRinE.cEom (remove "SPAMFREE" to email)
"
"   munged, completion type:
"        usernam____n.com (insert "e@domai" to email)
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
    vmap <silent> <S-F12> :call Cream_email_munge("v")<CR>
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

	" get random number (last two digits of localtime(), 00-99)
	let rnd1 = matchstr(localtime(), '..$') + 0
	" get random number, boolean (0 or 1)
	let rnd2 = rnd1[1] % 2
	" get random number, boolean (0 or 1) 
	" (uses tens of seconds register of localtime())
	let rnd3 = rnd1[0] % 2

	if rnd2 == 0
		" do separator-type munge
		let myemail = s:Cream_email_munge_separate(myemail, rnd1)
	else
		if rnd3 == 0
			" do substitution-type munge
			let myemail = s:Cream_email_munge_substitute(myemail, rnd1)
		else
			" do substitution-type munge
			let myemail = s:Cream_email_munge_completion(myemail, rnd1)
		endif
	endif

	" if visual mode, paste back over 
	if a:email == "v"
		let @x = myemail
		normal "xp
		normal gv
	else
		return myemail
	endif

endfunction

function! s:Cream_email_munge_separate(email, random)
" return separator-type munge of passed email address
" Example:  {username}at{domain}dot{com}

	let myemail = a:email
	let rnd1 = a:random

	" munge separators, switch every second
	if     rnd1[1] % 5 < 1
		let sep1 = "|"
		let sep2 = "|"
	elseif rnd1[1] % 5 < 2
		let sep1 = "["
		let sep2 = "]"
	elseif rnd1[1] % 5 < 3
		let sep1 = "{"
		let sep2 = "}"
	elseif rnd1[1] % 5 < 4
		let sep1 = "<"
		let sep2 = ">"
	else
		let sep1 = "("
		let sep2 = ")"
	endif

    " replace @ and . characters (most times), switch every 5 seconds
	if     rnd1 % 25 < 8
		let at = sep2 . "at" . sep1
		let dot = sep2 . "dot" . sep1
	elseif rnd1 % 25 < 12
		let at = sep2 . " " . sep1
		let dot = sep2 . " " . sep1
	elseif rnd1 % 25 < 15
		let at = sep2 . "·" . sep1
		let dot = sep2 . "·" . sep1
	elseif rnd1 % 25 < 18
		let at = sep2 . "*" . sep1
		let dot = sep2 . "*" . sep1
	else
		let at = sep2 . "@" . sep1
		let dot = sep2 . "." . sep1
	endif

	let myemail = substitute(myemail, "@", at, "g")
	let myemail = substitute(myemail, "\\.", dot, "g")

	let myemail = sep1 . myemail . sep2

	return myemail

endfunction

function! s:Cream_email_munge_substitute(email, random)
" return substitution-type munge of passed email address
" Example:  usNerOnameS@dPomAainM.com [remove NOSPAM to email]

	let myemail = tolower(a:email)
	let rnd1 = a:random

	if rnd1[0] < 5
		let spliceword = "NOSPAM"
	else
		let spliceword = "SPAMFREE"
	endif
	" * divide email length by spliceword length to obtain interval
	let interval = strlen(myemail) / strlen(spliceword)

	" splice in word at interval
	let strfirst = ""
	let strlast = ""
	let pos = (interval / 2) - interval + 1
	let i = 0
	while i < strlen(spliceword)
		let pos = pos + interval + 1
		" get first part
		let strfirst = strpart(myemail, 0, pos)
		" get last part
		let strlast = strpart(myemail, pos)
		" concatenate
		let myemail = strfirst . spliceword[i] . strlast
		let i = i + 1
	endwhile

	return myemail . " (remove \"" . spliceword . "\" to email)"

endfunction

function! s:Cream_email_munge_completion(email, random)
" Example:  userna____main.com [fill in blank with "me@do"]

	let myemail = a:email
	" random is (1 - 5) - 1, based on tens of seconds register of localtime()
	let rnd1 = (a:random[0] % 5) + 3

	let pos = match(myemail, "@") - (a:random[1] % 5 / 2)
	let strfirst = strpart(myemail, 0, pos)
	let strmiddle = strpart(myemail, pos, rnd1)
	let strlast = strpart(myemail, pos + rnd1)

	return strfirst . "____" . strlast . " (insert \"" . strmiddle . "\" to email)"

endfunction

