"======================================================================
" cream-keytest.vim -- Test keyboard characters
"
" Cream -- An easy-to-use configuration of the famous Vim text editor
" [ http://cream.sourceforge.net ] Copyright (C) 2002  Steve Hall
" 
" License:
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
" [ http://www.gnu.org/licenses/gpl.html ]
" 
" This program is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
" General Public License for more details.
" 
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
" 02111-1307, USA.
"
" Description:
" Test and interpret individual keyboard characters. Useful to see
" exactly what code your hardware and software combination returns for
" a given keystroke. Where our interpretation table isn't complete,
" the actual decimal values for the key pressed are returned.
"
" Date:    2002-11-18
" Version: 1.0
" Source:  http://vim.sourceforge.net/script.php?script_id=488
" Author:  Steve Hall  [ digitect@mindspring.com ]
" License: GPL (http://www.gnu.org/licenses/gpl.html)
"
" Installation:
" Just drop this file into your plugins directory, and start Vim.
"
" Usage:
" * Call "Cream_keytest()" to get feedback about each key pressed.
" * Call "Cream_keyinterpret()" to return only the interpretation.
"   An empty return means character not yet in the table.
"
" Notes:
" * The interpretation table is currently unfinished. There are
"   literally between 400 and 700 possible keystroke combinations on
"   the average keyboard. Please look at the complete Ctrl/Shift/Alt
"   combination interpretations of the Ins, Del, Home, End, PgUp, PgDn
"   keys for an example of a full test.
"
" * This has only been tested with &encoding=latin1.
"
" * If you take the time to develop the script further, please forward
"   your explorations or improvements them back to us! (Besides,
"   that's what the GPL is all about. ;)
"

" don't list unless Cream developer
if !exists("g:cream_dev")
	finish
endif

" list as an add-on if Cream project in use
if exists("$CREAM")
	call Cream_addon_list(
	\ 'Key Test', 
	\ 'Interpret keystroke characters', 
	\ "Test and interpret individual keyboard characters. Useful to see exactly what code your hardware and software combination returns for a given keystroke. Where our table isn't complete, the actual decimal value(s) are returned.", 
	\ 'Devel.Key Test', 
	\ 'call Cream_keytest()'
	\ )
endif

function! Cream_keytest()

	" currently, we've only tested &encoding=latin1
	if &encoding !=? 'latin1'
		call confirm(
			\ "Not tested with &encoding value \"" . &encoding . "\". Quitting...\n" .
			\ "(Please contact us if you wish to test an alternate coding!)\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	endif

	let n = confirm("Please press continue then press a key to interpret. (Esc to Quit)", "&Continue", 1, "Info")
	if n != 1
		return
	endif

	while n == 1
		let myreturn = Cream_keyinterpret(1)
		if myreturn ==? "esc"
			return
		endif
	endwhile

endfunction

function! Cream_keyinterpret(...)
" if optional argument provided, use per-character feedback dialog

	" our interpretation
	let mycharname = ""
	let multi = ""

	" get character
	let mychar = getchar()

	if     mychar == 9
		let mycharname = "tab"
	elseif mychar == 10
		let mycharname = "linefeed"
	elseif mychar == 13
		let mycharname = "return"
	elseif mychar == 26
		let mycharname = "ctrl+z"
	elseif mychar == 27
		let mycharname = "esc"
	elseif mychar == 32
		let mycharname = "space"
	" alpha-numeric
	elseif mychar > 32
	\ &&   mychar < 127
		let mycharname = nr2char(mychar)

	" multi-byte has initial Esc
	elseif mychar[0] == nr2char("128")
		" interpret remainder
		let mytemp = strpart(mychar, 1)

		" backspace
		if     mytemp ==# "kb"
			let mycharname = "backspace"
		elseif mytemp ==# "��kb"
			let mycharname = "shift+backspace"

		" down
		elseif mytemp ==# "kd"
			let mycharname = "down"

		" up
		elseif mytemp ==# "ku"
			let mycharname = "up"

		" left
		elseif mytemp ==# "kl"
			let mycharname = "left"
		elseif mytemp ==# "#4"
			let mycharname = "shift+left"

		" right
		elseif mytemp ==# "kr"
			let mycharname = "right"
		elseif mytemp ==# "%i"
			let mycharname = "shift+right"


		" insert
		elseif mytemp ==# "kI"
			let mycharname = "insert"
		elseif mytemp ==# "#3"
			let mycharname = "shift+insert"
		elseif mytemp ==# "��kI"
			let mycharname = "ctrl+insert"
		elseif mytemp ==# "��kI"
			let mycharname = "alt+insert"
		elseif mytemp ==# "��kI"
			let mycharname = "ctrl+alt+insert"
		elseif mytemp ==# "��#3"
			let mycharname = "ctrl+shift+insert"
		elseif mytemp ==# "��#3"
			let mycharname = "alt+shift+insert"
		elseif mytemp ==# "��#3"
			let mycharname = "ctrl+alt+shift+insert"

		" delete
		elseif mytemp ==# "kD"
			let mycharname = "delete"
		elseif mytemp ==# "*4"
			let mycharname = "shift+delete"
		elseif mytemp ==# "��kD"
			let mycharname = "ctrl+delete"
		elseif mytemp ==# "��kD"
			let mycharname = "alt+delete"
		elseif mytemp ==# "��*4"
			let mycharname = "ctrl+shift+delete"
		elseif mytemp ==# "��kD"
			let mycharname = "ctrl+alt+delete"
		elseif mytemp ==# "��*4"
			let mycharname = "alt+shift+delete"
		elseif mytemp ==# "��*4"
			let mycharname = "ctrl+alt+shift+delete"

		" home
		elseif mytemp ==# "kh"
			let mycharname = "home"
		elseif mytemp ==# "#2"
			let mycharname = "shift+home"
		elseif mytemp ==# "�O"
			let mycharname = "ctrl+home"
		elseif mytemp ==# "��kh"
			let mycharname = "alt+home"
		elseif mytemp ==# "��#2"
			let mycharname = "ctrl+shift+home"
		elseif mytemp ==# "���O"
			let mycharname = "ctrl+alt+home"
		elseif mytemp ==# "��#2"
			let mycharname = "alt+shift+home"
		elseif mytemp ==# "��#2"
			let mycharname = "ctrl+alt+shift+home"

		" end
		elseif mytemp ==# "@7"
			let mycharname = "end"
		elseif mytemp ==# "*7"
			let mycharname = "shift+end"
		elseif mytemp ==# "�P"
			let mycharname = "ctrl+end"
		elseif mytemp ==# "��@7"
			let mycharname = "alt+end"
		elseif mytemp ==# "��*7"
			let mycharname = "ctrl+shift+end"
		elseif mytemp ==# "���P"
			let mycharname = "ctrl+alt+end"
		elseif mytemp ==# "��*7"
			let mycharname = "alt+shift+end"
		elseif mytemp ==# "��*7"
			let mycharname = "ctrl+alt+shift+end"

		" pgup
		elseif mytemp ==# "kP"
			let mycharname = "pgup"
		elseif mytemp ==# "��kP"
			let mycharname = "shift+pgup"
		elseif mytemp ==# "��kP"
			let mycharname = "ctrl+pgup"
		elseif mytemp ==# "��kP"
			let mycharname = "alt+pgup"
		elseif mytemp ==# "��kP"
			let mycharname = "ctrl+shift+pgup"
		elseif mytemp ==# "��kP"
			let mycharname = "ctrl+alt+pgup"
		""*** broken (252.010.128.107.080) -- 010 removes to line end!
		"elseif mytemp ==# "� �kP"
		"    let mycharname = "alt+shift+pgup"
		elseif mytemp ==# "��kP"
			let mycharname = "ctrl+alt+shift+pgup"

		" pgdn
		elseif mytemp ==# "kN"
			let mycharname = "pgdn"
		elseif mytemp ==# "��kN"
			let mycharname = "shift+pgdn"
		elseif mytemp ==# "��kN"
			let mycharname = "ctrl+pgdn"
		elseif mytemp ==# "��kN"
			let mycharname = "alt+pgdn"
		elseif mytemp ==# "��kN"
			let mycharname = "ctrl+shift+pgdn"
		elseif mytemp ==# "��kN"
			let mycharname = "ctrl+alt+pgdn"
		""*** broken (252.010.128.107.078) -- 010 removes to line end!
		"elseif mytemp ==# "� �kN"
		"    let mycharname = "alt+shift+pgdn"
		elseif mytemp ==# "��kN"
			let mycharname = "ctrl+alt+shift+pgdn"


		" F-keys
		elseif mytemp ==# "k1"
			let mycharname = "F1"
		elseif mytemp ==# "�"
			let mycharname = "shift+F1"
		elseif mytemp ==# "��k1"
			let mycharname = "ctrl+F1"

		elseif mytemp ==# "k2"
			let mycharname = "F2"
		elseif mytemp ==# "�"
			let mycharname = "shift+F2"
		elseif mytemp ==# "��k2"
			let mycharname = "ctrl+F2"

		elseif mytemp ==# "k3"
			let mycharname = "F3"
		elseif mytemp ==# "�"
			let mycharname = "shift+F3"
		elseif mytemp ==# "��k3"
			let mycharname = "ctrl+F3"

		elseif mytemp ==# "k4"
			let mycharname = "F4"
		elseif mytemp ==# "�	"
			let mycharname = "shift+F4"
		elseif mytemp ==# "��k4"
			let mycharname = "ctrl+F4"

		elseif mytemp ==# "k5"
			let mycharname = "F5"
		""*** broken (128.253.010) -- 010 removes to line end!
		"elseif mytemp ==# "� "
		"    let mycharname = "shift+F5"
		elseif mytemp ==# "��k5"
			let mycharname = "ctrl+F5"

		elseif mytemp ==# "k6"
			let mycharname = "F6"
		elseif mytemp ==# "�"
			let mycharname = "shift+F6"
		elseif mytemp ==# "��k6"
			let mycharname = "ctrl+F6"

		elseif mytemp ==# "k7"
			let mycharname = "F7"
		elseif mytemp ==# "�"
			let mycharname = "shift+F7"
		elseif mytemp ==# "��k7"
			let mycharname = "ctrl+F7"

		elseif mytemp ==# "k8"
			let mycharname = "F8"
		elseif mytemp ==# "�"
			let mycharname = "shift+F8"
		elseif mytemp ==# "��k8"
			let mycharname = "ctrl+F8"

		elseif mytemp ==# "k9"
			let mycharname = "F9"
		elseif mytemp ==# "�"
			let mycharname = "shift+F9"
		elseif mytemp ==# "��k9"
			let mycharname = "ctrl+F9"

		elseif mytemp ==# "k;"
			let mycharname = "F10"
		elseif mytemp ==# "�"
			let mycharname = "shift+F10"
		" unknown (Windows usurps, didn't reboot to GNU/Linux ;)
		"elseif mytemp ==# "��k1"
		"    let mycharname = "ctrl+F10"

		elseif mytemp ==# "F1"
			let mycharname = "F11"
		elseif mytemp ==# "�"
			let mycharname = "shift+F11"
		elseif mytemp ==# "��F1"
			let mycharname = "ctrl+F11"

		elseif mytemp ==# "F2"
			let mycharname = "F12"
		elseif mytemp ==# "�"
			let mycharname = "shift+F12"
		elseif mytemp ==# "��F2"
			let mycharname = "ctrl+F12"

		endif

	else
		" uninterpreted, but not preceded by Esc.
	endif

	" requested optional per-character feedback
	if exists("a:1")

		if mycharname != ""
			let msg = " mycharname       = " . mycharname . "\n"
		else
			" decipher each char of multi-char codes
			if strlen(mychar) > 1
				let i = 0
				while i < strlen(mychar)
					if i != 0
						let multi = multi . " + "
					endif
					let multi = multi . char2nr(mychar[i])
					let i = i + 1
				endwhile
			endif
			" compose message
			let msg = ""
			let msg = msg . "Character not interpreted:\n"
			let msg = msg . "         mychar   = " . mychar . "\n"
			let msg = msg . " char2nr(mychar)  = " . char2nr(mychar) . "\n"
			let msg = msg . " nr2char(mychar)  = " . nr2char(mychar) . "\n"
			let msg = msg . "         (multi)  = " . multi . "\n"
			let msg = msg . "\n"
			let msg = msg . " mycharname       = " . mycharname . "\n"
		endif

		" feedback
		call confirm(
			\ msg .
			\ "\n", "&Ok", 1, "Info")

	endif

	return mycharname

endfunction

