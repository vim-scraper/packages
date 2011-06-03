" Vim script to move the cursor half way down/up the screen. This can be
" thought of as a binary search for a line that is visible in the window by
" either moving up or down.
"
" Created:	2007 Aug 05
" Last Change:	2007 Aug 05
" Author:	Andy Spencer <andy753421 at gmail dot com>
" Thanks To:	#vim@irc.freenode.net

" Installation:
"   1. Add this file to $RUNTIME/plugins
"
" Usage:
"   Press <C-K> to jump half way to the top of the screen
"   Press <C-J> to jump half way to the bottom of the screen
"   Pressing <C-[KJ]> several times in a row will jump halfway to the
"   top/bottom or half way to a previous jump location.
"   Example: To jump 3/8 of the way to the bottom of the screen press
"     <C-J><C-K><C-J>
"   
" Customization:
"   map <silent> <YOUR-DOWN-KEY> :call HM_Move("down")<CR>
"   map <silent> <YOUR-UP-KEY>   :call HM_Move("up")<CR>
"
" Problems:
"   <C-J> and <C-K> will not be mapped over existing mappings. If other
"   scripts map <C-J> or <C-K> then you will need to manually override those.

function HM_Inc()
	if (!exists("s:reset"))
		let s:reset = 1
	endif
	let s:reset += 1
endfunction

function HM_Move(dir)
	if (!exists("s:reset") || s:reset > 1)
		let s:upper = line("w0")
		let s:lower = line("w$")
	endif

	if (a:dir == "up")
		let s:lower = line(".") - 1
	elseif (a:dir == "down")
		let s:upper = line(".") + 1
	endif

	let s:dest = ((s:lower-s:upper)/2)+s:upper

	"echo "^".s:upper "v".s:lower "=".s:dest

	if (s:upper <= s:lower && s:dest != line("."))
		let s:reset = 0
		call cursor(s:dest, col("."))
		" call cursor increments reset
	else
		let s:reset = 1
	endif
endfunction

autocmd CursorMoved * call HM_Inc()

if mapcheck("<C-J>") == "" && mapcheck("<C-K>") == ""
	map <silent> <C-J> :call HM_Move("down")<CR>
	map <silent> <C-K> :call HM_Move("up")<CR>
endif
