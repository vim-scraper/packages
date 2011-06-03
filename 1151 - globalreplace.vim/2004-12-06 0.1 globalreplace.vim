" This function makes global search and replace a little easier 
" Author: Chris Rummel
" Date: 2004 Dec 05

if exists("*MISC_GlobalReplace")
	finish
endif

"map <Leader>mg :call MISC_GlobalReplace()<cr>

function! MISC_GlobalReplace()

	let l:find = input("Find: ")
	if l:find == ''
		return
	endif

	let l:replace = input("Replace: ")
	" don't check for empty l:replace in case
	" you want to replace l:find with nothing

	execute "%s/".l:find."/".l:replace."/gc"

endfunction


