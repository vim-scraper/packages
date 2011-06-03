"======================================================================
" cream-capitalization.vim
"
" One of the many custom utilities and functions for gVim from the
" Cream project ( http://cream.sourceforge.net ), a configuration of
" Vim in the vein of Apple and Windows software you already know.
"
" Date:   15 Apr 2002
" Source: http://vim.sourceforge.net/scripts/script.php?script_id=242
" Author: Steve Hall  [ digitect@mindspring.com ]
" License: GPL (http://www.gnu.org/licenses/gpl.html)
"
" Instructions:
" * Simply copy this file and paste it into your vimrc. Or you can
"   drop the entire file into your plugins directory.
" * As long as you don't already have keyboard mappings to the F5 key,
"   these keyboard shortcuts will now be available:
"   F5        Capitalize selection, title case
"   Shift+F5  Uppercase selection
"   Alt+F5    Lowercase selection
"   Ctrl+F5   Reverse case of selection
"
" Notes:
" * Restoration of selection is not entirely accurate, especially
"   across multiple lines.
"
" ChangeLog:
" 2002-04-15 -- Version 1.3
" * Fixed positioning "anomalies" for title case. (Hopefully for good!)
"
" 2002-03-26 -- Version 1.2
" * Work around broken Vim paste back behavior at column 1
"
" 2002-03-25 -- Version 1.1
" * Modified title case function to lower case all text before
"   capitalizing initial characters.
"
" 2002-03-10
" * Initial Release

" Uppercase
vmap <silent> <S-F5> U
" Lowercase
vmap <silent> <M-F5> u
" Reverse case
vmap <silent> <C-F5> ~gv

" Title case
" copy into register "x", call function
vmap <silent> <F5> "xy:call Cream_capitalize()<CR>
function! Cream_capitalize()
" * Uppercase characters following whitespace

	" assign selection (on register "x") to variable
	let mystr = @x
    " lower case entire string
    let mystr = tolower(mystr)
	" capitalize chars following these characters...
	let mystr = Cream_capitalize_after(mystr, " ")
	let mystr = Cream_capitalize_after(mystr, "(")
	let mystr = Cream_capitalize_after(mystr, ")")
	let mystr = Cream_capitalize_after(mystr, "\<Tab>")
	let mystr = Cream_capitalize_after(mystr, "\<NL>")
	let mystr = Cream_capitalize_after(mystr, "\<CR>")
	let @x = mystr

	" reselect
	normal gv
	" paste over selection (replacing it)
	normal "xp
	" reselect
	normal gv

endfunction

function! Cream_capitalize_after(mystr, mychar)
" * Used by Cream_capitalize() to capitalize the character of a string
"   following a given character
" * Also capitalizes the first character of a string (no way to decide not to)
	" initialize variables
	let mystrfirst = ""
	let mystrmiddle = ""
	let mystrlast = ""
	let mypos = 0
	let newstr = a:mystr
	" do while char still found
	while mypos != -1
		" get first part
		let mystrfirst = strpart(newstr, 0, mypos)
		" get middle part (always one char)
		let mystrmiddle = strpart(newstr, mypos, 1)
		" get last part
		let mystrlast = strpart(newstr, mypos + 1)
		" capitalize middle
		let mystrmiddle = toupper(mystrmiddle)
		" concatenate
		let newstr = mystrfirst . mystrmiddle . mystrlast
		" find new pos
		let mypos = matchend(newstr, a:mychar, mypos)
	endwhile
	return newstr
endfunction

