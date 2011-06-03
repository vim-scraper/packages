"=============================================================================
" File: iabassist.vim
" Author: batman900 <batman900+vim@gmail.com>
" Last Change: 22-Jun-2009.
" Version: 0.02
" Usage:
"
"	:Iab("lhs", "rhs")
"	then lhs<Tab> to expand to rhs
"
"	,, select next placeholder
"	,' set default for the current placeholder
"	,. delete the current placeholder
"

" Do not load more than once, but allow the user to force reloading if deisred
if exists('loaded_iabassist') && loaded_iabassist == 1
	finish
endif
let loaded_iabassist = 1

" Delete the character under the cursor if it is a space
function! Eatchar()
	let c = nr2char(getchar())
	return (c =~ '\s') ? '' : c
endfunction

" Get the syntax style of the item under the cursor
function! s:SyntaxAtCursor()
	return synIDattr(synIDtrans(synID(line("."), col(".")-1, 1)), "name")
endfunction

" Replace abbreviation if we're not in comment or other unwanted places
" from http://vim.wikia.com/wiki/C/C%2B%2B_function_abbreviations
" a:from is what to translate from
" a:to is what to translate to
function! ExpandIfSafe(from, to)
	let syn = s:SyntaxAtCursor()
	let c = nr2char(getchar())
	" This is the string of types to ignore, it can be modified to suit
	" tastes, also if the last character typed was not a tab do not
	" complete
	if c != "\t" || syn =~? 'comment\|string\|character\|doxygen\|bibbrace'
		return a:from . c
	else
		" Recover an <KEY> that were escaped in Iab
		exec 'return "' . substitute(a:to, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g') . '"'
	endif
endfunction

" Create abbreviation suitable for ExpandIfSafe
" a:ab is the abbreviation to define
" a:full is the full text to replace a:ab with
function! Iab(ab, full)
	exec "iab <silent> <buffer> " . a:ab . " <C-R>=ExpandIfSafe('" .
		\ a:ab . "', '" . escape(a:full . '<C-R>=Eatchar()<CR><ESC>?«$1[^»]*»<CR>:nohlsearch<CR>:call IabSelectNext()<CR><C-G><C-R>=Eatchar()<CR>', '<>\"') .
		\ "')<CR>"
endfunction

" Select the next «$i» item
function! IabSelectNext()
	let curl = line(".")
	let curc = col(".")
	call searchpair('«', '', '»')
	"let endl = line(".")
	"let endc = col(".")
	normal! m'
	call cursor(curl, curc)
	normal! v``l
endfunction

function! IabSelectDefault()
	let curl = line(".")
	let curc = col(".")
	call searchpair('«', '', '»')
	normal! "_x
	call cursor(curl, curc)
	normal! "_df:
endfunction

" Newer version for the templates, allows default values (accepting with ,',
" and searching with ,,)

" Accept the default and select the next placeholder
smap <silent> ,' <ESC>`<:call IabSelectDefault()<CR>/«\$\d\+[^»]*»<CR>:nohlsearch<CR>:call IabSelectNext()<CR><C-G>

" Deltete the currently selected item, and select the next placeholder
smap <silent> ,. <C-G>"_d/«\$\d\+[^»]*»<CR>:nohlsearch<CR>:call IabSelectNext()<CR><C-G>

" Select the next placeholder
map <silent> ,, /«\$\d\+[^»]*»<CR>:nohlsearch<CR>:call IabSelectNext()<CR><C-G>
imap <silent> ,, <ESC>/«\$\d\+[^»]*»<CR>:nohlsearch<CR>:call IabSelectNext()<CR><C-G>
