"
" uniq.vim -- Emulations of the "uniq" utility in Vim script
"
" Compiler: Steve Hall <digitect(at)mindspring.com>
" Updated:  2003-05-20
"
" Description:
" This script provides several examples of the Unix "uniq" utility
" emulated entirely with Vim script, found during some brief research
" through the Vim list archives. Like the original uniq, each requires
" the selected lines "uniq-ed" to already be sorted so that duplicates
" are adjacent.
"
" Note that there are generally two approches here. The first uses the
" getline() function to examine adjacent lines, and are used by the
" first two examples below. A second approach is illustrated by the
" last three examples which use a single global or substitution
" command to accomplish the same purpose.
"
" We share this compilation in hopes that it might be helpful. The
" actual origins of each section are documented below. Use at your own
" risk!
"


"---------------------------------------------------------------------
" Author:  Jean Jordaan <jean(at)upfrontsystems.co.za>
" Source:  (http://groups.yahoo.com/group/vim/message/16444
" Date:    Mon Feb 26, 2001  5:34 am
" Subject: Some functions to read mailing list digests with Vim.
"
function! Uniq1(...) range
	" use arguments if two passed
	if a:0 == 2
		let a = a:1
		let z = a:2
	" use range
	else
		let a = a:firstline
		let z = a:lastline
	endif
	while (a <= z)
		let str1 = getline(a)
		let str2 = getline(a+1)
		if (str1 == str2)
			execute a . "delete"
			let z = z - 1
		else
			let a = a + 1
		endif
	endwhile
endfunction
command! -nargs=0 -range Uniq1 <line1>,<line2>call Uniq1()


"---------------------------------------------------------------------
" Author: Thomas Köhler <jean-luc@p...> 
" Source: http://groups.yahoo.com/group/vim/message/36599
" Date:   Thu Feb 6, 2003  1:15 pm
" Re:     Re: emulation of uniq in VimL
"
function! Uniq2()
	" jump to first line in buffer
	normal 1G
	" while we're not on the last line
	while line(".") < line("$") 
		" current line is the same as last line
		if getline(".") == getline(line(".") - 1)
			" so delete it
			delete
		" current line != last line
		else
		" just go to next line
			normal j
		endif
	endwhile
	" we are on the last line
	while getline(".") == getline(line(".") - 1)
		" remove it if it matches
		delete
	" the line before
	endwhile
endfunction


"---------------------------------------------------------------------
" Author: Piet Delport <pjd(at)303.za.net>
" Source: http://groups.yahoo.com/group/vim/message/36621
" Date:   Thu Feb 6, 2003  5:24 pm
" Re:     Re: emulation of uniq in VimL
"
command -range=% Uniq3 <line1>,<line2>g/^\%<<line2>l\(.*\)\n\1$/d


"---------------------------------------------------------------------
" Author: Preben 'Peppe' Guldberg" <peppe@x...>
" Source: http://groups.yahoo.com/group/vim/message/36624
" Date:   Thu Feb 6, 2003  5:53 pm
" Re:     Re: emulation of uniq in VimL
"
function! Uniq4() range
	let l1 = a:firstline
	let l2 = a:lastline
	silent execute l1 . ',' . (l2 - 1) . 's/^\(.*\)\%(\n\%<' . (l2 + 1) . 'l\1$\)\+/\1/e'
endfunction


"---------------------------------------------------------------------
" Author: Luc Hermitte <hermitte@f...>
" Source: http://groups.yahoo.com/group/vim/message/36633
" Date:   Thu Feb 6, 2003  7:43 pm
" Re:     Re: emulation of uniq in VimL
"
function! Uniq5() range
	let l1 = a:firstline
	let l2 = a:lastline
	" avoid when l1 == l2 : one line selected
	if l1 < l2 
		silent execute l1 . ',' . (l2 - 1) . 's/^\(.*\)\%(\n\%<' . (l2 + 1) . 'l\1$\)\+/\1/e'
		" [ed. -- unnecessary, functions don't change this]
		"" purge 'search' history
		"call histdel('search', -1)
		"let @/ = histget('search', -1)
	endif
endfunction

