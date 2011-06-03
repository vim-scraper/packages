
if &cp || exists("g:loaded_insertlist")
    finish
endif
let g:loaded_insertlist = 1

let s:save_cpo = &cpo
set cpo&vim

function! s:getCharCount(str)
	let l:result = 0
	let l:result = strlen(substitute(a:str, ".", "x", "g"))
	return l:result
endfunction

function! s:insertlist(format) range

	let l:virtualedit_org = &virtualedit
	setlocal virtualedit=all

    let l:cur_pos = ["", "1", "10", ""]

	try
		normal `<
		let l:start_lnum = line(".")
		let l:start_col = col(".")
		let l:cur_pos = getpos(".") 

		normal `>
		let l:end_lnum = line(".")

		let l:select_lnum = (l:end_lnum - l:start_lnum) + 1

		let l:cur_pos[1] = l:start_lnum
		let l:cur_pos[2] = l:start_col
		call setpos('.', l:cur_pos)

		let i = 1
		let l:back_string = ""
		let l:add_number = ""
		let l:front_string = ""

		let l:inpstr_len = strlen(a:format)
		while strlen(a:format) >= i
			let tmp = strpart(a:format, l:inpstr_len - i, 1)
			if tmp =~"[0-9]"
				break
			else
				let l:back_string = tmp. l:back_string
			endif
			let i = i + 1
		endwhile

		while strlen(a:format) >= i
			let tmp = strpart(a:format, l:inpstr_len - i, 1)
			if tmp !~"[0-9]"
				break
			else
				let l:add_number = tmp . l:add_number
			endif
			let i = i + 1
		endwhile
		let number_len = strlen(l:add_number)

		while strlen(a:format) >= i
			let tmp = strpart(a:format, l:inpstr_len - i, 1)
			let l:front_string = tmp . l:front_string
			let i = i + 1
		endwhile

		let i = 1
		while l:select_lnum >= i
			if i > 1
				let l:add_number = str2nr(l:add_number , 10) + 1
				let l:command_str = "let l:add_number = printf(\"%0" . number_len . "d\", l:add_number)"
				exe l:command_str
				let l:string = l:front_string . l:add_number . l:back_string
				exe ":normal i" . l:string
				let l:command_str = ":normal " . (s:getCharCount(l:string) - 1) . "hj"
			else
				exe ":normal i" . a:format
				let l:string = a:format
				let l:command_str = ":normal " . (s:getCharCount(l:string) - 1) . "hj"
			endif
			exe l:command_str
			let i = i + 1
		endwhile

	catch
		echohl WarningMsg |
			\ echo "EXCEPTION :" v:exception |
			\ echo "THROWPOINT:" v:throwpoint |
			\ echohl None
		let &l:virtualedit = l:virtualedit_org
	endtry
	let &l:virtualedit = l:virtualedit_org
endfunction

" command
command! -range -narg=1 InsertList :<line1>,<line2>call s:insertlist("<args>")
command! -range -narg=1 INSERTLIST :<line1>,<line2>call s:insertlist("<args>")

let &cpo = s:save_cpo
finish

==============================================================================
InsertList.vim : execute vim script in selected area.
==============================================================================
Author     : Shingo Sato
Version    : 1.0 
Last Update: 2009-09-15 15:25:37
==============================================================================

Installation:
 ------------
 1. Copy the InsertList.vim file to one of the following directories:
       $HOME/.vim/plugin     - Unix like systems
       $HOME/vimfiles/plugin - MS-Windows
       $VIM:vimfiles:plugin  - Macintosh
       $VIM/vimfiles/plugin  - All

Function:
 ------------
 This plugin inserts input-string before select position, and increments last-number.

Usage:
 ------------
 Execute vim script in selected area (with Visual Mode).
 Plugin will be used for create following number-list.

number-list
--------------------------
1.
     1.1.
         1.1.1.
         1.1.2.
         1.1.3.
         1.1.4.
     1.2.
     1.3.
     1.4.
     1.5.
     1.6.
2.
     2.1.
     2.2.
     2.3.
     2.4.
--------------------------

------------------------------------------------------------------------------
[command format]
:'<,'>INSERTLIST numberlist
   or 
:'<,'>InsertList numberlist

ex1: 
========================================
before text
------------


------------

(1) Please use "Visual mode blockwise", and select following text area.
------------
|
|
------------

(2) Input following command.
:'<,'>INSERTLIST 1.

after text
------------
1.
2.
------------
========================================
ex2: 
before text
------------
aaa 
bbb 
------------

(1) Please use "Visual mode blockwise", and select following text area.
------------
aaa|
bbb|
------------

(2) Input following command.
:'<,'>INSERTLIST 1.

after text
------------
aaa1.
bbb2.
------------
========================================
ex3:

before text
------------


------------

(1) Please use "Visual mode blockwise", and select following text area.
------------
|
|
------------

(2) Input following command.
:'<,'>INSERTLIST strName[0] = 

after text
------------
strName[0] = 
strName[1] = 
------------
========================================
ex4:

before text
------------


------------

(1) Set following option.
:set virtualedit=all

(2) Please use "Visual mode blockwise", and select following text area.
------------
    |
    |
------------

(3) Input following command.
:'<,'>INSERTLIST 1.

after text
------------
    1.
    2.
------------
========================================
ex5: Other input-string format pattern.

(1) 1.1 to 1.N
(2) 1-1 to 1-N
(3) <1> to <N>
(4) 001 to 00N

==============================================================================
" vim: set ff=unix et ft=vim nowrap :
