"
" Defines a command 'Implement' to generate a skeleton implementation
" of a class from a C++ header file containing one or more class declarations.
"
" Author: Neil Vice
" Date:   01 December 2004
" 
" Updated 26/04/05 to prevent the absence of a namespace declaration from
" causing errors.
"

" Determines the number of lines to leave between functions
if !exists("g:InterFunctionGap")
	let g:InterFunctionGap = 2
endif

function s:SwitchWindows()
	" Switch to the last window 0 used to switch between two windows
	exe "normal \<c-w>p"
endfunction

function CopyDescriptionToLastWindow()
	" Copy the file description field if present
	if match(getline(1), '/[*][*][ \t]*$') != -1 && match(getline(4), '^ [*] .*') != -1
		exe "4"
		normal V
		let line = 5
		while match(getline(line), '^ [*][ \t]*$') == -1
			normal j
			let line = line + 1
		endwhile
		exe "normal \"ay"
		call s:SwitchWindows()
		exe "4"
		exe "normal dd\"aP"
		call s:SwitchWindows()
	endif
endfunction

function s:ExtractToken(s)
	" Search for a label
	let found = match(a:s, "\\s*\\w\\{-}[^:]:[^:]")
	if found != -1
		return stridx(a:s, ":")
	else
		" Search for a statement
		let found = match(a:s, "[^;]*[^}]\\s*;")
		if found != -1
			return stridx(a:s, ";")
		else
			" Search for the end of a class
			let found = match(a:s, "\\s*}")
			if found != -1
				return stridx(a:s, "}")
			else
				return -1
			endif
		endif
	endif
	return -1
endfunction

function s:DownCheckEOF()
	let before = line(".")
	normal j
	if before >= line(".")
		return 1
	endif
	return 0
endfunction

function s:ParseClass()
	" Get the class name
	exe "normal w\"cye/{\<cr>j"
	let class = getreg('c')

	" Parse the class declaration
	let done = 0
	let pos = -1
	let line = ""

	" Parse a token (label, function or attribute) at a time
	while done == 0
		" Start from where we left off
		if pos == -1 || pos == strlen(line)
			let line = getline(line("."))
			let done = s:DownCheckEOF()
		else
			let line = strpart(line, pos)
		endif

		" Skip single-line comments & empty lines
		while done == 0 && match(line, "^\\s*//") != -1 || match(line, "^\\s*$") != -1
			let line = getline(line("."))
			let done = s:DownCheckEOF()
		endwhile

		" Remove comments at end of string
		let comment_pos = match(line, "//")
		if comment_pos != -1
			let line = strpart(line, 0, comment_pos)
		endif
	
		" Extract a token (label, function or attribute)
		let token_end = -1
		let token_end = s:ExtractToken(line)
		while done == 0 && token_end == -1
			" Suffix a new line (removing comments)
			let newline = getline(line("."))
			let comment_pos = match(newline, "//")
			if comment_pos != -1
				let newline = strpart(newline, 0, comment_pos)
			endif
			let line = line . "\<cr>" . newline
			let done = s:DownCheckEOF()

			" Search for a token
			let token_end = s:ExtractToken(line)
		endwhile
		let token = strpart(line, 0, token_end + 1)
		let pos = token_end + 1

		" Determine token type
		if strpart(token, strlen(token) - 1) == ";"
			" Function or Attribute
			if match(token, "[^{};]*[^;{})]([^{}();]*)[^;(){}0]*;$") != -1
			
				" Function
				call s:SwitchWindows()
				let token = substitute(token, "\\s*/[*][*]", "/**", "")
				"let token = substitute(token, "\\(/[*][*].\\{-}\\) [*] ", "\\1\<cr>", "g")
				let token = substitute(token, "\\(/[*][*].*\\)\<cr>\\s*[*]/\\s*\<cr>", "\\1\<cr>/\<cr>", "g")
				let token = substitute(token, "virtual ", "", "g")
				let token = substitute(token, "static ", "", "g")
				let token = substitute(token, "\<cr>\\s* [*] ", "\<cr>", "g")
				let token = substitute(token, "\<cr>\\s* [*]", "\<cr>", "g")
				let token = substitute(token, "\\(\\i*\\)(", class . "::\\1(", "")
				let token = substitute(token, " = *0 *;$", "", "")
				let token = substitute(token, ";\\s*$", "", "")
				let token = substitute(token, "\<cr>\\s*", "\<cr>", "g")
				let token = substitute(token, "^\\s*", "", "g")
				exe "normal ddo" . token . "\<Esc>"
				exe "normal o{\<cr>}\<cr>"

				" Insert spacing after the function
				let i = 0
				while i < g:InterFunctionGap
					exe "normal o\<Esc>"
					let i = i + 1
				endwhile

				call s:SwitchWindows()
			else
				" Attribute - TODO: Handle static initialisers
			endif
		elseif strpart(token, strlen(token) - 1) == ":"
			" Label - no processing required
		elseif strpart(token, strlen(token) - 1) == "}"
			" End of class
			let done = 1
		else
			" Unknown ??
		endif
	endwhile
endfunction

function Implement()
	" Ensure this is a header file
	if expand("%:e") == "h"
		mark Z
		
		" Open the implementation in a new window
		let s:file = expand("%:p:r") . ".cc"
		let s:header = expand("%")
		silent "w " . s:file
		exe "normal \<c-w>v"
		exe "e " . s:file

		" If an implementation didn't already exist
		if filereadable(s:file) == 0
			" Disable folding
			let s:newfold = &l:foldenable
			set nofoldenable

			" Return to original window & disable folding
			call s:SwitchWindows()
			let s:oldfold = &l:foldenable
			set nofoldenable
		
			call CopyDescriptionToLastWindow()

			" Insert an include line for the header
			call s:SwitchWindows()
			exe "normal Gk2ddo#include \"" . s:header . "\"\<Esc>o"
			let i = 0
			while i < g:InterFunctionGap
				normal o
				let i = i + 1
			endwhile

			" Switch back to the header file
			call s:SwitchWindows()

			" Insert any namespace present
			normal gg
			let lastline = line(".")
			try
				exe "normal /namespace\<CR>"
			catch *
			endtry
			if lastline < line(".")
				exe "normal v/{\<cr>\"cy"
				call s:SwitchWindows()
				normal P
				exe "normal /{\<CR>o"
				call s:SwitchWindows()
				let s:Namespace = 1
			else
				let s:Namespace = 0
			endif

			" For each class declared...
			let lastline = -1
			exe "normal /^\\s*class\<CR>"
			while lastline < line(".")
				" Parse the class declaration
				call s:ParseClass()

				" Search for another class
				let lastline = line(".")
				exe "normal /^\\s*class\<CR>"
			endwhile

			" Re-enable folding and return cursor position (in orig window)
			normal 'Z
			let &l:foldenable = s:oldfold
			call s:SwitchWindows()
			normal dk
			if s:Namespace == 1
				normal O}
			endif
			set nofoldenable
			exe "normal gg/{\\n\\s*}\<CR>o"
		endif
	endif
endfunction

command Implement call Implement()

