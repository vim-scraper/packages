" UpdateModDate.vim Plugin.
" It is used to update a timestamp within a file whenever it is saved
" by vim.
"
" Written Sun Nov  7 21:48:44 UTC 2004 by Greek0
" This file is placed under the public domain, do with it whatever you want.
"
" Last Modified: Sun Nov  7 22:00:02 UTC 2004		// %DATE_TAG% 


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Defaults for UMD Options
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" This is the token the plugin searches to find the correct location to insert
" the date. This token has to be on the same line as the date being updated!
let s:UMD_DateToken = '%DATE_TAG%'

" This is the pattern that must match the date format that is being used.
" The pattern determinates which part of the current text should be replaced
" with the new date.
let s:UMD_DatePattern = 
		\ '\w\{3\}\s\w\{3\}\s\+\d\+\s\+\d\+:\d\+:\d\+\s\+UTC\s\+\d\+'

" This is the command being used to gain the new date. If you change it you'll
" probably have to adept the UMD_DatePattern regexp too.
" date -u outputs date/time in UTC format, the tr part ensures the last
" newline is deleted from the date output.
let s:UMD_DateCommand = 'date -u | tr -d "\n"'



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Utility functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Retrieve an option (if defined) or the default otherwise.
function! s:GetOption(name)
  if exists("w:" . a:name)
    execute "return w:".a:name
  elseif exists("b:" . a:name)
    execute "return b:".a:name
  elseif exists("g:" . a:name)
    execute "return g:".a:name
  else
	  " If the option is defined nowhere else, it HAS to be defined at
	  " s: scope.
    execute "return s:".a:name
  endif
endfunction



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Main UpdateModDate() function
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" This function searches for the DateToken and updates a date on the same line.
function! UpdateModDate()

	" Get the current UMD options
	let l:DateToken = s:GetOption("UMD_DateToken")
	let l:DatePattern = s:GetOption("UMD_DatePattern")
	let l:DateCommand = s:GetOption("UMD_DateCommand")

	" Remember the current Line/Column, to restore the position later.
	let l:CurLine = line('.')
	let l:CurCol = col('.')

	" Start searching at the beginning of the file
	call cursor(1,1)

	" Search for the DateToken in the current buffer, store the line number
	let l:DateLine = search(l:DateToken)
	if l:DateLine != 0
		" %DATE_TAG% found, update date
		
		let l:DateLineText = getline(l:DateLine)
		let l:NewDate = system(l:DateCommand)
		let l:SubstText = 
			\ substitute(l:DateLineText, l:DatePattern, l:NewDate, '')

		call setline(l:DateLine, l:SubstText)
	endif

	" Restore previous cursor position
	call cursor(l:CurLine,l:CurCol)
endfunction



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Main UpdateModDate() function
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Register UpdateModDate to be called before a buffer is written.
autocmd BufWritePre * call UpdateModDate()
