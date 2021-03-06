"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Buftabs (C) 2006 Ico Doornekamp
"
" Introduction
" ------------
" This is a simple script that shows a tabs-like list of buffers in the bottom
" of the window. The biggest advantage of this script over various others is
" that it does not take any lines away from your terminal, leaving more space
" for the document you're editing. The tabs are only visible when you need
" them - when you are switchin between buffers.
"
" Usage
" -----
" This script draws buffer tabs on vim startup, when a new buffer is created
" and when switching between buffers.
"
" It might be handy to create a few maps for easy switching of buffers in your
" .vimrc file. For example:
"
"   noremap <f1> :bprev<CR> 
"   noremap <f2> :bnext<CR>
"
"
" The following extra configuration variables are availabe:
" 
" - g:buftabs_only_basename
"
"   Set this to 1 to make buftabs only print the filename of each buffer,
"   omitting the directory name. Add to your .vimrc:
"
"   :let g:buftabs_only_basename=1
"
"
" Bugs
" ----
"
" Vim's 'set hidden' option is known to break this plugin - for some reason
" vim will overwrite the buftabs when this option is enabled. 
"
"
" Changelog
" ---------
" 
" 0.1	2006-09-22	Initial version	
"
" 0.2	2006-09-22  Better handling when the list of buffers is longer then the
"                 window width.
"
" 0.3	2006-09-27  Some cleanups, set 'hidden' mode by default
"
" 0.4	2007-02-26  Don't draw buftabs until VimEnter event to avoid clutter at
"                 startup in some circumstances
"
" 0.5	2007-02-26  Added option for showing only filenames without directories
"                 in tabs
"
" 0.6	2007-03-04  'only_basename' changed to a global variable.  Removed
"                 functions and add event handlers instead.  'hidden' mode 
"                 broke some things, so is disabled now. Fixed documentation
" 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"
" Called on VimEnter event
"

let s:buftabs_enabled = 0

function! Buftabs_enable()
	let s:buftabs_enabled = 1
endfunction


"
" Draw the buftabs
"

function! Buftabs_show()

	let l:i = 1
	let l:list = ''
	let l:start = 0
	let l:end = 0
	if ! exists("g:from") 
		let g:from = 0
	endif

	if s:buftabs_enabled != 1 
		return
	endif

	" Walk the list of buffers

	while(l:i <= bufnr('$'))

		" Only show buffers in the list, and omit help screens
	
		if buflisted(l:i) && getbufvar(l:i, "&modifiable") 

			" Append the current buffer number and name to the list. If the buffer
			" is the active buffer, it is enclosed in square brackets. If it is
			" modified, it is appended with an exclaimation mark

			if bufwinnr(l:i) != -1
				let l:list = l:list . '['
				let l:start = strlen(l:list)
			else
				let l:list = l:list . ' '
			endif
				
			let l:list = l:list . l:i . "-" 

			if exists("g:buftabs_only_basename")
				let l:list = l:list . fnamemodify(bufname(l:i), ":t")
			else
				let l:list = l:list . bufname(l:i)
			endif

			if getbufvar(l:i, "&modified") == 1
				let l:list = l:list . "!"
			endif
			
			if bufwinnr(l:i) != -1
				let l:list = l:list . ']'
				let l:end = strlen(l:list)
			else
				let l:list = l:list . ' '
			endif
		end

		let l:i = l:i + 1
	endwhile

	" If the resulting list is too long to fit on the screen, chop
	" out the appropriate part

	let l:width = winwidth(0) - 12

	if(l:start < g:from) 
		let g:from = l:start - 1
	endif
	if l:end > g:from + l:width
		let g:from = l:end - l:width 
	endif
		
	let l:list = strpart(l:list, g:from, l:width)

	" Show the list
	
	redraw
	echon l:list

endfunction


" Hook to events to show buftabs at startup, when creating and when switching
" buffers

autocmd VimEnter * call Buftabs_enable()
autocmd VimEnter * call Buftabs_show()
autocmd BufNew * call Buftabs_show()
autocmd BufEnter * call Buftabs_show()

" vi: ts=2 sw=2

