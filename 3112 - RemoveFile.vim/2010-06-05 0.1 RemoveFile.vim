" RemoveFile.vim  -  Remove a file from the disk
"
" Copyright June 2010 by Christian J. Robinson <infynity@onewest.net>
"
" Distributed under the terms of the Vim license.  See ":help license".
"
" Usage:
"
" :Remove[!] [file]
"
" With the ! it will remove the specified file without prompting.  If file is
" omitted, it uses the file associated with the current buffer.

command! -bar -bang -nargs=? Remove call RemoveFile(<q-args>, '<bang>')

function! RemoveFile(...)
	if a:0 >= 1 && a:1 != ''
		let file = a:1
	else
		let file = expand('%')
	endif

	if !filewritable(file)
		echohl ErrorMsg
		echomsg 'File "' . fnamemodify(file, ':t') . '" does not exist or is not writable.'
		echohl None
		return
	endif

	if a:0 >= 2 && a:2 == '!'
		let confirm = 1
	else
		let confirm = confirm('Really remove "' . fnamemodify(file, ':t') . '"?', "&Yes\n&No", 2, 'W')
	endif

	if confirm == 1
		call delete(file)
	endif
endfunction
