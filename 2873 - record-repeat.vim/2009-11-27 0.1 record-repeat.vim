" Vim plugin for automagically recording and
" repeating a sequence of commands
" Maintainer: Andy Spencer
" Last Change: 2009-11-27

nnoremap <silent> @( :call RecordRepeatStart()<CR>

" Save the number of times to execute the
" commands and start recording.
function! RecordRepeatStart(...) range
	let s:count   = v:count1 - 1
	let s:old_reg = getreg('r')
	let s:old_map = maparg(')', 'n')
	nunmap @(
	nnoremap <silent> q :<CR>
	nnoremap <silent> ) :call RecordRepeatEnd()<CR>

	normal! qr
endfunction

" End recording and execute the precisely recorded
" command however many times are still needed.
function! RecordRepeatEnd(...) range
	normal! q
	if s:count != 0
		let reg_r = getreg('r')[0:-2]
		call setreg('r', reg_r)
		exec 'normal '.s:count.'@r'
	endif

	" Apply old settings
	call setreg('r', s:old_reg)
	nunmap q
	nnoremap <silent> @( :call RecordRepeatStart()<CR>
	if s:old_map == ""
		nunmap )
	else
		execute 'nnoremap ) ' . s:old_map
	endif
endfunction
