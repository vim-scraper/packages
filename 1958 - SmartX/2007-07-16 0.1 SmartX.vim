" SmartX -- Smart deletion of \X escapes with the x/X/Backspace/Delete keys.
"           Whether it happens depends on which operation used and where the
"           cursor is located, and whether a count is used.
"
" Copyright July 16, 2007 Christian J. Robinson <infynity@onewest.net>
"
" Distributed under the terms of the Vim license.  See ":help license".
"
" See the commentary for the function below for customization information.


nnoremap <silent> <buffer> x     :<C-u>exe 'normal! ' . SmartX('x')<CR>
nnoremap <silent> <buffer> X     :<C-u>exe 'normal! ' . SmartX('X')<CR>
nnoremap <silent> <buffer> <DEL> :<C-u>exe 'normal! ' . SmartX('x')<CR>
inoremap <silent> <buffer> <DEL> <C-r>=SmartX("\<lt>DEL>")<CR>
inoremap <silent> <buffer> <BS>  <C-r>=SmartX("\<lt>BS>")<CR>


" SmartX({which} [, {chars}])  {{{1
"
" Arguments:
"  {which} -- What normal/insert mode key is running.
"  {chars} -- A list of characters that will be eaten if preceded by a
"             backslash.  Without a list of sequence characters to eat, eats
"             the following sequences:
"
"               \t    tab            (HT, TAB)
"               \n    newline        (NL)
"               \r    return         (CR)
"               \f    form feed      (FF)
"               \b    backspace      (BS)
"               \a    alarm (bell)   (BEL)
"               \e    escape         (ESC)
"
" {chars} can also be set by setting the b:SmartXchars or g:SmartXchars
" variables. (Precedence: function argument, b:SmartXchars, g:SmartXchars,
" internal default)
"
function! SmartX(which, ...)
	if a:0 == 1
		let l:match = a:1
	elseif exists('b:SmartXchars')
		let l:match = b:SmartXchars
	elseif exists('g:SmartXchars')
		let l:match = g:SmartXchars
	else
		let l:match = 'tnrfbae'
	endif

	if a:which ==# 'x' || a:which == "\<DEL>"
		let l:chars = strpart(getline('.'), col('.') - 1, 2)
	elseif a:which == "X" || a:which == "\<BS>"
		let l:chars = strpart(getline('.'), col('.') - 3, 2)
	else
		echoerr "Should never get here! a:which=" . a:which
	endif

	if v:count == 0 && l:chars =~# '\\[' . l:match . ']'
		return a:which . a:which
	elseif v:count > 0
		return v:count . a:which
	else
		return a:which
	endif
endfunction

" vim:fdm=marker:fdc=2:fdl=1:
