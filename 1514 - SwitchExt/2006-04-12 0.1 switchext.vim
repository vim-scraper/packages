" switchext.vim - Yuxuan Wang (wang@yuxuan.org)
"
" Switch between source and header files
"
"Usage:
" Set g:switchext_srcext in your vimrc for your source file extension.
" (default is "cpp" if ommited)
" You can map a key to :call SwitchExt() for fast switch
"
" Here's a vimrc example:
"
" let g:switchext_srcext = "cpp"
" map <F3> <ESC>:call SwitchExt()<CR>
"
"Version:
" 2006-04-12 - 0.1 - initial version

function! SwitchExt()
	let s:ext = expand('%:e')
	if exists("g:switchext_srcext")
		let s:cppext = g:switchext_srcext
	else
		let s:cppext = "cpp"
	endif
	if s:ext == "h"
		let s:newname = expand('%<') . "." . s:cppext
	elseif s:ext == s:cppext
		let s:newname = expand('%<') . ".h"
	endif
	if exists("s:newname")
		execute "e! " . s:newname
	else
		echo "This is neither a " . s:cppext . " file nor a h file"
	endif
endfunction

