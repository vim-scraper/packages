"cmd.vim: collection of shell commands in vim version 
"Author: ypguo<guoyoooping@163.com>
"Date: 2010.4.2
"Base version: 1.0

"date - print or set the system date and time
com! -nargs=0 Date call Cmd_Date()
fun! Cmd_Date ()
	let Cmd_Cmd = 'date'
	let Cmd_Output = system(Cmd_Cmd)
	echo Cmd_Output
endfun

"ls - list directory contents
com! -nargs=0 Ls call Cmd_Ls()
fun! Cmd_Ls ()
	let Cmd_Cmd = 'ls'
	let ls_output = system(Cmd_Cmd)
	echo ls_output
endfun

" sdcv: http://sdcv.sourceforge.net/
com! -nargs=* -range=0 -complete=file Sdcv call s:Cmd_Sdcv(<q-args>)
function! s:Cmd_Sdcv (word)
	"let Cmd_Output = 'sdcv --data-dir=/cygdrive/c/Program\ Files/StarDict/dic '
	let Cmd_Output = 'sdcv '
	if a:word == ""
		let Cmd_Output .= expand('<cword>')
	endif
	let Cmd_Output .= a:word
	let sdcv_output = system(Cmd_Output)

	echohl Statement | echo sdcv_output | echohl Normal
endfunction

