" Vim global plugin for visual rcsdiff
" Last Change: 2002 Jul 22
" Maintainer:  Brian Nelson <bcnelson@imcglobal.com> 

if exists("loaded_rcsdiff")
	finish
endif
let loaded_rcsdiff = 1

if !hasmapto('<Plug>Rcsdiff')
	map <unique> <Leader>rcsdiff <Plug>Rcsdiff
endif

noremap <unique> <script> <Plug>Rcsdiff :call <SID>RcsDiff()<CR>

if !exists(":Rcsdiff")
	command -nargs=? Rcsdiff :call s:RcsDiff(<f-args>)
endif


" optional argument to set revision number
function! s:RcsDiff(...)

	let ftype = &filetype
	let rev = ''

	if a:0 > 0
		let rev = a:1
	endif

	let tmpfile = tempname()
	let cmd = "co -p" . rev . " " . bufname("%") . " > " . tmpfile
	
	let cmd_output = system(cmd)

	if v:shell_error && cmd_output != ""
		echohl WarningMsg | echon cmd_output | echohl None
		return
	endif

	exe "vert diffsplit" . tmpfile
	exe "set filetype=" . ftype
	set nomodifiable
endfunction
