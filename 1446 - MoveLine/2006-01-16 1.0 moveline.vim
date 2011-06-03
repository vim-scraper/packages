" Move Line 
" Simply moves a line up or down
" Something I really liked in notepad2 for windows
"
"
" (c) 2006 Joe Chrzanowski
" Sonicdev
" http://sonicdev.net
map r :call MoveLine(line("."),-1)<cr>k
map f :call MoveLine(line("."),1)<cr>j

function! MoveLine(curline, shift)
	let curline=a:curline
	let shift=a:shift
	let toline=line(".")+shift
	if (toline > 0)
		let curlndata=getline(curline)
		let prevlndata=getline(toline)
		call setline(curline,prevlndata)
		call setline(toline,curlndata)
	endif
endfunc
