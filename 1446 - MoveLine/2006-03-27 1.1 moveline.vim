" Move Line
" Simply moves a line up or down
" Something I really liked in notepad2 for Windows
"
"
" (c) 2006 Joe Chrzanowski
" Sonicdev
" http://www.sonicdev.net
" joechrz@gmail.com
"
" --------------------------------------------------------------
" Note: I realize that if you hit f too much, then the line
" disappears.  I can't figure out how to fix this.  If you know
" how, please let me know.  Until then, just hit u if you go too
" far.
" --------------------------------------------------------------
map e :call MoveLine(line("."),-1)<cr>k
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
