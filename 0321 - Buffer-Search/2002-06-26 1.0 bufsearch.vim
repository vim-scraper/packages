" Name Of File: bufsearch.vim
"  Description: Vim Plugin to limit search to that buffer.
"   Maintainer: Naveen Chandra R (ncr at iitbombay dot org)
"  Last Change: Tuesday, June 25, 2002
"        Usage: Normally, this file should reside in the plugins
"               directory and be automatically sourced. If not, you must
"               manually source this file using ':source bufsearch.vim'.
"
"				Type :Bs <search-pattern>   - This will search forward
"											  within that buffer.
"		        n 		- Repeats the latest :Bs
"				N       - Repeats the latest :Bs in opposite direction
"
" Side-Effects: Unsets hlsearch option

function! BufSearch(pat)
	call setbufvar(bufnr("%"), "bufsearch", a:pat)
	let @/ = a:pat
	let &hls = 0
	call BufMatch(a:pat)
endfunction

function! BufMatch(pat)
	if &ignorecase == 1
		execute ":match Search /\\c". a:pat . "/"
	else
		execute ":match Search /".a:pat."/"
	endif
endfunction

function! SetBufPattern(pat)
	if a:pat != ""
		let @/ = a:pat
		call BufMatch(a:pat)
	else
		let @/ = ""
		execute ":match none"
	endif
endfunction

augroup BufSearch 
	au BufEnter * call SetBufPattern(getbufvar(bufnr("%"),"bufsearch"))
	"au BufLeave * 
	"	\ :match none
augroup END

command! -nargs=+ -complete=command Bs call BufSearch(<q-args>)
