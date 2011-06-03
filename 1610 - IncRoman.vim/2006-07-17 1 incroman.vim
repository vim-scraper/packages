" Vim script file
" File:		incroman.vim
" Vim Version:	Vim 6.0
" Date:		09-07-2006, 17-07-2006
" Author:	Andy Wokula <anwoku@yahoo.de>
"
" Description:
" Remaps the normal mode keys Ctrl-A and Ctrl-X to increase/decrease a Roman
" number (I, II, III, IV, ...) found under the cursor, with optional count.
" The standard behaviour of these keys is not available then (see :help
" Ctrl-A).
"
" Usage:
" :source incroman.vim
"
" Some Notes: Errors are never given, but standard values (zero or one).
" Thus you may turn any word (including a blank line) under the cursor into
" a roman number.  Try Ctrl-A + Ctrl-X to correct a roman number having a
" wrong format, e.g. IIII -> IV or IM -> CMXCIX.  Case is always turned into
" upper case.


" script local variables:

" roman values, curly-braces-names used {{{
let s:rvI = 1
let s:rvV = 5
let s:rvX = 10
let s:rvL = 50
let s:rvC = 100
let s:rvD = 500
let s:rvM = 1000
"}}}


" global functions:

" returns the decimal value of the roman <str> 
fun! Roman2Decimal(str) "{{{
    let r = toupper(a:str)
    if r=='' || r =~ '[^IVXLCDM]'
	" return 0 if empty or number contains non-roman chars
	return 0
    endif
    let rval = 0
    let rlen = strlen(r)
    let i = 0
    while i < rlen-1
	if s:rv{r[i]} < s:rv{r[i+1]}
	    let rval = rval - s:rv{r[i]}
	else
	    let rval = rval + s:rv{r[i]}
	endif
	let i = i+1
    endw
    return rval + s:rv{r[i]}
endfun "}}}

" returns the roman str of the decimal value <n>
fun! Decimal2Roman(n) "{{{
    let s = ''
    " turn numbers smaller than 1 into 1
    let n = a:n<1 ? 1 : a:n
    let rs = 'MDCLXVI'
    let a = 0
    while n > 0
	let rz = rs[a]
	let rzval = s:rv{rz}
	while n >= rzval
	    let s = s . rz
	    let n = n - rzval
	endw
	if rz =~ '[DLV]'
	    let of = 1
	else
	    let of = 2
	endif
	if a<6 && (n >= rzval - s:rv{rs[a+of]})
	    let s = s . rs[a+of] . rz
	    let n = n - (rzval - s:rv{rs[a+of]})
	endif
	let a = a+1
    endw
    return s
endfun "}}}

" adds decimal <cnt> to roman <str> and returns the roman result
fun! IncRoman(str, cnt) "{{{
    return Decimal2Roman(Roman2Decimal(a:str)+a:cnt)
endfun "}}}

" subtracts decimal <cnt> from roman <str> and returns the roman result
fun! DecRoman(str, cnt) "{{{
    return Decimal2Roman(Roman2Decimal(a:str)-a:cnt)
endfun "}}}


" global mappings:

nmap <silent> <c-a> :<c-u>let c=v:count1<cr>ciw<c-r>=IncRoman(@-,c)<cr><esc>
nmap <silent> <c-x> :<c-u>let c=v:count1<cr>ciw<c-r>=DecRoman(@-,c)<cr><esc>

" vim: set fdm=marker:
