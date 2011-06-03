" Vim script -- cope with Roman numbers
" File:		incroman.vim
" Version:	2
" Vim Version:	Vim 6.0
" Last Change:	2007 Jan 26
" Author:	Andy Wokula <anwoku@yahoo.de>
"
" Description:
" Increase or decrease a Roman number found in the text.  Convert a decimal
" number or any word into a Roman number.
" 
" Installation:
" Three options
" (1) Activate for current buffer:
"	:source incroman.vim
"
" (2) Source globally from .vimrc .  You should then also map Ctrl-A and
"     Ctrl-X globally:
"	:noremap <c-a> <plug>IncRoman
"	:noremap <c-x> <plug>DecRoman
"	:source incroman.vim
"
" (3) Source from an ftplugin.
"     You can map other keys than Ctrl-A/Ctrl-X, for example:
"	:noremap <buffer> + <plug>IncRoman
"	:noremap <buffer> - <plug>DecRoman
"	:source incroman.vim
"	
" Usage:
" Use Ctrl-A/Ctrl-X to increase/decrease a Roman number, but there is more
" about it.  If the word found under the cursor
" ... is a decimal number, it is converted into a Roman number and not
"     increased or decreased, a count is ignored.
" ... is a Roman number, then it is increased or decreased using count;
"     Roman numbers with slightly wrong format (e.g. IIII or IM) are
"     accepted.
" ... is some other string, it is replaced by the Roman number whose value
"     is count, "I" if count is missing.
"
" To show the value of a Roman number at the cursor:
"   :echo Roman2Decimal(expand("<cword>"))
"
" Global
"   Maps: <plug>IncRoman, <plug>DecRoman
"   Functions: Roman2Decimal(), Decimal2Roman()

" Buffer Local Stuff: load only once per buffer
if exists("b:loaded_incroman")
    finish
endif
let b:loaded_incroman = 1

" Bufferlocal Mappings:
if !hasmapto('<plug>IncRoman')
    nmap <unique><buffer> <c-a> <plug>IncRoman
endif
if !hasmapto('<plug>DecRoman')
    nmap <unique><buffer> <c-x> <plug>DecRoman
endif


" Global Stuff: load only once per session
if exists("loaded_incroman")
    finish
endif
let loaded_incroman = 1

noremap <unique><script> <plug>IncRoman <sid>inc
noremap <unique><script> <plug>DecRoman <sid>dec
noremap <silent> <sid>inc :<c-u>call<sid>Gc()<cr>ciw<c-r>=<sid>IncRoman(@-)<cr><esc>
noremap <silent> <sid>dec :<c-u>call<sid>Gc()<cr>ciw<c-r>=<sid>DecRoman(@-)<cr><esc>

let s:cpo_save = &cpo
set cpo&vim

" roman values, curly-braces-names used
let s:rvI = 1
let s:rvV = 5
let s:rvX = 10
let s:rvL = 50
let s:rvC = 100
let s:rvD = 500
let s:rvM = 1000

" Global Functions: can be used everywhere

" returns the decimal value of the Roman number <str> 
function Roman2Decimal(str)
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
endfunction

" returns the Roman number string of the decimal value <n>
function Decimal2Roman(n)
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
	let of = rz =~ '[DLV]' ? 1 : 2
	if a<6 && (n >= rzval - s:rv{rs[a+of]})
	    let s = s . rs[a+of] . rz
	    let n = n - (rzval - s:rv{rs[a+of]})
	endif
	let a = a+1
    endw
    return s
endfunction

" Script Local Functions:

" Get the count (use function to avoid global variable)
function <sid>Gc()
    let s:cnt = v:count1
endfunction

" adds decimal s:cnt to roman a:str and returns the roman result
function <sid>IncRoman(str)
    if a:str =~ '^\d\+$'
	" turn decimal argument into Roman number
	return Decimal2Roman(a:str)
    else
	return Decimal2Roman(Roman2Decimal(a:str)+s:cnt)
    endif
endfunction

" subtracts decimal s:cnt from roman a:str and returns the roman result
function <sid>DecRoman(str)
    if a:str =~ '^\d\+$'
	" turn decimal argument into Roman number
	return Decimal2Roman(a:str)
    else
	return Decimal2Roman(Roman2Decimal(a:str)-s:cnt)
    endif
endfunction


" Restore the previous value of 'cpoptions'.
let &cpo = s:cpo_save
unlet s:cpo_save
