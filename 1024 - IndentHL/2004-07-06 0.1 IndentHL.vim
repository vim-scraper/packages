" Name: IndentHL.vim
"
" Vim Syntax Highlighting for indent whitespace
"
" Last change: 2004 July 15
" Version: 0.1
" Author: Vedran Sajko <vsajko@oglasnik.hr>
" Usage:
" Put script in  ~/.vim/after/syntax/
" and rename it to filename of the syntax
" eg 
" ~/.vim/after/syntax/c.vim
" and
" ~/.vim/after/syntax/php.vim
" and
" ~/.vim/after/syntax/vim.vim
" and
" ~/.vim/after/syntax/html.vim
"
" I haven't test it with other syntaxes but hopefully works
"
" Tabs and spaces are colored in slightly different colors from lightest to
" darkest (more suitable for dark background)
" example (" x is darkest color and z lightest):
" 
"     function foo()
"     x if b
"     x y echo 'something'
"     x y while c
"     x y z echo 'something else'
"     x y endwhile
"     x endif
"     endfunction
"     
" Actually it form vertical stripes
" 
" You can switch it off and on by 
" 
"    :call OnOffIndentHi()
"
" Works for me, maybe somebody else will like it
" 
" This is my first plugin
" 
 




"Convert decimal to hexadecimal with help of Dec2hex16
function! s:Dec2hexcol(val)
	let x1=a:val % 16
	let x2=(a:val / 16) % 16
	
	return s:Dec2hex16(x2) . s:Dec2hex16(x1)
endfunction

"convert nombers 10 - 16 to alphabetic
function! s:Dec2hex16(val)
	if a:val==10
	return 'a'
	endif
	if a:val==11
	return 'b'
	endif
	if a:val==12
	return 'c'
	endif
	if a:val==13
	return 'd'
	endif
	if a:val==14
	return 'e'
	endif
	if a:val==15
	return 'f'
	endif
	return a:val
endfunction

"Defines a syntax for indent-spaces and indent-tabs separately
function! s:SynMatchTS()
	let s:n = 0
	let s:razmak = 0
	while s:n <= 12
		let s:razmak = s:n + 1
		exe 'syn match Tab'.s:n.'	"\%'.s:razmak.'c\t" containedin=phpRegion,Javascript,htmlHead'
		let s:razmak = &sw * s:n + 1
		exe 'syn match Space'.s:n.' "\%'.s:razmak.'c\ \{'.&sw.'}" containedin=phpRegion,Javascript,htmlHead'
		let s:n = s:n + 1
	endwhile
	return 1
endfunction

"Defines Highlighting Colors
function! s:HiDefST()
	let m=0
	let sp=0
	while m<=3
		
		let n=0
		
		let boja=synIDattr(synIDtrans(hlID("Normal")), "bg#")
		let bojaR='0x' . boja[1] . boja[2]
		let bojaG='0x' . boja[3] . boja[4]
		let bojaB='0x' . boja[5] . boja[6]
		let bojaR=bojaR + 51
		let bojaG=bojaG + 51
		let bojaB=bojaB + 51
		
		while n <= 2
			exe 'hi def Tab'. sp .' guibg=#'. (s:Dec2hexcol(bojaR)+s:tabRadd) . (s:Dec2hexcol(bojaG)+s:tabGadd) . (s:Dec2hexcol(bojaB)+s:tabBadd)
			exe 'hi def Space'. sp .' guibg=#'. (s:Dec2hexcol(bojaR)+s:spRadd) . (s:Dec2hexcol(bojaG)+s:spGadd) . (s:Dec2hexcol(bojaB)+s:spBadd)
			let bojaR = bojaR - 17
			let bojaG = bojaG - 17
			let bojaB = bojaB - 17
			let n = n + 1
			let sp = sp + 1
		endwhile 
		let m = m + 1
	endwhile
endfunction

"for switching indent highlighting off and on
function! OnOffIndentHi()
	if !s:SynMatchTSdefined
		call s:HiDefST()
		let s:SynMatchTSdefined = s:SynMatchTS()
	else
		let n = 0
		while n <= 12
			exe 'syntax clear Tab'.n
			exe 'highlight clear Tab'.n
			exe 'syntax clear Space'.n
			exe 'highlight clear Space'.n
			let n = n + 1
		endwhile
		let s:SynMatchTSdefined = 0
	endif
endfunction

" Colors can be changed by changing these values
" This is values whitch is added to color
let s:tabRadd = 0
let s:tabGadd = 0
let s:tabBadd = 20
let s:spRadd = 20
let s:spGadd = 0
let s:spBadd = 0

let s:SynMatchTSdefined = s:SynMatchTS()
call s:HiDefST()
