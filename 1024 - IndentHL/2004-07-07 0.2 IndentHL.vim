" Name: IndentHL.vim
"
" Vim Syntax Highlighting for indent whitespace
"
" Last change: 2004 July 7 (in 0.1 version date was wrong)
" Version: 0.2
" Author: Vedran Sajko <vsajko@oglasnik.hr>
" Install:
" Put script in  ~/.vim/after/syntax/php.vim
"                or 
"                ~/.vim/after/syntax/c.vim
"                and
"                ~/.vim/after/syntax/php.vim
"                and
"                ~/.vim/after/syntax/vim.vim
"                and
"                ~/.vim/after/syntax/html.vim
"
" I haven't test it with other syntaxes but hopefully works also
"
" Tabs and spaces are colored in slightly different colors from lightest to
" darkest for dark background or from darkest to lightest for light background
" example (x is darkest color and z lightest):
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
" Actually script forms vertical stripes
" 
" Usage: 
" You can switch it off and on by 
"
"    :call OnOffIndentHi()
"
" Works for me, maybe somebody else will like it
" 
" History:
"  0.2  - support for light background,
"         corrected some bugs
"         and some checking added
"  0.1  - initial version
"  
" 
" Colors can be changed by changing these values
" This is values whitch is added to color 
" (basic darkest color for dark background is 'Normal bg')
let s:tabRadd = 0x00 "(red for tab)
let s:tabGadd = 0x00 "(green for tab)
let s:tabBadd = 0x11 "(blue for tab)
let s:spRadd = 0x00  "(red for space)
let s:spGadd = 0x11  "(green for space)
let s:spBadd = 0x00  "(blue for space)

function! s:ChekingAdd(val)
	if a:val < 0 && &background == 'dark' 
		let retv = -a:val
	elseif a:val > 0 && &background == 'light' 
		let retv = -a:val
	else
		let retv = a:val
	endif

	if retv > 0 && retv > 0xDD
		return 0xDD
	endif
	if retv < 0 && retv < -0xdd
		return -0xdd
	endif
		return retv
endfunction




"Convert decimal to hexadecimal with help of Dec2hex16
function! s:Dec2hexcol(val)
	let x1=a:val % 16
	let x2=(a:val / 16) % 16
	
	return s:Dec2hex16(x2) . s:Dec2hex16(x1)
endfunction

"convert numbers 10 - 16 to alphabetic
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
	let m = 0
	let sp = 0
	while m <= 3
	
		let n=0
		" gets current normal background color for starting point
		let boja = synIDattr(synIDtrans(hlID("Normal")), "bg#", 'GUI')
		if strlen (boja) < 7 && &background == 'light'
			let boja = '#ffffff'
		elseif strlen (boja) < 7 && &background == 'dark'
			let boja = '#000000'
		endif
		
		let tabRaddc = s:ChekingAdd(s:tabRadd)
		let tabGaddc = s:ChekingAdd(s:tabGadd)
		let tabBaddc = s:ChekingAdd(s:tabBadd)
		let spRaddc = s:ChekingAdd(s:spRadd)
		let spGaddc = s:ChekingAdd(s:spGadd)
		let spBaddc = s:ChekingAdd(s:spBadd)
		
		let bojaR='0x' . boja[1] . boja[2]
		let bojaG='0x' . boja[3] . boja[4]
		let bojaB='0x' . boja[5] . boja[6]

		if &background == 'dark'
			let bojaR = bojaR + 0x22
			let bojaG = bojaG + 0x22
			let bojaB = bojaB + 0x22
		endif

		if &background == 'light'
			let bojaR = bojaR - 0x22
			let bojaG = bojaG - 0x22
			let bojaB = bojaB - 0x22
		endif
		
		while n <= 2
			exe 'hi def Tab'. sp .' guibg=#'. s:Dec2hexcol(bojaR+tabRaddc) . s:Dec2hexcol(bojaG+tabGaddc) . s:Dec2hexcol(bojaB+tabBaddc)
			exe 'hi def Space'. sp .' guibg=#'. s:Dec2hexcol(bojaR+spRaddc) . s:Dec2hexcol(bojaG+spGaddc) . s:Dec2hexcol(bojaB+spBaddc)

			if &background == 'dark'
				let bojaR = bojaR - 0x11
				let bojaG = bojaG - 0x11
				let bojaB = bojaB - 0x11
			endif

			if &background == 'light'
				let bojaR = bojaR + 0x11
				let bojaG = bojaG + 0x11
				let bojaB = bojaB + 0x11
			endif
			
			let n = n + 1
			let sp = sp + 1
		endwhile 
		let m = m + 1
	endwhile
endfunction

"for switching indent highlighting off and on
function! OnOffIndentHi()
	if strlen(synIDattr(synIDtrans(hlID("Tab0")), "bg#", 'GUI')) < 7
		call s:SynMatchTS()
		call s:HiDefST()
	else
		let n = 0
		while n <= 12
			exe 'syntax clear Tab'.n
			exe 'highlight clear Tab'.n
			exe 'syntax clear Space'.n
			exe 'highlight clear Space'.n
			let n = n + 1
		endwhile
	endif
endfunction




call OnOffIndentHi()
