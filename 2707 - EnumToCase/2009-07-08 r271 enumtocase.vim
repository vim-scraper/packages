" enumtocase
"   Author: A. S. Budden
"   Date:   8th July 2009
"   Version: r271

if &cp || exists("g:loaded_enumtocase")
	finish
endif
let g:loaded_enumtocase = 1

command! -range EnumToCase <line1>,<line2>call EnumToCase()


function! EnumToCase() range
	let EnumLines = getline(a:firstline, a:lastline)
	let OriginalLineCount = len(EnumLines)
	for index in range(OriginalLineCount)
		let EnumLines[index] = substitute(EnumLines[index], '\s\+\(\k\+\)\%(\s*=\*\d\+\)\?\%(\s*,\)\?$', 'case \1:', '')
		call setline(a:firstline + index, EnumLines[index])
	endfor
	let ExtraLineCount = 0
	for index in range(a:lastline, a:firstline, -1)
		if getline(index) =~ 'case \k\+:'
			exe index . 's/$/\rbreak;\r/'
			let ExtraLineCount += 2
		endif
	endfor

	let TotalLines = a:lastline - a:firstline + ExtraLineCount + 1
	for lineNumber in range(a:firstline, a:lastline+ExtraLineCount)
		exe a:firstline
		exe 'normal ' . TotalLines . '=='
	endfor

endfunction
