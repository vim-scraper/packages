" Vim global plugin for converting between ISBN-10 and 978 prefix EAN-13
" Last Change:	2010 February 15
" Maintainer:	Brandon Waskiewicz <brandon.waskiewicz@gmail.com>
" Licence:      This program is free software; you can redistribute it
"               and/or modify it under the terms of the GNU General Public
"               License.  See http://www.gnu.org/copyleft/gpl.txt

function! ReplaceCurrentLineIsbn10With978Ean13()
	let currline = getline(".")
	if IsValidIsbn10(currline)
		let ean13 = ConvertIsbn10To978Ean13(currline)
		call setline(".", ean13)
	else
		echo "Invalid ISBN-10"
	endif	
endfunction

function! ReplaceCurrentLine978Ean13WithIsbn10()
	let currline = getline(".")
	if (IsValidEan13(currline) && strpart(currline, 0, 3) == "978")
		let isbn10 = Convert978Ean13ToIsbn10(currline)
		call setline(".", isbn10)
	else
		echo "Invalid 978 EAN-13"
	endif	
endfunction

function! ReplaceSelectedTextIsbn10With978Ean13() range
	let startline = line("'<")
	let startcol = col("'<") - 1
	let endline = line("'>")
	let endcol = col("'>")
	if startline != endline
		echo "Cannot replace selected text across multiple lines"
		return
	endif
	let entireline = getline(startline)
	let isbnpart = strpart(entireline, startcol, endcol - startcol)
	if IsValidIsbn10(isbnpart)
		let firstpart = strpart(entireline, 0, startcol)
		let secondpart = strpart(entireline, endcol)
		let ean13 = ConvertIsbn10To978Ean13(isbnpart)
		call setline(startline, firstpart . ean13 . secondpart)
		call cursor(startline, startcol + 14)
	else
		echo "Invalid ISBN-10"
	endif
endfunction

function! ReplaceSelectedText978Ean13WithIsbn10() range
	let startline = line("'<")
	let startcol = col("'<") - 1
	let endline = line("'>")
	let endcol = col("'>")
	if startline != endline
		echo "Cannot replace selected text across multiple lines"
		return
	endif
	let entireline = getline(startline)
	let eanpart = strpart(entireline, startcol, endcol - startcol)
	if IsValidEan13(eanpart) && strpart(eanpart, 0, 3) == "978"
		let firstpart = strpart(entireline, 0, startcol)
		let secondpart = strpart(entireline, endcol)
		let isbn10 = Convert978Ean13ToIsbn10(eanpart)
		call setline(startline, firstpart . isbn10 . secondpart)
		call cursor(startline, startcol + 11)
	else
		echo "Invalid 978 EAN-13"
	endif
endfunction

function! ReplaceAllValidIsbn10InBufferWith978Ean13()
	call cursor(1, 1)
	while search('\d\{9}\(\d\|X\|x\)', "cW") > 0
		let matchcol = get(getpos("."), 2) - 1
		let matchline = get(getpos("."), 1)
		let entireline = getline(".")
		let isbnpart = strpart(entireline, matchcol, 10)
		if IsValidIsbn10(isbnpart)
			let firstpart = strpart(entireline, 0, matchcol)
			let secondpart = strpart(entireline, matchcol + 10)
			let ean13 = ConvertIsbn10To978Ean13(isbnpart)
			call setline(".", firstpart . ean13 . secondpart)
			call cursor(matchline, matchcol + 13)
		endif
	endwhile
endfunction

function! ReplaceAllValid978Ean13InBufferWithIsbn10()
	call cursor(1, 1)
	while search('\d\{13}', "cW") > 0
		let matchcol = get(getpos("."), 2) - 1
		let matchline = get(getpos("."), 1)
		let entireline = getline(".")
		let eanpart = strpart(entireline, matchcol, 13)
		if IsValidEan13(eanpart) && strpart(eanpart, 0, 3) == "978"
			let firstpart = strpart(entireline, 0, matchcol)
			let secondpart = strpart(entireline, matchcol + 13)
			let isbn10 = Convert978Ean13ToIsbn10(eanpart)
			call setline(".", firstpart . isbn10 . secondpart)
			call cursor(matchline, matchcol + 10)
		endif
	endwhile
endfunction

function! IsValidIsbn10(isbn10)
	let checkdigit = CalculateIsbn10Check(strpart(a:isbn10, 0, 9))
	let testdigit = strpart(a:isbn10, 9, 1)
	return (checkdigit == testdigit)
endfunction

function! IsValidEan13(ean13)
	let checkdigit = CalculateEan13Check(strpart(a:ean13, 0, 12))
	let testdigit = strpart(a:ean13, 12, 1)
	return (checkdigit == testdigit)
endfunction

function! CalculateEan13Check(ean13)
	" Takes in a string of 12 characters (the entire EAN-13
	" excluding the check digit).
	let checksum = 0
	for i in range(12)
		let chnum = str2nr(strpart(a:ean13, i, 1))
		if(i % 2 == 0)
			let checksum = checksum + chnum
		else
			let checksum = checksum + (chnum * 3)
		end
	endfor

	let checksum = checksum % 10
	let checksum = 10 - checksum
	let checkdigit = checksum == 10 ? "0" : checksum
	return checkdigit
endfunction

function! CalculateIsbn10Check(isbn10)
	" Takes in a string of 9 characters (the entire ISBN-10
	" excluding the check digit).
	let checksum = 0
	for i in range(9)
		let chnum = str2nr(strpart(a:isbn10, i, 1))
		let checksum = checksum + (chnum * (i + 1))
	endfor
	let checksum = checksum % 11
	let checkdigit = checksum == 10 ? "X" : checksum
	return checkdigit
endfunction

function! ConvertIsbn10To978Ean13(isbn10)
	if strlen(a:isbn10) != 10
		echo "Requires a string of 10 characters"
		return
	endif

	let firstpiece = "978" . strpart(a:isbn10, 0, 9)
	let checksum = CalculateEan13Check(firstpiece)
	return firstpiece . checksum
endfunction

function! Convert978Ean13ToIsbn10(ean13)
	if strlen(a:ean13) != 13
		echo "Requires a string of 13 characters"
		return
	endif
	if strpart(a:ean13, 0, 3) != "978"
		echo "Requires a 978 EAN-13"
		return
	endif

	let firstpiece = strpart(a:ean13, 3, 9)
	let checksum = CalculateIsbn10Check(firstpiece)
	return firstpiece . checksum
endfunction

:command! ReplaceLineIsbnEan call ReplaceCurrentLineIsbn10With978Ean13()
:command! ReplaceLineEanIsbn call ReplaceCurrentLine978Ean13WithIsbn10()
:command! -range ReplaceSelectedIsbnEan call ReplaceSelectedTextIsbn10With978Ean13()
:command! -range ReplaceSelectedEanIsbn call ReplaceSelectedText978Ean13WithIsbn10()
:command! ReplaceAllIsbnEan call ReplaceAllValidIsbn10InBufferWith978Ean13()
:command! ReplaceAllEanIsbn call ReplaceAllValid978Ean13InBufferWithIsbn10()
