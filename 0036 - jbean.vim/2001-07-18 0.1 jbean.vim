" File: jbean.vim
" Summary: Functions for making get/set method of bean's property
" Author: Kim sung hyun<blartum@hotmail.com>
" Assistant : Kalle Bj?klid <bjorklid@st.jyu.fi>
" Last Modified: 7.19.2001
" Version: 0.0

" To get last word from some string

function! GetLastWord(str)

	let line = a:str

	echo 'Input : ' . line . ' and length : ' . strlen(line)
	
	let lastWordPattern = '\S\+$'
	let lastWordStartPosition = match(line, lastWordPattern)
	
	echo 'value : ' . lastWordStartPosition
	
	let lastWordLength = strlen(line) - lastWordStartPosition

	if lastWordStartPosition == -1
		echo 'nothing found from the line'
	else	
		let lastWord = strpart(line, lastWordStartPosition, lastWordLength)
		
	endif
	
	return lastWord
	
endfunction

" To get insert position of method

function! GetInsertPosition(line)
	
	let currentLine = a:line + 1
	let str = getline(currentLine)
	
	let chk = 0
	
	while 1	
		
		if str =~ '{'
		
			let currentLine = currentLine - 2
			break
			
		elseif str =~ '}'
		
			let currentLine = currentLine - 1
			break
			
		endif
		
		let currentLine = currentLine + 1		
				
		let str = getline(currentLine)
		
	endwhile
			
	return currentLine
	
endfunction
	
function! GetIndent(str)

	let indentEndPos = matchend(a:str, '^\s*')
	return strpart(a:str, 0 , indentEndPos)
	
endfunction

" Starting point

function! MakeBeanFramework() range

	" What accessibility
	
	let ACCESS = 'public'
	let VOID = 'void'
	let TAB = "\t"
	
	" To get type and idetifier
	
	let line = getline(a:firstline)
	
	echo 'First Input : ' . line . ' and length : ' . strlen(line)
	
	let temp = GetLastWord(line)
	let identifier = strpart(temp, 0, strlen(temp) - 1)	
	
	let temp = strpart(line, 0, strlen(line) - ( strlen(temp) + 1))
	
	let type = GetLastWord(temp)
	
	echo 'Identifier : ' . identifier
	echo 'Type : ' . type
	
	" To get insert position
	
	let insertLine = GetInsertPosition(a:firstline)
	
	echo 'Line : ' . insertLine
	
	" To insert Get method
	
	let strIndent = GetIndent(line)
	
	call append(insertLine, strIndent . ACCESS . ' ' . type . ' get' . substitute(identifier, "[a-z]", "\\U\\0", "") . '()')
	let insertLine = insertLine + 1
	call append(insertLine, strIndent . '{')
	let insertLine = insertLine + 1
	call append(insertLine, strIndent . TAB . 'return this.' . identifier . ';')
	let insertLine = insertLine + 1
	call append(insertLine, strIndent . '}')
	let insertLine = insertLine + 1
	
	" To insert Set method	

	call append(insertLine, " ")
	call append(insertLine, strIndent . ACCESS . ' ' . VOID . ' set' . substitute(identifier, "[a-z]", "\\U\\0", "") . '('. type . ' ' . identifier . ')')
	let insertLine = insertLine + 1
	call append(insertLine, strIndent . '{')
	let insertLine = insertLine + 1
	call append(insertLine, strIndent . TAB . 'this.' . identifier . ' = ' . identifier . ';')
	let insertLine = insertLine + 1
	call append(insertLine, strIndent . '}')
	let insertLine = insertLine + 1
	
endfunction
