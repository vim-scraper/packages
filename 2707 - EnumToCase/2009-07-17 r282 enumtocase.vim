" enumtocase
"   Author: A. S. Budden
"   Date:   17th July 2009
"   Version: r282

if &cp || exists("g:loaded_enumtocase")
	finish
endif
let g:loaded_enumtocase = 1

command! -range EnumToCase <line1>,<line2>call EnumToCase()

if !exists('g:EnumToCaseKeepPrefixComments')
	let g:EnumToCaseKeepPrefixComments = 1
endif

if !exists('g:EnumToCaseKeepEndOfLineComments')
	let g:EnumToCaseKeepEndOfLineComments = 0
endif

if !exists('g:EnumToCaseKeepEndOfLine')
	let g:EnumToCaseKeepEndOfLine = 0
endif

let s:EnumRE  = '^'                   " Start of the line
let s:EnumRE .= '\s\+'                " Soak up one or more spaces
let s:EnumRE .= '\(\k\+\)'            " Grab the enumeration name (\1)
let s:EnumRE .= '\%('                 " Non-capturing group to get rid of set values
let s:EnumRE .=     '\s*=\s*\d\+'     " e.g. ' = 0' (this isn't needed)
let s:EnumRE .= '\)\?'                " End of the group, which is optional
let s:EnumRE .= '\%(\s*,\)\?'         " Another optional group to catch the comma
let s:EnumRE .= '\s*'                 " Any spaces that haven't been caught yet

if g:EnumToCaseKeepEndOfLine == 1
	let s:EnumRE .= '\('              " A group to catch anything on the end of the line
	let s:EnumRE .=    '.\{-}'        " Non-greedy match to skip spaces at the end
	let s:EnumRE .= '\)'              " End of the catchment group
elseif g:EnumToCaseKeepEndOfLineComments == 1
	let s:EnumRE .= '\('              " A group to catch an end-of-line comment
	let s:EnumRE .=    '//.*'         " A C++ style comment
	let s:EnumRE .= '\|'              " OR
	let s:EnumRE .=    '/\*.\{-}\*/'  " A C style comment
	let s:EnumRE .= '\)\?'            " End of the comment group
	let s:EnumRE .= '.\{-}'           " Soak up whatever is left
else
	let s:EnumRE .= '.*'              " Soak up everything unwanted
	let s:EnumRE .= '\(\s\{-}\)'      " False catcher to keep back references happy
endif

let s:EnumRE .= '\s*$'                " Any end-of-line spaces


function! EnumToCase() range
	let EnumLines = getline(a:firstline, a:lastline)
	let DelayedLines = []
	let OriginalLineCount = len(EnumLines)

	let LineNumber = a:firstline

	for index in range(OriginalLineCount)
		if EnumLines[index] =~ '[{}]'
			" If the line is contains a brace, just add
			" it without changes (used to simplify selection)
			if LineNumber <= a:lastline
				call setline(LineNumber, EnumLines[index])
			else
				call append(LineNumber-1, EnumLines[index])
			endif

			let LineNumber += 1

		elseif EnumLines[index] =~ '^\s*$'
			" Do nothing: ignore blank lines
			
		elseif EnumLines[index] =~ s:EnumRE
			" If the line matches the enumeration regular expression,
			" add a break to the end of the delayed lines, change the
			" statement to a case: statement and strip any trailing
			" spaces.
			call add(DelayedLines, 'break;')
			call add(DelayedLines, '')
			let line = 
						\ substitute(EnumLines[index],
						\ s:EnumRE,
						\ 'case \1: \2',
						\ '')
			let line = substitute(line, '\s\+$', '', '')

			" Add this line to the buffer
			if LineNumber <= a:lastline
				call setline(LineNumber, line)
			else
				call append(LineNumber-1, line)
			endif
			let LineNumber += 1

			" Add any delayed lines to the buffer
			for line in DelayedLines
				if LineNumber <= a:lastline
					call setline(LineNumber, line)
				else
					call append(LineNumber-1, line)
				endif

				let LineNumber += 1
			endfor
			let DelayedLines = []
		elseif g:EnumToCaseKeepPrefixComments == 1
			" If this is something else (e.g. a comment), just buffer
			" it for addition after the case statement
			call add(DelayedLines, EnumLines[index])
		else
			" Do nothing
		endif
	endfor

	" Calculate the new number of lines and auto-indent
	let TotalLines = LineNumber - a:firstline
	exe a:firstline
	exe 'normal ' . TotalLines . '=='

endfunction
