syn clear

"TODO: normal stuff like numbers, strings, URLs, email format etc.

" second match overrides hexnumber, word boundarys not detected
" numbers in headlines are also colored, overrides headline color
"syn match cHexNumber "0x[0-9a-fA-F]\+" 
"syn match cNumber "[0-9]\+" 

"syn match	cSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn region	cString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial
" don't match the apastrophe "'" in "don't" !
syn region	cSString	start=+\W'+ skip=+\\\\\|\\'+ end=+'+ contains=cSpecial

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	cNumbers	display transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctalError,cOctal
" Same, but without octal error (for comments)
syn match	cNumbersCom	display contained transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctal
syn match	cNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match	cNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
"syn match	cOctal		display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=cOctalZero
syn match	cOctalZero	display contained "\<0"
syn match	cFloat		display contained "\d\+f"
"floating point number, with dot, optional exponent
"syn match	cFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
syn match	cFloat		display contained "\d\+\.\d\+\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	cFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	cFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"
" flag an octal number with wrong digits
"syn match	cOctalError	display contained "0\o*[89]\d*"
syn case match


" match DOS paths
syn match cDOSPath		"[[:space:]=(/:][a-zA-Z]:[\.a-zA-Z0-9_\-\\\/]\+"lc=1

" match some date formats
" to keep it simple and match inedependant of language (format and chars)
" would also match '2 or 30' as a date
"syn match cDate "[^0-9a-zA-Z]\d\d\?[.]\?[ ./][äa-zA-Z0-9]\+[.]\?[ ./]\d\d\d\{0,2\}"
syn match cDate "\d\d\?[.]\?[ ./][äa-zA-Z0-9]\+[.]\?[ ./]\d\d\d\{0,2\}"
" with '<day>,' before
syn match cDate "\w\w\+, \d\d\?[.]\?[ ./][äa-zA-Z0-9]\+[.]\?[ ./]\d\d\d\{0,2\}"
syn match cTime "[0-5]\?\d:[0-5]\d"
syn match cTime "[0-2]\?\d:[0-5]\d:[0-5]\d"

" match example text to be substituted or special keys ike <Del>
syn region cExText		start="<" end=">" contains=cSpecial

" single enumeration 1) ... b)
syn match cEnum_A 	"^\s*[a-zA-Z0-9]\+[\.)]\s\+.*$"  

" match headlines: any text folleowed by line with '=' or '-'
syn match cHeadline_A "^.*\n===*$"
syn match cHeadline_B "^.*\n---*$" 

" underlined headline, might be used for header/footer separation ?
syn match cUnderlined "^.*\n__*$"

" single line list item starting with '-'
syn match cListItem_A1 "^\s*-\s\+.*$"
" multi line list item starting with '-'
syn match cListItem_A2 "^\s*-\s\+.*\n\s\+\S\+$" 
" single line list item starting with '*'
syn match cListItem_B1 "^\s*\*\s\+.*$"
" multi line list item starting with '*'
syn match cListItem_B2 "^\s*\*\s\+.*\n\s\+\S\+$"

" email address etc.
" TODO: include other allowed chars like -?=
syn match cEmail	"\w\+\.\?\w\+@\w\+\.\w\+"
syn match cIP		"\d\{1,3\}\.\d\{1,3\}\.\d\{1,3\}\.\d\{1,3\}"
syn match cWWW		"[wW][wW][wW]\.\w\+\.\w\+"
syn match cWWW		"http:\/\/[wW][wW][wW]\.\w\+\.\w\+"
syn match cWWW		"ftp:\/\/\w\+\.\w\+\.\w\+"

hi  cHeadline_A		gui=bold	guifg=Blue
hi  cHeadline_B		gui=italic	guifg=Blue
hi  cUnderlined		gui=bold 	guifg=DarkBlue
hi  cListItem_A1	gui=bold
hi  cListItem_A2	gui=bold
hi  cListItem_B1	gui=italic
hi  cListItem_B2	gui=italic
hi  cEnum_A 		gui=bold
hi  cDosPath		gui=bold

hi link cNumber			Number
hi link cOctal			Number
hi link cFloat			Number
hi link cCppString		String
hi link cString			String
hi link cSString		String
"hi link cDosPath		cUnderlined
hi link cExText			String
hi link cDate			cHeadline_B
hi link cTime			cDate
hi link cEmail			cDosPath
hi link cWWW			cDosPath
hi link cIP				cDosPath

" to syncronize highlighting when scrolling
syntax sync minlines=100
