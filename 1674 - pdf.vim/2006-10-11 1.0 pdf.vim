" Vim syntax file
" Language:	PDF File Format
" Maintainer:	Mohit Kalra (mohit_kalraREMOVETHIS@hotmail.com)	
" Last Change:	2006 May 24

" Quit when a (custom) syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"PDF is case sensitive
syn case match 

"PDF Header and End Of File 
syn match       pdfHeader			"%PDF-\d.\d"
syn match       pdfEOF				"%%EOF"

"Basic Datatypes
syn keyword 	pdfBoolean 			true false
syn match		pdfInt 				"\d\+"
syn match	 	pdfReal 			"\d\+\.\d\+"
syn match 		pdfReal 			"\.\d+"
" A PDF String can have \n \r \t \b \f \( \) \\
" \ddd where ddd is an octal number
syn match       pdfFormat  			"\\[nrtbf()]\|\\\o\{1,3}"
" A PDF String can have nested braces without any problems 
" Eg.  ( This is a (valid) \)string \n )
syn region      pdfString 		   	start="(" skip="\\)" end=")" contains=pdfFormat,pdfString
" A PDF String can also consist of hex numbers
syn match       pdfString      		"<\x\+>"
"Identifiers or Name Objects
syn match       pdfIdentifier  		"\/[^()%{}><\[\]\/ 	]\+"

"Arrays are of mixed type and can be nested
syn region      pdfArray   			matchgroup=pdfParen     start="\[" end="\]" contains=pdfInt, pdfReal, pdfString, pdfIdentifier, pdfBoolean, pdfArray, pdfIndirect
"Dictionaries
syn region      pdfDictionary  		matchgroup=pdfParen     start="<<" end=">>" contains=pdfInt, pdfReal, pdfString, pdfIdentifier, pdfBoolean, pdfArray, pdfDictionary, pdfIndirect
"XMP Data
"syn  include 	@Xml <sfile>:p:h/xml.vim
"syn region     	pdfXMPPacket   		start="<?xpacket begin=\"﻿\""  end="<?xpacket end=\".\{-}?>" contains=@Xml contained  keepend
"Streams
"syn region       pdfBinStream  		start="\d\+ \d\+ obj.\{-}\/Filter.\{-}stream" end="endstream.\{-}endobj"
if exists("pdf_streams")  "not accurate when set
	syn region   pdfStream  		matchgroup=pdfParen     start="stream" end="endstream" contains=pdfXMPPacket, pdfInt, pdfReal, pdfString, pdfIdentifier contained
else
	syn region   pdfStream  		matchgroup=pdfParen     start="stream" end="endstream" contains=pdfXMPPacket contained
endif 

"Objects along with the object IDs
syn  region 	pdfObject 			matchgroup=pdfParen2 	start="\d\+ \d\+ obj" end="endobj" contains=pdfStream,pdfDictionary,pdfArray
"Comments
syn region      pdfComment      	start="%" end="\|$" contains=pdfHeader
"Indirect object references
syn match       pdfIndirect     	"\d\+ \d\+ R"
"Cross reference table
syn match       pdfXRef         	"xref"
syn match       pdfXRefTable 		"\d\{10} \d\{5} f"
syn match       pdfXRefTable 		"\d\{10} \d\{5} n"

"Other Keywords
syn keyword     pdfKeyword      	trailer startxref


" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link  pdfHeader		Underlined 
hi def link  pdfEOF 		Underlined
hi def link  pdfBoolean     Boolean 
hi def link  pdfInt 		Number
hi def link  pdfReal 		Float
hi def link  pdfFormat      SpecialChar
hi def link  pdfString      String 
hi def link  pdfIdentifier  Identifier
hi def link  pdfStream      Debug
hi def link  pdfParen      	PreProc
hi def link  pdfParen2		Keyword
hi def link  pdfComment     Comment
hi def link  pdfIndirect    PreProc
hi def link  pdfXRef 		Keyword
hi def link  pdfXRefTable   Debug
hi def link  pdfKeyword 	Keyword
"hi def link  pdfXMPPacket   ToDo
hi def link  pdfBinStream   ToDo
let b:current_syntax = "pdf"

" vim: ts=4 sw=8 
