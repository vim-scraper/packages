" Vim syntax file
" Language:    Energy Plus IDF file
" Maintainer:  Dr. Christian Schiefer christian.schiefer@anlagenbau-austria.at
" Last Change: 06.05.2004 23:29:50
" Version:     0.1
" URL:         //http://www.anlagenbau-austria.at/
" 
" Simple syntax file for Energy Plus IDF files.
" http://www.eere.energy.gov/buildings/energyplus/

syntax case ignore

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syntax clear
syntax match idfComment /!.*$/ 
syntax match idfComment /!.*$/ contained 
syntax match idfComma /,/ contained 
syntax match idfSemiColon /;/ contained 

syntax region idfSection matchgroup=Typedef start="[a-zA-Z][^,]*" end=";" skip="!-.*$" keepend contains=idfComment,idfComma,idfSemiColon,idfInt,idfText
syntax match idfText /[a-zA-Z][^,;]*/ contained

syn sync lines=250

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_idf_syntax_inits")
	if version < 508
		let did_idf_syntax_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif
	HiLink idfComma				Operator
	HiLink idfSemiColon			Typedef
	HiLink idfComment			Comment
	HiLink idfSection			String
	HiLink idfText				Text

	delcommand HiLink
endif

let b:current_syntax = "idf"

" vim:ts=4
