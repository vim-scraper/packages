" Vim syntax file
" Language:	CVOPS state automation language, CVOPS v. 7.2
" Author:	Konstantin Shemyak <konstantin.shemyak@nokia.com>
" Last Change:

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" EFSA state: first on the line, either a single word, or several 
" comma-separated words surrounded with brackets

syn region	efsaState	start="^\s*(" end=")"
syn match	efsaState	"^\s*\w\+\s" contains=Comment

"syn match	efsaInput

"syn match	efsaAction

syn region	efsaStatement	start="{" end="}" contains=efsaStatement,cvopsStatement,Number,InterfaceName,Comment

" Comments
syn region      Comment start="(\*" end="\*)"


" CVOPS keywords. Case-insensitive.
syn case ignore
syn keyword	cvopsStatement	encode decode to start stop combine delete delay seed rand gamma copy split all
syn keyword	efsaState	GLOBAL ANYSTATE OTHERSTATE
syn keyword	efsaInput	timeout OTHER ANYINPUT OTHERINPUT

syn keyword	interfaceName   PEER UP DOWN MGMT RES20
syn match       interfaceName	"RES[1-9]\|RES1[0-9]"

syn case match

" Inheritance from C
syn keyword	cvopsStatement void if else while for

" Integer numbers
syn match	Number	display "\d\+\>"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_c_syn_inits")
  if version < 508
    let did_c_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink cvopsStatement		Statement
  HiLink cvopsComment		Comment
  HiLink cvopsReserved		Operator
  HiLink efsaState		Identifier
  HiLink efsaInput              Type
  HiLink cvopsTimer		Identifier
  HiLink cvopsTimeout		Identifier
  HiLink interfaceName		Type

  delcommand HiLink
endif

let b:current_syntax = "cvops-aut"

" vim: ts=8
