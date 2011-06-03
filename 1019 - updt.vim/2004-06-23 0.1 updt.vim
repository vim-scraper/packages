" Vim syntax file
" Language:	ClearCase update log file
" Maintainer:	Wenzhi Liang <wzhliang_at_speedymail.org>
" Last Change:	23 June 2004

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match ccupdateComment		"^#.*$"

syn keyword ccupdateType		TRUE FALSE

"syn match   ccupdateKey		contained "^\S\+:\s"
syn match   ccupdateKey			"^\S\+:"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_ccupdate_syntax_inits")
  if version < 508
    let did_ccupdate_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink ccupdateKey			String
  HiLink ccupdateComment		Comment
  HiLink ccupdateType			Function

  delcommand HiLink
endif

let b:current_syntax = "ccupdate"

"EOF	vim: ts=8 noet tw=100 sw=8 sts=0
