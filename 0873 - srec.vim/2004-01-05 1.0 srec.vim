" Vim syntax file
" Language:	Motorola S-Record
" Maintainer:	Peter Sommerfeld (psommerfeld@sbs.com)
" Last Change:	2003 Dec 23

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" storage types

syn match srecStart  contained "^S"
syn match srecType   "^S[0-9a-fA-F]" contains=srecStart
syn match srecLength "^S[0-9a-fA-F]\{3}" contains=srecType
syn match srec16BitAddress "^S[19][0-9a-fA-F]\{6}" contains=srecLength
syn match srec24BitAddress "^S[28][0-9a-fA-F]\{8}" contains=srecLength
syn match srec32BitAddress "^S[19][0-9a-fA-F]\{10}" contains=srecLength
syn match srecChecksum	"[0-9a-fA-F]\{2}$"

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_srec_syntax_inits")
  if version < 508
    let did_srec_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  HiLink srecStart		SpecialKey
  HiLink srecType   		WarningMsg
  HiLink srecLength       	Constant
  HiLink srec16BitAddress 	Comment
  HiLink srec24BitAddress 	Comment
  HiLink srec32BitAddress 	Comment
  HiLink srecChecksum		Search

  delcommand HiLink
endif

let b:current_syntax = "srec"

" vim: ts=8
