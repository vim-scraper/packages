" Vim syntax file
" Language:	RELAX NG Compact Syntax
" Maintainer:	Hans Fugal <hans@fugal.net>
" Last Change:	16 Aug 2002
" Remark:		

if version < 600
    syntax clear
elseif exists ("b:current_syntax")
    finish
endif

" Comments
syn match Comment /^\s*#[^#].*$/
syn match Documentation /^\s*##.*$/

" Literals
syn region literalSegment start=/"/ end=/"/ 
syn match patternSpecial /[,&|?*+\\]/

" Keywords
syn keyword patternKeyword  element attribute list mixed parent empty text notAllowed externalRef grammar
syn keyword grammarContentKeyword  div include
syn keyword startKeyword  start
syn keyword datatypeNameKeyword  string token
syn keyword namespaceUriKeyword  inherit
syn keyword inheritKeyword  inherit
syn keyword declKeyword  namespace default datatypes

" Links
hi link patternKeyword keyword
hi link grammarContentKeyword keyword
hi link startKeyword keyword
hi link datatypeNameKeyword keyword
hi link namespaceUriKeyword keyword
hi link inheritKeyword keyword
hi link declKeyword keyword

hi link literalSegment String
hi link Documentation Comment

hi link patternSpecial Special

let b:current_syntax = "rnc"
" vim: ts=8 sw=4 smarttab
