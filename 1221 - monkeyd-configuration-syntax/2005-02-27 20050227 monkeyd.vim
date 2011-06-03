" Vim syntax file
" Language:	Monkeyd Configuration Files
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Version:	20050227
" Copyright:	Copyright (c) 2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself

if exists("b:current_syntax")
  finish
endif

syn region MonkeydComment start=/#/ end=/$/

syn cluster MonkeydValue contains=MonkeydText,MonkeydNumber,MonkeydIP

syn match  MonkeydKeyword /\S\@<![a-zA-Z][a-zA-Z0-9\-_]*\S\@!/ 
             \ nextgroup=@MonkeydValue skipwhite

syn match  MonkeydText contained /\S\+/
            \  nextgroup=@MonkeydValue skipwhite

syn match  MonkeydNumber contained /\S\@<!\d\+\S\@!/
            \ nextgroup=@MonkeydValue skipwhite

syn match  MonkeydIP contained /\S\@<!\%(\d\{1,3\}\.\)\{3\}\d\{1,3\}/
            \  nextgroup=@MonkeydValue skipwhite

syn match  MonkeydGroup /\S\@<!<[^>]\+>\S\@!/

hi def link MonkeydComment Comment
hi def link MonkeydKeyword Keyword
hi def link MonkeydNumber  Number
hi def link MonkeydIP      Special
hi def link MonkeydText    Constant
hi def link MonkeydGroup   Identifier

let b:current_syntax = "monkeyd"
