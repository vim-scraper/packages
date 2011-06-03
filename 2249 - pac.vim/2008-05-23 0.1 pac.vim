" Vim syntax file
" Language:    Proxy .pac file syntax
" Maintainer:  Justin Randall <jrrandall@gmail.com>
" Last Change: Fri May 23 10:00 PM 2008 EDT
" Copyright:   Copyright (c) 2008 Justin Randall
" Licence:     You may redistribute this under the same terms as Vim itself

" Remove any old syntax stuff hanging around
syn clear

syn keyword pacConnection  DIRECT PROXY SOCKS
syn match  pacNumber  "-\=\<\d\+L\=\>\|0[xX]\x\+\>"
syn match pacSpecial contained "\\\d\d\d\|\\[abcfnrtv\\]"
syn region pacString start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=pacSpecial,pacNumber,pacConnection
syn region pacComment start=/^\s*\/\// end=/$/ contains=pacString,pacSpecial,pacNumber

syn keyword pacStatement   return if else
syn keyword pacType        dnsDomainIs shExpMatch function var isPlainHostName localHostOrDomainIs isResolvable isInNet dnsResolve myIpAddress dnsDomainLevels
syn keyword pacBoolean     true false
syn keyword pacIdentifier  host url domain

hi def link pacComment    Comment
hi def link pacStatement  Statement
hi def link pacType       Type
hi def link pacNumber     Number
hi def link pacBoolean    Boolean
hi def link pacString     String
hi def link pacIdentifier Identifier
hi def link pacSpecial    Special
hi def link pacConnection PreProc

let b:current_syntax = "pac"

" vim: ts=8
