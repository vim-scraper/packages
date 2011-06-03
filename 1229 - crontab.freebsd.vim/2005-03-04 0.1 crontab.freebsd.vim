" Vim syntax file
" Language:	FreeBSD crontab
" Author:	Fduch M. Pravking
" Based on:	crontab.vim by John Hoelzel
"		http://johnh51.get.to/vim/syntax/crontab.vim
" Last change:	Fri Mar  4 2005
" Filenames:    /tmp/crontab.* used by "crontab -e"
"
" crontab line format:
" Minutes   Hours   Days   Months   Days_of_Week   Commands # comments

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn match   crontabWordErr	"\S\+" contained
syn match   crontabEolErr	"\s*$" contained

syn match   crontabSol	"^"	nextgroup=crontabEnv,crontabMin,crontabWhence,crontabCmnt,crontabWordErr skipwhite

syn match   crontabEnv	_\(\w\+\|'.\{-}'\|".\{-}\)\(\s*=\)\@=_ contained nextgroup=crontabSet skipwhite
syn match   crontabSet	"=" contained nextgroup=crontabEnvValue skipwhite
syn match   crontabEnvValue '\S.*' contained

syn match   crontabMin	"\([0-9]\+\(-[0-9]\+\)\?\(,[0-9]\+\(-[0-9]\+\)\?\)*\|\*\)\(/[0-9]\+\)\?"
    \ nextgroup=crontabHr,crontabWordErr,crontabEolErr skipwhite contained
syn match   crontabHr	"\([0-9]\+\(-[0-9]\+\)\?\(,[0-9]\+\(-[0-9]\+\)\?\)*\|\*\)\(/[0-9]\+\)\?"
    \ nextgroup=crontabDay,crontabWordErr,crontabEolErr skipwhite contained
syn match   crontabDay	"\([0-9]\+\(-[0-9]\+\)\?\(,[0-9]\+\(-[0-9]\+\)\?\)*\|\*\)\(/[0-9]\+\)\?"
    \ nextgroup=crontabMon,crontabWordErr,crontabEolErr skipwhite contained
syn match   crontabMon	"\([a-z0-9]\+\(-[a-z0-9]\+\)\?\(,[a-z0-9]\+\(-[a-z0-9]\+\)\?\)*\|\*\)\(/[0-9]\+\)\?"
    \ nextgroup=crontabDow,crontabWordErr,crontabEolErr skipwhite contained contains=crontabMon12
syn match   crontabDow	"\([a-z0-9]\+\(-[a-z0-9]\+\)\?\(,[a-z0-9]\+\(-[a-z0-9]\+\)\?\)*\|\*\)\(/[0-9]\+\)\?"
    \ nextgroup=crontabCmd skipwhite contained contains=crontabDow7

syntax keyword crontabMon12	contained jan feb mar apr may jun jul aug sep oct nov dec
syntax keyword crontabDow7	contained sun mon tue wed thu fri sat

syn match   crontabWhence	"@\(reboot\|yearly\|annually\|monthly\|weekly\|daily\|midnight\|hourly\)\>"
    \ contained nextgroup=crontabCmd skipwhite


"  syntax region crontabCmd  start="\<[a-z0-9\/\(]" end="$" nextgroup=crontabCmnt skipwhite contained contains=crontabCmnt keepend

syntax region crontabCmd  start="\S" end="$" contained contains=crontabCmnt,crontabPercent,crontabSpecial,crontabString,crontabSubCmd keepend
syntax match  crontabCmnt /#.*/

syn region  crontabString	start="'" end="'" contained matchgroup=crontabQuote contains=crontabPercent,crontabSpecial
syn region  crontabString	start='"' end='"' skip='\\"' contained matchgroup=crontabQuote contains=crontabPercent,crontabSpecial
syn region  crontabString	start="`" end="`" contained matchgroup=crontabBackQuote contains=crontabPercent,crontabSpecial
syn match   crontabPercent	"%" contained
syn match   crontabSpecial	"\\%" contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_crontab_syn_inits")
  if version < 508
    let did_crontab_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink crontabEolErr		Error
  HiLink crontabWordErr		Error

  HiLink crontabEnv		Identifier
  HiLink crontabSet		Operator
  HiLink crontabEnvValue	String

  HiLink crontabMin		Number
  HiLink crontabHr		PreProc
  HiLink crontabDay		Type

  HiLink crontabMon		Number
  HiLink crontabMon12		Keyword

  HiLink crontabDow		PreProc
  HiLink crontabDow7		Keyword

  HiLink crontabWhence		Special

" comment out next line for to suppress unix commands coloring.
  "HiLink crontabCmd		Type
  HiLink crontabPercent		PreProc
  HiLink crontabSpecial		Special
  HiLink crontabString		String
  HiLink crontabQuote		Operator
  "HiLink crontabSubCmd		Special
  HiLink crontabBackQuote	Operator

  HiLink crontabCmnt		Comment

  delcommand HiLink
endif

let b:current_syntax = "crontab"

" vim: ts=8
