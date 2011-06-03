" Vim syntax file
" Language:	BUGS
" Maintainer:	Zhuojun Chen <uifiddle@gmail.com>
" Last Change:	2010 Feb 23
" Filenames:    *.bugs

" ---------------------------------------------------------------------
"  Load Once: {{{1
if version < 600
" For vim-version 5.x: Clear all syntax items
" For vim-version 6.x: Quit when a syntax file was already loaded
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" ---------------------------------------------------------------------
"  Clusters: {{{1

" ---------------------------------------------------------------------
"  Comment: {{{1
syn match bugsComment /\#.*/

" ---------------------------------------------------------------------
"  Identifier: {{{1
" identifier with leading letter and optional following keyword characters
syn keyword bugsFunction  cloglog equals exp inprod inverse log logdat logfact logam logit max mean min phi pow probit sd sqrt step sum

" ---------------------------------------------------------------------
"  Statement: {{{1
syn keyword bugsStatement   break next return
syn keyword bugsConditional if else
syn keyword bugsRepeat      for in repeat while

" ---------------------------------------------------------------------
"  Constant: {{{1
" string enclosed in double quotes
syn region  bugsString start=/"/ skip=/\\\\\|\\"/ end=/"/
" string enclosed in single quotes
syn region  bugsString start=/'/ skip=/\\\\\|\\'/ end=/'/
" number with no fractional part or exponent
syn match   bugsNumber /\d\+/
" floating point number with integer and fractional parts and optional exponent
syn match   bugsFloat /\d\+\.\d*\([Ee][-+]\=\d\+\)\=/
" floating point number with no integer part and optional exponent
syn match   bugsFloat /\.\d\+\([Ee][-+]\=\d\+\)\=/
" floating point number with no fractional part and optional exponent
syn match   bugsFloat /\d\+[Ee][-+]\=\d\+/
syn match   bugsOperator /[<-]\|[\~]/

" ---------------------------------------------------------------------
"  Special: {{{1
syn match   bugsDelimiter /[,;:]/
" A bunch of distributions
syn keyword bugsDistribution  dbern dbeta dcat dchisq ddexp ddirch dexp dgamma dlnorm dlogis dmnorm dmulti dnegbin dnorm dpar dpois dt dunif dweib dwish

" ---------------------------------------------------------------------
"  Error: {{{1


" ---------------------------------------------------------------------
"  Define The Default Highlighting: {{{1
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_bugs_syn_inits")
    if version < 508
	let did_bugs_syn_inits = 1
	command -nargs=+ HiLink hi link <args>
    else
	command -nargs=+ HiLink hi def link <args>
    endif
  HiLink bugsComment        Comment
  HiLink bugsConstant       Constant
  HiLink bugsString         String
  HiLink bugsNumber         Number
  HiLink bugsBoolean        Boolean
  HiLink bugsFloat          Float
  HiLink bugsStatement	    Statement
  HiLink bugsOperator       Statement
  HiLink bugsConditional    Conditional
  HiLink bugsRepeat         Repeat
  HiLink bugsDistribution   Special
  HiLink bugsFunction       Function
  HiLink bugsDelimiter      Delimiter
  
  delcommand HiLink
endif

let b:current_syntax = "bugs"

" vim: ts=8 sw=2 fdm=marker
