" Vim syntax file
" Language:	Remind
" Maintainer:	Davide Alberani <alberanid@libero.it>
" Last Change:	10 May 2006
" Version:	0.3
" URL:		http://erlug.linux.it/~da/vim/syntax/remind.vim
"
" remind is a sophisticated reminder service
" you can download remind from:
"   http://www.roaringpenguin.com/penguin/open_source_remind.php

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" shut case off
syn case ignore

syn keyword remindCommands	REM OMIT SET FSET UNSET
syn keyword remindExpiry	UNTIL SCANFROM SCAN WARN SCHED
syn keyword remindTag		PRIORITY TAG
syn keyword remindTimed		AT DURATION
syn keyword remindMove		ONCE SKIP BEFORE AFTER
syn keyword remindSpecial	INCLUDE INC BANNER PUSH-OMIT-CONTEXT PUSH CLEAR-OMIT-CONTEXT CLEAR POP-OMIT-CONTEXT POP
syn keyword remindRun		MSG MSF RUN CAL SATISFY SPECIAL PS PSFILE SHADE MOON
syn keyword remindConditional	IF ELSE ENDIF IFTRIG
syn match remindComment		"#.*$"
syn region remindString		start=+'+ end=+'+ skip=+\\\\\|\\'+ oneline
syn region remindString		start=+"+ end=+"+ skip=+\\\\\|\\"+ oneline
syn keyword remindDebug		DEBUG DUMPVARS DUMP ERRMSG FLUSH PRESERVE
syn match remindVar		"\$[_a-zA-Z][_a-zA-Z0-9]*"
syn match remindSubst		"%[^ ]"
syn match remindAdvanceNumber	"\(\*\|+\|-\|++\|--\)[0-9]\+"
" This will match trailing whitespaces that seem to broke rem2ps.
" Courtesy of Michael Dunn.
syn match remindWarning		display excludenl "\S\s\+$"ms=s+1


if version >= 508 || !exists("did_remind_syn_inits")
  if version < 508
    let did_remind_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink remindCommands		Function
  HiLink remindExpiry		Repeat
  HiLink remindTag		Label
  HiLink remindTimed		Statement
  HiLink remindMove		Statement
  HiLink remindSpecial		Include
  HiLink remindRun		Function
  HiLink remindConditional	Conditional
  HiLink remindComment		Comment
  HiLink remindString		String
  HiLink remindDebug		Debug
  HiLink remindVar		Identifier
  HiLink remindSubst		Constant
  HiLink remindAdvanceNumber	Number
  HiLink remindWarning		Error

  delcommand HiLink
endif

let b:current_syntax = "remind"

" vim: ts=8 sw=2
