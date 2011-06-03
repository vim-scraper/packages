" Vim syntax file
" Language:	SFL
" Maintainer:	Yoshihiro Iida <3aepm001@keyaki.cc.u-tokai.ac.jp>
" Last Change:	Jul 19, 2003
" URL:          http://shimizu-lab.dt.u-tokai.ac.jp/
" Filenames:    *.sfl
" Version:	0.2

syn case ignore


" storage types
syn keyword	sflType		module declare par any alt stage state instruct task
syn keyword	sflRepeat	if state_name first_state input output instrin bidirect
syn keyword	sflRepeat	instrout instr_arg instrself reg reg_ws reg_wr sel mem
syn keyword	sflRepeat	stage_name
syn match	sflInclude	"%i"
syn match	sflInclude	"%d"
syn match	sflLogic	"\^"
syn match	sflLogic	"&"
syn match	sflLogic	"/&"
syn match	sflLogic	"|"
syn match	sflLogic	"/|"
syn match	sflLogic	"@"
syn match	sflLogic	"/@"
syn match	sflType		"#"
syn match	sflRepeat	"=="
syn keyword	sflTodo		contained TODO

syn match	sflComment	"//.*" oneline contains=sflTodo
syntax region	sflComment	start="/\*" end="\*/" contains=sflTodo
syntax region	sflLogic	start="\"" end="\""

"syn match sflAny		"\s*[a-z_][a-z0-9_]*\s*:"he=e-1
"syn match sflAny		"\.*\s*[a-z_][a-z0-9_<>]*\s*:"he=e-1

syn match sflLabel		"[A-Z_][A-Z0-9_]*"
syn match sflIdentifier		"[a-z_][a-z0-9_]*"

syn match decNumber		"0\+[1-7]\=[\t\n$,; ]"
syn match decNumber		"[0-9]\d*"
syn match octNumber		"0[oO][0-7]\+" "hs=s+2
syn match hexNumber		"0[xX][0-9a-fA-F]\+" "hs=s+2
syn match binNumber		"0[bB][0-1]*" "hs=s+2

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_sfl_syn_inits")
  if version < 508
    let did_sfl_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  " The default methods for highlighting.  Can be overridden later
  HiLink sflSection	Special
  HiLink sflComment	Comment
  HiLink sflDirective	Statement

  HiLink sflInclude	Include
  HiLink sflCond	PreCondit
  HiLink sflMacro	Macro

  HiLink hexNumber	Number
  HiLink decNumber	Number
  HiLink octNumber	Number
  HiLink binNumber	Number

  HiLink sflTodo	Todo
  HiLink sflIdentifier Identifier
  HiLink sflType	Type
  HiLink sflRepeat	Repeat
  HiLink sflInclude	Include
  HiLink sflAny		Repeat
  HiLink sflLabel	Number
  HiLink sflLogic	Number

  " My default color overrides:
  " hi sflSpecialComment ctermfg=red
  " hi sflIdentifier ctermfg=lightcyan
  " hi sflType ctermbg=black ctermfg=brown

  delcommand HiLink
endif

let b:current_syntax = "sfl"

" vim: ts=8
