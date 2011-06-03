" Vim syntax file
" Language:    SFL
" Maintainer:  Yoshihiro Iida <3aepm001@keyaki.cc.u-tokai.ac.jp>
" Last Change: 2004 Feb 17
" URL:         http://shimizu-lab.dt.u-tokai.ac.jp/
" Filenames:   *.sfl
" Version:     2.0

syn case ignore

" storage types
syn keyword  Yellow  module declare if par any alt stage state else circuit
syn keyword  Yellow  state_name first_state input output instrin bidirect segment instruct task
syn keyword  Yellow  instrout instr_arg instrself reg reg_ws reg_wr sel mem sel_v bus bus_v
syn keyword  Yellow  stage_name goto call return generate relay finish
syn match    Purple  "%i"
syn match    Purple  "%d"
syn match    Green   "\^"
syn match    Green   "&"
syn match    Green   "/&"
syn match    Green   "|"
syn match    Green   "/|"
syn match    Green   "@"
syn match    Green   "/@"
syn match    Green   "#"
syn match    Green   "=="
syn match    Green   ":"
syn match    Green   "="
syn match    Green   "+"

syn match Blue   "//.*"
syn match Purple "[{}<>().,;]"
syn match White  "[a-z_][a-zA-Z0-9_]*"
syn match Red    "\<[A-Z][A-Z0-9_]*\>\C"

syn match Red    "0\+[1-7]\=[\t\n$,; ]"
syn match Red    "[0-9]\d*"
syn match Red    "0[oO][0-7]\+" "hs=s+2
syn match Red    "0[xX][0-9a-fA-F]\+" "hs=s+2
syn match Red    "0[bB][0-1]*" "hs=s+2

syntax region  Blue start="/\*" end="\*/"
syntax region  Red  start="\"" end="\""

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
  hi Red    ctermfg=darkred
  hi Blue   ctermfg=darkblue
  hi White  ctermfg=none
  hi Green  ctermfg=darkgreen
  hi Purple ctermfg=darkmagenta
  hi Yellow ctermfg=darkyellow
  hi Cyan   ctermfg=Cyan

  delcommand HiLink
endif

let b:current_syntax = "sfl"

" vim: ts=8
