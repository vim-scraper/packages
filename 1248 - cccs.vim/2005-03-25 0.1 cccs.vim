" Vim syntax file
" Language:	Clearcase config spec
" Maintainer:	Jean-Alain Geay <jageay@free.fr>
" Last Change:	2005 Mar 25

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

set iskeyword+=-

syntax keyword cccsType       element

syntax keyword cccsConstant   main LATEST CHECKEDOUT
syntax match   cccsConstant   /[\\/]0\>/hs=s+1
syntax match   cccsConstant   /[\\/]lost+found/hs=s+1

syntax keyword cccsStatement  mkbranch time load
syntax match   cccsStatement  /end\s\+mkbranch/
syntax match   cccsStatement  /end\s\+time/

syntax keyword cccsOptionalClause  -mkbranch -nocheckout -time
syntax keyword cccsOptionalClause  -override
syntax keyword cccsOptionalClause  -config -none -error
syntax keyword cccsOptionalClause  -file -directory -eltype

syntax keyword cccsInclude    include

syntax match   cccsComment    "#.*$"

" Default highlighting
if version >= 508 || !exists("did_cccs_syntax_inits")
  if version < 508
    let did_cccs_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink cccsType       Type
  HiLink cccsConstant   Constant
  HiLink cccsStatement  Statement
  HiLink cccsOptionalClause  Statement
  HiLink cccsInclude    Include
  HiLink cccsComment    Comment
  delcommand HiLink
endif

let b:current_syntax = "cccs"
