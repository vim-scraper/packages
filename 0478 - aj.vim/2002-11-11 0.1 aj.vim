" Vim syntax file
" Language: AspectJ
" Maintainer: Therapon Skotiniotis<skotthe@ccs.neu.edu>
" Last Change: 2002 Oct 28
"
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" source the java.vim file
if version < 600
  source <sfile>:p:h/java.vim
else
  runtime! syntax/java.vim
endif
unlet b:current_syntax

" remove the javaError for .. in aspectJ
syn clear javaError
syn match ajError "<<<\|=>\|<>\|||=\|&&=\|[^-]->\|\*\/"

syn keyword ajAspectDef aspect priviledged dominates perthis pertarget
syn keyword ajPointcut  pointcut
syn keyword ajAdviceDecl  before after around returning throwing proceed
syn keyword ajIntroductions declare soft error warning parents
syn keyword ajPointcuts call execution initialization staticinitialization get set handler within withincode cflow cflowbelow target args
" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_css_syn_inits")
  if version < 508
    let did_css_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink ajPointcuts Function
  HiLink ajAspectDef Type
  HiLink ajPointcut Type
  HiLink ajAdviceDecl Statement
  HiLink ajIntroductions Statement
  HiLink ajPointcuts Statement
  HiLink ajError Error
  delcommand HiLink
endif

let b:current_syntax = "aj"

