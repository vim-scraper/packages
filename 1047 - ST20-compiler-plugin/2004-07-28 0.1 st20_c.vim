" Vim compiler file
" Compiler:	STMicroelectronics C (st20icc)
" Maintainer:	Wenzhi Liang<wzhliang@speedymail.org>
" Last Change:	2004 July 28

if exists("current_compiler")
  finish
endif
let current_compiler = "st20_c"

set errorformat=Serious-st20icc-%f(%l)-%m,Error-st20icc-%f(%l)-%m,%DExamining\ %f,%X--
set makeprg=gmake
