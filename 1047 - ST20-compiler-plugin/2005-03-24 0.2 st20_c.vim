" Vim compiler file
" Usage: drop this file in $VIM/compiler/ and restart Vim.
" Configuration: g:st20_compile_enable_warning decideds whether warning is turned on or not. It is turned on by default.
"                Having the following line in your .vimrc will turn it off:
"                let g:st20_compile_enable_warning="no"
" Compiler:	STMicroelectronics C (st20icc)
" Maintainer:	Wenzhi Liang<wenzhi.liang_at_gmail.com>
" Last Change:	2005 March 24

if exists("current_compiler")
  finish
endif
let current_compiler = "st20_c"

"Setting default to ignore warning
if !exists("g:st20_compile_enable_warning")
    let g:st20_compile_enable_warning="yes"
endif

if g:st20_compile_enable_warning == "yes"
    set errorformat=Serious-st20icc-%f(%l)-%m,Warning-st20icc-%f(%l)-%m,Error-st20icc-%f(%l)-%m,%DExamining\ %f,%X--
else
    set errorformat=Serious-st20icc-%f(%l)-%m,Error-st20icc-%f(%l)-%m,%DExamining\ %f,%X--
endif

set makeprg=gmake
