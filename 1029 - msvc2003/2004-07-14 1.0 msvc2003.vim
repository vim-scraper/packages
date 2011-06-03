" Vim compiler file
" Compiler:	Miscrosoft Visual C++ 2003 ToolKit
" Important:    The version DOSE NOT support Mirosoft.Net Framework
"               Only for ISO C++ standard code
" Maintainer:   Zhang Bin <zhang.bin@163.com>
" Last Change:	2004-07-11

if exists("current_compiler")
  finish
endif
let current_compiler = "msvc2003"

let s:cpo_save = &cpo
set cpo-=C

" The errorformat for MSVC-2003 is the default.
setlocal errorformat&
setlocal makeprg=cl\ /EHsc\ /TP\ %<.cpp\ /Fe%<.exe

let &cpo = s:cpo_save
unlet s:cpo_save