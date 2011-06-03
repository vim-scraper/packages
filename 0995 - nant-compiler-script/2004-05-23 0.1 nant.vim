" Vim Compiler File
" Compiler:	nant
" Maintainer:
" Last Change:

if exists("current_compiler")
    finish
endif

let current_compiler = "nant"

setlocal makeprg=nant\ -find
setlocal errorformat=%\\s%#%f(%l%.%c):\ %trror\ %\\a%#%n:\ %m,%\\s%#%f(%l%.%c):\ %tarning\ %\\a%#%n:\ %m,%\\s%#%f(%l)\ :\ %trror\ %\\a%#%n:\ %m,%\\s%#%f(%l)\ :\ %tarning\ %\\a%#%n:\ %m,%\\s%#%f(%l)\ %tatal:\ %m,%\\s%#%f(%l)\ %trror:\ %m,%\\s%#%f(%l)\ %tarning:\ %m,%\\s%#%f(%l)\ %tint:\ %m,%EBUILD\ FAILED,%C%f(%l%.%c):,%C%m
