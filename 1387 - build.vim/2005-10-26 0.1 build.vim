" Compiler:	Windows ce build system.
" Maintainer:	idvorkin@gmail.com
" Last Change:	10/26/2005

if exists("current_compiler")
  finish
endif
let current_compiler = "build"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=build
CompilerSet errorformat=%.%#ERR%.%#]\ %f(%l)\ :%m,%-G%.%#PROGC%.%#,%-G%.%#PROGC%.%#
