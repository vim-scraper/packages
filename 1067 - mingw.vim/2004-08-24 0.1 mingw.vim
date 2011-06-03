" Vim compiler file
" Compiler:	MinGW(gcc) compiler
" Maintainer:	Sanel Zukan <karijes@users.sourceforge.net>
" Last Change:	2004 Aug 24

" Make sure that you have full MinGW package (with binutils).
" If you are using MSys tools, line 'CompilerSet makeprg=redir\ -eo\ make'
" you can replace with 'CompilerSet makeprg=redir\ -eo\ mingw32-make'

" You can also specify path to redir.exe and make.exe like this:
" 'CompilerSet makeprg=C:/path-to-bin/redir\ -eo\ C:/path-to-bin/make'

if exists("current_compiler")
  finish
endif
let current_compiler = "mingw"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

" A workable errorformat for Mingw
CompilerSet errorformat=%f:%l:\ %m		

" default make
CompilerSet makeprg=redir\ -eo\ make
