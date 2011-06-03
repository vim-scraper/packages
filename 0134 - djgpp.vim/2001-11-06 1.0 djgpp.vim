" Vim compiler file
" Compiler:		DJGPP MS-Dos Gnu compiler
" Maintainer:	Vadim V. Volkov vvolkov@mail.ru
" Last Change:	2001 Nov 6

if exists("current_compiler")
  finish
endif
let current_compiler = "djgpp"

" A errorformat for DJGPP
setlocal errorformat=%f:%l:\ %m,%Dgmake[%*\\d]:\ Entering\ directory\ `%f',%Dgmake[%*\\d]:\ Leaving\ directory\ `%f'
 
" default make
setlocal makeprg=redir\ -e\ error\ make
setlocal shellpipe=
setlocal makeef=error
