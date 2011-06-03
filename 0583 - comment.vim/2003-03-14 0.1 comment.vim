""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" File:			comment.vim
" Author:		shimony
" Created:		03/14/03
" Description:	Contains functions to comment and uncomment visually
"	selected blocks of code.
"
" TODO:
"	Add support for other languages.	
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Function:		CommentCBlock
" Author:		shimony
" Created:		03/14/03
" Description:	Comments out a visually selected block of C or C++ code.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
fu! CommentCBlock() range
	let l:firstln = a:firstline
	let l:lastln = a:lastline
	exec l:firstln
	silent put!='#if 0 /* ' . g:Author . ' ' . strftime ('%C') . ' */'
	exec l:lastln+1
	silent put='#endif'
endf

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Function:		UnCommentCBlock
" Author:		shimony
" Created:		03/14/03
" Description:	Undoes commenting done by CommentCBlock
"
" TODO: Make this function smart enough that it only deletes lines with
"	#ifdef and #endif's in case the user highlights an uncommented block.	
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
fu! UnCommentCBlock() range
	let l:firstln = a:firstline
	let l:lastln = a:lastline
	exec l:lastln . 'd'
	exec l:firstln . 'd'
endf

vmap .c :call CommentCBlock() <CR>
vmap .C :call UnCommentCBlock() <CR>

