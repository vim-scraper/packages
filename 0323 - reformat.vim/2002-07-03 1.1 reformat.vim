""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" File:          reformat.vim
" Author:        Jean-Baptiste Quenot <jb.quenot@caraldi.com>
" Purpose:       Reformat paragraphs, even in DocBook documents
" Date Created:  2002-06-28 13:24:42
" Last Modified: 2002-07-03 19:08:27
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" reformat text
imap <C-J> <C-O>:call ReformatParagraph()<cr>
nmap <C-J> :call ReformatParagraph()<cr>
vmap <C-J> gq<cr>
nmap Q <C-J>
vmap Q <C-J>

function ReformatParagraph ()
	if &filetype == 'docbk'
		exe 'normal! ?<para>v/<\/para>gq'
	else
		exe 'normal! gqap'
	endif
endfunction
