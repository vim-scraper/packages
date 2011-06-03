""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" File:          reformat.vim
" Author:        Jean-Baptiste Quenot <jb.quenot@caraldi.com>
" Purpose:       Reformat paragraphs according to filetype
" Date Created:  2002-06-28 13:24:42
" Last Modified: 2002-07-10 13:02:20
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
	elseif &filetype == 'php' || &filetype == 'html'
		exe 'normal! ?<p>v/<\/p>gq'
	elseif &filetype == 'python'
		exe 'normal! ?"""v/"""gq'
	elseif &filetype == 'c'
		exe 'normal! ?/*v/*\/gq'
	else
		exe 'normal! gqap'
	endif
endfunction
