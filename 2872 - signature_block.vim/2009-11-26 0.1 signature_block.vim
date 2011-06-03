"---------------------------------------------------------------------------
" Vim global plugin for adding and manipulating signature blocks in e-mails
" Maintainer:  Antonio Ospite <ospite@studenti.unina.it>
" Version:     0.1
" Last Change: 2009-11-24
" License:     This script is free software; you can redistribute it and/or
"              modify it under the terms of the GNU General Public License.

" Install Details:
" Drop this file into your $HOME/.vim/plugin directory.
"
" Examples:
" map <Leader>s :call AppendSignature('~/.signature')<CR>
" map <Leader>r :call ReplaceLastSignature('~/.signature')<CR>
" map <Leader>R :call ReplaceAllSignatures('~/.signature')<CR>
"
" " Append a signature block to all e-mails
" autocmd FileType mail silent call AddSignature('~/.signature') | w
"
" " Append a signature block to cover letters generated with git-format-patch
" autocmd BufRead 0000-cover-letter.patch silent call AddSignature('~/.signature') | w
" autocmd BufRead 0000-cover-letter.patch autocmd! BufRead 0000-cover-letter.patch
"
" References:
" http://en.wikipedia.org/wiki/Signature_block
" http://tools.ietf.org/html/rfc1855
"
"---------------------------------------------------------------------------

if exists("g:loaded_signaturePlugin") | finish | endif
let g:loaded_signaturePlugin = 1


"---------------------------------------------------------------------------
" Function:     AppendSignature()
" Purpose:      Append a signature block at the end of message
"---------------------------------------------------------------------------
func! AppendSignature(sigfile)
	" Add the signature marker at the end of the file
	exe '$put =\"-- \"'

	" Append the signature block file at the end of the file
	exe '$r ' . fnameescape(a:sigfile)
endfunc


"---------------------------------------------------------------------------
" Function:     AddSignature()
" Purpose:      Add a signature block if there isn't one already
"---------------------------------------------------------------------------
func! AddSignature(sigfile)
	" Save current cursor position in mark 'z'
	normal mz

	" Append a signature block only if there isn't one already
	try
		exe '0/^-- $/'
	catch /^Vim\%((\a\+)\)\=:E486/	" catch error E486 (search command failed)
		" put an extra newline
		exe '$put =\"\n\"'
		call AppendSignature(a:sigfile)
	endtry

	" restore cursor position from mark 'z' if the mark is still valid
	silent! normal `z
endfunc


"---------------------------------------------------------------------------
" Function:     ReplaceAllSignatures()
" Purpose:      Replace all signature blocks in the message
"---------------------------------------------------------------------------
func! ReplaceAllSignatures(sigfile)
	" Save current cursor position in mark 'z'
	normal mz

	try
		" delete from the FIRST signature marker '^-- $' down to
		" the end of the file
		exe '0/^-- $/,$d'
	catch /^Vim\%((\a\+)\)\=:E486/	" catch error E486 (search command failed)
	endtry

	call AppendSignature(a:sigfile)
	
	" restore cursor position from mark 'z' if the mark is still valid
	silent! normal `z
endfunc


"---------------------------------------------------------------------------
" Function:     ReplaceLastSignature()
" Purpose:      Replace only the last signature block in the message
"---------------------------------------------------------------------------
func! ReplaceLastSignature(sigfile)
	" Save current cursor position in mark 'z'
	normal mz

	try
		" delete from the LAST signature marker '^-- $' down to
		" the end of the file
		exe '$?^-- $?,$d'
	catch /^Vim\%((\a\+)\)\=:E486/	" catch error E486 (search command failed)
	endtry

	call AppendSignature(a:sigfile)
	
	" restore cursor position from mark 'z' if the mark is still valid
	silent! normal `z
endfunc
