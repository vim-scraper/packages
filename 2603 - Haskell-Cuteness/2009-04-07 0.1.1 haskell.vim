" Haskell Cuteness for Vim.
" Inspired by emacs-haskell-cuteness.
" Based on unilatex.vim by Jos van den Oever <oever@fenk.wau.nl>
" Version: 0.1
" Last Changed: 7 April 2009
" Maintainer: Andrey Popp <andrey.popp@braintrace.ru>

imap <buffer> \ λ
imap <buffer> <- ←
imap <buffer> -> →
imap <buffer> <= ≲
imap <buffer> >= ≳
imap <buffer> == ≡
imap <buffer> /= ≠

if exists("s:loaded_unihaskell")
	finish
endif
let s:loaded_unihaskell = 1

augroup HaskellC
	autocmd BufReadPost *.hs cal s:HaskellSrcToUTF8()
	autocmd BufWritePre *.hs cal s:UTF8ToHaskellSrc()
	autocmd BufWritePost *.hs cal s:HaskellSrcToUTF8()
augroup END

" function to convert ''fancy haskell source'' to haskell source
function s:UTF8ToHaskellSrc()
	let s:line = line(".")
	let s:column = col(".")

	silent %s/λ/\\/eg
	silent %s/←/<-/eg
	silent %s/→/->/eg
	silent %s/≲/<=/eg
	silent %s/≳/>=/eg
    silent %s/≡/==/eg
    silent %s/≠/\/=/eg

	let &l:fileencoding = s:oldencoding
	call cursor(s:line,s:column)
endfunction

" function to convert haskell source to ''fancy haskell source''
function s:HaskellSrcToUTF8()
	let s:line = line(".")
	let s:column = col(".")

	let s:oldencoding = &l:fileencoding
	set fileencoding=utf-8

	silent %s/\\/λ/eg
	silent %s/->/→/eg
	silent %s/<-/←/eg
	silent %s/<=/≲/eg
	silent %s/>=/≳/eg
    silent %s/==/≡/eg
    silent %s/\/=/≠/eg

	let &l:fileencoding = s:oldencoding
	call cursor(s:line,s:column)
endfunction

do HaskellC BufRead
