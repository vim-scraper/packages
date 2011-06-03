" Vim file plugin for editing files with unicode codes.
" i.e. it changes \u00E9 to é when viewing, and puts \u00E9 when writing
" I use it for java JSP "properties" files
" for help with the accented characters, see :help digraph
" copied/mangled from a script on www.vim.org that did it for TeX codes
"
" Usage: put in your $VIMDIR/plugin directory
"		then change "properties" to whatever file extension you want switched
" Last Change: $Date: 2004/02/12 14:58:28 $
" Version: $Revision: 1.1 $ 
" Maintainer: Roger Pilkey (rpilkey at magma.ca)

if exists("s:loaded_unicodeswitch")
	finish
endif
let s:loaded_unicodeswitch = 1

" multi-byte is required
if ! has("multi_byte")
	echoerr "unicodeswitch.vim: Sorry, this version of (g)vim was not compiled with +multi_byte" 
	finish
endif

"change "properties" to whatever file extension you want to switch for
augroup properties
	autocmd BufReadPost *.properties call s:toUTF8()
	autocmd BufWritePre *.properties call s:toUnicode()
	autocmd BufWritePost *.properties call s:toUTF8()
augroup END

" The function Nr2Hex() returns the Hex string of a number.
func s:Nr2Hex(nr)
	let n = a:nr
	let r = ""
	while n
		let r = '0123456789ABCDEF'[n % 16] . r
		let n = n / 16
	endwhile
	return r
endfunc

" function to convert utf8 symbols to unicode codes
function s:toUnicode()
	" store cursor position
	let s:line = line(".")
	let s:column = col(".")
	" convert specified utf-8 symbols to unicode codes
	silent %s/\(à\|á\|â\|ã\|ä\|å\|æ\|è\|é\|ê\|ë\|ì\|í\|î\|ï\|ò\|ó\|ô\|õ\|ö\|ù\|ú\|û\|ü\|ý\|ÿ\|¿\|¡\|ñ\|ß\|ç\|«\|»\)/\='\\u00'.s:Nr2Hex(char2nr(submatch(1)))/eg
	" restore old encoding before writing
	let &l:fileencoding = s:oldencoding
	" restore cursor position
	call cursor(s:line,s:column)

endfunction

" function to convert unicode codes to utf-8 symbols
function s:toUTF8()
	" store cursor position
	let s:line = line(".")
	let s:column = col(".")

	" store the fileencoding
	let s:oldencoding = &l:fileencoding
	" set the encoding to utf-8
	set fileencoding=utf-8

	" convert all unicode codes to utf-8 symbols
	silent %s/\\u00\(..\)/\=nr2char("0x".submatch(1))/eg
	" restore cursor position
	call cursor(s:line,s:column)

endfunction

