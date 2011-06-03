" Vim filetype plugin
" Language:     HP-41
" Version:		0.1
" Maintainer:   Geir Isene
" Last Change:  2005-09-17
" URL:          http://www.geir.isene.com/

" Only do this when not done yet for this buffer
if (exists("b:did_ftplugin"))
    finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

set nrformats-=octal

imap <CR> <CR><C-Y><C-Y><C-Y><ESC><C-A>a<SPACE><SPACE><M-+>
imap <M-L> <LEFT>*LBL<SPACE>
imap <M-G> GTO<SPACE>
imap <M-T> RTN<CR>
imap <M-O> STOP<CR>
imap <M-I> ISG<SPACE>
imap <M-D> DSE<SPACE>
imap <M-R> RCL<SPACE>
imap <M-S> STO<SPACE>
imap <M-X> XEQ<SPACE>
imap <M-E> ENTER<CR>
imap <M-Y> X<>Y<CR>
imap <M-P> PROMPT<CR>
imap <M-V> VIEW<CR>
imap <M-A> AVIEW<CR>
imap <M-C> CLX<CR>

imap  <M-+> <ESC>:call Renumber()<CR>a
map  <M-+> :call Renumber()<CR>

if !exists("*s:Renumber")
	function Renumber()
		let s:linenumber = line(".")
		let s:colnumber = col(".")
		call cursor(1,1)
		normal 0cw001
		while search("^[0-9][0-9][0-9] ", "W") > 0
			execute "normal 0cw\<c-y>\<c-y>\<c-y>\<esc>\<c-a>"
		endwhile
		call cursor(s:linenumber,s:colnumber)
	endfunction
endif
