" Vim filetype plugin
" Language:     HP-41
" Version:		0.4
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

imap <CR> <CR><C-Y><C-Y><C-Y><SPACE><ESC>0<C-A>A<SPACE><M-+>
imap <M-L> LBL<SPACE>
imap <M-l> <LEFT>*LBL "
imap <M-G> GTO<SPACE>
imap <M-g> GTO<SPACE>
imap <M-T> RTN<CR>
imap <M-t> RTN<CR>
imap <M-O> STOP<CR>
imap <M-o> STOP<CR>
imap <M-I> ISG<SPACE>
imap <M-i> ISG<SPACE>
imap <M-D> DSE<SPACE>
imap <M-d> DSE<SPACE>
imap <M-R> RCL<SPACE>
imap <M-r> RCL<SPACE>
imap <M-S> STO<SPACE>
imap <M-s> STO<SPACE>
imap <M-X> XEQ<SPACE>
imap <M-x> XEQ<SPACE>
imap <M-E> ENTER<CR>
imap <M-e> ENTER<CR>
imap <M-Y> X<>Y<CR>
imap <M-y> X<>Y<CR>
imap <M-P> PROMPT<CR>
imap <M-p> PROMPT<CR>
imap <M-V> VIEW<CR>
imap <M-v> VIEW<CR>
imap <M-A> AVIEW<CR>
imap <M-a> AVIEW<CR>
imap <M-C> CLX<CR>
imap <M-c> CLX<CR>

map <M-W> :%s/'.*//g<CR>gg<C-V>G4<RIGHT>d

imap  <M-+> <ESC>:call Renumber()<CR>a
map  <M-+> :call Renumber()<CR>

if !exists("*s:Renumber")
	function Renumber()
		let s:linenumber = line(".")
		let s:colnumber = col(".")
		call cursor(1,1)
		normal 0cw001
		while search("^[0-9][0-9][0-9] ", "W") > 0
			execute "normal 3cl\<c-y>\<c-y>\<c-y>\<esc>\<c-a>"
		endwhile
		call cursor(s:linenumber,s:colnumber)
	endfunction
endif
