" CONTENT: easy marking of changes in an document by <ins>, <del> tags
" AUTHOR: Mirko Scholz

" insert an insert tag
:nmap <F5> a<lt>ins></ins><ESC>6ha

" insert an attention tag
:nmap <F8> a<lt>attn></attn><ESC>7ha

" marks "deleted" the currently visually selected text
:vmap D "1c<lt>del><lt>/del><ESC>6h"1gp6l

" marks "inserted" the currently visually selected text
:vmap I "1c<lt>ins><lt>/ins><ESC>6h"1gp6l

" marks "replaced" the currently visually selected text
:vmap R Dh<F5>

" inserts the surrounding stuff
:nmap <silent> <F11> :call TXTInit()<CR>
:nmap <silent> <F8> :call TXTRevision()<CR>

:set noai

function! TXTInit()
	let @0 = "</pre>"
	let ln = line ("$")
	exec  ln."put 0"
	let @0 = "<pre>"
	exec "1put! 0"
	call TXTRevision()
endfunction

function! TXTRevision()
	let @0 = "<rev>".strftime ("%Y%m%d%H")."</rev>"
	exec "1put 0"
	exec "1,2join!"
endfunction

