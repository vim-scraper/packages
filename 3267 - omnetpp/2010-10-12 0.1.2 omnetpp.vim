" Vim plugin file
" Language:	Omnet++ NED
" Maintainer:	Lei Xue <carmark.dlut@gmail.com>
" URL:          http://carmark.javaeye.com
" Last Change:	Oct 6, 2010
" Version:      0.1.1
" Released under the terms of the GNU/GPL licence v2
"
" Very basic helpful shortcuts for NED files by OMNeT++
" I am learning the NED more deeply, then I will update this file.

"----------------------------------------------
"Mapping keys for omnet++ file
"----------------------------------------------
nmap <silent> <leader>af	ofor( ; ; )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3kf;i
nmap <silent> <leader>aw	owhile( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>2kf;i
nmap <silent> <leader>adw	odo{<CR><Tab><CR><Esc>xi}while( )<Esc>k;i
nmap <silent> <leader>ae	oforeach ( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3k;w;i

nmap <silent> <leader>cf	ifor( ; ; )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3kf;i
nmap <silent> <leader>cw	iwhile( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>2kf;i
nmap <silent> <leader>cdw	ido{<CR><Tab><CR><Esc>xi}while( )<Esc>k;i
nmap <silent> <leader>ce	iforeach ( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3k;w;i


imap <silent> <leader>af	<CR>for( ; ; )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3kf;i
imap <silent> <leader>aw	<CR>while( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>2kf;i
imap <silent> <leader>adw	<CR>do{<CR><Tab><CR><Esc>xi}while( )<Esc>k;i
imap <silent> <leader>ae	<CR>foreach ( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3k;w;i

imap <silent> <leader>cf	for( ; ; )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3kf;i
imap <silent> <leader>cw	while( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>2kf;i
imap <silent> <leader>cdw	do{<CR><Tab><CR><Esc>xi}while( )<Esc>k;i
imap <silent> <leader>ce	foreach ( )<CR>{<CR><Tab><CR><Esc>xi}<Esc>3k;w;i

nmap <silent> <leader>os	<Esc><Esc>:call Omnetpp_simple()<CR>
imap <silent> <leader>os	<Esc><Esc>:call Omnetpp_simple()<CR>

nmap <silent> <leader>om	<Esc><Esc>:call Omnetpp_module()<CR>
imap <silent> <leader>om	<Esc><Esc>:call Omnetpp_module()<CR>

nmap <silent> <leader>oc	<Esc><Esc>:call Omnetpp_channel()<CR>
imap <silent> <leader>oc	<Esc><Esc>:call Omnetpp_channel()<CR>

nmap <silent> <leader>on	<Esc><Esc>:call Omnetpp_network()<CR>
imap <silent> <leader>on	<Esc><Esc>:call Omnetpp_network()<CR>

nmap <silent> <leader>oe	<Esc><Esc>:call Omnetpp_message()<CR>
imap <silent> <leader>oe	<Esc><Esc>:call Omnetpp_message()<CR>
"-----------------------------------------------
"statements: simple from Omnet ned file
"-----------------------------------------------

function! Omnetpp_simple()
"
	let zz= "//\n// \n//\n"
	let zz= zz."simple  {\n\ttypes:\t\t\n\t\t\n"
	let zz= zz."\tparameters:\n\t\t\n\t\t\n"
	let zz= zz."\tgates:\n\t\t\n\t\t\n"
	let zz= zz."\tsubmodules:\n\t\t\n\t\t\n"
	let zz= zz."\tconnections:\n\t\t\n\t\t\n" 
	let zz= zz."}\t\t\t\t\/\/end of simple definition"
	put =zz

	normal 17kA
	:startinsert
endfunction

"-----------------------------------------------
"statements: module from OMNeT ned file
"-----------------------------------------------

function! Omnetpp_module()
"
	let zz1= "//\n// \n//\n"
	let zz1= zz1."module  {\n\ttypes:\t\t\n\t\t\n"
	let zz1= zz1."\tparameters:\n\t\t\n\t\t\n"
	let zz1= zz1."\tgates:\n\t\t\n\t\t\n"
	let zz1= zz1."\tsubmodules:\n\t\t\n\t\t\n"
	let zz1= zz1."\tconnections:\n\t\t\n\t\t\n" 
	let zz1= zz1."}\t\t\t\t\/\/end of module definition"
	put =zz1

	normal 17kA
	:startinsert
endfunction


"------------------------------------------------
"statements: channel from OMNeT ned file
"------------------------------------------------

function! Omnetpp_channel()
"
	let zz2= "//\n//  \n//\n"
	let zz2= zz2."channel  {\n\t\t\n\t\t"
	let zz2=zz2."}\t\t\t\/\/end of channel definition"
	put =zz2

	normal 4kA
	:startinsert
endfunction


"------------------------------------------------
"statements: network from OMNeT ned file
"------------------------------------------------

function! Omnetpp_network()
"
	let zz3= "//\n//  \n//\n"
	let zz3= zz3."network  {\n\t\t\n\t\t"
	let zz3= zz3."}\t\t\t\t\/\/end of network definition\n"
	put =zz3

	normal 4kA
	:startinsert
endfunction


"-------------------------------------------------
"statements: message from OMNeT ned file
"-------------------------------------------------

function! Omnetpp_message()
"
	let zz4= "//\n//  \n//\n"
	let zz4= zz4."message  {\n\t\t\n\t\t"
	let zz4= zz4."}\t\t\t\t\/\/end of message definition"
	put =zz4

	normal 4kA
	:startinsert
endfunction
