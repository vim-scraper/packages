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

nmap <silent> <leader>os	:call Omnetpp_simple()<CR>
imap <silent> <leader>os	:call Omnetpp_simple()<CR>

nmap <silent> <leader>om	:call Omnetpp_module()<CR>
imap <silent> <leader>om	:call Omnetpp_module()<CR>

nmap <silent> <leader>oc	:call Omnetpp_channel()<CR>
imap <silent> <leader>oc	:call Omnetpp_channel()<CR>

nmap <silent> <leader>on	:call Omnetpp_network()<CR>
imap <silent> <leader>on	:call Omnetpp_network()<CR>

nmap <silent> <leader>oe	:call Omnetpp_message()<CR>
imap <silent> <leader>oe	:call Omnetpp_message()<CR>
"-----------------------------------------------
"statements: simple from Omnet ned file
"-----------------------------------------------
function! Omnetpp_simple()
"
	let zz=	"//\n"
	let zz= zz."// \n"
	let zz= zz."//\n"
	let zz= zz."simple  {\n"
	let zz= zz."\tparameters:\n"
	let zz= zz."\t\t\n"
	let zz= zz."\t\t\n"
	let zz= zz."\tgates:\n"
	let zz= zz."\t\t\n"
	let zz= zz."} \t\t\t#end of simple definition"
	put =zz
	
	normal 8kA
	:startinsert
endfunction

"-----------------------------------------------
"statements: module from OMNeT ned file
"-----------------------------------------------

function! Omnetpp_module()
"
	let zz= "//\n// \n//\n"
	let zz= zz."module  {\n\ttypes:\t\t\n\t\t\n"
	let zz= zz."\tparameters:\n\t\t\n\t\t\n"
	let zz= zz."\tgates:\n\t\t\n\t\t\n"
	let zz= zz."\tsubmodules:\n\t\t\n\t\t\n"
	let zz= zz."\tconnections:\n\t\t\n\t\t\n" 
	let zz= zz."}\t\t\t\t#end of module definition"
	put =zz

	normal 17kA
	:startinsert
endfunction


"------------------------------------------------
"statements: channel from OMNeT ned file
"------------------------------------------------

function! Omnetpp_channel()
"
	let zz= "//\n//  \n//\n"
	let zz= zz."channel  {\n\t\t"


	let zz=zz."}\t\t\t#end of channel definition"
	normal 4k
	:startinsert
endfunction


"------------------------------------------------
"statements: network from OMNeT ned file
"------------------------------------------------

function! Omnetpp_network()
"
	let zz= "//\n//  \n//\n"
	let zz= zz."network  {\n\t\t"


	let zz= zz."}\t\t\t\t#end of network definition"
	normal 4k
	:startinsert
endfunction


"-------------------------------------------------
"statements: message from OMNeT ned file
"-------------------------------------------------

function! Omnetpp_message()
"
	let zz= "//\n//  \n//\n"
	let zz= zz."message  {\n\t\t"

	let zz= zz."}\t\t\t\t#end of message definition"
	normal 4k
	:startinsert
endfunction
