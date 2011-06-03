" Plugin: xmms2ctrl.vim
" Version: 0.1
" Purpose: XMMS2 Control through vim
" Author: Dereck Martin <dmartin@tekconxus.com> http://www.dereckmartin.com
" Website: http://code.google.com/p/xmms2ctrl
"
" Notes: nyxmms2 is required for this plugin to work.  It should installed as
" part of the XMMS2 GIT
"
" License: GPLv2
"
" Usage: \xh

if executable('nyxmms2') < 1
	echo 'nyxmms not found...not loading'
	finish
endif

function! s:XMMS2_Add()
	call system('nyxmms2 add "' . <SID>XMMS2_GetInput("Enter path and filename: ") . '"')
endfunction

function! s:XMMS2_Remove()
	call system('nyxmms2 remove ' . <SID>XMMS2_GetInput("Enter playlist number"))
endfunction

function! s:XMMS2_PlayToggle()
	call system('nyxmms2 toggle')
endfunction

function! s:XMMS2_Previous()
	call system('nyxmms2 prev')
endfunction

function! s:XMMS2_Next()
	call system('nyxmms2 next')
endfunction

function! s:XMMS2_Stop()
	call system('nyxmms2 stop')
endfunction

function! s:XMMS2_Clear()
	call system('nyxmms2 playlist clear')
endfunction

function!s:XMMS2_Seek()
	call system('nyxmms2 seek ' . <SID>XMMS2_GetInput("Enter seek value (n/+n/-n): "))
endfunction

function! s:XMMS2_Volume()
	call system('nyxmms2 server volume ' . <SID>XMMS2_GetInput("Enter volume level (0-100): "))
endfunction

function! s:XMMS2_Status()
	echo system('nyxmms2 status')
endfunction

function! s:XMMS2_Jump()
	call system('nyxmms2 jump ' . <SID>XMMS2_GetInput("Enter jump value (n/+n/-n): "))
endfunction

function! s:XMMS2_Playlist(cmd)
	if a:cmd == "list"
		echo system('nyxmms2 playlist list')
	endif
	if a:cmd == "load"
		echo <SID>XMMS2_Playlist("list")
		call system('nyxmms2 playlist switch "' . <SID>XMMS2_GetInput("Enter playlist: ") . '"')
		call <SID>XMMS2_PlayFirst()
	endif
	if a:cmd == "remove"
		echo <SID>XMMS2_Playlist("list")
		call system('xmms2 playlist remove "' . <SID>XMMS2_GetInput("Enter playlist: ") . '"')
	endif
endfunction

function! s:XMMS2_Config()
	echo system('nyxmms2 server config ' . <SID>XMMS2_GetInput("Enter Config Parameter and Value: "))
endfunction

function! s:XMMS2_PlayFirst()
	call system('nyxmms2 jump 1')
endfunction

function! s:XMMS2_GetInput(input)
	call inputsave()
	let value = input(a:input)
	call inputrestore()
	call <SID>XMMS2_ClearInput()
	return value
endfunction

function! s:XMMS2_ClearInput()
	echo ""
endfunction

function! s:XMMS2_Help()
	echo 'XMMS2 Control Help'
	echo '------------------'
	echo '\xx		toggle play/pause'
	echo '\xa		add file/url to current playlist'
	echo '\xd		delete track number from playlist'
	echo '\x,		previous track'
	echo '\x.		next track'
	echo '\xz		stop'
	echo '\xc 		clear playlist'
	echo '\xm		seek through song in seconds (aboslute/+seconds/-seconds)'
	echo '\xv		change volume (0-100)'
	echo '\xs		shows current song playing'
	echo '\xj		jump to a track number in playlist'
	echo '\xk		load a xmms2 playlist'
	echo '\xl		list all xmms2 playlists'
	echo '\x;		remove a xmms2 playlist'
	echo '\xh		show this help'
	echo '\xq		set xmms2 config options'
endfunction

nnoremap <silent> <unique> <Leader>xx :call <SID>XMMS2_PlayToggle()<CR>
nnoremap <silent> <unique> <Leader>xa :call <SID>XMMS2_Add()<CR>
nnoremap <silent> <unique> <Leader>xd :call <SID>XMMS2_Remove()<CR>
nnoremap <silent> <unique> <Leader>x, :call <SID>XMMS2_Previous()<CR>
nnoremap <silent> <unique> <Leader>x. :call <SID>XMMS2_Next()<CR>
nnoremap <silent> <unique> <Leader>xz :call <SID>XMMS2_Stop()<CR>
nnoremap <silent> <unique> <Leader>xc :call <SID>XMMS2_Clear()<CR>
nnoremap <silent> <unique> <Leader>xm :call <SID>XMMS2_Seek()<CR>
nnoremap <silent> <unique> <Leader>xv :call <SID>XMMS2_Volume()<CR>
nnoremap <silent> <unique> <Leader>xs :call <SID>XMMS2_Status()<CR> 
nnoremap <silent> <unique> <Leader>xj :call <SID>XMMS2_Jump()<CR>
nnoremap <silent> <unique> <leader>xk :call <SID>XMMS2_Playlist("load")<CR>
nnoremap <silent> <unique> <leader>xl :call <SID>XMMS2_Playlist("list")<CR>
nnoremap <silent> <unique> <leader>x; :call <SID>XMMS2_Playlist("remove")<CR>
nnoremap <silent> <unique> <leader>xh :call <SID>XMMS2_Help()<CR>
nnoremap <silent> <unique> <leader>xq :call <SID>XMMS2_Config()<CR>
