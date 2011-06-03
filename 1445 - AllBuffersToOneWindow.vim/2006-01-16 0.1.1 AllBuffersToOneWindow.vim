" AllBuffersToOneWindow.vim : Bring all remote Vim window buffers together in one Vim window.
"
" Name Of File: AllBuffersToOneWindow.vim
" Maintainer:   omi taku <advweb@jcom.home.ne.jp>
" URL:          http://members.jcom.home.ne.jp/advweb/
" Script URL:   http://www.vim.org/scripts/script.php?script_id=1445
" Last Change:  2006/01/15
" Version:      0.1.1
"
" Installation:
"    1. Copy the AllBuffersToOneWindow.vim script to
"       $HOME/vimfiles/plugin or $HOME/.vim/plugin directory directory.
"       Refer to ':help add-plugin', ':help add-global-plugin' and ':help runtimepath'
"       for more details about Vim plugins.
"    2. Restart Vim.
"    3. To run this script, Vim needs to be compiled with "+clientserver" option.
"
" Usage:
"    This script adds one new command ":AllBuffersToOneWindow".
"    You can use the ":AllBuffersToOneWindow" command to load buffers that are
"    loaded in remote Vim window in main Vim window.
"
"    1. Run Vim application (process A), and edit File1, File2, FIle3.
"    2. Run Vim application (process B), and edit File4, File5
"    3. Run Vim application (process C), and input ":AllBuffersToOneWindow"
"       command.
"    4. Input ":buffers" command. (Vim application process C)
"       File1, File2, File3, File4 and File5 are added to buffer list.
"
" Note:
"    o  To run this script, Vim needs to be compiled with "+clientserver" option.
"    o  If buffer is file and can be read, buffer will be loaded in main Vim window.
"    o  If buffer is already loaded in main window, buffer will not be loaded.
"
" History:
"    0.1.1   o  Some Changes are added to file loading logic.
"    0.1     o  Initial upload.


" if plugin is already loaded then, not load plugin.
if exists("loaded_AllBuffersToOneWindow")
	finish
endif
let loaded_AllBuffersToOneWindow = 1

" copy from broadcast.vim
" broadcast.vim : Send keystrokes to all VIM windows
" http://www.vim.org/scripts/script.php?script_id=316
function! s:ServerName(...)
	let sname = a:2
	let i = 0

	while sname != ''
		let j = stridx(sname, nr2char(10))
		if i == a:1
			if j > 0
				let sname = strpart(sname, 0, j)
			endif
			break
		endif

		let sname = strpart(sname, j + 1)
		let i = i + 1
	endwhile
	return sname
endfunction

" copy from broadcast.vim
" broadcast.vim : Send keystrokes to all VIM windows
" http://www.vim.org/scripts/script.php?script_id=316
function! s:Broadcast(...)
	let i = 0
	let slist = serverlist()

	" Send keystrokes to all other windows first
	while 1
		let sname = s:ServerName(i, slist)
		if sname == ''
			break
		endif
		if sname != v:servername
			call remote_send(sname, a:2)
		endif
		let i = i + 1
	endwhile

	" Send keystrokes to current window
	if a:1 == 1
		call remote_send(v:servername, a:2)
	endif
endfunction

" server
" send buffer name to client.
function! ABTOWServer(client)
	let bufnr = bufnr('$')
	let i = 0
	while i < bufnr
		let bufname = expand('#' . i . ':p')
		if filereadable(bufname)
			" sleep 100m each file
			sleep 100m
			call remote_send(a:client, ':call ABTOWClient("' . bufname . '")<CR>')
		endif
		let i = i + 1
	endwhile
endfunction

" client
" if buffer is not loaded, load buffer.
function! ABTOWClient(fname)
	if bufloaded(a:fname)
		" buffer is already loaded
		return
	endif
	" add buffer to main Vim window
	execute "badd " . a:fname
endfunction

" start
" call ABTOWServer()
function! AllBuffersToOneWindow()
	let i = 0
	let slist = serverlist()

	while 1
		let sname = s:ServerName(i, slist)
		if sname == ''
			break
		endif
		if sname != v:servername
			call Broadcast(0, ':call ABTOWServer("' . v:servername . '")<CR>')
		endif

		let i = i + 1
	endwhile
endfunction

" Register functions
if has('clientserver')
	command! -nargs=0 AllBuffersToOneWindow call AllBuffersToOneWindow()
else
	echo 'Vim is not compiled with +clientserver'
endif

