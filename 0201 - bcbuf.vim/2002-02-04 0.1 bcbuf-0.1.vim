" bcbuf.vim
" Name: Broadcast Buffer Operations
" Version: 0.1
" Author: Mark Hillebrand <dazzler@users.sf.net>, 20020201
" Last Change: Mon 04 Feb 2002 07:34:44 PM CET
" 
" Overview: this script provides functions to share / use buffers over
" multiple vim's (via clientserver communication, see :help +clientserver).
"
" Installation: drop this file into ~/.vim/plugin/
"
" Usage:
" List the buffers of the local vim and on all vim servers:
"     :BCbuffers or :BCls
"
" Close the current buffer and open it on a remote vim:
"     :BCtossbuf DESTSERVER
"
" Get a modifiable but unmodified buffer from a remote vim:
"     :BCb ~/filename or :BCbuffer ~/filename
"     No servername needs to be supplied since this command initiates a
"     broadcast to all other vim servers.
"     Please note, that the argument of :BCb is, in contrast to the
"     regulare :buf command *always* interpreted as a filename and not
"     possibly as a regulare expressions or a buffer number.
"
"	  Why only pass modifiable buffers? Because you may open a buffer
"	  read-only / not modifiable by using the standard :view and :sview
"	  commands.
"	  And why only pass unmodified buffers? Because, in my opinion, a default
"	  policy to either save the changes or discard the changes made in a
"	  remote vim is dangerous.
"
"	  The :BCb command may be used to circumvent the common E325 ('Found a
"	  swap file') warning, which may come up, if one tries opening a file
"	  currently being edited under another vim session. Instead of manually
"	  closing the buffer in the remote vim, :BCb makes it possible to
"	  directly obtain the buffer from the remote vim (if it happens to be a
"	  vim server).
"
" Both :BCtossbuf and :BCb employ the vim command :mkview to pass around the
" buffers, so in addition to the cursor position quite a lot of other
" information (e.g. the fold status) is preserverd.
" See :help :mkview for documentation on this.
"
" To get a list of all running servers:
"     :echo serverlist()
"     (serverlist() is a builtin vim6 function)
"
" Pitfalls:
" -  be sure that all participating vim's are invoked as servers.
"    gvim's usually are by default; for a console vim use the invocation
"    'vim --servername SOMENAME'. For vim servers to work under unix, X must
"    be running and vim must be compiled with the +clientserver feature turned
"    on.
"
" Bugs:
" - strange paths containing '/../' somewhere in the middle do not yet work as
"   an argument to :BCb, if someone knows an easy way to simplify such paths
"   in vim script, let me know. fnamemodify(), sadly, does not seem to do the
"   work.

" TODO make sure of use of expressions in buf.*() functions (they differ in
" semantic...)

" The following reg exp is used to match for servernames:
let s:servernamere = "[^\n]\\+"

fun! <SID>Tossbuf(destserver)
	" Description: close the current buffer and open it on the vim server with
	" name destserver
	" TODO if buffer is not modifiable, toss as not modifiable
	let destserver = toupper(a:destserver)
	if match("\n" . serverlist(),"\n" . destserver . "\n") < 0
		echo "?There is no server with name " . a:destserver
		return
	endif
	if &modified
		echo "Buffer is modified, doing nothing..."
		return
	endif
	let viewname = tempname()
	exec "mkview " . viewname
	bdel
	let dummy = remote_send(destserver, ":new | source " . viewname . '| silent echo delete("' . viewname . '")' . "\n" )
endfun

fun! <SID>Stealbuf(name)
	" Description: Allows to steal a (modifiable, but unmodified) buffer from a vim server
	" TODO make option to replace current window
	let serverlist = serverlist()
	let pos = 0
	let file = tempname()
	let name = expand(a:name)
	if name == ""
		echo "No file " . a:name . ", why look for its buffer in another vim?"
		return
	endif
	" up to now I know that name doesnt contain the home directory via ~
	" name may still be relative though, I don't want this
	let name = fnamemodify(name,":p")

	" see above for bug description:
	if match(name, '/\.\./') >= 0
		echo '?"' . name . '" contains relative path references /../, cannot handle this (yet)'
		return
	endif


	if bufexists(name) && bufloaded(name) && getbufvar(bufnr(name),"&modifiable")
		echo "You have a modifiable buffer for that yourself, why care stealing it?"
		return
	endif
	while match(serverlist,s:servernamere,pos) >= 0
		let server = matchstr(serverlist,s:servernamere,pos)
		let pos = matchend(serverlist,s:servernamere,pos)
		if remote_expr(server,'bufexists("' . name . '") && bufloaded("' . name . '") && getbufvar(bufnr("' . name . '"),"&modifiable")')
			if remote_expr(server,'getbufvar(bufnr("' . name . '"),"&modified")')
				echo "Server " . server . " has the desired file but it is modified, bullying out..."
				return
			endif
			let viewname = remote_expr(server,'Givemeview("' . name . '")')
			if bufexists(name) && bufloaded(name)
				" have the buffer myself but it is not modifiable, kill it
				exec "bdel " . name
			endif
			" TODO the following echo will require return if the line must be wrapped
			if bufname(winbufnr(winnr())) != ""
				" something in buffer already, open new one
				exec "new"
			endif
			exec 'silent source ' . viewname . '| silent echo delete("' . viewname . '")'
			echo "Server " . server . " gives me " . name . " (" . viewname . ")"
			return
		endif
	endwhile
	echo "No server has a modifiable version of the desired file. Why not open it the regular way?"
endfun

fun! Givemeview(name)
	" Givemeview() gets called from a remote vim via the <SID>Stealbuf()
	" function
	" It is passed the name of an existing, modifiable but nonmodified buffer
	" It must change the window to that buffer (or create a window for that
	" buffer), make an :mkview in a temporary file and return the filename
	" TODO have to think about how to plugginfy this function, the remote vims
	" must know the <SID> / <SNR> in order to call it...
	" TODO *If* the calling vim runs on a different user this probably won't work
	" because of the access rights
	" Note: this function is internal only
	let windownr = bufwinnr(a:name)
	if windownr < 0 
		" check if the following always worx:
		exec "sbuf " . a:name
		let windownr = winnr()
	else
		" change window to the number
		exec windownr . "winc w"
	endif
	let viewname = tempname()
	exec "mkview " . viewname
	bdel
	redraw
	echo "Buffer " . a:name . " has been stolen (" . strftime("%c") . ")"
	" TODO cursor disappears after this function and reappears on key hit
	return viewname
endfun

fun! <SID>BCbuffers()
	" Description: show a buffer listing from the local vim and all remote vim servers
	let serverlist = serverlist()
	let pos = 0
	let file = tempname()
	while match(serverlist,s:servernamere,pos) >= 0
		let server = matchstr(serverlist,s:servernamere,pos)
		let pos = matchend(serverlist,s:servernamere,pos)
		if server == v:servername
			" skip self
			continue
		endif
		let rc = remote_send(server,'<C-\><C-N>:redir >> ' . file . '| silent exe "echo v:servername | buffers" | redir END<CR>')
	endwhile
	" approach to cat/type from cvsmenu-1.7.vim from http://vim.sf.net 
	exec "echo 'local:' | buffers"
	if has("unix")
		let output = system("cat " . file)
	else
		let output = system("type " . file)
	endif
	let dummy = delete( file )
	echo output
endfun

com! -nargs=0 BCbuffers call <SID>BCbuffers()
com! -nargs=0 BCls call <SID>BCbuffers()
com! -nargs=1 BCtossbuf call <SID>Tossbuf(<f-args>)
  " TODO ^ would like to complete with servername's if that were possible
com! -nargs=1 -complete=file BCb call <SID>Stealbuf(<f-args>)
com! -nargs=1 -complete=file BCbuffer call <SID>Stealbuf(<f-args>)

