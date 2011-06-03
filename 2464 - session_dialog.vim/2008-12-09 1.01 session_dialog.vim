" ==========================================================================
" File:         session_dialog.vim
" Author:       David Liang <bmdavll at gmail dot com>
" Version:      1.01
" Last Change:  2008 Dec 4
" License:      GPL
"
" Description:
" session_dialog.vim provides a simple dialog and command-line based
" interface to Vim's :mksession feature. There are commands to save sessions
" and restore, delete, and list sessions from specified locations. It works
" well with the terminal as well as console dialogs in GVIM (set
" guioptions+=c), and provides command-line completion of session names.
"
" Usage:
" :SessionSave [SESSION]
"   With no arguments, displays the save dialog. Otherwise, saves a new
"   session with the given name in g:SessionSaveDirectory. The session file
"   on disk will have the g:SessionFilePrefix and g:SessionFileSuffix
"   modifiers (see Options). Spaces are allowed in the sesison name.
"   e.g. :SessionSave foo bar creates the session "foo bar"
"
" :SessionRestore [SESSION]
"   With no arguments, displays the restore dialog. Otherwise searches for a
"   session to load in g:SessionPath. If more than one session matches (if
"   sessions with the same name were found in different paths or if the
"   given pattern matched multiple sessions), you will be prompted to choose
"   one.
"   e.g. :SessionRestore foo bar will load the session created above
"
" :SessionDelete [SESSION]...
"   Displays the delete session dialog if no arguments were given, otherwise
"   deletes the sessions given as arguments. Spaces need to be escaped since
"   they would normally separate the arguments.
"   e.g. :SessionDelete foo will delete the session named "foo". If more
"   than one session was found, you will be prompted to select one. To
"   delete all sessions named "foo", use a pattern like
"        :SessionDelete [f]oo
"
" :SessionList [SESSION]...
"   Prints a list of all the sessions and their locations in g:SessionPath
"   if no arguments were given. Otherwise prints a list of sessions that
"   match the arguments.
"   e.g. :SessionList foo bar* *baz will show all the sessions named foo,
"   starting with bar, or ending with baz.
"
" ZS ZR ZD ZL
"   The default normal mode mappings for :SessionSave, :SessionRestore,
"   :SessionDelete, and :SessionList, respectively. These can be disabled
"   with g:SessionCreateDefaultMaps. Note that these will time out, unlike
"   native commands such as ZZ. (see :help 'timeoutlen')
"
" Options:
" g:SessionSaveDirectory
"   Default is "$HOME"
"   Where to save new sessions. This can be set in your vimrc or while Vim
"   is running. For example, to get the default behavior of :mksession,
"        let g:SessionSaveDirectory = "."
"
" g:SessionPath
"   Default is set to the initial value of g:SessionSaveDirectory
"   A comma-separated list of paths in which to look for session files to
"   restore or delete.
"   e.g. let g:SessionPath = ".,~,~/.vim/sessions"
"
" g:SessionFilePrefix
"   Default is ".vimsession_" (on Windows, "_vimsession_")
"
" g:SessionFileSuffix
"   Default is "" (on Windows, ".vim")
"
"   Strings to prepend and append to session names when saving to
"   g:SessionSaveDirectory. All files found in g:SessionPath that have this
"   prefix and suffix will be identified as sessions, so the combination of
"   the two should uniquely identify session files, unless you're certain
"   that all files found in g:SessionPath will be Vim sessions.
"
" g:SessionDefault
"   Unset by default
"   The default session name to save to or restore from. This is overridden
"   by v:this_session if it's not null. If both are unset, the Vim server
"   name (v:servername) will be used as the default.
"
" g:SessionConfirmOverwrite
"   0 or 1 (default)
"   Set this to 0 to save over an existing session file without asking for
"   confirmation.
"
" g:SessionQuitAfterSave
"   1 or 0 (default)
"   Whether to exit Vim after saving a new session.
"
" g:SessionCreateDefaultMaps
"   0 or 1 (default)
"   Whether to create the default normal mode mappings (ZS, ZR, ZD, and ZL).
"
" ==========================================================================

" initialization {{{
if !has("mksession") || exists("loaded_session_dialog")
	finish
endif
if v:version < 700
	echomsg "session_dialog: Vim 7.0 or better is required"
	finish
endif
let loaded_session_dialog = 1

function! s:InitVariable(var, value)
	if !exists(a:var)
		exec "let ".a:var." = '".a:value."'"
		return 1
	endif
	return 0
endfunction

call s:InitVariable("g:SessionSaveDirectory", "$HOME")
call s:InitVariable("g:SessionPath", g:SessionSaveDirectory)
call s:InitVariable("g:SessionConfirmOverwrite", 1)
call s:InitVariable("g:SessionQuitAfterSave", 0)
call s:InitVariable("g:SessionCreateDefaultMaps", 1)

if !has("win32")
call s:InitVariable("g:SessionFilePrefix", ".vimsession_")
call s:InitVariable("g:SessionFileSuffix", "")
else
call s:InitVariable("g:SessionFilePrefix", "_vimsession_")
call s:InitVariable("g:SessionFileSuffix", ".vim")
endif

let s:PatternEscape = '.*[^$\'
" }}}

" utility functions {{{
function! s:Quote(str)
	return '"'.a:str.'"'
endfunction

function! s:Shorten(path)
	return s:StripFrom(fnamemodify(a:path, ':~'),'[/\\]$')
endfunction

function! s:WarningMsg(msg)
	echohl WarningMsg | echomsg a:msg | echohl None
endfunction

function! s:ErrorMsg(msg)
	echohl ErrorMsg | echomsg a:msg | echohl None
endfunction

function! s:StripFrom(text, pat)
	return substitute(a:text, a:pat, '', 'g')
endfunction

" returns a string of choices for confirm()
function! s:GenerateChoices(list, ...)
	let taken = (a:0==1) ? a:1 : []
	let choices = ''
	for item in a:list
		if item == '' | let item = ' ' | endif
		for i in range(len(item))
			let char = tolower(strpart(item, i, 1))
			if index(taken, char) == -1
				let taken += [char]
				break
			endif
		endfor
		let choices .= strpart(item, 0, i)."&".strpart(item, i)."\n"
	endfor
	return s:StripFrom(choices,'\n$')
endfunction

function! s:MaxLen(list)
	let max = 0
	for item in a:list
		if len(item) > max
			let max = len(item)
		endif
	endfor
	return max
endfunction

function! s:RemoveDuplicates(list)
	let uniq = []
	for i in range(len(a:list))
		if index(uniq, a:list[i]) == -1
			call add(uniq, a:list[i])
		endif
	endfor
	return uniq
endfunction

function! s:IndentedBlock(str, indent, width, ...)
	let sep = (a:0==1) ? a:1 : "\n"
	let s = ''
	let strlen = len(a:str)
	let len = a:width-a:indent
	if len < 1 | return s | endif
	for i in range( strlen/len + (strlen%len>0 ? 1 : 0) )
		let s .= printf('%'.a:indent.'s', '')
		let s .= strpart(a:str, i*len, len).sep
	endfor
	return s:StripFrom(s, sep.'$')
endfunction
" }}}

" helper functions {{{
function! s:SessionToFileName(session)
	return g:SessionFilePrefix.a:session.g:SessionFileSuffix
endfunction

function! s:PathToSession(path)
	let session = fnamemodify(a:path, ':t')
	let session = s:StripFrom(session,'^'.escape(g:SessionFilePrefix, s:PatternEscape))
	let session = s:StripFrom(session,    escape(g:SessionFileSuffix, s:PatternEscape).'$')
	return session
endfunction

function! s:SessionToPath(session)
	let files = s:GetSessionFileList(a:session)
	if empty(files)
		call s:WarningMsg("No sessions match ".s:Quote(a:session))
		return ''
	elseif len(files) == 1
		return files[0]
	else
		let choices = [ "Multiple sessions match ".s:Quote(a:session).":" ]
		let j = len(len(files))
		for i in range(len(files))
			let s = printf("%".j."d. %s", i+1, s:Shorten(files[i]))
			call add(choices, s)
		endfor
		let choice = inputlist(choices)
		if choice > 0 && choice <= len(files)
			return files[choice-1]
		else
			return ''
		endif
	endif
endfunction

function! s:GetSessionFileList(session)
	let list = split(globpath(g:SessionPath, s:SessionToFileName(a:session)), "\n")
	let filelist = []
	for i in range(len(list))
		if getftype(list[i]) == 'file'
			call add(filelist, fnamemodify(list[i], ':p'))
		endif
	endfor
	return s:RemoveDuplicates(filelist)
endfunction

function! s:GetSessionNamesList(session)
	let list = s:GetSessionFileList(a:session)
	for i in range(len(list))
		let list[i] = s:PathToSession(list[i])
	endfor
	return s:RemoveDuplicates(list)
endfunction

function! s:CheckSavePath()
	let dirs = split(glob(g:SessionSaveDirectory), "\n")
	let n = len(dirs)
	if n > 1
		call s:ErrorMsg("Multiple directories match ".s:Quote(g:SessionSaveDirectory))
	elseif n == 0 || !isdirectory(dirs[0])
		call s:ErrorMsg("Invalid save directory ".s:Quote(g:SessionSaveDirectory))
		return -1
	endif
	return n-1
endfunction

function! s:GetSavePath()
	return fnamemodify(glob(g:SessionSaveDirectory), ':p')
endfunction

function! s:VerboseSessionString(sessionlist)
	let str = ''
	let files = []
	for session in a:sessionlist
		let files += s:GetSessionFileList(session)
	endfor
	let files = s:RemoveDuplicates(files)
	if empty(files) | return str | endif
	let sessions = []
	for i in range(len(files))
		call add(sessions, s:PathToSession(files[i]))
	endfor
	let indent = s:MaxLen(sessions) + 4
	if &columns - indent < 16
		for i in range(len(files))
			let str .= printf("%s\n", sessions[i])
			let str .= s:IndentedBlock(s:Shorten(files[i]), 2, &columns, "")."\n"
		endfor
	else
		for i in range(len(files))
			let str .= printf("%-".indent."s", sessions[i])
			let str .= s:StripFrom(s:IndentedBlock(s:Shorten(files[i]), indent, &columns, ""),'^\s\+')."\n"
		endfor
	endif
	return s:StripFrom(str,'\n$')
endfunction
" }}}

" I/O {{{
function! s:SaveSession(session)
	if a:session == '' || s:CheckSavePath()
		return
	endif
	let file = s:GetSavePath().s:SessionToFileName(a:session)
	let type = getftype(file)
	if type != ''
		if type != 'file'
			call s:ErrorMsg(s:Quote(s:Shorten(file))." exists and is not a file")
			return
		elseif g:SessionConfirmOverwrite == 1 && confirm("Overwrite ".s:Quote(s:Shorten(file))."?", "&Yes\n&No", 1, "Question") != 1
			echomsg "Not saved"
			return
		endif
	endif
	try
		exec "mksession! ".fnameescape(file)
		if g:SessionQuitAfterSave == 1
			qall
		endif
	catch
		call s:ErrorMsg("Could not save session to ".s:Quote(s:Shorten(file)))
	endtry
endfunction

function! s:SourceFile(file)
	if a:file == '' | return | endif
	if !filereadable(a:file)
		call s:ErrorMsg("Can't open file ".s:Quote(s:Shorten(a:file)))
		return
	endif
	try
		exec "source ".fnameescape(a:file)
	catch
		call s:ErrorMsg("Script error while loading ".s:Quote(s:Shorten(a:file)))
	endtry
endfunction

function! s:DeleteFile(file)
	if a:file == '' | return | endif
	if call("delete", [a:file]) != 0
		call s:ErrorMsg("Could not delete ".s:Quote(s:Shorten(a:file)))
	endif
endfunction
" }}}

" dialogs {{{
function! s:SaveDialog()
	if s:CheckSavePath() | return | endif
	if v:this_session != ''
		let default = s:PathToSession(v:this_session)
	elseif exists("g:SessionDefault") && g:SessionDefault != ''
		let default = g:SessionDefault
	else
		let default = s:StripFrom(v:servername,'\d\+$')
	endif
	let choices = s:GenerateChoices([default], ['o','c'])."\n&Other\n&Cancel"
	let choice = confirm("Save to which session?", choices, 1, "Question")
	if choice == 1
		let session = default
	elseif choice == 2
		let prompt = "Saving session in ".s:Shorten(s:GetSavePath())." (<Enter> cancels): "
		let session = input(prompt, "", "custom,SessionComplete")
		let session = s:StripFrom(s:StripFrom(session,'^\s\+'),'\s\+$')
	else
		return
	endif
	call s:SaveSession(session)
endfunction

function! s:RestoreDeleteDialog(action)
	let sessions = s:GetSessionNamesList('*')
	if empty(sessions)
		call s:WarningMsg("No sessions found")
		return
	endif
	let matches = [0,0,0]
	for i in range(len(sessions))
		if sessions[i] == s:PathToSession(v:this_session)
			let matches[0] = i+1
		elseif exists("g:SessionDefault") && g:SessionDefault == sessions[i]
			let matches[1] = i+1
		elseif sessions[i] == s:StripFrom(v:servername,'\d\+$')
			let matches[2] = i+1
		endif
	endfor
	if a:action == 1
		call s:WarningMsg("session_dialog:")
	endif
	let default = ( filter(matches, "v:val") + [0] )[0]
	let choices = s:GenerateChoices(sessions, ['c'])."\n&Cancel"
	let choice = confirm((a:action==0 ? "Restore" : "Delete")." which session?", choices, default, "Question")
	if choice > 0 && choice <= len(sessions)
		let path = s:SessionToPath(sessions[choice-1])
		if a:action == 0
			call s:SourceFile(path)
		else
			call s:DeleteFile(path)
		endif
	endif
endfunction
" }}}

" main handlers {{{
function! s:SessionSave(...)
	if a:0 == 0
		call s:SaveDialog()
	else
		call s:SaveSession(a:1)
	endif
endfunction

function! s:SessionRestore(...)
	if a:0 == 0
		call s:RestoreDeleteDialog(0)
	else
		call s:SourceFile(s:SessionToPath(a:1))
	endif
endfunction

function! s:SessionDelete(...)
	if a:0 == 0
		call s:RestoreDeleteDialog(1)
		return
	endif
	for session in a:000
		let matches = s:GetSessionNamesList(session)
		if empty(matches)
			call s:WarningMsg("No sessions match ".s:Quote(session))
			continue
		elseif len(matches) == 1 && matches[0] == session
			call s:DeleteFile(s:SessionToPath(session))
		else
			for file in s:GetSessionFileList(session)
				call s:DeleteFile(file)
			endfor
		endif
	endfor
endfunction

function! s:SessionList(...)
	if a:0 == 0
		let string = s:VerboseSessionString(['*'])
	else
		let string = s:VerboseSessionString(a:000)
	endif
	if string != ''
		echo string
	else
		call s:WarningMsg("No sessions found")
	endif
endfunction
" }}}

" commands and mappings {{{
function! SessionComplete(a,l,p)
	return join(s:GetSessionNamesList('*'), "\n")
endfunction
function! EscSessionComplete(a,l,p)
	return escape(SessionComplete(a:a,a:l,a:p), ' ')
endfunction

command! -nargs=? -complete=custom,SessionComplete		SessionSave		call s:SessionSave(<f-args>)
command! -nargs=? -complete=custom,SessionComplete		SessionRestore	call s:SessionRestore(<f-args>)
command! -nargs=* -complete=custom,EscSessionComplete	SessionDelete	call s:SessionDelete(<f-args>)
command! -nargs=* -complete=custom,EscSessionComplete	SessionList		call s:SessionList(<f-args>)

if g:SessionCreateDefaultMaps == 1
nnoremap	<silent>ZS	:SessionSave<CR>
nnoremap	<silent>ZR	:SessionRestore<CR>
nnoremap	<silent>ZD	:SessionDelete<CR>
nnoremap	<silent>ZL	:SessionList<CR>
endif
" }}}

" vim:set ts=4 sw=4 noet fdm=marker:
