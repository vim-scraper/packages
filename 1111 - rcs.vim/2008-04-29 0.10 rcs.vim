" rcs.vim -- Automatically handle RCS controlled files.
"
" Author:      Christian J. Robinson <infynity@onewest.net>
" URL:         http://www.infynity.spodzone.com/vim
" Last Change: April 29, 2008
" Version:     0.10
"
" Copyright (C) 2002-2008  Christian J. Robinson <infynity@onewest.net>
" Distributed under the terms of the Vim license.  See ":help license".
"
" Install Details: -----------------------------------------------------------
"
" Make the following directories (replace ~/.vim with the appropriate
" directory if you're on Windows--see ":help 'runtimepath'"):
"   ~/.vim/plugin
"   ~/.vim/doc
"
" Place this script in the plugin directory, then start Vim. The
" documentation should automatically be created. Then you can do:
"   :help rcs.txt
"
" Help File: ------------------------------------------------------------ {{{1
" *rcs.txt*	Assist with editing RCS controlled files.
"		Author: Christian J. Robinson
"
"						*rcs.vim* *rcs*
"
" This is a set of autocommands, commands, and a menu to help you handle RCS
" controlled files.  It requires Vim 7.0 or later to run.
"
" ------------------------------------------------------------------------------
"
" 1. Introduction				|rcs-intro|
" 2. Commands				|rcs-commands|
" 3. Configuration Variables		|rcs-configuration|
"
" ==============================================================================
" 1. Introduction					*rcs-intro*
"
" If you try to modify a readonly file that has a RCS/<file>,v counterpart you
" will be asked if you want to check the file out for modification, and when
" you unload the buffer you'll be prompted if you want to check the file back
" in, and allowed to enter a log message.
"
" The commands have corresponding menu items, which should be fairly
" self-explanatory.
"
" ==============================================================================
" 2. Commands					*rcs-commands*
"
"							*:RCSdiff*
" :RCSdiff
"	View the differences between the working file and the last revision,
"	using vimdiff.
"
"							*:RCSlog*
" :RCSlog
"	Show the entire log file--syntax highlighted--in a split window.
"	Individual log entries can be edited from this display.
"
"							*:RCSco*
" :RCSco ro
"	Check out the current file readonly (unlocked).
"
" :RCSco w
"	Check out the current file writable (locked).
"
"	The ":RCSco" command will ask if you want to discard changes if you
"	already have a locked/modifiable file.
"
"							*:RCSci*
" :RCSci
"	Check the current (changed) file in, you will be prompted for a log
"	message, and the file will automatically be checked back out readonly.
"
"							*:RCSUpdateHelp*
" :RCSUpdateHelp [directory]
"	Update the help file for this script.  If you specify a directory the help
"	file will be written there rather than the doc directory relative to where
"	this script is installed--useful if that directory is the wrong one.
"
"
" ==============================================================================
" 3. Configuration Variables			*rcs-configuration*
"
" *g:rcs_plugin_toplevel_menu*
" The name of the menu to place the RCS menu into, if you don't want it at the
" top level.
"
" *g:rcs_plugin_menu_priority*
" The menu priority to use for the RCS menu.
"
" *g:rcs_plugin_menu_force*
" Force loading of the menu in console Vim.
"
" Examples: >
"  :let g:rcs_plugin_toplevel_menu = '&Misc'
"  :let g:rcs_plugin_menu_priority = '130.10'
"  :let g:rcs_plugin_menu_force = 1
" <
" ----------------------------------------------------------------------- }}}1
"
" $Id: rcs.vim,v 1.30 2008/04/29 20:36:49 infynity Exp $
"
" ChangeLog: {{{1
"
" $Log: rcs.vim,v $
" Revision 1.30  2008/04/29 20:36:49  infynity
" :RCSUpdateHelp {arg} wasn't working
"
" Revision 1.29  2008/04/26 19:47:08  infynity
" Added the :RCSUpdateHelp [directory] command
"
" Revision 1.28  2008/04/17 06:11:32  infynity
" Delay the helpfile auto-update until Vim has initialized so it won't stop gvim
"  from starting from a non-terminal
"
" Revision 1.27  2008/04/16 03:54:31  infynity
" Enhanced the log display and editing feature
" Auto-install a help file if possible
" Refactoring
"
" Revision 1.26  2008/04/15 04:58:53  infynity
" *** empty log message ***
"
" Revision 1.25  2008/04/15 04:55:38  infynity
" Allow editing of individual revision log messages from the log display
"
" Revision 1.24  2008/04/08 16:28:31  infynity
" *** empty log message ***
"
" Revision 1.23  2008/04/08 16:19:43  infynity
" Internal documentation added
" Code clean up
"
" Revision 1.22  2007/08/08 04:16:45  infynity
" FIleChangedRO autocmd opens folds -- possibly undesirable
"
" Revision 1.21  2006/08/23 04:05:17  infynity
" Perserve cursor position when reloading the buffer after calling co/ci.
"
" Revision 1.20  2006/08/14 07:02:17  infynity
" Changed for Vim7 compatibility.
"
" Revision 1.19  2004/04/17 01:52:18  infynity
" Added copyright information.
"
" Revision 1.18  2004/03/22 12:43:24  infynity
" Fixed detection of existence of diff window for RCS_Diff().
"
" Revision 1.17  2004/01/26 22:31:28  infynity
" Restore 'foldcolumn' when the diff window is closed.
"
" Revision 1.16  2003/12/28 15:05:12  infynity
" Functionalize more stuff.
" Diffing now handled better, including restoring options when the diff window
"  is closed
"
" Revision 1.15  2003/12/20 06:11:26  infynity
" Use "setlocal" rather than "set" in some places.
" Close all folds in both windows when doing a diff view
"
" Revision 1.14  2003/10/09 21:36:05  infynity
" Properly escape $ characters.
"
" Revision 1.13  2003/05/23 21:26:48  infynity
" Change RCSco command call of RCS_CheckOut to use <f-args> rather than <args>.
"
" Revision 1.12  2003/04/21 22:21:12  infynity
" *** empty log message ***
"
" Revision 1.11  2003/04/14 00:24:57  infynity
" *** empty log message ***
"
" Revision 1.10  2003/04/13 03:08:30  infynity
" Syntax highlight the contents of the RCS log window.
"
" Revision 1.9  2003/04/13 01:13:50  infynity
" *** empty log message ***
"
" Revision 1.8  2003/04/12 09:35:56  infynity
" *** empty log message ***
"
" Revision 1.7  2003/04/12 09:34:43  infynity
" Commands for everything added for console vim.
" Show Log command displays in a vim window.
" Tweaks.
"
" Revision 1.6  2003/04/09 20:58:14  infynity
" *** empty log message ***
"
" Revision 1.5  2003/02/05 09:17:07  infynity
" Deleted old commented code.
"
" Revision 1.4  2002/12/07 00:29:48  infynity
" Set 'cpoptions' to make sure the file sources properly, then restore it.
"
" Revision 1.3  2002/09/06 07:26:26  infynity
" Moved RCS.Diff menu item contents to RCSdiff command.
" Use script-local functions &c.
" Other clean-ups.
"
" Revision 1.2  2002/06/29 14:30:00  infynity
" *** empty log message ***
"
" Revision 1.1  2002/06/29 14:10:03  infynity
" Initial revision
"
" }}}1

if v:version < 700
	echohl ErrorMsg
	echomsg "Vim 7.0 or greater is needed to run " . expand('<sfile>:p')
	echohl None
	finish
endif

" Auto-update the help file if necessary and possible:  {{{1
let s:self    = expand('<sfile>')
let s:selfdoc = expand('<sfile>:p:h:h') . '/doc/' . expand('<sfile>:p:t:r') . '.txt'
if getftime(s:self) > getftime(s:selfdoc)
	" Do this /after/ Vim has initialized, because tempname() breaks
	" otherwise:
	autocmd VimEnter * RCSUpdateHelp
endif  " }}}1

let s:savecpo = &cpoptions
set cpoptions&vim

" Menus: {{{1
if ! exists("g:loaded_rcs_plugin_menu")
	if has('gui_running') || exists('g:rcs_plugin_menu_force')
		let g:loaded_rcs_plugin_menu = 1

		if ! exists('g:rcs_plugin_toplevel_menu')
			let g:rcs_plugin_toplevel_menu = ''
		endif
		if ! exists('g:rcs_plugin_menu_priority')
			let g:rcs_plugin_menu_priority = ''
		endif

		let s:m = g:rcs_plugin_toplevel_menu
		let s:p = g:rcs_plugin_menu_priority

		if s:m[-1:] != '.'
			let s:m = s:m . '.'
		endif

		if s:p[-1:] != '.'
			let s:p = s:p . '.'
		endif

		" exe 'amenu <silent> ' . s:p . '10 ' . s:m .
		" 	\ '&RCS.Lock                                  :!rcs -l %<CR>'
		" exe 'amenu <silent> ' . s:p . '20 ' . s:m .
		" 	\ '&RCS.UnLock                                :!rcs -u %<CR>'
		exe 'amenu <silent> ' . s:p . '30 ' . s:m .
			\ '&RCS.&Diff<Tab>:RCSdiff                  :RCSdiff<CR>'
		exe 'amenu <silent> ' . s:p . '40 ' . s:m .
			\ '&RCS.Show\ &&\ Edit\ &Log<Tab>:RCSlog    :RCSlog<CR>'
		exe 'amenu <silent> ' . s:p . '60 ' . s:m .
			\ "&RCS.Check\\ Out\\ [&RO]<Tab>:RCSco\\ ro :RCSco ro<CR>"
		exe 'amenu <silent> ' . s:p . '60 ' . s:m .
			\ "&RCS.Check\\ Out\\ [&W]<Tab>:RCSco\\ w   :RCSco w<CR>"
		exe 'amenu <silent> ' . s:p . '70 ' . s:m .
			\ '&RCS.Check\ &In<Tab>:RCSci               :RCSci<CR>'

		unlet s:m s:p
	else
		augroup RCS_plugin_menu
			au!
			exe 'autocmd GUIEnter * source ' . expand('<sfile>')
		augroup END
	endif
endif
" }}}1

if exists("g:loaded_rcs_plugin")
	let &cpoptions = s:savecpo
	unlet s:savecpo
	finish
endif
let g:loaded_rcs_plugin = 1

" Autocommands: {{{1
augroup RCS_plugin
	au!
	autocmd FileChangedRO * nested call s:FileChangedRO()
	autocmd BufUnload * nested call s:BufUnload()
augroup END

" Commands: {{{1
command!          RCSdiff call s:Diff(expand("%:p"))
command!          RCSlog  call s:ViewLog(expand("%:p"))
command! -nargs=1 RCSco   call s:CheckOut(expand("%:p"), <f-args>)
command! -nargs=? RCSci   call s:CheckIn(expand("%:p"))

command! -nargs=? RCSUpdateHelp call s:UpdateHelp(
			\ s:self,
			\ (<q-args> != '' ? fnamemodify(<q-args> . '/' . fnamemodify(s:self, ':p:t:r') . '.txt', '') : s:selfdoc)
		\)

" Functions: {{{1

function! s:FileChangedRO()  " {{{2
	if filereadable(expand('<afile>:p:h') . '/RCS/' . expand("<afile>:t") . ',v')
				\ && (confirm("This is a read-only RCS controlled file, check out?", "&Yes\n&No", 1, "Q") == 1)
		call s:CheckOut(expand('<afile>:p'), 1)
		silent! foldopen!
	endif
endfunction

function! s:BufUnload()  " {{{2
	if getbufvar(expand('<afile>:p'), 'RCS_CheckedOut') != ''
				\ && (getbufvar(expand('<afile>:p'), 'RCS_CheckedOut') == expand('<afile>:p'))
				\ && (confirm(expand('<afile>:t') . " is an RCS controlled file checked out by Vim.\nCheck back in?", "&Yes\n&No", 1, "Q") == 1)
		call s:CheckIn(expand('<afile>:p'), 0)
	endif
endfunction

function! s:Diff(file)  " {{{2
	let rcs_diff_name = "[Previous version of " . fnamemodify(a:file, ':t') . "]"

	if bufnr('^\V' . rcs_diff_name) != -1
		echohl ErrorMsg
		echo "Already viewing differences for the current file."
		echohl None
		return
	endif

	let rcs_diff_name = escape(rcs_diff_name, ' \')
	let rcs_diff_file = tempname()
	let curbuf        = bufnr(a:file)
	let filetype      = getbufvar(a:file, '&filetype')
	let syntax        = getbufvar(a:file, '&syntax')
	let wrap          = getbufvar(a:file, '&wrap')
	let foldcolumn    = getbufvar(a:file, '&foldcolumn')
	let foldmethod    = getbufvar(a:file, '&foldmethod')

	silent call system('co -p ' . a:file . ' > ' . rcs_diff_file . ' 2> /dev/null')
	exe 'silent vertical rightbelow diffsplit ' . rcs_diff_file
	exe 'silent file ' . rcs_diff_name
	exe 'silent bwipe! ' . rcs_diff_file
	call delete(rcs_diff_file)
	exe 'setlocal filetype=' . filetype . ' syntax=' . syntax
	setlocal buftype=nofile noswapfile foldmethod=diff readonly nomodifiable
	setlocal bufhidden=wipe

	exe 'autocmd! BufDelete <buffer> ' .
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&diff", "0") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&wrap", "' . wrap . '") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&scrollbind", "0") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&foldcolumn", "' . foldcolumn . '") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&foldmethod", "' . foldmethod . '") | '
		\ 'redraw!'

	normal zX
	wincmd p
	normal zX
endfunction

function! s:CheckOut(file, mode)  " {{{2
	let mode = ''

	if a:mode == 1 || a:mode == 'w'
		let mode = '-l '
	endif

	if filewritable(a:file)
		if confirm(a:file . " is writable (locked).\nForce a check out of previous version (your changes will be lost)?", "&Yes\n&No", 2, 'Q') == 1
			let mode = '-f ' . mode
			let RCS_Out = system('co ' . mode . a:file)
		elseif a:mode == 1 || a:mode == 'w' && confirm('Tell Vim this is a controlled RCS file anyway?', "&Yes\n&No", 1, 'Q') == 1
			let b:RCS_CheckedOut = a:file
			return
		else
			return
		endif
	else
		let RCS_Out = system('co ' . mode . a:file)
	endif

	if v:shell_error
		echoerr "Nonzero exit status from 'co " . mode . "...':"
		echohl ErrorMsg | :echo RCS_Out | :echohl None
		let v:errmsg = RCS_Out
		"echoerr RCS_Out

		return 1
	endif

	if a:mode == 1 || a:mode == 'w'
		let b:RCS_CheckedOut = a:file
	elseif exists('b:RCS_CheckedOut')
		let b:RCS_CheckedOut = ''
	endif

	let eventignore_save = &eventignore
	let &eventignore = 'BufUnload,FileChangedRO'
	let l = line(".")
	let c = col(".")
	execute "silent e!"
	call cursor(l, c)
	let &eventignore = eventignore_save
endfunction

function! s:CheckIn(file, ...)  " {{{2
	call setbufvar(a:file, 'RCS_CheckedOut', '')

	let rlog = "" | let fullrlog = ""
	if &columns >= 82
		echo "  Enter log message (. to end):                                         <-70 80->|\n"
	else
		echo "  Enter log message (. to end):                                         <-70\n"
	endif

	while rlog != "."
		let fullrlog = fullrlog . "\n" . rlog
		let rlog = input("> ")
	endwhile

	let fullrlog = escape(fullrlog, '"$')

	if fullrlog =~ '^[[:return:][:space:]]*$'
		let fullrlog = '*** empty log message ***'
	endif

	let RCS_Out = system("ci -m\"" . fullrlog . "\" " . a:file)
	if v:shell_error
		echoerr "Nonzero exit status from 'ci -m ...':"
		echohl ErrorMsg | :echo RCS_Out | :echohl None
		let v:errmsg = RCS_Out
		"echoerr RCS_Out
	endif

	let RCS_Out = system('co ' . a:file)
	if v:shell_error
		echoerr "Nonzero exit status from 'co ...':"
		echohl ErrorMsg | :echo RCS_Out | :echohl None
		let v:errmsg = RCS_Out
		"echoerr RCS_Out
	endif

	if a:0 >= 1 && a:1 == 0
		return
	endif

	let eventignore_save = &eventignore
	let &eventignore = 'BufUnload,FileChangedRO'
	let l = line(".")
	let c = col(".")
	execute "silent e!"
	call cursor(l, c)
	let &eventignore = eventignore_save
endfunction

function! s:ViewLog(file)  " {{{2
	let file_escaped=escape(a:file, ' \')

	exe 'silent new [RCS\ Log:\ ' . file_escaped . ']'
	let b:rcs_filename = a:file

	call s:ViewLog2(a:file)

	syntax case match
	syntax match rcslogDelim  '^-\{4,}$'
	syntax match rcslogDelim  '^=\{4,}+$'
	syntax match rcslogValues '^revision [0-9.]\+$' contains=rcslogNumber
	syntax match rcslogFile   '^\(RCS file\|Working file\): .\+' contains=rcslogString
	syntax match rcslogValues '^\(head\|branch\|locks\|access list\|symbolic names\|keyword substitution\|total revisions\|description\|locked by\|date\):\( [^;]\+\)\=' contains=rcslogString
	syntax match rcslogValues '\(author\|state\): [^;]\+;'me=e-1 contains=rcslogString
	syntax match rcslogValues '\(lines\|selected revisions\): [ 0-9+-]\+$' contains=rcslogNumber
	syntax match rcslogString ': [^;]\+'ms=s+2 contained contains=rcslogNumber
	syntax match rcslogNumber '[+-]\=[0-9.]\+' contained
	highlight default link rcslogKeys    Comment
	highlight default link rcslogDelim   PreProc
	highlight default link rcslogValues  Identifier
	highlight default link rcslogFile    Type
	highlight default link rcslogString  String
	highlight default link rcslogNumber  Number
	highlight default link rcslogCurrent NonText

	setlocal buftype=nofile noswapfile readonly nomodifiable bufhidden=wipe

	nnoremap <buffer> q <C-w>c
	nnoremap <buffer> <space> <C-f>
	nnoremap <buffer> b <C-b>
	nnoremap <silent> <buffer> J :if search('^-\+\nrevision \d\+\.\d\+', 'W')<bar>exe 'normal j'<bar>endif<CR>
	nnoremap <silent> <buffer> K :call search('^revision \d\+\.\d\+', 'Wb')<CR>
				\:call search('^-\+\nrevision \d\+\.\d\+', 'Wb')<CR>j
	nnoremap <silent> <buffer> <cr> :call <SID>EditLogItem()<CR>
	nnoremap <silent> <buffer> <c-l> <c-l>:call <SID>ViewLog2(b:rcs_filename)<CR>

	autocmd CursorMoved <buffer> call s:LogHighlight()
endfunction

function! s:ViewLog2(file)  " {{{2
	setlocal noreadonly modifiable
	let where = s:ByteOffset()
	silent! 1,$delete
	exe 'silent 0r !rlog ' . a:file
	let keys = [
			\ '+++ Keys:                                                 +++',
			\ '+++  <space>     -  Page down                             +++',
			\ '+++  b           -  Page up                               +++',
			\ '+++  <control-l> -  Refresh the screen and reload the log +++',
			\ '+++  J           -  Jump to next log section              +++',
			\ '+++  K           -  Jump to previous log section          +++',
			\ "+++  <enter>     -  Edit the current log entry's message  +++",
			\ '+++  q           -  Close this log view                   +++'
		\ ]
	call append(0, keys)
	setlocal readonly nomodifiable
	1 " Go to the first line in the file.
	silent! execute 'goto ' . where

	exe 'syntax match rcslogKeys   =^\%<' . (len(keys) + 1) . 'l+++ .\+ +++$='
endfunction

function! s:LogHighlight()  " {{{2
	let curline = line('.')
	let back    = search('^-\+\nrevision \d\+\.\d\+', 'bWn')
	let forward = search('^-\+$', 'Wn')

	if back > 0 && curline >= back && curline <= forward && getline('.') !~ '^-\+$'
		execute '2match rcslogCurrent /^\%' . back . 'l\_.\+\%' . forward . 'l-\+/'
	else
		2match
	endif

	" A faster/easier way?:
	"  /^-\+\(\n\(-\+\)\@!.\+\)*\%#.*\(\n\(-\+\)\@!.\+\)*
endfunction

function! s:EditLogItem()  " {{{2
	if ! exists('b:rcs_filename')
		echohl ErrorMsg
		echomsg "Can't determine the filename associated with the current log"
		echohl None
		return 0
	endif

	let rcs_filename = b:rcs_filename
	let curline = line('.')
	let back    = search('^-\+\nrevision \d\+\.\d\+', 'bWn')
	let forward = search('^-\+$', 'Wn')

	if back > 0 && curline >= back && curline <= forward && getline('.') !~ '^-\+$'
		let line = getline(back + 1)
		let id   = substitute(line, 'revision \(\d\+\.\d\+\).*', '\1', '')

		let fname =  '[Log entry for ' . fnamemodify(rcs_filename, ':p:t') . ' revision ' . id . ']'

		if bufloaded(fname)
			echohl ErrorMsg
			echo "A buffer for that log message already exists"
			echohl None
			return 0
		endif

		execute 'new ' . escape(fname, ' \')
		setlocal buftype=acwrite
		let b:rcs_id       = id
		let b:rcs_filename = rcs_filename
		silent! execute 'read !rlog -r' . id . ' ' . escape(rcs_filename, ' \')
		silent! 1,/^revision .\+\ndate: \d\{4\}\/\d\d\/\d\d \d\d:\d\d:\d\d.*/+1 delete
		silent! $delete
		call append(0, ["+++ Change the log message below this line and write+quit +++", ''])
		setlocal nomodified

		syntax match rcslogKeys =^\%<2l+++ .\+ +++$=
		highlight default link rcslogKeys Todo

		autocmd BufWriteCmd <buffer> call s:SaveLogItem()
	else
		echohl ErrorMsg
		echom "The cursor isn't within a log section"
		echohl None
		return 0
	endif
endfunction

function! s:SaveLogItem()  " {{{2
	if ! exists('b:rcs_id') || ! exists('b:rcs_filename')
		return 0
	endif

	let lnum     = 1
	while match(getline(lnum), '^+++ .\+ +++$') >= 0
		let lnum = lnum + 1
	endwhile

	if match(getline(lnum), '^$') >= 0
		let lnum = lnum + 1
	endif

	let fullrlog = join(getline(lnum, '$'), "\n")
	let fullrlog = escape(fullrlog, '"$')

	if fullrlog =~ '^[[:return:][:space:]]*$'
		let fullrlog = '*** empty log message ***'
	endif

	let RCS_Out = system("rcs -m" . b:rcs_id  . ":\"" . fullrlog . "\" " . b:rcs_filename)
	if v:shell_error
		echoerr "Nonzero exit status from 'ci -m ...':"
		echohl ErrorMsg | :echo RCS_Out | :echohl None
		let v:errmsg = RCS_Out
	endif

	setlocal nomodified
endfunction

function! s:ByteOffset()  " {{{2
        return line2byte(line(".")) + col(".") - 1
endfunction

function! s:UpdateHelp(self, doc)  " {{{2
	let docdir = fnamemodify(a:doc, ':p:h')
	if filewritable(docdir) != 2
		echohl ErrorMsg
		echomsg "Can't write to directory \"" . docdir . "\"." . 
					\"  Please make sure it exists and is writable."
		echohl None
		return 0
	endif

	echomsg "Updating help file for " . fnamemodify(a:self, ':p:t')

	let temp = tempname()
	silent execute 'split ' . temp
	silent execute 'read ' . a:self
	silent /^" Last Change:
	let foo = getline('.')
	silent 1,/" Help File: -\+ {{{\d$/ delete
	silent /" -\+ }}}\d$/,$ delete
	silent call append(1, substitute(foo, '" ', '"\t\t', ''))
	silent %s/^" \=//
	silent execute 'w! ' . a:doc
	silent bwipe!
	call delete(temp)
	silent execute 'helptags ' . docdir
	nohlsearch

	return 1
endfunction
" }}}1

let &cpoptions = s:savecpo
unlet s:savecpo

" vim600:fdm=marker:fdc=3:cms=\ "\ %s:fml=2:
