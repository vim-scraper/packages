" rcs.vim -- Automatically handle RCS controlled files.
"
" Copyright (C) 2002-2004  Christian J. Robinson <infynity@onewest.net>
" Distributed under the terms of the Vim license.  See ":help license".
"
"------------------------------------------------------------------------------
"
" $Id: rcs.vim,v 1.21 2006/08/23 04:05:17 infynity Exp $
"
" Log: {{{1
"
" $Log: rcs.vim,v $
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

if v:version < 600
	finish
endif

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

		" exe 'amenu <silent> ' . g:rcs_plugin_menu_priority . '.10 ' . g:rcs_plugin_toplevel_menu .
		" 	\ '&RCS.Lock                                  :!rcs -l %<CR>'
		" exe 'amenu <silent> ' . g:rcs_plugin_menu_priority . '.20 ' . g:rcs_plugin_toplevel_menu .
		" 	\ '&RCS.UnLock                                :!rcs -u %<CR>'
		exe 'amenu <silent> ' . g:rcs_plugin_menu_priority . '.30 ' . g:rcs_plugin_toplevel_menu .
			\ '&RCS.&Diff<Tab>:RCSdiff                  :RCSdiff<CR>'
		exe 'amenu <silent> ' . g:rcs_plugin_menu_priority . '.40 ' . g:rcs_plugin_toplevel_menu .
			\ '&RCS.Show\ &Log<Tab>:RCSlog              :RCSlog<CR>'
		exe 'amenu <silent> ' . g:rcs_plugin_menu_priority . '.60 ' . g:rcs_plugin_toplevel_menu .
			\ "&RCS.Check\\ Out\\ [&RO]<Tab>:RCSco\\ ro :RCSco ro<CR>"
		exe 'amenu <silent> ' . g:rcs_plugin_menu_priority . '.60 ' . g:rcs_plugin_toplevel_menu .
			\ "&RCS.Check\\ Out\\ [&W]<Tab>:RCSco\\ w   :RCSco w<CR>"
		exe 'amenu <silent> ' . g:rcs_plugin_menu_priority . '.70 ' . g:rcs_plugin_toplevel_menu .
			\ '&RCS.Check\ &In<Tab>:RCSci               :RCSci<CR>'
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
	finish
endif
let g:loaded_rcs_plugin = 1

" Autocommands: {{{1
augroup RCS_plugin
au!
autocmd FileChangedRO * nested
	\ if filereadable(expand('<afile>:p:h') . '/RCS/' . expand("<afile>:t") . ',v') && (confirm("This is a read-only RCS controlled file, check out?", "&Yes\n&No", 1, "Q") == 1) |
	\   call s:RCS_CheckOut(expand('<afile>:p'), 1) |
	\ endif

autocmd BufUnload * nested
	\ if getbufvar(expand('<afile>:p'), 'RCS_CheckedOut') != '' && (getbufvar(expand('<afile>:p'), 'RCS_CheckedOut') == expand('<afile>:p')) && (confirm(expand('<afile>:t') . " is an RCS controlled file checked out by Vim.\nCheck back in?", "&Yes\n&No", 1, "Q") == 1) |
	\   call s:RCS_CheckIn(expand('<afile>:p'), 0) |
	\ endif
augroup END

" Commands: {{{1
command!          RCSdiff call <SID>RCS_Diff(expand("%:p"))
command!          RCSlog  call <SID>RCS_ViewLog(expand("%:p"))
command! -nargs=1 RCSco   call <SID>RCS_CheckOut(expand("%:p"), <f-args>)
command! -nargs=? RCSci   call <SID>RCS_CheckIn(expand("%:p"))

" Functions: {{{1

function! s:RCS_Diff(file)  " {{{2
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
	exe 'silent call delete("' . rcs_diff_file . '")'
	exe 'setlocal filetype=' . filetype . ' syntax=' . syntax
	setlocal buftype=nofile noswapfile foldmethod=diff readonly nomodifiable

	if v:version > 601 || v:version == 601 && has('patch287')
		setlocal bufhidden=wipe
	else
		setlocal bufhidden=delete
	endif

	exe 'autocmd! BufDelete ' . escape(rcs_diff_name, '[]') . ' ' .
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&diff", "0") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&wrap", "' . wrap . '") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&scrollbind", "0") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&foldcolumn", "' . foldcolumn . '") | '
		\ 'call setwinvar(bufwinnr(' . curbuf . '), "&foldmethod", "' . foldmethod . '") | '
		\ 'redraw! | '
		\ 'autocmd! BufDelete ' . escape(rcs_diff_name, '[]')

	normal zX
	wincmd p
	normal zX
endfunction

function! s:RCS_ViewLog(file)  " {{{2
	let file_escaped=escape(a:file, ' \')

	exe 'silent new [RCS\ Log:\ ' . file_escaped . ']'
	setlocal noreadonly
	exe 'silent 0r !rlog ' . a:file
	call append(0, '+++ Press space to page down, "b" to page up, "q" to quit. +++')

	syntax case match
	syntax match rcslogKeys   '^\%1l+++ .\+ +++$'
	syntax match rcslogDelim  '^-\{4,}$'
	syntax match rcslogDelim  '^=\{4,}+$'
	syntax match rcslogValues '^revision [0-9.]\+$' contains=rcslogNumber
	syntax match rcslogFile   '^\(RCS file\|Working file\): .\+' contains=rcslogString
	syntax match rcslogValues '^\(head\|branch\|locks\|access list\|symbolic names\|keyword substitution\|total revisions\|description\|locked by\|date\):\( [^;]\+\)\=' contains=rcslogString
	syntax match rcslogValues '\(author\|state\): [^;]\+;'me=e-1 contains=rcslogString
	syntax match rcslogValues '\(lines\|selected revisions\): [ 0-9+-]\+$' contains=rcslogNumber
	syntax match rcslogString ': [^;]\+'ms=s+2 contained contains=rcslogNumber
	syntax match rcslogNumber '[+-]\=[0-9.]\+' contained
	highlight default link rcslogKeys    Todo
	highlight default link rcslogDelim   PreProc
	highlight default link rcslogValues  Identifier
	highlight default link rcslogFile    Type
	highlight default link rcslogString  String
	highlight default link rcslogNumber  Number

	if v:version > 601 || v:version == 601 && has('patch287')
		setlocal bufhidden=wipe
	else
		setlocal bufhidden=delete
	endif

	setlocal buftype=nofile noswapfile readonly nomodifiable

	nnoremap <buffer> q <C-w>c
	nnoremap <buffer> <space> <C-f>
	nnoremap <buffer> b <C-b>

	1 " Go to the first line in the file.
endfunction

function! s:RCS_CheckOut(file, mode)  " {{{2
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

function! s:RCS_CheckIn(file, ...)  " {{{2
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

	let fullrlog = substitute(fullrlog, "\\([\"$]\\)", "\\\\\\1", "g")

	if fullrlog =~ '^[[:return:][:space:]]\+$'
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
" }}}1

let &cpoptions = s:savecpo
unlet s:savecpo

" vim600:fdm=marker:fdc=3:cms=\ "\ %s:
