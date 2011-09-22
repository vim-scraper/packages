" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/editqf.txt	[[[1
150
*editqf.txt*		For Vim version 7.3	   Last change: 2011 August 27

==============================================================================
EditQF                                                       *editqf* *editqf-toc*

DESCRIPTION                    |editqf-description|
USAGE                          |editqf-usage|
CUSTOMIZATION                  |editqf-customization|
INSTALLATION                   |editqf-installation|
KNOWN ISSUE                    |editqf-issues|
CHANGELOG                      |editqf-changelog|

==============================================================================
DESCRIPTION                                                 *editqf-description*

This script is a reimplementation and continuation of the QuickFixNotes script
(http://www.vim.org/scripts/script.php?script_id=2216). Besides the original
functionality of capturing notes and storing them in a file, this script
provides commands for easily loading the stored data, also for location lists.

Though the main functionality of this script is to make editing of quickfix
entries easy.

I did a screencast giving an overview of the basic functionality of this
plugin. It's available at vimeo: http://vimeo.com/jceb/editqf

This script can be downloaded from
http://www.vim.org/scripts/script.php?script_id=3557. The latest development
version is available at https://github.com/jceb/vim-editqf.

==============================================================================
USAGE                                                             *editqf-usage*

Create entries in the quickfix list by either running special a command like
:make or :grep or add a note by pressing <leader>n. Then bring up the quickfix
window by running the command :cw.

When you are in the quickfix window navigate to the entry you want to change.
Just press any key that would bring in into insert mode or change the text like
"i".  Automatically a new window will be opened containing all entries of the
quickfix window.

You can use the regular editing commands for editing the entries. Once you're
done, just save the buffer and leave or close the window. I recommend using :x,
because this command does both with just one command.  After that you are
brought back to the error you initially started editing in the quickfix window.

For changing the type of a quickfix entry from within the quickfix window three
convenience shortcuts are provided:
- I -> info
- W -> warning
- E -> error

Additionally the plugin provides the following commands that support
storing and restoring quickfix and location lists:
	:QFSave <FILENAME>
	:QFLoad <FILENAME>  " default is to append to the current quickfix list
	:QFLoad! <FILENAME> " replace quickfix list with the contents of file
	:QFAddNote [NOTE]   " add quickfix entry with message NOTE
	:QFAddNote! [NOTE]  " like :QFAddNote but start a new quickfix list
	:QFAddNotePattern[!] [NOTE] " add quickfix entry matching the pattern
	                            " of the current line

	:LocSave <FILENAME>
	:LocLoad <FILENAME>  " default is to append to the current location list
	:LocLoad! <FILENAME> " replace location list with the contents of file
	:LocAddNote [NOTE]   " add location entry with message NOTE
	:LocAddNote! [NOTE]  " like :LocAddNote but start a new location list
	:LocAddNotePattern[!] [NOTE] " add location entry matching the pattern
	                             " of the current line

Editqf has integrated support for the hier script
(http://www.vim.org/scripts/script.php?script_id=3564) which highlights
quickfix errors to make them more visible.

==============================================================================
CUSTOMIZATION                                             *editqf-customization*

The default filename for storing and loading quickfix and location lists is
customizable by setting the following variables in your vimrc:
	let g:editqf_saveqf_filename  = "quickfix.list"
	let g:editqf_saveloc_filename = "location.list"

Jump to the edited error when editing finished:
	let g:editqf_jump_to_error = 1

Store absolute filename when adding a new note
	let g:editqf_store_absolute_filename = 1

The default keybinding <leader>n for adding a quickfix note can be
customized by defining a mapping in your vimrc:
	nmap <leader>n <Plug>QFAddNote
	nmap <leader>N <Plug>QFAddNotePattern
	nmap <leader>l <Plug>LocAddNote
	nmap <leader>L <Plug>LocAddNotePattern

==============================================================================
INSTALLATION                                               *editqf-installation*

1. Download editqf.vba
2. Open file in vim and run :so % to install plugin
3. Restart vim

==============================================================================
KNOWN ISSUES                                                     *editqf-issues*

- When trying to edit a location list the quickfix list is opened instead!
  This is because it's not possible to tell the difference between a quickfix
  and a location list from vim script. A workaround is to open the location
  list manually: :e loc:list

- When opening a location list (filename loc:list) in a new window the location
  list of that window is opened instead! Location lists should always be edited
  by running :e loc:list. Once editing finished C-^ should be used to leave the
  location list and go back to the last edited buffer

==============================================================================
CHANGELOG                                                     *editqf-changelog*

1.3
- add convenience shortcuts for changing the type of a quickfix entry (info,
  warning or error)
- fix change of note type from error to info

1.2
- fix issue when editing entries without a type by setting the default type to
  error
- make commands overwrite commands with the same name
- clear autocommand groups at definition time
- change note type from error to info

1.1
- add support for hier script for highlighting quickfix and location entries
- add variable g:editqf_jump_to_error to make jumping to the last selected
  error optional
- add variable g:editqf_store_absolute_filename to let the user decide whether
  filenames are stored with an absolute or relative path
- prefix global variables with the plugin's name
- move all functionality from Edit to Read function to allow editing of qf:list
  and loc:list directly through vim commands (:e, :sp ...)
- add support for patterns matching
- add description of quickfix/location fields to the editing buffer
- allow deleting all entries from quickfix/location list
- change command and <Plug> names to start with prefix QF or Loc
- general refactoring and cleanup

1.0
- initial release

 vim:tw=78:ts=8:ft=help:norl:
plugin/editqf.vim	[[[1
336
" editqf.vim -- make quickfix entries editable
" Author:         Jan Christoph Ebersbach (jceb@e-jc.de)
" License:        GPL (see http://www.gnu.org/licenses/gpl.txt)
" Created:        2008-11-28
" Last Modified: Mon 25. Apr 2011 23:40:41 +0900 JST
" Revision:       1.2
" vi:             ft=vim:tw=80:sw=4:ts=8

if &cp || exists("loaded_editqf")
    finish
endif
let loaded_editqf = 1

let g:editqf_saveqf_filename         = !exists("g:editqf_saveqf_filename")         ? "quickfix.list" : g:editqf_saveqf_filename
let g:editqf_saveloc_filename        = !exists("g:editqf_saveloc_filename")        ? "location.list" : g:editqf_saveloc_filename
let g:editqf_jump_to_error           = !exists("g:editqf_jump_to_error")           ? 1               : g:editqf_jump_to_error
let g:editqf_store_absolute_filename = !exists("g:editqf_store_absolute_filename") ? 1               : g:editqf_store_absolute_filename

command! -nargs=* -bang QFAddNote :call <SID>AddNote("<bang>", "qf", 'l', <f-args>)
command! -nargs=* -bang QFAddNotePattern :call <SID>AddNote("<bang>", "qf", 'p', <f-args>)
command! -nargs=? -bang -complete=file QFSave :call <SID>Save("<bang>", "qf", <f-args>)
command! -nargs=? -bang -complete=file QFLoad :call <SID>Load("<bang>", "qf", <f-args>)

command! -nargs=* -bang LocAddNote :call <SID>AddNote("<bang>", "loc", 'l', <f-args>)
command! -nargs=* -bang LocAddNotePattern :call <SID>AddNote("<bang>", "loc", 'p', <f-args>)
command! -nargs=? -bang -complete=file LocSave :call <SID>Save("<bang>", "loc", <f-args>)
command! -nargs=? -bang -complete=file LocLoad :call <SID>Load("<bang>", "loc", <f-args>)

nmap <Plug>QFAddNote :QFAddNote<CR>
nmap <Plug>QFAddNotePattern :QFAddNotePattern<CR>
nmap <Plug>LocAddNote :LocAddNote<CR>
nmap <Plug>LocAddNotePattern :LocAddNotePattern<CR>

if ! hasmapto("<Plug>QFAddNote", "n")
	nmap <leader>n <Plug>QFAddNote
endif

if ! hasmapto("<Plug>AddNoteQFPattern", "n")
	nmap <leader>N <Plug>QFAddNotePattern
endif

function! s:Getlist(winnr, type)
	if a:type == 'qf'
		return getqflist()
	else
		return getloclist(a:winnr)
	endif
endfunction

function! s:Setlist(winnr, type, list, action)
	if a:type == 'qf'
		call setqflist(a:list, a:action)
	else
		call setloclist(a:winnr, a:list, a:action)
	endif
endfunction

function! s:RemoveEmptyPattern(winnr, type)
	let l = []
	let found_empty_pattern = 0
	for i in s:Getlist(a:winnr, a:type)
		if i.pattern == '^\V3MPT1\$'
			unlet i.pattern
			let found_empty_pattern = 1
		endif
		call add(l, i)
	endfor
	if found_empty_pattern == 1
		call s:Setlist(a:winnr, a:type, l, 'r')
	endif
endfunction

function! <SID>AddNote(bang, type, matchtype, ...)
	" @param	type		qf or loc
	" @param	matchtype	l(ine number) or p(attern)

	let note = ""
	if a:0 > 0
		let first = 1
		for i in a:000
			if first == 1
				let first = 0
				let note = i
			else
				let note = note." ".i
			endif
		endfor
	else
		let note = input("Enter Note: ")
	endif
	if note == ""
		return
	endif

	let entry = {}
	let entry["bufnr"]    = bufnr("%")
	if exists('g:editqf_store_absolute_filename') && g:editqf_store_absolute_filename == 1
		let entry["filename"] = expand("%:p")
	else
		let entry["filename"] = expand("%")
	endif
	let entry["lnum"]     = ""
	let entry["col"]      = ""
	let entry["pattern"]  = ""
	if a:matchtype == 'l'
		let entry["lnum"]     = line(".")
		let entry["col"]      = col(".")
	else
		" / needs to be escaped in order jump to prevent the pattern from
		" ending too early
		let entry["pattern"]  = '^\V'.substitute(getline('.'), '/', '\\/', 'g').'\$'
	endif
	let entry["vcol"]     = ""
	"let entry["nr"]       = 0
	let entry["text"]     = note
	let entry["type"]     = "I"

	if a:bang == '!'
		call s:Setlist(0, a:type, [entry], "r")
	else
		call s:Setlist(0, a:type, [entry], "a")
	endif

	if exists(':HierUpdate')
		HierUpdate
	endif
endfunction

function! <SID>Save(bang, type, ...)
	let file = a:type == "qf" ? g:editqf_saveqf_filename : g:editqf_saveloc_filename
	if a:0 > 0
		let file = a:1
	endif
	let file = expand(file)

	if (filewritable(fnameescape(file)) == 1 && a:bang == '!') || filereadable(fnameescape(file)) == 0
		let items = []
		let winnr = a:type == 'qf' ? '' : 0
		for i in s:Getlist(winnr, a:type)
			let pattern = i.pattern
			if pattern != '' && len(pattern) >= 5
				let pattern = pattern[3:-3]
			else
				let pattern = '3MPT1'
			endif
			let type = i.type
			if i.type == ''
				let type = 'E'
			endif
			call add(items, bufname(i.bufnr) . ':' . type . ':' . i.lnum . ':' . i.col . ':' . pattern . ':' . i.text)
		endfor
		call writefile(items, fnameescape(file))
	else
		echomsg "File exists (add ! to override) " . file
	endif
endfunction

function! <SID>Load(bang, type, ...)
	let file = g:editqf_saveqf_filename
	let get = a:type == "qf" ? "caddfile" : "laddfile"
	if a:bang == '!'
		let get = a:type == "qf" ? "cgetfile" : "lgetfile"
	endif

	if a:0 > 0
		let file = a:1
	endif
	let file = expand(file)

	if filereadable(fnameescape(file)) == 1
			let tmp_efm = &efm
			" set efm to the format used to store errors in a file
			set efm=%f:%t:%l:%c:%s:%m
			exec get." ".fnameescape(file)
			let &efm=tmp_efm

			call s:RemoveEmptyPattern(0, a:type)
	else
		echomsg "Unable to open file " . file
	endif

	if exists(':HierUpdate')
		HierUpdate
	endif
endfunction

function! <SID>Cleanup(loadqf)
	if ! exists("s:current_bufnr") || ! bufexists(s:current_bufnr)
		return
	endif
	let get = s:current_type == 'qf' ? 'cgetbuffer' : 'lgetbuffer'

	" delete every empty line - empty lines cause empty entries in quickfix
	" list
	silent! g/^\s*$/d
	silent! g/^bufnr:/d

	let empty_list = 0
	if getline(1) == ""
		let empty_list = 1
		call s:Setlist(s:current_winnr, s:current_type, [], 'r')
	endif

	if a:loadqf == 0
		" close quickfix window
		silent! au! qfbuffer
		exec "bw! ".s:current_bufnr
		if empty_list == 0 && g:editqf_jump_to_error == 1
			if s:current_type == "qf"
				exec "cc ".s:current_error
			else
				exec "ll ".s:current_error
			endif
		else
			cclose
		endif
		unlet! s:current_winnr s:current_bufnr s:current_error s:current_type
	else
		" read new quickfix data
		if empty_list == 0
			let tmp_efm = &efm
			" set efm to the format used to store errors in a file
			set efm=%f:%t:%l:%c:%s:%m
			exec get." ".s:current_bufnr

			call s:RemoveEmptyPattern(s:current_winnr, s:current_type)
			let &efm=tmp_efm
		endif
		" prepend column information again
		call append(0, ['bufnr:type:lnum:col:pattern:text'])
		set nomodified
	endif

	if exists(':HierUpdate')
		HierUpdate
	endif
endfunction

function! <SID>Edit()
	let line = line(".")
	let col = col(".") - 1

	" remember the current error jump to it once editing finished
	let s:current_error = line
	" increase line by one, because the Read function adds a description at
	" the top
	let line += 1

	" remember the current window number. It's used to update the location
	" list later on
	let s:current_winnr = winnr()

	" WARNING: unfortunately it's not possible to determine the difference
	" between quickfix and location list from within vim script. When editing
	" a location list the quickfix list is edited instead
	let type = 'qf'

	" split a new window and open quickfix/location list for editing
	exec "silent! ".winheight(0)."sp"
	if type == "qf"
		e qf:list
	else
		e loc:list
	endif

	" move cursor to the column it was in when editing started in the quickfix window
	exec "normal ".line."G0" . col . "l"
endfunction

function! <SID>Read(fname)
	let items = ['bufnr:type:lnum:col:pattern:text']
	let type = 'qf'
	if fnamemodify(a:fname, ':t') == 'loc:list'
		let type = 'loc'
	endif

	let s:current_error = ! exists('s:current_error') ? 1 : s:current_error
	let s:current_winnr = ! exists('s:current_winnr') ? 0 : s:current_winnr
	let s:current_bufnr = bufnr("")
	let s:current_type = type

	" workaround for difficulties handling pattern and line number
	" matches together
	for i in s:Getlist(s:current_winnr, s:current_type)
		let pattern = i.pattern
		if pattern != '' && len(pattern) >= 5
			let pattern = pattern[3:-3]
		else
			let pattern = '3MPT1'
		endif
		let type = i.type
		if i.type == ''
			let type = 'E'
		endif
		call add(items, bufname(i.bufnr) . ':' . type . ':' . i.lnum . ':' . i.col . ':' . pattern . ':' . i.text)
	endfor
	call append(0, items)
	normal Gdd

	augroup qfbuffer
		au!
		au BufWriteCmd <buffer> call <SID>Cleanup(1)
		au BufLeave <buffer> call <SID>Cleanup(0)
	augroup END

	" prevent text from being wrapped
	setlocal tw=0 fo-=trwnaocl
endfunction

function! <SID>ChangeType(type)
	" change the type of the quickfix entry the cursor is currently on
	let l:line = line(".")
	let l:col = col(".") - 1
	let l:qf = getqflist()

	if len(l:qf) < l:line
		return
	endif

	" change type of current error
	let l:qf[l:line - 1]['type'] = a:type
	call setqflist(l:qf, 'r')
	exec "normal ".l:line."G0" . l:col . "l"
endfunction

augroup qf
	au!
	au BufReadCmd qf:list call <SID>Read(expand("<amatch>"))
	au BufReadCmd loc:list call <SID>Read(expand("<amatch>"))
	for i in ["I", "W", "E"]
		exec "au BufReadPost quickfix nnoremap <silent> <buffer> ".i." :call <SID>ChangeType('".i."')<CR>"
	endfor
	for i in ["i", "a", "c", "o", "p", "r", "s", "d", "x", "A", "C", "O", "P", "R", "S", "D", "X"]
		exec "au BufReadPost quickfix nnoremap <silent> <buffer> ".i." :if !exists('s:current_bufnr')<Bar>call <SID>Edit()<Bar>endif<CR>"
	endfor
augroup END
