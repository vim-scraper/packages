"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Purpose: Create a menu with most recently used files
" Author: Rajesh Kallingal <RajeshKallingal@yahoo.com>
" Original Author: ???
" Version: 6.0.4
" Last Modified: Fri May 23 10:49:47 2003
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Description:
" 	This plugin will create a MRU menu to keep track of most recently visited
" 	buffers. You can customize Menu Label, menu size and whether to show
" 	numbered or non-numbered menu
"
"	Global Variables Used:
" 	Make sure you have added '!' in 'viminfo' option to save global valiables
"		MRU_BUFFERS - keeps the lsit of recent buffers, usedto build the menu
"		MRU_MENUSIZE - maximum entries to keep in the list
"		MRU_HOTKEYS - whether to have a hot key of 0-9, A-Z in the menu
"		MRU_LABEL - menu name to use, default is 'M&RU'
"
"	Excludes:
"		help files (not working)
"
" Installation:
" 	Just drop it this file in your plugin folder/directory.
"
" TODO:
" 	- Add different sort/group options (group by file type, or folder)
" 	- handling sessions	
" 	- help document
"
" vim ts=4 : sw=4 : tw=0
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Do not load, if already been loaded
if exists("g:loaded_mrumenu")
	finish
endif

let g:loaded_mrumenu = 1

" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

"+++ Cream uses "]"
let s:sep = "]"
let s:sep = "\377"
"+++

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Variables
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" following are the default values for the script variables used to control
" the script behaviour. The value will be remembered for the future sessions

" store file name to use in the menu
let s:script_file = expand ('<sfile>:p')
" The menu label to be used, default value
let s:mru_label = 'M&RU'
" default value
let s:mru_menusize = 10
" set this to 1 if you want to have 0-9, A-Z as the hotkeys in the mru list, set to 0 for the default file name
let s:mru_hotkeys = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! MRUInitialize()

	call MRUInitializeGlobals ()
	call MRURefreshMenu ()

endfunction

function! MRUInitializeGlobals()
	" Initialize the global variables with defaults if they are not stored in
	" viminfo.

	" Do not intialize, if already been intialized
	if exists("s:initialized_globals")
		return
	endif
	let s:initialized_globals=1

	if exists('g:MRU_LABEL')
		"name of the menu
		let s:mru_label = g:MRU_LABEL
		unlet g:MRU_LABEL
	endif

	if exists('g:MRU_MENUSIZE')
		" maximum number of entries to remember
		let s:mru_menusize = g:MRU_MENUSIZE
		unlet g:MRU_MENUSIZE
	endif

	if exists('g:MRU_HOTKEYS')
		" whether or not to use hot keys 0-9, A-Z for menu items
		let s:mru_hotkeys = g:MRU_HOTKEYS
		unlet g:MRU_HOTKEYS
	endif

	if exists('g:MRU_BUFFERS')
		" list of recent buffers
		let s:mru_buffers = g:MRU_BUFFERS
"		echo strlen (s:mru_buffers)
		unlet g:MRU_BUFFERS
	else
		let s:mru_buffers = ""
	endif

"	This is not required as vim handles really long global variables
"	if !exists('g:MRU_BUFFERS')
"		call MRUJoinBufList ()
"	endif
"
	let s:mru_count = 0
	
endfunction

function! MRURefreshMenu()
	" This function will recreate the menu entries from s:mru_buffers variable

	" remove the MRU Menu
	execute 'anoremenu <silent> ' . s:mru_label . '.x x'
	execute 'silent! aunmenu ' . s:mru_label

	" use this variable to keep the list of entries in the menu so far
	let l:menu_list = ''

	let l:list = s:mru_buffers
	let s:mru_count = 0
	while l:list != "" && s:mru_count < s:mru_menusize
		" put a separator after every 10 entries
		if s:mru_count % 10 == 0 && s:mru_count / 10 > 0
			execute 'amenu ' . s:mru_label . '.-sep' . s:mru_count . '- <NUL>'
		endif
		let l:entry_length = match(l:list, s:sep)
		if l:entry_length >= 0
			let l:fullpath = strpart(l:list, 0, l:entry_length)
			let l:list = strpart(l:list, l:entry_length + 1, strlen(l:list))
		else
			let l:fullpath = l:list
			let l:list = ""
		endif

		if l:fullpath != ""
			let l:filename = fnamemodify(l:fullpath, ':t')

			"+++ Cream: handle file names with tilde characters in matching through list
			let l:filename = escape(l:filename, "~")
			"+++

			" append a space to the filename to enable multiple entries in menu
			" which has the same filename but different path
			while ( match(l:menu_list, '\<' . l:filename . "\/") >= 0)
				let l:filename = l:filename . ' '
			endwhile

			let l:menu_list = l:menu_list . l:filename . "\/"

			"+++ Cream: un-escape tilde characters back so menu shows correctly
			let l:filename = substitute(l:filename, '\\\~', '\~', 'g')
			"+++

			let s:mru_count = s:mru_count + 1
			call MRUAddToMenu (l:fullpath, l:filename)
		endif

	endwhile

	call MRUAddOptionMenu ()

endfunction

function! MRUAddToMenu (fullpath, filename)
	" Add the entry to the menu

	let l:menu_entry = a:filename . "\t" . fnamemodify(a:fullpath,':h')
	let l:menu_entry = escape(l:menu_entry, '\\. 	|\~')
	let l:editfilename = escape(a:fullpath, '\\. 	|\~')
    "incase there is an & in the filename
	if      has("win32")
		\|| has("win16")
		\|| has("win95")
		\|| has("dos16")
		\|| has("dos32")
    	let l:menu_entry = substitute (l:menu_entry, '&', '&&', 'g')
		let l:editfilename = substitute (l:editfilename, '&', '&&', 'g')
	endif

	let l:menu_command = ' :call MRUEditFile("' . l:editfilename . '")<cr>'
	let l:tmenu_text = 'Edit File ' . a:fullpath
	if exists ("s:mru_hotkeys") && s:mru_hotkeys == 1
		 " use hot keys 0-9, A-Z
		 if s:mru_count <= 10
			let l:alt_key = s:mru_count - 1
		else
			let l:alt_key = nr2char (s:mru_count + 54) "start with A at 65
		endif
		execute 'anoremenu <silent> ' . s:mru_label . '.' . l:alt_key . '\.\ ' . l:menu_entry . l:menu_command
		execute 'tmenu <silent> ' . s:mru_label . '.' . l:alt_key . '\.\ ' . substitute (escape (a:filename, '\\. 	|\~'), '&', '&&', 'g') . ' ' . l:tmenu_text
	else
		execute 'anoremenu <silent> ' . s:mru_label . '.' . l:menu_entry . l:menu_command
		execute 'tmenu <silent> ' . s:mru_label . '.' . substitute (escape (a:filename, '\\. 	|\~'), '&', '&&', 'g') . ' ' . l:tmenu_text
	endif
endfunction

function! MRUEditFile(filename)
	" edit or go to the buffer 

	if bufloaded (a:filename)
		silent execute 'buffer ' . a:filename
	else
		silent execute 'edit ' . a:filename
	endif
endfunction

function! MRUAddOptionMenu()
" Add the Option menu entries
	
	execute 'anoremenu <silent> ' . s:mru_label . '.-sep0- <NUL>'
	if exists ("s:mru_hotkeys") && s:mru_hotkeys == 1
		execute 'anoremenu <silent> ' . s:mru_label . '.Options.Non\ Numbered\ Menu :call MRUToggleHotKey()<CR>'
		execute 'tmenu <silent> ' . s:mru_label . '.Options.Non\ Numbered\ Menu Remove sequential number/alphabet from the menu entry'
	else
		execute 'anoremenu <silent> ' . s:mru_label . '.Options.Numbered\ Menu :call MRUToggleHotKey()<CR>'
		execute 'tmenu <silent> ' . s:mru_label . '.Options.Numbered\ Menu Add a sequential number/alphabet to the menu entry (0-9/A-Z)'
	endif

	execute 'anoremenu <silent> ' . s:mru_label . '.Options.Set\ Menu\ Size :call MRUSetMenuSize()<CR>'
	execute 'tmenu <silent>' . s:mru_label . '.Options.Set\ Menu\ Size Allows you to change the number of entries in menu.'
	execute 'anoremenu <silent> ' . s:mru_label . '.Options.Rename\ Menu :call MRUSetMenuLabel()<CR>'
	execute 'tmenu <silent>' . s:mru_label . '.Options.Rename\ Menu Allows you to rename the Top Menu Name'

	execute 'anoremenu <silent> ' . s:mru_label . '.Options.-sep0- <NUL>'
	execute 'anoremenu <silent> ' . s:mru_label . '.Options.Remove\ Invalid :call MRURemoveInvalid()<CR>'
	execute 'tmenu <silent>' . s:mru_label . '.Options.Remove\ Invalid Removes files no longer exists from the list'
	execute 'anoremenu <silent> ' . s:mru_label . '.Options.Clear\ List :call MRUClearList()<CR>'
	execute 'tmenu <silent>' . s:mru_label . '.Options.Clear\ List Removes all the entries from this menu.'
	execute 'anoremenu <silent> ' . s:mru_label . '.Options.Customize		:edit ' . s:script_file . '<CR>'
	execute 'tmenu <silent>' . s:mru_label . '.Options.Customize Edit the code behind MRU Menu'

endfunction

function! MRUAddToList()
	" add current buffer to list of recent travellers.  Remove oldest if
	" bigger than MRU_MENUSIZE

	" incase vim is started with drag & drop
	call MRUInitializeGlobals ()

	let l:filename = expand("<afile>:p")

	"+++ Cream: fix exclusion of help files
	" Exclude following files/types/folders
	"if &filetype == 'help'
	"	" do not add help files to the list
	"	return	
	"endif
	if getbufvar(l:filename, "&filetype") == "help"
		return	
	endif
	"+++

"	if exists("g:spooldir") && l:filename =~ g:spooldir
"		" do not add spooled files to the list
"		return	
"	endif

	if l:filename != '' && filereadable (expand ("<afile>"))
		" Remove the current file entry from MRU_BUFFERS
		let s:mru_buffers = substitute(s:mru_buffers, escape(l:filename,'\\~'). s:sep, '', 'g')
		" Add current file as the first in MRU_BUFFERS list
		let s:mru_buffers = l:filename . s:sep . s:mru_buffers

		" Remove oldest entry if > MRU_MENUSIZE
		if s:mru_count > s:mru_menusize
			let l:trash = substitute(s:mru_buffers, s:sep, "^^", "g")
			let l:trash = matchstr(l:trash, '\([^^^]*^^\)\{'.s:mru_menusize.'\}')
			let s:mru_buffers = substitute(l:trash, "^^", s:sep, "g")
		endif
		call MRURefreshMenu()
	endif

endfunction


" Customizing Options

function! MRUClearList()
	" Clear the MRU List
	let l:choice = confirm("Are you sure you want to clear the list?", "&Yes\n&No", 2, "Question")

	if l:choice != 1
		return
	endif
	let s:mru_buffers = ''
	let s:mru_count = 0
	call MRURefreshMenu ()

endfunction


function! MRUToggleHotKey()
	if s:mru_hotkeys == 1
		let s:mru_hotkeys = 0
	else
		let s:mru_hotkeys = 1
	endif
	call MRURefreshMenu ()
"	call MRUDisplayMenu ()
endfunction

function! MRUSetMenuLabel()
	execute 'let l:menu_label = input ("Enter Menu Label [' . s:mru_label . ']: ")'

	if l:menu_label != ""
		" remove current MRU Menu
		execute 'anoremenu <silent> ' . s:mru_label . '.x x'
		execute 'silent! aunmenu ' . s:mru_label

		let s:mru_label = l:menu_label
		call MRURefreshMenu ()
	endif
endfunction

function! MRUSetMenuSize()
	execute 'let l:menu_size = input ("Enter Menu Size [' . s:mru_menusize . ']: ")'

	if l:menu_size != ""
		let s:mru_menusize = l:menu_size
		call MRURefreshMenu ()
	endif

"	call MRUDisplayMenu ()
endfunction

function! MRURemoveInvalid()
	" Remove non existing files from the menu

	let l:list = s:mru_buffers
	let l:buf_list = ""
	let l:buf_count = 0
	while l:list != ""
		let l:entry_length = match(l:list, s:sep)
		if l:entry_length >= 0
			let l:fullpath = strpart(l:list, 0, l:entry_length)
			let l:list = strpart(l:list, l:entry_length+1, strlen(l:list))
		else
			let l:fullpath = l:list
			let l:list = ""
		endif
		if filereadable (l:fullpath)
			if l:buf_count == 0
				let l:buf_list = l:fullpath
			else
				let l:buf_list = l:buf_list . s:sep . l:fullpath
			endif
			let l:buf_count = l:buf_count + 1
		endif
	endwhile

	let s:mru_buffers = l:buf_list
	let s:mru_count = l:buf_count
	call MRURefreshMenu()
"	call MRUDisplayMenu ()

endfunction


function! MRUVimLeavePre()
	let g:MRU_LABEL = s:mru_label
	let g:MRU_MENUSIZE = s:mru_menusize
	let g:MRU_HOTKEYS = s:mru_hotkeys
	if exists ("s:mru_buffers")
		let g:MRU_BUFFERS = s:mru_buffers
	endif
endfunction

augroup MRU
	autocmd!
	autocmd VimEnter * call MRUInitialize()
	"autocmd GUIEnter * call MRUInitialize()
    "+++ Cream: remove redundant events (When buffer not current,
    "           filetype is not able to be determined and excluded
    "           based on type.)
	"autocmd BufDelete,BufEnter,BufWritePost,FileWritePost * call MRUAddToList ()
	autocmd BufEnter,BufWritePost,FileWritePost * call MRUAddToList ()
	"+++
	autocmd VimLeavePre * nested call MRUVimLeavePre()
augroup END


" restore 'cpo'
let &cpo = s:cpo_save
unlet s:cpo_save
