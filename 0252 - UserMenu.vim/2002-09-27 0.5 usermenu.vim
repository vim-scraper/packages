" Author: Mikolaj Machowski
" Last change: pi± wrz 27 10:00  2002 C
" Description:
" Version: 0.6 
" For gvim and it will load only if has("gui_running") is true.
"  
" Registers are good for executing vim commands but not perfect for inserting
" longer parts of normal text. This menu for gvim help you easy creating,
" saving end editing eg. headers for various .tex and .html files.
"
" Installation:
" NOTE: libList.vim by Gontran Baerts is necessary. It is great library for
" handling lists.
" http://vim.sourceforge.net/scripts/script.php?script_id=______
"
" 1. Simplest way is to put this file into your plugin directory or source it
" from gvimrc (or vimrc) file. 
" 2. Also, you need create special directories where
" your texts will be stored. In default configuration it is:
" $HOME/.vim/usermenu/ (for unices).
" 3. Configuration:
" Path to libList.vim . It is not necessary when libList is in your plugin
" directory (default).

" source libList.vim

if !has("gui_running") | finish | endif

if exists("b:loaded_usermenu") | finish | endif
	let b:loaded_usermenu = 1

let s:cpo_save = &cpo
set cpo&vim

" Path to directory with usermenu `macros'
let s:path = "$HOME/.vim/usermenu/"

" Name of the folder with `macros'
let s:localpath = "usermenu"

" Action. Default is read, but maybe you want to change that (source).
let s:action = "read"

function! s:UserMenu0()
	if exists("b:m_menu")
		aunmenu UserMenu 
	endif
	let b:m_menu = 1
	let b_list = ""
	let nu_m_list = ""
	let b_list = glob(s:path . "*")

	let m_list = substitute(b_list, "\n", ",", "g")
	let nu_m_list = GetListCount(m_list)
	let basic_nu_m_list = 0

	amenu 9000.10 &UserMenu.&New :call <sid>UserMenuNew()<cr>
	amenu 9000.30 &UserMenu.&Save :call <sid>UserMenuSave()<cr>
	amenu 9000.50 &UserMenu.-sep1- :

	while basic_nu_m_list < nu_m_list
		let curr_item = GetListItem(m_list, basic_nu_m_list)
		exe "amenu 9000.20 &UserMenu.&Edit.&" .basic_nu_m_list." :call <sid>UserMenuEdit('".curr_item."')<cr>"
		exe "amenu 9000.40 &UserMenu.&Delete.&" .basic_nu_m_list." :call <sid>UserMenuDelete('".curr_item."')<cr>"
		let um_item = fnamemodify(curr_item, ":p:t:r")
		exe "amenu 9000.60 &UserMenu.&".basic_nu_m_list.":<Tab>".um_item." :call <sid>UserMenu('".curr_item."')<cr>"
		let basic_nu_m_list = basic_nu_m_list + 1
	endwhile

endfunction

call <sid>UserMenu0()

function! s:UserMenuNew()
	let macro_name = inputdialog("New macro has name\n(please don't use spaces):")
	if macro_name != ""
		exe "split " s:path . macro_name
		lcd
	endif
endfunction

function! s:UserMenuEdit(file_name)
	exe "split " a:file_name
	lcd
endfunction

function! s:UserMenuSave()
	let confirm_value = &confirm
	set confirm
	if expand("%:p") =~ s:localpath
		let backup_value = &backup
		set nobackup 
		write
		let &backup = backup_value
		call <sid>UserMenu0()
		quit
	else
		call confirm("This is not UserMenu file", "&Cancel")
	endif
	let &confirm = confirm_value
endfunction

function! s:UserMenuDelete(file_name)
	call delete(a:file_name)
	call <sid>UserMenu0()
endfunction

function! s:UserMenu(file_name)
	exe s:action . " " . a:file_name 
endfunction
let &cpo = s:cpo_save

