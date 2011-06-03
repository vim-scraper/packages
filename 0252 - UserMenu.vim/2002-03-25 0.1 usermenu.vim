" ===========================================================================
" usermenu.vim
" 
" Author: Mikolaj Machowski
" Date: 24.03.2002
" Description:
" For gvim.
" Registers are good for executing vim commands but not perfect for inserting
" longer parts of normal text. This menu for gvim help you easy creating,
" saving end editing eg. headers for various .tex and .html files.
"
" Installation:
" NOTE: libList.vim is necessary. It is great library for handling lists.
" http://vim.sourceforge.net/scripts/script.php?script_id=______
"
" 1. Simplest way is to put this file into your plugin directory or source it
" from vimrc (or gvimrc) file. 
" 2. Also you need create special directories where
" your texts will be stored. In default configuration it is:
" $HOME/.vim/usermenu/ (for unices).
" 3. Configuration:
" Path to libList.vim . It isn't necessary when libList.vim is in your plugin
" directory.
source $HOME/.vim/scripts/libList.vim

" Path to usermenu (for unices)
let s:path = "$HOME/.vim/usermenu/"

" Name of the folder with `macros'
let s:localpath = "usermenu"

" Action. Default is read, but maybe you want to change that (eg. source).
let s:action = "read"

function! UserMenu0()

  if exists("b:m_menu")
    aunmenu UserMenu 
  endif
  let b:m_menu = 1
let m_list = ""
let nu_m_list = ""
let m_list = glob(s:path . "*")

let m_list = substitute(m_list, "\n", ",", "g")
let nu_m_list = GetListCount(m_list)
let basic_nu_m_list = 0

amenu 9000.10 &UserMenu.&New :call UserMenuNew()<cr>

while basic_nu_m_list < nu_m_list
  exe "amenu 9000.20 &UserMenu.&Edit.&" . basic_nu_m_list .
        \ " :call UserMenuEdit('".GetListItem(m_list, basic_nu_m_list)."')<cr>"
  let basic_nu_m_list = basic_nu_m_list + 1
endwhile

amenu 9000.30 &UserMenu.&Save :call UserMenuSave()<cr>

let basic_nu_m_list = 0

while basic_nu_m_list < nu_m_list
  exe "amenu 9000.40 &UserMenu.&Delete.&" . basic_nu_m_list .
        \ " :call UserMenuDelete('".GetListItem(m_list, basic_nu_m_list)."')<cr>"
  let basic_nu_m_list = basic_nu_m_list + 1
endwhile

amenu 9000.50 &UserMenu.-sep1- :

let basic_nu_m_list = 0

while basic_nu_m_list < nu_m_list
 exe "amenu 9000.60 &UserMenu.&" . basic_nu_m_list . ":<Tab>" .
  \ substitute(substitute(GetListItem(m_list, basic_nu_m_list), "^\/.*\/", "", ""), "\\.", "\\\\.", "g")
  \ . " :call UserMenu('".GetListItem(m_list, basic_nu_m_list)."')<cr>"
 let basic_nu_m_list = basic_nu_m_list + 1
endwhile

endfunction

call UserMenu0()

function! UserMenuNew()
  let macro_name = inputdialog("New macro has name\n(please don't use spaces):")
  if macro_name != ""
   exe "split " s:path . macro_name
  endif
endfunction

function! UserMenuEdit(file_name)
  exe "split " a:file_name
endfunction

function! UserMenuSave()
  if &confirm == 0
    let b:conf = "1"
  endif
      set confirm
  if expand("%:p") =~ s:localpath
    set nobackup 
    write
    set backup
    call UserMenu0()
    quit
  else
    call confirm("This is not UserMenu file", "&Cancel")
  endif
  if exists("b:conf")
    set noconfirm
    unlet b:conf
  endif
endfunction

function! UserMenuDelete(file_name)
  exe "!rm -f " . a:file_name
  call UserMenu0()
endfunction

function! UserMenu(file_name)
  exe s:action . " " . a:file_name 
endfunction
