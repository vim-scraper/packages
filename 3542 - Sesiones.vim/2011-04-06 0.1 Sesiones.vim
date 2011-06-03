"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" File:  "Sesiones.vim"
" URL:  http://vim.sourceforge.net/script.php?script_id=
" Version: 0.1
" Last Modified: 06/04/2011
" Author: jmpicaza at gmail dot com
" Description: Plugin for managing sesions the easy way
" GetLatestVimScripts: 1823 1 :AutoInstall: Sesiones.vim
" 
" TODO: let customize path where to save the sessions (l:path) from within .vimrc
"
" Installation
" ------------
" 1. Copy the Sesiones.vim script to the $HOME/.vim/plugin or the
"    $HOME/vimfiles/plugin or the $VIM/vimfiles directory.  Refer to the
"    ':help add-plugin', ':help add-global-plugin' and ':help runtimepath'
"    topics for more details about Vim plugins.
"
" Usage
" -----
" Manage it from the menu in plugins or with the keyboard:
" <F3> to save a session
" <S-F3> to load a session
" <C-S-F3> to overwrite an existing session
" d<F3> to delete an existing session
"
"""""""""""""""""""""""""""""""""""""""""""
" Function for writing and loading sessions
"""""""""""""""""""""""""""""""""""""""""""
function! Sesiones(save_load)
	"echomsg "<F3> to save, <S-F3> to load, <C-S-F3> to overwrite, d<F3> to delete"
	" TODO: let customize l:path from within .vimrc
	let l:path=expand($VIM)."/vimfiles/session/".substitute(substitute(expand('%:p').'.vim',"\\","=+","g"),":","=-","")
	if (a:save_load == 0)
		if (filewritable(l:path))
			echohl MoreMsg
			echomsg "Already exists a session for this file. Use <C-S-F3> for Overwriting or <S-F3> to Load."
			echohl None
		else
			exe ":mksession " . l:path
			echohl NonText
			echomsg "Session successfully saved in file: " . l:path
			echohl None
		endif
	elseif (a:save_load == 1)
		if (filereadable(l:path))
			exe "source " . l:path
		else
			echohl ErrorMSG
			echomsg "ERROR: Does not exist a session associated to the file: " . expand('%')
			echohl None
		endif
	elseif (a:save_load == 2)
		if (filewritable(l:path))
			exe ":mksession! " . l:path
			echohl NonText
			echomsg "Session successfully overwritten"
			echohl None
		else
			echohl ErrorMSG
			echomsg "ERROR: Session does not exist. Use <F3> to create it"
			echohl None
		end
	elseif (a:save_load == 99)
		if (delete(l:path))
			echohl ErrorMSG
			echomsg "ERROR: There was not possible to delete the file: " . l:path
			echohl None
		else
			echohl NonText
			echomsg "Session deleted for this file."
			echohl None
		endif
	else
		echohl ErrorMSG
		echomsg "No valid option selected. Please review your code or command"
		echohl None
	endif
endfunction
nnoremap <F3> :call Sesiones(0)<ENTER>
nnoremap <S-F3> :call Sesiones(1)<ENTER>
nnoremap <C-S-F3> :call Sesiones(2)<ENTER>
nnoremap d<F3> :call Sesiones(99)<ENTER>
nmenu &Plugin.Se&ssions.&Save\ Session<Tab><F3>	:call Sesiones(0)<ENTER>
nmenu &Plugin.Se&ssions.&Load\ Session<Tab><S-F3>	:call Sesiones(1)<ENTER>
nmenu &Plugin.Se&ssions.&Overwrite\ Session<Tab><C-S-F3>	:call Sesiones(2)<ENTER>
nmenu &Plugin.Se&ssions.&Delete\ Session<Tab>d<F3>	:call Sesiones(99)<ENTER>
