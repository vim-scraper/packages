"======================================================================
" cream-showinvisibles.vim
" 
" Description:
" Indicates "invisible" characters within text, such as tabs, spaces
" and returns. This setup includes nice looking characters, a global
" variable which gets retained if you're using viminfo, and a toggle
" function accessed by a familiar (to some of us) F4 key.
"
" One of the many custom utilities and functions for gVim from the
" Cream project ( http://cream.sourceforge.net ), a configuration of
" Vim in the vein of Apple and Windows software you already know.
"
" Date:    03 Aug 2002
" Version: 1.1
" Source:  http://vim.sourceforge.net/scripts/script.php?script_id=363
" Author:  Steve Hall  [ digitect@mindspring.com ]
" License: GPL (http://www.gnu.org/licenses/gpl.html)
"
" Instructions:
" * Simply copy this file and paste it into your vimrc. Or you can
"   drop the entire file into your plugins directory.
" * As long as you don't already have keyboard mappings to the F4 key,
"   it will toggle invisible characters.
"
" Notes:
" For more information see Vim's ":help 'list",  ":help 'listchars",
" and ":help viminfo".
"
" ChangeLog:
"
" 2002-08-03 -- v.1.1
" * New normal mode mapping and slightly saner visual and insert mode mappings.
"
" 2002-08-03 -- v.1.0
" * Initial Release


" characters used to represent invisibles
set lcs+=tab:»\         " 187 followed by a space (032) (and no, it
                        " doesn't *need* to be escaped with a
                        " backslash for me on Windows or Linux)
set lcs+=eol:¶          " 182
set lcs+=trail:·        " 183
if has("win32")
	set lcs+=extends:…  " 133
	set lcs+=precedes:… " 133
elseif has("unix")
	set lcs+=extends:_  " underscore (095)
	set lcs+=precedes:_ " underscore (095)
endif

" initialize environment
function! List_init()
	if !exists("g:LIST")
		" initially off
		set nolist
		let g:LIST = "0"
	else
		if g:LIST == "1"
			set list
		else
			set nolist
		endif
	endif
endfunction
" initialize on Buffer enter/new
autocmd BufNewFile,BufEnter * call List_init()

" toggle on/off
function! List_toggle()
	if exists("g:LIST")
		if g:LIST == "0"
			set list
			let g:LIST = "1"
		elseif g:LIST == "1"
			set nolist
			let g:LIST = "0"
		endif
	else
		call confirm(
		\"Error: global uninitialized in List_toggle()", "&Ok", 1, "Error")
	endif
endfunction

" keyboard shortcut (use Microsoft Windows-like F4)
imap <silent> <F4> <C-o>:call List_toggle()<CR>
vmap <silent> <F4>      :call List_toggle()<CR>gv
nmap <silent> <F4>      :call List_toggle()<CR>

