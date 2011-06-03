"
" Filename: cream-statusline-prototype.vim
" Updated:  2004-09-29 23:41:36-0400
"
" Cream -- An easy-to-use configuration of the famous Vim text editor.
" <http://cream.sourceforge.net> (C) 2002-2004 Steve Hall
"
" License:
"
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
" <http://www.gnu.org/licenses/gpl.html>
"
" This program is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
" General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
" 02111-1307, USA.
"
" Description:
"
" Toggles the display of the current function prototype on the
" statusline with key <F8>. This is a basic illustration of how to
" write code to accomplish this...feel free to embellish!
"
" Installation:
"
" Dependent on the taglist.vim plugin by Yegappan Lakshmanan, found at
" <http://www.geocities.com/yegappan/taglist> and
" <http://vim.sourceforge.net/scripts/script.php?script_id=273>
"
" Just drop taglist.vim and this file into your plugins directory and
" (re)start Vim.
"
" Usage:
"
" By default, the <F8> key will toggle the display of the current
" prototype in the statusline. The prototype is recalculated each time
" it is toggled ON.
"

function! Cream_statusline_prototype_toggle()
" Toggle current prototype with the current statusline. Recalculate
" when turned on.

    " make sure statusline is on
    set laststatus=2

	if !exists("g:save_statusline")
		" save current statusline
		let g:save_statusline = &statusline
		" recalculate current prototype
		let g:cream_prototype = Cream_prototype()
		" set to prototype
		execute 'set statusline=PROTOTYPE:\ ' . s:Cream_statusline()
	else
		" set to saved statusline
		execute "set statusline=" . g:save_statusline
		" remove global
		unlet g:save_statusline
	endif

endfunction
" Keyboard shortcut: "F8"
nmap <silent> <F8>      :call Cream_statusline_prototype_toggle()<CR>
imap <silent> <F8> <C-o>:call Cream_statusline_prototype_toggle()<CR>
vmap <silent> <F8> :<C-u>call Cream_statusline_prototype_toggle()<CR>
" Commandline command "Stlp"
command! -nargs=0 Stlp call Cream_statusline_prototype_toggle()

function! Cream_statusline_global()
" Return the value of the prototype global.

    " Note: We once wrote code here to try and count repaints and
    " auto-update, but the statusline does not permit reading of
    " buffers so it would fail on any external jump.

    " return the global var
	return g:cream_prototype

endfunction

function! s:Cream_statusline()
" Return a statusline string containing call to function returning the
" globally-defined prototype.
"
" Note: We display a global var rather than directly calling a
" calculating function to avoid file jumping every time the statusline
" is re-painted (continuously).

	let mystl = ""
	" add customizations *before* prototype
	let mystl = mystl . ""
	" prototype
	let mystl = mystl . "%{Cream_statusline_global()}"
	" add customizations *after* prototype
	let mystl = mystl . ""
	return mystl

endfunction


function! Cream_prototype()
" Return the prototype of the tag under cursor.

    " back up behind a parenthesis to check
    normal h

	" unless we are at the very end of the line, we need to go back in
	" order to find the last word typed.
	if virtcol('.') != virtcol('$')
		normal! h
		let myword = expand('<cword>')
		normal! l
	else
		let myword = expand('<cword>')
	endif

	" try remote function prototype via ctag
	let mytag = expand("<cword>")
    " fix position
    normal l

	" remember pos and file
	let myvcol = virtcol('.')
	let myline = line('.')
	let mybufnr = bufnr("%")
	let mypos = s:Pos()
	" jump to function definition via tag
	execute "silent tag " . mytag
	" if we moved
	if  myvcol != virtcol('.')
	\|| myline != line('.')
	\|| mybufnr != bufnr("%")
		" get prototype
		let myprototype = s:Tlist_prototype_get()
		" jump back
		silent! pop
		" restore position
		execute mypos
		redraw
		return myprototype
	endif

	" try current function prototype
	let myprototype = s:Tlist_prototype_get()
	if myprototype != ""
		return myprototype . " (current) "
	endif

    "" dialog no info available
    "call confirm(
    "    \ "No information available for this word.\n" .
    "    \ "\n", "&Ok", 1, "Info")

	return "(none found)"

endfunction

function! s:Tlist_prototype_get()
" use taglist.vim to return a protype defined in the current file.
	call Tlist_Update_File_Tags(fnamemodify(expand("%"), ":p"), &ft)
	return Tlist_Get_Tag_Prototype_By_Line()
endfunction

function! s:Pos(...)
" return current position in the form of an executable command
" Origins: Benji Fisher's foo.vim, available at
"          http://vim.sourceforge.net
	" current pos
	let curpos = line(".") . "G" . virtcol(".") . "|"
	" mark statement
	let mymark = "normal "
	" go to screen top
	normal H
	let mymark = mymark . line(".") . "G"
	" go to screen bottom
	normal L
	let mymark = mymark . line(".") . "G"
	" go back to curpos
	execute "normal " . curpos
	" cat top/bottom screen marks to curpos
	let mymark = mymark . curpos
	execute mymark
	return mymark
endfunction

" when sourced the first time, turn on
if !exists("g:cream_prototype")
    call Cream_statusline_prototype_toggle()
endif

