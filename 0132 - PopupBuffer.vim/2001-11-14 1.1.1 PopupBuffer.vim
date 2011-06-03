" File : PopupBuffer.vim
" Last Change: 2001 Nov 13
" Maintainer: Gontran BAERTS <gbcreation@free.fr>
" Version : 1.1.1
"
" Please don't hesitate to correct my english :)
" Send corrections to gbcreation@free.fr
"
"-----------------------------------------------------------------------------
" Vim global plugin for selecting buffer from PopUp menu
"
"-----------------------------------------------------------------------------
" Normally, this file will reside in your plugins directory and be
" automatically sourced.  If not, you must manually source this file
" using :source PopupBuffer.vim
"
"-----------------------------------------------------------------------------
" Updates in version 1.1.1
" - Fix stupid bug in PBAdd() function. Directories were added to popup buffers
"   menu and never removed.
" Updates in version 1.1
" - Allow to control the menu item format with popupBufferPattern global
"   variable. Use %F for full file path, %f for file name, %p for file path,
"   and %n for buffer number.
"   For example:
"   let g:popupBufferPattern = "Select %f (%n) from %p directory"
" - Fix unremoved deleted buffers from menu
" - Improve addition of new item to menu. Don't add all buffers each time.
"
" Updates in version 1.0
" - First version

" Has this already been loaded ?
if exists("loaded_bufferpopup")
  finish
endif
let loaded_bufferpopup = 1

let s:pbmenu_wait = 1

if !exists("g:popupBufferPattern")
  let g:popupBufferPattern="%f (%n)\t%p"
endif

" Add new buffer to popup menu
function! <SID>PBAdd( fname, bnum )
	if !isdirectory(a:fname)
		exe "amenu PopUp.Buffers." . <SID>PBFormat( a:fname, a:bnum ) . " :b " . a:bnum . "<CR>"
	endif
endfunc

" Remove deleted buffer from popup menu
func! <SID>PBRemove()
	let name = expand("<afile>")
	if !isdirectory(name)
		exe "aunmenu PopUp.Buffers." . <SID>PBFormat(  name, expand("<abuf>") )
	endif
endfunc

" Format menu entry
func! <SID>PBFormat(fname, bnum)
	let name = a:fname
	if name == ''
		let name = "[No File]"
	else
		let name = fnamemodify(name, ':p:~')
	endif

	" Parse popupBufferPattern
	let item = substitute( g:popupBufferPattern, "%F", name, "g" )
	let item = substitute( item, "%f", fnamemodify(name, ':t'), "g" )
	let item = substitute( item, "%p", fnamemodify(name, ':h'), "g" )
	let item = substitute( item, "%n", a:bnum, "g" )

	" Some cleaning
	let item = escape( item, "\\. \t|")
	let item = substitute( item, "\n", "^@", "g")
	return item
endfunc

function! <SID>PBShow()
	" Add current buffers
	let buf = 1
	while buf <= bufnr('$')
		if bufexists(buf) && !getbufvar(buf, "&bufsecret")
			call <SID>PBAdd( bufname(buf), buf )
		endif
		let buf = buf + 1
	endwhile

	" Define autocommands
	augroup buffer_popup
	au!
	au BufCreate,BufFilePost * call <SID>PBAdd( expand("<afile>"), expand("<abuf>") )
	au BufDelete,BufFilePre * call <SID>PBRemove()
	augroup END
endfunction

" Initialization
call <SID>PBShow()
