" author: Johannes Richter <joe[underscore]r[at]gmx[dot]de>
"
" The BufMenuToo Plugin makes the Buffer Menu  a constant size for
" use as tearoff menu. Now it can permanently stay open to show your 
" buffers and still keeps a minimum size.
" version 0.0.1
"
" INCOMPATIBLE with MiniBufExplorer
"
"TODO:
" 	find a way to focus the Vim window again! - maybe an command-line tool
" 	(if anybody know's how to do this - drop me a note)
" 	make dummy entrys transparent
" 	full path or filename only
" 	different sorting
"

let s:MenuName = "Buffers"
let s:buffer_count = 1
let s:buffer_min = 14
let s:empty_buf_list = ['______________0', '______________1', '______________2', '______________3', '______________4', '______________5', '______________6', '______________7', '______________8', '______________9', '_____________10', '_____________11', '_____________12', '_____________13', '_____________14', '_____________15', '_____________16', '_____________17', '_____________18', '_____________19', '_____________20' ]


" can be called with echo FUNCNAME

function! s:Inc_menu_ent()
	let s:buffer_count -= 1
	if s:buffer_count <= s:buffer_min
		let i = s:empty_buf_list[s:buffer_min - s:buffer_count]
		let str = "menu ".s:MenuName.".".i." :<CR>"
"		echo str
		exec str
	endif
endfunction

function! s:Dec_menu_ent()
	if s:buffer_count <= s:buffer_min
		let i = s:empty_buf_list[s:buffer_min - s:buffer_count]
		let str = "unmenu ".s:MenuName.".".i
"		echo str
		exec str
	endif
	let s:buffer_count += 1	
endfunction

function! s:DrawBufferMenu()
		for i in s:empty_buf_list[0:s:buffer_min - s:buffer_count]
			let str = "menu ".s:MenuName.".".i." :<CR>"
"			echo str
			exec str
		endfor
endfunction	

function! s:BufMenuStartup()
	if !exists("s:bufmenu_loaded")
		let s:bufmenu_loaded = 1
		augroup BufMenuToo
			au!
			au BufDelete,BufFilePre * call s:Inc_menu_ent()
			au BufCreate,BufFilePost * call s:Dec_menu_ent()
		augroup END

		call s:DrawBufferMenu()
	endif
endfunc

"delay Startup till all other initialisation is done
au VimEnter * call s:BufMenuStartup()




