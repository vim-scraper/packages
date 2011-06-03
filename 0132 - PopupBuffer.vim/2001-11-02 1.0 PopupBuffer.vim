" Vim global plugin for selecting buffer from PopUp menu
" Last Change: 2001 Nov 02
" Maintainer: Gontran BAERTS <gbcreation@free.fr>

"if exists("loaded_bufferpopup")
"  finish
"endif
"let loaded_bufferpopup = 1

let s:bmenu_wait = 1

function! <SID>PBAdd()
	if s:bmenu_wait == 0
		let buf = 1
		while buf <= bufnr('$')
			if bufexists(buf) && !isdirectory(bufname(buf)) && !getbufvar(buf, "&bufsecret")
				exe "amenu PopUp.Buffers." . <SID>PBFormat( bufname(buf), buf ) . " :b " . buf . "<CR>"
			endif
			let buf = buf + 1
		endwhile
	endif
endfunc

func! <SID>PBRemove()
	if s:bmenu_wait == 0
		let name = expand("<afile>")
		if isdirectory(name)
			return
		endif

		exe "aunmenu PopUp.Buffers." . <SID>PBFormat(  name, expand("<abuf>") )
	endif
endfunc

func! <SID>PBFormat(fname, bnum)
	let name = a:fname
	if name == ''
		let name = "[No File]"
	else
		let name = fnamemodify(name, ':p:~')
	endif
	" detach file name and separate it out:
	let name2 = fnamemodify(name, ':t')
	if a:bnum >= 0
		let name2 = name2 . ' (' . a:bnum . ')'
	endif
	let name = name2 . "\t" . fnamemodify(name,':h')
	let name = escape(name, "\\. \t|")
	let name = substitute(name, "\n", "^@", "g")
	return name
endfunc

function! <SID>PBShow()
	augroup buffer_popup
	au!
	au BufCreate,BufFilePost * call <SID>PBAdd()
	au BufDelete,BufFilePre * call <SID>PBRemove()
	augroup END
	let s:bmenu_wait = 0
	call <SID>PBAdd()
endfunction

if has("vim_starting")
	augroup LoadBufferPopup
	au! VimEnter * call <SID>PBShow()
	au  VimEnter * au! LoadBufferPopup
	augroup END
else
	call <SID>PBShow()
endif
