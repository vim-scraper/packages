" FileMenu.vim: Vim menu for picking Files from menu
" Author: V Singh <vsingh@hymail.com>
" Modified: 2005 Apr 15
" License: Public Domain

if exists("loaded_filemenu")
    finish
endif
let loaded_filemenu = 1

function! s:ReloadMenu()
    "FLAG: First add menu name which you want to associate
    aunmenu CFiles
    amenu CFiles.Reload	:call <SID>ReloadMenu()<CR>
    amenu CFiles.-Sep-	:
    "FLAG: Change here for type of file which you want *.cpp etc etc
    let l:files = globpath(".", "*.c")
    while strlen(l:files) > 0
	let l:newline = stridx(l:files,"\n")
	if l:newline == -1
	    break
	endif
	let l:current = strpart(l:files, 0, l:newline-1)
	let l:files = strpart(l:files, l:newline+1)
	let l:filename = fnamemodify(l:current, ':t:r')
        """"""""""""""""""""""""""
        "FLAG: Dont forget this place for extension and the method edit or spit the screen
	execute ('amenu CFiles.' . l:filename . ' :edit ' . l:filename . '.c <cr>')
        """"""""""""""""""""""""""

    endwhile
endfunction

"FLAG: Change here too
amenu CFiles.dummy :
call s:ReloadMenu()

let s:save_cpo = &cpo
set cpo&vim
let &cpo = s:save_cpo

if exists("loaded_filea51menu")
    finish
endif
let loaded_filea51menu = 1

function! s:ReloadMenu()
    "FLAG: First add menu name which you want to associate
    aunmenu ASM
    amenu ASM.Reload	:call <SID>ReloadMenu()<CR>
    amenu ASM.-Sep-	:
    "FLAG: Change here for type of file which you want *.cpp etc etc
    let l:files = globpath(".", "*.a51")
    while strlen(l:files) > 0
	let l:newline = stridx(l:files,"\n")
	if l:newline == -1
	    break
	endif
	let l:current = strpart(l:files, 0, l:newline-1)
	let l:files = strpart(l:files, l:newline+1)
	let l:filename = fnamemodify(l:current, ':t:r')
        """"""""""""""""""""""""""
        "FLAG: Dont forget this place
	execute ('amenu ASM.' . l:filename . ' :edit ' . l:filename . '.a51 <cr>')
        """"""""""""""""""""""""""

    endwhile
endfunction

"FLAG: Change here too
amenu ASM.dummy :
call s:ReloadMenu()

let s:save_cpo = &cpo
set cpo&vim
let &cpo = s:save_cpo
" Now you are Expert 
if exists("loaded_filehmenu")
    finish
endif
let loaded_filehmenu = 1

function! s:ReloadMenu()
    "FLAG: First add menu name which you want to associate
    aunmenu HFiles
    amenu HFiles.Reload	:call <SID>ReloadMenu()<CR>
    amenu HFiles.-Sep-	:
    "FLAG: Change here for type of file which you want *.cpp etc etc
    let l:files = globpath(".", "*.h")
    while strlen(l:files) > 0
	let l:newline = stridx(l:files,"\n")
	if l:newline == -1
	    break
	endif
	let l:current = strpart(l:files, 0, l:newline-1)
	let l:files = strpart(l:files, l:newline+1)
	let l:filename = fnamemodify(l:current, ':t:r')
        "FLAG: Dont forget this place
	execute ('amenu HFiles.' . l:filename . ' :spl ' . l:filename . '.h <cr>')

    endwhile
endfunction

"FLAG: Change here too
amenu HFiles.dummy :
call s:ReloadMenu()

let s:save_cpo = &cpo
set cpo&vim
let &cpo = s:save_cpo
