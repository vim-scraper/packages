" File: alternate_workspace
" Author: Nickolay Golubev
" Email: golubev.nikolay@gmail.com
"
" This script provide a way to quickly switch between files from different
" workspaces (Child and Parent). And to see difference between them.
"
" Set variable g:aw_child_path and g:aw_parent_path to work with this script
" Example
" 	let g:aw_child_path = /home/ni/branch
" 	let g:aw_parent_path = /project/

if exists('loaded_workspace_alternate_file')
	finish
endif

if ! exists('g:aw_parent_path')
    let g:aw_parent_path = getcwd()
endif
if ! exists('g:aw_child_path')
    let g:aw_child_path = getcwd()
endif

let loaded_workspace_alternate_file = 1

if !hasmapto('<Plug>AWEditMirrorFile')
	map <unique> <Leader>we <Plug>AWEditMirrorFile
endif

if !hasmapto('<Plug>AWDiffMirrorFile')
	map <unique> <Leader>wd <Plug>AWDiffMirrorFile
endif

nnoremap <unique> <script> <Plug>AWEditMirrorFile :call <SID>EditMirrorFile(expand("%:p"))<CR>
nnoremap <unique> <script> <Plug>AWDiffMirrorFile :call <SID>DiffMirrorFile(expand("%:p"))<CR>
command! -complete=dir AWSetChild call <SID>AW_SetChild()
command! -complete=dir AWSetParent call <SID>AW_SetParent()

function! <SID>WorkSpaceChangeFileName(child_work_space, parent_work_space, file_name)
	let l:child_workspace = substitute(a:child_work_space, '\\', '/', 'g')
	let l:child_workspace = substitute(l:child_workspace, '/$', '', '')
	let l:file_name = substitute(a:file_name, '\\', '/', 'g')

	let l:parent_workspace = substitute(a:parent_work_space, '\\', '/', 'g')
	let l:parent_workspace = substitute(l:parent_workspace, '/$', '', '')

	let l:new_file_name = "!!!"
	if l:file_name =~ l:child_workspace
		let l:new_file_name = substitute(l:file_name, l:child_workspace, l:parent_workspace, '')
	else
		if l:file_name =~ l:parent_workspace
			let l:new_file_name = substitute(l:file_name, l:parent_workspace, l:child_workspace, '')
		else
			echohl WarningMsg | echo "Not workspace's file: ".l:file_name | echohl None
		endif
	
	endif

	return l:new_file_name
endfunction

function! <SID>EditMirrorFile(file_name)
	if g:aw_parent_path == g:aw_child_path
	        echohl WarningMsg | echo "Child and parent directories are same" | echohl None
		return
	endif
	let l:new_file = <SID>WorkSpaceChangeFileName(g:aw_child_path, g:aw_parent_path, a:file_name)
	if filereadable(l:new_file)
		exec "edit ".l:new_file
		return
	endif

	echohl WarningMsg | echo "Alternate file not found : ".l:new_file | echohl None
endfunction

function! <SID>DiffMirrorFile(file_name)
	if g:aw_parent_path == g:aw_child_path
	        echohl WarningMsg | echo "Child and parent directories are same" | echohl None
		return
	endif
	let l:new_file = <SID>WorkSpaceChangeFileName(g:aw_child_path, g:aw_parent_path, a:file_name)
	if filereadable(l:new_file)
		exec "vert diffsplit ".l:new_file
		return
	endif

	echohl WarningMsg | echo "Alternate file not found : ".l:new_file | echohl None
endfunction

function! <SID>AWSetDir(cur_dir, text)
	if a:cur_dir == ''
	    let tmp = getcwd() . "/"
	else
	    let tmp = a:cur_dir
	endif
	let tmp = input("Alternate workspace set " . a:text. ":", tmp, "dir")
	return tmp
endfunction

function! <SID>AW_SetChild()
	let g:aw_child_path = <SID>AWSetDir(g:aw_child_path, "child")
endfunction
function! <SID>AW_SetParent()
	let g:aw_parent_path = <SID>AWSetDir(g:aw_parent_path, "parent")
endfunction
