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

if ! exists('g:aw_stop_word')
    let g:aw_stop_word = ""
endif

let loaded_workspace_alternate_file = 1

command! AWEditMirrorFile call <SID>EditMirrorFile(expand("%:p"))
command! AWDiffMirrorFile call <SID>DiffMirrorFile(expand("%:p"))
command! AWAutoSetChild call s:AutoSetChild(expand("%:p"))
command! AWAutoSetParent call s:AutoSetParent(expand("%:p"))
command! -complete=dir AWSetChild call <SID>AW_SetChild()
command! -complete=dir AWSetParent call <SID>AW_SetParent()

function! s:ErrorMsg(message)
    echohl WarningMsg | echo a:message | echohl None
endfunction

function! s:CommentMsg(message)
    echohl Comment | echo a:message | echohl None
endfunction

function! s:CutPath(file_name, stop_word)
    if a:stop_word == ""
        call s:ErrorMsg("AW stop word is empty")
        return ""
    endif

    let file_name = substitute(a:file_name, '\\', '/', 'g')
    let file_name = substitute(file_name, '/$', '', '')

    let index = stridx(file_name, a:stop_word)
    return strpart(file_name, 0, index)
endfunction

function! s:AutoSetChild(file_name)
    let path = s:CutPath( a:file_name, g:aw_stop_word )

    if path == ""
        call s:ErrorMsg( "Couldn't generate path" )
        return
    endif

    let g:aw_child_path = path
    call s:CommentMsg( "Child path:".path )
endfunction

function! s:AutoSetParent(file_name)
    let path = s:CutPath( a:file_name, g:aw_stop_word )

    if path == ""
        call s:ErrorMsg( "Couldn't generate path" )
        return
    endif

    let g:aw_parent_path = path
    call s:CommentMsg( "Parent path:".path )
endfunction

" return how many times search pattern match (from the beginig of file)
function! s:Search(line_number) 
	let pattern = getline(a:line_number)
	let pattern = <SID>EscapePattern(pattern)
	let pattern = '^'.pattern.'$'
	exec ':0'
	let is_found = search(pattern,'c')
	if is_found == 0
		return 0
	endif

	let jump_count = 1
	while is_found != a:line_number
		let is_found = search(l:pattern,'')
		let jump_count = jump_count + 1
	endwhile
	return jump_count
endfunction 

" move to N'th pattern match 
function! s:JumpTo(pattern, jump_count)
	exec ':0'
	let flag = 'c'
	let completed_jumps = 0
	let pattern = '^'.a:pattern.'$'
	while completed_jumps < a:jump_count
		let line_number = search(pattern, flag)
		if line_number == 0
			return 0
		endif
		let completed_jumps = completed_jumps + 1
		let flag = ''
	endwhile
        return 1
endfunction 

" escape search pattern
function! s:EscapePattern(line)
	return escape(a:line,'.$^|\*[]~')
endfunction 

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
			call s:ErrorMsg("Not workspace's file: ".l:file_name)
		endif
	
	endif

	return l:new_file_name
endfunction

function! <SID>EditMirrorFile(file_name)
	if g:aw_parent_path == g:aw_child_path
	        call s:ErrorMsg("Child and parent directories are same")
		return
	endif
	let l:new_file = <SID>WorkSpaceChangeFileName(g:aw_child_path, g:aw_parent_path, a:file_name)
	if filereadable(l:new_file)
		let line_number = line('.')
                let jumps_count = s:Search(line_number)
                let pattern = getline('.')
                exec "edit ".l:new_file

                let jump_success = s:JumpTo(s:EscapePattern(pattern), jumps_count)
                if ! jump_success
                    exec line_number
                endif

		return
	endif

	call s:ErrorMsg("Alternate file not found : ".l:new_file)
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

	call s:ErrorMsg("Alternate file not found : ".l:new_file)
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
