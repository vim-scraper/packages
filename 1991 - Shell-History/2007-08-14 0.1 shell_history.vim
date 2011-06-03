"==================================================
" File:         shell_history.vim
" Brief:        shell history autocomplete
" Author:       Michael Brown <michael@ascetinteractive.com> 
" Last Change:  2007-08-13
" Version:      0.1
"
" Install:      1. Put shell_history.vim to plugin
"                  directory.
"
" Usage:
"           hotkey:
"				insert mode map. 
"				control+b is the current default.
"
"           variables:
"
"               g:shellcompletekey
"                   the key used to complete function
"                   parameters and key words.
"
"==================================================

if v:version < 700
    finish
endif

" Variable Definations: {{{1
" options, define them as you like in vimrc:
if !exists("g:shellcompletekey")
    let g:shellcompletekey = "<c-b>"   "hotkey
endif

if !exists("g:shellhistoryfile")
    let g:shellhistoryfile = $HOME . "/.bash_history"   "hotkey
endif
" ----------------------------

" Autocommands: 
autocmd BufReadPost,BufNewFile * call ShellHistoryStart()

" Menus:
menu <silent>       &Tools.Shell\ History\ Complete\ Start          :call ShellHistoryStart()<CR>
menu <silent>       &Tools.Shell\ History\ Complete\ Stop           :call ShellHistoryStop()<CR>

" Function Definations: 

function! ShellHistoryStart()
    exec "silent! iunmap  <buffer> ".g:shellcompletekey
    exec "inoremap <buffer> ".g:shellcompletekey."  <c-r>=ShellHistoryComplete()<cr>"
endfunction

function! ShellHistoryStop()
    exec "silent! iunmap <buffer> ".g:shellcompletekey
endfunction

function! GetShellHistory()
	return readfile(g:shellhistoryfile)
endfunction

function!ShellHistoryComplete()
    let s:shellhistory_list=[]
	let s:shellhistory_selected = ''
	
    let fhistory=GetShellHistory()

    for i in fhistory
			if (match (i ,  substitute (getline ('.'), '^[\t ]*','','' )) != -1 )
					if (index (s:shellhistory_list, i) <= 0 )
						let s:shellhistory_list += [i]
					endif
			endif
    endfor
    if s:shellhistory_list==[]
        return ''
    endif
    if len(s:shellhistory_list)==1
        return s:shellhistory_list[0]['word']
    else
	 	exec 'normal ddi'	
        call  complete(col('.'),s:shellhistory_list)
        return ''
    endif
endfunction


