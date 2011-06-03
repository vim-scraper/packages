" Plugin: acscope.vim --- Frontend for CScope
" 
" License:
" Do whatever you want with this file.
" 
" Version: 0.2
"
" Changelog:
" 2005.04.28
"   o bugfix: store proper filename in the tagstack
" 
" Author: Gabor Fekete
" Date: 2004.08.someday
" 
" Description:
" It uses two windows, one for listing the queries done and
" another for listing the result of the selected query.
"
" It can be used parallel with the normal cscope frontend of
" vim because they use different settings and key bindings.
" 
" Installation:
" Copy this file into ~/.vim/plugins/
" 
" Usage:
" Start it up with
" a) ':ACSstart cscope_out_directory_path' or
" b) 1. :let g:acs_cscope_dir="cscope.out directory path"
"    2. :ACSstart
"
" It will create two new windows at the bottom of the screen.
" They are initially empty. The left window lists queries,
" the right one lists results for a specific query.
" 
" Keybindings: The 'symbol' is the word under the cursor.
" Ctrl+1 - Find symbol definition
" Ctrl+2 - Find functions called by the symbol
" Ctrl+3 - Find functions calling the symbol
" Ctrl+4 - Find symbol as text
" Ctrl+6 - Find symbol as egrep pattern
" Ctrl+7 - Find symbol as a file
" Ctrl+8 - Find files including symbol
"
" To jump to a result, click twice on it or press enter on it.
" To select a query from the list, click on it or press enter on it.
" To delete a query press 'd' on it.
"
" Alt+Left - Jump to the previous result
" Alt+Right - Jump to a most recent result
"

" s:acs_qrescnt_{i} = the number of results belonging to the ith query
" s:acs_qres_{i}_{j}_1 = The file name of the query result belonging to
"                        the jth result of the ith query.
"                   _2 = The function name
"                   _3 = The line number
"                   _4 = Line contents
" 
let s:acs_query_cnt = 0	" Number of queries in the query list window.
let g:acs_query_title = "QUERY_LIST"
let g:acs_queryres_title = "QUERY_RESULTS"
let s:acs_csres_pattern = "/\=\f "
let g:acs_cscope_dir = ""
let s:acs_active_query = 0 " The index of the selected query in the
			   " query list window.
" points at the item on the top of the stack
let s:acs_stack_top = 0
" points at the real item in the top of the stack
let s:acs_stack_end = 0

command! -nargs=* ACSstart call ACS_start(<f-args>)
command! -nargs=* ACSfindsym call ACS_find_symbol(<f-args>)
" Key bindings
noremap <C-F1> :ACSfindsym 1 <c-r>=expand("<cword>")<cr>
noremap <C-F2> :ACSfindsym 2 <c-r>=expand("<cword>")<cr>
noremap <C-F3> :ACSfindsym 3 <c-r>=expand("<cword>")<cr>
noremap <C-F4> :ACSfindsym 4 <c-r>=expand("<cword>")<cr>
noremap <C-F5> :ACSfindsym 5 <c-r>=expand("<cword>")<cr>
noremap <C-F6> :ACSfindsym 6 <c-r>=expand("<cword>")<cr>
noremap <C-F7> :ACSfindsym 7 <c-r>=expand("<cword>")<cr>
noremap <C-F8> :ACSfindsym 8 <c-r>=expand("<cword>")<cr>
inoremap <A-Left> <ESC>:call ACS_pop_stack()<cr>
inoremap <A-Right> <ESC>:call ACS_pop_stack_forward()<cr>
noremap <A-Left> :call ACS_pop_stack()<cr>
noremap <A-Right> :call ACS_pop_stack_forward()<cr>

" Deletes a query from the query list.
function! s:ACS_del_query()
	let ii = line('.') - 1
	if ii <= 0
		return
	endif
	if ii > s:acs_query_cnt
		return
	endif
	" pull qres ii+1 over ii
	let ii = ii + 1
	while ii <= s:acs_query_cnt
		let jj = 1
		while jj <= s:acs_qrescnt_{ii}
			let s:acs_qres_{ii-1}_{jj}_1 = s:acs_qres_{ii}_{jj}_1
			let s:acs_qres_{ii-1}_{jj}_2 = s:acs_qres_{ii}_{jj}_2
			let s:acs_qres_{ii-1}_{jj}_3 = s:acs_qres_{ii}_{jj}_3
			let s:acs_qres_{ii-1}_{jj}_4 = s:acs_qres_{ii}_{jj}_4
			unlet s:acs_qres_{ii}_{jj}_1
			unlet s:acs_qres_{ii}_{jj}_2
			unlet s:acs_qres_{ii}_{jj}_3
			unlet s:acs_qres_{ii}_{jj}_4
			let jj = jj + 1
		endwhile
		" Pull qrescnt
		let s:acs_qrescnt_{ii-1} = s:acs_qrescnt_{ii}
		" Pull queryname
		let s:acs_queryname_{ii-1} = s:acs_queryname_{ii}
		let ii = ii + 1
	endwhile
	" Unlet unused (last) entries.
	unlet s:acs_qrescnt_{ii-1}
	unlet s:acs_queryname_{ii-1}
	
	let s:acs_query_cnt = s:acs_query_cnt - 1
	if s:acs_query_cnt == 0
		let s:acs_active_query = 0
	endif
	if s:acs_active_query > s:acs_query_cnt
		let s:acs_active_query = s:acs_query_cnt
	endif
	" Delete the line from the query list window
	setlocal modifiable
	exe "delete"
	setlocal nomodifiable
	call s:ACS_populate_reswin(s:acs_active_query)
endfunction

" Jumps at a more recent query result (i.e. Alt+Right)
function! ACS_pop_stack_forward()
	if s:acs_stack_top == s:acs_stack_end
		return
	endif
	let s:acs_stack_top = s:acs_stack_top + 1
	exe "edit " . s:acs_pos_stack_{s:acs_stack_top}_file
	exe s:acs_pos_stack_{s:acs_stack_top}_line
endfunction

" Jumps at an older query result (Alt+Left).
function! ACS_pop_stack()
	if s:acs_stack_top == 0
		return
	endif
	" jump to the line in the file
	let s:acs_stack_top = s:acs_stack_top - 1
	exe "edit " . s:acs_pos_stack_{s:acs_stack_top}_file
	exe s:acs_pos_stack_{s:acs_stack_top}_line
endfunction

" Jumps at a query result.
" Opens the file to edit and jumps at the proper line.
"
" The query result window must be the active one when
" calling this function!
function! s:ACS_goto_respos()
	if s:acs_query_cnt == 0
		return
	endif
	" if no query is activated then the res window shows
	" the res for the last query
	if s:acs_active_query == 0
		let s:acs_active_query = s:acs_query_cnt
	endif
	" get the index of the query result
	let idx = line('.') - 1
	if idx == 0
		return
	endif
	if idx > s:acs_qrescnt_{s:acs_active_query}
		return
	endif
	" find the acs window and select it
	let acs_winnum = -1
        let i = 1
        while winbufnr(i) != -1
        	if getwinvar(i, 'acs_window') == "acs window"
                	let acs_winnum = i
                	break
            	endif
            	let i = i + 1
        endwhile
	if acs_winnum == -1
		echo "ACS window not found!"
		return
	endif
	exec acs_winnum . "wincmd w"
	" remember the current file and pos
	let s:acs_pos_stack_{s:acs_stack_top}_file = expand("%:p")
	let s:acs_pos_stack_{s:acs_stack_top}_line = line('.')
	let s:acs_stack_top = s:acs_stack_top + 1
	if s:acs_stack_top > s:acs_stack_end
		let s:acs_stack_end = s:acs_stack_top
	endif
	" [open the file and] goto the line number
	exec 'edit ' . s:acs_qres_{s:acs_active_query}_{idx}_1
	exec s:acs_qres_{s:acs_active_query}_{idx}_3
	" remember the target
	let s:acs_pos_stack_{s:acs_stack_top}_file = expand("%:p")
	let s:acs_pos_stack_{s:acs_stack_top}_line = line('.')
endfunction

" Selects a query from the query list window.
function! s:ACS_switch_qres()
	let idx = line('.') - 1
	if idx <= 0
		return
	endif
	if idx > s:acs_query_cnt
		return
	endif
	let s:acs_active_query = idx
	call s:ACS_populate_reswin(idx)
endfunction

" Parses the output of cscope
function! s:ACS_parse_res(res, qidx)
	let cmd_output = a:res
	"if cmd_output ~= 'cscope: 0 lines'
	"	return 0
	"endif
	" parse line-by-line
	let i = 0
	while cmd_output != ''
		" Extract one line at a time
		let one_line = strpart(cmd_output, 0, stridx(cmd_output, "\n"))
		" Remove the line from the output
		let cmd_output = strpart(cmd_output, stridx(cmd_output, "\n") + 1)
		"if one_line ~= '^cscope:'
			" skip this informational line
		"	continue
		"endif
		let i = i + 1
		let jj = 1
		" A result consists of 3 parts: file name, function name,
		" line number, context.
		while jj <= 3
			let s:acs_qres_{a:qidx}_{i}_{jj} = strpart(one_line, 0, stridx(one_line, " "))
			let one_line = strpart(one_line, stridx(one_line, " ") + 1)
			let jj = jj + 1
		endwhile
		let s:acs_qres_{a:qidx}_{i}_{jj} = one_line
	endwhile
	return i
endfunction

" Adds content to the query result window.
function! s:ACS_populate_reswin(qidx)
	let qres_win = bufwinnr(g:acs_queryres_title)
	let oldwin = winnr()
	" goto query res window
	exe qres_win . "wincmd w"
	setlocal modifiable
	exe '1,$delete'

	if a:qidx == 0
		call append(0, "There are now queries.")
		setlocal nomodifiable
		exe oldwin . "wincmd w"
		return
	endif

	call append(0, "Results for: " . s:acs_queryname_{a:qidx})
	if s:acs_qrescnt_{a:qidx} == 0
		call append(1, "No result!")
	else
		let i = 1
		while i <= s:acs_qrescnt_{a:qidx}
			let jj = 1
			let line = ""
			while jj <= 4
				let line = line . s:acs_qres_{a:qidx}_{i}_{jj} . " "
				let jj = jj + 1
			endwhile
			call append(i, line)
			let i = i + 1
		endwhile
	endif
	setlocal nomodifiable
	
	exe oldwin . "wincmd w"
endfunction

" Executes a query.
function! ACS_find_symbol(ndx, symbol)
	let query_win = bufwinnr(g:acs_query_title)
	let cur_win = winnr()
	let cur_dir = getcwd()
	let s:acs_query_cnt = s:acs_query_cnt + 1
	" Set up the query title
	if a:ndx == 0
		let s:acs_queryname_{s:acs_query_cnt} = "REF " . a:symbol
	elseif a:ndx == 1
		let s:acs_queryname_{s:acs_query_cnt} = "DEF " . a:symbol
	elseif a:ndx == 2
		let s:acs_queryname_{s:acs_query_cnt} = "<-- " . a:symbol
	elseif a:ndx == 3
		let s:acs_queryname_{s:acs_query_cnt} = "--> " . a:symbol
	elseif a:ndx == 4
		let s:acs_queryname_{s:acs_query_cnt} = "TXT " . a:symbol
	elseif a:ndx == 6
		let s:acs_queryname_{s:acs_query_cnt} = "GRP " . a:symbol
	elseif a:ndx == 7
		let s:acs_queryname_{s:acs_query_cnt} = "FIL " . a:symbol
	elseif a:ndx == 8
		let s:acs_queryname_{s:acs_query_cnt} = "INC " . a:symbol
	endif
	" Goto the query list window and append the new query text
	exe query_win . 'wincmd w'
	setlocal modifiable
	call append(s:acs_query_cnt, s:acs_queryname_{s:acs_query_cnt})
	setlocal nomodifiable
	" execute cscope query
	let qcmd = "-" . a:ndx . a:symbol
	let result = system("cd ". g:acs_cscope_dir ."; cscope -R -L " . qcmd)
	" parse the result
	let s:acs_qrescnt_{s:acs_query_cnt} = s:ACS_parse_res(result, s:acs_query_cnt)
	call s:ACS_populate_reswin(s:acs_query_cnt)
	let s:acs_active_query = s:acs_query_cnt
	" go back
	exe cur_win . "wincmd w"
endfunction

" Initializes the query result window
function! s:ACS_init_queryres_win()
	call append(0,"Results for ")
	setlocal nomodifiable
	silent! setlocal buftype=nofile
	silent! setlocal bufhidden=delete
	silent! setlocal noswapfile
	silent! setlocal nowrap
	silent! setlocal nonumber
        silent! setlocal nobuflisted
	nnoremap <buffer> <silent> <CR> :call <SID>ACS_goto_respos()<CR>
	nnoremap <buffer> <silent> <2-LeftMouse> :call <SID>ACS_goto_respos()<CR>

	if has('syntax')
		syntax match QResLineNum '\<[1-9][0-9]*\>'
		syntax keyword QResTitle Results for
		syntax match QResFileName '^\f\+\>'
		syntax match QResFuncName '\<\I\+\i*\>'

		" Define the highlighting only if colors are supported
		if has('gui_running') || &t_Co > 2
		    " Colors to highlight various taglist window elements
		    " If user defined highlighting group exists, then use them.
		    " Otherwise, use default highlight groups.
		    " Colors to highlight comments and titles
		    if hlexists('MyQResLineNum')
			highlight link QResLineNum MyQResLineNum
		    else
			highlight clear QResLineNum
			highlight link QResLineNum keyword
		    endif
		    if hlexists('MyQResFuncName')
			highlight link QResFuncName MyQResFuncName
		    else
			highlight clear QResFuncName
			highlight link QResFuncName normal
		    endif
		    if hlexists('MyQResTite')
			highlight link QResTitle MyQResTitle
		    else
			highlight clear QResTitle
			highlight link QResTitle keyword
		    endif
		    if hlexists('MyQResFileName')
			highlight link QResFileName MyQResFileName
		    else
			highlight clear QResFileName
			highlight link QResFileName type
		    endif
		else
		    highlight QResActive term=reverse cterm=reverse
		endif
	endif
endfunction

" Initializes the query list window
function! s:ACS_init_querylist_win()
	call append(0,"Queries")
	setlocal nomodifiable
	silent! setlocal buftype=nofile
	silent! setlocal bufhidden=delete
	silent! setlocal noswapfile
	silent! setlocal nowrap
	silent! setlocal nonumber
        silent! setlocal nobuflisted
	nnoremap <buffer> <silent> <CR> :call <SID>ACS_switch_qres()<CR>
	nnoremap <buffer> <silent> d :call <SID>ACS_del_query()<CR>
	nnoremap <silent> <LeftMouse> <LeftMouse>:if bufname("%") =~ g:acs_query_title
                        \ <bar> call <SID>ACS_switch_qres() <bar> endif <CR>

	if has('syntax')
		syntax match QListType '^.\+\s'
		syntax keyword QListTitle Queries

		" Define the highlighting only if colors are supported
		if has('gui_running') || &t_Co > 2
		    " Colors to highlight various taglist window elements
		    " If user defined highlighting group exists, then use them.
		    " Otherwise, use default highlight groups.
		    " Colors to highlight comments and titles
		    if hlexists('MyQListTitle')
			highlight link QListTitle MyQListTitle
		    else
			highlight clear QListTitle
			highlight link QListTitle keyword
		    endif
		    if hlexists('MyQListType')
			highlight link QListType MyQListType
		    else
			highlight clear QListType
			highlight link QListType type
		    endif
		endif
	endif
endfunction

" main()
function! ACS_start(...)
	if a:0 == 0
		if g:acs_cscope_dir == ''
			echo "Set the cscope.out directory in g:acs_cscope_dir first!"
			return
		endif
	else
		let g:acs_cscope_dir = a:1
	endif
	let w:acs_window = "acs window"
	" Init query res window
	exe 'botright 15new ' . g:acs_queryres_title
	call s:ACS_init_queryres_win()
	" Init query list window
	exe '20vnew ' . g:acs_query_title
	call s:ACS_init_querylist_win()
endfunction


