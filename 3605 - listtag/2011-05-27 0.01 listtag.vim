" File:         listtag.vim
" Brief:        List all functions, types, macro definitions in current 
"               file and jump to one of your choice; and jump between with 
"               .h/.c files.
" Author:       Fangxm (smecf@163.com)
" Version:      0.01
" Last Change:  2011-05-27 13:26:57
"
" Required:     ctags
"
" Install:      Put this file to your plugin directory.
"
" Usage:
"               <Ctrl-J>    /* list functions */
"               <Ctrl-P>    /* list functions prototype */
"               <Ctrl-K>    /* list types */
"               <Ctrl-N>    /* list macro definitions */
"               <Ctrl-H>    /* jump between with .h/.c files */
"

" keymap
nmap <silent> <C-J> :call LstTags('f')<CR>
nmap <silent> <C-P> :call LstTags('p')<CR>
nmap <silent> <C-K> :call LstTags('tsg')<CR>
nmap <silent> <C-N> :call LstTags('d')<CR>
nmap <silent> <C-H> :call JmpHC()<CR>


" show message
let s:WARN = "WarningMsg"
let s:ERR  = "ErrorMsg"
function! s:Show_Msg(type, msg)
	if a:type == s:WARN
		echohl WarningMsg
	elseif a:type == s:ERR
		echohl ErrorMsg
	endif

	echomsg a:msg
	echohl None
endfunction

" list tags in current file
function! LstTags(kinds)
	" prompt
	if a:kinds == 'f'
		let prompt = "function"
	elseif a:kinds == 'p'
		let prompt = "function prototype"
	elseif a:kinds == 'tsg'
		let prompt = "struct, enum, typedef"
	elseif a:kinds == 'd'
		let prompt = "macro definition"
	else
		return
	endif
	call s:Show_Msg(s:WARN, "NO  NAME [" . prompt . "]")
	call s:Show_Msg(s:WARN, "--  ----------------------------------------")

	" Run ctags and get the tag list
	let ctag_cmd = 
		\ "ctags -f - --format=2 --excmd=pattern --sort=no --fields=nks "
	let cmd_output =
		\ system(ctag_cmd . "--c-kinds=" . a:kinds . " " . bufname("%"))

	" split the ctags result and process each line
	let lines = split(cmd_output, "\n")
	let lst	= []
	let i = 0
	for line in lines
		" parse shout name
		let end  = stridx(line, "\t")
		let short_name = strpart(line, 0, end)

		" parse full name
		if a:kinds == 'd'
			let end_flag = "\/;\""
		else
			let end_flag = "$\/;\""
		endif
		let bgn  = match(line, "\/^")
		let end  = match(line, end_flag, bgn)
		if (bgn == -1) || (end == -1)
			continue
		endif
		if a:kinds == 'f' || a:kinds == 'p'
			let full_name = strpart(line, bgn + 2, end - bgn - 2)
		else
			let full_name = short_name
		endif

		" parse line number
		let bgn  = match(line, "line:", end)
		if bgn == -1
			continue
		endif
		let nu   = strpart(line, bgn + 5)
		
		let i += 1
		echo printf("%2d: %s", i , full_name)

		" add item into list
		call add(lst, [nu, short_name])
	endfor

	if i == 0
		call s:Show_Msg(s:WARN, "tags no found!")
		return
	endif

	" let user input a number or name, then jump to it
	echohl WarningMsg
	let ipt = input("choice[1-" . i . "]: \n")
	echohl None
	let choice = str2nr(ipt)

	if choice > 0 && choice <= i
		call setpos('.', ["%", lst[choice - 1][0], 1, 0])
	elseif strlen(ipt) == 0 || ipt == "q"
		return
	else
		for item in lst
			if ipt == item[1]
				call setpos('.', ["%", item[0], 1, 0])
				return
			endif
		endfor
		call s:Show_Msg(s:WARN, "input error!")
	endif
endfunction


" Jump between with .h/.c files
function! JmpHC()
	" get the target file's name
	let file_curr = bufname("%")
	let type = file_curr[strlen(file_curr) - 1]
	if type == 'h'
		let file_jump = substitute(file_curr, ".h$", ".c", "")
	elseif type == 'c'
		let file_jump = substitute(file_curr, ".c$", ".h", "")
	else
		return
	endif

	" check target file is exist
	if empty(findfile(file_jump))
		call s:Show_Msg(s:WARN, "\"". file_jump . "\" not found!")
		return
	endif

	" edit the target file
	execute "edit " . file_jump
endfunction
