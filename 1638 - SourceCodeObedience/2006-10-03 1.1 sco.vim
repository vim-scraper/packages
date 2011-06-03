" File: sco.vim
" Author: Nickolay Golubev
" Email: golubev.nikolay@gmail.com
"
" This script allow to work with cscope programm inside vim
" You can easily store cscope results and filter them
" Mark system allow to store interesting places of code for future analyse
"
" Please mail me problems

if exists('g:sco_plugin_loaded')
	finish
endif

let g:sco_plugin_loaded = 1

if ! exists('g:sco_default_db')
	let g:sco_default_db = "cscope.out"
endif

if ! exists('g:sco_default_exe')
	let g:sco_default_exe = "cscope"
endif

let s:sco_settings = {}
let s:sco_requests = {0:'C Symbol', 1:'Global Definition of',4:'Functions calling',5:'Text',6:'Grep', 8:'Files including file'}
let s:last_sco_buffer = -1
let s:preview = 0

" save sco result
function! <SID>SaveResult() "{{{
	exec "w"
endfunction "}}}
" escape search pattern
function! <SID>EscapePattern(line) "{{{
	return escape(a:line,'.$^|\')
endfunction "}}}

" return how many times search pattern match (from the beginig of file)
function! <SID>Search(line_number) "{{{
	let l:pattern = getline(a:line_number)
	let l:pattern = <SID>EscapePattern(l:pattern)
	let l:pattern = '^'.l:pattern.'$'
	"echo "Pattern:".l:pattern
	exec ':0'
	let l:is_found = search(l:pattern,'c')
	if l:is_found == 0
		return 0
	endif

	let l:jump_count = 1
	while l:is_found != a:line_number
		let l:is_found = search(l:pattern,'')
		let l:jump_count = l:jump_count + 1
	endwhile

	"echo "jumps:".l:jump_count
	return l:jump_count
endfunction "}}}

" move to N'th pattern match 
function! <SID>JumpTo(pattern, jump_count) "{{{
	exec ':0'
	let l:flag = 'c'
	let l:completed_jumps = 0
	let l:pattern = <SID>EscapePattern(a:pattern)
	let l:pattern = '^'.l:pattern.'$'
	while l:completed_jumps < a:jump_count
		let l:line_number = search(l:pattern, l:flag)
		if l:line_number == 0
			echohl WarningMsg | echo "Can't find pattern:".a:pattern | echohl None
			return
		endif
		let l:completed_jumps = l:completed_jumps + 1
		let l:flag = ''
	endwhile
endfunction "}}}
" Parse sco settings (% option:value)
function! <SID>ParseSCOSettings() "{{{
	let l:line_count = line("$")
	let s:sco_settings = {}
	if l:line_count == 0 
		return 
	endif

	let l:line_number = 1
	while l:line_number <= l:line_count
		let l:line = getline(l:line_number)
		let l:option_pattern = '^%\s\(\S\+\):\s\(\S\+\)'
		"echo 'test:'.l:line_number.' :'.l:line
		if l:line =~ l:option_pattern
			"echo 'pattern_match'
			let l:option_name = substitute(l:line, l:option_pattern, '\1', '')
			let l:option_value = substitute(l:line, l:option_pattern, '\2', '')
			"echo "otpname:".l:option_name." ; value:".l:option_value
			let s:sco_settings[ l:option_name ] = l:option_value
		else
			break
		endif

		let l:line_number = l:line_number + 1
	endwhile
endfunction "}}}

" Put default option settings to sco buffer if not present
function! <SID>DefaultSetting(name, value) "{{{
	if has_key(s:sco_settings, a:name)==0
		call append(0, '% '.a:name.': '.a:value)
		let s:sco_settings[a:name] = a:value
	endif
endfunction "}}}

" set all sco settings 
function! <SID>SetSettings() "{{{
	call <SID>ParseSCOSettings()
	call <SID>DefaultSetting('cscope_db', g:sco_default_db)
	call <SID>DefaultSetting('cscope_exe', g:sco_default_exe)
endfunction "}}}

" open last sco buffer
function! <SID>GoToLastScoBuffer() "{{{
	let l:last_sco_buffer_name = bufname(s:last_sco_buffer)

	if l:last_sco_buffer_name !~ "sco$"
		echohl WarningMsg | echo 'Last sco ['.s:last_sco_buffer.'] buffer not present' | echohl None
		"echo ""
		return 0
	endif

	exec 'buffer! '.s:last_sco_buffer
	return 1
endfunction "}}}

" put results from cscope
function! <SID>CScopeResult(type, word) "{{{
	if ! <SID>GoToLastScoBuffer()
		return
	endif

	call <SID>SetSettings()
	let l:cmd_line = s:sco_settings['cscope_exe']
	let l:data_base = s:sco_settings['cscope_db']
	let l:cmd_line = l:cmd_line.' -d -L -'.a:type.' "'.a:word.'" -f '.l:data_base
	let l:request_line = s:sco_requests[ a:type ].' "'.a:word.'"'

	exec ':'.line("$")
	let l:first_line = line(".")
	exec 'r !'.l:cmd_line
	let l:last_line = line(".")

	if l:first_line == l:last_line
		echohl WarningMsg | echo l:request_line.' not found in '.l:data_base | echohl None
		echo ""
		return
	endif
	let l:failed = append(l:first_line, '>>> '.l:request_line)
	let l:failed = append(l:last_line+1, '<<<')

	let l:first_line = l:first_line + 2
	let l:last_line = l:last_line + 1
	let l:line_number = l:first_line
	let l:max_file_name_length = 0
	let l:max_function_name_length = 0
	let l:max_line_number_length = 0

	let l:pattern = '^\(\S\+\)\s\(\S\+\)\s\(\d\+\)\s\(.*\)$'

	while l:line_number <= l:last_line
		let l:line = getline(l:line_number)

		let l:file_name = substitute(l:line, l:pattern, '\1', '')
		let l:function_name = substitute(l:line, l:pattern, '\2', '')
		let l:file_line_number = substitute(l:line, l:pattern, '\3', '')
		let l:body = substitute(l:line, l:pattern, '\4', '')

		if strlen(l:file_name) > l:max_file_name_length
			let l:max_file_name_length = strlen(l:file_name)
		endif

		if strlen(l:function_name) > l:max_function_name_length
			let l:max_function_name_length = strlen(l:function_name)
		endif

		if strlen(l:file_line_number) > l:max_line_number_length
			let l:max_line_number_length = strlen(l:file_line_number)
		endif

		let l:line_number = l:line_number + 1
	endwhile

	let l:line_number = l:first_line
	while l:line_number <= l:last_line
		let l:line = getline(l:line_number)

		let l:file_name = substitute(l:line, l:pattern, '\1', '')
		let l:function_name = substitute(l:line, l:pattern, '\2', '')
		let l:file_line_number = substitute(l:line, l:pattern, '\3', '')
		let l:body = substitute(l:line, l:pattern, '\4', '')

		let l:new_line = printf('# %-'.max_file_name_length.'s %'.max_function_name_length.'s %'.max_line_number_length.'s %s', l:file_name, l:function_name, l:file_line_number, l:body)
		call setline(l:line_number, l:new_line)

		let l:line_number = l:line_number + 1
	endwhile
	exec ':'.l:first_line
endfunction "}}}

" function get line number of open fold >>>
function! <SID>OpenFoldLineNumber() "{{{
	let l:first_line_num = -1
	let l:line_num = line('.')
	while l:line_num > 0
		let l:line = getline(l:line_num)
		if line =~ '^>>>'
			let l:first_line_num = l:line_num
			break
		endif
		let l:line_num = l:line_num - 1
	endwhile
	return l:first_line_num
endfunction "}}}

" function get line number of close fold <<<
function! <SID>CloseFoldLineNumber() "{{{
	let l:last_line_num = -1
	let l:line_num = line('.')
	let l:line_count = line('$')
	while l:line_num <= l:line_count
		let l:line = getline(l:line_num)
		if line =~ '^<<<'
			let l:last_line_num = l:line_num
			break
		endif
		let l:line_num = l:line_num + 1
	endwhile
	return l:last_line_num
endfunction "}}}

" allign folded result
function! <SID>AllignFoldResult()
	let l:first_line_num = <SID>OpenFoldLineNumber()
	let l:last_line_num = <SID>CloseFoldLineNumber()

	if l:first_line_num<0 || l:last_line_num<0
		return
	endif

	call <SID>AllignResult(l:first_line_num+1, l:last_line_num-1)
	call <SID>AllignResultNewMarks(l:first_line_num+1, l:last_line_num-1)
endfunction

" filter result inside >>> <<<
function! <SID>FilterResult(delete_pattern, leave_pattern) "{{{
	let l:first_line_num = <SID>OpenFoldLineNumber()
	let l:last_line_num = <SID>CloseFoldLineNumber()

	if l:first_line_num < 0 || l:last_line_num < 0
		return
	endif

	let l:line_num = l:first_line_num + 1
	while l:line_num < l:last_line_num
		let l:line = getline(l:line_num)

		if l:line =~ a:delete_pattern && l:line !~ a:leave_pattern
			exec l:line_num.'d'
			let l:line_num = l:line_num - 1
			let l:last_line_num = l:last_line_num - 1
		endif
		let l:line_num = l:line_num + 1
	endwhile
	call <SID>AllignFoldResult()
endfunction "}}}

" allign sco result
function! <SID>AllignResult(first_line, last_line) "{{{
	let l:pattern = '^#\s\+\(\S\+\)\s\+\(\S\+\)\s\+\(\d\+\)\s\+\(.*\)$'

	let l:max_file_name_length = 0
	let l:max_function_name_length = 0
	let l:max_line_number_length = 0

	let l:line_number = a:first_line
	while l:line_number <= a:last_line
		let l:line = getline(l:line_number)
		if l:line !~ l:pattern
			let l:line_number = l:line_number + 1
			continue
		endif

		let l:file_name = substitute(l:line, l:pattern, '\1', '')
		let l:function_name = substitute(l:line, l:pattern, '\2', '')
		let l:file_line_number = substitute(l:line, l:pattern, '\3', '')
		let l:body = substitute(l:line, l:pattern, '\4', '')

		if strlen(l:file_name) > l:max_file_name_length
			let l:max_file_name_length = strlen(l:file_name)
		endif

		if strlen(l:function_name) > l:max_function_name_length
			let l:max_function_name_length = strlen(l:function_name)
		endif

		if strlen(l:file_line_number) > l:max_line_number_length
			let l:max_line_number_length = strlen(l:file_line_number)
		endif

		let l:line_number = l:line_number + 1
	endwhile

	let l:line_number = a:first_line
	while l:line_number <= a:last_line
		let l:line = getline(l:line_number)
		if l:line !~ l:pattern
			let l:line_number = l:line_number + 1
			continue
		endif

		let l:file_name = substitute(l:line, l:pattern, '\1', '')
		let l:function_name = substitute(l:line, l:pattern, '\2', '')
		let l:file_line_number = substitute(l:line, l:pattern, '\3', '')
		let l:body = substitute(l:line, l:pattern, '\4', '')

		let l:new_line = printf('# %-'.max_file_name_length.'s %'.max_function_name_length.'s %'.max_line_number_length.'s %s', l:file_name, l:function_name, l:file_line_number, l:body)
		call setline(l:line_number, l:new_line)

		let l:line_number = l:line_number + 1
	endwhile
endfunction "}}}

" allign sco result (new marks format)
function! <SID>AllignResultNewMarks(first_line, last_line) "{{{
	let l:pattern = '^@\s\+\(\S\+\)\s\+\(\d\+\)\s\(.*\)$'

	let l:max_file_name_length = 0
	let l:max_repeat_count_length = 0

	let l:line_number = a:first_line
	while l:line_number <= a:last_line
		let l:line = getline(l:line_number)

		if l:line !~ l:pattern
			let l:line_number = l:line_number + 1
			continue
		endif

		let l:file_name = substitute(l:line, l:pattern, '\1', '')
		let l:repeat_count = substitute(l:line, l:pattern, '\2', '')

		if strlen(l:file_name) > l:max_file_name_length
			let l:max_file_name_length = strlen(l:file_name)
		endif

		if strlen(l:repeat_count) > l:max_repeat_count_length
			let l:max_repeat_count_length = strlen(l:repeat_count)
		endif

		let l:line_number = l:line_number + 1
	endwhile

	let l:line_number = a:first_line
	while l:line_number <= a:last_line
		let l:line = getline(l:line_number)
		if l:line !~ l:pattern
			let l:line_number = l:line_number + 1
			continue
		endif

		let l:file_name = substitute(l:line, l:pattern, '\1', '')
		let l:repeat_count = substitute(l:line, l:pattern, '\2', '')
		let l:search_pattern = substitute(l:line, l:pattern, '\3', '')

		let l:new_line = printf('@ %-'.max_file_name_length.'s %'.max_repeat_count_length.'s %s', l:file_name, l:repeat_count, l:search_pattern)
		call setline(l:line_number, l:new_line)

		let l:line_number = l:line_number + 1
	endwhile
endfunction "}}}

" add mark from file
function! <SID>AddMark() "{{{
	let l:line_number = line('.')
	let l:line = getline('.')
	let l:file_name = expand('%:p')

	if ! <SID>GoToLastScoBuffer()
		return
	endif


	let l:mark_line = '# '.l:file_name.' <mark> '.l:line_number.' '.l:line
	call append(line('$'), l:mark_line)

	let l:line_number = line('$')
	while l:line_number >= 0
		let l:line = getline(l:line_number)
		if l:line !~ '<mark>'
			break
		endif
		let l:line_number = l:line_number - 1
	endwhile

	call <SID>AllignResult(l:line_number+1, line('$'))
endfunction "}}}

" add smart mark
function! <SID>AddSmartMark() "{{{
	let l:line = getline('.')
	let l:file_name = expand('%:p')
	let l:jumps_count = <SID>Search(line('.'))

	if ! <SID>GoToLastScoBuffer()
		return
	endif

	let l:smart_mark_line = '@ '.l:file_name.' '.l:jumps_count.' '.l:line
	call append(line('$'), l:smart_mark_line)

	let l:line_number = line('$')
	while l:line_number >= 0
		let l:line = getline(l:line_number)
		if l:line !~ '^@'
			break
		endif
		let l:line_number = l:line_number - 1
	endwhile

	call <SID>AllignResultNewMarks(l:line_number+1, line('$'))
endfunction "}}}

" select file to edit
function! <SID>EditFile() "{{{
	let l:pattern = '^#\s\+\(\S\+\)\s\+\(\S\+\)\s\+\(\d\+\)\s\+\(.*\)$'
	let l:current_line = getline('.')
	if l:current_line =~ l:pattern
		let l:file_name = substitute(l:current_line, l:pattern, '\1','')
		let l:line_number = substitute(l:current_line, l:pattern, '\3','')

		if s:preview
			exec 'pclose'
		endif
		exec 'edit  +:'.l:line_number.' '.l:file_name
		return
	endif

	let l:pattern = '^@\s\+\(\S\+\)\s\+\(\d\+\)\s\(.*\)$'
	if l:current_line =~ l:pattern
		let l:file_name = substitute(l:current_line, l:pattern, '\1','')
		let l:repeat_count = substitute(l:current_line, l:pattern, '\2','')
		let l:search_pattern = substitute(l:current_line, l:pattern, '\3','') 
		if s:preview
			exec 'pclose'
		endif
		exec 'edit '.l:file_name
		call <SID>JumpTo(l:search_pattern, l:repeat_count)
		return
	endif
endfunction "}}}

" cursor move handler
function! <SID>Cursor_move_handler() "{{{
	if ! s:preview
		return
	endif

	let l:pattern = '^#\s\+\(\S\+\)\s\+\(\S\+\)\s\+\(\d\+\)\s\+\(.*\)$'
	let l:current_line = getline('.')

	if l:current_line =~ l:pattern
		let l:file_name = substitute(l:current_line, l:pattern, '\1', '')
		let l:line_number = substitute(l:current_line, l:pattern, '\3', '')
		exec 'pedit +'.l:line_number.' '.l:file_name
	else
		exec 'pclose'
	endif
endfunction "}}}

" toggle preview
function! <SID>TogglePreview() "{{{
	if s:preview
		let s:preview = 0
		exec 'pclose'
		return
	endif

	if ! s:preview
		let s:preview = 1
		call <SID>Cursor_move_handler()
		return
	endif
endfunction "}}}

" put help to sco buffer
function! <SID>AddHelpLines() "{{{
	let l:help_lines = []
	call add(l:help_lines, '>>> sco help')
	call add(l:help_lines, '/Hot keys local to this buffer:/')
	call add(l:help_lines, '<CR> - select file to edit from result')
	call add(l:help_lines, 'c<Space>p - Toggle preview mode')
	call add(l:help_lines, 'c<Space>a - Allign folded result')
	call add(l:help_lines, '/Global hot keys:/')
	call add(l:help_lines, 'c<Space>g - Find Global Definition of symbol under cursor')
	call add(l:help_lines, 'c<Space>c - Find C Symbol')
	call add(l:help_lines, 'c<Space>f - Find Files including this file')
	call add(l:help_lines, 'c<Space>w - Find Functions calling this function')
	call add(l:help_lines, 'c<Space>b - Open last sco buffer')
	call add(l:help_lines, 'c<Space>m - Mark current line')
	call add(l:help_lines, 'c<Space>n - Mark smart current line')
	call add(l:help_lines, '/Commands local to this buffer:/')
	call add(l:help_lines, ":Delete 'pattern' - delete all rows which match 'pattern' inside folded result")
	call add(l:help_lines, ":Leave 'pattern' - leave only rows which match 'pattern' inside folded result")
	call add(l:help_lines, ":Filter 'delete_pattern', 'leave_pattern' - leave only rows which match 'pattern' inside folded result and delete rows with 'delete_pattern'")
	call add(l:help_lines, ':Preview - toggle preview mode')
	call add(l:help_lines, ':Allign - allign folded result')
	call add(l:help_lines, '/Global commands:/')
	call add(l:help_lines, ":SCOSymbol 'symbolname' - find C Symbol")
	call add(l:help_lines, ":SCOGlobal 'functionname' - find Global definition")
	call add(l:help_lines, ":SCOInclude 'filename' - find files including <filename>")
	call add(l:help_lines, ":SCOWhoCall 'functionname' - find functions calling <functionname> function")
	call add(l:help_lines, ":SCOText 'text' - find text")
	call add(l:help_lines, ":SCOGrep 'pattern' - find grep pattern")
	call add(l:help_lines, ":SCOBuffer - go to last sco buffer")
	call add(l:help_lines, ":SCOMark - mark current line")
	call add(l:help_lines, ":SCOMarkSmart - mark smart current line")
	call add(l:help_lines, "/Format of mark/")
	call add(l:help_lines, "# filename functionname linenumber body")
	call add(l:help_lines, "/Format of smart mark/")
	call add(l:help_lines, "@ filename jumpscount pattern")
	call add(l:help_lines, "/jumpscount/")
	call add(l:help_lines, "if you have in text two patterns '^  return ERROR_CODE$' and mark second of them")
	call add(l:help_lines, "then jumpscount will be equal 2")
	call add(l:help_lines, '<<< sco help')
	call add(l:help_lines, '>>> fold help')
	call add(l:help_lines, 'zj - move to next fold')
	call add(l:help_lines, 'zk - previous fold')
	call add(l:help_lines, 'zo - open fold')
	call add(l:help_lines, 'zc - close fold')
	call add(l:help_lines, '[z - top of fold')
	call add(l:help_lines, ']z - bottom of fold')
	call add(l:help_lines, 'zR - unfold all')
	call add(l:help_lines, 'zM - fold all')
	call add(l:help_lines, '<<< fold help')
	call append(line('$'), l:help_lines)
endfunction "}}}

function! <SID>EnterScoBuffer() "{{{
	let s:last_sco_buffer = bufnr('%')
	call <SID>Cursor_move_handler()
endfunction "}}}

function! <SID>LeaveScoBuffer() "{{{
	echo "test"
	echo "test"
	echo "test"
	echo "test"
	call <SID>SaveResult()
endfunction "}}}

" set highlight and commands - called when .sco file readed
function! <SID>Prepare_sco_settings() "{{{
	setlocal filetype=sco
	setlocal foldmethod=marker
	setlocal foldmarker=>>>,<<<
	setlocal foldclose=all
	setlocal cursorline
	setlocal autowriteall

	call <SID>SetSettings()

	let s:last_sco_buffer = bufnr('%')

	command! -nargs=1 SCOSymbol call <SID>CScopeResult(0, <args>)
	command! -nargs=1 SCOGlobal call <SID>CScopeResult(1, <args>)
	command! -nargs=1 SCOWhoCall call <SID>CScopeResult(4, <args>)
	command! -nargs=1 SCOText call <SID>CScopeResult(5, <args>)
	command! -nargs=1 SCOGrep call <SID>CScopeResult(6, <args>)
	command! -nargs=1 SCOInclude call <SID>CScopeResult(8, <args>)
	command! SCOBuffer call <SID>GoToLastScoBuffer()
	command! SCOMark call <SID>AddMark()
	command! SCOMarkSmart call <SID>AddSmartMark()
	command! -buffer Preview call <SID>TogglePreview()
	command! -buffer Allign call <SID>AllignFoldResult()
	command! -buffer -nargs=1 Delete call <SID>FilterResult(<args>, '$^')
	command! -buffer -nargs=1 Leave call <SID>FilterResult('.*', <args>)
	command! -buffer -nargs=1 -nargs=1 -complete=tag Filter call <SID>FilterResult(<args>)

	nnoremap <buffer> <CR> :call <SID>EditFile()<CR>
	nnoremap <buffer> c<Space>p :Preview<CR>
	nnoremap <buffer> c<Space>a :Allign<CR>

	nnoremap c<Space>g :SCOGlobal expand('<cword>')<CR>
	nnoremap c<Space>c :SCOSymbol expand('<cword>')<CR>
	nnoremap c<Space>t :SCOText expand('<cword>')<CR>
	nnoremap c<Space>w :SCOWhoCall expand('<cword>')<CR>
	nnoremap c<Space>f :SCOInclude expand('<cfile>')<CR>
	nnoremap c<Space>b :SCOBuffer<CR>
	nnoremap c<Space>m :SCOMark<CR>
	nnoremap c<Space>n :SCOMarkSmart<CR>

	syn match sco_header /^% cscope_db: / nextgroup=sco_header_param
	syn match sco_header /^% cscope_exe: / nextgroup=sco_header_param
	syn match sco_comment /^\/.\+\/$/

	syn match sco_header_param /.*/ contained
	syn match sco  />>>.*$/
	syn match sco0 /<<<.*$/
	syn match sco1 /^#/ nextgroup=sco2 skipwhite
	syn match sco2 /\s\S\+/ nextgroup=sco3 contained skipwhite
	syn match sco3 /\s\S\+/ nextgroup=sco4 contained skipwhite
	syn match sco4 /\s\S\+/ nextgroup=sco5 contained skipwhite
	syn region sco5 start="\s" end="$" contains=Comment,Number,Float contained keepend

	syn match mark1 /^@/ nextgroup=mark2 skipwhite
	syn match mark2 /\s\S\+/ nextgroup=mark3 contained skipwhite
	syn match mark3 /\s\S\+/ nextgroup=mark4 contained skipwhite
	syn region mark4 start='\s' end='$' contained keepend

	hi link sco_header	Define
	hi link sco_header_param   Identifier
	hi link sco_comment     Comment
	hi link sco	 	Comment
	hi link sco0	 	Comment
	hi link sco1		Comment
	hi link sco2		Conditional
	hi link sco3		Identifier
	hi link sco4		Underlined
	hi link sco5		String

	hi link mark1	Comment
	hi link mark2	Statement
	hi link mark3	Special
	hi link mark4  	String
endfunction "}}}

" set highlight and commands, put help - called when .sco file created
function! <SID>Prepare_sco_settings_new_file()
	call <SID>Prepare_sco_settings()
	call <SID>AddHelpLines()
endfunction

augroup filetypedetect
au! BufRead *.sco call <SID>Prepare_sco_settings()
au! BufNewFile *.sco call <SID>Prepare_sco_settings_new_file()
au! BufWinEnter *.sco call <SID>EnterScoBuffer()
au! CursorMoved *.sco nested call <SID>Cursor_move_handler()
augroup END
" vim:ft=vim:fdm=marker:ff=unix:nowrap
