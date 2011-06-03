" Vim plugin for automatically generating C function documentation
" Last Change:  2006 Aug 04
" Maintainer:   Kaustubh <kaustubhp@gmail.com>
" TODO: 	Add support for C functions that accept
" 		variable number of arguments using '...'
" 		Also, at present, functions that don't
" 		accept any arguments (i.e. void) result in
" 		an incorrect documentation format.


" Saving value of cpoptions, and then setting to vim default
let s:save_cpo = &cpo
set cpo&vim

" Ensure that the plugin is loaded only once
if exists("loaded_c_function_header_plugin")
	finish
endif
let loaded_c_function_header_plugin = 1

" change this line if you want to have some different leader for the
" key-bindings of this plugin.
" let mapleader = '#'

" mapping 'f' as the default key for adding a file header
" default leader is '\', hence following function shall be invoked as
" '\f'
if !hasmapto('<Plug>c_function_header_add')
	map <unique> <Leader>f <Plug>c_function_header_add
endif

" defining the function that will add the function header
function s:add_function_header()

	let s:stmt_end = searchpos('\({\)\|\(;\)', 'cpW')
	let s:param_end = searchpos('\()\)', 'b')
	let s:param_start = searchpos('\((\)', 'b')

	:normal b
	:normal yw
	let s:function_name = @"

	let s:ret_type_end = getpos('.')
	let s:ret_type_end = [s:ret_type_end[1], s:ret_type_end[2]]
	let s:ret_type_start = searchpos('\(}\)\|\(;\)\|\("\)\|\(>\)\|\(\/\)', 'cbW')
	if s:ret_type_start[0] == 0 && s:ret_type_start[1] == 0
		let s:ret_type_start = [1, 1]
		:normal 1G^
	else
		let s:ret_type_start = searchpos("\\h", '')
		let s:ret_type_start = getpos('.')
		let s:ret_type_start = [s:ret_type_start[1], s:ret_type_start[2]]
	endif

	" now getting the return type...
	let s:ret_type = ''
	let s:temp_pos = [getpos('.')[1], getpos('.')[2]]
	while s:temp_pos[0] < s:ret_type_end[0] || (s:temp_pos[0] == s:ret_type_end[0] && s:temp_pos[1] < s:ret_type_end[1])
		 :normal yw
		let s:temp_str = @"
		let s:ret_type = s:ret_type . s:temp_str
		:normal w
		let s:temp_pos = [getpos('.')[1], getpos('.')[2]]
	endwhile


	" now getting the parameter list in List format
	" put the cursor on the opening bracket '('
	:normal w 

	" cursor now on first parameter qualifier
	let s:temp_pos = searchpos(',\|)', '')
	let s:old_pos = deepcopy(s:param_start)
	
	" cursor now on first comma, or the closing bracket
	let s:temp_pos = [getpos('.')[1], getpos('.')[2]]
	let s:temp_str = ''
	let s:param_str = ''
	while (s:temp_pos[0] < s:param_end[0] || (s:temp_pos[0] == s:param_end[0] && s:temp_pos[1] <= (s:param_end[1] ))) && (s:old_pos[0] < s:temp_pos[0] || (s:old_pos[0] == s:temp_pos[0] && s:old_pos[1] < s:temp_pos[1]))
		:normal byw
		let s:temp_str = s:temp_str . @" . ','
		:normal w
		let s:old_pos = deepcopy(s:temp_pos)
		let s:temp_pos = searchpos(',\|)', '')
		let s:temp_pos = [getpos('.')[1], getpos('.')[2]]
	endwhile

	let s:temp_list = split(s:temp_str, ',')
	let s:temp_str = " \\param " . join(s:temp_list, " : \n \\param ") . " : \n"

	" now putting the information in the required format
	let s:output_str = "\/*! \n \\function " . s:function_name . "\n"
	let s:output_str .= " \\brief \n"
	let s:output_str .= " \\retval " . s:ret_type . ":\n"
	let s:output_str .= s:temp_str
	let s:output_str .= "*/\n"

	call setpos('.', [0, s:ret_type_start[0], s:ret_type_start[1], 0])
	let @" = s:output_str
	:normal P
	

		
endfunction


noremap <unique> <script> <Plug>c_function_header_add  <SID>add_function_header
noremap <SID>add_function_header :call <SID>add_function_header()<CR>

" adding user command
if !exists(":eisoheader")
	command -nargs=0 Eisofuncheader :call s:add_function_header()
endif

" restoring cpoptions
let &cpo=s:save_cpo
