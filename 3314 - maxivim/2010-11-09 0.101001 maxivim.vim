" maxivim.vim: maximize/unmaximize a gvim window using columns/lines
"
" vim: set ts=4 sw=4 sts=0 noet ai:

if exists('g:maxivim_loaded')
	finish
endif
let g:maxivim_loaded = 1

if has('gui_running')

	if ! exists("g:maxivim_default_columns")
		let g:maxivim_default_columns = $GVIM_COLUMNS
	endif
	if ! g:maxivim_default_columns
		let g:maxivim_default_columns = 80
	endif
	if ! exists("g:maxivim_default_lines")
		let g:maxivim_default_lines = $GVIM_LINES
	endif
	if ! g:maxivim_default_lines
		let g:maxivim_default_lines = 40
	endif
	if ! exists("g:maxivim_max_columns")
		let g:maxivim_max_columns = $GVIM_MAX_COLUMNS
	endif
	if ! g:maxivim_max_columns
		let g:maxivim_max_columns = 400
	endif
	if ! exists("g:maxivim_max_lines")
		let g:maxivim_max_lines = $GVIM_MAX_LINES
	endif
	if ! g:maxivim_max_lines
		let g:maxivim_max_lines = 200
	endif
	"if &number
	"	let g:maxivim_default_columns += &numberwidth
	"	let g:maxivim_max_columns += &numberwidth
	"endif

	let s:maximized_x = 0
	let s:maximized_y = 0
	let s:saved_columns = g:maxivim_default_columns
	let s:saved_lines = g:maxivim_default_lines
	let s:saved_winposx = -1
	let s:saved_winposy = -1

	function! Maxivim_MaximizeHorizontal(...)
		let l:columns = 0
		let l:window_position_cmd = ''
		let l:margin_width = 0
		if &number
			let l:margin_width = &numberwidth
		endif
		if (a:0 == 1)
			let l:columns = a:1 + l:margin_width
		elseif s:maximized_x
			let l:columns = s:saved_columns
			if s:saved_winposx >= 0
				let l:window_position_cmd = 'winpos ' . s:saved_winposx . ' ' . getwinposy()
			endif
			let s:maximized_x = 0
		else
			let l:columns = g:maxivim_max_columns + l:margin_width
			let s:saved_columns = &columns
			let s:saved_winposx = getwinposx()
			let s:maximized_x = 1
		endif
		let l:window_size_cmd = 'set columns=' . l:columns
		execute l:window_size_cmd
		if strlen(l:window_position_cmd) > 0
			execute l:window_position_cmd
		endif
	endfunction

	function! Maxivim_MaximizeVertical(...)
		let l:lines = 0
		let l:window_position_cmd = ''
		if (a:0 == 1)
			let l:lines = a:1
		elseif s:maximized_y
			let l:lines = s:saved_lines
			if s:saved_winposy >= 0
				let l:window_position_cmd = 'winpos ' . getwinposx() . ' ' . s:saved_winposy
			endif
			let s:maximized_y = 0
		else
			let l:lines = g:maxivim_max_lines
			let s:saved_lines = &lines
			let s:saved_winposy = getwinposy()
			let s:maximized_y = 1
		endif
		let l:window_size_cmd = 'set lines=' . l:lines
		execute l:window_size_cmd
		if strlen(l:window_position_cmd) > 0
			execute l:window_position_cmd
		endif
	endfunction

	function! Maxivim_MaximizeBoth(...)
		let l:columns = 0
		let l:lines = 0
		let l:winposx = -1
		let l:winposy = -1
		let l:window_position_cmd = ''
		let l:margin_width = 0
		if &number
			let l:margin_width = &numberwidth
		endif
		if (a:0 == 2)
			" columns and lines supplied as arguments;
			" unmaximize to those dimensions
			let l:columns = a:1 + l:margin_width
			let l:lines = a:2
			if s:maximized_x
				let l:winposx = s:saved_winposx
			else
				let l:winposx = getwinposx()
			endif
			if s:maximized_y
				let l:winposy = s:saved_winposy
			else
				let l:winposy = getwinposy()
			endif
			if (l:winposx >= 0) && (l:winposy >= 0)
				let l:window_position_cmd = 'winpos ' . l:winposx . ' ' . l:winposy
			endif
			let s:maximized_x = 0
			let s:maximized_y = 0
		elseif s:maximized_x && s:maximized_y
			let l:columns = s:saved_columns
			let l:lines = s:saved_lines
			if (s:saved_winposx >= 0) && (s:saved_winposy >= 0)
				let l:window_position_cmd = 'winpos ' . s:saved_winposx . ' ' . s:saved_winposy
			endif
			let s:maximized_x = 0
			let s:maximized_y = 0
		elseif s:maximized_x
			call Maxivim_MaximizeVertical()
			return
		elseif s:maximized_y
			call Maxivim_MaximizeHorizontal()
			return
		else
			let l:columns = g:maxivim_max_columns + l:margin_width
			let l:lines = g:maxivim_max_lines
			let s:saved_columns = &columns
			let s:saved_lines = &lines
			let s:saved_winposx = getwinposx()
			let s:saved_winposy = getwinposy()
			let s:maximized_x = 1
			let s:maximized_y = 1
		endif
		let l:window_size_cmd = 'set columns=' . l:columns . ' lines=' . l:lines
		execute l:window_size_cmd
		if strlen(l:window_position_cmd) > 0
			execute l:window_position_cmd
		endif
	endfunction

	function! Maxivim_ResizeToDefault(...)
		call Maxivim_MaximizeBoth(g:maxivim_default_columns, g:maxivim_default_lines)
	endfunction

	noremap <unique> <script> <Plug>Maxivim_MaximizeHorizontal	:call Maxivim_MaximizeHorizontal()<CR>
	noremap <unique> <script> <Plug>Maxivim_MaximizeVertical	:call Maxivim_MaximizeVertical()<CR>
	noremap <unique> <script> <Plug>Maxivim_MaximizeBoth		:call Maxivim_MaximizeBoth()<CR>
	noremap <unique> <script> <Plug>Maxivim_ResizeToDefault		:call Maxivim_ResizeToDefault()<CR>

	if !exists('g:maxivim_should_map_keys')
		let g:maxivim_should_map_keys = 1
	endif
	if !exists('g:maxivim_should_resize_to_default')
		let g:maxivim_should_resize_to_default = 1
	endif

	if g:maxivim_should_map_keys
		map <unique> <F5> <Plug>Maxivim_MaximizeVertical
		map <unique> <F6> <Plug>Maxivim_MaximizeBoth
		map <unique> <F7> <Plug>Maxivim_MaximizeHorizontal
		map <unique> <S-F6> <Plug>Maxivim_ResizeToDefault
	endif
	if g:maxivim_should_resize_to_default
		call Maxivim_ResizeToDefault()
	endif

endif
