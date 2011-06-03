
"------------------------------------------------------------------------------
"                     CD_Plus.vim : 'cd' accelerator  {{{
"
"
"
" Author:		Eric Arnold ( eric_p_arnold@yahoo.com )
"                             ^^^^^^^^^^^^^^^^^^^^^^^
"                             Comments, bugs, feedback welcome.
" Created:		May, 2006
" Updated:		Sun Jun 04, 06/04/2006 9:32:12 PM
" Requirements:	Vim 7
"
" Version:		1.0		Mon May 22, 05/22/2006 10:30:52 AM
" 						-	initial release
"
" Version:		1.5		Thu Jun 01, 06/01/2006 9:13:39 AM
" 						-	many minor fixes
" 						-	added filename completion, and quick switch to
"	 						edit, etc., and cmap e<space> .....
"	 					-	added separate directory and filename
"	 						histories.
"	 					-	will try to create intermediate directories.
" Version:		2.0		Sun Jun 04, 06/04/2006 9:32:12 PM
" 						-	Added literal input mode (allow non-existent
" 							names).
" 						-	Added ^F command window access.
" 						-	Display and highlighting cleaned and improved.
" 						-	Added options aliases for col wrap and
" 							autochdir.
" 						-	Added ^E ^Y passthrough for scrolling.
" 						-	Misc fixes.
"
" Start_help_section {{{
"
"
" Features:
"
" 	Gets you where you're going with the minimum possible keystrokes:
"
" 		Each key stroke is completed as it is typed.  Since I used <Tab>
" 		and ^D constantly, I figured, why not make it the default?
"
" 		A directory listing is maintained in a separate window.  The clean,
" 		scrollable/editable directory/file completion list is in a regular
" 		VIm window.
"
" 		A history is maintained individually for directories and files
" 		accessed.
"
" 		The completion is optimized for fast directory browsing, and file
" 		opening.
"
" 		You can create aliases for any directory/file.  So, if you've set an
" 		alias for '1', then ':cd 1<cr>' moves to that directory.
"
"
"
"	Key Commands:
"
"	^H <BS>		The backspace keys are a combination of single char backspace
"				and immediate history "rollback".  They attempt to go back to
"				the first valid directory completion point.
"
"	^W			Erase backwards a word/directory.
"
"	^U			Clear the line.
"
"	<TAB>		Rotate through the current list.
"	<S-TAB>		
"
"	<CR>		Execute a "cd" command to the currently shown directory.
"
"	<ESC>		Jump into the __CD__ display window.   <ESC> jumps back.
"				<CR> selects and jumps back.  Changes made to history lists
"				in the buffer are saved to the global history vars.
"
"	<C-F>		^F opens the command edit window.
"
"	<C-C>		^C quits without changing directories.
"
"	<C-N>		Forward/backward in the directory/file history.
"	<C-P>		
"
"	/			Can have special actions, see 'Navigating'
"
"	<C-E>		Scroll one line
"	<C-Y>
"
"
"	Other Commands:
"
"		The built-in commands are implemented as aliases (as are pc-drive
"		names).  You can override them, but then it's up to you to create
"		new ones.
"
"	'set '		Set an alias to the last directory displayed.
"
"	'opt '		Sets options.
"		
"			-	Display column wrap width.
"			-	Auto-change directory.
"
"	'delete '	Delete an alias.
"
"	'help '		This listing.
"
"	'history '	Browse the directory history.
"
"	'edit '		Switch to edit mode.
"
"	':'			Input other command.  <SPACE> finishes.
"
"			Most of these can be abbreviated as first-char<space>  .
"
"
"		Note:	Spaces might not be displayed at the end of a line due to
"				some problem with 'echo' and 'input()'.
"
"
"	Navigating:
"
"		Every character typed will be treated as part of a directory or alias,
"		and the display will change accordingly.  Unmatched characters will
"		generally be discarded.
"
"		The  /  character is somewhat special.  If you add a slash after a
"		directory name, it moves the display to that directory, and sets the
"		current 'input' value to null.  Therefore, typing aliases is best done
"		after a trailing  /  char.
"
"		Literal input:
"
"			'//' must be used to enter non-existing names.
"
"			Adding a / to the end of a line which already ends in / will
"			toggle the  'g:CD_any_input'  option (default off).
"			Normally, keys which don't match anything are thrown away unless
"			this option == 1.  It also affects what names can be taken
"			from the ^N/^P history.  Aliases are not completed, and must be
"			entered as .../full_alias<space>/
"
"
"
"	Aliases:
"
"	-	Aliases automatically have a space added after them to help
"		distinguish from directories of the same name.
"
"	-	Best way to see all aliases is to clear the line with ^U
"
"
"
"
"	Customizing:
"
"		The default key mapping is:
"			cnoremap <expr> cd ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('cd') : 'cd' )
"			cnoremap <expr> lcd ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('lcd') : 'lcd' )
"		
"		a simpler keymap might be:
"			nnoremap <leader>cd :call CD_Plus('cd')<CR>
"
"		let g:CD_dir_history_max = 100
"		let g:CD_scan_pc_drives = 0
"		let s:RC_file = $HOME . '/.vim_cd_plus'
"		let g:CD_autochdir = 'local'		" 'off', 'local' or 'global'
"
"
"	Caveats:
"
" 	-	Works only with 'shellslash' on.
" 	-	Restoring the command line height is a true pain in the ass.  It
" 		works most of the time, though that is little consolation.
"
" End_help_section }}}
"
"}}}



let s:This_script_fname = expand("<sfile>:p")
if v:version < 700
	echomsg "Vim 7 or higher is required for " . s:This_script_fname 
	finish
endif


if !exists('g:CD_aliases')			| let g:CD_aliases = {} 			| endif
if !exists('g:CD_scan_pc_drives')	| let g:CD_scan_pc_drives = 0		| endif
if !exists('g:CD_path_history')		| let g:CD_path_history = []		| endif
if !exists('g:CD_path_history_max')	| let g:CD_path_history_max = 100	| endif
if !exists('g:CD_dir_history')		| let g:CD_dir_history = []			| endif
if !exists('g:CD_dir_history_max')	| let g:CD_dir_history_max = 100	| endif
if !exists('g:CD_dsp_wrap_len')		| let g:CD_dsp_wrap_len = 19		| endif
if !exists('g:CD_any_input')		| let g:CD_any_input = 0			| endif
if !exists('g:CD_autochdir')		| let g:CD_autochdir = 'local'		| endif
let s:CD_history_idx = -1
let g:CD_glob_cache = {}



" Customize this function here, or copy it to the vimrc:
function! s:Init_highlight()
	hi! star term=standout ctermfg=12 guifg=Red
	hi! slash term=standout ctermfg=10 gui=bold guifg=Green
	hi! junk ctermfg=8 guifg=grey60
	hi! continue term=bold cterm=bold ctermfg=6 guifg=#00a0ff
	hi! link matched WildMenu
	hi! recent ctermfg=lightred guifg=lightred
	hi! clear ext 
	hi! ext ctermfg=8 guifg=darkyellow

	"hi! clear Normal
	"hi! Normal ctermfg=6 guifg=lightgreen

	syn clear
	syntax match star /\*/
	syntax match slash /\//
	syntax match continue />/
	syntax match ext ;\v[^.]\zs\.[^ .><]+\ze([ /*]|$);
endfunction



call extend( g:CD_aliases, { 
				\   'set '		: 'set :',		's ' : 'set :'
				\ , 'cd '		: 'cd :' 
				\ , 'lcd '		: 'lcd :' 
				\ , 'delete '	: 'delete :',	'd ' : 'delete :' 
				\ , 'edit '		: 'edit :',		'e ' : 'edit :' 
				\ , 'new '		: 'new :'
				\ , 'help '		: 'help :'
				\ , 'history '	: 'history :'
				\ ,	'opt 1 column width ( %{g:CD_dsp_wrap_len} )' : 'wrap :'
				\ ,	'opt 2 auto change directory ( %{g:CD_autochdir} )' : 'autochdir :'
				\ } 
				\ )
				"\ , ':' : 'excmd :'
				" Note: ':' isn't handled by s:Do_commands anymore


function! s:is_command( cmd )
	let cmd = a:cmd
	"let cmd = tolower( s:fnamemodify_pc( a:cmd, ':t' ) )
	if has_key( g:CD_aliases, cmd )
	\ && g:CD_aliases[ cmd ] =~ ':$'
		return g:CD_aliases[ cmd ]
	endif
	return ""
endfunction



let s:OS = ''
if has('gui_win32')
\ || has('gui_win32s')
\ || has('winaltkeys')
\ || has('win16')
\ || has('win32')
\ || has('win64')
\ || has('win32unix')
\ || has('win95')
	let s:OS = 'mswin'
endif



if s:OS == 'mswin' && g:CD_scan_pc_drives
	for i in range( 0, 255 ) 
		let a = nr2char(i)
		if a !~# '[A-Z]' | continue | endif
		let dir = a . ':'
		if has_key( g:CD_aliases, dir ) | continue | endif
		if s:isdirectory_cached( dir )
			let g:CD_aliases[ dir ] = dir . '/'
		endif
	endfor
endif



let s:RC_file = $HOME . '/.vim_cd_plus'
if filereadable( s:RC_file )
	exe 'so ' . s:RC_file
endif




function! CD_Plus( cmd, path )


	let s:tab_path_list = []
	let s:tab_path_list_idx = -1

	let s:scrolling = -1

	let s:CD_history_idx = -1

	let s:inputting_ex = 0

	let s:Cmd = a:cmd

	let old_cwd = getcwd()

	let start_bufnr = bufnr("%")

	" If cmdheight == 1, we've probably moved out of the command line 
	" into to the display window.
	if &cmdheight > 1
		let s:save_lazyredraw = &lazyredraw
		"let s:save_isfname = &isfname
		"set isfname+=:
		let s:save_cmdheight = &cmdheight
	endif


	
	let inp_char= ''
	let inp_stack = []

	let inp = a:path == '' ? getcwd() . '/' : a:path
	let s:Path = inp

	call s:Add_input_stack( inp_stack, inp )
	let paths = s:Get_paths( inp )

	let &cmdheight = 1

	let bufnr = bufnr("__CD__")
	let winnr = bufwinnr( bufnr )
	if winnr > 0
		try
			silent exe winnr . " wincmd w"
		catch
			if bufname("%") !~ '__CD__'
				echomsg 'Error, cannot access __CD__ window (' . bufname("%") . '), err: ' . v:errmsg
				return ''
			endif
		endtry
	else
		try
			silent wincmd b
			let sv = &splitbelow
			set splitbelow
			silent botright 6 split
			let &splitbelow = sv
			if bufnr > -1
				silent exe 'buf ' . bufnr
			else
				silent edit __CD__
			endif
			setlocal bufhidden=hide
		catch
		finally
			"
			" Attempt to find a reliable way to reset cmdheight:
			"
			"			aug Cd_auto
			"				au!
			"				exe "au BufHidden <buffer> set cmdheight=" . s:save_cmdheight
			"			aug END
			"exe "nmap <silent> <buffer> <esc> <esc>:set cmdheight=" . s:save_cmdheight . "<CR>:cd"
		endtry
	endif
	silent let s:cd_winheight = 6
	silent set buftype=nofile
	silent set noswapfile



	" ----------------------------------------------------------------------
	"
	"  Main processing loop, getchar() called
	"
	while 1

		let &cmdheight = 1
		let winnr = bufwinnr( bufnr("__CD__") )
		exe winnr . " wincmd w"
		exe "resize " . s:cd_winheight
		

		if s:scrolling == -1 
		\ && s:tab_path_list_idx == -1
		\ && s:CD_history_idx == -1
			"MoreMsg
			echohl LineNr 
			echon 'L O A D I N G'
			echohl NONE
			redraw
		endif


		if inp == ''
			let paths = s:Get_paths( $HOME . '/' )
		else
			let paths = s:Get_paths( inp )
		endif
		if len( paths ) < 1
			let paths = s:Get_paths( inp . '/' )
			if len( paths ) > 0
				let inp .= '/'
			endif
		endif


		let replace_trailing = matchstr( inp, '/*$' )
		let inp_tail = s:fnamemodify_pc( inp, ':t' )

		let matching_paths = s:Get_matching_paths( paths, inp )

		let matching_aliases = s:Get_matching_aliases( inp_tail )
	

		let matching_path_tails = map( deepcopy( matching_paths ), 
					\ 's:fnamemodify_pc( v:val, ":t" ) ' )
		let longest_path = s:Get_longest_common_path( matching_paths )
		let longest_alias = s:Get_longest_common_alias( matching_aliases )
		let longest_common = s:Get_longest_common_prefix( matching_path_tails + matching_aliases )


		" Give priority to directory name in current directory, over same
		" alias name:
		"
		if has_key( g:CD_aliases, longest_common )
					\&& len( matching_paths ) < 1
					"\&& longest_path !~ longest_alias
			let longest_path = g:CD_aliases[ longest_alias ] 
			let matching_paths = s:Get_paths( longest_path . '/' )
			let replace_trailing = '/'

		elseif inp == '' && longest_common == ''
			let longest_path = ''

		elseif inp != ''
			let longest_path = s:fnamemodify_pc( inp, ':h' )
			if s:isdirectory_cached( longest_path )
				let longest_path = substitute( longest_path, '/*$', '/', '' )
			endif
			let longest_path .= longest_common
		endif




		" ------------------------------------------------------------
		"
		"  Trap special cases:
		"
		if s:inputting_ex
			if inp_char == ' '
				let inp = s:fnamemodify_pc( inp, ':h' )
				if s:isdirectory_cached( inp )
					let inp = substitute( inp, '/*$', '/', '' )
				endif
				let s:inputting_ex = 0
			else
				let s:Cmd .= inp_char
				let inp = inp[0:-2]
			endif

		elseif inp =~ '/:$'
			let s:inputting_ex = 1
			let s:Cmd = ''

		elseif inp =~ '//$'
			let g:CD_any_input = !g:CD_any_input
			let inp = substitute( inp, '/*$', '/', '' )
			continue

		elseif inp =~ '/\./$'
			" Junk case
			let inp = substitute( inp, '\./$', '', '' )
			call remove( inp_stack, -1 )
			continue

		elseif inp =~ '\.\.$'
			let inp = s:fnamemodify_pc( inp, ':h' )	" remove ..
			let inp = s:fnamemodify_pc( inp, ':h' )	" remove current dir
			let inp .= '/'
			let paths = s:Get_paths( inp )
			" reset stack so backspace doesn't return to lower directory
			let inp_stack = [ inp ]
			continue

		elseif inp =~? '/\~$'
			let inp = substitute( $HOME, '\\', '/', 'g' ) . '/'
			let paths = s:Get_paths( inp )
			continue

"		elseif s:fnamemodify_pc( inp , ':t' ) =~ '^:'
"			if inp =~ '^:.*\S\+ $'
"				let cmd = s:fnamemodify_pc( inp, ':t' )
"				let cmd = substitute( cmd, '^: ', '', '' )
"				let inp = s:fnamemodify_pc( inp, ':h' )
"				let ret = s:Do_commands( cmd, inp )
"				continue
"			endif

		"elseif inp =~? s:Commands_patt
		elseif s:is_command( inp ) != ''
			let cmd = s:fnamemodify_pc( inp, ':t' )

			" Remove command aliases from the input stack:
			while s:is_command( inp ) != '' && len( inp_stack ) > 0
				let inp = remove( inp_stack, -1 )
			endwhile
			let inp = s:fnamemodify_pc( inp, ':h' )

			let ret = s:Do_commands( s:is_command( cmd ), inp )

			if ret != ''
				let inp = ret
			endif

			if s:isdirectory_cached( inp )
				let inp = substitute( inp, '/*$', '/', '' )
			endif
			let paths = s:Get_paths( inp )
			continue

		" Complete as a command, and re-try:
		elseif s:is_command( longest_common ) != ''
		"elseif longest_path =~? s:Commands_patt
			let inp = longest_common
			continue

		elseif inp =~ '[.\\]$' || inp =~ '\*'
			"
			" Allow simple globs and regex to continue unmolested.
			"
			call s:Add_input_stack( inp_stack, inp )

		elseif g:CD_any_input
			call s:Add_input_stack( inp_stack, inp )
			" nada

		elseif longest_common == ''
		\ && inp_char != ''
		\ && inp !~ '/$' 
		\ && inp =~ '/'
		\ && !g:CD_any_input
			" 
			" Throw away non-matching chars, usually typed accidentally, or
			" redundantly:
			"
			let inp = inp[0:-2]
			let inp_char = ''

			" Try again with the shorter string before continuing to getchar()
			continue
		elseif inp != ''
			let inp = longest_path == '' ? inp : longest_path
			let inp = substitute( inp, '/*$', replace_trailing, '' )
			call s:Add_input_stack( inp_stack, inp )
		endif





		" ------------------------------------------------------------
		"
		"  Print it out
		"

		if inp == ''
			" All will be matched, so filter out commands.
			let matching_aliases = filter( matching_aliases, 'g:CD_aliases[ v:val ] !~ ":$" ' )
		endif


		if s:tab_path_list_idx > -1
			let show_paths = deepcopy( s:tab_path_list )
		else
			let show_paths = deepcopy( matching_paths )
		endif

		let show_paths = map(  show_paths, 
					\   ' s:fnamemodify_pc( v:val, ":t" ) '
					\ . ' . '
					\ . '( s:isdirectory_cached( v:val ) '
					\ . ' ? "/" : "" ) ' )

		if match( longest_path, '/$' ) > -1
			" Clear aliases when ending in /, since all will be matched.
			let matching_aliases = []
		endif


		if s:scrolling > -1
			" nada
		elseif s:CD_history_idx > -1
			exe "resize " . min( [ &lines / 3, len( s:CD_history ) ] )
		else
			silent %d _
			let show_paths1 = s:Format_paths( show_paths )
			let show_aliases = s:Format_aliases( matching_aliases )
			for str in show_paths1
				call append("$", str)
			endfor
			for str in show_aliases
				call append("$", str)
			endfor
			1d _
			2match diffchange /^\s*\zs.*\ze >>/

			if s:tab_path_list_idx == -1
				let inp_tail = s:fnamemodify_pc( substitute( inp, '/*$', '', '' ), ':t' )
				if inp_tail == '' || inp_tail =~ '^\*$' || inp =~ '/$'
					match
				else
					exe 'match matched /\v(^|\s)\zs' . s:escape_path( inp_tail ) . '/'
				endif
			endif


			syn clear
			call s:Init_highlight()
			" Individual syntax highlighting
			"
			let idx = 0
			for path in show_paths
				let cmd  = ''
				let hist_idx = index( g:CD_path_history, matching_paths[idx] ) 
				if s:isdirectory_cached( path )
					let cmd = s:hl_syntax( path, 'Directory' )
				elseif path =~ '\v[-~]$'
					let cmd = s:hl_syntax( path, 'junk' )
				"elseif hist_idx > -1 && hist_idx < 20
				elseif hist_idx > -1 
					let cmd = s:hl_syntax( path, 'recent' )
				endif

				exe cmd
				let idx += 1
			endfor


			" Resize to fit
			let lines = len( show_paths1 + show_aliases )
			if lines > s:cd_winheight
				let s:cd_winheight = lines 
				exe "resize " . s:cd_winheight
			elseif lines < s:cd_winheight 
				if s:cd_winheight > ( &lines / 3 )
							\ && lines < ( &lines / 3 )
					let s:cd_winheight = ( &lines / 3 )
					exe "resize " . s:cd_winheight
				endif
			endif


		endif

		redraw

		let prompt = ( g:CD_any_input ? "[ANY]" : "" ) . ":" . s:Cmd . " "
		echon prompt 
		if s:isdirectory_cached( longest_path ) && longest_alias == ''
		elseif has_key( g:CD_aliases, longest_common ) 
		\&& longest_path == g:CD_aliases[ longest_alias ] 
			echohl DiffChange
			echon longest_common . '>>'
			echohl NONE
		else
		endif
		echon inp


		let s:Path = inp


		" ------------------------------------------------------------
		"
		"  Process a character
		"
		let inp_char = getchar()
		let inp_char = nr2char( inp_char ) == '' ? inp_char : nr2char( inp_char )

		if inp == ''
			if inp_char == '/'
				if s:isdirectory_cached( '/' )
					let inp = '/'
				elseif s:isdirectory_cached( 'C:/' )
					let inp = 'C:/'
				endif
				let paths = s:Get_paths( inp )
				continue
			else
			endif
		endif

		if	inp_char =~# "\\v(\<esc>|\<c-c>)" 
			normal :
			break
		" ------------------------------------------------------------
		elseif	inp_char =~# "\\v(\<c-w>|\<c-h>|\<bs>)" 
			if len( inp_stack ) > 1 && inp_char !~ "\<c-w>"
				call remove( inp_stack, -1 )
				let inp = remove( inp_stack, -1 )
				let paths = s:Get_paths( inp )
			elseif strlen( inp ) > 0
				let inp = s:fnamemodify_pc( inp, ':h' )
				let paths = s:Get_paths( inp )
				let inp_stack = []
			else
				let inp = ''
				let inp_stack = []
			endif

		" ------------------------------------------------------------
		elseif	inp_char =~# "\<c-l>"
			redraw

		" ------------------------------------------------------------
		elseif	inp_char =~# "\<c-u>"
			let inp = ''

		" ------------------------------------------------------------
		"  Next in history
		"
		elseif	inp_char =~# "\<c-n>"
			if s:CD_history_idx == -1
				if s:Cmd =~? 'cd'
					let s:CD_history = g:CD_dir_history 
				else
					let s:CD_history = g:CD_path_history 
				endif
				call s:Fill_history( s:CD_history )
				silent $-1
			endif
			let s:CD_history_idx -= 1
			if s:CD_history_idx < 0
				let s:CD_history_idx = len( s:CD_history ) - 1
			endif
			let inp = s:CD_history[ s:CD_history_idx ]

			silent! exe '/' . escape( inp, ' [].-~/\\' ) . '$'

		" ------------------------------------------------------------
		"  Previous in history
		"
		elseif	inp_char =~# "\<c-p>"
			if s:CD_history_idx == -1
				if s:Cmd =~? 'cd'
					let s:CD_history = g:CD_dir_history 
				else
					let s:CD_history = g:CD_path_history 
				endif
				call s:Fill_history( s:CD_history )
				silent 0
			endif
			let s:CD_history_idx += 1
			if s:CD_history_idx >= len( s:CD_history )
				let s:CD_history_idx = 0
			endif
			let inp = s:CD_history[ s:CD_history_idx ]

			silent! exe '?' . escape( inp, ' [].-~/\\' ) . '$'

		" ------------------------------------------------------------
		"  Handle when inputting / means 'change directory'
		"
		"elseif	inp_char == "/" && inp !~ '^\a:/$' && inp !~ '/$'
		elseif	inp_char == "/" && inp !~ '/$'
			" Don't match trailing / so it can be used as a toggle for
			" g:CD_any_input

			let inp = substitute( inp, '/*$', '', '' )

			" Expand any globs:
			"
			let paths1 = s:Get_paths( inp )
			if len( paths1 ) > 0
				" Set inp to the first match, and then find matches
				" under that.
				let inp = paths1[0]
				let inp .= '/'
				let paths = s:Get_paths( inp )
			endif

			if len( paths ) < 1 && !s:isdirectory_cached( inp ) && !filereadable( inp )
				let tail = s:fnamemodify_pc( inp, ':t' )
				if has_key( g:CD_aliases, tail )
					let inp = g:CD_aliases[ tail ]
					if s:isdirectory_cached( inp )
						let inp .= '/'
					endif
					continue
				elseif input("Create directory? ") =~? '^y'
					call mkdir( inp, "p" )
				endif
			endif


		" ------------------------------------------------------------
		elseif	inp_char =~# "\\v(\<tab>|\<S-Tab>)" 
			if s:tab_path_list_idx == -1
				let s:tab_path_list_idx = 0
				if len( matching_paths ) < 1
					let inp = substitute( inp, '/*$', '/', '' )
					let paths = s:Get_paths( inp )
					if len( paths ) > 0
						let matching_paths = paths
					else
					endif
				endif
				let s:tab_path_list = matching_paths " + matching_aliases

				"let s:tab_path_list = filter( matching_paths, 'v:val !~ ''\.\+$'' ' )

			elseif len( s:tab_path_list ) > 0

				if	inp_char =~# "\<tab>"
					let s:tab_path_list_idx = s:tab_path_list_idx == len( s:tab_path_list ) - 1
								\ ? 0 : s:tab_path_list_idx + 1
				else
					let s:tab_path_list_idx = s:tab_path_list_idx == 0 
								\ ? len( s:tab_path_list ) - 1 : s:tab_path_list_idx - 1
				endif

			endif

			let inp = s:tab_path_list[ s:tab_path_list_idx ]

			let inp_tail = s:fnamemodify_pc( inp, ':t' )

if 0
			if has_key( s:highlight_table, s ) 
			elseif has_key( s:highlight_table, s . '/' ) 
				let s .= '/'
			else
				let s = ''
			endif
			"echomsg ' has_key( ' . s:highlight_table[s] . ',' . s . ')' 
			"echomsg string( s:highlight_table )
			"
"			let recent_list = {}
"			for h in g:CD_path_history[ 0 : 20 ]
"				let recent_list[ s:fnamemodify_pc( h ) ] = 1
"			endfor
			
			syn clear recent

			if s != ''
				let s3 = s:highlight_table[ s ]
				"echomsg string( s:highlight_table )
				"echomsg s3
				let l1 = matchlist( s3, '%\(\d*\)l', 0, -1 )
				let l2 = matchlist( s3, '%\(\d*\)c', 0, -1 )
				"echomsg string(l1)
				"echomsg string(l2)
				if len( l1 ) > 1 && len( l2 ) > 1
					echomsg  'cursor( ' . l1[1] .','. l2[1] 
					call cursor( l1[1], l2[1] )
				endif
				"if has_key( recent_list, inp_tail )
			endif
endif


			let regex = s:hl_regex( inp_tail )
			if regex == ''
				let regex = s:hl_regex( inp_tail . '/' )
			endif

			if regex != ''
				let do = 'match matched ;' . regex . ';'
				exe do
			endif

			" End: tab handling
			"
			" ------------------------------------------------------------


		" ------------------------------------------------------------
		elseif	inp_char =~# "\\v(\<c-y>)" 
			exe "normal! \<c-y>"
			let s:scrolling = 1
			let inp_char = ''

		" ------------------------------------------------------------
		elseif	inp_char =~# "\\v(\<c-e>)" 
			exe "normal! \<c-e>"
			let s:scrolling = 1
			let inp_char = ''


		" ------------------------------------------------------------
		elseif	inp_char =~# "\\v(\<c-f>)" 
			let g:CD_Plus_return_to = 1
			call histadd( "cmd", s:Cmd . ' ' . inp )
			call feedkeys( "q:", "t")
			return


		" ------------------------------------------------------------
		"  Execute input and return, Part 1
		"
		elseif	inp_char =~# "\\v(\<cr>|\<nl>)" 
			if inp == ''
				let inp = $HOME
			endif
			if !s:isdirectory_cached( inp ) && !filereadable( inp )
				if s:Cmd =~ 'cd'
					let dir = inp
				else
					let dir = s:fnamemodify_pc( inp, ':h' )
				endif
				if input("Create directory? ") =~? '^y'
					call mkdir( dir, "p" )
					if !s:isdirectory_cached( dir )
						echomsg inp . " is not a directory."
					else
						call CD_Plus_add_history( inp )
					endif
				endif
			else
				call CD_Plus_add_history( inp )
			endif

			call s:Save_cfg()

			break

		" ------------------------------------------------------------
		"  Default
		"
		else 
			let inp .= inp_char
			let inp = substitute( inp, '^\s*', '', '' )
			let paths = s:Get_paths( inp )
		endif


		" ------------------------------------------------------------
		"  Reset various state indicators:
		"
		if	inp_char !~# "\\v(\<tab>|\<S-Tab>)" && s:tab_path_list_idx != -1
			let s:tab_path_list_idx = -1
			match
		endif

		if	inp_char !~# "\\v(\<c-n>|\<c-p>)" && s:CD_history_idx != -1
			let s:CD_history_idx = -1
			match
		endif

		if	inp_char != '' && inp_char !~# "\\v(\<c-e>|\<c-y>)" && s:scrolling != -1
			let s:scrolling = -1
		endif

	endwhile
	"
	"  End: Main processing loop
	"
	" ----------------------------------------------------------------------


	let g:CD_aliases[ '-' ] = old_cwd

	" ----------------------------------------------------------------------
	"  Set up key maps for switching between the __CD__ buffer and the
	"  command line.
	"
	let bufnr = bufnr("__CD__")
	let winnr = bufwinnr( bufnr )
	if winnr > 0
		exe winnr . " wincmd w"
		"let &cmdheight = s:save_cmdheight
		if inp_char == "\<ESC>"
			exe "nnoremap <silent> <buffer> <esc> <esc>"
						\ . ":call CD_Plus( '" . s:Cmd . "' , '" . inp . "') <CR>"
			exe "nnoremap <silent> <buffer> <CR> "
						\ . ":call CD_Plus_return_to( '" . s:Cmd . "' , '" . inp . "', 'CR' ) <CR>"
			exe "nnoremap <silent> <buffer> <2-leftmouse> "
						\ . ":call CD_Plus_return_to( '" . s:Cmd . "' , '" . inp . "', '2-leftmouse' ) <CR>"
		else
			let &lazyredraw = s:save_lazyredraw
			"let &isfname = s:save_isfname
			let &cmdheight = s:save_cmdheight
			silent hide
			exe bufwinnr( start_bufnr ) . " wincmd w"
		endif
	else
		exe bufwinnr( start_bufnr ) . " wincmd w"
	endif


	" ------------------------------------------------------------
	"  Execute input and return, Part 2
	"
	if	inp_char =~ "\\v(\<cr>|\<nl>)" 
		let do = s:Cmd . " " . inp
		try 
			exe do
		catch /\v(E37|E89|E162)/
			if s:Cmd =~? '\v(^%[edit]|%[buffer])'
				let do = "new " . inp
				exe do
			endif
		endtry

		redraw
		call histadd( "cmd", do )
		echo do

		if s:Cmd !~ 'cd'
			if g:CD_autochdir ==? 'local'
				let do = 'lcd'
			elseif g:CD_autochdir ==? 'global'
				let do = 'cd'
			else
				let do = ''
			endif
			if do != ''
				if !s:isdirectory_cached( inp )
					let inp = s:fnamemodify_pc( inp, ':h' )
				endif
				let do .= ' ' . inp
				exe do
			endif
		endif

	endif

endfunction
" End:  function! CD_Plus( cmd, path )




aug CD_auto
	au!
	au BufWinEnter * call CD_Plus_add_history( s:fnamemodify_pc( expand("%"), ':p' ) )
	au BufHidden __CD__ let &cmdheight = s:save_cmdheight
	" There is no good buffer or window event when returning from the
	" command window, so use CursorMoved:
	au CursorMoved __CD__	if g:CD_Plus_return_to && nr2char( getchar(1) ) != 'q' 
	au CursorMoved __CD__		call CD_Plus_return_to( s:Cmd, s:Path, "\<c-f>" ) 
	au CursorMoved __CD__	endif
aug END




let g:CD_Plus_return_to = 0

function! CD_Plus_return_to( cmd, path, key )

	let g:CD_Plus_return_to = 0

	let path = a:path
	let contents = getline( "." )
	let col = col(".")

	"let word = matchstr( contents, '\<\S*\%' . ( col ) . 'c\S*\>' )
	"let word1 = matchstr( contents, '\S*\%#\S*' )
	let word = matchstr( contents, '\S*\%' . ( col ) . 'c\S*' )

	if a:key =~ "\\v(\<c-f>)"
		call CD_Plus( a:cmd, a:path )
		return
	elseif a:key =~ "\\v(\<CR>|CR|\<2-leftmouse>|2-leftmouse)"
		if s:CD_history_idx == -1
			let path = substitute( path, '/*$', '/', '' )
			let path .= word
		else
			let path = word
		endif
	endif

	if s:CD_history_idx != -1
		let sv = @/
		silent %s/^\d*\s*//
		let @/ = sv
		let hist = reverse( getline(1,"$") )
		if s:Cmd =~? 'cd'
			let g:CD_dir_history = hist
		else
			let g:CD_path_history = hist
		endif
	endif

	if !s:isdirectory_cached( path ) && !filereadable( path )
		let g:CD_any_input = 1
	endif

	call CD_Plus( a:cmd, path )

endfunction




function! CD_Plus_add_history( path )
	if s:isdirectory_cached( a:path )
		let idx = index( g:CD_dir_history, a:path )
		if idx > -1
			call remove( g:CD_dir_history, idx )
		endif
		call extend( g:CD_dir_history, [ a:path ], 0 )

		while len( g:CD_dir_history ) > g:CD_dir_history_max
			call remove( g:CD_dir_history, -1 )
		endwhile
	else
		if a:path =~ '\v(__CD__|vim_cd_plus|^$)'
			return
		endif
		let idx = index( g:CD_path_history, a:path )
		while idx > -1
			call remove( g:CD_path_history, idx )
			let idx = index( g:CD_path_history, a:path )
		endwhile
		call extend( g:CD_path_history, [ a:path ], 0 )

		while len( g:CD_path_history ) > g:CD_path_history_max
			call remove( g:CD_path_history, -1 )
		endwhile
	endif
endfunction




function! s:Fill_history( hist )

	%d _

	let hist = copy( a:hist )
	let idx = 0
	"for path in reverse( hist )
	for path in hist
		call append( 0, printf( "%-3d ", idx ) . path )
		"call append( 0, path )
		let idx += 1
	endfor
	exe "resize " . min( [ &lines / 3, len( hist ) ] )

	$d _
	redraw

	return

	try
		let idx = input("Enter # ")
		if idx == '' || idx < 0 || idx > len( g:CD_dir_history ) - 1
			"hide
			return ''
		endif
	catch
		" Catch ^C
	endtry

	return g:CD_dir_history[ len(g:CD_dir_history) - idx ]
endfunction




function! s:Do_commands( cmd, dir )

	if a:cmd =~? '^history'
		return s:Fill_history()
	endif

	if a:cmd =~? '^set'
		let prompt = 'Set alias for ' . a:dir . ' : '
	elseif a:cmd =~? '^delete'
		%d _
		let l = filter( keys(g:CD_aliases) , 'g:CD_aliases[ v:val ] !~ ":$" ' )
		call append("$",  s:Format_aliases( l ) )
		2match diffchange /^\s*\zs.*\ze >>/
		exe 'resize ' . len(l)
		redraw
		let prompt = 'Delete alias for : '
	elseif a:cmd =~? '^excmd'
		let prompt = 'Enter ex : '
	elseif a:cmd =~? '^help'
		call CD_Plus_help_extract_tmp_buf()
		return a:dir
	elseif a:cmd =~? '^wrap'
		let prompt = 'enter column wrap width : '
	elseif a:cmd =~? '^autochdir'
		let prompt = 'Enter autochdir ( "local", "global", or "off" ) : '
	else
		let s:Cmd = a:cmd
		let s:Cmd = substitute( s:Cmd, '\s*:$', '', '' )
		return a:dir
	endif

	let cmd = input( prompt, "" )

	if cmd == '' | return a:dir | endif

	if a:cmd =~? '^set'
		let g:CD_aliases[ cmd . ' ' ] = a:dir
	elseif a:cmd =~? '^wrap'
		let g:CD_dsp_wrap_len = str2nr( cmd )
	elseif a:cmd =~? '^autochdir'
		let cmd = cmd[0]
		if cmd ==? 'g'
			let g:CD_autochdir = 'global'
		elseif cmd ==? 'l'
			let g:CD_autochdir = 'local'
		else
			let g:CD_autochdir = 'off'
		endif
	elseif a:cmd =~? '^delete'
		silent! call remove( g:CD_aliases, cmd )
		silent! call remove( g:CD_aliases, cmd . ' ' )
	elseif a:cmd =~? '^excmd'
		let s:Cmd = cmd
	endif

	call s:Save_cfg()
	return a:dir
endfunction



function! s:Add_input_stack( inp_stack, inp )

	" Don't add to stack when tabbing is active:
	if s:tab_path_list_idx > -1 | return | endif

	if len(a:inp_stack) > 0 && a:inp == a:inp_stack[-1]
		" Don't add duplicates
	else
		call add( a:inp_stack, a:inp )
	endif
endfunction




function! s:escape_path( s )
	return escape( a:s, ' <>[].-~' )
endfunction



let s:isdirectory_cached_cache = {}
function! s:isdirectory_cached( dir )
	if a:dir == '' | return | endif
	if has_key( s:isdirectory_cached_cache, a:dir )
		"echomsg 'cached ' . a:dir
	else
		let s:isdirectory_cached_cache[ a:dir ] = isdirectory( a:dir )
	endif
	return s:isdirectory_cached_cache[ a:dir ]
endfunction




function! s:hl_syntax( path, hl )
	"echomsg 'path ' . a:path . ' , hl=' . a:hl

	let regex = s:hl_regex( a:path )
	if regex == ''
		let regex = s:hl_regex( a:path . '/' )
	endif

	let do = ''
	if regex != ''
		let do = 'syn match ' . a:hl . ' ;' . regex . ';'
	endif
	return do
endfunction



function! s:hl_regex( path )
	if !has_key( s:highlight_table, a:path )
		"echomsg "CD_Plus err: bad path arg to hl_regex()"
		return ''
	endif

	if has_key( s:highlight_table[ a:path ], 'regex' )
		return s:highlight_table[ a:path ].regex
	else
		let s:highlight_table[ a:path ].regex = ''
	endif

	let regex = '\v('
	for idx in range( 0, len( s:highlight_table[ a:path ].chunks ) - 1 )
		if idx > 0
			let regex .= '|'
		endif
		let regex .= '%'
					\ . s:highlight_table[ a:path ].chunks_coord[idx].line
					\ . 'l'
					\ . '%'
					\ . s:highlight_table[ a:path ].chunks_coord[idx].col
					\ . 'c'
					\ . repeat( '.', strlen( s:highlight_table[ a:path ].chunks_raw[idx] ) )
	endfor

	"let regex .= ')\ze($|[* ])'
	let regex .= ')'

	let s:highlight_table[ a:path ].regex = regex

	return regex

endfunction





let s:old_paths = []

function! s:Format_paths( path_tails )
	if len( a:path_tails ) < 1 | return [] | endif

	if exists('s:old_path_tails') && a:path_tails == s:old_path_tails
		return s:out_lines
	endif
	let s:old_path_tails = a:path_tails

	let path_tails = deepcopy( a:path_tails )
	let path_lens = deepcopy( path_tails )
	call map( path_lens, 'strlen(v:val)' )
	let longest_path = max( path_lens )
	let longest = min( [longest_path, g:CD_dsp_wrap_len ] )		" 19+1 space divides evenly usually

	if len( path_tails ) < 8
		let longest = longest_path
	endif

	let s:out_chunks = []

	" First, wrap file names which are too long:
	"
	unlet! s:highlight_table
	let s:highlight_table = {}

	for path in deepcopy( path_tails )

		let path_copy = path
		"let path_copy = substitute( path, '/*$', '', '' )
		let s:highlight_table[ path_copy ] = {}

		" MSwin isn't supported correctly:
		if !s:isdirectory_cached( path_copy )
			if s:OS == 'mswin'
				let e = path_copy =~ '\v(\.exe|\.bat|\.cmd)'
			else
				let e = executable( path_copy )
			endif
			let path = ( e ) ? path . "*" : path
		endif

		let part = ''
		let s:highlight_table[ path_copy ].chunks = []
		let s:highlight_table[ path_copy ].chunks_raw = []
		let fmt = "%-" . longest . "." . longest . "s "

		while strlen( path ) > longest 
			let part = strpart( path, 0, longest - 1 - 1 ) . '>'
			let s:highlight_table[ path_copy ].chunks_raw += [ part ]
			let path = '>' . strpart( path, longest - 1 - 1 ) . ' '
			let seg = printf( fmt, part )
			let s:out_chunks += [ seg ]
			let s:highlight_table[ path_copy ].chunks += [ seg ]
		endwhile

		let s:highlight_table[ path_copy ].chunks_raw += [ path ]
		let seg = printf( fmt, path )
		let s:out_chunks += [ seg ]
		let s:highlight_table[ path_copy ].chunks += [ seg ]

	endfor


	let cols = 0 + ( winwidth(winnr()) / ( longest + 1 ) ) 
	let rows = 1 + ( len( s:out_chunks ) / cols ) 

	let idx = 0
	let paths = deepcopy( a:path_tails )
	let s:out_lines = []

	let path_ref = remove( paths, 0 )
	let s:highlight_table[ path_ref ].chunks_coord = []

	for col in range( 0, cols - 1 )
		for row in range( 0, rows - 1 )

			call extend( s:highlight_table[ path_ref ].chunks_coord, [ { 'line' : 1, 'col' : 1 } ] )
			let s:highlight_table[ path_ref ].chunks_coord[idx].line = row + 1
			let s:highlight_table[ path_ref ].chunks_coord[idx].col = 
						\ ( ( col * ( longest + 1 ) ) + 1 )
			" Initialize
			while row >= len( s:out_lines )
				call add( s:out_lines, '' )
			endwhile

			let s:out_lines[row] .= s:highlight_table[ path_ref ].chunks[ idx ]

			let idx += 1
			if idx >= len( s:highlight_table[ path_ref ].chunks )
				let idx = 0
				if len( paths ) < 1
					return s:out_lines
				endif

				let path_ref = remove( paths, 0 )
				let s:highlight_table[ path_ref ].chunks_coord = []
			endif

		endfor
	endfor

	return s:out_lines

endfunction





function! s:Format_aliases( aliases )
	if len( a:aliases ) < 1 | return [] | endif
	let alias_lens = deepcopy( a:aliases )
	call map( alias_lens, 'strlen(v:val)' )
	let longest = max( alias_lens )
	let longest = min( [longest, 20] )
	let fmt = "%" . longest . "s "
	let out_aliases = []
	let col = 1
	let lines = 0
	for alias in a:aliases
		function! s:expand_opt(...)
			return eval( a:1 )
		endfunction
		let alias1 = substitute( alias, '%{\([^}]*\)}', '\=s:expand_opt( submatch(1) )', '' )
		let alias1 = printf( fmt, alias1 ) . '>>' . g:CD_aliases[ alias ]
		let out_aliases += [ alias1 ]
		let lines += 1
	endfor

	return out_aliases
endfunction




function! s:Get_paths( in_path )

	let in_path = a:in_path

	" On windows, at least, reading the directory files can be slow:
	let key = s:Cmd . ' ' . in_path
	if has_key( g:CD_glob_cache, key )
		if g:CD_glob_cache[ key ].timestamp ==
		\ getftime( g:CD_glob_cache[ key ].dir )
			"echomsg 'returning cached ' . key
			return g:CD_glob_cache[ key ].paths 
		endif
	endif

	if s:isdirectory_cached( in_path )
		let dir = in_path
	elseif s:isdirectory_cached( s:fnamemodify_pc( in_path, ':h' ) )
		let dir = s:isdirectory_cached( s:fnamemodify_pc( in_path, ':h' ) )
	else
		"echomsg 'CD_Plus err: cannot find directory for path (' . in_path . ')'
		return []
	endif

	let g:CD_glob_cache[ key ] = {}
	let g:CD_glob_cache[ key ].dir = dir

	let g:CD_glob_cache[ key ].timestamp =
		\ getftime( g:CD_glob_cache[ key ].dir )

	let paths = []

	let in_path = in_path =~ '\*$' ? in_path : in_path . '*'

	let globs = glob( in_path )
	if in_path =~ '/\*$'

		" get directories normally hidden with a "."
		"
		let globs .= "\n" . glob( substitute( in_path, '/\*$', '/.*', '' ) )
	endif

	for file in split( globs, "\n" )
		if s:isdirectory_cached( file ) || s:Cmd !~? 'cd'
			" 
			" 'cd' only wants to see directory types:
			"
			let paths += [ file ]
		endif
	endfor

	let paths = sort( paths )

	let g:CD_glob_cache[ key ].paths = paths

	return paths

endfunction





function! s:Get_matching_paths( paths, look )
	let paths1 = []
	let look = a:look

	" escape dots if they aren't part of a .*
	"
	let look = substitute( look, '\([^\\]\)\.\([^*]\)', '\1\\.\2', 'g' )

	" make file type glob * into .*
	"
	let look = substitute( look, '\([^.]\)\*', '\1.*', 'g' )

	let look = escape( look, ' ' )

	for path in a:paths
		try
			if path =~? '^' . look 
				let path = substitute( path, '/*$', '', '' )
				let paths1 += [ path ]
			endif
		catch
			" Throw away regex errors
		endtry
	endfor
	return paths1
endfunction



function! s:Get_matching_aliases( look )
	let out_list = []
	for alias in sort( keys( g:CD_aliases ) )
		try
			if alias =~? '^' . a:look
				let out_list += [ alias ]
			endif
		catch
		endtry
	endfor
	return out_list
endfunction





function! s:Get_longest_common_path( paths )
	return s:Get_longest_common_prefix( a:paths )
endfunction




function! s:Get_longest_common_alias( list )
	if len( a:list ) < 1 | return '' | endif
	return s:Get_longest_common_prefix( a:list )
endfunction



function! s:Get_longest_common_prefix( list )
	let longest = ''
	for elem in a:list
		if strlen( longest ) < strlen( elem )
			let longest = elem
		endif
	endfor
	for elem in a:list
		while strlen( longest ) > 0
			if longest ==? elem[0:strlen(longest)-1]
				break
			endif
			let longest = longest[0:-2]
		endwhile
	endfor
	return longest
endfunction



function! s:Save_cfg()
	let s:RC_file = $HOME . '/.vim_cd_plus'
	silent 1 split
	try
		silent exe 'edit ' . s:RC_file
		setlocal bufhidden=wipe
		%d _

		call append("$", 'call extend( g:CD_aliases, { ' )
		for key in keys( g:CD_aliases ) 
			call append("$", "\\ '" . key . "' : '" . g:CD_aliases[key] .  "'," )
		endfor
		call append("$", "\\ } ) " )

		call append("$", 'let g:CD_dir_history = [' )
		for dir in g:CD_dir_history 
			call append("$", "\\ '" . dir . "'," )
		endfor
		call append("$", "\\ ]" )

		call append("$", 'let g:CD_path_history = [' )
		for path in g:CD_path_history 
			call append("$", "\\ '" . path . "'," )
		endfor
		call append("$", "\\ ]" )

		silent write!

	catch
		echomsg "Caught error " . v:errmsg . " when trying to save to config file, " . s:RC_file
	finally
		silent bwipe!
	endtry


endfunction



function! s:fnamemodify_pc( fname, flags )

	" The default tail option doesn't handle C:/somedir
	"
	if a:flags =~ ':t' 
		if match( a:fname, '/[^/]\+$' ) > -1
			return matchstr( a:fname, '[^/]\+$' )
		elseif a:fname !~ '/'
			return a:fname
		endif
	endif

	let pc_drive = matchstr( a:fname, '^\a:' )
	let fname = substitute( a:fname, '^\a:', '', '' )
	let fname = fnamemodify( fname, a:flags )
	if fname =~ '^/'
		let fname = pc_drive . fname
	endif
	return fname
endfunction



function! CD_Plus_help_extract_tmp_buf()

	" Use new then edit, as a simple way to deal with an existing hidden 
	" help buffer.
	wincmd t
	silent new 20
	silent setlocal hidden=wipe
	silent edit! _WinWalker Help_
	silent setlocal modifiable
	silent setlocal noreadonly
	"silent setlocal nofoldenable
	" Refreshing each time is mostly useful for development, but doesn't hurt
	" in general:
	silent 1,$d
	exe "silent read " . s:This_script_fname
	silent 1,/^["]*\s*Start_help_section/ d
	silent /^["]*\s*End_help_section/,$ d
	silent 1,$ s/^"//

	silent 1

	silent setlocal nomodifiable
	silent setlocal readonly
	silent setlocal buftype=nofile
	silent setlocal filetype=help
	silent setlocal noswapfile
	"silent setlocal foldenable
	"silent set foldopen=hor,jump,mark,search
	"silent set foldclose=all
	"silent setlocal foldmarker={{{,}}}
	"silent setlocal foldmethod=marker
	"silent setlocal foldtext=RH_foldtext1()
	"silent setlocal syntax=help
endfunction 


silent! cunmap cd

function! CD_Plus_start( cd_cmd )
   return ":call CD_Plus('" . a:cd_cmd . "', '')\<cr>"
endfunction

cnoremap <silent> <expr> e<space> ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('e') : 'e' )
cnoremap <expr> cd ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('cd') : 'cd' )
cnoremap <expr> lcd ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('lcd') : 'lcd' )


" Failed attemps, for future reference:
"

"function! CD_Plus_start()
   "call feedkeys(":call CD_Plus()\<cr>", 't')
   "return ''
"endfunction
"cnoremap cd <c-r>=getcmdpos()==1 && getcmdtype() == ':' ? CD_Plus_start():'cd'<cr>

"cnoremap <silent> cd<space> call CD_Plus()<CR>
"cnoremap <silent> ccd call CD_Plus()<CR>
"nnoremap <silent> ,cd :call CD_Plus()<CR>

"cnoremap <silent> cd echo getcmdpos()<CR>
"cnoremap <silent> cd if getcmdpos() < 3 <bar> call CD_Plus() <bar> else <bar> call feedkeys('cd','n') <bar> call setcmdpos(1) <bar> end <CR>
"cnoremap <expr> cd ( getcmdpos() == 1 ? CD_Plus() : 'cd' )
"cnoremap <silent> <expr> cd CD_Plus()
"abbr <silent> <expr> cd CD_Plus()



" vim7:tw=75:ts=4:sw=4:foldenable:foldmarker={{{,}}}:foldmethod=marker:foldopen=hor,tag,jump,mark,search:noexpandtab:foldclose=
"
