
"------------------------------------------------------------------------------
"                     CD_Plus.vim : 'cd' accelerator  {{{
"
"
"
" Author:		Eric Arnold ( eric_p_arnold@yahoo.com )
"                             ^^^^^^^^^^^^^^^^^^^^^^^
"                             Comments, bugs, feedback welcome.
" Created:		May, 2006
" Updated:		Mon May 22, 05/22/2006 10:30:41 AM
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
"
"
"
" Features:
"
" 	Gets you where you're going with the minimum possible keystrokes:
"
" 		Each key stroke is completed as it is typed, and a directory
" 		listing is maintained in a separate window.
"
" 		The completion is optimized for fast directory browsing, and file
" 		opening.
"
" 		You can create aliases for any directory.  So, if you've set an
" 		alias for '1', then ':cd 1<cr>' moves to that directory.
"
"
" Start_help_section {{{
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
"	<C-C>		^C quits without changing directories.
"
"	<C-N>		Forward/backward in the directory/file history.
"	<C-P>		
"
"	/			Can have special actions, see 'Navigating'
"
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
"			toggle the  'g:Allow_any_input'  option (default off).
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
"
"
"
"	Caveats:
"
" 	-	Works only with 'shellslash' on.
" 	-	Restoring the command line height is a true pain in the ass.  It
" 		works better than half the time, though that is no consolation.
"
" End_help_section }}}
"
"}}}



let s:This_script_fname = expand("<sfile>:p")
if v:version < 700
	echomsg "Vim 7 or higher is required for " . s:This_script_fname 
	finish
endif


if !exists('g:CD_aliases')					| let g:CD_aliases = {} 			| endif
if !exists('g:CD_scan_pc_drives')			| let g:CD_scan_pc_drives = 0		| endif
if !exists('g:CD_path_history')				| let g:CD_path_history = []		| endif
if !exists('g:CD_path_history_max')			| let g:CD_path_history_max = 100	| endif
if !exists('g:CD_dir_history')				| let g:CD_dir_history = []			| endif
if !exists('g:CD_dir_history_max')			| let g:CD_dir_history_max = 100	| endif
if !exists('g:CD_Plus_name_wrap_len')		| let g:CD_Plus_name_wrap_len = 19	| endif
if !exists('g:Allow_any_input')				| let g:Allow_any_input = 0			| endif
let s:CD_history_idx = -1


call extend( g:CD_aliases, { 
				\   'set '		: 'set :',		's ' : 'set :'
				\ , 'cd '		: 'cd :' 
				\ , 'lcd '		: 'lcd :' 
				\ , 'delete '	: 'delete :',	'd ' : 'delete :' 
				\ , 'edit '		: 'edit :',		'e ' : 'edit :' 
				\ , 'new '		: 'new :'
				\ , 'help '		: 'help :'
				\ , 'history '	: 'history :'
				\ } 
				\ )
				"\ , ':' : 'excmd :'
				" Note: ':' isn't handled by s:Do_commands anymore


function! s:is_command( cmd )
	let cmd = tolower( s:fnamemodify_pc( a:cmd, ':t' ) )
	if has_key( g:CD_aliases, cmd )
	\ && g:CD_aliases[ cmd ] =~ ':$'
		return g:CD_aliases[ cmd ]
	endif
	return ""
endfunction



if g:CD_scan_pc_drives
	for i in range( 0, 255 ) 
		let a = nr2char(i)
		if a !~# '[A-Z]' | continue | endif
		let dir = a . ':'
		if has_key( g:CD_aliases, dir ) | continue | endif
		if isdirectory( dir )
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

	call s:Add_input_stack( inp_stack, inp )
	let paths = s:Get_paths( inp )

	let &cmdheight = 1

	let bufnr = bufnr("__CD__")
	let winnr = bufwinnr( bufnr )
	if winnr > 0
		try
			silent exe winnr . " wincmd w"
		catch
			echomsg 'Error, cannot access __CD__ window'
			return ''
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
			if isdirectory( longest_path )
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
				if isdirectory( inp )
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
			let g:Allow_any_input = !g:Allow_any_input
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
			echomsg 'ret:'.ret.',cmd:'.cmd

			if ret != ''
				let inp = ret
			endif

			if isdirectory( inp )
				let inp = substitute( inp, '/*$', '/', '' )
			endif
			let paths = s:Get_paths( inp )
			continue

		" Complete as a command, and re-try:
		elseif s:is_command( longest_common ) != ''
		"elseif longest_path =~? s:Commands_patt
			let inp = longest_common
			continue

		elseif g:Allow_any_input
			call s:Add_input_stack( inp_stack, inp )
			" nada

		elseif inp =~ '[.\\]$' || inp =~ '\*'
			"
			" Allow simple globs and regex to continue unmolested.
			"
			call s:Add_input_stack( inp_stack, inp )

		elseif longest_common == ''
		\ && inp_char != ''
		\ && inp !~ '/$' 
		\ && inp =~ '/'
		\ && !g:Allow_any_input
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
					\ . '( isdirectory( v:val ) '
					\ . ' ? "/" : "" ) ' )

		if match( longest_path, '/$' ) > -1
			" Clear aliases when ending in /, since all will be matched.
			let matching_aliases = []
		endif



		if s:CD_history_idx > -1
			exe "resize " . min( [ &lines / 3, len( s:CD_history ) ] )
		else
			silent %d _
			let show_paths = s:Format_paths( show_paths )
			let show_aliases = s:Format_aliases( matching_aliases )
			for str in show_paths
				call append("$", str)
			endfor
			for str in show_aliases
				call append("$", str)
			endfor
			1d _
			2match diffchange /^\s*\zs.*\ze >>/

			let lines = len( show_paths + show_aliases )
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

		let prompt = ( g:Allow_any_input ? "+" : "" ) . ":" . s:Cmd . " "
		echon prompt 
		if isdirectory( longest_path ) && longest_alias == ''
		elseif has_key( g:CD_aliases, longest_common ) 
		\&& longest_path == g:CD_aliases[ longest_alias ] 
			echohl DiffChange
			echon longest_common . '>>'
			echohl NONE
		else
		endif
		echon inp




		" ------------------------------------------------------------
		"
		"  Process a character
		"
		let inp_char = getchar()
		let inp_char = nr2char( inp_char ) == '' ? inp_char : nr2char( inp_char )

		if inp == ''
			if inp_char == '/'
				if isdirectory( '/' )
					let inp = '/'
				elseif isdirectory( 'C:/' )
					let inp = 'C:/'
				endif
				let paths = s:Get_paths( inp )
				continue
			else
			endif
		endif

		if	inp_char =~# "\\(\<esc>\\|\<c-c>\\)" 
			normal :
			break
		" ------------------------------------------------------------
		elseif	inp_char =~# "\\(\<c-w>\\|\<c-h>\\|\<bs>\\)" 
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
		"elseif	inp_char == "/" && inp !~ '^\a:/$' && inp !~ '/$'
		elseif	inp_char == "/" && inp !~ '/$'
			" Don't match trailing / so it can be used as a toggle for
			" g:Allow_any_input

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

			if len( paths ) < 1 && !isdirectory( inp ) && !filereadable( inp )
				let tail = s:fnamemodify_pc( inp, ':t' )
				if has_key( g:CD_aliases, tail )
					let inp = g:CD_aliases[ tail ]
					if isdirectory( inp )
						let inp .= '/'
					endif
					continue
				elseif input("Create directory? ") =~? '^y'
					call mkdir( inp, "p" )
				endif
			endif


		" ------------------------------------------------------------
		elseif	inp_char =~# "\\(\<tab>\\|\<S-Tab>\\)" 
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

				let s:tab_path_list = filter( matching_paths, 'v:val !~ ''\.\+$'' ' )

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

			let s = inp
			let s = s:fnamemodify_pc( s, ':t' )
			if len( s:tab_path_list ) < 8 || strlen( s ) <= g:CD_Plus_name_wrap_len
				let s1 = escape( s, ' [].-~' )
				let s2 = 'not_a_match'
			else
				let s1 = strpart( s, 0, ( g:CD_Plus_name_wrap_len - 2 ) )
				let s2 = strpart( s, ( g:CD_Plus_name_wrap_len - 2 ) )
				let s1 = escape( s1, ' [].-~' )
				let s2 = escape( s2, ' [].-~' )
				let s2 = s2 == '' ? 'not_a_match' : '>' . s2
			endif

			let s3 = escape( s, ' [].-~' )
			let s2 = 'not_a_match'

			"let do = 'match wildmenu ;\(^\|\s\)\%[' . s . ']\ze\([>/ ]\|$\);'

			let do = 'match wildmenu ;\v<%[' . s3 . ']\ze([>/ ]|$);'
			exe do
			"echomsg do

			"exe 'match wildmenu ;\(\%[' . s3 . ']\{1,}\)\ze\([>/ ]\|$\);'
			"exe 'match wildmenu ;\(\%[' . s3 . ']\|' . s2 . '\)\ze\([>/ ]\|$\);'

			" End: tab handling
			"


		" ------------------------------------------------------------
		elseif	inp_char =~# "\\(\<cr>\\|\<nl>\\)" 
			if inp == ''
				let inp = $HOME
			endif
			if !isdirectory( inp ) && !filereadable( inp )
				if s:Cmd =~ 'cd'
					let dir = inp
				else
					let dir = s:fnamemodify_pc( inp, ':h' )
				endif
				if input("Create directory? ") =~? '^y'
					call mkdir( dir, "p" )
					if !isdirectory( dir )
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
		else 
			let inp .= inp_char
			let inp = substitute( inp, '^\s*', '', '' )
			let paths = s:Get_paths( inp )
		endif

		if	inp_char !~# "\\(\<tab>\\|\<S-Tab>\\)" && s:tab_path_list_idx != -1
			let s:tab_path_list_idx = -1
			match
		endif

		if	inp_char !~# "\\(\<c-n>\\|\<c-p>\\)" && s:CD_history_idx != -1
			let s:CD_history_idx = -1
			match
		endif

	endwhile
	"
	"  End: Main processing loop
	"
	" ----------------------------------------------------------------------


	let g:CD_aliases[ '-' ] = old_cwd

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


	if	inp_char =~ "\\(\<cr>\\|\<nl>\\)" 
		let do = s:Cmd . " " . inp
		exe s:Cmd . " " . inp
		"call feedkeys( ":" . s:Cmd . " " . inp . "\<CR>" , "n" )
		redraw
		call histadd( "cmd", do )
		echo do
	endif

endfunction
" End:  function! CD_Plus( cmd, path )


function! CD_Plus_return_to( cmd, path, key )

	let path = a:path
	let contents = getline( "." )
	let col = col(".")

	"let word = matchstr( contents, '\<\S*\%' . ( col ) . 'c\S*\>' )
	"let word1 = matchstr( contents, '\S*\%#\S*' )
	let word = matchstr( contents, '\S*\%' . ( col ) . 'c\S*' )

	if a:key =~ "\\v(\<CR>|CR)"
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

	if !isdirectory( path ) && !filereadable( path )
		let g:Allow_any_input = 1
	endif
	call CD_Plus( a:cmd, path )

endfunction




aug CD_auto
	au!
	au BufWinEnter * call CD_Plus_add_history( s:fnamemodify_pc( expand("%"), ':p' ) )
	au BufHidden __CD__ let &cmdheight = s:save_cmdheight
aug END



function! CD_Plus_add_history( path )
	if isdirectory( a:path )
		let idx = index( g:CD_dir_history, a:path )
		if idx > -1
			call remove( g:CD_dir_history, idx )
		endif
		call extend( g:CD_dir_history, [ a:path ], 0 )

		while len( g:CD_dir_history ) > g:CD_dir_history_max
			call remove( g:CD_dir_history, -1 )
		endwhile
	else
		if a:path =~ '\v(__CD__|^$)'
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

"	let winnr = bufwinnr( bufnr("__CD__") )
"	if winnr > 0
"		exe winnr . " wincmd w"
"	else
"		echomsg 'error finding window'
"	endif

"	else
"		wincmd b
"		wincmd k
"		6 split __CD_HISTORY__
"	endif
"	setlocal buftype=nofile
"	setlocal noswapfile
"	setlocal bufhidden=wipe

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

	"hide

	return g:CD_dir_history[ len(g:CD_dir_history) - idx ]
endfunction




function! s:Do_commands( cmd, dir )

	if a:cmd =~? '^history'
		return s:Fill_history()
	endif

	if a:cmd =~? '^set'
		let prompt = 'set alias for ' . a:dir . ' : '
	elseif a:cmd =~? '^delete'
		%d _
		let l = filter( keys(g:CD_aliases) , 'g:CD_aliases[ v:val ] !~ ":$" ' )
		call append("$",  s:Format_aliases( l ) )
		2match diffchange /^\s*\zs.*\ze >>/
		exe 'resize ' . len(l)
		redraw
		let prompt = 'delete alias for : '
	elseif a:cmd =~? '^excmd'
		let prompt = 'enter ex : '
	elseif a:cmd =~? '^help'
		call CD_Plus_help_extract_tmp_buf()
		return a:dir
	else
		let s:Cmd = a:cmd
		let s:Cmd = substitute( s:Cmd, '\s*:$', '', '' )
		return a:dir
	endif

	let cmd = input( prompt, "" )

	if cmd == '' | return a:dir | endif

	if a:cmd =~? '^set'
		let g:CD_aliases[ cmd . ' ' ] = a:dir
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




function! s:Format_paths( paths )
	if len( a:paths ) < 1 | return [] | endif
	let path_lens = deepcopy( a:paths )
	call map( path_lens, 'strlen(v:val)' )
	let longest_path = max( path_lens )
	let longest = min( [longest_path, g:CD_Plus_name_wrap_len ] )		" 19+1 space divides evenly usually

"	let total = strlen( join( a:paths, ' ' ) )
"	if total < winwidth( winnr() )
"		let longest = longest_path
"	endif
	if len( a:paths ) < 8
		let longest = longest_path
	endif

	let fmt_norm  = "%-" . longest . "." . longest . "s "
	let fmt_indent = "  %-" . longest . "." . longest . "s "

	let out_paths = []

	" First, wrap file names which are too long:
	"
	for path in deepcopy( a:paths )
		let part = ''
		let fmt = fmt_norm
		while strlen( path ) > longest 
			let part = strpart( path, 0, longest - 1 - 1 ) . '>'
			let path = ' >' . strpart( path, longest - 1 - 1 )
			let out_paths += [ printf( fmt, part ) ]
		endwhile
		let out_paths += [ printf( fmt, path ) ]
	endfor


	let cols = 0 + ( winwidth(winnr()) / ( longest + 1 ) ) 
	let rows = 1 + ( len( out_paths ) / cols ) 

	let out_lines = []

	let idx = 0
	while idx < len( out_paths )
		for col in range( 0, cols - 1 )
			for row in range( 0, rows - 1 )
				"echomsg row . ',' . col
				while row >= len( out_lines )
					call add( out_lines, '' )
				endwhile
				let out_lines[row] .= out_paths[ idx ]
				let idx += 1
				if idx >= len( out_paths )
					return out_lines
				endif
			endfor
		endfor
		let cols = 1
		" Kludge alert!
	endwhile


	echomsg 'eeek........ didnt finish directory list: '
			\ . 'idx=' . idx . ',' . string( out_paths[ idx : ] )
	return out_paths[ idx : ]

"	for dir in a:paths
"		let dir1 = printf( fmt, dir )
"		if ( 1 + strlen( dir1 ) + strlen( out ) ) >= winwidth( winnr() )
"			let out_paths += [ out ]
"			let out = ''
"		endif
"		let out .= dir1
"	endfor
"	let out_paths += [ out ]

"	return out_paths
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
		let alias1 = printf( fmt, alias ) . '>>' . g:CD_aliases[ alias ]
		let out_aliases += [ alias1 ]
		let lines += 1
	endfor

	return out_aliases
endfunction




function! s:Get_paths( in_path )

	let in_path = a:in_path
	let paths = []

	let in_path = in_path =~ '\*$' ? in_path : in_path . '*'

	let globs = glob( in_path )
	if in_path =~ '/\*$'

		" get directories normally hidden with a "."
		"
		let globs .= "\n" . glob( substitute( in_path, '/\*$', '/.*', '' ) )
	endif

	for file in split( globs, "\n" )
		if isdirectory( file ) || s:Cmd !~? 'cd'
			let paths += [ file ]
		endif
	endfor

	return sort( paths )


	"	Uniq -- not needed?
	"
"	let paths2 = []
"	for i in range( 1, len(paths) - 1 )
"		if paths[i] != paths[i-1]
"			call add( paths2, paths[i] )
"		endif
"	endfor

"	return sort( paths2 )

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

cnoremap <expr> e<space> ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('e') : 'e' )
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
