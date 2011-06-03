
"------------------------------------------------------------------------------
"                     CD_Plus.vim : 'cd' accelerator  {{{
"
"
"
" Author:		Eric Arnold ( eric_p_arnold@yahoo.com )
"                             ^^^^^^^^^^^^^^^^^^^^^^^
"                             Comments, bugs, feedback welcome.
" Created:		May, 2006
" Updated:		Wed Jun 07, 06/07/2006 6:37:20 PM
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
" Version:		2.1		Wed Jun 07, 06/07/2006 6:37:20 PM
" 						-	Misc fixes (stray error messages)
"						-	Option to recan for pc drives
"						-	Misc fixes for pc drive pathnames
"							(Still some problems).
"						-	Added sorting options
"						-	Faster scrolling and tabbing
"						-	Changed :excmd enter method
"						-	Added file size and time options.
"						-	Added match collapse listing option.
"						-	Added highlighting toggle
"						-	Added long listing toggle
" Version:		2.2		Thu Jun 08, 06/08/2006 8:54:34 AM
"						-	Fixed accumulating syntax entries
"						-	Highlighting shortcuts to speed up tabbing
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
" 		Vim window.
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
"				If <CR> is typed in the display window, the name under the
"				cursor will be copied to the command line.
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
"	<C-L>		Refresh
"
"	<C-E>		Scroll one line
"	<C-Y>
"	<C-D>		Scroll page
"	<C-B>
"				Note : it is almost as easy to do your scrolling using
"				<ESC> to bounce to/from the display window.
"
"
"	Other Commands:
"
"		The built-in commands are implemented as aliases (as are pc-drive
"		names).  You can override them, but then it's up to you to create
"		new ones.  Enter commands after a trailing  / .
"
"	'set '		Set an alias to the current directory displayed.
"
"	'delete '	Delete an alias.
"
"	'o pt '		Sets options.  (o<SPACE>pt is to force it to match sooner.)
"		
"			-	Set display column wrap width.  Set to 0 for auto-sizing to
"				longest path name.
"
"			-	Auto-change directory.  (default is 'local', as in 'lcd')
"
"			-	Recan for pc drives, and create 'C:/' style aliases.
"
"			-	Match collapses (shortens) listings (default on; off means
"				always show full directory listing).
"
"			-	Toggle highlighting on/off
"
"
"	'sort '		Sets sorting type. (default==alpha, time, size)
"
"	'long'		Long listing toggle (sets wrap, fsize, etc.)
"
"	'help '		This listing.
"
"	'history '	Browse the history.  A separate history is accessed
"				depending on whether the current command is 'cd' or 'lcd',
"				or some other file oriented command like 'e' or 'new'.
"
"	'edit '		Switch to 'open/edit' file mode.
"	'new '		
"
"	': '		Input other ex command.  I.e.  :vimgrep aug<CR>
"				will prepend as ':vimgrep aug /someroot/somefiles*'
"
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
"		generally be discarded.  This takes a little getting used to, since
"		you might tend to type ahead more than will match (see also // below ).
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
"
"			cnoremap <silent> <expr> e<space> ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('e') : 'e' )
"			cnoremap <expr> cd ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('cd') : 'cd' )
"			cnoremap <expr> lcd ( getcmdpos() == 1 && getcmdtype() == ':' ? CD_Plus_start('lcd') : 'lcd' )
"		
"		a simpler keymap might be:
"			nnoremap <leader>cd :call CD_Plus('cd')<CR>
"
"		There is a list of globals,  following the script header, which you
"		can change.  There are a few more, but these are the simple ones,
"		which can also be set via option aliases.
"
"			g:CD_rc_file 				default: $HOME . '/.vim_cd_plus'
"									Most globals and history are saved
"									here.
"
"			g:CD_autochdir			'off', 'local', or 'global'
"
"			g:CD_dsp_wrap_len		default 19, specifies display column
"									wrapping.
"
"			g:CD_any_input			0 or 1, the starting value for literal
"									input (also set by trailing // toggle).
"
"			g:CD_scan_pc_drives		0 or 1, whether to attempt to discover
"									pc drive names (like C:, D:, etc.) and
"									create aliases for them.
"
"		If you want to customize the highlighting or sorting, it's a little
"		more complicated.  For highlighting, there is an 'init' function
"		that needs to be changed.  For sorting, an entry must be added to
"		the g:CD_aliases dict var (see top of script), and a new function
"		must be defined to do the sorting.  See the Sort_by_ftime() or
"		Sort_by_extension() functions for examples.
"
"
"
"	Caveats:
"
" 	-	Works only with 'shellslash' on.
" 	-	Restoring the command line height is a true pain in the ass.  It
" 		works most of the time, though that is little consolation.
"	-	Tabbing is a little slow on large directory lists because of the
"		redraw requirement.  It isn't much a problem, though since it's only
"		noticable when you hold the tab key down.
"
" End_help_section }}}
"
" To Do:
"
" 	-	Better handling of user supplied globs and regex's.  I.e. backslash
" 		is problematic.
"
"}}}



let s:This_script_fname = expand("<sfile>:p")
if v:version < 700
	echomsg "Vim 7 or higher is required for " . s:This_script_fname 
	finish
endif


" User settable globals:
"
if !exists('g:CD_rc_file')			| let g:CD_rc_file = $HOME . '/.vim_cd_plus' | endif
if !exists('g:CD_scan_pc_drives')	| let g:CD_scan_pc_drives = 1		| endif
if !exists('g:CD_dsp_wrap_len')		| let g:CD_dsp_wrap_len = 19		| endif
if !exists('g:CD_any_input')		| let g:CD_any_input = 0			| endif
if !exists('g:CD_autochdir')		| let g:CD_autochdir = 'local'		| endif
if !exists('g:CD_match_collapse')	| let g:CD_match_collapse = 1		| endif
if !exists('g:CD_do_highlighting')	| let g:CD_do_highlighting = 1		| endif


" Other globals, which tend to be set internally, but can be modified with
" care:
if !exists('g:CD_aliases')			| let g:CD_aliases = {} 			| endif
if !exists('g:CD_path_history')		| let g:CD_path_history = []		| endif
if !exists('g:CD_path_history_max')	| let g:CD_path_history_max = 100	| endif
if !exists('g:CD_dir_history')		| let g:CD_dir_history = []			| endif
if !exists('g:CD_dir_history_max')	| let g:CD_dir_history_max = 100	| endif
if !exists('g:CD_sort_func')		| let g:CD_sort_func = 'default'	| endif
let g:CD_glob_cache = {}



" You can customize the highlighting by editing CD_Plus_init_highlight()
" and CD_Plus_hl_set()here, or copy to your vimrc:
"
function! CD_Plus_init_highlight()

	syn clear

	if !g:CD_do_highlighting | return | endif

	hi! star			term=standout ctermfg=12 guifg=Red
	hi! slash			term=standout ctermfg=10 gui=bold guifg=Green
	hi! junk			ctermfg=8 guifg=grey60
	hi! continue		gui=bold term=bold cterm=bold ctermfg=6 guifg=#00a0ff
	hi! link dir		Directory
	hi! link matched	WildMenu
	hi! in_history		ctermfg=lightred guifg=lightred
	hi! ext				ctermfg=8 guifg=darkyellow

	"hi! clear Normal
	"hi! Normal ctermfg=6 guifg=lightgreen

	syntax match star /\*/
	syntax match slash ;\v/\ze( |$);
	syntax match continue />/
	syntax match ext ;\v[^.]\zs\.[^ .><]+\ze([ /*]|$);
endfunction


"let s:old_hl_paths = []
function! CD_Plus_hl_set( paths )
	"syn clear

	if !g:CD_do_highlighting | return | endif

"	if a:paths == s:old_hl_paths
"		return
"	endif
"	let s:old_hl_paths = a:paths

	call CD_Plus_init_highlight()
	" Individual syntax highlighting
	"
	for path in a:paths
		let cmd  = ''
		let tail = s:fnamemodify_pc( path, ':t' )
		let hist_idx = index( g:CD_path_history, path ) 
		if s:isdirectory_cached( path )
			let cmd = s:hl_syntax( tail . '/', 'dir' )
		elseif path =~ '\v[-~]$'
			let cmd = s:hl_syntax( tail, 'junk' )
		elseif path =~ '\v\.sw\a$'
			let cmd = s:hl_syntax( tail, 'junk' )
		elseif hist_idx > -1 && hist_idx < 10
			let cmd = s:hl_syntax( tail, 'in_history' )
		endif

		exe cmd
	endfor
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
				\ , ': '		: 'excmd :'
				\ , 'long listing'		: 'long :' 
				\ ,	'o pt 1 column width ( %{g:CD_dsp_wrap_len} )' : 'wrap :'
				\ ,	'o pt 2 auto change directory ( %{g:CD_autochdir} )' : 'autochdir :'
				\ ,	'o pt 3 scan for new PC drives ' : 'pc_drive_scan :'
				\ ,	'o pt 4 toggle file size' : 'fsize :'
				\ ,	'o pt 5 toggle file time' : 'ftime :'
				\ ,	'o pt 6 toggle match collapse listings(%{g:CD_match_collapse})' : 'match_collapse :'
				\ ,	'o pt 7 toggle highlighting (%{g:CD_do_highlighting})' : 'do_highlighting :'
				\ ,	'sort default (%{g:CD_sort_func})'		: 'sort default :'
				\ ,	'sort time (%{g:CD_sort_func})'			: 'sort Sort_by_ftime :'
				\ ,	'sort extension (%{g:CD_sort_func})'	: 'sort Sort_by_extension :'
				\ } 
				\ )
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



function! s:scan_pc_drives()
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
endfunction



if filereadable( g:CD_rc_file )
	exe 'so ' . g:CD_rc_file
endif


let s:CD_history_idx = -1

function! CD_Plus( cmd, path )


	let s:tab_path_list = []
	let s:tab_path_list_idx = -1

	"let s:scrolling = -1

	let s:CD_history_idx = -1

	let s:inputting_ex = 0

	let s:Cmd = a:cmd

	let old_cwd = getcwd()

	let start_bufnr = bufnr("%")

	" If cmdheight == 1, we've probably moved out of the command line 
	" into to the display window.
	if &cmdheight > 1
		let s:save_lazyredraw = &lazyredraw
		let g:CD_save_cmdheight = &cmdheight
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
				if bufnr("%") == bufnr
					%d _
				endif
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
			"				exe "au BufHidden <buffer> set cmdheight=" . g:CD_save_cmdheight
			"			aug END
			"exe "nmap <silent> <buffer> <esc> <esc>:set cmdheight=" . g:CD_save_cmdheight . "<CR>:cd"
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
		

		"if s:scrolling == -1 
		if s:tab_path_list_idx == -1
		\ && s:CD_history_idx == -1
			"MoreMsg
			echohl LineNr 
			echon 'L O A D I N G'
			echohl NONE
			redraw
		endif


		let inp = substitute( inp, '^\a:', '\=toupper(submatch(0))', '' )
		let inp = substitute( inp, '^\a:$', '&/', '' )

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
	
		"echomsg 'paths=' . string( paths )
		"echomsg 'tail='.inp_tail  . ', matching='. string( matching_paths )

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
"		if s:inputting_ex
"			if inp_char == ' '
"				let inp = s:fnamemodify_pc( inp, ':h' )
"				if s:isdirectory_cached( inp )
"					let inp = substitute( inp, '/*$', '/', '' )
"				endif
"				let s:inputting_ex = 0
"			else
"				let s:Cmd .= inp_char
"				let inp = inp[0:-2]
"			endif
"
"		elseif inp =~ '/:$'
"			let s:inputting_ex = 1
"			let s:Cmd = ''
"
"		elseif inp =~ '//$'
		if inp =~ '//$'
			let g:CD_any_input = !g:CD_any_input
			let inp = substitute( inp, '/*$', '/', '' )
			continue

		elseif inp =~ '^\a:*$'
			"echomsg 'here'
			call s:Add_input_stack( inp_stack, inp )

		elseif inp =~ '/\./$'
			" Junk case
			let inp = substitute( inp, '\./$', '', '' )
			call remove( inp_stack, -1 )
			continue

		elseif inp =~ '/\.\.$'
			let inp = s:fnamemodify_pc( inp, ':h' )	" remove ..
			let inp = s:fnamemodify_pc( inp, ':h' )	" remove current dir
			let inp .= '/'
			let paths = s:Get_paths( inp )
			" reset stack so backspace doesn't return to lower directory
			let inp_stack = [ inp ]
			continue

		elseif inp =~? '/\~$' 
			"|| inp =~? '^\~$'
			let inp = substitute( $HOME, '\\', '/', 'g' ) . '/'
			"echomsg 'home ' .  inp 
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

		elseif inp =~ '[.\\]$' || inp =~ '\*' || inp =~ '\*\*/$'
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
		elseif g:CD_match_collapse == 0
			let dir = inp
			"echomsg 'show_paths' . string( matching_paths)
			"if dir =~ '[*.\\]' || dir =~ '/$'
			if dir =~ '/$' "|| s:isdirectory_cached( dir )
				" nada
			else
				let dir = s:fnamemodify_pc( dir, ':h' )
				"let dir .= '/' 
			endif
			let show_paths = s:Get_paths( dir . '/' )
			"echomsg 'dir='.dir
		else
			let show_paths = deepcopy( matching_paths )
		endif

		let show_path_tails = deepcopy( show_paths )
		let show_path_tails = map(  show_path_tails, 
					\   ' s:fnamemodify_pc( v:val, ":t" ) '
					\ . ' . '
					\ . '( s:isdirectory_cached( v:val ) '
					\ . ' ? "/" : "" ) ' )

		if match( longest_path, '/$' ) > -1
			" Clear aliases when ending in /, since all will be matched.
			let matching_aliases = []
		endif


		"if s:scrolling > -1
			" nada
		let lines = 0
		if s:CD_history_idx > -1
			exe "resize " . min( [ &lines / 3, len( s:CD_history ) ] )
		else
			silent %d _

			for str in s:Format_paths( inp, show_path_tails )
				let lines += 1
				call append("$", str)
			endfor

			for str in s:Format_aliases( matching_aliases )
				let lines += 1
				call append("$", str)
			endfor

			1d _
			2match diffchange /^\s*\zs.*\ze >>/

			if s:tab_path_list_idx == -1
				let inp_tail = s:fnamemodify_pc( substitute( inp, '/*$', '', '' ), ':t' )
				if inp_tail == '' || inp_tail =~ '^\*\**$' || inp =~ '/$'
					match
				else
					" remove trailing backslashes until next input char
					let inp_tail = substitute( inp_tail, '\\$', '', '' )
					let inp_tail = substitute( inp_tail, '\[$', '', '' )
					let inp_tail = substitute( inp_tail, '\([^. *]*\)\*\([^. *]*\)', '\1[^ ]*\2', '' )
					let s = 'match matched /\v(^|\s)\zs' . inp_tail . '/'
					try
						exe s
					catch
					endtry
					"echomsg s
					"let @a = s
					"exe 'match matched /\v(^|\s)\zs' . s:escape_path( inp_tail ) . '/'
				endif
			endif


			"call CD_Plus_hl_set( matching_paths )
			call CD_Plus_hl_set( show_paths )

			" Resize to fit
			"let lines = len( show_paths1 + show_aliases )
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


		" ------------------------------------------------------------
		"  Process a character
		"
		" Loop here for operations that should be instant, like scrolling,
		" tab, etc.
		"
		let syn_cleared = 0
		while 1
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

			let inp_char = getchar()
			let inp_char = nr2char( inp_char ) == '' ? inp_char : nr2char( inp_char )

			if inp_char =~# "\\v(\<c-e>|\<c-y>|\<c-d>|\<c-b>)" 
				exe "normal! " . inp_char
			elseif	inp_char =~# "\\v(\<tab>|\<S-Tab>)" 
				if len( matching_paths ) > 30
					let syn_cleared = 1
					syn clear
					if s:Cmd =~ 'cd'
						syn match directory /.*/
					else
						syn match comment /.*/
					endif
				endif
				let inp = s:Do_tab( inp_char, inp, matching_paths )
			else
				break
			endif
		endwhile

		if syn_cleared
			call CD_Plus_hl_set( matching_paths )
		endif

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
						\&& s:tab_path_list_idx == -1
				call remove( inp_stack, -1 )
				let inp = remove( inp_stack, -1 )
				let paths = s:Get_paths( inp )
			elseif strlen( inp ) > 0
				let inp = substitute( inp, '/*$', '', '' )
				let inp = s:fnamemodify_pc( inp, ':h' ) . '/'
				let paths = s:Get_paths( inp )
				let inp_stack = []
			else
				let inp = ''
				let inp_stack = []
			endif

		" ------------------------------------------------------------
		elseif	inp_char =~# "\<c-l>"
			unlet! s:old_path_tails
			let inp_char = ''
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

			" Special case:  .../**/
			if inp =~ '\*\*/*'
				let inp .= '/'
				continue
			endif

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
						\ && inp !~ '\*'
				let tail = s:fnamemodify_pc( inp, ':t' )
				if has_key( g:CD_aliases, tail )
					let inp = g:CD_aliases[ tail ]
					if s:isdirectory_cached( inp )
						let inp .= '/'
					endif
					continue
				elseif input("Create directory (" . inp . ")? ") =~? '^y'
					call mkdir( inp, "p" )
				endif
			endif


		" ------------------------------------------------------------
		elseif	inp_char =~# "\\v(\<tab>|\<S-Tab>)" 

			" End: tab handling
			"
			" ------------------------------------------------------------


"		" ------------------------------------------------------------
"		elseif	inp_char =~# "\\v(\<c-y>)" 
"			exe "normal! \<c-y>"
"			let s:scrolling = 1
"			let inp_char = ''

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
			\&& inp !~ '\*'
				if s:Cmd =~ 'cd'
					let dir = inp
				else
					let dir = s:fnamemodify_pc( inp, ':h' )
				endif
				if input("Create directory? (" . inp . ")") =~? '^y'
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

		"if	inp_char != '' && inp_char !~# "\\v(\<c-e>|\<c-y>)" && s:scrolling != -1
			"let s:scrolling = -1
		"endif

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
		"let &cmdheight = g:CD_save_cmdheight
		if inp_char == "\<ESC>"
			exe "nnoremap <silent> <buffer> <esc> <esc>"
						\ . ":call CD_Plus( '" . s:Cmd . "' , '" . inp . "') <CR>"
			exe "nnoremap <silent> <buffer> <CR> "
						\ . ":call CD_Plus_return_to( '" . s:Cmd . "' , '" . inp . "', 'CR' ) <CR>"
			exe "nnoremap <silent> <buffer> <2-leftmouse> "
						\ . ":call CD_Plus_return_to( '" . s:Cmd . "' , '" . inp . "', '2-leftmouse' ) <CR>"
		else
			let &lazyredraw = s:save_lazyredraw
			let &cmdheight = g:CD_save_cmdheight
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




function! s:Do_tab( key, inp, paths )
	let inp = a:inp

	if s:tab_path_list_idx == -1
		let s:tab_path_list_idx = 0
		if len( a:paths ) < 1
			return inp
			"					let inp = substitute( inp, '/*$', '/', '' )
			"					let a:paths = s:Get_paths( inp )
			"					if len( a:paths ) > 0
			"						let matching_paths = a:paths
			"					else
			"					endif
		endif
		let s:tab_path_list = a:paths

	elseif len( s:tab_path_list ) > 0

		if	a:key =~# "\<tab>"
			let s:tab_path_list_idx = s:tab_path_list_idx == len( s:tab_path_list ) - 1
						\ ? 0 : s:tab_path_list_idx + 1
		else
			let s:tab_path_list_idx = s:tab_path_list_idx == 0 
						\ ? len( s:tab_path_list ) - 1 : s:tab_path_list_idx - 1
		endif

	endif

	let inp = s:tab_path_list[ s:tab_path_list_idx ]

	let inp_tail = s:fnamemodify_pc( inp, ':t' )
	if s:isdirectory_cached( inp )
		let inp_tail .= '/'
	endif

	let regex = s:hl_regex( inp_tail )

	"echomsg string( s:highlight_table )
	call cursor( s:highlight_table[ inp_tail ].chunks_coord[0].line,
				\ s:highlight_table[ inp_tail ].chunks_coord[0].col )
	"echomsg s:highlight_table[ inp_tail ].chunks_coord[0].line

	if regex != ''
		let do = 'match matched ;' . regex . ';'
		exe do
	endif
	return inp
endfunction





aug CD_auto
	au!
	au BufWinEnter * call CD_Plus_add_history( s:fnamemodify_pc( expand("%"), ':p' ) )
	au BufHidden __CD__ let &cmdheight = g:CD_save_cmdheight
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
	let line = line(".")

	"let word = matchstr( contents, '\<\S*\%' . ( col ) . 'c\S*\>' )
	"let word1 = matchstr( contents, '\S*\%#\S*' )
	"let [ num, word; junk ] = matchlist( contents, '^\s*\d\+\s\+\(.*\)' )
	if contents =~ '^\s*\d\+\s\+\(.*\)'
		let word = substitute( contents, '^\s*\d\+\s\+', '', '' )
	else
		"let word = matchstr( contents, '\S*\%' . ( col ) . 'c\S*' )

		" Use the coordinates in the s:highlight_table to find the filename
		" under the cursor, since it might be split/wrapped.
		"
		let word = 'error'
		let max_col = 0
		for path1 in sort( keys( s:highlight_table ) )

			"for idx in range( 0, len( s:highlight_table[ path1 ].chunks ) - 1 )
			for coord in s:highlight_table[ path1 ].chunks_coord
				"if s:highlight_table[ path1 ].chunks_coord[idx].line == line
							"\&& s:highlight_table[ path1 ].chunks_coord[idx].col <= ( col - 1 )
				if coord.line == line && coord.col <= col
					if coord.col > max_col
						let max_col = coord.col
						let word = path1
					endif
				endif
			endfor
		endfor

	endif

	if a:key =~ "\\v(\<c-f>)"
		call CD_Plus( a:cmd, a:path )
		return
	elseif a:key =~ "\\v(\<CR>|CR|\<2-leftmouse>|2-leftmouse)"
		if s:CD_history_idx == -1
			if path !~ '/$'
				let path = s:fnamemodify_pc( path, ':h' )
			endif
			let path = substitute( path, '/*$', '/', '' )
			let path .= word
		else
			let path = word
		endif
	endif

	if s:CD_history_idx != -1
		"
		" Transfer the possibly changed history list to global var:
		"
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
		"echomsg 'bad path ' . path
		let g:CD_any_input = 1
	endif

	"echomsg 'returning cmd=' . a:cmd . ',path=' . path
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
	syn clear

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
"		if s:Cmd =~? 'cd'
"			let s:CD_history = g:CD_dir_history 
"		else
"			let s:CD_history = g:CD_path_history 
"		endif
"		call s:Fill_history( s:CD_history )
		call feedkeys( "\<c-p>\<esc>\<c-l>", "t" )
		return a:dir
	endif

	let cmd = ''

	if a:cmd =~? '^set'
		let prompt = 'Set alias for ' . a:dir . ' : '
		let cmd = input( prompt, "" )
	elseif a:cmd =~? '^delete'
		%d _
		let l = filter( keys(g:CD_aliases) , 'g:CD_aliases[ v:val ] !~ ":$" ' )
		call append("$",  s:Format_aliases( l ) )
		2match diffchange /^\s*\zs.*\ze >>/
		exe 'resize ' . len(l)
		redraw
		let prompt = 'Delete alias for : '
		let cmd = input( prompt, "" )
	elseif a:cmd =~? '^excmd'
		let prompt = 'Enter ex : '
		let cmd = input( prompt, "", "command" )
	elseif a:cmd =~? '^help'
		call CD_Plus_help_extract_tmp_buf()
		return a:dir
	elseif a:cmd =~? '^wrap'
		let prompt = 'enter column wrap width : '
		let cmd = input( prompt, "" )
	elseif a:cmd =~? '^autochdir'
		let prompt = 'Enter autochdir ( "local", "global", or "off" ) : '
		let cmd = input( prompt, "" )
	elseif a:cmd =~? '^pc_drive_scan'
		let s = g:CD_scan_pc_drives
		let g:CD_scan_pc_drives = 1
		call s:scan_pc_drives()
		let g:CD_scan_pc_drives = s
		return
	elseif a:cmd =~? '^long'
		if !exists('s:long_listings')
			let s:want_info_sv = s:want_info
			let s:want_info = 'ftime,fsize'
			let s:CD_dsp_wrap_len_sv = g:CD_dsp_wrap_len
			let g:CD_dsp_wrap_len = 0
			let s:long_listings = 1
			let s:old_path_tails = []
		else
			unlet! s:long_listings
			let s:want_info = s:want_info_sv
			let g:CD_dsp_wrap_len = s:CD_dsp_wrap_len_sv
			let s:old_path_tails = []
		endif
		return
	elseif a:cmd =~? '^fsize'
		if s:want_info =~ 'fsize'
			let s:want_info = substitute( s:want_info, ',*fsize', '', '' )
		else
			let s:want_info .= ',fsize'
		endif
		return
	elseif a:cmd =~? '^ftime'
		if s:want_info =~ 'ftime'
			let s:want_info = substitute( s:want_info, ',*ftime', '', '' )
		else
			let s:want_info .= ',ftime'
		endif
		return
	elseif a:cmd =~? '^match_collapse'
		if g:CD_match_collapse == 0
			let g:CD_match_collapse = 1
		else
			let g:CD_match_collapse = 0
		endif
		return
	elseif a:cmd =~? '^do_highlighting'
		if g:CD_do_highlighting == 0
			let g:CD_do_highlighting = 1
		else
			let g:CD_do_highlighting = 0
		endif
		return
	elseif a:cmd =~? '^sort'
		let g:CD_sort_func = a:cmd
		let g:CD_sort_func = substitute( g:CD_sort_func, '^sort\s*', '', '' )
		let g:CD_sort_func = substitute( g:CD_sort_func, '\s*:\s*$', '', '' )
		return
	else
		let s:Cmd = a:cmd
		let s:Cmd = substitute( s:Cmd, '\s*:$', '', '' )
		return a:dir
	endif


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



" For now, caching is disabled, since it doesn't
" seem to be a bottle neck:
"
let s:isdirectory_cached_cache = {}
function! s:isdirectory_cached( dir )
	if a:dir == '' | return | endif

	return isdirectory( a:dir )

"	if has_key( s:isdirectory_cached_cache, a:dir )
"		"echomsg 'cached ' . a:dir
"	else
"		let s:isdirectory_cached_cache[ a:dir ] = isdirectory( a:dir )
"	endif
"	return s:isdirectory_cached_cache[ a:dir ]
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





let s:tst = 0

function! s:hl_regex( path )
	if !has_key( s:highlight_table, a:path )
		"echomsg "CD_Plus err: bad path arg to hl_regex()"
		return ''
	endif

	"echomsg 'len ' . len( s:highlight_table )
	if has_key( s:highlight_table[ a:path ], 'regex' )
		"echomsg 'returning cached'
		return s:highlight_table[ a:path ].regex
	else
		"echomsg 'building regex'
		let s:highlight_table[ a:path ].regex = ''
	endif

	let regex = '\v('
	for idx in range( 0, len( s:highlight_table[ a:path ].chunks ) - 1 )
		let s:tst += 1
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

	" Don't include the trailiing slash, so it can be highlighted
	" differently:
"	if a:path =~ '/$'
"		let regex = substitute( regex, '.$', '', '' )
"	endif

	"let regex .= ')\ze($|[* ])'
	let regex .= ')'

	let s:highlight_table[ a:path ].regex = regex

	"echomsg 'tst ' . s:tst
	return regex

endfunction





let s:old_paths = []
let s:want_info = ''

function! s:Format_paths( base_dir, path_tails )
	if len( a:path_tails ) < 1 | return [] | endif

	if exists('s:old_path_tails') && a:path_tails == s:old_path_tails
		return s:out_lines
	endif
	let s:old_path_tails = a:path_tails

	let base_dir = a:base_dir 
	if base_dir !~ '/$'
		let base_dir = s:fnamemodify_pc( base_dir, ':h' )
	endif

	let path_tails = deepcopy( a:path_tails )
	let path_lens = deepcopy( path_tails )
	call map( path_lens, 'strlen(v:val)' )
	let longest_path = max( path_lens )

	let info_len = 0
	let info_len += ( s:want_info =~ 'ftime' ? 21 : 0 )
	let info_len += ( s:want_info =~ 'fsize' ? 8 : 0 )

	if g:CD_dsp_wrap_len > 0

		let longest = min( [longest_path, g:CD_dsp_wrap_len ] )		" 19+1 space divides evenly usually

	else
		let longest = longest_path + info_len

	endif

	if len( path_tails ) < 8
		let longest = longest_path + info_len
	endif

	let longest = max( [longest, 8 ] )

	let s:out_chunks = []

	" First, wrap file names which are too long:
	"
	unlet! s:highlight_table
	let s:highlight_table = {}

	let tst = 0

	for path in deepcopy( path_tails )

		let path_copy = path
		"let path_copy = substitute( path, '/*$', '', '' )
		let s:highlight_table[ path_copy ] = {}

		let fsize = getfsize( base_dir . '/' . path )
		let ftime = getftime( base_dir . '/' . path )
		let ftime = strftime( '%y/%m/%d;%H:%M:%S', ftime )

		let info = ''

		if s:want_info =~ 'fsize' && !s:isdirectory_cached( path )
			let info = ' ' . fsize
		endif

		if s:want_info =~ 'ftime'
			let info .= ' ' . ftime
		endif


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

		if info != ''
			let dots = longest - strlen( path ) - strlen( info ) - 2
			let dots = dots > 1 ? repeat( '_', dots ) : ';'
			let path .= ' ' . dots . info . ' ' 
		endif

		while strlen( path ) > longest 
			let part = strpart( path, 0, longest - 1 - 1 ) . '>'
			let s:highlight_table[ path_copy ].chunks_raw += [ part ]
			let path = '>' . strpart( path, longest - 1 - 1 ) . ' '
			let seg = printf( fmt, part )
			let s:out_chunks += [ seg ]
			let s:highlight_table[ path_copy ].chunks += [ seg ]
			"echo tst . ',' . longest . ',' . longest_path . ',(' . path . ')'
			"let tst+=1
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

	function! s:expand_opt(...)
		return eval( a:1 )
	endfunction

	let expanded_aliases = []
	for alias in a:aliases
		let alias = substitute( alias, '%{\([^}]*\)}', '\=s:expand_opt( submatch(1) )', '' )
		let expanded_aliases += [ alias ]
	endfor

	let alias_lens = deepcopy( expanded_aliases )
	call map( alias_lens, 'strlen(v:val)' )
	let longest = max( alias_lens )
	"let longest = min( [longest, 20] )
	let fmt = "%" . longest . "s "

	let out_aliases = []
	"let col = 1
	"let lines = 0

	for idx in range( 0, len( a:aliases ) - 1 )
		let alias = a:aliases[ idx ]
		let expanded_alias = expanded_aliases[ idx ]
		let s = printf( fmt, expanded_alias ) . '>>' . g:CD_aliases[ alias ]
		let out_aliases += [ s ]
		"let lines += 1
	endfor

	return out_aliases
endfunction




function! s:Get_paths( in_path )

	let in_path = a:in_path

	" On windows, at least, reading the directory files can be slow:
"	let key = s:Cmd . ' ' . in_path
"	if has_key( g:CD_glob_cache, key )
"		if g:CD_glob_cache[ key ].timestamp ==
"		\ getftime( g:CD_glob_cache[ key ].dir )
"			"echomsg 'returning cached ' . key
"			return g:CD_glob_cache[ key ].paths 
"		endif
"	endif

	if s:isdirectory_cached( in_path )
		let dir = in_path
	elseif s:isdirectory_cached( s:fnamemodify_pc( in_path, ':h' ) )
		let dir = s:isdirectory_cached( s:fnamemodify_pc( in_path, ':h' ) )
	else
		"echomsg 'CD_Plus err: cannot find directory for path (' . in_path . ')'
		return []
	endif

"	let g:CD_glob_cache[ key ] = {}
"	let g:CD_glob_cache[ key ].dir = dir
"
"	let g:CD_glob_cache[ key ].timestamp =
"		\ getftime( g:CD_glob_cache[ key ].dir )

	let paths = []

	let in_path = in_path =~ '\*$' ? in_path : in_path . '*'

	"echomsg 'in_path='.in_path
	let globs = glob( in_path )
	"echomsg 'globs='.globs
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



	if g:CD_sort_func == '' || g:CD_sort_func == 'default'
		let paths = sort( paths )
	else
		let paths = {g:CD_sort_func}( paths )
	endif

"	if in_path =~ '\*\*'
"		"echomsg 'path='.in_path.', globs='.globs
"		echomsg 'path='.in_path.', paths='.string(paths)
"	endif

	return paths

endfunction




function! Sort_by_ftime( paths )
	let s:path_times = {}
	for path in a:paths
		let s:path_times[ path ] = getftime( path )
	endfor
	return sort( a:paths, "Sort_by_ftime0")
endfunction

function! Sort_by_ftime0(i1, i2)
	let t1 = s:path_times[ a:i1 ]
	let t2 = s:path_times[ a:i2 ]
	return t1 == t2 ? 0 : t1 < t2 ? 1 : -1
endfunction



function! Sort_by_extension( paths )
	return sort( a:paths, "Sort_by_extension0" )
endfunction

function! Sort_by_extension0(i1, i2)
	let ext1 = matchstr( a:i1, '\.[^\.]\+$' )
	let ext2 = matchstr( a:i2, '\.[^\.]\+$' )

	let ret = 0

	if a:i1 == a:i2
		let ret = 0
	elseif ext1 == ext2
		let ret = a:i1 > a:i2 ? 1 : -1
	else
		let ret = ext1 > ext2 ? 1 : -1
	endif

	return ret

endfunction

"	function! Dict_by_strlen(i1, i2)
"		let len1 = strlen( s:sort_dict[ a:i1 ] )
"		let len2 = strlen( s:sort_dict[ a:i2 ] )
"		return len1 == len2 ? 0 : len1 > len2 ? 1 : -1
"	endfunction




function! s:Get_matching_paths( paths, look )
	let paths1 = []
	let look = a:look

	" escape dots if they aren't part of a .*
	"
	let look = substitute( look, '\([^\\]\)\.\([^*]\)', '\1\\.\2', 'g' )

	" make file type glob * into regex .*
	"
	let look = substitute( look, '\([^.*]\)\*', '\1.*', 'g' )

	" Fix when ** is changed to .** above
	"let look = substitute( look, '\.\*\*', '[^/]*', 'g' )
	let look = substitute( look, '\.\*\*', '.*', 'g' )

	let look = escape( look, ' ~' )

	for path in a:paths
		try
			if path =~? '^' . look 
				let path = substitute( path, '/*$', '', '' )
				let paths1 += [ path ]
"echomsg "\nyes match: \nlook ".look . ', path=' . path
"else
"echomsg "\nno match: \nlook ".look . ', path=' . path
			endif
		catch
			" Throw away regex errors
		endtry
	endfor
"echomsg string(paths1)
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
	let g:CD_rc_file = $HOME . '/.vim_cd_plus'
	silent 1 split
	try
		silent exe 'edit ' . g:CD_rc_file
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

		call append("$", 'let g:CD_dsp_wrap_len = ' . g:CD_dsp_wrap_len )

		call append("$", 'let g:CD_autochdir = "' . g:CD_autochdir . '"')

		call append("$", 'let g:CD_match_collapse = ' . g:CD_match_collapse  )

		call append("$", 'let g:CD_do_highlighting = ' . g:CD_do_highlighting  )

		silent write!

	catch
		echomsg "Caught error " . v:errmsg . " when trying to save to config file, " . g:CD_rc_file
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
