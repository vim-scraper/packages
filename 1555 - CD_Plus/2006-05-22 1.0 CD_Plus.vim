
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
" 						- initial release
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
" 		The completion is optimized for directory browsing.
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
"	<CR>		Execute a "cd" command to the currently shown directory.
"
"	<ESC>		Jump into the __CD__ display window.   <ESC> jumps back.
"
"	<C-C>		^C quits without changing directories.
"
"	<C-N>		Forward/backward in the directory history
"	<C-P>		
"
"
"
"	Other Commands:
"
"		The built-in commands are implemented as aliases (as are pc-drive
"		names).  You can override them, but then it's up to you to create
"		new ones.
"
"	'set :'		Set an alias to the last directory displayed.
"
"	'delete :'	Delete an alias.
"
"	'help : '	This listing.
"
"	'history :'	Browse the directory history.
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
"		let g:Cd_history_max = 100
"		let g:Cd_scan_pc_drives = 0
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


if !exists('g:Cd_aliases')			| let g:Cd_aliases = {} | endif
if !exists('g:Cd_scan_pc_drives')	| let g:Cd_scan_pc_drives = 0 | endif
if !exists('g:Cd_history')			| let g:Cd_history = [] | endif
if !exists('g:Cd_history_max')		| let g:Cd_history_max = 100 | endif
let s:Cd_history_idx = 0


call extend( g:Cd_aliases, { 
				\ 'set' : 'set :', 's ' : 'set :'
				\ , 'delete' : 'delete :', 'd ' : 'delete :' 
				\ , 'help' : 'help :'
				\ , 'history' : 'history :'
				\ } 
				\ )

if g:Cd_scan_pc_drives
	for i in range( 0, 255 ) 
		let a = nr2char(i)
		if a !~# '[A-Z]' | continue | endif
		let dir = a . ':'
		if has_key( g:Cd_aliases, dir ) | continue | endif
		if isdirectory( dir )
			let g:Cd_aliases[ dir ] = dir . '/'
		endif
	endfor
endif


let s:Commands_patt = '^\(set\|delete\|help\|history\) '

let s:RC_file = $HOME . '/.vim_cd_plus'
if filereadable( s:RC_file )
	exe 'so ' . s:RC_file
endif





let s:tab_dir_list = []
let s:tab_dir_list_idx = -1

function! CD_Plus( cd_cmd )

	if a:cd_cmd =~? '\(cd\|lcd\)'
	else
		echomsg "CD_Plus: bad command argument."
		return ''
	endif

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


	let prompt = ":" . a:cd_cmd . " "
	
	let inp_char= ''
	let inp_stack = []
	let inp = getcwd() . '/'
	call s:Add_input_stack( inp_stack, inp )
	let dirs = s:Get_dirs( inp )

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
			let s = &splitbelow
			set splitbelow
			silent 6 split
			let &splitbelow = s
			setlocal bufhidden=hide
			if bufnr > -1
				silent exe 'buf ' . bufnr
			else
				silent edit __CD__
			endif
		catch
		finally
			"
			" Attempt to find a reliable way to reset cmdheight:
			"
			aug Cd_auto
				au!
				exe "au BufHidden <buffer> set cmdheight=" . s:save_cmdheight
			aug END
			exe "nmap <silent> <buffer> <esc> <esc>:set cmdheight=" . s:save_cmdheight . "<CR>:cd"
		endtry
	endif
	silent let s:cd_winheight = 6
	silent set buftype=nofile
	silent set noswapfile

	while 1
		let &cmdheight = 1
		let winnr = bufwinnr( bufnr("__CD__") )
		exe winnr . " wincmd w"
		exe "resize " . s:cd_winheight
		


		if inp == ''
			let dirs = s:Get_dirs( $HOME . '/' )
		else
			let dirs = s:Get_dirs( inp )
		endif
		if len( dirs ) < 1
			let dirs = s:Get_dirs( inp . '/' )
			if len( dirs ) > 0
				let inp .= '/'
			endif
		endif


		let replace_trailing = matchstr( inp, '/*$' )
		let inp_tail = s:fnamemodify_pc( inp, ':t' )

		let matching_dirs = s:Get_matching_dirs( dirs, inp )

		let matching_aliases = s:Get_matching_aliases( inp_tail )
	

		let matching_dir_tails = map( deepcopy( matching_dirs ), 
					\ 's:fnamemodify_pc( v:val, ":t" ) ' )
		let longest_dir = s:Get_longest_common_dir( matching_dirs )
		let longest_alias = s:Get_longest_common_alias( matching_aliases )
		let longest_common = s:Get_longest_common_prefix( matching_dir_tails + matching_aliases )


		" Give priority to directory name in current directory, over same
		" alias name:
		"
		if has_key( g:Cd_aliases, longest_common )
					\&& len( matching_dirs ) < 1
					"\&& longest_dir !~ longest_alias
			let longest_dir = g:Cd_aliases[ longest_alias ] 
			let matching_dirs = s:Get_dirs( longest_dir . '/' )
			let replace_trailing = '/'

		elseif inp == '' && longest_common == ''
			let longest_dir = ''

		elseif inp != ''
			let longest_dir = s:fnamemodify_pc( inp, ':h' )
			let longest_dir = substitute( longest_dir, '/*$', '/', '' )
			let longest_dir .= longest_common
		endif


		" ------------------------------------------------------------
		"
		"  Trap special cases:
		"
		if inp =~ '\.\.$'
			let inp = s:fnamemodify_pc( inp, ':h' )	" remove ..
			let inp = s:fnamemodify_pc( inp, ':h' )	" remove current dir
			let inp .= '/'
			let dirs = s:Get_dirs( inp )
			" reset stack so backspace doesn't return to lower directory
			let inp_stack = [ inp ]
			continue

		elseif inp =~? '/\~$'
			let inp = substitute( $HOME, '\\', '/', 'g' ) . '/'
			let dirs = s:Get_dirs( inp )
			continue

		elseif inp =~? s:Commands_patt
			let save_inp = inp

			while !isdirectory( inp ) && len( inp_stack ) > 0
				let inp = remove( inp_stack, -1 )
			endwhile

			let ret = s:Do_commands( save_inp, inp )

			if ret != ''
				let inp = ret
			endif

			let inp = substitute( inp, '/*$', '/', '' )
			let dirs = s:Get_dirs( inp )
			continue

		elseif longest_dir =~? s:Commands_patt
			let inp = longest_dir
			continue

		elseif inp =~ '[.\\]$' || inp =~ '\*'
			"
			" Allow globs and regex to continue unmolested.
			"
			call s:Add_input_stack( inp_stack, inp )

		elseif longest_common == '' 
		\ && inp_char != ''
		\ && inp !~ '/$' 
		\ && inp =~ '/'
			" 
			" Throw away non-matching chars, usually typed accidentally, or
			" redundantly:
			"
			let inp = inp[0:-2]
			let inp_char = ''

			" Try again with the shorter string before continuing to getchar()
			continue
		elseif inp != ''
			let inp = longest_dir == '' ? inp : longest_dir
			let inp = substitute( inp, '/*$', replace_trailing, '' )
			call s:Add_input_stack( inp_stack, inp )
		endif





		" ------------------------------------------------------------
		"
		"  Print it out
		"

		if inp == ''
			" All will be matched, so filter out commands.
			let matching_aliases = filter( matching_aliases, 'g:Cd_aliases[ v:val ] !~ s:Commands_patt ' )
		endif

		if s:tab_dir_list_idx > -1
			let show_dirs = deepcopy( s:tab_dir_list )
		else
			let show_dirs = deepcopy( matching_dirs )
		endif

		let show_dirs = map(  show_dirs, 's:fnamemodify_pc( v:val, ":t" ) . "/" ' )

		if match( longest_dir, '/$' ) > -1
			" Clear aliases when ending in /, since all will be matched.
			let matching_aliases = []
		endif


		silent %d _

		let show_dirs = s:Format_dirs( show_dirs )
		let show_aliases = s:Format_aliases( matching_aliases )
		for str in show_dirs
			call append("$", str)
		endfor
		for str in show_aliases
			call append("$", str)
		endfor
		1d _
		2match diffchange /^\s*\zs.*\ze >>/

		let lines = len( show_dirs + show_aliases )
		if ( lines ) > s:cd_winheight
			let s:cd_winheight = lines 
			exe "resize " . s:cd_winheight
		elseif ( lines ) < s:cd_winheight 
		endif

		redraw

		echon prompt 
		if isdirectory( longest_dir ) && longest_alias == ''
		elseif has_key( g:Cd_aliases, longest_common ) 
		\&& longest_dir == g:Cd_aliases[ longest_alias ] 
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
				let dirs = s:Get_dirs( inp )
				continue
			else
			endif
		endif

		if	inp_char =~ "\\(\<esc>\\|\<c-c>\\)" 
			normal :
			break
		elseif	inp_char =~ "\\(\<c-w>\\|\<c-h>\\|\<bs>\\)" 
			if len( inp_stack ) > 1 && inp_char !~ "\<c-w>"
				call remove( inp_stack, -1 )
				let inp = remove( inp_stack, -1 )
				let dirs = s:Get_dirs( inp )
			elseif strlen( inp ) > 0
				let inp = s:fnamemodify_pc( inp, ':h' )
				let dirs = s:Get_dirs( inp )
				let inp_stack = []
			else
				let inp = ''
				let inp_stack = []
			endif

		elseif	inp_char =~ "\<c-l>"
			redraw

		elseif	inp_char =~ "\<c-u>"
			let inp = ''

		elseif	inp_char =~ "\<c-p>"
			let s:Cd_history_idx -= 1
			if s:Cd_history_idx < 0
				let s:Cd_history_idx = len( g:Cd_history ) - 1
			endif
			let inp = g:Cd_history[ s:Cd_history_idx ]

		elseif	inp_char =~ "\<c-n>"
			let s:Cd_history_idx += 1
			if s:Cd_history_idx >= len( g:Cd_history )
				let s:Cd_history_idx = 0
			endif
			let inp = g:Cd_history[ s:Cd_history_idx ]

		elseif	inp_char == "/" && inp !~ '^\a:/$'

			let inp = substitute( inp, '/*$', '', '' )

			" Expand any globs:
			"
			let dirs1 = s:Get_dirs( inp )
			if len( dirs1 ) > 0
				let inp = dirs1[0]
				let inp .= '/'
				let dirs = s:Get_dirs( inp )
			endif

		elseif	inp_char =~ "\<tab>" 
			if s:tab_dir_list_idx == -1
				let s:tab_dir_list_idx = 0
				if len( matching_dirs ) < 1
					let inp = substitute( inp, '/*$', '/', '' )
					let dirs = s:Get_dirs( inp )
					if len( dirs ) > 0
						let matching_dirs = dirs
					else
					endif
				endif
				let s:tab_dir_list = matching_dirs " + matching_aliases

				let s:tab_dir_list = filter( matching_dirs, 'v:val !~ ''\.\+$'' ' )
			elseif s:tab_dir_list_idx >= len( s:tab_dir_list )
				let s:tab_dir_list_idx = 0
			endif
			if len( s:tab_dir_list ) > 0
				let inp = s:tab_dir_list[ s:tab_dir_list_idx ]
				let s:tab_dir_list_idx += 1
			endif
			exe 'match wildmenu ;' . escape( s:fnamemodify_pc( inp, ':t' ), ' [].-' ) . '\ze/;'

		elseif	inp_char =~ "\\(\<cr>\\|\<nl>\\)" 
			if inp == ''
				let inp = $HOME
			endif
			if !isdirectory( inp )
				echomsg inp . " is not a directory."
				break
			endif

			while index( g:Cd_history, inp ) > -1
				call remove( g:Cd_history, index( g:Cd_history, inp ) )
			endwhile
			call add( g:Cd_history, inp )
			while len( g:Cd_history ) > g:Cd_history_max
				call remove( g:Cd_history, 0 )
			endwhile
			call s:Save_cfg()

			break

		else 
			let inp .= inp_char
			let inp = substitute( inp, '^\s*', '', '' )
			let dirs = s:Get_dirs( inp )

		endif

	endwhile


	let g:Cd_aliases[ '-' ] = old_cwd

	let bufnr = bufnr("__CD__")
	let winnr = bufwinnr( bufnr )
	if winnr > 0
		exe winnr . " wincmd w"
		if inp_char != "\<ESC>"
			let &lazyredraw = s:save_lazyredraw
			"let &isfname = s:save_isfname
			let &cmdheight = s:save_cmdheight
			silent hide
		else
		endif
	endif

	exe bufwinnr( start_bufnr ) . " wincmd w"

	if	inp_char =~ "\\(\<cr>\\|\<nl>\\)" 
		exe a:cd_cmd . " " . inp
		redraw
		echo a:cd_cmd . " " . inp
	endif

endfunction




function! s:Do_history()

	let winnr = bufwinnr( bufnr("__CD_HISTORY__") )
	if winnr > 0
		exe winnr . " wincmd w"
	else
		wincmd b
		wincmd k
		6 split __CD_HISTORY__
	endif
	setlocal buftype=nofile
	setlocal noswapfile
	setlocal bufhidden=wipe
	%d _

	let idx = len( g:Cd_history )
	for dir in g:Cd_history
		call append("$", printf( "%-3d ", idx ) . dir )
		let idx -= 1
	endfor
	exe "resize " . len( g:Cd_history )

	$
	redraw

	try
		let idx = input("Enter # ")
		if idx == '' || idx < 0 || idx > len( g:Cd_history ) - 1
			hide
			return ''
		endif
	catch
		" Catch ^C
	endtry

	hide

	return g:Cd_history[ len(g:Cd_history) - idx ]
endfunction




function! s:Do_commands( cmd, dir )

	if a:cmd =~? '^history'
		return s:Do_history()
	endif

	if a:cmd =~? '^set'
		let prompt = 'set alias for ' . a:dir . ' : '
	elseif a:cmd =~? '^delete'
		%d _
		let l = filter( keys(g:Cd_aliases) , 'g:Cd_aliases[ v:val ] !~ s:Commands_patt ' )
		call append("$",  s:Format_aliases( l ) )
		2match diffchange /^\s*\zs.*\ze >>/
		exe 'resize ' . len(l)
		redraw
		let prompt = 'delete alias for : '
	elseif a:cmd =~? '^help'
		call CD_Plus_help_extract_tmp_buf()
		return a:dir
	else
		"echomsg "Bad command " . a:cmd
	endif

	let cmd = input( prompt, "" )

	if cmd == '' | return a:dir | endif

	if a:cmd =~? '^set'
		let g:Cd_aliases[ cmd . ' ' ] = a:dir
	elseif a:cmd =~? '^delete'
		silent! call remove( g:Cd_aliases, cmd )
		silent! call remove( g:Cd_aliases, cmd . ' ' )
	endif

	call s:Save_cfg()
	return a:dir
endfunction



function! s:Add_input_stack( inp_stack, inp )

	" Don't add to stack when tabbing is active:
	if s:tab_dir_list_idx > -1 | return | endif

	if len(a:inp_stack) > 0 && a:inp == a:inp_stack[-1]
		" Don't add duplicates
	else
		call add( a:inp_stack, a:inp )
	endif
endfunction




function! s:Format_dirs( dirs )
	if len( a:dirs ) < 1 | return [] | endif
	let dir_lens = deepcopy( a:dirs )
	call map( dir_lens, 'strlen(v:val)' )
	let longest = max( dir_lens )
	let longest = min( [longest, 19] )		" 19+1 space divides evenly usually
	let fmt  = "%-" . longest . "." . longest . "s "
	let fmt1 = "%" . longest . "." . longest . "s "
	let fmt1 = fmt	" fmt1 not used right now
	let out_dirs = []

	" First, wrap file names which are too long:
	"
	for dir in deepcopy( a:dirs )
		let part = ''
		while strlen( dir ) > longest
			let part = strpart( dir, 0, longest - 1 )
			let out_dirs += [ printf( fmt1, part ) ]
			let dir = strpart( dir, longest - 1 )
		endwhile
		let fmt2 = part == '' ? fmt : fmt1
		let out_dirs += [ printf( fmt2, dir ) ]
	endfor


	let cols = 0 + ( winwidth(winnr()) / ( longest + 1 ) ) 
	let rows = 1 + ( len( out_dirs ) / cols ) 

	let out_lines = []

	let idx = 0
	while idx < len( out_dirs )
		for col in range( 0, cols - 1 )
			for row in range( 0, rows - 1 )
				"echomsg row . ',' . col
				while row >= len( out_lines )
					call add( out_lines, '' )
				endwhile
				let out_lines[row] .= out_dirs[ idx ]
				let idx += 1
				if idx >= len( out_dirs )
					return out_lines
				endif
			endfor
		endfor
		let cols = 1
		" Kludge alert!
	endwhile


	echomsg 'eeek........ didnt finish directory list: '
			\ . 'idx=' . idx . ',' . string( out_dirs[ idx : ] )
	return out_dirs[ idx : ]

"	for dir in a:dirs
"		let dir1 = printf( fmt, dir )
"		if ( 1 + strlen( dir1 ) + strlen( out ) ) >= winwidth( winnr() )
"			let out_dirs += [ out ]
"			let out = ''
"		endif
"		let out .= dir1
"	endfor
"	let out_dirs += [ out ]

"	return out_dirs
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
		let alias1 = printf( fmt, alias ) . '>>' . g:Cd_aliases[ alias ]
		let out_aliases += [ alias1 ]
		let lines += 1
	endfor

	return out_aliases
endfunction




function! s:Get_dirs( in_dir )

	let s:tab_dir_list_idx = -1
	match NONE
	let in_dir = a:in_dir
	let dirs = []

	let in_dir = in_dir =~ '\*$' ? in_dir : in_dir . '*'

	let globs = glob( in_dir )
	if in_dir =~ '/\*$'

		" get directories normally hidden with a "."
		"
		let globs .= "\n" . glob( substitute( in_dir, '/\*$', '/.*', '' ) )
	endif

	for file in split( globs, "\n" )
		if isdirectory( file )
			let dirs += [ file ]
		endif
	endfor

	return sort( dirs )


	"	Uniq -- not needed?
	"
"	let dirs2 = []
"	for i in range( 1, len(dirs) - 1 )
"		if dirs[i] != dirs[i-1]
"			call add( dirs2, dirs[i] )
"		endif
"	endfor

"	return sort( dirs2 )

endfunction





function! s:Get_matching_dirs( dirs, look )
	let dirs1 = []
	let look = a:look

	" escape dots if they aren't part of a .*
	"
	let look = substitute( look, '\([^\\]\)\.\([^*]\)', '\1\\.\2', 'g' )

	" make file type glob * into .*
	"
	let look = substitute( look, '\([^.]\)\*', '\1.*', 'g' )

	let look = escape( look, ' ' )

	for dir in a:dirs
		try
			if dir =~? '^' . look 
				let dir = substitute( dir, '/*$', '', '' )
				let dirs1 += [ dir ]
			endif
		catch
			" Throw away regex errors
		endtry
	endfor
	return dirs1
endfunction



function! s:Get_matching_aliases( look )
	let out_list = []
	for alias in sort( keys( g:Cd_aliases ) )
		try
			if alias =~? '^' . a:look
				let out_list += [ alias ]
			endif
		catch
		endtry
	endfor
	return out_list
endfunction





function! s:Get_longest_common_dir( dirs )
	return s:Get_longest_common_prefix( a:dirs )
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

		call append("$", 'call extend( g:Cd_aliases, { ' )
		for key in keys( g:Cd_aliases ) 
			call append("$", "\\ '" . key . "' : '" . g:Cd_aliases[key] .  "'," )
		endfor
		call append("$", "\\ } ) " )

		call append("$", 'let g:Cd_history = [' )
		for dir in g:Cd_history 
			call append("$", "\\ '" . dir . "'," )
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
	if a:flags =~ ':t' && match( a:fname, '/[^/]\+$' ) > -1
		return matchstr( a:fname, '[^/]\+$' )
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

	" Grrrr!  No local mode for these:
	"let s:restore_foldopen = &foldopen
	"let s:restore_foldclose = &foldclose

	" Use new then edit, as a simple way to deal with an existing hidden 
	" help buffer.
	wincmd t
	silent new 20
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
   return ":call CD_Plus('" . a:cd_cmd . "')\<cr>"
endfunction

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
