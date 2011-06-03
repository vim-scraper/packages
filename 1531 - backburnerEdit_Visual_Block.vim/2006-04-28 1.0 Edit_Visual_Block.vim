
" ---------------------------------------------------------------------------
"
" Edit_Visual_Block.vim :	Edit the text in a Visual Block as 
" 							if it were in a virtual window.
" 
"
" Author:		Eric Arnold ( eric_p_arnold@yahoo.com )
" Created:		Apr, 2006
" Updated:		Fri Apr 28, 04/28/2006 11:07:42 AM
" Requirements:	Vim 7
"
" Version:		1.0		Fri Apr 28, 04/28/2006 11:07:42 AM
" 						Initial release
"
"
"	Description:
"	
"		Creates a virtual 'edit window' inside the visual selection area.
"		Once the boundary box is drawn, you can insert/replace/cut/paste as
"		normal, and it will try to maintain the boundary box in some
"		reasonable fashion.
"
"	Usage:
"
"		i	Select a Visual Block (with ^V or ^Q), and type 'i' (default
"			mapping).  It will draw a box around the visual area, and return
"			you to Vim Normal mode.  Begin editing.  See Notes for things
"			not to do.
"
"		<ESC>
"			Hitting escape while in Vim Normal mode will terminate the
"			Edit Visual Block mode, and return to the visual block
"			selection.
"
"	Setup:
"
"		-	Put into your plugin directory, or source as desired.
"
"		-	Decide what key to map.
"
"	Notes:
"
"	-	While this mode is in effect, for the lines that are part of
"		the seletion, characters outside of the visual block selection
"		cannot be changed,.  It needs these strings as markers for the
"		boundaries of the virtual edit window.
"
"		If changes are made, the boundary strings will be restored, with
"		other text moved inwards, if possible.
"		
"	-	Deleting or adding lines in the selected area is not handled or
"		recommended.
"
"	-	Pasting simple block-wise("^V"), or line-wise ("V") text will
"		generally do something useful, but pasting multi-line char-wise
"		("v"), will generally do something messy.
"			
"	-	Until I can decypher 'undojoin', you will have use more than one
"		'undo' at a time, i.e.  '2u' or '3u' or whatever, to undo the last
"		change while the mode is in effect.


if v:version < 700
	echomsg "Cannot load Edit_Visual_Block, Vim 7 required."
	finish
endif


" ----------------------------------------------------------------------
"
" I don't think 'i' or 'a' are used for anything, but if I'm wrong, use
" some other chars.
"
vnoremap <silent> i <esc>:call EVB_start()<cr>
"vnoremap <silent> a <esc>:call EVB_start()<cr>



"	-	It sets 'updatetime' which can interfere with multi-key commands
"		so change g:EVB_updatetime if it's a problem.
"
"if !exists('g:EVB_updatetime')
"	let g:EVB_updatetime = 1000
"endif



" ----------------------------------------------------------------------
"
"
function! EVB_start()
	nnoremap <buffer> <silent> <esc> <esc>:call EVB_end()<CR>
	nnoremap <buffer> <silent> <c-c> <esc>:call EVB_end()<CR>

	call EVB_visual_block_normalize()
	call EVB_init_brackets(
				\ b:visual_line_start,
				\ b:visual_col_start,
				\ b:visual_lines,
				\ b:visual_cols )

	let b:save_shiftwidth = &shiftwidth
	let &shiftwidth = 1

"	let s:save_updatetime = &updatetime
"	let &updatetime = g:EVB_updatetime

	let b:visual_maxlen = 0

	exe "normal! \<esc>"
	normal! g`<

	let b:has_active_block = 1

	aug Edit_Visual_Block
		au!
		au CursorMoved * call EVB_insert()
		au CursorMovedI * call EVB_insert()
		au CursorHold * call EVB_insert()
		au CursorHoldI * call EVB_insert()
	aug END

	let s:save_eventignore = &eventignore

	set eventignore-=CursorMoved
	set eventignore-=CursorMovedI

endfunction



" ----------------------------------------------------------------------
"
"
function! EVB_end()
	nunmap <buffer> <esc>
	nunmap <buffer> <c-c>

	let b:has_active_block = 0

	aug Edit_Visual_Block
	au!
	aug END

	call s:EVB_syn_clear()

	call EVB_visual_block_set(
				\ b:visual_line_start,
				\ b:visual_col_start,
				\ b:visual_line_end,
				\ b:EVB_right_bracket_col - 1 )

	let &shiftwidth = b:save_shiftwidth
"	let &updatetime = s:save_updatetime
	let &eventignore = s:save_eventignore

endfunction



" ---------------------------------------------------------------------------
"
" Called for every CursorMoved[I] event to re-adjust the boundaries of the
" selected area.
"
let s:counter =0
function! EVB_insert()
	let s:counter +=1
	"echomsg 'count#' . s:counter . ',len:' . strlen(getline("."))

	if !exists('b:has_active_block') || !b:has_active_block | return | endif

	" Doesn't work:
	"silent undojoin

	let b:cursor_line = line(".")
	let b:cursor_col = col(".")

	let cols = b:cursor_col - b:visual_col_start + 1

	if EVB_adjust_brackets( ) < 0
		return
	endif

	call EVB_visual_block_inverse_match(
				\ b:visual_line_start,
				\ b:visual_col_start,
				\ b:visual_line_end,
				\ b:EVB_right_bracket_col - 1 )

	call cursor( b:cursor_line, b:cursor_col )
	return
endfunction






" ---------------------------------------------------------------------------
"
" Normalize the Visual Block start position ( `< ) to the top left, and the
" end ( `> ) position the boltom right.  Otherwise, the start/end marks could
" be in any corner of the block depending on how the user selected it.
"
function! EVB_visual_block_normalize()

	" Don't trigger recursive updates:
	set eventignore+=CursorMoved
	set eventignore+=CursorMovedI

	" Position in top left:
	"exe "silent normal! \<esc>gvdu"
	exe "silent normal! \<esc>gv"

	"silent normal! g`<
	let line_start = line(".")
	let col_start = col(".")

	silent normal o

	"silent normal! g`>
	let line_end = line(".")
	let col_end = col(".")

	if line_start > line_end
		let tmp = line_start
		let line_start = line_end
		let line_end = tmp
	endif

	if col_start > col_end
		let tmp = col_start
		let col_start = col_end
		let col_end = tmp
	endif


	let b:visual_line_start = line_start
	let b:visual_col_start = col_start
	let b:visual_line_end = line_end
	let b:visual_col_end = col_end

	if &selection == 'exclusive'
		" `> puts the cursor AFTER the last col which is part of the selection.
		let b:visual_col_end -= 1
	endif


	let b:visual_lines = b:visual_line_end - b:visual_line_start + 1
	let b:visual_cols = b:visual_col_end - b:visual_col_start + 1

	let b:visual_lines = max( [ 1, b:visual_lines ] )
	let b:visual_cols = max( [ 1, b:visual_cols ] )

	let b:EVB_right_bracket_col = b:visual_col_end + 1

	call EVB_visual_block_set( b:visual_line_start, b:visual_col_start, b:visual_line_end, b:visual_col_end )

	set eventignore-=CursorMoved
	set eventignore-=CursorMovedI
endfunction




" ---------------------------------------------------------------------------
"
" Change the Visual Block selection area, as if user adjusted with ^V
"
function! EVB_visual_block_set( line_start, col_start, line_end, col_end )
	if min( [ a:line_start, a:col_start, a:line_end, a:col_end ] ) < 1
		echomsg "Edit_visual_block_set: error, some arg < 1"
		return
	endif

	set eventignore+=CursorMoved
	set eventignore+=CursorMovedI

	call cursor( a:line_start, a:col_start )
	exe "silent normal! \<ESC>\<C-V>"
	"call cursor( a:line_end, a:col_end )

	let lines = a:line_end - a:line_start + 1
	if lines > 1
		exe "silent normal! " . ( lines - 1 ) . "j"
	endif
	let cols = a:col_end - a:col_start + 1
	if cols > 1
		exe "silent normal! " . cols . "l"
	endif
	set eventignore-=CursorMoved
	set eventignore-=CursorMovedI
endfunction



" ---------------------------------------------------------------------------
"
" Get the string segments for each line in the selected block, that don't
" include the selected columns.
"
function! EVB_init_brackets( line, col, lines, cols )

	silent! syn clear EVB_window_segs

	if min( [ a:line, a:col, a:lines, a:cols ] ) < 1
		echomsg "EVB_init_brackets: error, some arg < 1"
		return -1
	endif

	let b:EVB_window_brackets = {}
	for linenum in range( a:line, a:line + a:lines - 1 )
		let line = getline( linenum )
		let b:EVB_window_brackets[ linenum ] = {}
		let b:EVB_window_brackets[ linenum ].start_seg =
					\ strpart( line, 0, a:col - 1 )
		let b:EVB_window_brackets[ linenum ].end_seg =
					\ strpart( line, ( a:col - 1 ) + a:cols )

		" for debugging:
		if 0
			let s = escape( b:EVB_window_brackets[ linenum ].start_seg,
						\ '^$*.\/[]' )
			let cmd = 'syn match EVB_window_segs '
						\ . '/^'
						\ . s
						\ . '/'
						\ . ' contained containedin=EVB_window_lines'
			exe cmd
			hi link EVB_window_segs statusline

			let s = escape( b:EVB_window_brackets[ linenum ].end_seg, 
						\ '^$*.\/[]' )
			let cmd = 'syn match EVB_window_segs '
						\ . '/'
						\ . s
						\ . '$/'
						\ . ' contained containedin=EVB_window_lines'
			exe cmd
			hi link EVB_window_segs statusline
		endif
	endfor

endfunction




" ---------------------------------------------------------------------------
"
" Maintain the Visual Block area so that the original surround text remains
" constant.
"
let b:EVB_right_bracket_col = 1

function! EVB_adjust_brackets( )

	set eventignore-=CursorMoved
	set eventignore-=CursorMovedI

	let linenums = sort( keys( b:EVB_window_brackets ) )
	let want_trim = 0
	let do_trim = 0
	let min_space_avail = 99999

	let do_continue = 0
	let need_retry = 1
	let last_retry = 1
	let safety = 0
	while need_retry && ( safety < 100 )
		let safety += 1
		let need_retry = 0
		let rightmost_col = 0  " rightmost for all the bracket end segments

	
		" Find the largest column value for all the bracket end segments:
		"
		for linenum in linenums
			if linenum > line("$")
				break
			endif

			let line = getline( linenum )
			let idx = strridx( line, b:EVB_window_brackets[ linenum ].end_seg )
			let idx1 = strridx( line, b:EVB_window_brackets[ linenum ].start_seg )
			let start_seg = b:EVB_window_brackets[ linenum ].start_seg 
			let end_seg = b:EVB_window_brackets[ linenum ].end_seg 


			" Try to handle when new lines are inserted or pasted into the
			" selected area:
			"
			let do_continue = 0
			if idx < 0 && idx1 < 0
				" 
				" First see if we can find a good candiate for what used to
				" be the current line:
				"
				let linenum1 = linenum
				let found_it = 0
				while linenum1 <= line("$")
					let line1 = getline( linenum1 )
					if strpart( line1, 0, strlen( start_seg ) ) == start_seg
						let found_it = linenum1
					endif
					let linenum1 += 1
				endwhile


				if found_it
					let save_a = @a
					exe 'silent ' . linenum . ',' . ( found_it - 1 ) . 'd a'
					let deleted_lines = split( @a, "\n", 1 )
					let @a = save_a

					"
					" If found, delete the inserted lines, and insert as many as
					" can fit into the selected area:
					"
					let linenum1 = linenum
					for line2 in deleted_lines
						if !has_key( b:EVB_window_brackets, linenum1 )
							break
						endif
						let line1 = getline( linenum1 )
						let line1 = strpart( line1, 0, strlen( start_seg ) )
									\ . line2
									\ . strpart( line1, strlen( start_seg ) )
						call setline( linenum1, line1 )
						let linenum1 += 1
					endfor
					let need_retry = 1
				endif
				break
			endif  " found an inserted line


			" Restore the ending bracket segment:
			"
			let len_seg = strlen( end_seg )
			let len_line = strlen( line )
			if idx < 0 && len_seg > 0
				let need_remove = 0
				for idx in range( 1, len_seg )
					if strpart( end_seg, len_seg - idx, 1 ) 
								\ == strpart( line, len_line - idx, 1 ) 
						let need_remove += 1
					endif
				endfor
				let line = strpart( line, 0, strlen( line ) - need_remove )
				let line .= end_seg
				call setline( linenum, line )
				let need_retry = 1
			endif

			let idx = strridx( line, b:EVB_window_brackets[ linenum ].end_seg )

			if idx < 0
				echomsg 'Edit_Visual_Block:  lost border sync, '
							\ . 'aborting from line #' . linenum
							\  . ', line=' . line .
							\', seg=' . b:EVB_window_brackets[ linenum ].end_seg
				call EVB_end()
				return -1
				" - handle Replace/overwrite:  replace bracket by starting at
				"   EOL and working backward until first mismatch is found
			endif

			let rightmost_col = max( [ idx + 1, rightmost_col ] )

		endfor " setting rightmost_col

		" proably should use try/throw
		if do_continue
			continue
		endif

		for linenum in linenums
			let contents = getline( linenum )
			let out = contents

			let idx = strridx( contents, 
						\ b:EVB_window_brackets[ linenum ].end_seg )
			let end_seg_col = idx + 1

			if do_trim
				let s1 = strpart( out, 0, idx - do_trim )
				let s2 = strpart( out, idx )
				let out = s1 . s2
				"let need_retry = 1		" bad??
				let contents = out
			endif
			

			" Restore the starting bracket segment:
			"
			let seg = b:EVB_window_brackets[ linenum ].start_seg
			if stridx( out, seg ) != 0

				for idx in range( 0, strlen( seg ) )
					if strpart( seg, idx, 1 ) != strpart( out, idx, 1 )
						let out = strpart( out, 0, idx ) .
								\ strpart( seg, idx, 1 ) .
								\ strpart( out, idx )
					endif
				endfor
				let b:cursor_col = strlen( seg ) + 1

				let idx = strridx( contents, 
							\ b:EVB_window_brackets[ linenum ].end_seg )
				let end_seg_col = idx + 1
				let need_retry = 1
			endif




			" User has deleted some chars on this line:
			" First, append spaces to visual area to restore right border,
			" then see if all lines have empty space to allow the right border
			" to move leftward.
			"
			if end_seg_col < b:EVB_right_bracket_col
				let need_spaces = rightmost_col - end_seg_col
				let want_trim += need_spaces
				let out = strpart( contents, 0, idx )
							\ . repeat( ' ', need_spaces )
							\ . strpart( contents, idx )
				let end_seg_col += need_spaces
				let need_retry = 1
			endif
			" Note: following insertion invalidates above, so do above first


			" Some line has grown beyond the right border.
			" This means that all lines must be checked for adjustment:
			"
			if rightmost_col > b:EVB_right_bracket_col
				\ && end_seg_col < rightmost_col

				let need_spaces = rightmost_col - end_seg_col
				let out = strpart( contents, 0, idx )
							\ . repeat( ' ', need_spaces )
							\ . strpart( contents, idx )

				let end_seg_col += need_spaces
				let need_retry = 1
			endif

			call setline( linenum, out )

			" Do a check for trailing space:
			let s = strpart( out, 0, end_seg_col - 1 )
			let min = matchstr( s, '  *$' )
			let min = strlen( min )
			let min_space_avail = min( [ min, min_space_avail ] )

		endfor "for linenum in linenums

		let do_trim = 0

		if want_trim 
			if want_trim >= min_space_avail
				let want_trim = min_space_avail
			endif
			let do_trim = want_trim
			let want_trim = 0
		endif
		let min_space_avail = 99999
			

		let b:EVB_right_bracket_col = rightmost_col

		" Do one last loop for clean up:
		if ! need_retry && last_retry
			let need_retry = 1
			let last_retry = 0
		endif

	endwhile " need_retry

	if safety >= 100
		echomsg "Edit_Visual_Block: Warning, safety reached"
	endif


	set eventignore-=CursorMoved
	set eventignore-=CursorMovedI

	return 1
 
endfunction




" ---------------------------------------------------------------------------
"
"  Highlight everything except the selected Visual Block
"
function! EVB_visual_block_inverse_match( line_start, col_start, line_end, col_end )

	let line_start = a:line_start
	let col_start = a:col_start
	let line_end = a:line_end
	let col_end = a:col_end


	
"	Highlight everything but the box:
"
"	silent! syn clear EVB_window_cols
"	silent! syn clear EVB_window_lines
"	silent! syn clear EVB_all
"
"
"	let line_end = a:line_start + a:lines
"	let col_end = a:col_start + a:cols
"
"	syn match EVB_all /./
"	hi link EVB_all Visual
"
"	exe 'syn match EVB_window_lines '
"				\ . '/'
"				\ . '\%' . a:line_start . 'l'
"				\ . '\_.*'
"				\ . '\%' . line_end . 'l'
"				\ . '/'
"				\ . ' contained containedin=EVB_all'
"				" transparent allows EVB_all to show through, but it
"				" inhibits subsequent EVB_window_cols from working.  Why?
"
"	hi link EVB_window_lines Visual
"
"	exe 'syn match EVB_window_cols '
"				\ . '/'
"				\ . '\%' . a:col_start . 'c'
"				\ . '.*'
"				\ . '\%' . col_end . 'c'
"				\ . '/'
"				\ . ' contained containedin=EVB_window_lines'
"
"	hi link EVB_window_cols Normal


	silent! syn clear EVB_border

	hi link EVB_border Visual

	" Top border line
	if line_start > 1
		let col_start1 = col_start
		if col_start < 2
			let col_start1 = 2
		endif
		exe 'syn match EVB_border '
					\ . '/'
					\ . '\%>' . ( line_start - 2 ) . 'l'
					\ . '\%<' . ( line_start ) . 'l'
					\ . '\%>' . ( col_start1 - 2 ) . 'c'
					\ . '\%<' . ( col_end + 2 ) . 'c'
					\ . '.'
					\ . '/'
					\ . ' containedin=ALL'
	endif

	" Bottom border line
	if line_start < line("$")
		let col_start1 = col_start
		if col_start < 2
			let col_start1 = 2
		endif
		exe 'syn match EVB_border '
					\ . '/'
					\ . '\%>' . ( line_end ) . 'l'
					\ . '\%<' . ( line_end + 2 ) . 'l'
					\ . '\%>' . ( col_start1 - 2 ) . 'c'
					\ . '\%<' . ( col_end + 2 ) . 'c'
					\ . '.'
					\ . '/'
					\ . ' containedin=ALL'
	endif

	" Left border line
	if col_start > 1

		let line_start1 = line_start
		if line_start < 2
			let line_start1 = 2
		endif
		let line_end1 = line_end
		if line_end == line("$")
			let line_end1 -= 1
		endif
		exe 'syn match EVB_border '
					\ . '/'
					\ . '\%>' . ( line_start1 - 2 ) . 'l'
					\ . '\%<' . ( line_end1 + 2 ) . 'l'
					\ . '\%>' . ( col_start - 2 ) . 'c'
					\ . '\%<' . ( col_start ) . 'c'
					\ . '.'
					\ . '/'
					\ . ' containedin=ALL'
	endif

	" Right border line
	if col_end > 1 && col_end < ( col("$") - 1 )
		let line_start1 = line_start
		if line_start < 2
			let line_start1 = 2
		endif
		let line_end1 = line_end
		if line_end == line("$")
			let line_end1 -= 1
		endif
		exe 'syn match EVB_border '
					\ . '/'
					\ . '\%>' . ( line_start1 - 2 ) . 'l'
					\ . '\%<' . ( line_end1 + 2 ) . 'l'
					\ . '\%>' . ( col_end ) . 'c'
					\ . '\%<' . ( col_end + 2 ) . 'c'
					\ . '.'
					\ . '/'
					\ . ' containedin=ALL'
	endif

endfunction




function! s:EVB_syn_clear()
	silent! syn clear EVB_window_segs
	silent! syn clear EVB_window_cols
	silent! syn clear EVB_window_lines
	silent! syn clear EVB_all
	silent! syn clear EVB_border
endfunction

