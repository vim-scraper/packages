" ==============================================================================
" Name:        ShowMarks
" Description: Visually displays the location of marks local to a buffer.
" Authors:     Anthony Kruize <trandor@labyrinth.net.au>
"              Michael Geddes <michaelrgeddes@optushome.com.au>
" Version:     1.1
" Modified:    6 February 2002
" License:     Released into the public domain.
" ChangeLog:   1.1 - Added support for the A-Z marks.
"                    Fixed sign staying placed if the line it was on is deleted.
"                    Clear autocommands before making new ones.
"              1.0 - First release.
" Usage:       Copy this file into the plugins directory so it will be
"              automatically sourced.
"
"              Default keymappings are:
"                <Leader>mt  - Toggles ShowMarks on and off.
"                <Leader>mh  - Hides a mark.
"
"              Hiding a mark doesn't actually remove it, it simply moves it to
"              line 1 and hides it visually.
"
" ==============================================================================

" Check if we should continue loading
if exists( "loaded_showmarks" )
	finish
endif
let loaded_showmarks = 1

" Mappings
if !hasmapto( '<Plug>ShowmarksShowMarksToggle' )
	map <unique> <leader>mt <Plug>ShowmarksShowMarksToggle
endif
if !hasmapto( '<Plug>ShowmarksHideMark' )
	map <unique> <leader>mh <Plug>ShowmarksHideMark
endif

noremap <unique> <script> <Plug>ShowmarksShowMarksToggle :call <SID>ShowMarksToggle()<CR>
noremap <unique> <script> <Plug>ShowmarksHideMark :call <SID>HideMark()<CR>
noremap <unique> <script> \sm m
noremap m :exe 'norm \sm'.nr2char(getchar())<bar>call <sid>ShowMarks()<CR>

" AutoCommands:
" CursorHold checks the marks and set the signs
" GuiEnter loads the default theme for graphical icons
aug ShowMarks
	au!
	autocmd CursorHold * call s:ShowMarks()
aug END

" Highlighting: Setup some nice colours to show the mark position.
hi default ShowMarksHL ctermfg=blue ctermbg=lightblue cterm=bold guifg=blue guibg=lightblue gui=bold

" Setup the sign definitions for each mark
fun! s:ShowMarksSetup()
	let n = 0
	while n < 26
		let c = nr2char(char2nr('a') + n)
		let C = nr2char(char2nr('A') + n)
		exe 'sign define ShowMark'.c.' text='.c.'> texthl=ShowMarksHL'
		exe 'sign define ShowMark'.C.' text='.C.'> texthl=ShowMarksHL'
		let n = n + 1
	endw
endf

" Set things up
call s:ShowMarksSetup()

" Toggle whether we display marks
fun! s:ShowMarksToggle()
	if !exists("b:ShowMarks_Enabled")
		let b:ShowMarks_Enabled = 1
	endif

	if b:ShowMarks_Enabled == 0
		let b:ShowMarks_Enabled = 1
		aug ShowMarks
			autocmd CursorHold * call s:ShowMarks()
		aug END
	else
		let b:ShowMarks_Enabled = 0
		let n = 0
		while n < 26
			let c = nr2char(char2nr('a') + n)
			let C = nr2char(char2nr('A') + n)
			let id = n + 52 * winbufnr(0)
			let ID = id + 26
			if exists('b:placed_'.c)
				let b:placed_{c} = 1
				exe 'sign unplace '.id.' buffer='.winbufnr(0)
			endif
			if exists('b:placed_'.C)
				let b:placed_{C} = 1
				exe 'sign unplace '.ID.' buffer='.winbufnr(0)
			endif
			let n = n + 1
		endw
		aug ShowMarks
			au!
		aug END
	endif
endf

" This function is called on the CursorHold autocommand.
" It runs through all the marks and displays or removes signs as appropriate.
fun! s:ShowMarks()
	if exists("b:ShowMarks_Enabled")
		if b:ShowMarks_Enabled == 0
			return
		endif
	endif

	let n = 0
	while n < 26
		let c = nr2char(char2nr('a') + n)
		let id = n + 52 * winbufnr(0)
		let ln = line("'".c)
		let C = nr2char(char2nr('A') + n)
		let ID = id + 26
		let LN = line("'".C)
		if ln == 0 && (exists('b:placed_'.c) && b:placed_{c} != ln )
			exe 'sign unplace '.id.' buffer='.winbufnr(0)
		elseif ln != 0 && (!exists('b:placed_'.c) || b:placed_{c} != ln )
			echo id
			exe 'sign unplace '.id.' buffer='.winbufnr(0)
			exe 'sign place '.id.' name=ShowMark'.c.' line='.ln.' buffer='.winbufnr(0)
		endif
		if LN == 0 && (exists('b:placed_'.C) && b:placed_{C} != LN )
			exe 'sign unplace '.ID.' buffer='.winbufnr(0)
		elseif LN != 0 && (!exists('b:placed_'.C) || b:placed_{C} != LN )
			echo ID
			exe 'sign unplace '.ID.' buffer='.winbufnr(0)
			exe 'sign place '.ID.' name=ShowMark'.C.' line='.LN.' buffer='.winbufnr(0)
		endif
		let b:placed_{c} = ln
		let b:placed_{C} = LN
		let n = n + 1
	endw
endf

" Hide the mark at the current line.
" This simply moves the mark to line 1 and hides the sign.
fun! s:HideMark()
	let ln = line(".")
	let n = 0
	while n < 26
		let c = nr2char(char2nr('a') + n)
		let C = nr2char(char2nr('A') + n)
		let markln = line("'".c)
		let markLN = line("'".C)
		if ln == markln
			let id = n + 52 * winbufnr(0)
			exe 'sign unplace '.id.' buffer='.winbufnr(0)
			exe '1 mark '.c
			let b:placed_{c} = 1
		endif
		if ln == markLN
			let ID = (n + 52 * winbufnr(0)) + 26
			exe 'sign unplace '.ID.' buffer='.winbufnr(0)
			exe '1 mark '.C
			let b:placed_{C} = 1
		endif
		let n = n + 1
	endw
endf
