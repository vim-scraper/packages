" {{{ File header information
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
" \file		feraltogglecommentify.vim
" \date		Mon, 21 Jul 2003 13:32 PDT
"
" \brief	Adds, removes or toggles comment characters. Ranges are supported
"			as is custom text to insert/remove/toggle.
" Maintainer: (same as \author below)
" \author	Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
" Based On: {{{
" \note		Based On Vincent Nijs's ToggleCommentify.vim v1.2 Last Change:
"			Sunday, June 9th, 2001 (VIMSCRIPT#4)
" \author	Vincent Nijs <Vincent.Nijs@econ.kuleuven.ac.be>
" \note		Some comment definitions extracted from EnhancedCommentify.vim by
"			Meikel Brandmeyer
" \author	Meikel Brandmeyer <Brandels_Mikesh@web.de>
" }}}
" \note		This was VIMSCRIPT#4: (originally; See next note)
"			URL:	http://vim.sourceforge.net/script.php?script_id=4
"			I am unable to UL new versions (reasonable, wasn't my script
"			originally), so I'll make a new entry to keep updating this script
" \note		This is VIMSCRIPT#665 (now):
"			URL:	http://vim.sourceforge.net/scripts/script.php?script_id=665
" Contrabutions From: {{{
"	Bernhard Wagner		(12-Nov-2002 xml changes)
"	Jörn Horstmann.		Changes are marked with "JH:". -- Also inspired the
"						v1.55 options.
" }}}
"
" \version	$Id$
" Version:	1.56
" History: {{{
"	[Feral:202/03@13:31] 1.56
"	BUG FIX:		Fixed silly cursor restore problem, screen no longer jumps
"		on occasion.
"	[Feral:196/03@07:30] 1.55
"	Integrating changes from Jörn Horstmann. Changes are marked with "JH:".
"	Improvement:	Added options to determine how this should react.
"		* Optionally does not mess with blank lines.
"		* Change by JH, comment does not have to be in first column
"		* Change by JH, only add comment when not already commented
"		Please see "Customizing:" for details and usage:
"	[Feral:193/03@09:08] 1.54
"	BUG FIX:		(minor) no longer complains when uncommenting and there
"		are no comment char(s). (aka middle of a range is uncommented already)
"	BUG FIX:		Respects 'hlsearch'; won't mess with highlighting if off,
"		etc.  (Previous turned it on at end of script)
"	Improvement:	Can now comment/uncomment a fold without opening it.
"		(courtesy of normal! zn and normal! zN)
"	Improvement:	Changed DLAC into a proper command (:DLAC) (from mapping);
"		map <Plug>FtcDLAC as desired, defaults to <C-c>. Ranges are supported
"		(default current line) and you can specify the comment chars just as
"		you can with :CC, :TC, :UC.
"	[Feral:155/03@06:40] 1.532
"		Added: C# // comment style.
"	[Feral:336/02@16:31] 1.531
"		Addition:	Added c style /* */ pair; css also; Made it work.
"		Note: This really needs to wrap comments around a visual; Perhaps not
"			this however.
"	[Feral:317/02@05:27] 1.53
"		Addition:	Incorperated Bernhard Wagner's 12-Nov-2002 v1.52 xml
"			changes
"		Improvement:	Will make default mappings to <M-c> only if no map to
"			<Plug>FtcTC and <M-c> is unused.
"	[Feral:309/02@18:44] 1.52
"		Merged DLAC with this; DLAC = duplicate line and comment, simple
"			mappings that are handy to use to save and comment a line before
"			you mangle it. However this mangles mark z
"		Bugfix:	mismatched <Plug> names; This is <Plug>FtcTc
"	[Feral:308/02@02:11] 1.51
"		Hacked in eol comments and added html's <!-- --> comments.
"	[Feral:300/02@07:03] 1.5
"		Rewrite, condenced toggle, comment and uncomment into one function,
"			much less duplicate code this way, hehe.
"		Now saves the cursor position (I believe) properly.
"		TODO could be better by handeling ranges internal.
"	[Feral:249/02@00:41] v1.4:
"	Bugfix, a comment char of + failed.
"	Modification: commands always place cursor on col 1 this is for
"	consistancy... comment left on col1, uncomment on first word.
"		I would prefer the cursor's column be unchanged however a range
"		command starts with cursor on col1, thus the cursor col would have to
"		be saved in a map or something that calls the commands.
"
"	Sat, 20  Jul  2002 02:26:29 Pacific Daylight Time:	v1.3
"	Massive changes. Proper handling of ranges (not that it didn't work before
"		mind you)
"	Made ToggleCommentify, UnCommentify and Commentify commands (:TC, :UC and
"		:CC respectivly) so you can choose what to do to a line, or line
"		range. (I like to comment off entire sections of code, sometimes those
"		sections contain line comments.. toggleing in this instance has
"		undesired results.) Now I can simply highlight and :CC
"
"	Another (possibly) nice addition is the ability to specify the comment
"		symbol on after the command, I.e. :CC // which would add // chars to
"		the start of the range of lines.
"
"	The range defaults to the currnet line, as per VIM documentation.
"
" }}}
"
" Customizing: (how to customize) {{{
"
"	Each option var can be specified as buffer or global, with that
"	precedence.
"
"	I.e. in .vimrc:
"	let g:FTCOperateOnFirstColumnOnly = 0
"	let g:FTCOperateOnlyOnNonComments = 1
"
"	Option: g:FTCOperateOnFirstColumnOnly
"		If this is 0 (false) then the comment char does not have to be at col1
"		to be removed, in addition comment chars are placed just before the
"		first word of the line.
"		Valid Values: 0 or 1
"		Default: 1
"
"	Option: g:FTCOperateOnlyOnNonComments
"		If 1 (true) a line will not be commented if it already starts with
"		comment chars. With this 0 (false) you can (easily) comment entire
"		functions without fear of messing up commenting (i.e. '<,'>CC ...
"		'<,'>UC ...  I.e. comment then uncomment. If this is true the above
"		MAY not work (depending on how the existing comments are.)
"		Valid Values: 0 or 1
"		Default: 0
"
"	Option: g:FTCOperateOnBlankLines
"		If 1 (true) blank lines will be processed, else they are skipped.
"		Valid Values: 0 or 1
"		Default: 1
"
"	Option: g:FTCBangEffectsWhat
"		Will toggle an option if command is banged: (i.e. :CC!)
"			1. OperateOnFirstColumnOnly
"			2. OperateOnlyOnNonComments
"			3. OperateOnBlankLines
"		Valid Values: 1, 2 or 3
"		Default: 1
"
"
" }}}
"
" Note:		I am happy to add new file types or any other suggestions just
"			email me and I'll see what I can do.
"
" Examples:
"	:CC--
"	:CC --
"	" spaces are ate between the command and the string, if you want a space
"	use \ , i.e.
"	:CC\ --
"
" }}}


if exists("loaded_feraltogglecommentify")
	finish
endif
let loaded_feraltogglecommentify = 1


" Look to the vim entry here to add your own file types.
function s:FindCommentify() " {{{
	" finding out the file-type, and specifying the comment symbol
	let fileType = &ft

	if fileType == 'ox' || fileType == 'cpp' || fileType == 'php' || fileType == 'java'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	" [Feral:189/03@20:40] bnk (custom format)
	elseif fileType == 'bnk'
		let commentSymbol_L = 'X-'
		let commentSymbol_R = ''
	"[Feral:155/03@06:40] C#
	elseif fileType == 'cs'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	"[Feral:201/02@01:17] ftf is my hypertext markup format. (which is to say
	"	txt with a few special chars)
	elseif fileType == 'ftf'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	"[Feral:283/02@04:14] torque-script: See: http://www.garagegames.com/
	elseif fileType == 'torquescript'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	"[Feral:303/02@19:20] fte is a template expansion system, See:
	"	http://vim.sourceforge.net/scripts/script.php?script_id=648
	elseif fileType == 'fte'
		let commentSymbol_L = 'FTE:'
		let commentSymbol_R = ''
	"[Feral:308/02@02:02] html -- first start/end comment
	"12-Nov-2002: Bernhard Wagner's xml handling
	elseif fileType == 'html' || fileType == 'xml'
		let commentSymbol_L = '<!-- '
		let commentSymbol_R = ' -->'
	elseif fileType == 'c' || fileType == "css"
		let commentSymbol_L = '/*'
		let commentSymbol_R = '*/'
	" The rest...
	elseif fileType == 'pov'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	elseif fileType == 'vim'
		let commentSymbol_L = '"'
		let commentSymbol_R = ''
	elseif fileType == 'lisp' || fileType == 'scheme' || fileType == 'dosini'
		let commentSymbol_L = ';'
		let commentSymbol_R = ''
	elseif fileType == 'tex'
		let commentSymbol_L = '%'
		let commentSymbol_R = ''
	elseif fileType == 'caos'
		let commentSymbol_L = '*'
		let commentSymbol_R = ''
	elseif fileType == 'm4' || fileType == 'config' || fileType == 'automake'
		let commentSymbol_L = 'dnl '
		let commentSymbol_R = ''
	elseif fileType == 'python' || fileType == 'perl' || fileType == 'make' || fileType =~ '[^w]sh$' || fileType == 'tcl' || fileType == 'jproperties'
		let commentSymbol_L = '#'
		let commentSymbol_R = ''
	elseif fileType == 'vb' || fileType == 'aspvbs'
		let commentSymbol_L == "'"
		let commentSymbol_R = ''
	elseif fileType == 'plsql' || fileType == 'lua'
		let commentSymbol_L = '--'
		let commentSymbol_R = ''
	else
		echohl WarningMsg
		echo "feraltogglecommentify.vim has not (yet) implemented for this file-type"
		echo "Your best bet is to specify the comment chars manually, i.e. :CC --"
		echo "Please feel free to email me with the comment char(s) and file type."
		echohl None
		let commentSymbol_L = ''
		let commentSymbol_R = ''
	endif

	" this function is ment to be executed as a way of returning two vars; see
	" :h return
	return "let CommentSymbol_L = '" . commentSymbol_L . "' | let CommentSymbol_R = '" . commentSymbol_R ."'"

endfunction " }}}

function s:DoCommentify(DaMode, DaBang, ...) " {{{

	" Initialize: CommentSymbol_L, CommentSymbol_R
	" {{{
	if(a:0 == 0)
		execute s:FindCommentify()
	elseif a:0 == 2
		let CommentSymbol_L = a:1
		let CommentSymbol_R = a:2
	else
		let CommentSymbol_L = a:1
		let CommentSymbol_R = ""
	endif
	" }}}

	" [Feral:201/02@01:46] GATE: nothing to do if we have no comment symbol.
	" CommentSymbol_R is allowed to be blank so we only check CommentSymbol_L
	if strlen(CommentSymbol_L) == 0
		return
	endif


	" Initialize: OperateOnFirstColumnOnly
	" {{{
	" If this is false (0) then the comment char does not have to be at col1
	" to be removed. Added comment chars are placed just before the first word
	" of the line.
	if exists('b:FTCOperateOnFirstColumnOnly')
		let OperateOnFirstColumnOnly = b:FTCOperateOnFirstColumnOnly
	elseif exists('g:FTCOperateOnFirstColumnOnly')
		let OperateOnFirstColumnOnly = g:FTCOperateOnFirstColumnOnly
	else
		let OperateOnFirstColumnOnly = 1
		"let OperateOnFirstColumnOnly = 0
	endif
	" }}}

	" Initialize: OperateOnlyOnNonComments
	" {{{
	" If true a line will not be commented if it already starts with comment
	" chars. With this false you can (easily) comment entire functions without
	" fear of messing up commenting (i.e. '<,'>CC ... '<,'>UC ... I.e. comment
	" then uncomment. If this is true the above MAY not work (depending on how
	" the existing comments are.)
	if exists('b:FTCOperateOnlyOnNonComments')
		let OperateOnlyOnNonComments = b:FTCOperateOnlyOnNonComments
	elseif exists('g:FTCOperateOnlyOnNonComments')
		let OperateOnlyOnNonComments = g:FTCOperateOnlyOnNonComments
	else
		"let OperateOnlyOnNonComments = 1
		let OperateOnlyOnNonComments = 0
	endif
	" }}}

	" Initialize: OperateOnBlankLines
	" {{{
	" If true blank lines will be processed, else they are skipped.
	if exists('b:FTCOperateOnBlankLines')
		let OperateOnBlankLines = b:FTCOperateOnBlankLines
	elseif exists('g:FTCOperateOnBlankLines')
		let OperateOnBlankLines = g:FTCOperateOnBlankLines
	else
		let OperateOnBlankLines = 1
		"let OperateOnBlankLines = 0
	endif
	" }}}

	" Initialize: BangEffectsWhat
	" {{{
	" Will toggle an option if command is banded:
	"	1. OperateOnFirstColumnOnly
	"	2. OperateOnlyOnNonComments
	"	3. OperateOnBlankLines
	if exists('b:FTCBangEffectsWhat')
		let BangEffectsWhat = b:FTCBangEffectsWhat
	elseif exists('g:FTCBangEffectsWhat')
		let BangEffectsWhat = g:FTCBangEffectsWhat
	else
		let BangEffectsWhat = 1
	endif
	" }}}

	" [Feral:196/03@15:57] Bang will toggle options.
	if a:DaBang == "!"

		if BangEffectsWhat == 1
			if OperateOnFirstColumnOnly == 0
				let OperateOnFirstColumnOnly = 1
			else
				let OperateOnFirstColumnOnly = 0
			endif
		elseif BangEffectsWhat == 2
			if OperateOnlyOnNonComments == 0
				let OperateOnlyOnNonComments = 1
			else
				let OperateOnlyOnNonComments = 0
			endif
		elseif BangEffectsWhat == 3
			if OperateOnBlankLines == 0
				let OperateOnBlankLines = 1
			else
				let OperateOnBlankLines = 0
			endif
		else
			echohl ErrorMsg
			echo "feraltogglecommentify.vim ERROR:"
			echo "FTCBangEffectsWhat set to an invalid value; please fix."
			echo "Quick Help:"
			echo "let g:FTCBangEffectsWhat = 1. for OperateOnFirstColumnOnly"
			echo "let g:FTCBangEffectsWhat = 2. for OperateOnlyOnNonComments"
			echo "let g:FTCBangEffectsWhat = 3. for OperateOnBlankLines"
			echo "Aborting."
			echohl None
			return
		endif

	endif


	" Save where we are
	let SavedMark = line('.').'G'.b:FTCSaveCol.'|'
	normal! H
"	let SavedMark = 'normal! '.line('.').'Gzt'.SavedMark
	let SavedMark = line('.').'Gzt'.SavedMark
	if has('folding')
		let SavedMark = 'zN'.SavedMark
	endif
	let SavedMark = 'normal! '.SavedMark
	execute SavedMark


	" [Feral:201/02@03:43] folded lines must be opend because a substitute
	" operation on a fold effects all lines of the fold, so (temp) turn off
	" folding.
	if has('folding')
		normal! zn
	endif


	let lineString = getline(".")

	" [Feral:196/03@14:31] I could place the if OperateOnFirstColumnOnly here
	" just as easily; then we would bail on lines that just contained
	" whitespace as well as empty lines.

	if OperateOnBlankLines == 0
		if lineString == $
			" the line is blank, do nothing.
			" Return to where we were
			execute SavedMark
			return
		endif
	endif



	if OperateOnFirstColumnOnly == 0
		" JH: remove leading white space
		let lineString = substitute(lineString,'^\s*','','e')
	endif

	" FERAL: extract the first x chars of the line, where x is the
	" width/length of the comment symbol and see if they match our Left
	" comment symbol.
	if strpart(lineString,0,strlen(CommentSymbol_L) ) == CommentSymbol_L
		let isCommented = 0
	else
		let isCommented = 1
	endif


	" 0 = toggle
	" 1 = comment
	" 2 = uncomment.
"	if a:DaMode == 1
	let ModeOfOperation = a:DaMode
	if ModeOfOperation == 0
		" Toggle:
		if isCommented == 0
			" already commented, so uncomment.
			let ModeOfOperation = 2
		else
			" not already commented, so comment.
			let ModeOfOperation = 1
		endif
	endif

	let CommentSymbol_L = escape(CommentSymbol_L, '/\\*')
	let CommentSymbol_R = escape(CommentSymbol_R, '/\\*')
	let MessWith_HLS = 0
	if &hlsearch
		let MessWith_HLS = 1
		set nohlsearch
	endif
	if ModeOfOperation == 2
		" Uncomment: -- remove the comment markers.
		" [Feral:193/03@09:07] But don't scream if the comment char isn't
		"	there.
		if OperateOnFirstColumnOnly == 1
			silent execute ':sm/^'.CommentSymbol_L.'//e'
		else
			" JH: comment symbol is not always on the first column
			silent execute ':sm/^\(\s*\)'.CommentSymbol_L.'/\1/e'
		endif

		if strlen(CommentSymbol_R)
			silent execute ':sm/'.CommentSymbol_R.'$//e'
		endif
	else
"		" else ModeOfOperation == 1
"		" Comment: -- add the comment markers.
		" JH: only add comment when not already commented
		let DoIt = 0
		if OperateOnlyOnNonComments == 0
			let DoIt = 1
		else
			if isCommented == 1
				let DoIt = 1
			endif
		endif

		if DoIt == 1
			if OperateOnFirstColumnOnly == 1
				silent execute ':sm/^/'.CommentSymbol_L.'/'
			else
				silent execute ':sm/^\(\s*\)/\1'.CommentSymbol_L.'/'
			endif

			if strlen(CommentSymbol_R)
				silent execute ':sm/$/'.CommentSymbol_R.'/'
			endif
		endif
	endif
	if MessWith_HLS
		let MessWith_HLS = 0
		set hlsearch
	endif


	" Return to where we were
	execute SavedMark

endfunction
" }}}

function s:DLAC(...) range " {{{

	if(a:0 == 0)
		execute s:FindCommentify()
	elseif a:0 == 2
		let CommentSymbol_L = a:1
		let CommentSymbol_R = a:2
	else
		let CommentSymbol_L = a:1
		let CommentSymbol_R = ""
	endif

	" [Feral:201/02@01:46] GATE: nothing to do if we have no comment symbol.
	" [Feral:308/02@02:04] CommentSymbol_R is allowed to be blank so we only
	"	check CommentSymbol_L
	if strlen(CommentSymbol_L) == 0
		return
	endif


	" Save where we are
	let SavedMark = line('.') . 'G'.b:FTCSaveCol.'|'
	normal! H
	let SavedMark = 'normal! '.line('.').'Gzt'.SavedMark
	if has('folding')
		let SavedMark = SavedMark.'zN'
	endif
	execute SavedMark

	" [Feral:201/02@03:43] folded lines must be opend because a substitute
	" operation on a fold effects all lines of the fold.
	" temp turn off folding.
	if has('folding')
		normal! zn
	endif



	let CommentSymbol_L = escape(CommentSymbol_L, '/\\*')
	let CommentSymbol_R = escape(CommentSymbol_R, '/\\*')
	let MessWith_HLS = 0
	if &hlsearch
		let MessWith_HLS = 1
		set nohlsearch
	endif


	let SR = a:firstline.",".a:lastline

	" Set report option to a huge value to prevent informations messages
	" while deleting the lines
	let old_report = &report
	set report=99999

	let Was_Reg_w = @w
	":[range]y[ank] [x]	Yank [range] lines [into register x].
	execute ":".SR."yank w"

	" Restore the report option
	let &report = old_report

	put! w
	let @w=Was_Reg_w



	" Set report option to a huge value to prevent informative messages while
	" deleting the lines
	let old_report = &report
	set report=99999

	" Comment -- add the comment markers.
	silent execute SR.':sm/^/'.CommentSymbol_L.'/'
	if strlen(CommentSymbol_R)
		silent execute SR.':sm/$/'.CommentSymbol_R.'/'
	endif


	" Restore the report option
	let &report = old_report

	if MessWith_HLS
		let MessWith_HLS = 0
		set hlsearch
	endif

	" Return to where we were
	execute SavedMark
	" but goto to the new duplicate lines
	execute ":".(a:lastline+1)

endfunction
" }}}

"*****************************************************************
" Commands: {{{
"*****************************************************************
"Holding: {{{
":command -nargs=? -range TC :<line1>,<line2>call <SID>ToggleCommentify(<f-args>)
":command -nargs=? -range CC :<line1>,<line2>call <SID>Commentify(<f-args>)
":command -nargs=? -range UC :<line1>,<line2>call <SID>UnCommentify(<f-args>)
"}}}
if !exists(":TC")
	:command -nargs=? -range -bang TC		:let b:FTCSaveCol = virtcol('.')|<line1>,<line2>call <SID>DoCommentify(0, <q-bang>, <f-args>)
endif
if !exists(":CC")
	:command -nargs=? -range -bang CC		:let b:FTCSaveCol = virtcol('.')|<line1>,<line2>call <SID>DoCommentify(1, <q-bang>, <f-args>)
endif
if !exists(":UC")
	:command -nargs=? -range -bang UC		:let b:FTCSaveCol = virtcol('.')|<line1>,<line2>call <SID>DoCommentify(2, <q-bang>, <f-args>)
endif

"if !hasmapto('<Plug>FtcTc') && mapcheck("<M-c>", "nvi") == ""
if !hasmapto('<Plug>FtcTc')
	nmap <unique>	<M-c>	<Plug>FtcTc
	vmap <unique>	<M-c>	<Plug>FtcTc
	imap <unique>	<M-c>	<esc><Plug>FtcTc
endif
noremap <unique> <script> <Plug>FtcTc  :TC<CR>j



" {{{ HOLDING: Old dlac mapping
""[Feral:317/02@05:40] This is basicaly a hack; hopefully I'll COMBAK to this
""	someday and clean it up. (there is no reason for a <plug> to rely on the
""	:commands the script defines for example)
"" DLAC -- duplicate line(s) and comment.
"" Mangles mark z
"if exists(":CC") && exists(":UC")
"	if !hasmapto('<Plug>FtcDlacNormal') && mapcheck("<C-c>", "n") == ""
"		" Normal Same keys as Multi-Edit, fwiw.
"		"[Feral:314/02@19:28] Save shift is not recognised; these come out as
"		"	<C-c>, dern!
"		nmap <unique>	<C-c>	<plug>FtcDlacNormal
"	endif
"	if !hasmapto('<Plug>FtcDlacVisual') && mapcheck("<C-c>", "v") == ""
"		" visual maping to handle multiple lines...
"		vmap <unique>	<C-c>	<plug>FtcDlacVisual
"	endif
"
"	noremap		<unique> <script> <Plug>FtcDlacNormal	mzyyp`z:CC<CR>j
"	vnoremap	<unique> <script> <Plug>FtcDlacVisual	mz:CC<cr>gvyPgv:UC<CR>`z
"endif
" }}}
:command -nargs=? -range DLAC		:let b:FTCSaveCol = virtcol('.')|<line1>,<line2>call <SID>DLAC(<f-args>)
if !hasmapto('<Plug>FtcDLAC')
	nmap <unique>	<C-c>	<Plug>FtcDLAC
	vmap <unique>	<C-c>	<Plug>FtcDLAC
" [Feral:194/03@07:08] Collision, See: i_CTRL-C
"	imap <unique>	<C-c>	<esc><Plug>FtcDLAC
endif
noremap <unique> <script> <Plug>FtcDLAC  :DLAC<cr>



" }}}

"End of file
