" {{{ File header
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
"	\file		markjump.vim
"
"	\brief		Code transversal functions, move to prev/next marker and
"				insert marker. Marker chars are configurable.
"	\note		Inspiration from Luc Hermitte's bracketing.base.vim (a
"				modified version of Stephen Riehm's braketing macros for vim)
"
"	Maintainer:
"	\author		Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
"	\note		MANY usefull comments, codesniplets and general insight from
"				Luc Hermitte!
"
"	Last Change:
"	\date		Thu, 14 Nov 2002 23:56 Pacific Standard Time
"	\version	$Id: markjump.vim,v 1.9 2002/11/13 06:56:48 root Exp $
"	Version:	0.29
"	History: {{{
"	[Feral:311/02@13:43] 0.29
"		Properlly inserts marker chars litterally in insertmode. (proper
"		implimentation of 0.28)
"	[Feral:311/02@03:09] 0.28
"		Now inserting (as a hack) the marker chars litterally in insertmode;
"		this may cure off by one troubles when marking visual selections when
"		the marker chars cause reindentation.
"	[Feral:310/02@16:38] 0.27
"		Added <m-m> mapping for insert mode short cut to <c-o>:MI
"	[Feral:302/02@15:37] 0.26
"	This is Luc's fault :)
"		* fixed menu entries and only add them if gui is running and has menu.
"		* Modified MarkerInsert to use q-args, easier that.
"		* Modified MarkerInsert to insert the marker by default and in general
"			fixed cursor positioning; can append as an option.
"		* Added :MA for marker append which is like :MI except it appends.
"	[Feral:301/02@18:38] 0.25
"		* Added MarkerStrip and corresponding command, :MS
"	[Feral:301/02@12:52] 0.24
"		* Bugfix, properly handles Left and right marker chars greater than
"			one char wide. tested with (", ") and %%%(, ) ... Should work.
"		* no longer automatically zz when jumping to a mark. Probably should
"			be an option.
"		* Refined MarkerInsert
"	[Feral:300/02@13:11] 0.23
"		* New name, now called markjump.vim
"		* Proper (I believe) plugin, place in ~/.vim/plugin or the like.
"	Still Needs:
"		- documtation, i.e. markjump.txt for ~/.vim/doc
"		- syntax fragment file (so you can include in syntax file and get
"			coloring for the default marks.. least I don't think I can do
"			coloring based on vars.
"	[Feral:299/02@22:23] 0.22
"		* Small, happy changes.
"		* Did away with C_Marker and used Luc's idea of strlen(R_Marker), good
"			thinking Luc!
"		* Added ... param to MarkerInsert, text placed here will be inserted
"			between the marker chars. Luc's idea!
"	Still Needs:
"		proper plugin. -- I'll get to this honest! (=
"	[Feral:299/02@13:12] 0.21:
"		visual marker insert works, either via :MI or ¡mark! with a visual
"		selection.
"	Still Needs:
"		proper plugin.
"	[Feral:299/02@11:18] 0.2:
"		Redesigned and inspired by Luc Hermitte's braketing.base.vim
"	Still Needs:
"		visual marker insert.
"		proper plugin.
"	[Feral:249/02@01:43] 0.1:
"		Broke off from my version of DrChip's C stubs.
" }}}
"
" }}}

if exists("loaded_markjump")
	finish
endif
let loaded_markjump = 1

let s:save_cpo = &cpo
set cpo&vim

" GetL_Marker() and GetR_Marker() general idea inspired by Luc Hermitte's
"	bracketing.base.vim (a modified version of Stephen Riehm's braketing
"	macros for vim) Also usefull coments from Luc
" Note: b:L_Marker is used before g:L_Marker and if neither are defined then
"	the default is used, same for b:R_Marker before g:R_Marker
function s:GetL_Marker() " {{{
	if exists('b:L_Marker')
		return b:L_Marker
	elseif exists('g:L_Marker')
		return g:L_Marker
	else
		" <c-k><<
		return '«'
	endif
endfunction
" }}}
function s:GetR_Marker() " {{{
	if exists('b:R_Marker')
		return b:R_Marker
	elseif exists('g:R_Marker')
		return g:R_Marker
	else
		" <c-k>>>
		return '»'
	endif
endfunction
" }}}

" Info: Finds a marker, searches forward(1) or reverse(2) depending on the
"	value of flags.
function s:MarkerFind(flags) " {{{


" TODO COMBAK to this.
"	"{{{ Luc's sniplet
""Regarding, your code, I think you can change the "normal ".n_times."h"
""by: searchpair(open, '', close.'\zs', 'b') which works better.
""In my correction of this part, I use:
"
"	let position = line('.') . "normal! ".virtcol('.').'|'
"	if direction == 'b'
"		" then: go to the start of the marker.
"		" Principle: {{{
"		" 1- search backward the pair {open, close}
"		"    In order to find the current pair correctly, we must consider the
"		"    beginning of the match (\zs) to be just before the last character of
"		"    the second pair.
"		" 2- Then, in order to be sure we don't jump to a match of the open
"		"    marker, we search forward for its closing counter-part.
"		"    Test: with open='«', close = 'ééé', and the text:{{{
"		"       blah «»
"		"       «1ééé  «2ééé
"		"       «3ééé foo
"		"       «4ééé
"		"    with the cursor on any character. }}}
"		"    Without this second test, the cursor would have been moved to the end
"		"    of "blah «" which is not the beginning of a marker.
"		" }}}
"		if searchpair(Marker_Open(), '', substitute(Marker_Close(), '.$', '\\zs\0', ''), 'b')
"			if ! searchpair(Marker_Open(), '', Marker_Close(), 'n')
"				" restore cursor position as we are not within a marker.
"				exe position
"			endif
"		endif
"	endif
"	"}}}



	" a:flags = 1	Next (goes forward)
	" a:flags = 2	Prev (goes backwards)
	let L_Marker = s:GetL_Marker()
	let R_Marker = s:GetR_Marker()
	let SearchFlags = ""
	" position the cursor so searchpair and search find what we want. (ignore
	"	the marker set we are on, if any).
	if virtcol('.') > 1
		execute "normal! ".strlen(R_Marker)."h"
	endif
	if a:flags == 2
		let SearchFlags = "b" . SearchFlags
		call searchpair(L_Marker, "", R_Marker, "b")
	endif

	if search(L_Marker.'.\{-}'.R_Marker, SearchFlags) > 0
"		" center line in screen
"		execute "normal! zz"

		" start char-visual (yea I know duh)
		execute "normal! v"

		" Move to the end of the R_Marker
		call searchpair(L_Marker, "", R_Marker, "")
		execute "normal! ".strlen(R_Marker)."l"

		" turn on select mode.
		execute "normal! \<c-g>"

		echo "Found marker on line " . line(".") . ", column " . virtcol(".") . "."
	endif
endfunc
" }}}

" Info: Inserts a marker pair, optionally around suplied(Elipsis) text or visual
"	(single line) selection.
function s:MarkerInsert(CurMode, InsertStyle, Elipsis) range " {{{
	" CurMode:
	" 1 = Normal
	" 2 = Insert
	" 3 = Visual
	" InsertStyle:
	" 0 = insert (i.e. i)
	" 1 = append (i.e. a)
	" Elipsis:
	" optional text that is inserted between the markers.


"	" [Feral:301/02@09:30] Just testing.. we do need a:column right?
"	if a:column != virtcol('.')
"		echo confirm("Just testing -- we do need a:column right?\na:column != virtcol('.')")
"	endif
	" [Feral:301/02@18:43] Actually it would seem we do not. Wonder what I was
	"	thinking...
	let column = virtcol('.')
"	echo confirm(column)


"	"[Feral:299/02@22:57] If there is an optial string(sentence) (in ...) grab
"	"	it and stuff it into StringToUse.
""	echo a:0
"	let Index = 1
"	let StringToUse = ''
"	while Index <= a:0
"		" Woo, thanks for the reminder Luc!
"		let StringToUse = StringToUse.a:{Index}
"		if Index != a:0
"			let StringToUse = StringToUse.' '
"		endif
"		let Index = Index + 1
"	endwhile
"	unlet Index
""	echo confirm('StringToUse is '.StringToUse)
	" Neat, thank you Luc!
	let StringToUse = a:Elipsis


	if a:firstline != a:lastline
		echo "ERROR: multi line range not suported"
		return "ERROR: multi line range not suported"
	endif

"	let LR = a:firstline.",".a:lastline
"	let mode = mode()
"	let VLnR = virtcol("'<").",".virtcol("'>")
"	let VLR = line("'<").",".line("'>")
"	echo confirm('Column:'.column.";\nLR:".LR.";\nMode:".mode.";\nVLnR".VLnR.";\nVLR".VLR.';')

"	"[Feral:299/02@22:20] This method for checking if we are in an active
"	"	(i.e. displayed) visual selection is not much more than a hack...
"	"	there HAS to be a better way!
	let CurMode = a:CurMode
""	echo confirm(CurMode)
"	if CurMode == 1
"		" Insert mode is the only mode in question.
"		if column == virtcol("'<")
"			" the virtual area starts on the same col we are at
"			if line("'<") == a:firstline
"				" the virtual area starts on the same line we start at
"				if line("'<") == line("'>")
"					" the virtual area is only one line
"					let CurMode = 3
"					" This fails when the cursor starts on `< ... I've no way
"					"	to tell if the visual mode is active; it never will be
"					"	in here. I need to know this to determin what to do
"					"	below.
"
""[Feral:302/02@22:05] this does NOT work because we are NOT in visual mode in
""here.... I guess mode() is accuret.,
""					let Was_Reg_f = @f
""					let @f=""
""					execute 'normal! gv"fy'
""					echo confirm(@f)
""					if @f != ""
""						echo confirm('Visual is:'.@f)
""						let CurMode = 3
""					endif
""					let @f = Was_Reg_f
"
"				else
"					echo "ERROR: multi line range not suported"
"					return "ERROR: multi line range not suported"
"				endif
"			endif
"		endif
"	endif

	" so far the only clue I have is if column == '< ....

"	echo confirm(CurMode)
	let L_Marker = s:GetL_Marker()
	let R_Marker = s:GetR_Marker()
	if CurMode == 1
		"normal mode
		if a:InsertStyle == 0
			let InsertStyle = 'i'
		else
			let InsertStyle = 'a'
		endif
		execute "normal! ".InsertStyle.L_Marker.StringToUse.R_Marker

		" if the StringToUse is not empty (aka passed params on cmd line),
		"	then goto the end of the marker as we have already filled it in,
		"	else place cursor ready for insert
		if StringToUse == ""
			execute "normal! \<ESC>".strlen(R_Marker)."h"
		else
			execute "normal! \<ESC>".strlen(R_Marker)."l"
		endif
	elseif CurMode == 2
		" insert mode
		" position the cursor inside the marker pair
"		return L_Marker.R_Marker."\<ESC>".strlen(R_Marker)."ha"
		" don't position the cursor.
		return L_Marker.R_Marker
	elseif CurMode == 3
		" visual area
"		echo confirm("Looks like a visual!")
"		execute "normal! `<i".L_Marker."\<ESC>`>a".R_Marker."\<ESC>"
"		execute "normal! `>i".R_Marker."\<ESC>`<i".L_Marker."\<ESC>"
		" [Feral:311/02@03:04] I wonder if automatic indention is what is
		"	causing the descrapancy(a off by one) between Luc and my running
		"	of this.
		" NOTE this is a hack; and really does not support multi char
		"	markers... how to fix?
"		execute "normal! `>i\<c-v>".R_Marker."\<ESC>`<i\<c-v>".L_Marker."\<ESC>"
		" [Feral:311/02@13:36] Using a substitue to place \<c-v> before each
		" char works quite well; Ran into problems with the \<c-v> notation
		" however (kept wanting to insert that text litterally as 6 chars);
		" Using literal <c-v>  (aka <c-v><c-v>) works as expected however.
		" Long form for when we try to get <> notation for <c-v> working again.
"		let Litteral_L_Marker = substitute(L_Marker, '\(.\)', '\1', "g")
"		let Litteral_R_Marker = substitute(R_Marker, '\(.\)', '\1', "g")
"		echo confirm(Litteral_L_Marker."\n".Litteral_R_Marker)
"		execute "normal! `>i".Litteral_R_Marker."\<ESC>`<i".Litteral_L_Marker."\<ESC>"
"		unlet Litteral_L_Marker
"		unlet Litteral_R_Marker
		" Short form:
		execute "normal! `>i".substitute(R_Marker, '\(.\)', '\1', "g")."\<ESC>`<i".substitute(L_Marker, '\(.\)', '\1', "g")."\<ESC>"
	else
		echo "ERROR: Unknown mode of operation, DEV ERROR!"
		return "ERROR: Unknown mode of operation, DEV ERROR!"
	endif
endfunc
" }}}


" Info: Strips markers from a marker (aka if you use the text between markers
" as a default answer (or something) and you want to easily remove the marker
" chars. This does that.
" Note:	Positioning of the cursor for this is a little odd, but as long as you
"	have your cursor in the marker you want to strip you'll have no problems.
function s:MarkerStrip() " {{{
	let L_Marker = s:GetL_Marker()
	let R_Marker = s:GetR_Marker()
"	" position the cursor to help searchpair find what we want it to.
"	if virtcol('.') > 1
"		execute "normal! ".strlen(R_Marker)."h"
"	endif

	" Move to the start of the marker
	if searchpair(L_Marker, "", R_Marker, "b") > 0
		" save where we are
		let LeftMarkerCol = virtcol('.')
		" move to the end of the marker pair
		if searchpair(L_Marker, "", R_Marker, "") > 0
			" x the R_Marker.
			execute "normal! ".strlen(R_Marker)."x"
			" move to start of the L_Marker and x it
			execute "normal! ".LeftMarkerCol."|".strlen(L_Marker)."x"
		endif
	endif

endfunc
" }}}


" =====================================================================
" Commands: {{{
" ---------------------------------------------------------------------
if !exists(":MN")
	:command -nargs=0		MN	call <SID>MarkerFind(1)
endif
if !exists(":MP")
	:command -nargs=0		MP	call <SID>MarkerFind(2)
endif
if !exists(":MI")
	command -nargs=*		MI	call <SID>MarkerInsert(1,0,<q-args>)
endif
if !exists(":MIV")
	command -nargs=* -range	MIV	call <SID>MarkerInsert(3,0,<q-args>)
endif
if !exists(":MA")
	command -nargs=*		MA	call <SID>MarkerInsert(1,1,<q-args>)
endif
if !exists(":MAV")
	command -nargs=* -range	MAV	call <SID>MarkerInsert(3,1,<q-args>)
endif
if !exists(":MS")
	command -nargs=0		MS	call <SID>MarkerStrip()
endif
" }}}

" =====================================================================
" Mappings: {{{
" ---------------------------------------------------------------------
if !hasmapto('<Plug>MjMark', 'i') && mapcheck("¡mark!", "i") == ""
	imap ¡mark!		<plug>MjMark
endif
if !hasmapto('<Plug>MjJump', 'i') && mapcheck("¡jump!", "i") == ""
	imap ¡jump!		<plug>MjJump
endif
if !hasmapto('<Plug>MjMark', 'v') && mapcheck("¡mark!", "v") == ""
	vmap ¡mark!		<plug>MjVMark
endif
inoremap <unique> <script> <plug>MjMark		<C-R>=<SID>MarkerInsert(2,0,"")<CR>
inoremap <unique> <script> <plug>MjJump		<ESC>:call <SID>MarkerFind(1)<CR>
vnoremap <unique> <script> <plug>MjVMark	<ESC>:call <SID>MarkerInsert(3,0,"")<CR>


"[Feral:300/02@12:44] I am REALLY not sure if this is the proper way to do
"	this.. the help only uses map an that just isn't what I want here.
if !hasmapto('<Plug>MjPrev', 'n') && mapcheck("<M-Insert>", "n") == ""
	nmap <M-Insert>		<plug>MjPrev
endif
if !hasmapto('<Plug>MjPrev', 'i') && mapcheck("<M-Insert>", "i") == ""
	imap <M-Insert>		<ESC><plug>MjPrev
endif
if !hasmapto('<Plug>MjPrev', 'v') && mapcheck("<M-Insert>", "v") == ""
	vmap <M-Insert>		<ESC><plug>MjPrev
endif
noremap <unique> <script> <plug>MjPrev		:call <SID>MarkerFind(2)<CR>


if !hasmapto('<Plug>MjNext', 'n') && mapcheck("<M-Del>", "n") == ""
	nmap <M-Del>		<plug>MjNext
endif
if !hasmapto('<Plug>MjNext', 'i') && mapcheck("<M-Del>", "i") == ""
	imap <M-Del>		<ESC><plug>MjNext
endif
if !hasmapto('<Plug>MjNext', 'v') && mapcheck("<M-Del>", "v") == ""
	vmap <M-Del>		<ESC><plug>MjNext
endif
noremap <unique> <script> <plug>MjNext		:call <SID>MarkerFind(1)<CR>



if exists(":MI") && !hasmapto('<Plug>MjInsMarker', 'i') && mapcheck("<M-m>", "i") == ""
	imap <M-m>		<plug>MjInsMarker
endif
inoremap <unique> <script> <plug>MjInsMarker	<C-o>:MI<space>


" }}}


" =====================================================================
" Menu Entries: {{{
" ---------------------------------------------------------------------
" Only add menu entries if the gui is going and has menu. Thank you Luc!
if has('gui_running') && has('menu')
	noremenu <script> Plugin.markjump.Prev\ Marker		:call <SID>MarkerFind(2)<CR>
	noremenu <script> Plugin.markjump.Next\ Marker		:call <SID>MarkerFind(1)<CR>
	noremenu <script> Plugin.markjump.Insert\ Marker	:call <SID>MarkerInsert(1,0,"")<CR>
	noremenu <script> Plugin.markjump.Append\ Marker	:call <SID>MarkerInsert(1,1,"")<CR>
	noremenu <script> Plugin.markjump.Strip\ Markers	:call <SID>MarkerStrip()<CR>
endif
" }}}




" TODO: Needs help	|write-local-help|

let &cpo = s:save_cpo

" ---------------------------------------------------------------------
"EOF
