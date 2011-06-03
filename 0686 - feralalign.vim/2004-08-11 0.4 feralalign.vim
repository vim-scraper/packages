" {{{ File header
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
" \file		feralalign.vim
" \date		Tue, 10 Aug 2004 22:54 PST
"
" \brief	Allows you to easily tab a section of spaces to a desired column;
"			I.e. if you want all your comments to line up on virt col 41 try
"			":FeralAlign 41" when your cursor is in a position to "ciw"; I.e.
"			saves you from "ciw<tab><tab><tab>"etc. But that is about all this
"			does (=
"
"			Works with single lines or ranges. Ranges must be specified by a
"			single column, block-wise visual selection; (i.e. "<ctrl-v>4j").
"			So to align 5 lines, place your cursor in a good spot (where a
"			"ciw" won’t keel something important; i.e. spaces before the
"			comment chars) and "<ctrl-v>4j:FeralAlign 41"; easy as that.
"
"			As of 0.4 you can bang :FeralAlign to use the space (\s*) between
"			; and first non space (\S) chars, i.e. C++ style end of line
"			comments.
"			:FeralAlign2 is a shortcut for basicaly :FeralAlign! virtcol(".")
"
" \note		This is VIMSCRIPT #686
"			URL: http://vim.sourceforge.net/scripts/script.php?script_id=686
"
"	Author:	Robert KellyIV <sreny@rpyrpgvpjvmneq.pbz> (Rot13ed)
"	License:	Donated to the Public Domain Sun, 15 Jun 2003 22:44:50 PDT
"	Version:	0.4
"	History: {{{
"	[Feral:223/04@22:41] 0.4
"	Change:
"		:FeralAlign now can take a bang, bang specifis replaces space (\s) between ; and first non space (\S), i.e. good for aligning C++ style end of line comments;
"	New:
"		:FeralAlign2 is an alies for basicaly :FeralAlign! virtcol(".")
"	Removed: (remed really)
"		mappings for F6, F7 the :FeralAlignMarkedCol, and b:feralalign_MarkedVirtCol
"
"	[Feral:205/03@17:53] 0.31
"	Change:
"		Will now ignore blank lines. (for a little easier usage, in my
"			experience)
"	[Feral:166/03@22:40] 0.3
"		Rewrote range handling; Now works as advertised (":FeralAlign 41" with
"			cursor on a blank(i.e. \s) that is safe to "ciw" on; Single line
"			OR range; But please use a single column, visual block to specify
"			a range due to weirdness. (In general it was the only way I found
"			to reliably get the cursor virtcol position).
"	Limitations:
"		This is a `only what I need` type of script; Ultimately it should be
"			able to deal with spaces as well as tabs (some people just can't
"			stand tabs (I was one of them) and it would be nice to accommodate
"			them too!)
"
"			[Feral:167/03@19:19] Correction: <tab> will insert shiftwidth
"				space characters when 'expandtab' is set. So the only real
"				limitation is that we are bound to tabstops currently. (i.e.
"				cannot get 2 spaces past a tabstop
"
"	[Feral:164/03@21:48] 0.2
"		Expanded to include a range; but GRRR not as I wanted; seems there is
"			no way at all to get the cursor's column position with a -range
"			user command. So, having to pass in the start column manually as a
"			second parameter. Stinkie. (as opposed to requireing the cursor be
"			in a position so we can ciw for each line)
"
"		:FeralAlign target start
"		or (etc.)
"		:'<,'>FeralAlign target start
"
"		Meaning target is the column you want to stop <tab>ing at, and start
"			is the column where it is safe to ciw.
"	[Feral:164/03@17:33] 0.1
"		Initial version.
"		Place cursor where you would ciw and :FeralAlign <target virtcol>
" }}}
" }}}

if exists("loaded_feralalign")
	finish
endif
let loaded_feralalign = 1

let s:save_cpo = &cpo
set cpo&vim


"*****************************************************************
" Banged version replaces space between ; and \S, i.e. ';\s*\S'
" In effect this is something like: :s/\(;\)\(\s*\)\(\S\)/\1\t\t\t\t\2
function s:Feral_Align(Bang, TargetCol) " {{{

	" We use s:CursorVirtCol as a gate; if it is -1 (manually set on error;
	"	below) then abort the rest of the calls; (range); it will be reset
	"	next time we are called by the user as part of the :FeralAlign
	"	command.
	if s:CursorVirtCol == -1
		return
	endif


	" As visualmode() is set even after the visual selection is no longer used
	"	(or at least when we are not called as a range) only check for it when
	"	our a:firstline and a:lastline differ.
	if a:firstline != a:lastline
		let DaVisualMode = visualmode()
		if DaVisualMode == 'v' || DaVisualMode == 'V'
			echohl ErrorMsg
			echo "Feral_Align ERROR:"
			echo "Due to wierdness please use a single column visual block (<ctrl-v>)"
			echohl WarningMsg
			echo "You can probably convert the existing visual with: gv<ctrl-v>"
			echohl None
			let s:CursorVirtCol = -1
			return
		endif
	endif
	" s:CursorVirtCol is where the cursor was; presumably.
"	echo confirm("s:CursorVirtCol: ".s:CursorVirtCol)


	" [Feral:205/03@17:51](v0.31) Lets ignore blank lines...
	if getline(".") == $
		return
	endif



	if a:Bang == "!"
		let StartCol = match(getline("."), ";\\s*\\S")
		if StartCol <= -1
			return
		endif
		call cursor(0, StartCol+2)
		unlet StartCol
	else
		" this is needed because when called as a range our cursor pos is 1
		execute 'normal! '.s:CursorVirtCol.'|'
	endif


" ciw and tab untill we reach TargetCol

	" remove the spaces (ciw)
	:exe "norm! \"_ciw\<tab>"

	" tab untill we reach targetcol
	while virtcol('.') < (a:TargetCol-1)
		:exe "norm! a\<tab>"
	endwhile

	" Return to where we were; mostly for when we are not called as a range.
	execute 'normal! '.s:CursorVirtCol.'|'

endfunction " }}}




"*****************************************************************
" Commands: {{{
"*****************************************************************
:command -nargs=1	-range	-bang	FeralAlign		:let s:CursorVirtCol = virtcol('.')|:<line1>,<line2>call <SID>Feral_Align("<bang>", <f-args>)

":command -nargs=0			FeralAlignCurColMark	:let b:feralalign_MarkedVirtCol = virtcol('.')
":command 			-range	FeralAlignMarkedCol		:let s:CursorVirtCol = virtcol('.')|:<line1>,<line2>call <SID>Feral_Align("", b:feralalign_MarkedVirtCol)

:command			-range			FeralAlign2		:let s:CursorVirtCol = virtcol('.')|:<line1>,<line2>call <SID>Feral_Align("!", s:CursorVirtCol)

"}}}

"*****************************************************************
" Mappings: {{{
"*****************************************************************

"nnoremap	<F6>	ciw<tab><esc>l:FeralAlignCurColMark<cr>:echo "feralalign.vim: Current Col Marked"<cr>
"vnoremap	<F7>	:FeralAlignCurCol<cr>
"nnoremap	<F6>	ciw<tab><esc>l:let b:feralalign_MarkedVirtCol = virtcol('.')<cr>:echo "feralalign.vim: Current Col Marked"<cr>

"nnoremap	<F6>	ciw<tab><esc>l:let b:feralalign_MarkedVirtCol = virtcol('.')<cr>:echo "feralalign.vim: Current Col Marked:".b:feralalign_MarkedVirtCol<cr>
"vnoremap	<F7>	:FeralAlignMarkedCol<cr>

nnoremap	<leader>fa	:FeralAlign2<cr>
vnoremap	<leader>fa	:FeralAlign2<cr>

"}}}

let &cpo = s:save_cpo
"EOF
