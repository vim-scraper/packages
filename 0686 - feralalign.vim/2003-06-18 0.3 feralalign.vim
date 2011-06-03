" {{{ File header
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
" \file		feralalign.vim
" \date		Wed, 18 Jun 2003 18:00 PDT
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
" \note		This is (probably) VIMSCRIPT #457
"			URL: «COMBAK»
"
" \author	Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
" License:	Donated to the Public Domain Sun, 15 Jun 2003 22:44:50 PDT
" \version	$Id$
" Version:	0.3
" History: {{{
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
function s:Feral_Align(TargetCol) " {{{

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
			"		let s:AbortAll = 1
			let s:CursorVirtCol = -1
			return
		endif
	endif
	" s:CursorVirtCol is where the cursor was; presumably.
"	echo confirm("s:CursorVirtCol: ".s:CursorVirtCol)


" ciw and tab untill we reach TargetCol


	" this is needed because when called as a range our cursor pos is 1
	execute 'normal! '.s:CursorVirtCol.'|'

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
if !exists(":FeralAlign")
	" This works because only the <line1>,<line2>call is called for each line
	"	in range;
	:command -nargs=1 -range FeralAlign		:let s:CursorVirtCol = virtcol('.')|:<line1>,<line2>call <SID>Feral_Align(<f-args>)
endif


"}}}

let &cpo = s:save_cpo
"
"EOF
