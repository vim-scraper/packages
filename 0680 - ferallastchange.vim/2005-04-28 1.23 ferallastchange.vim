"{{{ File header information
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
"
"	\file		ferallastchange.vim
"	\date		Thu, 28 Apr 2005 14:03 PST
"
"	\brief		Update the last modification time of the file if an appropriate
"				string is found in the first 20 (default; see
"				flastchangeMaxLines) lines of the file. This is a
"				fork of Srinath Avadhanula's lastchange.vim.
"	\note		This is VIMSCRIPT #680
"				URL: <URL:http://vim.sourceforge.net/scripts/script.php?script_id=680>
"	\author		Robert KellyIV <sreny@srenyqernzf.pbz> (Rot13ed)
"	\author		Srinath Avadhanula <srinath@fastmail.fm> (original work:
"				lastchange.vim; dated Sat Mar 30 04:00 PM 2002 PST;
"				VIMSCRIPT #259;
"				URL: <URL:http://vim.sourceforge.net/script.php?script_id=259>
"	License:	I claim no copyright for MY changes;
"	Version:	1.23
"	History: {{{
"	[Feral:118/05@13:53] 1.23
"	Improved/Fixed cursor restore. Screen should not jump; ever.
"	[Feral:100/05@19:24] 1.22
"		I guess I'm stuck with undo; Good I guess.
"		Fiddled with cursor/screen position save/restore. *should* be better.
"			[Feral:118/05@12:54] Almost; It messes up by one line when second
"			to top line in window wraps around screen.
"	[Feral:048/05@10:49] 1.21
"		Fiddled with cursor positioning; now uses "call cursor()". 'better'
"
"	BUG FOUND: Found aparent bug with search(); it does not find string on line 1;
"		thus the string denoted by g:flastchangeleader must be on line 2 or
"		greater for now. This is the same as it has been as far as I know.
"
"	Added:	flastchangeMaxLines (default 20) to let you specify how many lines
"		maximum from the top of the file to consider searchable.
"
"		I.e.
"			let g:flastchangeMaxLines = 20
"		Is the same as the default; Which is the same as previous versions.
"
"		NOTE that a lower values does not improve speed nor efficiency, simply
"			if the found search string is to be considered or not.
"
"	[Feral:185/03@23:48] 1.2
"	Thanks to Asier for pointing out messing with undolevels is not at all
"		what I had intended.
"	BUG FIX:
"		no longer messes with undolevels.
"	Changes:
"		Changed method used to change the time stamp (now using setline() );
"		removed RemoveLastHistoryItem()
"	Limitation:
"		You have to undo twice after just :w now (first time to undo the
"			timestamp change, the second time to undo whatever it was you
"			wanted to undo.)
"	[Feral:166/03@12:31] 1.1
"		Initial fork of lastchange.vim VIMSCRIPT#259:
"		URL: http://vim.sourceforge.net/script.php?script_id=259
"
"		This is now a almost proper vim60 plugin:
"			File name is too long; Apologies I can not stand 8.3 file names.
"	Changes: From lastchange.vim v1.0
"		Time stamp can be defined by setting g:flastchangetimeformat to the
"			desired string for strftime().
"
"			Example: from my .vimrc
"	" [Feral:166/03@13:11] How I like ferallastchange.vim setup
"	let g:flastchangeleader = '\\date\t\t'
"	" Condense `Pacific Daylight Time` into `PDT` and tack that onto the end
"	"	of our time stamp format string
"	let g:flastchangetimeformat = '%a, %d %b %Y %H:%M '.substitute(strftime('%Z'), '\<\(\w\)\(\w*\)\>\(\W\|$\)', '\1', 'g')
"
"			NOTE how format %Z is special cased and replaced with a shortened
"				string. I.e. `Pacific Daylight Time` becomes `PDT`. This is
"				also done in the default time format string.
"			NOTE (as an aside):
"				iabbr _RFC822 <C-R>=strftime('%a, %d  %b  %Y %H:%M:%S %z')<CR>
"
"		Slightly more robust checking to see if we should operate; to speed up
"			writing when we have nothing to do or can't do anything, etc.;
"
"		[Feral:06/04/2002 @ 08:06:11 GMT-8] with the pos command on one line
"			and the cursor on a really long line (well long enough to cause
"			the resulting string to wrap in the command window) you got a
"			press enter to continue pause message after saving. Breaking up
"			the pos cmd into line (posL) and column (posC) fixes that....
"			Seems the join command ability(|) of exe displayes the line it
"			worked on. That prolly makes since really. Not what we wanted here
"			however.
"		This was true as of VIM6.0 (possibly beta); and IS true in VIM6.2;
"
"		Simplified and fixed RemoveLastHistoryItem(); (could muck up search
"			pattern)
"
"		Remed out :MOD and :NOMOD commands; I have never used them; Just unrem
"			if wanted. (just seach for nomod and unrem the code, 2 places.)
"
"		The :sub that changes the timestamp is not part of undo; thus you can
"			change the buffer, :write, then undo; Previous you had to undo
"			twice, once for the timestamp update during :write and then again
"			for your change of the buffer.
"
"	Based On:
"		Srinath Avadhanula <srinath@fastmail.fm>'s work in lastchange.vim v1.0
"		dated Sat Mar 30 04:00 PM 2002 PST
"
"	Limitation:
"		The search to find the timestamp messes with the jumplist; I do not
"		want it to; will fix when I know how!
" }}}
"
"}}}
" Original File Comments: {{{
"        File: lastchange.vim
"      Author: Srinath Avadhanula <srinath@fastmail.fm>
" Last Change: Sat Mar 30 04:00 PM 2002 PST
" Description: sets the last modification time of the current file.
"              the modification time is truncated to the last hour.  and the
"              next time the time stamp is changed, it is checked against the
"              time already stamped. this ensures that the time-stamp is
"              changed only once every hour, ensuring that the undo buffer is
"              not screwed around with every time we save.
"              To force the time stamp to be not updated, use the command:
"              		:NOMOD
"              To change it back, use
"              		:MOD
" }}}

if exists("loaded_ferallastchange")
	finish
endif
let loaded_ferallastchange = 1

" save cpo, just because...
let s:save_cpo = &cpo
set cpo&vim

" {{{ g:flastchangeleader
" If the global (presumably defined in .vimrc) flastchangeleader is not defined
"	use a default; else use the global one.
if !exists('g:flastchangeleader')
	let s:flastchangeleader = 'Last Change: '
else
	let s:flastchangeleader = g:flastchangeleader
endif
" }}}

" {{{ g:flastchangetimeformat
" If the global (presumably defined in .vimrc) flastchangetimeformat is not
"	defined use a default; else use the global one.
if !exists('g:flastchangetimeformat')
"	let s:flastchangetimeformat = '%a %b %d %I:00 %p %Y %Z'

"	Sun Jun 15 12:00 PM 2003 PDT
	let s:flastchangetimeformat = '%a %b %d %I:00 %p %Y '.substitute(strftime('%Z'), '\<\(\w\)\(\w*\)\>\(\W\|$\)', '\1', 'g')
else
	let s:flastchangetimeformat = g:flastchangetimeformat
endif
" }}}

" {{{ g:flastchangeMaxLines
" If the global (presumably defined in .vimrc) flastchangeMaxLines is not
"	defined we use a default value of 20; else use the global one.
" This is the max number of lines to consider for flastchangeleader validity.
" Set to 0 for no maximum.
if !exists('g:flastchangeMaxLines')
	let s:flastchangeMaxLines = 20
else
	let s:flastchangeMaxLines = g:flastchangeMaxLines
endif
" }}}

function s:UpdateWithLastMod() " {{{
	" If we cannot be modified; don't even try.
	if !&modifiable
		return
	end
	" If we have not been modified we have nothing to do; go away.
	if !&modified
		return
	end

"	" check for local override (set via :NOMOD and :MOD)
"	if exists('b:nomod') && b:nomod
"		return
"	end

	" Save where we are
	" See: <URL:vimhelp:restore-position>

	" save &scrolloff so H and zt do what we want.
	let old_scrolloff = &scrolloff
	let &scrolloff=0
	let inner_line = line('.')
	let inner_col = col('.')
	normal! H
	let upper_line = line('.')

	call cursor(inner_line, inner_col)

	let mess_with_folding = 0
	if has('folding') && &foldenable
		normal! zn
		let mess_with_folding = 1
	endif






	" Goto the top of the file so we seach from there.
	:call cursor(1,1)
	" search... (don't wrap around)
	let retval = search(s:flastchangeleader, 'W') "BUG:(As of 603): search() does not find string on line1;
	" if the found line is in bounds (retval will be 0 if not found)
	let DoProcess = 0
	if retval > 0
		if s:flastchangeMaxLines == 0
			let DoProcess = 1
		elseif retval <= s:flastchangeMaxLines
			let DoProcess = 1
		end
	end

	if DoProcess > 0
		let lastdate = matchstr(getline('.'), s:flastchangeleader.'\zs.*')
		let newdate = strftime(s:flastchangetimeformat)
		" if what we would write is different from what is there (aka enough
		"	time has elapsed between writes) then write our new timestamp.
		if lastdate != newdate
			"[Feral:185/03@23:55] Thanks to Asier for pointing out fiddling
			"	with undolevels did not have the desired effect. (and in fact
			"	was quite bad (flushed undo information). Ack!)

			"[Feral:186/03@00:26] This substitute() and setline() is an
			"	attempt at efficiency(this removes the histdel() call) vs the
			"	execute and histdel(); I believe they are equivalent as far as
			"	what gets done.
			let DaLine = substitute(getline(retval), s:flastchangeleader.'.*', s:flastchangeleader.newdate, '')
			:call setline(retval, DaLine)
		end
	end



	"Replace the cursor to where we found it.
"	:call cursor(posL,posC)

"	" Return to where we were

	if mess_with_folding
		normal! zN
	endif
	call cursor(upper_line, inner_col)
	normal! zt
	call cursor(inner_line, inner_col)
	let &scrolloff=old_scrolloff
endfunction " }}}

:augroup FeralLastChange " {{{
	:autocmd!
	if v:version >= 603
" :keepjumps (new with vim6.3) -- Not 100% sure it is working, but then I'm
" not 100% sure what it should be doing. If I understand it right, not
" modifying the |changelist| is exactly what I want, i.e. we won't undo. At
" least I think. This does not seem to be the case however.
"autocmd BufWritePre,FileWritePre *.abc keepjumps call SetLastChange()
" [Feral:206/04@16:16] Touch wiser; This does not effect undo, to my dismay,
" and it looks like each :call needs a keepjumps. Learning, I still am. :)
"		:autocmd BufWritePre,FileWritePre * keepjumps call <SID>UpdateWithLastMod()
		:autocmd BufWritePre,FileWritePre * :lockmarks call <SID>UpdateWithLastMod()
	else
		:autocmd BufWritePre * :call <SID>UpdateWithLastMod()
	endif

:augroup END " }}}

"command -nargs=0 NOMOD :let b:nomod = 1
"command -nargs=1 MOD   :let b:nomod = 0


" restore cpo
let &cpo = s:save_cpo
"EOF
