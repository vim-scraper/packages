

" This script remembers cursor position relative to the window for every visited buffer
" and restores it when the cursor is switched back to the buffer.
" For more pleasure you can ":set nostartofline" option.

fu! SaveCursor()
	let b:winline = winline()

" set up the hook to call TuneCursor on a buffer we switched to
" BufEnter, WinEnter, BufWinEnter don't work so here is a dirty hack via CursorHold event
" set updatetime to 0 to make window be redrawn ASAP
	let s:updatetime = &updatetime
	let &updatetime = 0
	aug StickyCursor
		au! CursorHold * call TuneCursor()
	aug END
endfu

fu! TuneCursor()
" remove the hook and restore normal updatetime
	au! StickyCursor
	let &updatetime = s:updatetime

	if exists("b:winline")
		let offset = winline() - b:winline

		if offset > 0 "
			execute "normal " . offset . "\<C-E>"
		elseif offset < 0
			execute "normal " . -offset . "\<C-Y>"
		endif
	endif 
endfu

au! BufLeave * call SaveCursor()

