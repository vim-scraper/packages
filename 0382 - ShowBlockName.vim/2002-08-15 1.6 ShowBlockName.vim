" Display the name of the current "block" the cursor is in in the command-line
" area (i.e., with vim's echo).
"
" A "block" is context-sensitive, (e.g., function name, variable name if in an
" initialization, type name if in a typedef, or screen section if in a screen
" file, etc.)
"
" Typical use (.vimrc):
"	au CursorHold *	call ShowBlockName('quiet')

"	" Tweak commands like [d so that they disable ShowBlockName so it doesn't
"	" obliterate the output of this command.
"	"
"	nnoremap [d :call ShowBlockName('[d')<CR>
"	nnoremap [i :call ShowBlockName('[i')<CR>
"
" NOTE:
"	This will NOT work if the start-of-block is the first line in a file.
"
" $Header: /usr/home/gary/.vim/autoload/RCS/ShowBlockName.vim,v 1.6 2002/08/15 22:47:08 gary Exp $

function! ShowBlockName(...)
     if a:0 == 0
	  " If called with no arguments, make sure we're enabled.
	  let s:disabled = 0

     elseif a:1 != 'quiet'
	  " We're given an argument (that's not "quiet"), disable this function
	  " for the next call; so that if called with 'quiet' (e.g., by a
	  " CursorHold autocommand), we don't do anything.
	  "
	  " This is done, e.g., by a [d re-mapping so that the macro displayed
	  " by the result displayed by [d doesn't get obliterated by the
	  " subsequent call to this function by the CursorHold autocommand.
	  "
	  let s:disabled = 1

	  if a:1 != 'OFF'
	       " If given an argument other that OFF (e.g., '[d'), execute it;
	       " we do it this way so that the command to execute is shown on
	       " the command line in the call to this function.
	       "
	       if strpart(a:1, 0, 1) == ':'
		    exe a:1
	       else
		    exe 'normal! ' . a:1
	       endif
	  endif
	  return
     endif

     if &buftype == 'quickfix' || &buftype == 'help'
	  " There are no blocks in quickfix or help windows.
	  return
     endif

     if exists("s:disabled") && s:disabled
	  " Only disable for one call (presumably the CursorHold autocommand).
	  let s:disabled = 0
	  return
     endif

     let s:disabled = 0

     if &filetype == 'vim'
	  call s:FindBlock('\s*fu\%[nction]\>!\=\s.*(\%([^)]*)\|\%(\n\s*\\[^)]*\)*\n)\)', '', '', '^\s*endf\%[unction]', 0)

     elseif &filetype == 'perl'
	  call s:FindBlock('sub\s\+\i\+\>\s*\%(.*\)\?$', '', '', '^}', 0)

     elseif &filetype == 'c'
	  call s:FindBlock('^\%(\i\+\>\%(\*\|\s\)*\)\+(\s*\%(\%(\%(,\s*\)\?\i\+\>\%(\*\|\s\)*\)\+\|\%(\%(\n[^)].*\)*\n\)\)\?)\n{', '',  '', '^}', 0)

	  " In C source, and don't appear to be in a function; see if
	  " we're in an initialization block...
	  "
	  if s:top_of_block == 0
	       call s:FindBlock('^\s*\%(\i\+\>\%(\*\|\s\)*\)\+\%(\[\]\s*\)\?=\s*{', '', '', '^\s*};', 0)
	  endif

	  if s:top_of_block == 0
	       " ...nope; how about a typedef initialization...
	       call s:FindBlock('^\s*typedef\s\+\%(\i\+\>\%(\*\|\s\)*\)\+{', '', '', '^\s*}\s*\i\+;', 1)
	  endif

     else
	  call s:FindBlock('^\%(\i\+\>\%(\*\|\s\)*\)\+(\%(\%(\n[^)].*\)*\n\)\?)', '', '', '^}', 0)
     endif

     "DEBUG call input('Top='.s:top_of_block.',Mid='.s:middle_of_block.',Bot='.s:bottom_of_block)
     if s:top_of_block != 0 || s:middle_of_block != 0 || s:bottom_of_block != 0
	  call ShowLine(s:top_of_block, s:middle_of_block, s:bottom_of_block)

     elseif a:0 == 0 || a:1 != 'quiet'
	  echohl WarningMsg | echo "NOT IN RECOGNIZED BLOCK" | echohl NONE
	  if &errorbells
	       normal! \<Esc>	" Ring bell
	  endif
     endif
endfunction

" =============================================================================

" Locate the pair of lines (top and bottom of block), if any, matching the
" block delimited by <start_pattern>...<end_pattern> that the current line is
" in.
"
" This will NOT find a block starting on the first line in a file.

function! s:FindBlock(start_pattern, middle_pattern_above, middle_pattern_below, end_pattern, set_bottom)
     " NOTE: One major reason for using searchpair() instead of search() is to
     " avoid moving the cursor (which we'd have to put back, and which also
     " affects the jumplist stack).  Besides, generally, there's a top and a
     " bottom to the block we're looking for, and searchpair takes care of
     " making sure the cursor is currently between the two.

     " We move the cursor forward one character using a normal-mode space
     " (sufficiently prefixed with a movement no-op to keep the "normal"
     " command happy) so that if the cursor is *at* the beginning of the start
     " pattern, we still find it.
     "
     " If the cursor is *on* the end pattern, an initial searchpair() will
     " fail, so we need to move up a line before the initial searchpair() call.
     "

     if (a:start_pattern =~ '\\n') ? (a:end_pattern !~ '\\n' && getline('.') !~ a:end_pattern) : (getline('.') =~ a:start_pattern)
	  exe "normal! 1 "
	  let fix_col = "normal! \<BS>"
     else
	  let fix_col = ''
     endif

     if getline('.') =~ a:end_pattern
	  " If we're sitting on the end-pattern line, we need to move up a line
	  " before doing the first searchpair, otherwise we won't be considered
	  " "inside" the pair (the cursor must be before the end pattern).
	  "
	  " Moving up one line is a pain in the ass because it could cause the
	  " screen to scroll.  If this happens, the user will have an
	  " "unexplained" screen scroll when ShowBlockName is put into a
	  " CursorHold autocommand.
	  "
	  " Even a CTRL-Y followed by a CTRL-E isn't foolproof, because if line
	  " 1 (of the file) is at the top of the screen, CTRL-Y has no effect.
	  "
	  " So, we see what the current "window line" is, do the 'k' to move up
	  " a line, and see if our screen-line is the same (i.e., 1), which
	  " means we were at the top of the screen, and we need to do a CTRL-E
	  " after the restoring 'j' to put the screen back the way it was.
	  "
	  let winline = winline()
	  normal! k
	  let fix_line = 'normal! j'
	  if winline == winline()
	       " Must be at the top of the screen, since the winline didn't
	       " change; we'll need a CTRL-E after the 'j' to fix the screen.
	       "
	       let fix_line = fix_line."\<C-E>"
	  endif
     else
	  let fix_line = ''
     endif

     let current_line  = line('.')

     "DEBUG call input(line('.').':Start="'       .a:start_pattern       .'"')
     "DEBUG call input(line('.').':Middle Above="'.a:middle_pattern_above.'"')
     "DEBUG call input(line('.').':Middle Below="'.a:middle_pattern_below.'"')
     "DEBUG call input(line('.').':End="'         .a:end_pattern         .'"')

     let s:top_of_block    = searchpair(a:start_pattern, '', a:end_pattern, 'bWn')
     let s:middle_of_block = 0
     let s:bottom_of_block = 0

     "DEBUG call input(line('.').':TOP("'.a:start_pattern.'" , "" , "'.a:end_pattern.'")--Top:'.s:top_of_block)

     if s:top_of_block != 0
	  if a:set_bottom
	       let s:bottom_of_block = searchpair(a:start_pattern, '', a:end_pattern, 'Wn')
	       "DEBUG call input(line('.').':BOT("'.a:start_pattern.'" , "" , "'.a:end_pattern.'")--Bot:'.s:bottom_of_block)
	  endif

	  if a:middle_pattern_above != '' && getline('.') =~ a:middle_pattern_above && current_line != s:top_of_block
	       let s:middle_of_block = current_line
	       "DEBUG call input(line('.').':MIDa1("'.a:start_pattern.'" , "'.a:middle_pattern_above.'" , "'.a:end_pattern.'")--Mid:'.s:middle_of_block)
	  else
	       if a:middle_pattern_below != ''
		    let s:middle_of_block = searchpair(a:start_pattern, a:middle_pattern_below, a:end_pattern, 'Wn')
		    if s:middle_of_block == s:bottom_of_block
			 let s:middle_of_block = 0
		    endif
		    "DEBUG call input(line('.').':MIDb("'.a:start_pattern.'" , "'.a:middle_pattern_below.'" , "'.a:end_pattern.'")--Mid:'.s:middle_of_block)
	       endif

	       " Undo the 'normal k' we (possibly) did above.
	       exe fix_line

	       " We're intentionally NOT testing for s:middle_of_block == 0 here
	       " because <middle_pattern_above> takes priority over
	       " <middle_pattern_below>; however we need to call searchpair after
	       " executing <fix_line>, so we do this search after the first search for
	       " <middle_pattern_below>.
	       "
	       if a:middle_pattern_above != ''
		    let middle = searchpair(a:sttart_pattern, a:middle_pattern_above, a:end_pattern, 'bWn')
		    if middle != 0 && middle != s:top_of_block
			 let s:middle_of_block = middle
		    endif
		    "DEBUG call input(line('.').':MIDa2("'.a:start_pattern.'" , "'.a:middle_pattern_above.'" , "'.a:end_pattern.'")--Mid:'.s:middle_of_block)
	       endif

	       if a:middle_pattern_below != '' && s:middle_of_block == 0
		    let s:middle_of_block = searchpair(a:start_pattern, a:middle_pattern_below, a:end_pattern, 'bWn')
		    if s:middle_of_block == s:top_of_block
			 let s:middle_of_block = 0
		    endif
		    "DEBUG call input(line('.').':MIDb2("'.a:start_pattern.'" , "'.a:middle_pattern.'" , "'.a:end_pattern.'")--Mid:'.s:middle_of_block)
	       endif
	  endif
     else
	  exe fix_line
     endif
     exe fix_col

     "DEBUG call input(line('.').':END--Top:'.s:top_of_block.' Mid:'.s:middle_of_block.' Bot:'.s:bottom_of_block)

endfunction

let g:loaded_ShowBlockName = 1

" =============================================================================
