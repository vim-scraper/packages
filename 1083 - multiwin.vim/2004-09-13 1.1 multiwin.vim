"	multiwin.vim: Show every buffer in its own window and use
"	statuslines as "tabs"
"	Last Change: September 13, 2004
"	Maintainer:	Patrick Avery, patrick.avery@gmail.com
"	Usage: place in vimfiles/plugin
"	Version: 1.1

if exists("s:loaded_multiwin") || &cp || &diff || exists("g:singlewin")
	finish
endif

" UI Settings: These settings change the behavior of vim to allow the
" script to work.
"____________________________________________________________________
set noequalalways
set splitbelow
set winheight=1
set winminheight=0
set laststatus=2
	
" Auto Commands: These autocommands make each window shrink and grow
" effectively without the negative effects of having winheight set too
" high and make VIM always behave as if -o was on the command line.
"_____________________________________________________________________

augroup MultiWin
	autocmd VimEnter * nested all | wincmd _
	autocmd WinEnter * nested wincmd _
augroup END

let s:loaded_multiwin=1

" vim:ts=4:fo=roq:
