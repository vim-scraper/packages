"	multiwin.vim:	Show every buffer in its own window and use
"					statuslines as "tabs"
"	Maintainer:		Patrick Avery, patrick.avery@gmail.com
"	Created:		Tue 01 Apr 2004 03:35:39 AM CDT
"	Last Modified:	Sat 02 Oct 2004 03:29:15 AM CDT
"	Version:		1.2
"	Usage:			place in vimfiles/plugin

if exists("s:loaded") || &cp || &diff || exists("g:singlewin")
	finish
endif
let s:initialized = 0

" UI Settings: These settings change the behavior of vim to allow the
" script to work.
"____________________________________________________________________
function! s:SetUI()
	set noequalalways
	set splitbelow
	set winheight=1
	set winminheight=0
	set laststatus=2
endfunction

function! s:BackupUI()
	if &equalalways
		let s:ea = "ea"
	else
		let s:ea = "noea"
	endif
	if &sb
		let s:sb = "sb"
	else
		let s:sb = "nosb"
	endif
	let s:wh = &winheight
	let s:wmh = &winminheight
	let s:ls = &laststatus
endfunction

function! s:RestoreUI()
	exec "set " . s:ea
	exec "set " . s:sb
	exec "set winheight=" . s:wh
	exec "set winminheight=" . s:wmh
	exec "set laststatus=" . s:ls
endfunction

" Auto Commands: These autocommands make each window shrink and grow
" effectively without the negative effects of having winheight set too
" high and make VIM always behave as if -o was on the command line.
"_____________________________________________________________________
function! s:AutoCommands()
	augroup MultiWin
		autocmd!
		autocmd VimEnter * nested all | wincmd _
		autocmd WinEnter * nested wincmd _
	augroup END
endfunction

function! s:RemoveAutoCommands()
	augroup MultiWin
		autocmd!
	augroup END
endfunction

" Mappings: Makes a runtime toggle available to the user
"_____________________________________________________________________
if !hasmapto("<Plug>MultiWinToggle")
	nmap <unique> <silent> <Leader>win <Plug>MultiWinToggle
endif
noremap <silent> <script>	<Plug>MultiWinToggle	<SID>Toggle
noremap	<silent>			<SID>Toggle				:call <SID>ToggleMultiWin()<CR>

" State Functions: these functions initialize, destroy, and toggle
"_____________________________________________________________________
function! s:EnableMultiWin()
	call <SID>BackupUI()
	call <SID>SetUI()
	call <SID>AutoCommands()
	let s:initialized = 1
endfunction

function! s:DisableMultiWin()
	call <SID>RestoreUI()
	call <SID>RemoveAutoCommands()
	let s:initialized = 0
endfunction

function! s:ToggleMultiWin()
	if s:initialized
		call <SID>DisableMultiWin()
	else
		call <SID>EnableMultiWin()
	endif
endfunction

" Main:
"_____________________________________________________________________
call <SID>EnableMultiWin()


let s:loaded = 1

" vim:ts=4:fo=roq:
