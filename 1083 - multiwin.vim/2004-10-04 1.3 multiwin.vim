"	multiwin.vim:	Show every buffer in its own window and use
"					statuslines as "tabs"
"	Maintainer:		Patrick Avery, patrick.avery@gmail.com
"	Created:		Tue 01 Apr 2004 03:35:39 AM CDT
"	Last Modified:	Mon 04 Oct 2004 08:12:33 PM CDT
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
noremap	<silent> <script>	<Plug>MultiWinToggle	<SID>Toggle
noremap	<silent>			<SID>Toggle				:call <SID>ToggleMultiWin()<CR>

function! s:ExtraMappings()
	nmap	<silent>		gf						:new <cfile><CR>
	nmap	<silent>		<M-Left>				:wincmd W<CR>
	nmap	<silent>		<M-Right>				:wincmd w<CR>
endfunction

function! s:RemoveExtraMappings()
	unmap gf
	unmap <M-Left>
	unmap <M-Right>
endfunction

" State Functions: these functions initialize, destroy, and toggle
"_____________________________________________________________________
function! s:EnableMultiWin()
	call <SID>BackupUI()
	call <SID>SetUI()
	call <SID>AutoCommands()
	if !exists("g:multiwin_noextra")
		call <SID>ExtraMappings()
	endif
	let s:initialized = 1
endfunction

function! s:DisableMultiWin()
	call <SID>RestoreUI()
	call <SID>RemoveAutoCommands()
	if !exists("g:multiwin_noextra")
		call <SID>RemoveExtraMappings()
	endif
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
" HelpExtractor:
"  Author:	Charles E. Campbell, Jr.
"  Version:	3
"  Date:	Sep 09, 2004
" ---------------------------------------------------------------------
set lz
let docdir = substitute(expand("<sfile>:r").".txt",'\<plugin[/\\].*$','doc','')
if !isdirectory(docdir)
 if has("win32")
  echoerr 'Please make '.docdir.' directory first'
  unlet docdir
  finish
 elseif !has("mac")
  exe "!mkdir ".docdir
 endif
endif

let curfile = expand("<sfile>:t:r")
let docfile = substitute(expand("<sfile>:r").".txt",'\<plugin\>','doc','')
exe "silent! 1new ".docfile
silent! %d
exe "silent! 0r ".expand("<sfile>:p")
silent! 1,/^" HelpExtractorDoc:$/d
exe 'silent! %s/%FILE%/'.curfile.'/ge'
exe 'silent! %s/%DATE%/'.strftime("%b %d, %Y").'/ge'
norm! Gdd
silent! wq!
exe "helptags ".substitute(docfile,'^\(.*doc.\).*$','\1','e')

exe "silent! 1new ".expand("<sfile>:p")
1
silent! /^" HelpExtractor:$/,$g/.*/d
silent! wq!

set nolz
unlet docdir
unlet curfile
finish

" ---------------------------------------------------------------------
" HelpExtractorDoc:

*multiwin.txt*	Rolodex Windows in VIM							Sep 30, 2004

Author:  Patrick Avery <patrick.avery+multwin@gmail.com>

==============================================================================
1. Contents													*multiwin-contents*

	1. Contents......................: |multiwin-contents|
	2. MultiWin Manual...............: |multiwin|
	3. MultiWin Global Variables.....: |multiwin-var|
	4. MultiWin Command Keys.........: |multiwin-keys|
	5. MultiWin History..............: |multiwin-history|

==============================================================================
2. MultiWin Manual											*multiwin*

This plugin tries to replicate the command line option '-o' and forces the
active window to use as many lines as it can, while forcing inactive windows
to show one 1 line (the statusline).

Once VIM is opened with more than one file, you'll be able to navigate between
windows by clicking on their respective statusline.  This will maximize the
window and minimize all others.  You can also use Alt-Left and Alt-Right to
flip back and forth between windows (unless you have disabled it |multiwin-var|).

Be sure to use |:new| or |:split| to open new windows, as using |:e| will just
change the buffer in the current window.

If VIM is in compatible mode or in diff mode, MultiWin will not load.

==============================================================================
3. MultiWin Global Variables								*multiwin-var*


	g:singlewin	-	disable multiwin >
					Setting this in vimrc will stop multiwin from loading
<
	
	g:multiwin_noextras -	disable "extra" mappings >
							This will stop multiwin from mapping "gf", "Alt-
							Right" and "Alt-Left"
<

==============================================================================
4. MultiWin Command Keys									*multiwin-keys*


	\win -	runtime toggle									*multiwin-\win*
>
			This temporarily turns multiwin off
<

	gf -	open current file under cursor					*multiwin-gf*
>
			This remaps gf to open the current file under the cursor into a
			new window instead of in the current window
<

	A-Left -	Previous Window								*multiwin-alt-left*

	A-Right -	Next Window									*multiwin-alt-right*



==============================================================================
4. MultiWin History											*multiwin-history*

	v1.3	Oct 03 2004	Added extra mappings and documentation
	v1.2	Oct 01 2004 Added runtime toggle
	v1.1	Sep 13 2004 Changed to use wincmd _ instead of a kludge
	v1.0	Apr 01 2004 Initial release

==============================================================================
  vim:tw=78:ts=4:ft=help

