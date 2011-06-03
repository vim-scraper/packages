"{{{ File header information
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
"
"	\file		teol.vim
"
"	\brief		:TEOL <string> and :TSOL <string>
"				Toggle <string> at end of line ($)
"				Toggle <string> at start of line i.e. ^ (as opposed to 0): (^\s*)
"
"	\author		Robert KellyIV <sreny@rpyrpgvpjvmneq.pbz> (Rot13ed)
"				VIMSCRIPT #«XXX»;
"				URL: <URL:http://vim.sourceforge.net/script.php?script_id=«XXX»>
"	\note		This is a vim remake of an old macro I had for Multi-Edit,
"				handy it was though not so necessary with VIM, given A and :s
"				but this will not mess up your cursor position, works on a
"				range, etc. It has some use.
"	\note		I claim no copyright.
"	\date		Thu, 28 Apr 2005 14:10 PST
"	Version:	0.2
"	History: {{{
"	[Feral:118/05@13:56] 0.2
"	Improved/Fixed cursor restore. Screen should not jump; ever.
"	[Feral:224/04@19:52] 0.1
"		Initial.
" }}}
"
"}}}

if exists("loaded_teol")
	finish
endif
let loaded_teol = 1

let s:save_cpo = &cpo
set cpo&vim



"*****************************************************************
" Functions: {{{

function s:ToggleStringAt(start_or_end, String) "{{{

	if a:String == ""
		" Blank string, nothing to do.
		return
	endif

	" Save where we are
	" See: <URL:vimhelp:restore-position>
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


	let DaLine = getline('.')

	if a:start_or_end == 1
		if match(DaLine, a:String."$") == -1
			" not present, add it
"			execute 's/$/'.a:String
			let DaLine = DaLine.a:String
			:call setline(inner_line, DaLine)
		else
			" present, remove it
"			execute 's/'.a:String.'$//'
			let str_length = strlen(a:String)
			let line_length = strlen(DaLine)
			let length = line_length - str_length
			let DaLine = strpart(DaLine, 0, length)
			:call setline(inner_line, DaLine)
		endif
	else
		if match(DaLine, '^\s*'.a:String) == -1
			" not present, add it
"			execute 's/^\(\s*\)\(\S\)/\1'.a:String.'\2'
			let DaLine = substitute(DaLine,'^\(\s*\)\(\S\)', '\1'.a:String.'\2', '')
			:call setline(inner_line, DaLine)
		else
"			" present, remove it
"			execute 's/^\(\s*\)'.a:String.'/\1/'
			let DaLine = substitute(DaLine,'^\(\s*\)'.a:String, '\1', '')
			:call setline(inner_line, DaLine)
		endif
	endif


"	" Return to where we were

	if mess_with_folding
		normal! zN
	endif
	call cursor(upper_line, inner_col)
	normal zt
	call cursor(inner_line, inner_col)
	let &scrolloff=old_scrolloff

endfunction "}}}

" }}}

"*****************************************************************
" Commands: {{{
"*****************************************************************
command	-nargs=1	-range	TEOL	:lockmarks <line1>,<line2>call <SID>ToggleStringAt(1,<q-args>)
command	-nargs=1	-range	TSOL	:lockmarks <line1>,<line2>call <SID>ToggleStringAt(0,<q-args>)
"}}}


"*****************************************************************
" Mappings: {{{
"*****************************************************************

nnoremap	<leader>;	:TEOL;<cr>
nnoremap	<leader>,	:TEOL,<cr>
nnoremap	<leader>:	:TEOL:<cr>

nnoremap	<leader>s,	:TSOL, <cr>
nnoremap	<leader>s:	:TSOL: <cr>

vnoremap	<leader>;	:TEOL;<cr>
vnoremap	<leader>,	:TEOL,<cr>
vnoremap	<leader>:	:TEOL:<cr>

vnoremap	<leader>s,	:TSOL, <cr>
vnoremap	<leader>s:	:TSOL: <cr>

"}}}

let &cpo = s:save_cpo
"EOF
