" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/php/phpErrorMarker.vim	[[[1
30
"=============================================================================
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Sep 25 14:48:22 CEST 2009
" Licence:					GPL version 2.0 license
" GetLatestVimScripts:	2794 11432 :AutoInstall: phpErrorMarker.vim
"=============================================================================
if (!exists('phpErrorMarker#disable') || phpErrorMarker#disable <= 0) && !exists('b:phpErrorMarker_loaded')
	let b:phpErrorMarker_loaded = 1

	if &cp
		echomsg 'No compatible mode is required by phpErrorMarker'
	elseif !has('signs')
		echomsg 'Signs feature is required by phpErrorMarker'
	else
		let s:cpo = &cpo
		setlocal cpo&vim

		command -buffer MarkPhpErrors call phpErrorMarker#markErrors()
		command -buffer UnmarkPhpErrors call phpErrorMarker#unmarkErrors()
		
		call phpErrorMarker#init()

		let &cpo = s:cpo
		unlet s:cpo
	endif
endif

finish

" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
autoload/phpErrorMarker.vim	[[[1
97
"=============================================================================
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Sep 25 14:29:10 CEST 2009
" Licence:					GPL version 2.0 license
"=============================================================================
let s:id = 666
let s:counter = 0

if !exists('g:phpErrorMarker#autowrite')
	let g:phpErrorMarker#autowrite = 0
endif

if !exists('g:phpErrorMarker#openQuickfix')
	let g:phpErrorMarker#openQuickfix = 1
endif

if !exists('g:phpErrorMarker#automake')
	let g:phpErrorMarker#automake = 0
endif

if !exists('g:phpErrorMarker#php')
	let g:phpErrorMarker#php = 'php'
endif

if !exists('g:phpErrorMarker#errorformat')
	let g:phpErrorMarker#errorformat = '%m\ in\ %f\ on\ line\ %l'
endif

if !exists('g:phpErrorMarker#textError')
	let g:phpErrorMarker#textError = '><'
endif

if !exists('g:phpErrorMarker#textWarning')
	let phpErrorMarker#textWarning = '--'
endif

highlight default phpErrorMarkerWarning guifg=LightRed ctermfg=lightRed term=underline cterm=underline gui=underline
highlight default phpErrorMarkerError guifg=Red ctermfg=Red term=underline cterm=underline gui=underline

execute 'sign define phpErrorMarkerError text=' . g:phpErrorMarker#textError . ' linehl=phpErrorMarkerError texthl=phpErrorMarkerError'
execute 'sign define phpErrorMarkerWarning text=' . g:phpErrorMarker#textWarning . ' linehl=phpErrorMarkerWarning texthl=phpErrorMarkerWarning'

"init {{{1
function phpErrorMarker#init()
	let &makeprg = g:phpErrorMarker#php . ' -l %'
	let &errorformat = g:phpErrorMarker#errorformat

	augroup phpErrorMarker
		au! * <buffer>
		au QuickFixCmdPre <buffer> call phpErrorMarker#autowrite()
		au BufWritePost <buffer> call phpErrorMarker#automake()
		au QuickFixCmdPost <buffer> call phpErrorMarker#markErrors()
	augroup end
endfunction
"automake {{{1
function phpErrorMarker#automake()
	if g:phpErrorMarker#automake
		silent make
	endif
endfunction
"autowrite {{{1
function phpErrorMarker#autowrite()
	if g:phpErrorMarker#autowrite && &modified
		w
	endif
endfunction
"markErrors {{{1
function phpErrorMarker#markErrors()
	call phpErrorMarker#unmarkErrors()

	for error in getqflist()
		if error['valid'] > 0
			let s:counter += 1
			silent! execute 'sign place ' . (s:id + s:counter) . ' line=' . error['lnum'] . ' name=phpErrorMarker' . (error['text']  =~ '\cwarning' ? 'Warning' : 'Error')  . ' buffer=' . error['bufnr']
		endif
	endfor

	if s:counter > 0
		redraw
	endif

	if g:phpErrorMarker#openQuickfix
		cw
	endif
endfunction
"unmarkErrors {{{1
function phpErrorMarker#unmarkErrors()
	if s:counter > 0
		while s:counter > 0
			silent! execute 'sign unplace ' . (s:id + s:counter)
			let s:counter -= 1
		endwhile

		redraw
	endif
endfunction
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
