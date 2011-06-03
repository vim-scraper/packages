" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/php/phpErrorMarker.vim	[[[1
34
"=============================================================================
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Sep 25 14:48:22 CEST 2009
" Licence:					GPL version 2.0 license
" GetLatestVimScripts:	
"=============================================================================
if (!exists('phpErrorMarker#disable') || phpErrorMarker#disable <= 0) && !exists('b:phpErrorMarker_loaded')
	let b:phpErrorMarker_loaded = 1

	if &cp
		echomsg 'No compatible mode is required by phpErrorMarker'
	else
		let s:cpo = &cpo
		setlocal cpo&vim

		command -buffer MarkPhpErrors call phpErrorMarker#markErrors()
		command -buffer UnmarkPhpErrors call phpErrorMarker#unmarkErrors()
		
		call phpErrorMarker#init()

		augroup phpErrorMarker
			au!
			au QuickFixCmdPre make call phpErrorMarker#write()
			au QuickFixCmdPost make call phpErrorMarker#markErrors()
		augroup end

		let &cpo = s:cpo
		unlet s:cpo
	endif
endif

finish

" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
