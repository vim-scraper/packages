" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/fch/message.vim	[[[1
28
"=============================================================================
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Aug  7 09:43:24 CEST 2009
" Licence:					GPL version 2.0 license
"=============================================================================
"get {{{1
function fch#message#get(plugin, message)
	return '[' . a:plugin . '] ' . substitute(a:message, '\.$', '', '') . '.'
endfunction
"echo {{{1
function fch#message#echo(plugin, message)
	echo fch#message#get(a:plugin, a:message)
endfunction
"message {{{1
function fch#message#message(plugin, message)
	echomsg fch#message#get(a:plugin, a:message)
endfunction
"loadingError {{{1
function fch#message#loadingError(plugin, message)
	call fch#message#echo(a:plugin,  'Loading error: ' . a:message)
endfunction
"runtimeError {{{1
function fch#message#runtimeError(plugin, message)
	echohl ErrorMsg
	call fch#message#message(a:plugin, 'Runtime error: ' . a:message)
	echohl Normal
endfunction
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
autoload/sherlock.vim	[[[1
124
"=============================================================================
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Aug  7 09:43:24 CEST 2009
" Licence:					GPL version 2.0 license
"=============================================================================
let s:incsearch = &incsearch
"s:reset {{{1
function s:reset()
	let commandLine = getcmdline()

	if exists('s:pattern') && s:pattern['value'] != ''
		call s:resetMatch()

		let commandType = getcmdtype()

		if commandType == ':' || commandType == '/'
			cunmap /
		elseif commandType == '?'
			cunmap ?
		endif

		cunmap <Esc>
		cunmap <CR>

		let &incsearch = s:incsearch
	endif

	let s:pattern = { 'value': '', 'commandLine': '', 'position': [], 'matchID': 0, 'match': '' }

	return commandLine
endfunction
"s:setMatch {{{1
function s:setMatch(match)
	call s:resetMatch()
	let s:pattern['matchID'] = matchadd('Search', s:pattern['value'] . a:match)
endfunction
"s:resetMatch {{{1
function s:resetMatch()
	if s:pattern['matchID'] != 0
		call matchdelete(s:pattern['matchID'])
		let s:pattern['matchID'] = 0
	endif
endfunction
"s:complete {{{1
function s:complete(direction)
	if !exists('s:pattern')
		call s:reset()
	endif

	let commandLine = getcmdline()
	let commandType = getcmdtype()

	if commandType == ':' || commandType == '/' || commandType == '?'
		set noincsearch
		nohlsearch

		let value = ''

		if commandType == '/' || commandType == '?'
			let value = commandLine
		elseif commandType == ':' && commandLine =~ '\/.\{-}$'
			let value = substitute(commandLine, '^.*\/\(.\{-}\)$', '\1', '')
		endif

		if value == ''
			call s:reset()
		elseif value != s:pattern['match']
			call s:reset()

			let s:pattern['value'] = value
			let s:pattern['commandLine'] = commandLine

			if commandType == '/' || commandType == ':'
				cnoremap / <C-\>e<SID>reset()<CR>/
			else
				cnoremap ? <C-\>e<SID>reset()<CR>?
			endif

			cnoremap <Esc> <C-\>e<SID>reset()<CR><Esc>
			cnoremap <CR> <C-\>e<SID>reset()<CR><CR>
		endif

		if s:pattern['value'] != ''
			let commandLine = s:pattern['commandLine']

			let position = searchpos(s:pattern['value'], 'w' . (a:direction < 0 ? 'b' : ''))

			if position[0] == 0 && position[1] == 0
				call s:reset()
			else
				let match = ''

				if len(s:pattern['position']) > 0 && position[0] == s:pattern['position'][0] && position[1] == s:pattern['position'][1]
					let s:pattern['position'] = []
				else
					let match = escape(substitute(strpart(getline(position[0]), position[1] - 1), '^' . s:pattern['value'] . '\([^[:space:]]*\).*$', '\1', ''), '/[]')

					let s:pattern['match'] = s:pattern['value'] . match

					if len(s:pattern['position']) <= 0
						let s:pattern['position'] = position
					endif

					let commandLine .= match
				endif

				call s:setMatch(match)
			endif
		endif

		redraw
	endif

	return commandLine
endfunction
"sherlock#completeForward {{{1
function sherlock#completeForward()
	return s:complete(1)
endfunction
"sherlock#completeBackward {{{1
function sherlock#completeBackward()
	return s:complete(-1)
endfunction
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
plugin/sherlock.vim	[[[1
36
"=============================================================================
" File:						sherlock.vim
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Thu Aug  6 10:57:14 CEST 2009
" Licence:					GPL version 2.0 license
" GetLatestVimScripts:	2731 11146 :AutoInstall: sherlock.vim
"=============================================================================
if (!exists('sherlock#disable') || sherlock#disable == 0) && !exists('sherlock#loaded')
	let sherlock#name = 'sherlock'
	let sherlock#loaded = 1

	if v:version < 700
		call fch#message#loadingError(sherlock#name, "Vim version >= 7 is required")
	elseif &cp
		call fch#message#loadingError(sherlock#name, "No compatible mode is required")
	else
		let s:cpo = &cpo

		setlocal cpo&vim

		if !hasmapto('<Plug>sherlockCompleteBackward()')
			cnoremap <C-Tab> <C-\>esherlock#completeBackward()<CR>
		endif

		if !hasmapto('sherlock#completeForward()')
			cnoremap <C-S-Tab> <C-\>esherlock#completeForward()<CR>
		endif

		let &cpo= s:cpo
		unlet s:cpo
	endif
endif

finish

" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
