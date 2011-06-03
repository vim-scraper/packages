" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/tex_atp.vim	[[[1
67
" Vim filetype plugin file
" Language:	tex
" Author:	Marcin Szamotulski
" Last Changed: 2010 July 2
" URL:		
" Email:	mszamot [AT] gmail [DOT] com
" GetLatestVimScripts: 2945 27 :AutoInstall: tex_atp.vim
" LINE ADDED.
" Copyright:    Copyright (C) 2010 Marcin Szamotulski Permission is hereby 
"		granted to use and distribute this code, with or without
" 		modifications, provided that this copyright notice is copied
" 		with it. Like anything else that's free, Automatic TeX Plugin
" 		is provided *as is* and comes with no warranty of any kind,
" 		either expressed or implied. By using this plugin, you agree
" 		that in no event will the copyright holder be liable for any
" 		damages resulting from the use of this software. 
" 		This licence is valid for all files distributed with ATP
" 		plugin.

let prefix 	= expand('<sfile>:p:h') . '/ATP_files'
let pprefix = expand('<sfile>:p:h:h')

if &cpoptions =~ '<'
	echohl WarningMsg
	echo "ATP is removing < from cpoptions"
	echohl None
	setl cpoptions-=<
endif

" if !exists('s:loaded')

	" Functions needed before setting options
	execute 'source ' . fnameescape(prefix . '/common.vim')

" endif


execute 'source '  . fnameescape(prefix . '/options.vim')


	execute 'source ' . fnameescape(prefix . '/common.vim')
	execute 'source ' . fnameescape(prefix . '/compiler.vim')

	if g:atp_LatexBox
		execute 'source ' . fnameescape(prefix . '/LatexBox_common.vim')
		execute 'source ' . fnameescape(prefix . '/LatexBox_complete.vim')
		execute 'source ' . fnameescape(prefix . '/LatexBox_motion.vim')
	endif


	execute 'source ' . fnameescape(prefix . '/motion.vim')
	execute 'source ' . fnameescape(prefix . '/search.vim')
	execute 'source ' . fnameescape(prefix . '/various.vim')


" Source maps and menu files.
execute 'source ' . fnameescape(prefix . '/mappings.vim')

if g:atp_LatexBox

	execute 'source ' . fnameescape(prefix . '/LatexBox_mappings.vim')
		
endif

execute 'source ' . fnameescape(prefix . '/menu.vim')

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/ATP_files/LatexBox_complete.vim	[[[1
386

" Language:	tex
" Maintainer:	Marcin Szamotulski
" Author:	David Mnuger
" Comment:	This is a part of LatexBox plugin 
" URL:		
" Email:	mszamot [AT] gmail [DOT] com


" latex-box/complete.vim

" <SID> Wrap {{{
function! s:GetSID()
	return matchstr(expand('<sfile>'), '\zs<SNR>\d\+_\ze.*$')
endfunction
let s:SID = s:GetSID()
call extend(g:atp_sid,{ fnamemodify(expand('<sfile>'),':t') : s:SID })
" a:1 is the file where the function is defined. 
function! s:SIDWrap(func,...)
    if a:0 == 0
	return s:SID . a:func
    else
	let l:sid=get(g:atp_sid, 'tex_atp.vim', 'error')
	if l:sid == 'error'
	    echoerr 'atp sid error'
	    return ''
	else
	    return l:sid . a:func
	endif
endfunction

" }}}


" Omni Completion {{{

let s:completion_type = ''

function! LatexBox_Complete(findstart, base)
	if a:findstart
		" return the starting position of the word
		let line = getline('.')
		let pos = col('.') - 1
		while pos > 0 && line[pos - 1] !~ '\\\|{'
			let pos -= 1
		endwhile

		let line_start = line[:pos-1]
		if line_start =~ '\C\\begin\_\s*{$'
			let s:completion_type = 'begin'
		elseif line_start =~ '\C\\end\_\s*{$'
			let s:completion_type = 'end'
		elseif line_start =~ g:LatexBox_ref_pattern . '$'
			let s:completion_type = 'ref'
		elseif line_start =~ g:LatexBox_cite_pattern . '$'
			let s:completion_type = 'bib'
			" check for multiple citations
			let pos = col('.') - 1
			while pos > 0 && line[pos - 1] !~ '{\|,'
				let pos -= 1
			endwhile
		else
			let s:completion_type = 'command'
			if line[pos - 1] == '\'
				let pos -= 1
			endif
		endif
		return pos
	else
		" return suggestions in an array
		let suggestions = []

		if s:completion_type == 'begin'
			" suggest known environments
			for entry in g:LatexBox_completion_environments
				if entry.word =~ '^' . escape(a:base, '\')
					if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^}')
						" add trailing '}'
						let entry = copy(entry)
						let entry.abbr = entry.word
						let entry.word = entry.word . '}'
					endif
					call add(suggestions, entry)
				endif
			endfor
		elseif s:completion_type == 'end'
			" suggest known environments
			let env = LatexBox_GetCurrentEnvironment()
			if env != ''
				if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^\s*[,}]')
					call add(suggestions, {'word': env . '}', 'abbr': env})
				else
					call add(suggestions, env)
				endif
			endif
		elseif s:completion_type == 'command'
			" suggest known commands
			for entry in g:LatexBox_completion_commands
				if entry.word =~ '^' . escape(a:base, '\')
					" do not display trailing '{'
					if entry.word =~ '{'
						let entry.abbr = entry.word[0:-2]
					endif
					call add(suggestions, entry)
				endif
			endfor
		elseif s:completion_type == 'ref'
			let suggestions = s:CompleteLabels(a:base)
		elseif s:completion_type == 'bib'
			" suggest BibTeX entries
			let suggestions = LatexBox_BibComplete(a:base)
		endif
		if !has('gui_running')
			redraw!
		endif
		return suggestions
	endif
endfunction
" }}}


" BibTeX search {{{

" find the \bibliography{...} commands
" the optional argument is the file name to be searched
function! LatexBox_kpsewhich(file)
	let old_dir = getcwd()
	execute 'lcd ' . LatexBox_GetTexRoot()
	redir => out
	silent execute '!kpsewhich ' . a:file
	redir END

	let out = split(out, "\<NL>")[-1]
	let out = substitute(out, '\r', '', 'g')
	let out = glob(fnamemodify(out, ':p'), 1)
	
	execute 'lcd ' . old_dir

	return out
endfunction

function! s:FindBibData(...)

	if a:0 == 0
		let file = b:atp_MainFile
	else
		let file = a:1
	endif

	if empty(glob(file, 1))
		return ''
		endif

	let lines = readfile(file)

	let bibdata_list = []

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\C\\bibliography\s*{[^}]\+}'''),
				\ 'matchstr(v:val, ''\C\\bibliography\s*{\zs[^}]\+\ze}'')')

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\C\\\%(input\|include\)\s*{[^}]\+}'''),
				\ 's:FindBibData(LatexBox_kpsewhich(matchstr(v:val, ''\C\\\%(input\|include\)\s*{\zs[^}]\+\ze}'')))')

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\C\\\%(input\|include\)\s\+\S\+'''),
				\ 's:FindBibData(LatexBox_kpsewhich(matchstr(v:val, ''\C\\\%(input\|include\)\s\+\zs\S\+\ze'')))')

	let bibdata = join(bibdata_list, ',')

	return bibdata
endfunction

let s:bstfile = expand('<sfile>:p:h') . '/vimcomplete'

function! LatexBox_BibSearch(regexp)

	" find bib data
    let bibdata = s:FindBibData()
    if bibdata == ''
	echomsg 'error: no \bibliography{...} command found'
	return
    endif

    " write temporary aux file
    let tmpbase = fnamemodify(b:atp_MainFile,":h") . '/_LatexBox_BibComplete'
    let auxfile = tmpbase . '.aux'
    let bblfile = tmpbase . '.bbl'
    let blgfile = tmpbase . '.blg'

    call writefile(['\citation{*}', '\bibstyle{' . s:bstfile . '}', '\bibdata{' . bibdata . '}'], auxfile)

    silent execute '! cd ' shellescape(fnamemodify(b:atp_MainFile,":h")) .
				\ ' ; bibtex -terse ' . fnamemodify(auxfile, ':t') . ' >/dev/null'

    let res = []
    let curentry = ''

	let lines = split(substitute(join(readfile(bblfile), "\n"), "\(\S\)\s*\n", '\1 ', 'g'), "\n")
			
    for line in filter(lines, 'v:val =~ a:regexp')
            let matches = matchlist(line, '^{\(.*\)}{\(.*\)}{\(.*\)}{\(.*\)}{\(.*\)}.*')
            if !empty(matches) && !empty(matches[1])
                call add(res, {'key': matches[1], 'type': matches[2],
							\ 'author': matches[3], 'year': matches[4], 'title': matches[5]})
	    endif
    endfor

	call delete(auxfile)
	call delete(bblfile)
	call delete(blgfile)

	return res
endfunction
" }}}

" BibTeX completion {{{
function! LatexBox_BibComplete(regexp)

	" treat spaces as '.*' if needed
	if g:LatexBox_bibtex_wild_spaces
		"let regexp = substitute(a:regexp, '\s\+', '.*', 'g')
		let regexp = '.*' . substitute(a:regexp, '\s\+', '\\\&.*', 'g')
	else
		let regexp = a:regexp
	endif

    let res = []
    for m in LatexBox_BibSearch(regexp)

        let w = {'word': m['key'],
					\ 'abbr': '[' . m['type'] . '] ' . m['author'] . ' (' . m['year'] . ')',
					\ 'menu': m['title']}

		" close braces if needed
		if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^\s*[,}]')
			let w.word = w.word . '}'
		endif

	call add(res, w)
    endfor
    return res
endfunction
" }}}

" Complete Labels {{{
" the optional argument is the file name to be searched
function! s:CompleteLabels(regex, ...)

	if a:0 == 0
		let file = LatexBox_GetAuxFile()
	else
		let file = a:1
	endif

	if empty(glob(file, 1))
		return []
	endif

	let suggestions = []

	" search for the target equation number
	for line in filter(readfile(file), 'v:val =~ ''^\\newlabel{\|^\\@input{''')

		echomsg "matching line: " . line

		" search for matching label
		let matches = matchlist(line, '^\\newlabel{\(' . a:regex . '[^}]*\)}{{\([^}]*\)}{\([^}]*\)}.*}')

		if empty(matches)
		    " also try to match label and number
		    let regex_split = split(a:regex)
			if len(regex_split) > 1
			let base = regex_split[0]
			let number = escape(join(regex_split[1:], ' '), '.')
			let matches = matchlist(line, '^\\newlabel{\(' . base . '[^}]*\)}{{\(' . number . '\)}{\([^}]*\)}.*}')
		    endif
		endif

		if empty(matches)
			" also try to match number
			let matches = matchlist(line, '^\\newlabel{\([^}]*\)}{{\(' . escape(a:regex, '.') . '\)}{\([^}]*\)}.*}')
		endif

		if !empty(matches)

			let entry = {'word': matches[1], 'menu': '(' . matches[2] . ') [p.' . matches[3] . ']'}

			if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^\s*[,}]')
				" add trailing '}'
				let entry = copy(entry)
				let entry.abbr = entry.word
				let entry.word = entry.word . '}'
			endif
			call add(suggestions, entry)
		endif

		" search for included files
		let included_file = matchstr(line, '^\\@input{\zs[^}]*\ze}')
		if included_file != ''
			let included_file = LatexBox_kpsewhich(included_file)
			call extend(suggestions, s:CompleteLabels(a:regex, included_file))
		endif
	endfor

	return suggestions

endfunction
" }}}

" Change Environment {{{
function! s:ChangeEnvPrompt()

	let [env, lnum, cnum, lnum2, cnum2] = LatexBox_GetCurrentEnvironment(1)

	let new_env = input('change ' . env . ' for: ', '', 'customlist,' . s:SIDWrap('GetEnvironmentList'))
	if empty(new_env)
		return
	endif

	if new_env == '\[' || new_env == '['
		let begin = '\['
		let end = '\]'
	elseif new_env == '\(' || new_env == '('
		let begin = '\('
		let end = '\)'
	else
		let l:begin = '\begin{' . new_env . '}'
		let l:end = '\end{' . new_env . '}'
	endif
	
	if env == '\[' || env == '\('
		let line = getline(lnum2)
		let line = strpart(line, 0, cnum2 - 1) . l:end . strpart(line, cnum2 + 1)
		call setline(lnum2, line)

		let line = getline(lnum)
		let line = strpart(line, 0, cnum - 1) . l:begin . strpart(line, cnum + 1)
		call setline(lnum, line)
	else
		let line = getline(lnum2)
		let line = strpart(line, 0, cnum2 - 1) . l:end . strpart(line, cnum2 + len(env) + 5)
		call setline(lnum2, line)

		let line = getline(lnum)
		let line = strpart(line, 0, cnum - 1) . l:begin . strpart(line, cnum + len(env) + 7)
		call setline(lnum, line)
	endif

endfunction

function! s:GetEnvironmentList(lead, cmdline, pos)
	let suggestions = []
	
	if !exists("b:atp_LocalEnvironments")
	    call LocalCommands()
	endif
	let l:completion_list=atplib#Extend(g:atp_Environments,b:atp_LocalEnvironments)

	for entry in l:completion_list
" 		let env = entry.word
		if entry =~ '^' . a:lead
			call add(suggestions, entry)
		endif
	endfor

	if len(suggestions) > 5
	    call sort(suggestions)
	endif

	return suggestions
endfunction
" }}}

" Next Charaters Match {{{
function! s:NextCharsMatch(regex)
	let rest_of_line = strpart(getline('.'), col('.') - 1)
	return rest_of_line =~ a:regex
endfunction
" }}}

" Mappings {{{
nmap <Plug>LatexChangeEnv			:call <SID>ChangeEnvPrompt()<CR>
" }}}

ftplugin/ATP_files/LatexBox_indent.vim	[[[1
75
" LaTeX indent file (part of LaTeX Box)
" Maintainer: David Munger (mungerd@gmail.com)

let b:did_indent = 1

setlocal indentexpr=LatexBox_TexIndent()
setlocal indentkeys==\end,=\item,),],},o,0\\

let s:itemize_envs = ['itemize', 'enumerate', 'description']

" indent on \left( and on \(, but not on (
" indent on \left[ and on \[, and on [
" indent on \left\{ and on \{, and on {
let s:open_pat = '\\begin\>\|\%(\\left\s*\)\=\\\=[[{]\|\%(\\left\s*\|\\\)('
let s:close_pat = '\\end\>\|\%(\\right\s*\)\=\\\=[]}]\|\%(\\right\s*\|\\\))'


" Compute Level {{{
function! s:ComputeLevel(lnum_prev, open_pat, close_pat)

	let n = 0

	let line_prev = getline(a:lnum_prev)

	" strip comments
	let line_prev = substitute(line_prev, '\\\@<!%.*', '', 'g')

	" find unmatched opening patterns on previous line
	let n += len(substitute(substitute(line_prev, a:open_pat, "\n", 'g'), "[^\n]", '', 'g'))
	let n -= len(substitute(substitute(line_prev, a:close_pat, "\n", 'g'), "[^\n]", '', 'g'))

	" reduce indentation if current line starts with a closing pattern
	if getline(v:lnum) =~ '^\s*\%(' . a:close_pat . '\)'
		let n -= 1
	endif

	" compensate indentation if previous line starts with a closing pattern
	if line_prev =~ '^\s*\%(' . a:close_pat . '\)'
		let n += 1
	endif
	
	return n
endfunction
" }}}

" TexIndent {{{
function! LatexBox_TexIndent()

	let lnum_prev = prevnonblank(v:lnum - 1)

	if lnum_prev == 0
		return 0
	endif

	let n = 0

	let n += s:ComputeLevel(lnum_prev, s:open_pat, s:close_pat)

	let n += s:ComputeLevel(lnum_prev, '\\begin{\%(' . join(s:itemize_envs, '\|') . '\)}',
				\ '\\end{\%(' . join(s:itemize_envs, '\|') . '\)}')

	" less shift for lines starting with \item
	let item_here = getline(v:lnum) =~ '^\s*\\item'
	let item_above = getline(lnum_prev) =~ '^\s*\\item'
	if !item_here && item_above
		let n += 1
	elseif item_here && !item_above
		let n -= 1
	endif

	return indent(lnum_prev) + n * &sw
endfunction
" }}}

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/LatexBox_mappings.vim	[[[1
19
" Author:	David Mungerd
" Maintainer:	Marcin Szamotulski

" begin/end pairs {{{
nmap <buffer> % <Plug>LatexBox_JumpToMatch
xmap <buffer> % <Plug>LatexBox_JumpToMatch
" xmap <buffer> <C-%> <Plug>LatexBox_BackJumpToMatch
vmap <buffer> ie <Plug>LatexBox_SelectCurrentEnvInner
vmap <buffer> iE <Plug>LatexBox_SelectCurrentEnVInner
vmap <buffer> ae <Plug>LatexBox_SelectCurrentEnvOuter
omap <buffer> ie :normal vie<CR>
omap <buffer> ae :normal vae<CR>
vmap <buffer> i$ <Plug>LatexBox_SelectInlineMathInner
vmap <buffer> a$ <Plug>LatexBox_SelectInlineMathOuter
omap <buffer> i$ :normal vi$<CR>
omap <buffer> a$ :normal va$<CR>

setlocal omnifunc=LatexBox_Complete

ftplugin/ATP_files/LatexBox_motion.vim	[[[1
670
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Author:	David Mnuger
" Comment:	This is a part of ATP plugin borrowed from LatexBox plugin 
" URL:		
" Email:	mszamot [AT] gmail [DOT] com

" Some things is enough to source once
let s:did_script = exists("s:did_script") ? 1 : 0

" From latex-box/common.vim 
" In Comment {{{
" LatexBox_InComment([line], [col])
" return true if inside comment
function! LatexBox_InComment(...)
	let line	= a:0 >= 1 ? a:1 : line('.')
	let col		= a:0 >= 2 ? a:2 : col('.')
	return synIDattr(synID(line("."), col("."), 0), "name") =~# '^texComment'
endfunction
" }}}

" s:HasSyntax(syntaxName, [line], [col])
function! s:HasSyntax(syntaxName, ...)
	let line	= a:0 >= 1 ? a:1 : line('.')
	let col		= a:0 >= 2 ? a:2 : col('.')
	return index(map(synstack(line, col), 'synIDattr(v:val, "name") == "' . a:syntaxName . '"'), 1) >= 0
endfunction

" s:SearchAndSkipComments(pattern, [flags], [stopline])
function! s:SearchAndSkipComments(pat, ...)
	let flags	= a:0 >= 1 ? a:1 : ''
	let stopline	= a:0 >= 2 ? a:2 : 0
	let saved_pos 	= getpos('.')

	" search once
	let ret = search(a:pat, flags, stopline)

	if ret
		" do not match at current position if inside comment
		let flags = substitute(flags, 'c', '', 'g')

		" keep searching while in comment
		while LatexBox_InComment()
			let ret = search(a:pat, flags, stopline)
			if !ret
				break
			endif
		endwhile
	endif

	if !ret
		" if no match found, restore position
		keepjumps call setpos('.', saved_pos)
	endif

	return ret
endfunction

" begin/end pairs {{{
"
" s:JumpToMatch(mode, [backward])
" - search backwards if backward is given and nonzero
" - search forward otherwise
"
function! s:JumpToMatch(mode, ...)

    	if a:0 >= 1
	    let backward = a:1
	else
	    let backward = 0
	endif

	let sflags = backward ? 'cbW' : 'cW'

	" selection is lost upon function call, reselect
	if a:mode == 'v'
		normal! gv
	endif

	" open/close pairs (dollars signs are treated apart)
	let open_pats 		= ['{', '\[', '(', '\\begin\>', '\\left\>']
	let close_pats 		= ['}', '\]', ')', '\\end\>', '\\right\>']
	let dollar_pat 		= '\\\@<!\$'
	let two_dollar_pat 	= '\\\@<!\$\$'

	let saved_pos = getpos('.')

	" move to the left until not on alphabetic characters
	call search('\A', 'cbW', line('.'))

	" go to next opening/closing pattern on same line
	if !s:SearchAndSkipComments(
				\	'\m\C\%(' . join(open_pats + close_pats + [dollar_pat], '\|') . '\)',
				\	sflags, line('.'))
		" abort if no match or if match is inside a comment
		keepjumps call setpos('.', saved_pos)
		return
	endif

	let rest_of_line = strpart(getline('.'), col('.') - 1)

	" match for '$' pairs
	if rest_of_line =~ '^\$'

		" check if next character is in inline math
		let [lnum, cnum] = searchpos('.', 'nW')
		if lnum && s:HasSyntax('texMathZoneX', lnum, cnum)
			call s:SearchAndSkipComments(dollar_pat, 'W')
		elseif !s:HasSyntax('texMathZoneY', lnum, cnum)
			call s:SearchAndSkipComments(dollar_pat, 'bW')
		endif

		if lnum && s:HasSyntax('texMathZoneY', lnum, cnum)
			call s:SearchAndSkipComments(two_dollar_pat, 'W')
		else 
			call s:SearchAndSkipComments(two_dollar_pat, 'bW')
		endif
	else

		" match other pairs
	for i in range(len(open_pats))
		let open_pat = open_pats[i]
		let close_pat = close_pats[i]

			if rest_of_line =~ '^\C\%(' . open_pat . '\)'
			" if on opening pattern, go to closing pattern
				call searchpair('\C' . open_pat, '', '\C' . close_pat, 'W', 'LatexBox_InComment()')
			return
			elseif rest_of_line =~ '^\C\%(' . close_pat . '\)'
			" if on closing pattern, go to opening pattern
				call searchpair('\C' . open_pat, '', '\C' . close_pat, 'bW', 'LatexBox_InComment()')
			return
		endif

	endfor
    endif

endfunction

nnoremap <silent> <Plug>LatexBox_JumpToMatch 		:call <SID>JumpToMatch('n')<CR>
vnoremap <silent> <Plug>LatexBox_JumpToMatch 		:<C-U>call <SID>JumpToMatch('v')<CR>
nnoremap <silent> <Plug>LatexBox_BackJumpToMatch 	:call <SID>JumpToMatch('n', 1)<CR>
vnoremap <silent> <Plug>LatexBox_BackJumpToMatch 	:<C-U>call <SID>JumpToMatch('v', 1)<CR>
" }}}

" select inline math {{{
" s:SelectInlineMath(seltype)
" where seltype is either 'inner' or 'outer'
function! s:SelectInlineMath(seltype)

    	let saved_pos		= getpos('.')

	let ZoneX_pat_O 	= '\\\@<!\$'
	let ZoneX_pat_C 	= '\\\@<!\$'
	let ZoneY_pat_O 	= '\\\@<!\$\$'
	let ZoneY_pat_C 	= a:seltype == 'inner' ? '\\\@<!\$\$' 	: '\\\@<!\$\$'
	let ZoneV_pat_O		= '\\\@<!\\('
	let ZoneV_pat_C		= a:seltype == 'inner' ? '\\\@<!\\)' 	: '\\\@<!\\\zs)' 
	let ZoneW_pat_O		= '\\\@<!\\\['
	let ZoneW_pat_C		= a:seltype == 'inner' ? '\\\@<!\\\]'	: '\\\@<!\\\zs\]'

	if 	( s:HasSyntax('texMathZoneV', line("."), max([1,col(".")-1])) ||
		\ s:HasSyntax('texMathZoneW', line("."), max([1,col(".")-1])) ||
		\ s:HasSyntax('texMathZoneX', line("."), max([1,col(".")-1])) ||
		\ s:HasSyntax('texMathZoneY', line("."), max([1,col(".")-1])) && b:atp_TexFlavour == 'plaintex' )  && 
		\ col(".") > 1
	    normal! h
	elseif 	( s:HasSyntax('texMathZoneV', line("."), max([1,col(".")-2])) ||
		\ s:HasSyntax('texMathZoneW', line("."), max([1,col(".")-2])) ||
		\ s:HasSyntax('texMathZoneY', line("."), max([1,col(".")-2])) && b:atp_TexFlavour == 'plaintex' )  && 
		\ col(".") > 2
	    normal! 2h
	endif

	let return 		= 1 
	let math_zones		= ( b:atp_TexFlavour == 'plaintex' ? [ 'V', 'W', 'X', 'Y'] : [ 'V', 'W', 'X'] )
	for L in math_zones
	    if s:HasSyntax('texMathZone'. L, line(".")) ||
			\ s:HasSyntax('texMathZone'. L, line("."), max([1, col(".")-1]))
		    call s:SearchAndSkipComments(Zone{L}_pat_O, 'cbW')
		    let zone 	= L
		    let return 	= 0
	    endif
	endfor

	if return
	    call cursor(saved_pos[1], saved_pos[2])
	    return
	endif

	if a:seltype == 'inner'
	    if zone =~ '^V\|W$' || zone == 'Y' && b:atp_TexFlavour == 'plaintex'
		normal! 2l
	    elseif zone == 'X'
		normal! l
	    endif
	    if getline(".")[col(".")-1] == ' '
		normal! w
	    endif
	endif

	if visualmode() ==# 'V'
		normal! V
	else
		normal! v
	endif

	call s:SearchAndSkipComments(Zone{zone}_pat_C, 'W')

	if a:seltype == 'inner'
	    if getline(".")[col(".")-2] == ' '
		normal! ge
	    else
		if col(".") > 1
		    call cursor(line("."),col(".")-1)
		else
		    call cursor(line(".")-1, len(getline(line(".")-1)))
		endif
	    endif
	endif

	if a:seltype == 'outer' && zone == 'Y'
	    call cursor(line("."),col(".")+1)
	endif
endfunction


vnoremap <silent> <Plug>LatexBox_SelectInlineMathInner :<C-U>call <SID>SelectInlineMath('inner')<CR>
vnoremap <silent> <Plug>LatexBox_SelectInlineMathOuter :<C-U>call <SID>SelectInlineMath('outer')<CR>
" }}}

" {{{ select current bracket
" a:seltype		= 'inner' / 'outer' / 'INNER' / 'OUTER'
function! s:SelectCurrentBracket(seltype) 

    let bracket_dict	= { '{' : '}', '(': ')', '[' : '\]' }
    let OUTER_flag	= a:seltype =~  '\C^OUTER\|INNER$'  ? 'r' : ''

    if getline(".")[col(".")-1] =~ '^}\|)\|\]$'
	let line = searchpair('{\|\\\@<!(\|\\\@<!\[', '', '}\|\\\@<!)\|\\\@<!\]', 'bsW' . OUTER_flag )
    else
	let line = searchpair('{\|\\\@<!(\|\\\@<!\[', '', '}\|\\\@<!)\|\\\@<!\]', 'bcsW' . OUTER_flag )
    endif

    let b:pos1 = getpos(".")

    " return if opening bracket was not found.
    if !line
	return
    endif

    let opening_bracket	= getline(".")[col(".")-1]
    let closing_bracket = get(bracket_dict, opening_bracket, 0) 
    let opening_bracket = opening_bracket == '[' ? '\[' : opening_bracket

    let pos	= getpos(".")

    if visualmode() ==# 'V'
	normal! V
    else
	normal! v
    endif

    if a:seltype == 'outer'
	call cursor(pos[1], pos[2])
    endif

    " Find closing bracket.
    call searchpair( opening_bracket, '', closing_bracket, 'W' )

    " begining offset
    normal! o
    if a:seltype == 'outer'
	call search('\\\%'.col('.').'c\%'.line('.').'l', 'b')
    elseif a:seltype == 'inner'
	normal! l
	if getline(".")[col(".")-1] == ' '
	    normal! w
	endif
    endif

    " end offset
    normal! o
    if a:seltype == 'inner'
	if getline(".")[col(".")-2] =~ '^\s\|\\$'
	    normal! ge
	else
	    if col(".") > 1
		call cursor(line("."),col(".")-1)
	    else
		call cursor(line(".")-1, len(getline(line(".")-1)))
	    endif
	endif
    endif

endfunction
vnoremap <silent> <Plug>SelectInnerBracket :call <SID>SelectCurrentBracket('inner')<CR>
vnoremap <silent> <Plug>SelectOuterBracket :call <SID>SelectCurrentBracket('outer')<CR>
vnoremap <silent> <Plug>SelectINNERBracket :call <SID>SelectCurrentBracket('INNER')<CR>
vnoremap <silent> <Plug>SelectOUTERBracket :call <SID>SelectCurrentBracket('OUTER')<CR>
" These maps should be moved to mappings.vim:
vmap i)		<ESC>:call <SID>SelectCurrentBracket('inner')<CR>
vmap a)		<ESC>:call <SID>SelectCurrentBracket('outer')<CR>
vmap A)		<ESC>:call <SID>SelectCurrentBracket('OUTER')<CR>
vmap I)		<ESC>:call <SID>SelectCurrentBracket('INNER')<CR>
" }}}

" {{{ select syntax
" syntax groups 'texDocZone' and 'texSectionZone' need to be synchronized
" before ':syntax sync fromstart' which is quite slow. It is better to provide
" other method of doing this. (If s:SelectSyntax is not syncing the syntax
" then the behaviour is unpredictable).
function! s:SelectSyntax(syntax)

    " mark the current position
    normal! m'

    let synstack	= map(synstack(line("."),col(".")), 'synIDattr(v:val, "name")')
    if  synstack == []
	return

    endif

    if a:syntax == 'inner'

	let len		= len(synstack)
	let syntax	= synstack[max([0, len-1])]

    elseif a:syntax == 'outer'
	let syntax	= synstack[0]

    else
	let syntax	= a:syntax

    endif

    " there are better method 
    if count([ 'texDocZone', 'texSectionZone'], syntax)
	return
    endif

    let save_ww		= &l:ww
    set ww		+=b,l
    let save_pos	= getpos(".")	 


    if !count(map(synstack(line("."),col(".")), 'synIDattr(v:val, "name")'), syntax)
	return

    endif

    while count(map(synstack(line("."),col(".")), 'synIDattr(v:val, "name")'), syntax)
	normal! h
	" for some syntax groups do not move to previous line
	if col(".") == 1 && count(['texStatement', 'texTypeSize'], syntax)
	    keepjumps normal! h
	    break
	endif

    endwhile

    " begin offset
    if getpos(".")[2] < len(getline("."))
	call cursor(line("."),col(".")+1)

    else
	call cursor(line(".")+1, 1)

    endif

    if visualmode() ==# 'V'
	normal! V

    else
	normal! v

    endif

    call cursor(save_pos[1], save_pos[2]) 
    while count(map(synstack(line("."),max([1, min([col("."), len(getline("."))])])), 'synIDattr(v:val, "name")'), syntax) || len(getline(".")) == 0 
	keepjumps normal! l
	" for some syntax groups do not move to next line
	if col(".") == len(getline(".")) && count(['texStatement', 'texTypeSize'], syntax)
	    keepjumps normal! l
	    break
	endif
    endwhile

    " end offset
    if len(getline(".")) == 0
	call cursor(line(".")-1,len(getline(line(".")-1)))
    endif
    if count(['texParen', 'texLength', 'Delimiter', 'texStatement', 'texTypeSize', 'texRefZone', 'texSectionMarker', 'texTypeStyle'], syntax)
	if col(".") > 1
	    call cursor(line("."),col(".")-1)

	else
	    call cursor(line(".")-1,len(getline(line(".")-1)))

	endif
    elseif count(['texMathZoneV', 'texMathZoneW', 'texMathZoneY'], syntax)
	    call cursor(line("."),col(".")+1)

    endif

    let &l:ww	= save_ww
endfunction
vnoremap <silent> <buffer> <Plug>SelectInnerSyntax 	<ESC>:<C-U>call <SID>SelectSyntax('inner')<CR>
vnoremap <silent> <buffer> <Plug>SelectOuterSyntax 	<ESC>:<C-U>call <SID>SelectSyntax('outer')<CR>
vmap as		<ESC>:<C-U>call <SID>SelectSyntax('outer')<CR>
vmap is		<ESC>:<C-U>call <SID>SelectSyntax('inner')<CR>
" }}}

" select current environment {{{
function! s:SelectCurrentEnv(seltype)
	let [env, lnum, cnum, lnum2, cnum2] = LatexBox_GetCurrentEnvironment(1)
	call cursor(lnum, cnum)
	if a:seltype == 'inner'
		if env =~ '^\'
			call search('\\.\_\s*\S', 'eW')
		else
" 			call search('}\%(\_\s*\[\_[^]]*\]\)\?\%(\_\s*\\label\s*{[^}]*}\)\?\_\s*\S', 'eW')
			call search('}\%(\_\s*\[\_[^]]*\]\)\?\_\s*\S', 'eW')
		endif
	endif
	if visualmode() ==# 'V'
		normal! V
	else
		normal! v
	endif
	call cursor(lnum2, cnum2)
	if a:seltype == 'inner'
		call search('\S\_\s*', 'bW')
	else
		if env =~ '^\'
			normal! l
		else
			call search('}', 'eW')
		endif
	endif
endfunction

function! s:SelectCurrentEnV()
	call s:SelectCurrentEnv('inner')
	execute 'normal o'
	call s:JumpToMatch('n', 1)
	execute 'normal o'
endfunction

vnoremap <silent> <Plug>LatexBox_SelectCurrentEnvInner :<C-U>call <SID>SelectCurrentEnv('inner')<CR>
vnoremap <silent> <Plug>LatexBox_SelectCurrentEnVInner :<C-U>call <SID>SelectCurrentEnV()<CR>
vnoremap <silent> <Plug>LatexBox_SelectCurrentEnvOuter :<C-U>call <SID>SelectCurrentEnv('outer')<CR>
" }}}

" Jump to the next braces {{{
"
function! LatexBox_JumpToNextBraces(backward)
	let flags = ''
	if a:backward
		normal h
		let flags .= 'b'
	else
		let flags .= 'c'
	endif
	if search('[][}{]', flags) > 0
		normal l
	endif
	let prev = strpart(getline('.'), col('.') - 2, 1)
	let next = strpart(getline('.'), col('.') - 1, 1)
	if next =~ '[]}]' && prev !~ '[][{}]'
		return "\<Right>"
	else
		return ''
	endif
endfunction
" }}}

" Highlight Matching Pair {{{
" TODO: Redefine NoMatchParen and DoMatchParen functions to handle
" s:HighlightMatchingPair function.
" TODO: do not match for \begin{document}:\end{document}
" 	or limit matches to the window (anyway it is done every time the
" 	cursor moves).
" 	winheight(0)			returns window height
" 	winsaveview()['topline'] 	returns the top line
function! s:HighlightMatchingPair()

	2match none

	if LatexBox_InComment()
		return
	endif

	let open_pats 		= ['\\begin\>\ze\(\s*{\s*document\s*}\)\@!', '\\left\>']
	let close_pats 		= ['\\end\>\ze\(\s*{\s*document\s*}\)\@!', '\\right\>']
	let dollar_pat 		= '\\\@<!\$'
	let two_dollar_pat 	= '\\\@<!\$\$'

	let saved_pos = getpos('.')

	if getline('.')[col('.') - 1] == '$'

	   if strpart(getline('.'), col('.') - 2, 1) == '\'
		   return
	   endif

		" match $-pairs
		let lnum = line('.')
		let cnum = col('.')

		" check if next or previous character is \$
		let two_dollars = ( getline('.')[col('.') - 2] == '$' ? 'p' : 
			    			\ ( getline('.')[col('.') ] == '$' ? 'n' : '0' ) )
		let b:two_dollars = two_dollars
	
		if two_dollars == '0' || b:atp_TexFlavour == 'tex'

		    " check if next character is in inline math
		    let [lnum2, cnum2] = searchpos('.', 'nW')
		    if lnum2 && s:HasSyntax('texMathZoneX', lnum2, cnum2)
			    call s:SearchAndSkipComments(dollar_pat, 'W')
		    else
			    call s:SearchAndSkipComments(dollar_pat, 'bW')
		    endif

		    execute '2match MatchParen /\%(\%' . lnum . 'l\%' . cnum . 'c\$'
					    \	. '\|\%' . line('.') . 'l\%' . col('.') . 'c\$\)/'

		elseif b:atp_TexFlavour == 'plaintex'
		    
		    " check if next character is in inline math
		    if two_dollars == 'n'
			call cursor(line('.'), col('.')+1)
		    endif
		    " position of the openning \$\$
		    let cnum = col('.')-1
		    let [lnum2, cnum2] = searchpos( '.' , 'nW')
		    if lnum2 && s:HasSyntax('texMathZoneY', lnum2, cnum2)
			    call s:SearchAndSkipComments(two_dollar_pat, 'W')
		    else
			    call s:SearchAndSkipComments(two_dollar_pat, 'bW')
		    endif
		    let cnum_e=cnum+1
		    let cnum_E=col('.')
		    let cnum_Ee=cnum_E+1
		    execute '2match MatchParen /\%(\%' . lnum . 'l\%' . cnum . 'c\$'
					    \	. '\|\%' . lnum . 'l\%' . cnum_e . 'c\$'
					    \	. '\|\%' . line('.') . 'l\%' . cnum_E . 'c\$'
					    \	. '\|\%' . line('.') . 'l\%' . cnum_Ee . 'c\$\)/'

		endif

	else
		" match other pairs

		" find first non-alpha character to the left on the same line
		let [lnum, cnum] = searchpos('\A', 'cbW', line('.'))

		let delim = matchstr(getline(lnum), '^\m\(' . join(open_pats + close_pats, '\|') . '\)', cnum - 1)

		if empty(delim)
			call setpos('.', saved_pos)
			return
		endif

		for i in range(len(open_pats))
			let open_pat = open_pats[i]
			let close_pat = close_pats[i]

			if delim =~# '^' . open_pat
				" if on opening pattern, go to closing pattern
				call searchpair('\C' . open_pat, '', '\C' . close_pat, 'W', 'LatexBox_InComment()')
				execute '2match MatchParen /\%(\%' . lnum . 'l\%' . cnum . 'c' . open_pats[i]
							\	. '\|\%' . line('.') . 'l\%' . col('.') . 'c' . close_pats[i] . '\)/'
				break
			elseif delim =~# '^' . close_pat
				" if on closing pattern, go to opening pattern
				call searchpair('\C' . open_pat, '', '\C' . close_pat, 'bW', 'LatexBox_InComment()')
				execute '2match MatchParen /\%(\%' . line('.') . 'l\%' . col('.') . 'c' . open_pats[i]
							\	. '\|\%' . lnum . 'l\%' . cnum . 'c' . close_pats[i] . '\)/'
				break
			endif
		endfor
	endif

	call setpos('.', saved_pos)
endfunction
" }}}


if !s:did_script
    augroup LatexBox_HighlightPairs
      " Replace all matchparen autocommands
      autocmd! CursorMoved *.tex call s:HighlightMatchingPair()
    augroup END
endif

" Highlight bold and italic, by M. Szamotulski
" (to add: optionaly only in gui) 
" this function should do that for every \texbf on the screen
" {{{
" THIS IS TOO SLOW:
function! HighlightEmphText()

     let saved_pos	= getpos('.')
     
     let top_line	= winsaveview()['topline']
     let end_line	= top_line + winheight(0)

     call cursor(top_line, 1)

     keepjumps let [start_lnum, start_cnum] = searchpos('\\\%(textbf{\|bf\)\zs', 'W', end_line)
     let [lnum, cnum] = copy([ start_lnum, start_cnum])

     " if there are no matches, return. 
     if [ lnum, cnum] == [0, 0]
	 return
     endif

     while start_lnum <= end_line && [lnum, cnum] != [0, 0]
     
	 let [lnum, cnum] = copy([ start_lnum, start_cnum])

	 if [lnum, cnum] == [ 0, 0]
	     keepjumps call setpos( '.', saved_pos)
	     return
	 endif

" 	 echomsg lnum . " " . cnum
	 while s:HasSyntax('texMatcher', lnum, cnum)
	     if cnum < len(getline(lnum))
		 let cnum += 1
	     else
		 let lnum += 1
		 let cnum  = 1
	     endif
	 endwhile

	 if cnum == 1
	     let stop_lnum = lnum-1
	     let stop_cnum = len(getline(stop_lnum))
	 else
	     let stop_lnum = lnum
	     let stop_cnum = cnum
	 endif


	 let start_lnum 	-= 1
	 let start_cnum		-= 1
	 let stop_lnum  	+= 1

	 call matchadd( 'textBold', '\%>' . start_lnum . 'l\%>' . start_cnum . 'c' . '\%<' . stop_lnum . 'l\%<' . stop_cnum . 'c')

	 let [start_lnum, start_cnum] = searchpos('\\\%(textbf{\|bf\)\zs', 'W', end_line)

     endwhile

     keepjumps call setpos( '.', saved_pos)

"      return [start_lnum, start_cnum, stop_lnum, stop_cnum]
 endfunction
" the 2match function can be run once:
" call s:HighlightEmphText()
" if !s:did_script
"     augroup HighlightEmphText
"       " Replace all matchparen autocommands
"       autocmd CursorMoved *.tex call HighlightEmphText()
"     augroup END
" endif
" }}}
ftplugin/ATP_files/LatexBox_variables.vim	[[[1
45
" Author: 	David Munger
" Maintainer:	Marcin Szamotulski

" Latex_Box variables used by tools from David Munger.
if g:atp_LatexBox == 1 || (g:atp_check_if_LatexBox && len(split(globpath(&rtp,'ftplugin/tex_LatexBox.vim')))) 
" {{{
    if !exists('g:LatexBox_cite_pattern')
	    let g:LatexBox_cite_pattern = '\\cite\(p\|t\)\?\*\?\_\s*{'
    endif

    if !exists('g:LatexBox_ref_pattern')
	    let g:LatexBox_ref_pattern = '\\v\?\(eq\|page\)\?ref\*\?\_\s*{'
    endif

    let g:LatexBox_complete_with_brackets = 1
    let g:LatexBox_bibtex_wild_spaces = 1

    let g:LatexBox_completion_environments = [
	    \ {'word': 'itemize',		'menu': 'bullet list' },
	    \ {'word': 'enumerate',		'menu': 'numbered list' },
	    \ {'word': 'description',	'menu': 'description' },
	    \ {'word': 'center',		'menu': 'centered text' },
	    \ {'word': 'figure',		'menu': 'floating figure' },
	    \ {'word': 'table',		'menu': 'floating table' },
	    \ {'word': 'equation',		'menu': 'equation (numbered)' },
	    \ {'word': 'align',		'menu': 'aligned equations (numbered)' },
	    \ {'word': 'align*',		'menu': 'aligned equations' },
	    \ {'word': 'document' },
	    \ {'word': 'abstract' },
	    \ ]

    let g:LatexBox_completion_commands = [
	    \ {'word': '\begin{' },
	    \ {'word': '\end{' },
	    \ {'word': '\item' },
	    \ {'word': '\label{' },
	    \ {'word': '\ref{' },
	    \ {'word': '\eqref{eq:' },
	    \ {'word': '\cite{' },
	    \ {'word': '\nonumber' },
	    \ {'word': '\bibliography' },
	    \ {'word': '\bibliographystyle' },
	    \ ]
" }}}
endif
ftplugin/ATP_files/common.vim	[[[1
269
" Author: Marcin Szamotulski
" 
" This file contains set of functions which are needed to set to set the atp
" options and some common tools.

" This function finds all the input files declared in the source file.
" {{{ Find Input Files
" It returns a dictionary of the form:
" { <input__name> : [ 'type', 'main_file', 'full_path'] } 
" where 'type' is one of 'bib', 'input', 'main file'. 
" 			'main file' 	if it is recognized as a main project file
" 			'bib' 		if it is a bibliography 'input' 	if it
" 					is an included file in the source file ( by
" 					one of \include, \includeonly, \input
" FindInputFile([bufname],[echo])

" ToDo: this function should have a mode to find input files recursively.
" a:1 	= file name 	(if not present the current buffer)
" a:2   = 0/1		(echo the results or not, default is to echo) 
" there is also a function atplib#Find
" This function is needed outside the script (atplib.vim)
function! FindInputFiles(...)    

    let l:echo=1
    let l:bufname 	= ( a:0 == 0 ? bufname("%") : a:1 )
    let l:echo 		= ( a:0 >= 2 ? a:2 : 1 )

    let l:dir=fnamemodify(l:bufname,":p:h")
    if buflisted(fnamemodify(l:bufname,":t"))
	let l:texfile=getbufline(fnamemodify(l:bufname,":t"),1,'$')
    else
	let l:texfile=readfile(fnamemodify(l:bufname,":p"))
    endif
    let b:texfile=l:texfile
    let s:i=0
    let l:inputlines=[]
    for l:line in l:texfile
	if l:line =~ "\\\\\\(input\\|include\\|includeonly\\)" && l:line !~ "^\s*%"
	    "add the line but cut it before first '%', thus we should get the
	    "file name.
	    let l:col=stridx(l:line,"%")
	    if l:col != -1
		let l:line=strpart(l:line,0,l:col)
	    endif
	    let l:inputlines=add(l:inputlines,l:line) 
	endif
    endfor

   " this is the dictionary that will be returned, its format is:
   " { input file name (as appear in tex file : [ input/include/bib, name of the main tex file ] }
    let l:inputfiles={}

    for l:line in l:inputlines
	if l:line !~ '{'
	    let l:inputfile=substitute(l:line,'\\\%(input\|include\|includeonly\)\s\+\(.*\)','\1','')
	    call extend(l:inputfiles, { l:inputfile : [ 'input' , fnamemodify(expand("%"),":p") ] } )
	else
	    let l:bidx=stridx(l:line,'{')
	    let l:eidx=len(l:line)-stridx(join(reverse(split(l:line,'\zs')),''),'}')-1
	    let l:inputfile=strpart(l:line,l:bidx+1,l:eidx-l:bidx-1)
	    call extend(l:inputfiles, { l:inputfile : [ 'include' , fnamemodify(expand("%"),":p") ] } )
	endif
    endfor
    call extend(l:inputfiles, FindBibFiles(l:bufname))
    " this function is used to set b:atp_MainFile, but at this stage there is no
    " need to add b:atp_MainFile to the list of input files (this is also
    " a requirement for the function s:setprojectname.
    if exists("b:atp_MainFile")
	call extend(l:inputfiles, { fnamemodify(b:atp_MainFile,":t") : ['main file', b:atp_MainFile]}, "error") 
    endif
    let l:inputfiless=deepcopy(l:inputfiles)
    call filter(l:inputfiless, 'v:key !~ fnamemodify(bufname("%"),":t:r")')
    if l:echo 
	if len(keys(l:inputfiless)) > 0 
	    echohl WarningMsg | echomsg "Found input files:" 
	else
	    echohl WarningMsg | echomsg "No input files found." | echohl None
	    return []
	endif
	echohl texInput
	let l:nr=1
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'main file'
		echomsg fnamemodify(l:inputfile,":t") 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'input'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'include'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'bib'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	echohl None
    endif
    let s:inputfiles=l:inputfiles
    return l:inputfiles
endfunction
command! -buffer -nargs=? -complete=buffer	FindInputFiles	:call FindInputFiles(<f-args>)
" }}}

" This function finds all the bibliography files declared in the source file.
" {{{ Find Bib Files 
" Returns a dictionary:
" { <input_name> : [ 'bib', 'main file', 'full path' ] }
"			 with the same format as the output of FindInputFiles
function! FindBibFiles(...)

    if a:0==0
	let l:bufname=bufname("%")
    else
	let l:bufname=a:1
    endif

"     let b:texfile=readfile(l:bufname)
    if buflisted(fnamemodify(l:bufname,":p"))
	let b:texfile=getbufline(l:bufname,1,'$')
    else
	let b:texfile=readfile(fnameescape(fnamemodify(l:bufname,":p")))
    endif
    let s:i=0
    let s:bibline=[]
    " find all lines which define bibliography files
    for line in b:texfile
	" ToDo: %\bibliography should not be matched!
	if line =~ "^[^%]*\\\\bibliography{"
	    let s:bibline=add(s:bibline,line) 
	    let s:i+=1
	endif
    endfor
    let l:nr=s:i
    let s:i=1
    let files=""
    " make a comma separated list of bibfiles
    for l:line in s:bibline
	if s:i==1
	    let files=substitute(l:line,"\\\\bibliography{\\(.*\\)}","\\1","") . ","
	else
	    let files=files . substitute(l:line,"\\\\bibliography{\\(.*\\)}","\\1","") . "," 
	endif
	let s:i+=1
    endfor

    " rewrite files into a vim list
    let l:allbibfiles=split(files,',')
    
    " add the list b:bibfiles 
    if exists('b:bibfiles')
	call extend(l:allbibfiles,b:bibfiles)
    endif
    
    " clear the list s:allbibfile from double entries 
    let l:callbibfiles=[]
    for l:f in l:allbibfiles
	if count(l:callbibfiles,l:f) == 0
	    call add(l:callbibfiles,l:f)
	endif
    endfor
    let l:allbibfiles=deepcopy(l:callbibfiles)
"     let b:abf=l:allbibfiles

    " this variable will store unreadable bibfiles:    
    let s:notreadablebibfiles=[]

    " this variable will store the final result:   
"     let l:bibfiles={}
    let l:bibfiles_dict={}
    let b:bibfiles_dict=l:bibfiles_dict

    " Make a list of all bib files which tex can find.
    let l:bibfiles_list=[]
    let b:bibfiles_list=l:bibfiles_list " DEBUG
    for l:dir in g:atp_bibinputs
	let l:bibfiles_list=extend(l:bibfiles_list,atplib#FindInputFilesInDir(l:dir,0,".bib"))
    endfor

    for l:f in l:allbibfiles
	" ToDo: change this to find in any directory under g:atp_bibinputs. 
	" also change in the line 1406 ( atplib#s:searchbib )
	for l:bibfile in l:bibfiles_list
	    if count(l:allbibfiles,fnamemodify(l:bibfile,":t:r"))
		if filereadable(l:bibfile) 
		call extend(l:bibfiles_dict, 
		    \ {fnamemodify(l:bibfile,":t:r") : [ 'bib' , fnamemodify(expand("%"),":p"), l:bibfile ] })
		else
		" echo warning if a bibfile is not readable
		    echohl WarningMsg | echomsg "Bibfile " . l:f . ".bib not found." | echohl None
		    if count(s:notreadablebibfiles,fnamemodify(l:f,":t:r")) == 0 
			call add(s:notreadablebibfiles,fnamemodify(l:f,":t:r"))
		    endif
		endif
	    endif
	endfor
    endfor

    " return the list  of readable bibfiles
    return l:bibfiles_dict
endfunction
"}}}

" All Status Line related things:
"{{{ Status Line
function! ATPStatusOutDir() "{{{
let status=""
if exists("b:atp_OutDir")
    if b:atp_OutDir != "" 
	let status= status . "Output dir: " . pathshorten(substitute(b:atp_OutDir,"\/\s*$","","")) 
    else
	let status= status . "Please set the Output directory, b:atp_OutDir"
    endif
endif	
    return status
endfunction "}}}

function! ATPRunning() "{{{
    if exists("b:atp_running") && exists("g:atp_callback") && b:atp_running && g:atp_callback
	redrawstatus
	if b:atp_running >= 2
	    return b:atp_running." ".b:atp_TexCompiler." "
	else
	    return b:atp_TexCompiler." "
	endif
    endif
    return ''
endfunction "}}}

" {{{ Syntax and Hilighting
syntax 	match 	atp_statustitle 	/.*/ 
syntax 	match 	atp_statussection 	/.*/ 
syntax 	match 	atp_statusoutdir 	/.*/ 
hi 	link 	atp_statustitle 	Number
hi 	link 	atp_statussection 	Title
hi 	link 	atp_statusoutdir 	String
" }}}

" The main status function, it is called via autocommand defined in 'options.vim'.
function! ATPStatus() "{{{
"     echomsg "Status line set by ATP." 
    if &filetype == 'tex'
	if g:atp_status_notification
	    let &statusline='%<%f %(%h%m%r %)  %{CTOC("return")}%= %{ATPRunning()} %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	else
	    let &statusline='%<%f %(%h%m%r %)  %{CTOC("return")}%= %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	endif 
    else 
	if g:atp_status_notification
	    let  &statusline='%<%f %(%h%m%r %)  %= %{ATPRunning()} %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	else
	    let  &statusline='%<%f %(%h%m%r %)  %= %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	endif
    endif
endfunction
command! -buffer ATPStatus		:call ATPStatus() 
" }}}
"}}}

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/compiler.vim	[[[1
794
" Author: 	Marcin Szamotulski	
" Note:		this file contain the main compiler function and related tools, to
" 		view the output, see error file.

" Some options (functions) should be set once:
let s:sourced	 		= exists("s:sourced") ? 1 : 0

" Internal Variables
" {{{
" This limits how many consecutive runs there can be maximally.
let s:runlimit		= 5 

let s:texinteraction	= "nonstopmode"
compiler tex
"}}}
"
" This is the function to view output. It calls compiler if the output is a not
" readable file.
" {{{ ViewOutput
function! s:ViewOutput()
    call atplib#outdir()

    " Set the correct output extension (if nothing matches set the default '.pdf')
    let l:ext	= get(g:atp_CompilersDict, b:atp_TexCompiler, ".pdf") 

    let l:link=system("readlink " . shellescape(b:atp_MainFile))
    if l:link != ""
	let l:outfile=fnamemodify(l:link,":r") . l:ext
    else
	let l:outfile=fnamemodify(b:atp_MainFile,":r"). l:ext 
    endif

    if b:atp_Viewer == "xpdf"	
	let l:viewer=b:atp_Viewer . " -remote " . shellescape(b:atp_XpdfServer) . " " . b:atp_ViewerOptions 
    else
	let l:viewer=b:atp_Viewer  . " " . b:atp_ViewerOptions
    endif

    let l:view=l:viewer . " " . shellescape(l:outfile)  . " &"

    if filereadable(l:outfile)
	if b:atp_Viewer == "xpdf"	
	    call system(l:view)
	else
	    call system(l:view)
	    redraw!
	endif
    else
	echomsg "Output file do not exists. Calling " . b:atp_TexCompiler
	call s:compiler(0,1,1,0,"AU",b:atp_MainFile)
    endif	
endfunction

noremap <silent> <Plug>ATP_ViewOutput	:call <SID>ViewOutput()<CR>
command! -buffer ViewOutput		:call <SID>ViewOutput()
"}}}

" This function gets the pid of the running compiler
" ToDo: review LatexBox has a better approach!
"{{{ Get PID Functions
function! s:getpid()
	let s:command="ps -ef | grep -v " . $SHELL  . " | grep " . b:atp_TexCompiler . " | grep -v grep | grep " . fnameescape(expand("%")) . " | awk 'BEGIN {ORS=\" \"} {print $2}'" 
	let s:var=system(s:command)
	return s:var
endfunction
function! s:GetPID()
	let s:var=s:getpid()
	if s:var != ""
		echomsg b:atp_TexCompiler . " pid " . s:var 
	else
		echomsg b:atp_TexCompiler . " is not running"
	endif
endfunction
command! -buffer PID		:call <SID>GetPID()
"}}}

" To check if xpdf is running we use 'ps' unix program.
"{{{ s:xpdfpid
if !exists("*s:xpdfpid")
function! s:xpdfpid() 
    let s:checkxpdf="ps -ef | grep -v grep | grep xpdf | grep '-remote '" . shellescape(b:atp_XpdfServer) . " | awk '{print $2}'"
    return substitute(system(s:checkxpdf),'\D','','')
endfunction
endif
"}}}

" This function compares two files: file written on the disk a:file and the current
" buffer
"{{{ s:compare
" relevant variables:
" g:atp_compare_embedded_comments
" g:atp_compare_double_empty_lines
function! s:compare(file)
    let l:buffer=getbufline(bufname("%"),"1","$")

    " rewrite l:buffer to remove all commands 
    let l:buffer=filter(l:buffer, 'v:val !~ "^\s*%"')

    let l:i = 0
    if g:atp_compare_double_empty_lines == 0 || g:atp_compare_embedded_comments == 0
    while l:i < len(l:buffer)-1
	let l:rem=0
	" remove comment lines at the end of a line
	if g:atp_compare_embedded_comments == 0
	    let l:buffer[l:i] = substitute(l:buffer[l:i],'%.*$','','')
	endif

	" remove double empty lines (i.e. from two conecutive empty lines
	" the first one is deleted, the second remains), if the line was
	" removed we do not need to add 1 to l:i (this is the role of
	" l:rem).
	if g:atp_compare_double_empty_lines == 0 && l:i< len(l:buffer)-2
	    if l:buffer[l:i] =~ '^\s*$' && l:buffer[l:i+1] =~ '^\s*$'
		call remove(l:buffer,l:i)
		let l:rem=1
	    endif
	endif
	if l:rem == 0
	    let l:i+=1
	endif
    endwhile
    endif
 
    " do the same with a:file
    let l:file=filter(a:file, 'v:val !~ "^\s*%"')

    let l:i = 0
    if g:atp_compare_double_empty_lines == 0 || g:atp_compare_embedded_comments == 0
    while l:i < len(l:file)-1
	let l:rem=0
	" remove comment lines at the end of a line
	if g:atp_compare_embedded_comments == 0
	    let l:file[l:i] = substitute(a:file[l:i],'%.*$','','')
	endif
	
	" remove double empty lines (i.e. from two conecutive empty lines
	" the first one is deleted, the second remains), if the line was
	" removed we do not need to add 1 to l:i (this is the role of
	" l:rem).
	if g:atp_compare_double_empty_lines == 0 && l:i < len(l:file)-2
	    if l:file[l:i] =~ '^\s*$' && l:file[l:i+1] =~ '^\s*$'
		call remove(l:file,l:i)
		let l:rem=1
	    endif
	endif
	if l:rem == 0
	    let l:i+=1
	endif
    endwhile
    endif

    return l:file !=# l:buffer
endfunction
"}}}

" This function copies the file a:input to a:output
"{{{ s:copy
function! s:copy(input,output)
	call writefile(readfile(a:input),a:output)
endfunction
"}}}

" This is the CALL BACK mechanism 
" (with the help of David Munger - LatexBox) 
"{{{ call back
function! s:GetSid() "{{{
    return matchstr(expand('<sfile>'), '\zs<SNR>\d\+_\ze.*$')
endfunction 
let s:SID = s:GetSid() "}}}

let g:atp_sid={ fnamemodify(expand('<sfile>'),':t') : s:SID }

function! s:SidWrap(func) "{{{
    return s:SID . a:func
endfunction "}}}

" CatchStatus {{{
function! s:CatchStatus(status)
	let b:atp_TexStatus=a:status
endfunction
" }}}

" Callback {{{
" a:mode 	= a:verbose 	of s:compiler ( one of 'default', 'silent',
" 				'debug', 'verbose')
" a:commnad	= a:commmand 	of s:compiler 
"		 		( a:commnad = 'AU' if run from background)
"
" Uses b:atp_TexStatus which is equal to the value returned by tex
" compiler.
function! s:CallBack(mode)
    let b:mode=a:mode

	let b:atp_running-=1

	" Read the log file
	cg

	" redraw the status line /for the notification to appear as fast as
	" possible/ 
	if a:mode != 'verbose'
	    redrawstatus
	endif

	if b:atp_TexStatus && t:atp_DebugMode != "silent"
	    if b:atp_ReloadOnError
		echomsg b:atp_TexCompiler." exited with status " . b:atp_TexStatus
	    else
		echomsg b:atp_TexCompiler." exited with status " . b:atp_TexStatus . " output file not reloaded"
	    endif
	elseif !g:atp_status_notification || !g:atp_statusline
	    echomsg b:atp_TexCompiler." finished"
	endif

	" End the debug mode if there are no errors
	if b:atp_TexStatus == 0 && t:atp_DebugMode == "debug"
	    cclose
	    echomsg b:atp_TexCompiler." finished with status " . b:atp_TexStatus . " going out of debuging mode."
	    let t:atp_DebugMode == g:atp_DefaultDebugMode
	endif

	if t:atp_DebugMode == "debug" || a:mode == "debug"
	    if !t:atp_QuickFixOpen
		ShowErrors
	    endif
	    " In debug mode, go to first error. 
	    if t:atp_DebugMode == "debug"
		cc
	    endif
	endif
endfunction
"}}}
"}}}

" THE MAIN COMPILER FUNCTION
" {{{ s:compiler 
" This is the MAIN FUNCTION which sets the command and calls it.
" NOTE: the <filename> argument is not escaped!
" make a:verbose=verbose or debug or 0
" 	debug 	-- switch to show errors after compilation.
" 	verbose -- show compiling procedure.
" 	0 	-- compile silently (gives status information if fails)
function! s:compiler(bibtex, start, runs, verbose, command, filename)

    if !has('gui') && a:verbose == 'verbose' && b:atp_running > 0
	redraw!
	echomsg "Please wait until compilation stops."
	return
    endif

    if has('clientserver') && !empty(v:servername) && g:atp_callback && a:verbose != 'verbose'
	let b:atp_running+=1
    endif
    call atplib#outdir()
    	" IF b:atp_TexCompiler is not compatible with the viewer
	" ToDo: (move this in a better place). (luatex can produce both pdf and dvi
	" files according to options so this is not the right approach.) 
	if t:atp_DebugMode != "silent" && b:atp_TexCompiler !~ "luatex" &&
		    \ (b:atp_TexCompiler =~ "^\s*\%(pdf\|xetex\)" && b:atp_Viewer == "xdvi" ? 1 :  
		    \ b:atp_TexCompiler !~ "^\s*pdf" && b:atp_TexCompiler !~ "xetex" &&  (b:atp_Viewer == "xpdf" || b:atp_Viewer == "epdfview" || b:atp_Viewer == "acroread" || b:atp_Viewer == "kpdf"))
	     
	    echohl WaningMsg | echomsg "Your ".b:atp_TexCompiler." and ".b:atp_Viewer." are not compatible:" 
	    echomsg "b:atp_TexCompiler=" . b:atp_TexCompiler	
	    echomsg "b:atp_Viewer=" . b:atp_Viewer	
	endif

	" there is no need to run more than s:runlimit (=5) consecutive runs
	" this prevents from running tex as many times as the current line
	" what can be done by a mistake using the range for the command.
	if a:runs > s:runlimit
	    let l:runs = s:runlimit
	else
	    let l:runs = a:runs
	endif

	let s:tmpdir=tempname()
	let s:tmpfile=atplib#append(s:tmpdir,"/") . fnamemodify(a:filename,":t:r")
	if exists("*mkdir")
	    call mkdir(s:tmpdir, "p", 0700)
	else
	    echoerr 'Your vim doesn't have mkdir function, there is a workaround this though. 
			\ Send an email to the author: mszamot@gmail.com '
	endif

	" SET THE NAME OF OUTPUT FILES
	" first set the extension pdf/dvi
	let l:ext	= get(g:atp_CompilersDict, b:atp_TexCompiler, '.pdf')

	" check if the file is a symbolic link, if it is then use the target
	" name.
	let l:link=system("readlink " . a:filename)
	if l:link != ""
	    let l:basename=fnamemodify(l:link,":r")
	else
	    let l:basename=a:filename
	endif

	" finally, set the output file names. 
	let l:outfile = b:atp_OutDir . fnamemodify(l:basename,":t:r") . l:ext
	let l:outaux  = b:atp_OutDir . fnamemodify(l:basename,":t:r") . ".aux"
	let l:outlog  = b:atp_OutDir . fnamemodify(l:basename,":t:r") . ".log"

"	COPY IMPORTANT FILES TO TEMP DIRECTORY WITH CORRECT NAME 
	let l:list=filter(copy(g:keep),'v:val != "log"')
	for l:i in l:list
	    let l:ftc=b:atp_OutDir . fnamemodify(l:basename,":t:r") . "." . l:i
	    if filereadable(l:ftc)
		call s:copy(l:ftc,s:tmpfile . "." . l:i)
	    endif
	endfor

" 	HANDLE XPDF RELOAD 
	if b:atp_Viewer == "xpdf"
	    if a:start == 1
		"if xpdf is not running and we want to run it.
		let s:xpdfreload = b:atp_Viewer . " -remote " . shellescape(b:atp_XpdfServer) . " " . shellescape(l:outfile)
	    else
" TIME: this take 1/3 of time! 0.039
		if s:xpdfpid() != ""
		    "if xpdf is running (then we want to reload it).
		    "This is where I use 'ps' command to check if xpdf is
		    "running.
		    let s:xpdfreload = b:atp_Viewer . " -remote " . shellescape(b:atp_XpdfServer) . " -reload"	
		else
		    "if xpdf is not running (but we do not want
		    "to run it).
		    let s:xpdfreload = ""
		endif
	    endif
	else
	    if a:start == 1
		" if b:atp_Viewer is not running and we want to open it.
		let s:xpdfreload = b:atp_Viewer . " " . shellescape(l:outfile) 
	    else
		" if b:atp_Viewer is not running then we do not want to
		" open it.
		let s:xpdfreload = ""
	    endif	
	endif

" 	IF OPENING NON EXISTING OUTPUT FILE
"	only xpdf needs to be run before (we are going to reload it)
	if a:start == 1 && b:atp_Viewer == "xpdf"
	    let s:start = b:atp_Viewer . " -remote " . shellescape(b:atp_XpdfServer) . " " . b:atp_ViewerOptions . " & "
	else
	    let s:start = ""	
	endif

"	SET THE COMMAND 
	let s:comp	= b:atp_TexCompiler . " " . b:atp_TexOptions . " -interaction " . s:texinteraction . " -output-directory " . s:tmpdir . " " . fnameescape(a:filename)
	let s:vcomp	= b:atp_TexCompiler . " " . b:atp_TexOptions  . " -interaction errorstopmode -output-directory " . s:tmpdir .  " " . fnameescape(a:filename)
	if a:verbose == 'verbose' 
	    let s:texcomp=s:vcomp
	else
	    let s:texcomp=s:comp
	endif
	if l:runs >= 2 && a:bibtex != 1
	    " how many times we want to call b:atp_TexCompiler
	    let l:i=1
	    while l:i < l:runs - 1
		let l:i+=1
		let s:texcomp=s:texcomp . " ; " . s:comp
	    endwhile
	    if a:verbose != 'verbose'
		let s:texcomp=s:texcomp . " ; " . s:comp
	    else
		let s:texcomp=s:texcomp . " ; " . s:vcomp
	    endif
	endif
	
	if a:bibtex == 1
	    " this should be decided using the log file as well.
	    if filereadable(l:outaux)
		call s:copy(l:outaux,s:tmpfile . ".aux")
		let s:texcomp="bibtex " . s:tmpfile . ".aux ; " . s:comp . "  1>/dev/null 2>&1 "
	    else
		let s:texcomp=s:comp . " ; clear ; bibtex " . s:tmpfile . ".aux ; " . s:comp . " 1>/dev/null 2>&1 "
	    endif
	    if a:verbose != 'verbose'
		let s:texcomp=s:texcomp . " ; " . s:comp
	    else
		let s:texcomp=s:texcomp . " ; " . s:vcomp
	    endif
	endif

	" catch the status
	if has('clientserver') && v:servername != "" && g:atp_callback == 1

	    let catchstatus = s:SidWrap('CatchStatus')
	    let catchstatus_cmd = 'vim ' . ' --servername ' . v:servername . ' --remote-expr ' . 
			\ shellescape(catchstatus)  . '\($?\) ; ' 

	else
	    let catchstatus_cmd = ''
	endif

	" copy output file (.pdf\|.ps\|.dvi)
	let s:cpoption="--remove-destination "
	let s:cpoutfile="cp " . s:cpoption . shellescape(atplib#append(s:tmpdir,"/")) . "*" . l:ext . " " . shellescape(atplib#append(b:atp_OutDir,"/")) . " ; "

	if a:start
	    let s:command="(" . s:texcomp . " ; (" . catchstatus_cmd . " " . s:cpoutfile . " " . s:xpdfreload . " ) || ( ". catchstatus_cmd . " " . s:cpoutfile . ") ; " 
	else
	    " 	Reload on Error:
	    " 	for xpdf it copies the out file but does not reload the xpdf
	    " 	server for other viewers it simply doesn't copy the out file.
	    if exists("b:atp_ReloadOnError") && b:atp_ReloadOnError
		let s:command="(" . s:texcomp . " ; " . catchstatus_cmd . " " . s:cpoutfile . " " . s:xpdfreload 
	    else
		if b:atp_Viewer =~ '\<xpdf\>'
		    let s:command="(" . s:texcomp . " && (" . catchstatus_cmd . s:cpoutfile . " " . s:xpdfreload . ") || (" . catchstatus_cmd . " " . s:cpoutfile . ") ; " 
		else
		    let s:command="(" . s:texcomp . " && (" . catchstatus_cmd . s:cpoutfile . " " . s:xpdfreload . ") || (" . catchstatus_cmd . ") ; " 
		endif
	    endif
	endif

	" Preserve files with extension belonging to the g:keep list variable.
	let s:copy=""
	let l:j=1
	for l:i in g:keep 
" ToDo: this can be done using internal vim functions.
	    let s:copycmd=" cp " . s:cpoption . " " . shellescape(atplib#append(s:tmpdir,"/")) . 
			\ "*." . l:i . " " . shellescape(atplib#append(b:atp_OutDir,"/"))  
	    if l:j == 1
		let s:copy=s:copycmd
	    else
		let s:copy=s:copy . " ; " . s:copycmd	  
	    endif
	    let l:j+=1
	endfor
	let s:command=s:command . " " . s:copy . " ; "

	if has('clientserver') && v:servername != "" && g:atp_callback == 1

	    let callback=s:SidWrap('CallBack')
	    let callback_cmd = ' vim ' . ' --servername ' . v:servername . ' --remote-expr ' . 
				    \ shellescape(callback).'\(\"'.a:verbose.'\"\)'. " ; "

	    let s:command = s:command . " " . callback_cmd

	endif

 	let s:rmtmp="rm -r " . shellescape(s:tmpdir)
	let s:command=s:command . " " . s:rmtmp . ")&"

	if a:start == 1 
	    let s:command=s:start . s:command
	endif


	" Take care about backup and writebackup options.
	let s:backup=&backup
	let s:writebackup=&writebackup
	if a:command == "AU"  
	    if &backup || &writebackup | setlocal nobackup | setlocal nowritebackup | endif
	endif
" This takes lots of time! 0.049s (more than 1/3)	
	silent! w
	if a:command == "AU"  
	    let &l:backup=s:backup 
	    let &l:writebackup=s:writebackup 
	endif

	if a:verbose != 'verbose'
	    call system(s:command)
	else
	    let s:command="!clear;" . s:texcomp . " " . s:cpoutfile . " " . s:copy 
	    exe s:command
	endif
	let b:texcommand=s:command
endfunction
"}}}

" AUTOMATIC TEX PROCESSING 
" {{{1 s:auTeX
" To Do: we can now check if the last environment is closed and run latex if it is, to
" not run latex if the 
function! s:auTeX()
    let mode 	= ( g:atp_DefaultDebugMode == 'verbose' ? 'debug' : g:atp_DefaultDebugMode )

    if !b:atp_autex
       return "autex is off"
    endif
    " if the file (or input file is modified) compile the document 
    if filereadable(expand("%"))
	if s:compare(readfile(expand("%")))
	    call s:compiler(0,0,b:atp_auruns, mode, "AU",b:atp_MainFile)
	    redraw
	    return "compile" 
	endif
    " if compiling for the first time
    else
	call s:compiler(0,0,b:atp_auruns, mode, "AU",b:atp_MainFile)
	w
	redraw
	return "compile for the first time"
    endif
    return "files does not differ"
endfunction

if !s:sourced
    au CursorHold *.tex call s:auTeX()
endif
"}}}

" Related Functions
" {{{ TeX

" a:runs	= how many consecutive runs
" a:1		= one of 'default','silent', 'debug', 'verbose'
" 		  if not specified uses 'default' mode
" 		  (g:atp_DefaultDebugMode).
function! s:TeX(runs, ...)
let s:name=tempname()

    if a:0 >= 1
	let mode = ( a:1 != 'default' ? a:1 : g:atp_DefaultDebugMode )
    else
	let mode = g:atp_DefaultDebugMode
    endif

    if l:mode != 'silent'
	if a:runs > 2 && a:runs <= 5
	    echomsg b:atp_TexCompiler . " will run " . a:1 . " times."
	elseif a:runs == 2
	    echomsg b:atp_TexCompiler . " will run twice."
	elseif a:runs == 1
	    echomsg b:atp_TexCompiler . " will run once."
	elseif a:runs > 5
	    echomsg b:atp_TexCompiler . " will run " . s:runlimit . " times."
	endif
    endif
    call s:compiler(0,0, a:runs, mode, "COM", b:atp_MainFile)
endfunction
command! -buffer -nargs=? -count=1 TEX		:call <SID>TeX(<count>, <f-args>)
noremap <silent> <Plug>ATP_TeXCurrent		:<C-U>call <SID>TeX(v:count1, t:atp_DebugMode)<CR>
noremap <silent> <Plug>ATP_TeXDefault		:<C-U>call <SID>TeX(v:count1, 'default')<CR>
noremap <silent> <Plug>ATP_TeXSilent		:<C-U>call <SID>TeX(v:count1, 'silent')<CR>
noremap <silent> <Plug>ATP_TeXDebug		:<C-U>call <SID>TeX(v:count1, 'debug')<CR>
noremap <silent> <Plug>ATP_TeXVerbose		:<C-U>call <SID>TeX(v:count1, 'verbose')<CR>
inoremap <silent> <Plug>iATP_TeXVerbose		<Esc>:<C-U>call <SID>TeX(v:count1, 'verbose')<CR>
"}}}
"{{{ Bibtex
function! s:SimpleBibtex()
    let bibcommand 	= "bibtex "
    let auxfile		= b:atp_OutDir . (fnamemodify(expand("%"),":t:r")) . ".aux"
    if filereadable(auxfile)
	let command	= bibcommand . shellescape(l:auxfile)
	echo system(command)
    else
	echomsg "No aux file in " . b:atp_OutDir
    endif
endfunction
command! -buffer SBibtex	:call <SID>SimpleBibtex()
nnoremap <silent> <Plug>SimpleBibtex	:call <SID>SimpleBibtex()<CR>

function! s:Bibtex(...)
    if a:0 >= 1
	let mode = ( a:1 != 'default' ? a:1 : g:atp_DefaultDebugMode )
    else
	let mode = g:atp_DefaultDebugMode
    endif

    call s:compiler(1,0,0, mode,"COM",b:atp_MainFile)
endfunction
command! -buffer -nargs=? Bibtex	:call <SID>Bibtex(<f-args>)
nnoremap <silent> <Plug>BibtexDefault	:call <SID>Bibtex("")<CR>
nnoremap <silent> <Plug>BibtexSilent	:call <SID>Bibtex("silent")<CR>
nnoremap <silent> <Plug>BibtexDebug	:call <SID>Bibtex("debug")<CR>
nnoremap <silent> <Plug>BibtexVerbose	:call <SID>Bibtex("verbose")<CR>
"}}}

" Show Errors Function
" {{{ SHOW ERRORS
"
" this functions sets errorformat according to the flag given in the argument,
" possible flags:
" e	- errors (or empty flag)
" w	- all warning messages
" c	- citation warning messages
" r	- reference warning messages
" f	- font warning messages
" fi	- font warning and info messages
" F	- files
" p	- package info messages

" {{{ s:SetErrorFormat
" first argument is a word in flags 
" the default is a:1=e /show only error messages/
function! s:SetErrorFormat(...)
    if a:0 > 0
	let b:arg1=a:1
	if a:0 > 1
	    let b:arg1.=" ".a:2
	endif
    endif
    let &l:errorformat=""
    if a:0 == 0 || a:0 > 0 && a:1 =~ 'e'
	if &l:errorformat == ""
	    let &l:errorformat= "%E!\ LaTeX\ %trror:\ %m,\%E!\ %m"
	else
	    let &l:errorformat= &l:errorformat . ",%E!\ LaTeX\ %trror:\ %m,\%E!\ %m"
	endif
    endif
    if a:0>0 &&  a:1 =~ 'w'
	if &l:errorformat == ""
	    let &l:errorformat='%WLaTeX\ %tarning:\ %m\ on\ input\ line\ %l%.,
			\%WLaTeX\ %.%#Warning:\ %m,
	    		\%Z(Font) %m\ on\ input\ line\ %l%.,
			\%+W%.%#\ at\ lines\ %l--%*\\d'
	else
	    let &l:errorformat= &l:errorformat . ',%WLaTeX\ %tarning:\ %m\ on\ input\ line\ %l%.,
			\%WLaTeX\ %.%#Warning:\ %m,
	    		\%Z(Font) %m\ on\ input\ line\ %l%.,
			\%+W%.%#\ at\ lines\ %l--%*\\d'
" 	    let &l:errorformat= &l:errorformat . ',%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,
" 			\%WLaTeX\ %.%#Warning:\ %m,
" 			\%+W%.%#\ at\ lines\ %l--%*\\d'
	endif
    endif
    if a:0>0 && a:1 =~ '\Cc'
" NOTE:
" I would like to include 'Reference/Citation' as an error message (into %m)
" but not include the 'LaTeX Warning:'. I don't see how to do that actually. 
" The only solution, that I'm aware of, is to include the whole line using
" '%+W' but then the error messages are long and thus not readable.
	if &l:errorformat == ""
	    let &l:errorformat = "%WLaTeX\ Warning:\ Citation\ %m\ on\ input\ line\ %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%WLaTeX\ Warning:\ Citation\ %m\ on\ input\ line\ %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ '\Cr'
	if &l:errorformat == ""
	    let &l:errorformat = "%WLaTeX\ Warning:\ Reference %m on\ input\ line\ %l%.%#,%WLaTeX\ %.%#Warning:\ Reference %m,%C %m on input line %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%WLaTeX\ Warning:\ Reference %m on\ input\ line\ %l%.%#,%WLaTeX\ %.%#Warning:\ Reference %m,%C %m on input line %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ '\Cf'
	if &l:errorformat == ""
	    let &l:errorformat = "%WLaTeX\ Font\ Warning:\ %m,%Z(Font) %m on input line %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%WLaTeX\ Font\ Warning:\ %m,%Z(Font) %m on input line %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ '\Cfi'
	if &l:errorformat == ""
	    let &l:errorformat = '%ILatex\ Font\ Info:\ %m on input line %l%.%#,
			\%ILatex\ Font\ Info:\ %m,
			\%Z(Font) %m\ on input line %l%.%#,
			\%C\ %m on input line %l%.%#'
	else
	    let &l:errorformat = &l:errorformat . ',%ILatex\ Font\ Info:\ %m on input line %l%.%#,
			\%ILatex\ Font\ Info:\ %m,
			\%Z(Font) %m\ on input line %l%.%#,
			\%C\ %m on input line %l%.%#'
	endif
    endif
    if a:0>0 && a:1 =~ '\CF'
	if &l:errorformat == ""
	    let &l:errorformat = 'File: %m'
	else
	    let &l:errorformat = &l:errorformat . ',File: %m'
	endif
    endif
    if a:0>0 && a:1 =~ '\Cp'
	if &l:errorformat == ""
	    let &l:errorformat = 'Package: %m'
	else
	    let &l:errorformat = &l:errorformat . ',Package: %m'
	endif
    endif
    if &l:errorformat != ""

	let pm = ( g:atp_show_all_lines == 1 ? '+' : '-' )

	let l:dont_ignore = 0
	if a:0 >= 1 && a:1 =~ '\cALL'
	    let l:dont_ignore = 1
	    let pm = '+'
	endif
	let b:dont_ignore=l:dont_ignore.a:0

	let &l:errorformat = &l:errorformat.",
		    	    \%Cl.%l\ %m,
			    \%".pm."C\ \ %m%.%#,
			    \%".pm."C%.%#-%.%#,
			    \%".pm."C%.%#[]%.%#,
			    \%".pm."C[]%.%#,
			    \%".pm."C%.%#%[{}\\]%.%#,
			    \%".pm."C<%.%#>%.%#,
			    \%".pm."C%m,
			    \%".pm."GSee\ the\ LaTeX%m,
			    \%".pm."GType\ \ H\ <return>%m,
			    \%".pm."G%.%#\ (C)\ %.%#,
			    \%".pm."G(see\ the\ transcript%.%#),
			    \%-G\\s%#"
	if (g:atp_ignore_unmatched && !g:atp_show_all_lines)
	    exec 'setlocal efm+=%-G%.%#' 
	elseif l:dont_ignore
	    exec 'setlocal efm+=%-G%.%#' 
	endif
	let &l:errorformat = &l:errorformat.",
			    \%".pm."O(%*[^()])%r,
			    \%".pm."O%*[^()](%*[^()])%r,
			    \%".pm."P(%f%r,
			    \%".pm."P\ %\\=(%f%r,
			    \%".pm."P%*[^()](%f%r,
			    \%".pm."P[%\\d%[^()]%#(%f%r"
	if g:atp_ignore_unmatched && !g:atp_show_all_lines
	    exec 'setlocal efm+=%-P%*[^()]' 
	elseif l:dont_ignore
	    exec 'setlocal efm+=%-P%*[^()]' 
	endif
	let &l:errorformat = &l:errorformat.",
			    \%".pm."Q)%r,
			    \%".pm."Q%*[^()])%r,
			    \%".pm."Q[%\\d%*[^()])%r"
	if g:atp_ignore_unmatched && !g:atp_show_all_lines
	    let &l:errorformat = &l:errorformat.",%-Q%*[^()]"
	elseif l:dont_ignore
	    let &l:errorformat = &l:errorformat.",%-Q%*[^()]"
	endif

" 			    removed after GType
" 			    \%-G\ ...%.%#,
    endif
endfunction
command! -buffer -nargs=? 	SetErrorFormat 	:call <SID>SetErrorFormat(<f-args>)
"}}}
"{{{ s:ShowErrors
" each argument can be a word in flags as for s:SetErrorFormat (except the
" word 'whole') + two other flags: all (include all errors) and ALL (include
" all errors and don't ignore any line - this overrides the variables
" g:atp_ignore_unmatched and g:atp_show_all_lines.
function! s:ShowErrors(...)

    " read the log file and merge warning lines 
    if !filereadable(&errorfile)
	echohl WarningMsg
	echomsg "No error file: " . &errorfile  
	echohl Normal
	return
    endif

    let l:log=readfile(&errorfile)

    let l:nr=1
    for l:line in l:log
	if l:line =~ "LaTeX Warning:" && l:log[l:nr] !~ "^$" 
	    let l:newline=l:line . l:log[l:nr]
	    let l:log[l:nr-1]=l:newline
	    call remove(l:log,l:nr)
	endif
	let l:nr+=1
    endfor
    call writefile(l:log,&errorfile)
    
    " set errorformat 
    let l:arg = ( a:0 > 0 ? a:1 : "e" )
    if l:arg =~ 'o'
	OpenLog
	return
    endif
    call s:SetErrorFormat(l:arg)

    let l:show_message = ( a:0 >= 2 ? a:2 : 1 )

    " read the log file
    cg

    " final stuff
    if len(getqflist()) == 0 
	if l:show_message
	    echomsg "no errors"
	endif
	return ":)"
    else
	cl
	return 1
    endif
endfunction
command! -buffer -nargs=? -complete=custom,ListErrorsFlags 	ShowErrors 	:call <SID>ShowErrors(<f-args>)
"}}}
if !exists("*ListErrorsFlags")
function! ListErrorsFlags(A,L,P)
	return "e\nw\nc\nr\ncr\nf\nfi\nall\nF"
endfunction
endif
"}}}

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/mappings.vim	[[[1
306
" Author:	Marcin Szmotulski

" Commands to library functions (autoload/atplib.vim)
command! -buffer -nargs=* FontSearch	:call atplib#FontSearch(<f-args>)
command! -buffer -nargs=* FontPreview	:call atplib#FontPreview(<f-args>)
command! -buffer -nargs=1 -complete=customlist,atplib#Fd_completion OpenFdFile	:call atplib#OpenFdFile(<f-args>) 
command! -buffer -nargs=* CloseLastEnvironment	:call atplib#CloseLastEnvironment(<f-args>)
command! -buffer 	  CloseLastBracket	:call atplib#CloseLastBracket()


" Add maps, unless the user didn't want this.
" ToDo: to doc.
if !exists("no_plugin_maps") && !exists("no_atp_maps")
    " ToDo to doc. + vmaps!
    map  <buffer> <LocalLeader>ns 	<Plug>GoToNextSection<CR>
"     vmap <buffer> <LocalLeader>ns 	<Plug>GoToNextSection<CR>
"     omap <buffer> <LocalLeader>ns 	<Plug>GoToNextSection<CR>
    map  <buffer> <LocalLeader>ps 	<Plug>GoToPreviousSection<CR>
"     vmap <buffer> <LocalLeader>ps 	<Plug>GoToPreviousSection<CR>
"     omap <buffer> <LocalLeader>ps 	<Plug>GoToPreviousSection<CR>
    map  <buffer> <LocalLeader>nc 	<Plug>GoToNextChapter<CR>
"     vmap <buffer> <LocalLeader>nc 	<Plug>GoToNextChapter<CR>
"     omap <buffer> <LocalLeader>nc 	<Plug>GoToNextChapter<CR>
    map  <buffer> <LocalLeader>pc 	<Plug>GoToPreviousChapter<CR>
"     vmap <buffer> <LocalLeader>pc 	<Plug>GoToPreviousChapter<CR>
"     omap <buffer> <LocalLeader>pc 	<Plug>GoToPreviousChapter<CR>
    map  <buffer> <LocalLeader>np 	<Plug>GoToNextPart<CR>
"     vmap <buffer> <LocalLeader>np 	<Plug>GoToNextPart<CR>
"     omap <buffer> <LocalLeader>np 	<Plug>GoToNextPart<CR>
    map  <buffer> <LocalLeader>pp 	<Plug>GoToPreviousPart<CR>
"     vmap <buffer> <LocalLeader>pp 	<Plug>GoToPreviousPart<CR>
"     omap <buffer> <LocalLeader>pp 	<Plug>GoToPreviousPart<CR>
    " ToDo to doc.
    if exists("g:atp_no_tab_map") && g:atp_no_tab_map == 1
	imap <buffer> <F7> 		<C-R>=atplib#TabCompletion(1)<CR>
	nmap <buffer> <F7>		:call atplib#TabCompletion(1,1)<CR>
	imap <buffer> <S-F7> 		<C-R>=atplib#TabCompletion(0)<CR>
	nmap <buffer> <S-F7>		:call atplib#TabCompletion(0,1)<CR> 
    else 
	" the default:
	imap <buffer> <Tab> 		<C-R>=atplib#TabCompletion(1)<CR>
	imap <buffer> <S-Tab> 		<C-R>=atplib#TabCompletion(0)<CR>
	" HOW TO: do this with <tab>? Streightforward solution interacts with
	" other maps (e.g. after \l this map is called).
	" when this is set it also runs after the \l map: ?!?
" 	nmap <buffer> <Tab>		:call atplib#TabCompletion(1,1)<CR>
	nmap <buffer> <S-Tab>		:call atplib#TabCompletion(0,1)<CR> 
	vmap <buffer> <silent> <F7> 	:WrapSelection '\{','}','begin'<CR>
    endif

"     nmap <buffer> <F7>c 	:call atplib#CloseLastEnvironment()<CR>
"     imap <buffer> <F7>c 	<Esc>:call atplib#CloseLastEnvironment()<CR>i
"     nmap <buffer> <F7>b 	:call atplib#CloseLastBracket()<CR>
"     imap <buffer> <F7>b 	<Esc>:call atplib#CloseLastBracket()<CR>i

    " Fonts
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."f		:WrapSelection '{\\usefont{".g:atp_font_encoding."}{}{}{}\\selectfont ', '}', '".(len(g:atp_font_encoding)+11)."'<CR>"

    if !exists("g:atp_vmap_text_font_leader")
	let g:atp_vmap_text_font_leader="<LocalLeader>"
    else
	let g:debug=1
    endif

    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."rm	:<C-U>InteligentWrapSelection ['\\textrm{'],['\\mathrm{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."em	:<C-U>InteligentWrapSelection ['\\emph{'],['\\mathit{']<CR>"
"   Suggested maps:
"     execute "vmap <buffer> ".g:atp_vmap_text_font_leader."tx	:<C-U>InteligentWrapSelection [''],['\\text{']<CR>"
"     execute "vmap <buffer> ".g:atp_vmap_text_font_leader."in	:<C-U>InteligentWrapSelection [''],['\\intertext{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."it	:<C-U>InteligentWrapSelection ['\\textit{'],['\\mathit{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."sf	:<C-U>InteligentWrapSelection ['\\textsf{'],['\\mathsf{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."tt	:<C-U>InteligentWrapSelection ['\\texttt{'],['\\mathtt{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."bf	:<C-U>InteligentWrapSelection ['\\textbf{'],['\\mathbf{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."bb	:<C-U>InteligentWrapSelection ['\\textbf{'],['\\mathbb{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."sl	:<C-U>WrapSelection '\\textsl{'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."sc	:<C-U>WrapSelection '\\textsc{'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."up	:<C-U>WrapSelection '\\textup{'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."md	:<C-U>WrapSelection '\\textmd{'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."n	:<C-U>InteligentWrapSelection ['\\textnormal{'],['\\mathnormal{']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_text_font_leader."cal	:<C-U>InteligentWrapSelection [''],['\\mathcal{']<CR>"

    " Environments
    if !exists("atp_vmap_environment_leader")
	let g:atp_vmap_environment_leader=""
    endif
    execute "vmap <buffer> ".g:atp_vmap_environment_leader."C   :WrapSelection '"."\\"."begin{center}','"."\\"."end{center}','0','1'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_environment_leader."R   :WrapSelection '"."\\"."begin{flushright}','"."\\"."end{flushright}','0','1'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_environment_leader."L   :WrapSelection '"."\\"."begin{flushleft}','"."\\"."end{flushleft}','0','1'<CR>"

    " Math modes
    vmap <buffer> m						:WrapSelection '\(', '\)'<CR>
    vmap <buffer> M						:WrapSelection '\[', '\]'<CR>

    " Brackets
    if !exists("*atp_vmap_bracket_leader")
	let g:atp_vmap_bracket_leader="<LocalLeader>"
    endif
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader."( 	:WrapSelection '(', ')', 'begin'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader."[ 	:WrapSelection '[', ']', 'begin'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader."\\{ 	:WrapSelection '\\{', '\\}', 'begin'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader."{ 	:WrapSelection '{', '}', 'begin'<CR>"
"     execute "vmap <buffer> ".g:atp_vmap_bracket_leader."{	:<C-U>InteligentWrapSelection ['{', '}'],['\\{', '\\}']<CR>"
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader.")	:WrapSelection '(', ')', 'end'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader."]	:WrapSelection '[', ']', 'end'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader."\\}	:WrapSelection '\\{', '\\}', 'end'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_bracket_leader."}	:WrapSelection '{', '}', 'end'<CR>"

    if !exists("*atp_vmap_big_bracket_leader")
	let g:atp_vmap_big_bracket_leader='<LocalLeader>b'
    endif
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader."(	:WrapSelection '\\left(', '\\right)', 'begin'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader."[	:WrapSelection '\\left[', '\\right]', 'begin'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader."{	:WrapSelection '\\left\\{','\\right\\}', 'begin'<CR>"
    " for compatibility:
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader."\\{	:WrapSelection '\\left\\{','\\right\\}', 'begin'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader.")	:WrapSelection '\\left(', '\\right)', 'end'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader."]	:WrapSelection '\\left[', '\\right]', 'end'<CR>"
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader."}	:WrapSelection '\\left\\{', '\\right\\}', 'end'<CR>"
    " for compatibility:
    execute "vmap <buffer> ".g:atp_vmap_big_bracket_leader."\\}	:WrapSelection '\\left\\{', '\\right\\}', 'end'<CR>"

"     vmap i)		<Esc><Plug>SelectInnerBracket<CR>
"     vmap a)		<Esc><Plug>SelectAuterBracket<CR>
"     vmap I)		<Esc><Plug>SelectINNERBracket<CR>
"     vmap A)		<Esc><Plug>SelectAUTERBracket<CR>

    nmap <buffer> <LocalLeader>E		<Plug>Echo<CR>
    " Normal mode maps (mostly)
    nmap  <buffer> <LocalLeader>v		<Plug>ATP_ViewOutput<CR>
    nmap  <buffer> <F2> 			<Plug>ToggleSpace<CR>
    nmap  <buffer> <LocalLeader>s		<Plug>ToggleStar<CR>
    " Todo: to doc:
    nmap  <buffer> <LocalLeader>D		<Plug>ToggleDebugMode<CR>
    nmap  <buffer> <F4>				<Plug>ToggleEnvForward<CR>
    nmap  <buffer> <S-F4>			<Plug>ToggleEnvBackward<CR>
    nmap  <buffer> <C-S-F4>			<Plug>LatexEnvPrompt<CR>
"     ToDo:
"     if g:atp_LatexBox
" 	nmap  <buffer> <F3>			:call <Sid>ChangeEnv()<CR>
"     endif
    nmap  <buffer> <F3>        			<Plug>ViewOutput<CR>
    imap  <buffer> <F3> 			<Esc><Plug>ViewOutput<CR>
    nmap  <buffer> <LocalLeader>g 		<Plug>Getpid<CR>
    nmap  <buffer> <LocalLeader>t		<Plug>ATP_TOC<CR>
    nmap  <buffer> <LocalLeader>L		<Plug>ATP_Labels<CR>
    nmap  <buffer> <LocalLeader>l 		<Plug>ATP_TeXCurrent<CR>
    nmap  <buffer> <LocalLeader>d 		<Plug>ATP_TeXDebug<CR>
    "ToDo: imaps!
    nmap  <buffer> <F5> 			<Plug>ATP_TeXVerbose<CR>
    nmap  <buffer> <s-F5> 			<Plug>ToggleAuTeX<CR>
    nmap  <buffer> `<Tab>			<Plug>ToggleTab<CR>
    imap  <buffer> `<Tab>			<Esc><Plug>ToggleTab<CR>i
    nmap  <buffer> <LocalLeader>B		<Plug>SimpleBibtex<CR>
    nmap  <buffer> <LocalLeader>b		<Plug>BibtexDefault<CR>
    nmap  <buffer> <F6>d 			<Plug>Delete
    imap  <buffer> <silent> <F6>l 		<Plug>OpenLog<CR>
    nmap  <buffer> <silent> <F6>l 		<Plug>OpenLog<CR>
    nmap  <buffer> <LocalLeader>e 		:cf<CR> 
    nmap  <buffer> <F6> 			:ShowErrors e<CR>
    imap  <buffer> <F6>e 			:ShowErrors e<CR>
    nmap  <buffer> <F6>w 			:ShowErrors w<CR>
    imap  <buffer> <F6>w 			:ShowErrors w<CR>
    nmap  <buffer> <F6>r 			:ShowErrors rc<CR>
    nmap  <buffer> <F6>r 			:ShowErrors rc<CR>
    nmap  <buffer> <F6>f 			:ShowErrors f<CR>
    imap  <buffer> <F6>f 			:ShowErrors f<CR>
    nmap  <buffer> <F6>g 			<Plug>PdfFonts<CR>
    nmap  <buffer> <F1>				:TeXdoc<space>
    imap  <buffer> <F1> <esc> 			:TeXdoc<space>
    nmap  <buffer> <LocalLeader>p 		<Plug>SshPrint<CR>

    " FONT MAPPINGS
    execute 'imap <buffer> '.g:atp_imap_second_leader.'rm \textrm{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'up \textup{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'md \textmd{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'it \textit{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'sl \textsl{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'sc \textsc{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'sf \textsf{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'bf \textbf{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'tt \texttt{}<Left>'
	    
    execute 'imap <buffer>' .g:atp_imap_second_leader.'mit \mathit{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'mrm \mathrm{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'msf \mathsf{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'mbf \mathbf{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'mtt \mathtt{}<Left>'
    execute 'imap <buffer>' .g:atp_imap_second_leader.'mcal \mathcal{}<Left>'

    " GREEK LETTERS
    execute 'imap <buffer> '.g:atp_imap_first_leader.'a \alpha'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'b \beta'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'c \chi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'d \delta'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'e \epsilon'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'f \phi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'y \psi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'g \gamma'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'h \eta'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'k \kappa'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'l \lambda'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'i \iota'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'m \mu'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'n \nu'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'p \pi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'o \theta'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'r \rho'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'s \sigma'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'t \tau'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'u \upsilon'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'vs \varsigma'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'vo \vartheta'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'w \omega'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'x \xi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'z \zeta'

    execute 'imap <buffer> '.g:atp_imap_first_leader.'D \Delta'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'Y \Psi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'F \Phi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'G \Gamma'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'L \Lambda'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'M \Mu'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'N \Nu'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'P \Pi'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'O \Theta'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'S \Sigma'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'T \Tau'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'U \Upsilon'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'V \Varsigma'
    execute 'imap <buffer> '.g:atp_imap_first_leader.'W \Omega'

    if g:atp_no_env_maps != 1
	if g:atp_env_maps_old == 1
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'b \begin{}<Left>'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'e \end{}<Left>'

	    execute 'imap <buffer> '.g:atp_imap_third_leader.'c \begin{center}<CR>\end{center}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_fourth_leader.'c \begin{corollary}<CR>\end{corollary}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'d \begin{definition}<CR>\end{definition}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_fourth_leader.'e \begin{enumerate}<CR>\end{enumerate}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'a \begin{align}<CR>\end{align}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'i \item'
	    execute 'imap <buffer> '.g:atp_imap_fourth_leader.'i \begin{itemize}<CR>\end{itemize}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'l \begin{lemma}<CR>\end{lemma}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_fourth_leader.'p \begin{proof}<CR>\end{proof}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'p \begin{proposition}<CR>\end{proposition}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'t \begin{theorem}<CR>\end{theorem}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_fourth_leader.'t \begin{center}<CR>\begin{tikzpicture}<CR><CR>\end{tikzpicture}<CR>\end{center}<Up><Up>'

	    if g:atp_extra_env_maps == 1
		execute 'imap <buffer> '.g:atp_imap_third_leader.'r \begin{remark}<CR>\end{remark}<Esc>O'
		execute 'imap <buffer> '.g:atp_imap_fourth_leader.'l \begin{flushleft}<CR>\end{flushleft}<Esc>O'
		execute 'imap <buffer> '.g:atp_imap_third_leader.'r \begin{flushright}<CR>\end{flushright}<Esc>O'
		execute 'imap <buffer> '.g:atp_imap_third_leader.'f \begin{frame}<CR>\end{frame}<Esc>O'
		execute 'imap <buffer> '.g:atp_imap_fourth_leader.'q \begin{equation}<CR>\end{equation}<Esc>O'
		execute 'imap <buffer> '.g:atp_imap_third_leader.'n \begin{note}<CR>\end{note}<Esc>O'
		execute 'imap <buffer> '.g:atp_imap_third_leader.'o \begin{observation}<CR>\end{observation}<Esc>O'
		execute 'imap <buffer> '.g:atp_imap_third_leader.'x \begin{example}<CR>\end{example}<Esc>O'
	    endif
	else
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'b \begin{}<Left>'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'e \end{}<Left>'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'c \begin{center}<CR>\end{center}<Esc>O'

	    execute 'imap <buffer> '.g:atp_imap_third_leader.'d \begin{definition}<CR>\end{definition}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'t \begin{theorem}<CR>\end{theorem}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'P \begin{proposition}<CR>\end{proposition}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'l \begin{lemma}<CR>\end{lemma}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'r \begin{remark}<CR>\end{remark}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'o \begin{corollary}<CR>\end{corollary}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'p \begin{proof}<CR>\end{proof}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'x \begin{example}<CR>\end{example}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'n \begin{note}<CR>\end{note}<Esc>O'

	    execute 'imap <buffer> '.g:atp_imap_third_leader.'u \begin{enumerate}<CR>\end{enumerate}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'i \begin{itemize}<CR>\end{itemize}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'I \item'


	    execute 'imap <buffer> '.g:atp_imap_third_leader.'a \begin{align}<CR>\end{align}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'q \begin{equation}<CR>\end{equation}<Esc>O'

	    execute 'imap <buffer> '.g:atp_imap_third_leader.'l \begin{flushleft}<CR>\end{flushleft}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'R \begin{flushright}<CR>\end{flushright}<Esc>O'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'z \begin{center}<CR>\begin{tikzpicture}<CR><CR>\end{tikzpicture}<CR>\end{center}<Up><Up>'
	    execute 'imap <buffer> '.g:atp_imap_third_leader.'f \begin{frame}<CR>\end{frame}<Esc>O'
	endif

	" imap {c \begin{corollary*}<CR>\end{corollary*}<Esc>O
	" imap {d \begin{definition*}<CR>\end{definition*}<Esc>O
	" imap {x \begin{example*}\normalfont<CR>\end{example*}<Esc>O
	" imap {l \begin{lemma*}<CR>\end{lemma*}<Esc>O
	" imap {n \begin{note*}<CR>\end{note*}<Esc>O
	" imap {o \begin{observation*}<CR>\end{observation*}<Esc>O
	" imap {p \begin{proposition*}<CR>\end{proposition*}<Esc>O
	" imap {r \begin{remark*}<CR>\end{remark*}<Esc>O
	" imap {t \begin{theorem*}<CR>\end{theorem*}<Esc>O
    endif

    imap <buffer> __ _{}<Left>
    imap <buffer> ^^ ^{}<Left>
    imap <buffer> ]m \(\)<Left><Left>
    imap <buffer> ]M \[\]<Left><Left>
endif

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/menu.vim	[[[1
101
"Author:		Marcin Szamotulski
" This file sets up the menu.


if !exists("no_plugin_menu") && !exists("no_atp_menu")
nmenu 550.10 &LaTeX.&Make<Tab>:TEX						:TEX<CR>
nmenu 550.10 &LaTeX.Make\ &twice<Tab>:2TEX					:2TEX<CR>
nmenu 550.10 &LaTeX.Make\ verbose<Tab>:VTEX					:VTEX<CR>
nmenu 550.10 &LaTeX.&Bibtex<Tab>:Bibtex						:Bibtex<CR>
" nmenu 550.10 &LaTeX.&Bibtex\ (bibtex)<Tab>:SBibtex				:SBibtex<CR>
nmenu 550.10 &LaTeX.&View<Tab>:ViewOutput 					:ViewOutput<CR>
"
nmenu 550.20.1 &LaTeX.&Errors<Tab>:ShowErrors					:ShowErrors<CR>
nmenu 550.20.1 &LaTeX.&Log.&Open\ Log\ File<Tab>:map\ <F6>l			:OpenLog<CR>
if t:atp_DebugMode == "debug"
    nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [on]			:ToggleDebugMode<CR>
else
    nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [off]			:ToggleDebugMode<CR>
endif  
nmenu 550.20.20 &LaTeX.&Log.-ShowErrors-	:
nmenu 550.20.20 &LaTeX.&Log.&Warnings<Tab>:ShowErrors\ w 			:ShowErrors w<CR>
nmenu 550.20.20 &LaTeX.&Log.&Citation\ Warnings<Tab>:ShowErrors\ c		:ShowErrors c<CR>
nmenu 550.20.20 &LaTeX.&Log.&Reference\ Warnings<Tab>:ShowErrors\ r		:ShowErrors r<CR>
nmenu 550.20.20 &LaTeX.&Log.&Font\ Warnings<Tab>ShowErrors\ f			:ShowErrors f<CR>
nmenu 550.20.20 &LaTeX.&Log.Font\ Warnings\ &&\ Info<Tab>:ShowErrors\ fi	:ShowErrors fi<CR>
nmenu 550.20.20 &LaTeX.&Log.&Show\ Files<Tab>:ShowErrors\ F			:ShowErrors F<CR>
"
nmenu 550.20.20 &LaTeX.&Log.-PdfFotns- :
nmenu 550.20.20 &LaTeX.&Log.&Pdf\ Fonts<Tab>:PdfFonts				:PdfFonts<CR>

nmenu 550.20.20 &LaTeX.&Log.-Delete-	:
nmenu 550.20.20 &LaTeX.&Log.&Delete\ Tex\ Output\ Files<Tab>:map\ <F6>d		:call Delete()<CR>
nmenu 550.20.20 &LaTeX.&Log.Set\ Error\ File<Tab>:SetErrorFile			:SetErrorFile<CR> 
"
nmenu 550.30 &LaTeX.-TOC- :
nmenu 550.30 &LaTeX.&Table\ of\ Contents<Tab>:TOC				:TOC<CR>
nmenu 550.30 &LaTeX.L&abels<Tab>:Labels						:Labels<CR>
"
nmenu 550.40 &LaTeX.&Go\ to.&EditInputFile<Tab>:EditInputFile			:EditInputFile<CR>
"
nmenu 550.40 &LaTeX.&Go\ to.-Environment- :
nmenu 550.40 &LaTeX.&Go\ to.Next\ Definition<Tab>:NEnv\ definition		:NEnv definition<CR>
nmenu 550.40 &LaTeX.&Go\ to.Previuos\ Definition<Tab>:PEnv\ definition		:PEnv definition<CR>
nmenu 550.40 &LaTeX.&Go\ to.Next\ Environment<Tab>:NEnv\ <arg>			:NEnv 
nmenu 550.40 &LaTeX.&Go\ to.Previuos\ Environment<Tab>:PEnv\ <arg>		:PEnv 
"
nmenu 550.40 &LaTeX.&Go\ to.-Section- :
nmenu 550.40 &LaTeX.&Go\ to.&Next\ Section<Tab>:NSec				:NSec<CR>
nmenu 550.40 &LaTeX.&Go\ to.&Previuos\ Section<Tab>:PSec			:PSec<CR>
nmenu 550.40 &LaTeX.&Go\ to.Next\ Chapter<Tab>:NChap				:NChap<CR>
nmenu 550.40 &LaTeX.&Go\ to.Previous\ Chapter<Tab>:PChap			:PChap<CR>
nmenu 550.40 &LaTeX.&Go\ to.Next\ Part<Tab>:NPart				:NPart<CR>
nmenu 550.40 &LaTeX.&Go\ to.Previuos\ Part<Tab>:PPart				:PPart<CR>
"
nmenu 550.50 &LaTeX.-Bib-			:
nmenu 550.50 &LaTeX.Bib\ Search<Tab>:Bibsearch\ <arg>				:BibSearch 
nmenu 550.50 &LaTeX.Find\ Bib\ Files<Tab>:FindBibFiles				:FindBibFiles<CR> 
nmenu 550.50 &LaTeX.Find\ Input\ Files<Tab>:FindInputFiles			:FindInputFiles<CR>
"
nmenu 550.60 &LaTeX.-Viewer-			:
nmenu 550.60 &LaTeX.Set\ &XPdf<Tab>:SetXpdf					:SetXpdf<CR>
nmenu 550.60 &LaTeX.Set\ X&Dvi\ (inverse\/reverse\ search)<Tab>:SetXdvi		:SetXdvi<CR>
"
nmenu 550.70 &LaTeX.-Editting-			:
"
" ToDo: show options doesn't work from the menu (it disappears immediately, but at
" some point I might change it completely)
nmenu 550.70 &LaTeX.&Options.&Show\ Options<Tab>:ShowOptions			:ShowOptions<CR> 
if g:atp_callback
    nmenu 550.70 &LaTeX.&Options.Toggle\ &Call\ Back\ [on]<Tab>g:atp_callback		:ToggleCallBack<CR>
else
    nmenu 550.70 &LaTeX.&Options.Toggle\ &Call\ Back\ [off]<Tab>g:atp_callback		:ToggleCallBack<CR>
endif  
nmenu 550.70 &LaTeX.&Options.-set\ options- :
nmenu 550.70 &LaTeX.&Options.Automatic\ TeX\ Processing<Tab>b:atp_autex		:let b:atp_autex=
nmenu 550.70 &LaTeX.&Options.Set\ Runs<Tab>b:atp_auruns				:let b:atp_auruns=
nmenu 550.70 &LaTeX.&Options.Set\ TeX\ Compiler<Tab>b:atp_TexCompiler		:let b:atp_TexCompiler="
nmenu 550.70 &LaTeX.&Options.Set\ Viewer<Tab>b:atp_Viewer				:let b:atp_Viewer="
nmenu 550.70 &LaTeX.&Options.Set\ Viewer\ Options<Tab>b:atp_ViewerOptions		:let b:atp_ViewerOptions="
nmenu 550.70 &LaTeX.&Options.Set\ Output\ Directory<Tab>b:atp_OutDir		:let b:atp_ViewerOptions="
nmenu 550.70 &LaTeX.&Options.Set\ Output\ Directory\ to\ the\ default\ value<Tab>:SetOutDir	:SetOutDir<CR> 
nmenu 550.70 &LaTeX.&Options.Ask\ for\ the\ Output\ Directory<Tab>g:askfortheoutdir		:let g:askfortheoutdir="
nmenu 550.70 &LaTeX.&Options.Open\ Viewer<Tab>b:atp_OpenViewer			:let b:atp_OpenViewer="
nmenu 550.70 &LaTeX.&Options.Open\ Viewer<Tab>b:atp_OpenViewer			:let b:atp_OpenViewer="
nmenu 550.70 &LaTeX.&Options.Set\ Error\ File<Tab>:SetErrorFile			:SetErrorFile<CR> 
nmenu 550.70 &LaTeX.&Options.Which\ TeX\ files\ to\ copy<Tab>g:keep		:let g:keep="
nmenu 550.70 &LaTeX.&Options.Tex\ extensions<Tab>g:atp_tex_extensions		:let g:atp_tex_extensions="
nmenu 550.70 &LaTeX.&Options.Remove\ Command<Tab>g:rmcommand			:let g:rmcommand="
nmenu 550.70 &LaTeX.&Options.Default\ Bib\ Flags<Tab>g:defaultbibflags		:let g:defaultbibflags="
"
nmenu 550.78 &LaTeX.&Toggle\ Space\ [off]<Tab>cmap\ <space>\ \\_s\\+ 	:ToggleSpace<CR>
if g:atp_math_opened
    nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [on]<Tab>g:atp_math_opened  :ToggleCheckMathOpened<CR>
else
    nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [off]<Tab>g:atp_math_opened :ToggleCheckMathOpened<CR>
endif
tmenu &LaTeX.&Toggle\ Space\ [off] cmap <space> \_s\+ is curently off
" ToDo: add menu for printing.
endif

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/motion.vim	[[[1
1027
" Author:	Marcin Szamotulski
" This file contains motion and highlight functions of ATP.

" All table  of contents stuff: variables, functions and commands. 
" {{{ Table Of Contents
let g:atp_sections={
    \	'chapter' 	: [           '^\s*\(\\chapter\*\?\s*{\)',	'\\chapter\*'],	
    \	'section' 	: [           '^\s*\(\\section\*\?\s*{\)',	'\\section\*'],
    \ 	'subsection' 	: [	   '^\s*\(\\subsection\*\?\s*{\)',	'\\subsection\*'],
    \	'subsubsection' : [ 	'^\s*\(\\subsubsection\*\?\s*{\)',	'\\subsubsection\*'],
    \	'bibliography' 	: ['^\s*\(\\begin\s*{bibliography}\|\\bibliography\s*{\)' , 'nopattern'],
    \	'abstract' 	: ['^\s*\(\\begin\s*{abstract}\|\\abstract\s*{\)',	'nopattern']}

"     \   'part'		: [ 		 '^\s*\(\\part.*\)',	'\\part\*'],

"--Make TOC -----------------------------
" This makes sense only for latex documents.
"
" It makes the t:atp_toc - a dictionary (with keys: full path of the buffer name)
" which values are dictionaries which keys are: line numbers and values lists:
" [ 'section-name', 'number', 'title'] where section name is element of
" keys(g:atp_sections), number is the total number, 'title=\1' where \1 is
" returned by the g:section['key'][0] pattern.
" {{{ maketoc_fromaux
" function! s:maketoc_fromaux(filename)
"     let l:toc={}
"     " if the dictinary with labels is not defined, define it
"     if !exists("t:atp_labels")
" 	let t:atp_labels={}
"     endif
"     " TODO we could check if there are changes in the file and copy the buffer
"     " to this variable only if there where changes.
"     let l:auxfile=[]
"     " getbufline reads only loaded buffers, unloaded can be read from file.
"     let l:auxfname=fnamemodify(a:filename,":r").'.aux'
"     if filereadable(l:auxfname)
" 	let l:auxfile=readfile(l:auxfname)
"     else
" 	echohl WarningMsg
" 	echomsg "No aux file, run ".b:atp_TexCompiler."."
" 	echohl Normal
"     endif
"     call filter(l:auxfile,' v:val =~ "\\\\@writefile{toc}"') 
" "     while l:line in l:auxfile
"     for l:line in l:auxfile
" 	let l:sec_unit=matchstr(l:line,'\\contentsline\s*{\zs[^}]*\ze}')
" 	let l:sec_number=matchstr(l:line,'\\numberline\s*{\zs[^}]*\ze}')
" " 	echomsg l:sec_number
" 	" To Do: how to match the long_title
" 	let l:long_title=matchstr(l:line,'\\contentsline\s*{[^}]*}{\%(\\numberline\s*{[^}]*}\)\?\s*\zs.*') 
" 	if l:long_title =~ '\\GenericError'
" 	    let l:long_title=substitute(l:long_title,'\\GenericError\s*{[^}]*}{[^}]*}{[^}]*}{[^}]*}','','g')
" 	endif
" 	if l:long_title =~ '\\relax\s'
" 	    let l:long_title=substitute(l:long_title,'\\relax\s','','g')
" 	endif	   
" 	if l:long_title =~ '\\unhbox '
" 	    let l:long_title=substitute(l:long_title,'\\unhbox\s','','g')
" 	endif	   
" 	if l:long_title =~ '\\nobreakspace'
" 	    let l:long_title=substitute(l:long_title,'\\nobreakspace\s*{}',' ','g')
" 	endif	   
" 	let l:i=0
" 	let l:braces=0
" 	while l:braces >= 0 && l:i < len(l:long_title)
" 	    if l:long_title[l:i] == '{'
" 		let l:braces+=1
" 	    elseif l:long_title[l:i] == '}'
" 		let l:braces-=1
" 	    endif
" 	    let l:i+=1
" 	endwhile
" " 	echo "len " len(l:long_title) . " l:i" . l:i
" 	let l:long_title=strpart(l:long_title,0,l:i-1)
" 
" " 	let l:star_c=matchstr(l:line,'\\contentsline\s*{[^}]*}{\%(\\numberline\s*{[^}]*}\)\?\s*\%([^}]*\|{[^}]*}\)*}{\zs[^}]*\ze}')
" 	let l:star_version= l:line =~ l:sec_unit.'\*'
" 
" 	" find line number in the current tex file (this is not the best!) we
" 	" should check if we are in mainfile.
" 	let l:line_nr=search('\\'.l:sec_unit.'.*'.l:long_title,'n')
" 	let l:tex_line=getline(l:line_nr)
" " 	echomsg "tex_line=".l:tex_line."sec_unit=".l:sec_unit." sec_number=".l:sec_number." l:long_title=".l:long_title." star=".l:star_version 
" 	" find short title
" 	let l:short_title=l:line
" 	let l:start=stridx(l:short_title,'[')+1
" 	if l:start == 0
" 	    let l:short_title=''
" 	else
" 	    let l:short_title=strpart(l:short_title,l:start)
" 	    " we are looking for the maching ']' 
" 	    let l:count=1
" 	    let l:i=-1
" 	    while l:i<=len(l:short_title)
" 		let l:i+=1
" 		if strpart(l:short_title,l:i,1) == '['	
" 		    let l:count+=1
" 		elseif strpart(l:short_title,l:i,1) == ']'
" 		    let l:count-=1
" 		endif
" 		if l:count==0
" 		    break
" 		endif
" 	    endwhile	
" 	    let l:short_title=strpart(l:short_title,0,l:i)
" 	endif
" 	call extend(l:toc, { l:line_nr : [l:sec_unit, l:sec_number, l:long_title, l:star_version, l:short_title] }) 
"     endfor
"     let t:atp_toc_new={ a:filename : l:toc }
"     return t:atp_toc_new
" endfunction
" }}}2
" {{{2 s:find_toc_lines
function! s:find_toc_lines()
    let l:toc_lines_nr=[]
    let l:toc_lines=[]
    let b:toc_lines_nr=l:toc_lines_nr

    let l:pos_saved=getpos(".")
    let l:pos=[0,1,1,0]
    keepjumps call setpos(".",l:pos)

    " Pattern:
    let l:j=0
    for l:section in keys(g:atp_sections)
	if l:j == 0 
	    let l:filter=g:atp_sections[l:section][0] . ''
	else
	    let l:filter=l:filter . '\|' . g:atp_sections[l:section][0] 
	endif
	let l:j+=1
    endfor
"     let b:filter=l:filter

    " Searching Loop:
    let l:line=search(l:filter,'W')
    while l:line
	call add(l:toc_lines_nr,l:line)
	let l:line=search(l:filter,'W')
    endwhile
    keepjumps call setpos(".",l:pos_saved)
    for l:line in l:toc_lines_nr
	call add(l:toc_lines,getline(l:line))
    endfor
    return l:toc_lines
endfunction
" }}}2
" {{{2 s:maketoc 
function! s:maketoc(filename)
    
    " this will store information { 'linenumber' : ['chapter/section/..', 'sectionnumber', 'section title', '0/1=not starred/starred'] }
    let l:toc={}

    " if the dictinary with labels is not defined, define it
    if !exists("t:atp_labels")
	let t:atp_labels={}
    endif
    " TODO we could check if there are changes in the file and copy the buffer
    " to this variable only if there where changes.
    let l:texfile=[]
    " getbufline reads only loaded buffers, unloaded can be read from file.
    let l:bufname=fnamemodify(a:filename,":t")
    let b:bufname=l:bufname
    if bufloaded(l:bufname)
	let l:texfile=getbufline("^" . l:bufname . "$","1","$")
    else
" 	w
	let l:texfile=readfile(a:filename)
    endif
    let l:true=1
    let l:i=0
    " remove the part before \begin{document}
    while l:true == 1 && len(l:texfile)>0
	if l:texfile[0] =~ '\\begin\s*{document}'
		let l:true=0
	endif
	call remove(l:texfile,0)
	let l:i+=1
    endwhile
    let l:bline=l:i
    let l:i=1
    " set variables for chapter/section numbers
    for l:section in keys(g:atp_sections)
	let l:ind{l:section}=0
    endfor
    " make a filter
    let l:j=0
    for l:section in keys(g:atp_sections)
	if l:j == 0 
	    let l:filter=g:atp_sections[l:section][0] . ''
	else
	    let l:filter=l:filter . '\|' . g:atp_sections[l:section][0] 
	endif
	let l:j+=1
    endfor
    let b:filter=l:filter " DEBUG
    " ToDo: HOW TO MAKE THIS FAST?
    let s:filtered=filter(deepcopy(l:texfile),'v:val =~ l:filter')
    let b:filtered=s:filtered
    let b:texfile=l:texfile
" this works but only for one file:
"     let s:filtered=s:find_toc_lines()
    for l:line in s:filtered
	for l:section in keys(g:atp_sections)
	    if l:line =~ g:atp_sections[l:section][0] 
		if l:line !~ '^\s*%'
		    " THIS DO NOT WORKS WITH \abstract{ --> empty set, but with
		    " \chapter{title} --> title, solution: the name of
		    " 'Abstract' will be plased, as we know what we have
		    " matched
		    let l:title=l:line

		    " test if it is a starred version.
		    let l:star=0
		    if g:atp_sections[l:section][1] != 'nopattern' && l:line =~ g:atp_sections[l:section][1] 
			let l:star=1 
		    else
			let l:star=0
		    endif
		    let l:i=index(l:texfile,l:line)
		    let l:tline=l:i+l:bline+1

		    " Find Title:
		    let l:start=stridx(l:title,'{')+1
		    let l:title=strpart(l:title,l:start)
		    " we are looking for the maching '}' 
		    let l:count=1
		    let l:i=-1
		    while l:i<=len(l:title)
			let l:i+=1
			if strpart(l:title,l:i,1) == '{'	
			    let l:count+=1
			elseif strpart(l:title,l:i,1) == '}'
			    let l:count-=1
			endif
			if l:count==0
			    break
			endif
		    endwhile	
		    let l:title=strpart(l:title,0,l:i)

		    " Section Number:
		    " if it is not starred version add one to the section number
		    " or it is not an abstract 
		    if l:star == 0  
			if !(l:section == 'chapter' && l:title =~ '^\cabstract$')
			    let l:ind{l:section}+=1
" 			else
" 			    echomsg "XXXXXXXXX" l:section . " " . l:title . "  " . l:ind{l:section}
			endif
		    endif

		    if l:section == 'part'
			let l:indchapter=0
			let l:indsection=0
			let l:indsubsection=0
			let l:indsubsubsection=0
		    elseif l:section ==  'chapter'
			let l:indsection=0
			let l:indsubsection=0
			let l:indsubsubsection=0
		    elseif l:section ==  'section'
			let l:indsubsection=0
			let l:indsubsubsection=0
		    elseif l:section ==  'subsection'
			let l:indsubsubsection=0
		    endif

		    " Find Short Title:
		    let l:shorttitle=l:line
		    let l:start=stridx(l:shorttitle,'[')+1
		    if l:start == 0
			let l:shorttitle=''
		    else
			let l:shorttitle=strpart(l:shorttitle,l:start)
			" we are looking for the maching ']' 
			let l:count=1
			let l:i=-1
			while l:i<=len(l:shorttitle)
			    let l:i+=1
			    if strpart(l:shorttitle,l:i,1) == '['	
				let l:count+=1
			    elseif strpart(l:shorttitle,l:i,1) == ']'
				let l:count-=1
			    endif
			    if l:count==0
				break
			    endif
			endwhile	
			let l:shorttitle=strpart(l:shorttitle,0,l:i)
		    endif
		    call extend(l:toc, { l:tline : [ l:section, l:ind{l:section}, l:title, l:star, l:shorttitle] }) 

		    " Extend t:atp_labels
		    let l:lname=matchstr(l:line,'\\label\s*{.*','')
		    let l:start=stridx(l:lname,'{')+1
		    let l:lname=strpart(l:lname,l:start)
		    let l:end=stridx(l:lname,'}')
		    let l:lname=strpart(l:lname,0,l:end)
" 		    let b:lname=l:lname
		    if	l:lname != ''
			" if there was no t:atp_labels for a:filename make an entry in
			" t:atp_labels
			if !has_key(t:atp_labels,a:filename)
			    let t:atp_labels[a:filename] = {}
			endif
			call extend(t:atp_labels[a:filename],{ l:tline : l:lname },"force")
		    endif
		endif
	    endif
	endfor
    endfor
    if exists("t:atp_toc")
	call extend(t:atp_toc, { a:filename : l:toc },"force")
    else
	let t:atp_toc={ a:filename : l:toc }
    endif
    return t:atp_toc
endfunction
" }}}2
" {{{2 Make a List of Buffers
if !exists("t:buflist")
    let t:buflist=[]
endif
function! s:buflist()
    " this names are used in TOC and passed to s:maketoc, which
    " makes a dictionary whose keys are the values of l:name defined
    " just below:
    let l:name=resolve(fnamemodify(bufname("%"),":p"))
    " add an entry to the list t:buflist if it is not there.
    if bufname("") =~ ".tex" && index(t:buflist,l:name) == -1
	call add(t:buflist,l:name)
    endif
    return t:buflist
endfunction
call s:buflist()
" }}}2
" {{{2 RemoveFromBufList
if !exists("*RemoveFromBufList")
    function RemoveFromBufList()
	let l:i=1
	for l:f in t:buflist
	    echo "(" . l:i . ") " . l:f
	    let l:i+=1
	endfor
	let l:which=input("Which file to remove (press <Enter> for none)")
	if l:which != "" && l:which =~ '\d\+'
	    call remove(t:buflist,l:f-1)
	endif
    endfunction
endif
" }}}2
" {{{2 s:showtoc
function! s:showtoc(toc,...)
    let l:new=0
    if a:0 == 1
	let l:new=a:1
    endif
    " this is a dictionary of line numbers where a new file begins.
    let l:cline=line(".")
"     " Open new window or jump to the existing one.
"     " Remember the place from which we are coming:
"     let t:atp_bufname=bufname("")
"     let t:atp_winnr=winnr()	 these are already set by TOC()
    let l:bname="__ToC__"
    let l:tocwinnr=bufwinnr("^" . l:bname . "$") 
"     echomsg "DEBUG a " . l:tocwinnr
    if l:tocwinnr != -1
	" Jump to the existing window.
	    exe l:tocwinnr . " wincmd w"
	    silent exe "%delete"
    else
	" Open new window if its width is defined (if it is not the code below
	" will put toc in the current buffer so it is better to return.
	if !exists("t:toc_window_width")
	    echoerr "t:toc_window_width not set"
	    return
	endif
	let l:openbuffer=t:toc_window_width . "vsplit +setl\\ wiw=15\\ buftype=nofile\\ filetype=toc_atp\\ nowrap __ToC__"
	silent exe l:openbuffer
	" We are setting the address from which we have come.
	silent call atplib#setwindow()
    endif
    setlocal tabstop=4
    let l:number=1
    " this is the line number in ToC.
    " l:number is a line number relative to the file listed in ToC.
    " the current line number is l:linenumber+l:number
    " there are two loops: one over l:linenumber and the second over l:number.
    let l:numberdict={}
    " this variable will be used to set the cursor position in ToC.
    for l:openfile in keys(a:toc)
	call extend(l:numberdict,{ l:openfile : l:number })
	let l:part_on=0
	let l:chap_on=0
	let l:chnr=0
	let l:secnr=0
	let l:ssecnr=0
	let l:sssecnr=0
	let l:path=fnamemodify(bufname(""),":p:h")
	for l:line in keys(a:toc[l:openfile])
	    if a:toc[l:openfile][l:line][0] == 'chapter'
		let l:chap_on=1
		break
	    elseif a:toc[l:openfile][l:line][0] == 'part'
		let l:part_on=1
	    endif
	endfor
	let l:sorted=sort(keys(a:toc[l:openfile]),"atplib#CompareList")
	let l:len=len(l:sorted)
	" write the file name in ToC (with a full path in paranthesis)
	call setline(l:number,fnamemodify(l:openfile,":t") . " (" . fnamemodify(l:openfile,":p:h") . ")")
	let l:number+=1
	for l:line in l:sorted
	    let l:lineidx=index(l:sorted,l:line)
	    let l:nlineidx=l:lineidx+1
	    if l:nlineidx< len(l:sorted)
		let l:nline=l:sorted[l:nlineidx]
	    else
		let l:nline=line("$")
	    endif
	    let l:lenght=len(l:line) 	
	    if l:lenght == 0
		let l:showline="     "
	    elseif l:lenght == 1
		let l:showline="    " . l:line
	    elseif l:lenght == 2
		let l:showline="   " . l:line
	    elseif l:lenght == 3
		let l:showline="  " . l:line
	    elseif l:lenght == 4
		let l:showline=" " . l:line
	    elseif l:lenght>=5
		let l:showline=l:line
	    endif
	    " Print ToC lines.
	    if a:toc[l:openfile][l:line][0] == 'abstract' || a:toc[l:openfile][l:line][2] =~ '^\cabstract$'
		call setline(l:number, l:showline . "\t" . "  " . "Abstract" )
	    elseif a:toc[l:openfile][l:line][0] =~ 'bibliography\|references'
		call setline (l:number, l:showline . "\t" . "  " . a:toc[l:openfile][l:line][2])
	    elseif a:toc[l:openfile][l:line][0] == 'chapter'
		let l:chnr=a:toc[l:openfile][l:line][1]
		let l:nr=l:chnr
" 		if l:new
" 		    let l:nr=a:toc[l:openfile][l:line][1]
" 		endif
		if a:toc[l:openfile][l:line][3]
		    "if it is stared version" 
		    let l:nr=substitute(l:nr,'.',' ','')
		endif
		if a:toc[l:openfile][l:line][4] != ''
		    call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		else
		    call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		endif
	    elseif a:toc[l:openfile][l:line][0] == 'section'
		let l:secnr=a:toc[l:openfile][l:line][1]
		if l:chap_on
		    let l:nr=l:chnr . "." . l:secnr  
" 		    if l:new
" 			let l:nr=a:toc[l:openfile][l:line][1]
" 		    endif
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		else
		    let l:nr=l:secnr 
" 		    if l:new
" 			let l:nr=a:toc[l:openfile][l:line][1]
" 		    endif
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		endif
	    elseif a:toc[l:openfile][l:line][0] == 'subsection'
		let l:ssecnr=a:toc[l:openfile][l:line][1]
		if l:chap_on
		    let l:nr=l:chnr . "." . l:secnr  . "." . l:ssecnr
" 		    if l:new
" 			let l:nr=a:toc[l:openfile][l:line][1]
" 		    endif
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		else
		    let l:nr=l:secnr  . "." . l:ssecnr
" 		    if l:new
" 			let l:nr=a:toc[l:openfile][l:line][1]
" 		    endif
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		endif
	    elseif a:toc[l:openfile][l:line][0] == 'subsubsection'
		let l:sssecnr=a:toc[l:openfile][l:line][1]
		if l:chap_on
		    let l:nr=l:chnr . "." . l:secnr . "." . l:sssecnr  
" 		    if l:new
" 			let l:nr=a:toc[l:openfile][l:line][1]
" 		    endif
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline(l:number, a:toc[l:openfile][l:line][0] . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline(l:number, a:toc[l:openfile][l:line][0] . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		else
		    let l:nr=l:secnr  . "." . l:ssecnr . "." . l:sssecnr
" 		    if l:new
" 			let l:nr=a:toc[l:openfile][l:line][1]
" 		    endif
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		endif
	    else
		let l:nr=""
	    endif
	    let l:number+=1
	endfor
    endfor
    " set the cursor position on the correct line number.
    " first get the line number of the begging of the ToC of t:atp_bufname
    " (current buffer)
" 	let t:numberdict=l:numberdict	"DEBUG
" 	t:atp_bufname is the full path to the current buffer.
    let l:num=l:numberdict[t:atp_bufname]
    let l:sorted=sort(keys(a:toc[t:atp_bufname]),"atplib#CompareList")
    let t:sorted=l:sorted
    for l:line in l:sorted
	if l:cline>=l:line
	    let l:num+=1
	endif
    keepjumps call setpos('.',[bufnr(""),l:num,1,0])
    endfor
   
    " Help Lines:
    let l:number=len(getbufline("%",1,"$"))
    call setline(l:number+1,"") 
    call setline(l:number+2,"<Space> jump") 
    call setline(l:number+3,"<Enter> jump and close") 
    call setline(l:number+4,"s       jump and split") 
    call setline(l:number+5,"y or c  yank label") 
    call setline(l:number+6,"p       paste label") 
    call setline(l:number+7,"q       close") 
endfunction
"}}}2

" This is the User Front End Function 
"{{{2 TOC
function! s:TOC(...)
    " skip generating t:atp_toc list if it exists and if a:0 != 0
    let l:skip = 0
    if a:0 >= 1 && a:1 == 1
	let l:skip = 1
    endif
    let l:new=0
    if a:0 >= 1
	let l:new=1
    endif
    if &filetype != 'tex'    
	echoerr "Wrong 'filetype'. This function works only for latex documents."
	return
    endif
    " for each buffer in t:buflist (set by s:buflist)
    if l:skip == 0 || ( l:skip == 1 && !exists("t:atp_toc") )
	for l:buffer in t:buflist 
    " 	    let t:atp_toc=s:make_toc(l:buffer)
		let t:atp_toc=s:maketoc(l:buffer)
	endfor
    endif
    call s:showtoc(t:atp_toc,l:new)
endfunction
nnoremap <Plug>ATP_TOC	:call <SID>TOC(1)
command! -buffer TOC	:call <SID>TOC()

" }}}2

" This finds the name of currently eddited section/chapter units. 
" {{{2 Current TOC
" ToDo: make this faster!
" {{{3 s:nearestsection
" This function finds the section name of the current section unit with
" respect to the dictionary a:section={ 'line number' : 'section name', ... }
" it returns the [ section_name, section line, next section line ]
function! s:nearestsection(section)
    let l:cline=line('.')

    let l:sorted=sort(keys(a:section),"atplib#CompareList")
    let l:x=0
    while l:x<len(l:sorted) && l:sorted[l:x]<=l:cline
       let l:x+=1 
    endwhile
    if l:x>=1 && l:x < len(l:sorted)
	let l:section_name=a:section[l:sorted[l:x-1]]
	return [l:section_name, l:sorted[l:x-1], l:sorted[l:x]]
    elseif l:x>=1 && l:x >= len(l:sorted)
	let l:section_name=a:section[l:sorted[l:x-1]]
	return [l:section_name,l:sorted[l:x-1], line('$')]
    elseif l:x<1 && l:x < len(l:sorted)
	" if we are before the first section return the empty string
	return ['','0', l:sorted[l:x]]
    elseif l:x<1 && l:x >= len(l:sorted)
	return ['','0', line('$')]
    endif
endfunction
" }}}3
" {{{3 s:ctoc
function! s:ctoc()
    if &filetype != 'tex' 
" TO DO:
" 	if  exists(g:tex_flavor)
" 	    if g:tex_flavor != "latex"
" 		echomsg "CTOC: Wrong 'filetype'. This function works only for latex documents."
" 	    endif
" 	endif
	" Set the status line once more, to remove the CTOC() function.
	call ATPStatus()
	return []
    endif
    " resolve the full path:
    let t:atp_bufname=resolve(fnamemodify(bufname("%"),":p"))
    
    " if t:atp_toc(t:atp_bufname) exists use it otherwise make it 
    if !exists("t:atp_toc") || !has_key(t:atp_toc,t:atp_bufname) 
	silent let t:atp_toc=s:maketoc(t:atp_bufname)
    endif

    " count where the preambule ends
    let l:buffer=getbufline(bufname("%"),"1","$")
    let l:i=0
    let l:line=l:buffer[0]
    while l:line !~ '\\begin\s*{document}' && l:i < len(l:buffer)
	let l:line=l:buffer[l:i]
	if l:line !~ '\\begin\s*{document}' 
	    let l:i+=1
	endif
    endwhile
	
    " if we are before the '\\begin{document}' line: 
    if line(".") <= l:i
	let l:return=['Preambule']
	return l:return
    endif

    let l:chapter={}
    let l:section={}
    let l:subsection={}

    for l:key in keys(t:atp_toc[t:atp_bufname])
	if t:atp_toc[t:atp_bufname][l:key][0] == 'chapter'
	    " return the short title if it is provided
	    if t:atp_toc[t:atp_bufname][l:key][4] != ''
		call extend(l:chapter, {l:key : t:atp_toc[t:atp_bufname][l:key][4]},'force')
	    else
		call extend(l:chapter, {l:key : t:atp_toc[t:atp_bufname][l:key][2]},'force')
	    endif
	elseif t:atp_toc[t:atp_bufname][l:key][0] == 'section'
	    " return the short title if it is provided
	    if t:atp_toc[t:atp_bufname][l:key][4] != ''
		call extend(l:section, {l:key : t:atp_toc[t:atp_bufname][l:key][4]},'force')
	    else
		call extend(l:section, {l:key : t:atp_toc[t:atp_bufname][l:key][2]},'force')
	    endif
	elseif t:atp_toc[t:atp_bufname][l:key][0] == 'subsection'
	    " return the short title if it is provided
	    if t:atp_toc[t:atp_bufname][l:key][4] != ''
		call extend(l:subsection, {l:key : t:atp_toc[t:atp_bufname][l:key][4]},'force')
	    else
		call extend(l:subsection, {l:key : t:atp_toc[t:atp_bufname][l:key][2]},'force')
	    endif
	endif
    endfor

    " Remove $ from chapter/section/subsection names to save the space.
    let l:chapter_name=substitute(s:nearestsection(l:chapter)[0],'\$','','g')
    let l:chapter_line=s:nearestsection(l:chapter)[1]
    let l:chapter_nline=s:nearestsection(l:chapter)[2]

    let l:section_name=substitute(s:nearestsection(l:section)[0],'\$','','g')
    let l:section_line=s:nearestsection(l:section)[1]
    let l:section_nline=s:nearestsection(l:section)[2]
"     let b:section=s:nearestsection(l:section)		" DEBUG

    let l:subsection_name=substitute(s:nearestsection(l:subsection)[0],'\$','','g')
    let l:subsection_line=s:nearestsection(l:subsection)[1]
    let l:subsection_nline=s:nearestsection(l:subsection)[2]
"     let b:ssection=s:nearestsection(l:subsection)		" DEBUG

    let l:names	= [ l:chapter_name ]
    if (l:section_line+0 >= l:chapter_line+0 && l:section_line+0 <= l:chapter_nline+0) || l:chapter_name == '' 
	call add(l:names, l:section_name) 
    elseif l:subsection_line+0 >= l:section_line+0 && l:subsection_line+0 <= l:section_nline+0
	call add(l:names, l:subsection_name)
    endif
    return l:names
endfunction
" }}}3
" {{{3 CTOC
function! CTOC(...)
    " if there is any argument given, then the function returns the value
    " (used by ATPStatus()), otherwise it echoes the section/subsection
    " title. It returns only the first b:atp_TruncateStatusSection
    " characters of the the whole titles.
    let l:names=s:ctoc()
    let b:names=l:names
" 	echo " DEBUG CTOC " . join(l:names)
    let l:chapter_name=get(l:names,0,'')
    let l:section_name=get(l:names,1,'')
    let l:subsection_name=get(l:names,2,'')

    if l:chapter_name == "" && l:section_name == "" && l:subsection_name == ""

    if a:0 == '0'
	echo "" 
    else
	return ""
    endif
	
    elseif l:chapter_name != ""
	if l:section_name != ""
" 		if a:0 == '0'
" 		    echo "XXX" . l:chapter_name . "/" . l:section_name 
" 		else
	    if a:0 != 0
		return substitute(strpart(l:chapter_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','') . "/" . substitute(strpart(l:section_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','')
	    endif
	else
" 		if a:0 == '0'
" 		    echo "XXX" . l:chapter_name
" 		else
	    if a:0 != 0
		return substitute(strpart(l:chapter_name,0,b:atp_TruncateStatusSection), '\_s*$', '','')
	    endif
	endif

    elseif l:chapter_name == "" && l:section_name != ""
	if l:subsection_name != ""
" 		if a:0 == '0'
" 		    echo "XXX" . l:section_name . "/" . l:subsection_name 
" 		else
	    if a:0 != 0
		return substitute(strpart(l:section_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','') . "/" . substitute(strpart(l:subsection_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','')
	    endif
	else
" 		if a:0 == '0'
" 		    echo "XXX" . l:section_name
" 		else
	    if a:0 != 0
		return substitute(strpart(l:section_name,0,b:atp_TruncateStatusSection), '\_s*$', '','')
	    endif
	endif

    elseif l:chapter_name == "" && l:section_name == "" && l:subsection_name != ""
" 	    if a:0 == '0'
" 		echo "XXX" . l:subsection_name
" 	    else
	if a:0 != 0
	    return substitute(strpart(l:subsection_name,0,b:atp_TruncateStatusSection), '\_s*$', '','')
	endif
    endif
endfunction
command! -buffer CTOC		:call CTOC()
" }}}3
" }}}2
" }}}

" Labels Front End Finction. The search engine/show function are in autoload/atplib.vim script
" library.
" {{{ Labels
function! s:Labels()
    let t:atp_bufname=bufname("%")
    let l:bufname=resolve(fnamemodify(t:atp_bufname,":p"))
    " Generate the dictionary with labels
    let t:atp_labels=atplib#generatelabels(l:bufname)
    " Show the labels in seprate window
    call atplib#showlabels(t:atp_labels[l:bufname])
endfunction
nnoremap <Plug>ATP_Labels	:call <SID>Labels()
command! -buffer Labels		:call <SID>Labels()
" }}}

" Edit Input Files
" {{{1 Edit Input Files 
function! EditInputFile(...)

    let l:mainfile=b:atp_MainFile

    if a:0 == 0
	let l:inputfile=""
	let l:bufname=b:atp_MainFile
	let l:opencom="edit"
    elseif a:0 == 1
	let l:inputfile=a:1
	let l:bufname=b:atp_MainFile
	let l:opencom="edit"
    else
	let l:inputfile=a:1
	let l:opencom=a:2

	" the last argument is the bufername in which search for the input files 
	if a:0 > 2
	    let l:bufname=a:3
	else
	    let l:bufname=b:atp_MainFile
	endif
    endif

    let l:dir=fnamemodify(b:atp_MainFile,":p:h")

    if a:0 == 0
	let l:inputfiles=FindInputFiles(l:bufname)
    else
	let l:inputfiles=FindInputFiles(l:bufname,0)
    endif

    if !len(l:inputfiles) > 0
	return 
    endif

    if index(keys(l:inputfiles),l:inputfile) == '-1'
	let l:which=input("Which file to edit? <enter> for none ","","customlist,EI_compl")
	if l:which == ""
	    return
	endif
    else
	let l:which=l:inputfile
    endif

    if l:which =~ '^\s*\d\+\s*$'
	let l:ifile=keys(l:inputfiles)[l:which-1]
    else
	let l:ifile=l:which
    endif

    "if the choosen file is the main file put the whole path.
"     if l:ifile == fnamemodify(b:atp_MainFile,":t")
" 	let l:ifile=b:atp_MainFile
"     endif

    "g:texmf should end with a '/', if not add it.
    if g:texmf !~ "\/$"
	let g:texmf=g:texmf . "/"
    endif

    " remove all '"' from the line (latex do not supports file names with '"')
    " this make the function work with lines like: '\\input "file name with spaces.tex"'
    let l:ifile=substitute(l:ifile,'^\s*\"\|\"\s*$','','g')
    " add .tex extension if it was not present
    if l:inputfiles[l:ifile][0] == 'input' || l:inputfiles[l:ifile][0] == 'include'
	let l:ifilename=atplib#append(l:ifile,'.tex')
    elseif l:inputfiles[l:ifile][0] == 'bib'
	let l:ifilename=atplib#append(l:ifile,'.bib')
    elseif  l:inputfiles[l:ifile][0] == 'main file'
	let l:ifilename=b:atp_MainFile
    endif
    if l:ifile !~ '\s*\/'
	if filereadable(l:dir . "/" . l:ifilename) 
	    let s:ft=&filetype
	    exe "edit " . fnameescape(b:atp_OutDir . l:ifilename)
	    let &l:filetype=s:ft
	else
	    if l:inputfiles[l:ifile][0] == 'input' || l:inputfiles[l:ifile][0] == 'include'
		let l:ifilename=findfile(l:ifile,g:texmf . '**')
		let s:ft=&filetype
		exe l:opencom . " " . fnameescape(l:ifilename)
		let &l:filetype=s:ft
		let b:atp_MainFile=l:mainfile
	    elseif l:inputfiles[l:ifile][0] == 'bib' 
		let s:ft=&filetype
		exe l:opencom . " " . l:inputfiles[l:ifile][2]
		let &l:filetype=s:ft
		let b:atp_MainFile=l:mainfile
	    elseif  l:inputfiles[l:ifile][0] == 'main file' 
		exe l:opencom . " " . b:atp_MainFile
		let b:atp_MainFile=l:mainfile
	    endif
	endif
    else
	exe l:opencom . " " . fnameescape(l:ifilename)
	let b:atp_MainFile=l:mainfile
    endif
endfunction
command! -buffer -nargs=* -complete=customlist,<SID>EI_compl	EditInputFile 	:call <SID>EditInputFile(<f-args>)
nnoremap <silent> <buffer> <Plug>EditInputFile			:call <SID>EditInputFile(<f-args>)<CR>


fun! s:EI_compl(A,P,L)
"     let l:inputfiles=FindInputFiles(bufname("%"),1)

    let l:inputfiles=filter(FindInputFiles(b:atp_MainFile,1), 'v:key !~ fnamemodify(bufname("%"),":t:r")')
    " rewrite the keys of FindInputFiles the order: input files, bibfiles
    let l:oif=[]
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'main file'
	    call add(l:oif,fnamemodify(l:key,":t"))
	endif
    endfor
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'input'
	    call add(l:oif,l:key)
	endif
    endfor
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'include'
	    call add(l:oif,l:key)
	endif
    endfor
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'bib'
	    call add(l:oif,l:key)
	endif
    endfor

    " check what is already written, if it matches something return only the
    " matching strings
    let l:return_oif=[]
    for l:i in l:oif
	if l:i =~ '^' . a:A 
	    call add(l:return_oif,l:i)
	endif
    endfor
    return l:return_oif
endfun
" }}}1

" Motion functions through environments and sections. 
" {{{ Motion functions
" Move to next environment which name is given as the argument. Do not wrap
" around the end of the file.
function! s:NextEnv(...)
    let env_name = ( a:0 == 0 ? '[^}]*' : a:1 )
    call search('\%(%.*\)\@<!\\begin{' . env_name . '.*}', 'W')
    let @/='\%(%.*\)\@<!\\begin{' . env_name . '.*}'
endfunction
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl NEnv		:call <SID>NextEnv(<f-args>)
nnoremap <silent> <Plug>NextEnv		:call <SID>NextEnv('[^}]*')

function! s:PrevEnv(...)
    let env_name = a:0 == 0 ? '[^}]*' : a:1
    call search('\%(%.*\)\@<!\\begin{' . env_name . '.*}', 'bW')
    let @/='\%(%.*\)\@<!\\begin{' . env_name . '.*}'
endfunction
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl PEnv		:call <SID>PrevEnv(<f-args>)
nnoremap <silent> <Plug>PreviousEnv	:call <SID>PrevEnv('[^}]*')

" Move to next section, the extra argument is a pattern to match for the
" section title. The first, obsolete argument stands for:
" part,chapter,section,subsection,etc.
" This commands wrap around the end of the file.
function! s:NextSection(secname,...)
    let section_title_pattern = ( a:0 == 0 ? '' : '\s*{.*' . a:1 )
    call search('\\' . a:secname . '\>' . section_title_pattern ,'w')
    let @/='\\' . a:secname . '\>' . section_title_pattern
endfunction
nnoremap <silent> <Plug>GoToNextSection		:call <SID>NextSection('section')
nnoremap <silent> <Plug>GoToNextChapter		:call <SID>NextSection('chapter')
nnoremap <silent> <Plug>GoToNextPart		:call <SID>NextSection('part')
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl NSec		:call <SID>NextSection('section',<f-args>)
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl NChap		:call <SID>NextSection('chapter',<f-args>)
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl NPart		:call <SID>NextSection('part',<f-args>)

function! s:PreviousSection(secname,...)
    let section_title_pattern = ( a:0 == 0 ? '' : '\s*{.*' . a:1 )
    call search('\\' . a:secname . '\>' . section_title_pattern ,'bw')
    let @/='\\' . a:secname . '\>' . section_title_pattern
endfunction
nnoremap <silent> <Plug>GoToPreviousSection		:call <SID>PreviousSection('section')
nnoremap <silent> <Plug>GoToPreviousChapter		:call <SID>PreviousSection('chapter')
nnoremap <silent> <Plug>GoToPreviousPart		:call <SID>PreviousSection('part')
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl PSec		:call <SID>PreviousSection('section',<f-args>)
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl PChap		:call <SID>PreviousSection('chapter',<f-args>)
command! -buffer -count=1 -nargs=? -complete=customlist,Env_compl PPart		:call <SID>PreviousSection('part',<f-args>)

function! Env_compl(A,P,L)
    let l:envlist=sort(['abstract', 'definition', 'equation', 'proposition', 
		\ 'theorem', 'lemma', 'array', 'tikzpicture', 
		\ 'tabular', 'table', 'align\*\?', 'alignat\*\?', 'proof', 
		\ 'corollary', 'enumerate', 'examples\?', 'itemize', 'remark', 
		\ 'notation', 'center', 'quotation', 'quote', 'tabbing', 
		\ 'picture', 'minipage', 'list', 'flushright', 'flushleft', 
		\ 'figure', 'eqnarray', 'thebibliography', 'titlepage', 
		\ 'verbatim', 'verse' ])
    let l:returnlist=[]
    for l:env in l:envlist
	if l:env =~ '^' . a:A 
	    call add(l:returnlist,l:env)
	endif
    endfor
    return l:returnlist
endfunction
" }}}

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/options.vim	[[[1
1291
" This file contains all the options defined on startup of ATP
" you can add your local settings to ftplugin/ATP_files/atprc file


" Some options (functions) should be set once:
let s:did_options 	= exists("s:did_options") ? 1 : 0


if filereadable(fnameescape($HOME . '/.atprc.vim'))

	" Note: in $HOME/.atprc file the user can set all the local buffer
	" variables without using autocommands
	execute 'source ' . fnameescape($HOME . '/.atprc.vim')

endif

"{{{ tab-local variables
" We need to know bufnumber and bufname in a tabpage.
" ToDo: we can set them with s: and call them using <SID> stack
" (how to make the <SID> stack invisible to the user!

    let t:atp_bufname	= bufname("")
    let t:atp_bufnr	= bufnr("")
    let t:atp_winnr	= winnr()


" autocommands for buf/win numbers
" These autocommands are used to remember the last opened buffer number and its
" window number:
if !s:did_options
    au BufLeave *.tex 	let t:atp_bufname	= resolve(fnamemodify(bufname(""),":p"))
    au BufLeave *.tex 	let t:atp_bufnr		= bufnr("")
    " t:atp_winnr the last window used by tex, ToC or Labels buffers:
    au WinEnter *.tex 	let t:atp_winnr		= winnr("#")
    au WinEnter __ToC__ 	let t:atp_winnr		= winnr("#")
    au WinEnter __Labels__ 	let t:atp_winnr		= winnr("#")
endif
"}}}

" vim options + indetation
" {{{ Vim options

" {{{ Intendation
if !exists("g:atp_indentation")
    let g:atp_indentation=1
endif
" if !exists("g:atp_tex_indent_paragraphs")
"     let g:atp_tex_indent_paragraphs=1
" endif
if g:atp_indentation
"     setl indentexpr=GetTeXIndent()
"     setl nolisp
"     setl nosmartindent
"     setl autoindent
"     setl indentkeys+=},=\\item,=\\bibitem,=\\[,=\\],=<CR>
"     let prefix = expand('<sfile>:p:h:h')
"     exe 'so '.prefix.'/indent/tex_atp.vim'
    let prefix = expand('<sfile>:p:h')    
    exe 'so '.prefix.'/LatexBox_indent.vim'
endif
" }}}

setl keywordprg=texdoc\ -m
" Borrowed from tex.vim written by Benji Fisher:
    " Set 'comments' to format dashed lists in comments
    setlocal com=sO:%\ -,mO:%\ \ ,eO:%%,:%

    " Set 'commentstring' to recognize the % comment character:
    " (Thanks to Ajit Thakkar.)
    setlocal cms=%%s

    " Allow "[d" to be used to find a macro definition:
    " Recognize plain TeX \def as well as LaTeX \newcommand and \renewcommand .
    " I may as well add the AMS-LaTeX DeclareMathOperator as well.
    let &l:define='\\\([egx]\|char\|mathchar\|count\|dimen\|muskip\|skip\|toks\)\='
	    \ .	'def\|\\font\|\\\(future\)\=let'
	    \ . '\|\\new\(count\|dimen\|skip\|muskip\|box\|toks\|read\|write'
	    \ .	'\|fam\|insert\)'
	    \ . '\|\\\(re\)\=new\(boolean\|command\|counter\|environment\|font'
	    \ . '\|if\|length\|savebox\|theorem\(style\)\=\)\s*\*\=\s*{\='
	    \ . '\|DeclareMathOperator\s*{\=\s*'
    setlocal include=\\\\input\\\\|\\\\include{
    setlocal suffixesadd=.tex

    setl includeexpr=substitute(v:fname,'\\%(.tex\\)\\?$','.tex','')
    " TODO set define and work on the above settings, these settings work with [i
    " command but not with [d, [D and [+CTRL D (jump to first macro definition)
" }}}

"{{{ Set the project name 
" This function sets the main project name (b:atp_MainFile)
" It is used by EditInputFile which copies the value of this variable to every
" input file included in the main source file. 
" ToDo: CHECK IF THIS IS WORKS RECURSIVELY?
" {{{ s:setprojectname
" store a list of all input files associated to some file
fun! s:setprojectname()
    " if the project name was already set do not set it for the second time
    " (which sets then b:atp_MainFile to wrong value!)  
    if &filetype == "fd_atp"
	" this is needed for EditInputFile function to come back to the main
	" file.
	let b:atp_MainFile	= fnamemodify(expand("%"),":p")
	let b:did_project_name	= 1
    endif

    if exists("b:did_project_name") 
	return " project name was already set"
    else
	let b:did_project_name	= 1
    endif

    if !exists("s:inputfiles")
	let s:inputfiles 	= FindInputFiles(expand("%"),0)
    else
	call extend(s:inputfiles,FindInputFiles(bufname("%"),0))
    endif

    if !exists("g:atp_project")
	" the main file is not an input file (at this stage!)
	if index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) == '-1' &&
	 \ index(keys(s:inputfiles),fnamemodify(bufname("%"),":t"))   == '-1' &&
	 \ index(keys(s:inputfiles),fnamemodify(bufname("%"),":p:r")) == '-1' &&
	 \ index(keys(s:inputfiles),fnamemodify(bufname("%"),":p"))   == '-1' 
	    let b:atp_MainFile=fnamemodify(expand("%"),":p")
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":t")) != '-1'
	    let b:atp_MainFile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":t")][1],":p")
	    let s:pn_return="input file 1"
	    if !exists('#CursorHold#' 	. fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " 	. fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1'
	    let b:atp_MainFile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":t:r")][1],":p")
	    let s:pn_return="input file 2"
	    if !exists('#CursorHold#' 	. fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " 	. fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":p:r")) != '-1' 
	    let b:atp_MainFile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":p:r")][1],":p")
	    if !exists('#CursorHold#' 	. fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " 	. fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":p"))   != '-1' 
	    let b:atp_MainFile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":p")][1],":p")
	    if !exists('#CursorHold#' 	. fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " 	. fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	endif
	let s:pn_return 	= " set"
    elseif exists("g:atp_project")
	let b:atp_MainFile	= g:atp_project
	let s:pn_return		= " set from g:atp_project"
    endif

    " we need to escape white spaces in b:atp_MainFile but not in all places so
    " this is not done here
    let b:pn_return=s:pn_return
    return s:pn_return
endfun
command! SetProjectName	:call <SID>setprojectname()
" }}}

if !s:did_options
    au BufEnter *.tex :call s:setprojectname()
    au BufEnter *.fd  :call s:setprojectname()
endif
"}}}

" This function sets vim 'errorfile' option.
" {{{ Set error file (function and autocommands)
" let &l:errorfile=b:atp_OutDir . fnameescape(fnamemodify(expand("%"),":t:r")) . ".log"
"{{{
function! s:SetErrorFile()

    " set b:atp_OutDir if it is not set
    if !exists("b:atp_OutDir")
	call s:setoutdir(0)
    endif

    " set the b:atp_MainFile varibale if it is not set (the project name)
    if !exists("b:atp_MainFile")
	call s:setprojectname()
    endif

"     let l:ef=b:atp_OutDir . fnamemodify(expand("%"),":t:r") . ".log"
    let l:ef=atplib#append(b:atp_OutDir,'/') . fnamemodify(b:atp_MainFile,":t:r") . ".log"
    let &l:errorfile=l:ef
    return &l:errorfile
endfunction
command! -buffer SetErrorFile		:call s:SetErrorFile()
"}}}

if !s:did_options
    au BufEnter *.tex 		call 		<SID>SetErrorFile()
    au BufRead 	$l:errorfile 	setlocal 	autoread 
endif
"}}}

" This functions sets the value of b:atp_OutDir variable
" {{{ s:setoutdir
" This options are set also when editing .cls files.
" It can overwrite the value of b:atp_OutDir
function! s:setoutdir(arg)
    " first we have to check if this is not a project file
    if exists("g:atp_project") || exists("s:inputfiles") && 
		\ ( index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1' || 
		\ index(keys(s:inputfiles),fnamemodify(bufname("%"),":t")) != '-1' )
	    " if we are in a project input/include file take the correct value of b:atp_OutDir from the atplib#s:outdir_dict dictionary.
	    
	    if index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1'
		let b:atp_OutDir=g:outdir_dict[s:inputfiles[fnamemodify(bufname("%"),":t:r")][1]]
	    elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":t")) != '-1'
		let b:atp_OutDir=g:outdir_dict[s:inputfiles[fnamemodify(bufname("%"),":t")][1]]
	    endif
    else
	
	    " if we are not in a project input/include file set the b:atp_OutDir
	    " variable	

	    " if the user want to be asked for b:atp_OutDir
	    if g:askfortheoutdir == 1 
		let b:atp_OutDir=input("Where to put output? do not escape white spaces ")
	    endif

	    if ( get(getbufvar(bufname("%"),""),"outdir","optionnotset") == "optionnotset" 
			\ && g:askfortheoutdir != 1 
			\ || b:atp_OutDir == "" && g:askfortheoutdir == 1 )
			\ && !exists("$TEXMFOUTPUT")
		 let b:atp_OutDir=fnamemodify(resolve(expand("%:p")),":h") . "/"
		 echoh WarningMsg | echomsg "Output Directory "b:atp_OutDir | echoh None

	    elseif exists("$TEXMFOUTPUT")
		 let b:atp_OutDir=$TEXMFOUTPUT 
	    endif	

	    " if arg != 0 then set errorfile option accordingly to b:atp_OutDir
	    if bufname("") =~ ".tex$" && a:arg != 0
		 call s:SetErrorFile()
	    endif

	    if exists("g:outdir_dict")
		let g:outdir_dict=extend(g:outdir_dict, {fnamemodify(bufname("%"),":p") : b:atp_OutDir })
	    else
		let g:outdir_dict={ fnamemodify(bufname("%"),":p") : b:atp_OutDir }
	    endif
    endif
    return b:atp_OutDir
endfunction
" }}}

" Almost all global variables 
" {{{ global variables 
if !exists("g:atp_TeXdocDefault")
    let g:atp_TeXdocDefault	= '-a lshort'
endif
"ToDo: to doc.
"ToDo: luatex! (can produce both!)
if !exists("g:atp_CompilersDict")
    let g:atp_CompilersDict = { 
		\ "pdflatex" 	: ".pdf", 	"pdftex" 	: ".pdf", 
		\ "xetex" 	: ".pdf", 	"latex" 	: ".dvi", 
		\ "tex" 	: ".dvi",	"luatex"	: ".pdf"}
endif
"ToDo: to doc.
if !exists("g:atp_DefaultDebugMode")
    " recognised values: silent, normal, debug.
    let g:atp_DefaultDebugMode="normal"
endif
if !exists("g:atp_show_all_lines")
    " boolean
    let g:atp_show_all_lines = 0
endif
if !exists("g:atp_ignore_unmatched")
    " boolean
    let g:atp_ignore_unmatched = 1
endif
if !exists("g:atp_imap_first_leader")
    let g:atp_imap_first_leader="#"
endif
if !exists("g:atp_imap_second_leader")
    let g:atp_imap_second_leader="##"
endif
if !exists("g:atp_imap_third_leader")
    let g:atp_imap_third_leader="]"
endif
if !exists("g:atp_imap_fourth_leader")
    let g:atp_imap_fourth_leader="["
endif
" todo: to doc.
if !exists("g:atp_completion_font_encodings")
    let g:atp_completion_font_encodings=['T1', 'T2', 'T3', 'T5', 'OT1', 'OT2', 'OT4', 'UT1'] 
endif
" todo: to doc.
if !exists("g:atp_font_encoding")
    let s:line=atplib#SearchPackage('fontenc')
    if s:line != 0
	" the last enconding is the default one for fontenc, this we will
	" use
	let s:enc=matchstr(getline(s:line),'\\usepackage\s*\[\%([^,]*,\)*\zs[^]]*\ze\]\s*{fontenc}')
    else
	let s:enc='OT1'
    endif
    let g:atp_font_encoding=s:enc
    unlet s:line
    unlet s:enc
endif
if !exists("g:atp_no_star_environments")
    let g:atp_no_star_environments=['document', 'flushright', 'flushleft', 'center', 
		\ 'enumerate', 'itemize', 'tikzpicture', 'scope', 
		\ 'picture', 'array', 'proof', 'tabular', 'table' ]
endif
let s:ask={ "ask" : "0" }
if !exists("g:atp_sizes_of_brackets")
    let g:atp_sizes_of_brackets={'\left': '\right', '\bigl' : '\bigr', 
		\ '\Bigl' : '\Bigr', '\biggl' : '\biggr' , 
		\ '\Biggl' : '\Biggr', '\' : '\' }
endif
if !exists("g:atp_bracket_dict")
    let g:atp_bracket_dict = { '(' : ')', '{' : '}', '[' : ']'  }
endif
" }}}2 			variables
if !exists("g:atp_LatexBox")
    let g:atp_LatexBox=1
endif
if !exists("g:atp_check_if_LatexBox")
    let g:atp_check_if_LatexBox=1
endif
if !exists("g:atp_autex_check_if_closed")
    let g:atp_autex_check_if_closed=1
endif
if !exists("g:rmcommand") && executable("perltrash")
    let g:rmcommand="perltrash"
elseif !exists("g:rmcommand")
    let g:rmcommand="rm"
endif
if !exists("g:atp_env_maps_old")
    let g:atp_env_maps_old=0
endif
if !exists("g:atp_amsmath")
    let g:atp_amsmath=0
endif
if !exists("g:atp_no_math_command_completion")
    let g:atp_no_math_command_completion=0
endif
if !exists("g:askfortheoutdir")
    let g:askfortheoutdir=0
endif
if !exists("g:atp_tex_extensions")
    let g:atp_tex_extensions=["aux", "log", "bbl", "blg", "spl", "snm", "nav", "thm", "brf", "out", "toc", "mpx", "idx", "maf", "blg", "glo", "mtc[0-9]", "mtc1[0-9]"]
endif
if !exists("g:atp_delete_output")
    let g:atp_delete_output=0
endif
if !exists("g:keep")
    let g:keep=["log","aux","toc","bbl"]
endif
if !exists("g:printingoptions")
    let g:printingoptions=''
endif
if !exists("g:atp_ssh")
    let g:atp_ssh=substitute(system("whoami"),'\n','','') . "@localhost"
endif
" opens bibsearch results in vertically split window.
if !exists("g:vertical")
    let g:vertical=1
endif
if !exists("g:matchpair")
    let g:matchpair="(:),[:],{:}"
endif
if !exists("g:texmf")
    let g:texmf=$HOME . "/texmf"
endif
" a list where tex looks for bib files
if !exists("g:atp_bibinputs")
    let g:atp_bibinputs=split(substitute(substitute(
		\ system("kpsewhich -show-path bib")
		\ ,'\/\/\+','\/','g'),'!\|\n','','g'),':')
endif
if !exists("g:atp_compare_embedded_comments") || g:atp_compare_embedded_comments != 1
    let g:atp_compare_embedded_comments = 0
endif
if !exists("g:atp_compare_double_empty_lines") || g:atp_compare_double_empty_lines != 0
    let g:atp_compare_double_empty_lines = 1
endif
"TODO: put toc_window_with and labels_window_width into DOC file
if !exists("t:toc_window_width")
    if exists("g:toc_window_width")
	let t:toc_window_width=g:toc_window_width
    else
	let t:toc_window_width=30
    endif
endif
if !exists("t:atp_labels_window_width")
    if exists("g:labels_window_width")
	let t:atp_labels_window_width=g:labels_window_width
    else
	let t:atp_labels_window_width=30
    endif
endif
if !exists("g:atp_completion_limits")
    let g:atp_completion_limits=[40,60,80,120]
endif
if !exists("g:atp_long_environments")
    let g:atp_long_environments=[]
endif
if !exists("g:atp_no_complete")
     let g:atp_no_complete=['document']
endif
" if !exists("g:atp_close_after_last_closed")
"     let g:atp_close_after_last_closed=1
" endif
if !exists("g:atp_no_env_maps")
    let g:atp_no_env_maps=0
endif
if !exists("g:atp_extra_env_maps")
    let g:atp_extra_env_maps=0
endif
" todo: to doc. Now they go first.
" if !exists("g:atp_math_commands_first")
"     let g:atp_math_commands_first=1
" endif
if !exists("g:atp_completion_truncate")
    let g:atp_completion_truncate=4
endif
" ToDo: to doc.
" add server call back (then automatically reads errorfiles)
if !exists("g:atp_status_notification")
    if has('clientserver') && !empty(v:servername) 
	let g:atp_status_notification=1
    else
	let g:atp_status_notification=0
    endif
endif
if !exists("g:atp_callback")
    if exists("g:atp_status_notification") && g:atp_status_notification == 1
	let g:atp_callback=1
    elseif has('clientserver') && !empty(v:servername) 
	let g:atp_callback=1
    else
	let g:atp_callback=0
    endif
endif
" ToDo: to doc.
" I switched this off.
" if !exists("g:atp_complete_math_env_first")
"     let g:atp_complete_math_env_first=0
" endif
" }}}

" Buffer-local variables
" {{{ buffer variables
let b:atp_running=0

" these are all buffer related variables:
let s:optionsDict= { 	"atp_TexOptions" 	: "", 		
	        \ "atp_ReloadOnError" 		: "1", 
		\ "atp_OpenViewer" 		: "1", 		
		\ "atp_autex" 			: "1", 
		\ "atp_Viewer" 			: "xpdf", 	
		\ "atp_OutputFlavour" 		: "pdf", 
		\ "atp_TexFlavour" 		: &filetype, 
		\ "atp_ViewerOptions" 		: "", 
		\ "atp_XpdfServer" 		: fnamemodify(expand("%"),":t"), 
		\ "atp_OutDir" 			: fnameescape(fnamemodify(resolve(expand("%:p")),":h")) . "/",
		\ "atp_TexCompiler" 		: "pdflatex",	
		\ "atp_auruns"			: "1",
		\ "atp_TruncateStatusSection"	: "40", 
		\ "atp_LastBibPattern"		: "" }

" We changed some variable names and we want to be nice :)
" {{{ Be Nice :)
let s:optionsDict_old= { "texoptions" 	: "atp_TexOptions",
	    	\ 	"reloadonerror"	: "atp_ReloadOnError", 
		\	"openviewer" 	: "atp_OpenViewer",
		\ 	"autex" 	: "atp_autex", 
		\	"Viewer" 	: "atp_Viewer",
		\ 	"ViewerOptions"	: "atp_ViewerOptions", 
		\	"XpdfServer" 	: "atp_XpdfServer",
		\	"outdir" 	: "atp_OutDir",
		\	"texcompiler" 	: "atp_TexCompiler",	
		\ 	"auruns"	: "atp_auruns",
		\ 	"truncate_status_section" : "atp_TruncateStatusSection",
		\ 	"atp_local_commands"	: "atp_LocalCommands",
		\ 	"atp_local_environments": "atp_LocalEnvironments",
		\ 	"atp_local_colors"	: "atp_LocalColors"}

function! BeNice(key) 
    let var = get(s:optionsDict_old,a:key,"")
    if var == ""
	return 0
    endif
    if exists("b:".a:key)
	echohl WarningMsg
	echomsg "The variable b:".a:key." is depracated, use b:".var." instead"
	echomsg "It will be REMOVED in future releases."
	echomsg "Setting the value of b:".var." to b:".a:key
	echohl Normal
	execute "let b:".var."=b:".a:key
	return 1
    endif
    return 2
endfunction

    " BeNice / the change of names of local variables/
if !s:did_options
    let s:be_nice_dict=getbufvar(bufname("%"),"")
    for key in keys(s:be_nice_dict)
	call BeNice(key)
    endfor
endif
"}}}

" This function sets options (values of buffer related variables) which were
" not already set by the user.
" {{{ s:SetOptions
function! s:SetOptions()

    let s:optionsKeys		= keys(s:optionsDict)
    let s:optionsinuseDict	= getbufvar(bufname("%"),"")

    "for each key in s:optionsKeys set the corresponding variable to its default
    "value unless it was already set in .vimrc file.
    for l:key in s:optionsKeys
	if string(get(s:optionsinuseDict,l:key, "optionnotset")) == string("optionnotset") && l:key != "outdir" && l:key != "atp_autex"
	    call setbufvar(bufname("%"),l:key,s:optionsDict[l:key])
	elseif l:key == "atp_OutDir"
	    call BeNice(l:key)
	    
	    " set b:atp_OutDir and the value of errorfile option
	    if !exists("b:atp_OutDir")
		call s:setoutdir(1)
	    endif
	    let s:ask["ask"] 	= 1
	endif
    endfor
    if string(get(s:optionsinuseDict,"atp_autex", 'optionnotset')) == string('optionnotset')
	let l:atp_texinputs=split(substitute(substitute(system("kpsewhich -show-path tex"),'\/\/\+','\/','g'),'!\|\n','','g'),':')
	call remove(l:atp_texinputs,'.')
	call filter(l:atp_texinputs,'v:val =~ b:atp_OutDir')
	if len(l:atp_texinputs) == 0
	    let b:atp_autex	= 1
	else
	    let b:atp_autex	= 0
	endif
    endif
endfunction
"}}}
call s:SetOptions()

" This is to be extended into a nice function which shows the important options
" and alows to reconfigure atp
"{{{ ShowOptions
function! ShowOptions()
    let message_dict=""
    for key in keys(s:optionsDict)
	echo key
" 	let message.=key."\t\t".s:optionDict[key]."\n"
    endfor
    call confirm(message)
endfunction
command! -buffer -nargs=? ShowOptions		:call <SID>ShowOptions(<f-args>)
"}}}
"}}}

" Variables for the Debug Mode
" {{{
" ToDo: to doc.
let t:atp_DebugMode	= g:atp_DefaultDebugMode 
" there are three possible values of t:atp_DebugMode
" 	silent/normal/debug
let t:atp_QuickFixOpen	= 0

if !s:did_options
    au FileType *.tex 	let t:atp_DebugMode	= g:atp_DefaultDebugMode
    " When opening the quickfix error buffer:  
    au FileType qf 		let t:atp_QuickFixOpen=1
    " When closing the quickfix error buffer (:close, :q) also end the Debug Mode.
    au FileType qf 		au BufUnload <buffer> let t:atp_DebugMode = g:atp_DefaultDebugMode | let t:atp_QuickFixOpen = 0
    au FileType qf		setl nospell
endif
"}}}

" These are two functions which sets options for Xpdf and Xdvi. 
" {{{ Xpdf, Xdvi
if !s:did_options
" xdvi - supports forward and reverse searching
" {{{ SetXdvi
fun! s:SetXdvi()
    let b:atp_TexCompiler	= "latex"
    let b:atp_TexOptions	= "-src-specials"
    if exists("g:xdviOptions")
	let b:atp_ViewerOptions	= g:xdviOptions
    endif
    let b:atp_Viewer="xdvi " . b:atp_ViewerOptions . " -editor 'gvim --servername " . v:servername . " --remote-wait +%l %f'"
    if !exists("*RevSearch")
    function RevSearch()
	let b:xdvi_reverse_search="xdvi " . b:atp_ViewerOptions . 
		\ " -editor 'gvim --servername " . v:servername . 
		\ " --remote-wait +%l %f' -sourceposition " . 
		\ line(".") . ":" . col(".") . fnameescape(fnamemodify(expand("%"),":p")) . 
		\ " " . fnameescape(fnamemodify(expand("%"),":p:r") . ".dvi")
	call system(b:xdvi_reverse_search)
    endfunction
    endif
    command! -buffer RevSearch 					:call RevSearch()
    map <buffer> <LocalLeader>rs				:call RevSearch()<CR>
    nmenu 550.65 &LaTeX.Reverse\ Search<Tab>:map\ <LocalLeader>rs	:RevSearch<CR>
endfun
command! -buffer SetXdvi		:call <SID>SetXdvi()
nnoremap <silent> <buffer> <Plug>SetXdvi	:call <SID>SetXdvi()<CR>
" }}}

" xpdf - supports server option (we use the reoding mechanism, which allows to
" copy the output file but not reload the viewer if there were errors during
" compilation (b:atp_ReloadOnError variable)
" {{{ SetXpdf
fun! SetXpdf()
    let b:atp_TexCompiler	= "pdflatex"
    let b:atp_TexOptions	= ""
    let b:atp_Viewer		= "xpdf"
    if exists("g:xpdfOptions")
	let b:atp_ViewerOptions	= g:xpdfOptions
    else
	let b:atp_ViewerOptions	= ''
    endif
    if hasmapto("RevSearch()",'n')
	unmap <buffer> <LocalLeader>rs
    endif
    if exists("RevSearch")
	delcommand RevSearch
    endif
    aunmenu LaTeX.Reverse\ Search
endfun
command! -buffer SetXpdf			:call <SID>SetXpdf()
nnoremap <silent> <buffer> <Plug>SetXpdf	:call <SID>SetXpdf()<CR>
" }}}
endif
" }}}

" These are functions which toggles some of the options:
"{{{ Toggle Functions
if !s:did_options
" {{{ ToggleAuTeX
" command! -buffer -count=1 TEX	:call TEX(<count>)		 
function! s:ToggleAuTeX()
  if b:atp_autex != 1
    let b:atp_autex=1	
    echo "automatic tex processing is ON"
  else
    let b:atp_autex=0
    echo "automatic tex processing is OFF"
  endif
endfunction
command! -buffer 	ToggleAuTeX 		:call <SID>ToggleAuTeX()
nnoremap <silent> <Plug>ToggleAuTeX 		:ToggleAuTeX
"}}}
" {{{ ToggleSpace
" Special Space for Searching 
let s:special_space="[off]"
function! s:ToggleSpace()
    if maparg('<space>','c') == ""
	echomsg "special space is on"
	cmap <Space> \_s\+
	let s:special_space="[on]"
	silent! aunmenu LaTeX.Toggle\ Space\ [off]
	silent! aunmenu LaTeX.Toggle\ Space\ [on]
	nmenu 550.78 &LaTeX.&Toggle\ Space\ [on]<Tab>cmap\ <space>\ \\_s\\+	:ToggleSpace<CR>
	tmenu &LaTeX.&Toggle\ Space\ [on] cmap <space> \_s\+ is curently on
    else
	echomsg "special space is off"
 	cunmap <Space>
	let s:special_space="[off]"
	silent! aunmenu LaTeX.Toggle\ Space\ [on]
	silent! aunmenu LaTeX.Toggle\ Space\ [off]
	nmenu 550.78 &LaTeX.&Toggle\ Space\ [off]<Tab>cmap\ <space>\ \\_s\\+	:ToggleSpace<CR>
	tmenu &LaTeX.&Toggle\ Space\ [off] cmap <space> \_s\+ is curently off
    endif
endfunction
command! -buffer 	ToggleSpace 	:call <SID>ToggleSpace()
nnoremap <silent> <Plug>ToggleSpace 	:ToggleSpace
"}}}
" {{{ ToggleCheckMathOpened
" This function toggles if ATP is checking if editing a math mode.
" This is used by insert completion.
" ToDo: to doc.
function! s:ToggleCheckMathOpened()
    if g:atp_math_opened
	echomsg "check if in math environment is off"
	silent! aunmenu LaTeX.Toggle\ Check\ if\ in\ Math\ [on]
	silent! aunmenu LaTeX.Toggle\ Check\ if\ in\ Math\ [off]
	nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [off]<Tab>g:atp_math_opened			
		    \ :ToggleCheckMathOpened<CR>
    else
	echomsg "check if in math environment is on"
	silent! aunmenu LaTeX.Toggle\ Check\ if\ in\ Math\ [off]
	silent! aunmenu LaTeX.Toggle\ Check\ if\ in\ Math\ [off]
	nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [on]<Tab>g:atp_math_opened
		    \ :ToggleCheckMathOpened<CR>
    endif
    let g:atp_math_opened=!g:atp_math_opened
endfunction
command! -buffer 	ToggleCheckMathOpened 	:call <SID>ToggleCheckMathOpened()
nnoremap <silent> <Plug>ToggleCheckMathOpened	:ToggleCheckMathOpened
"}}}
" {{{ ToggleCallBack
function! s:ToggleCallBack()
    if g:atp_callback
	echomsg "call back is off"
	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [on]
	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [off]
	nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [off]<Tab>g:atp_callback	
		    \ :call ToggleCallBack()<CR>
    else
	echomsg "call back is on"
	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [on]
	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [off]
	nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [on]<Tab>g:atp_callback
		    \ :call ToggleCallBack()<CR>
    endif
    let g:atp_callback=!g:atp_callback
endfunction
command! -buffer 	ToggleCallBack 		:call <SID>ToggleCallBack()
nnoremap <silent> <Plug>ToggleCallBack		:ToggleCallBack
"}}}
" {{{ ToggleDebugMode
" ToDo: to doc.
" TODO: it would be nice to have this command (and the map) in quickflist (FileType qf)
" describe DEBUG MODE in doc properly.
function! s:ToggleDebugMode()
"     call ToggleCallBack()
    if t:atp_DebugMode == "debug"
	echomsg "debug mode is off"

	silent! aunmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [on]
	silent! aunmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [off]
	nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [off]<Tab>t:atp_DebugMode			
		    \ :ToggleDebugMode<CR>

	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [on]
	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [off]
	nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [off]<Tab>g:atp_callback	
		    \ :ToggleDebugMode<CR>

	let t:atp_DebugMode	= g:atp_DefaultDebugMode
	silent cclose
    else
	echomsg "debug mode is on"

	silent! aunmenu 550.20.5 LaTeX.Log.Toggle\ Debug\ Mode\ [off]
	silent! aunmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [on]
	nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [on]<Tab>t:atp_DebugMode
		    \ :ToggleDebugMode<CR>

	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [on]
	silent! aunmenu LaTeX.Toggle\ Call\ Back\ [off]
	nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [on]<Tab>g:atp_callback	
		    \ :ToggleDebugMode<CR>

	let g:atp_callback=1
	let t:atp_DebugMode	= "debug"
	silent copen
    endif
endfunction
command! -buffer 	ToggleDebugMode 	:call <SID>ToggleDebugMode()
nnoremap <silent> <Plug>ToggleDebugMode		:ToggleDebugMode
if !s:did_options
    au FileType qf command! -buffer ToggleDebugMode 		:call <SID>ToggleDebugMode()
    au FileType qf nnoremap <silent> <LocalLeader>D		:ToggleDebugMode<CR>
endif
" }}}
" {{{ ToggleTab
" switches on/off the <Tab> map for TabCompletion
function! s:ToggleTab() 
    if mapcheck('<F7>','i') !~ 'atplib#TabCompletion'
	if mapcheck('<Tab>','i') =~ 'atplib#TabCompletion'
	    iunmap <buffer> <Tab>
	    let l:map=0
	else
	    let l:map=1
	    imap <buffer> <Tab> <C-R>=atplib#TabCompletion(1)<CR>
	endif
" 	if mapcheck('<Tab>','n') =~ 'atplib#TabCompletion'
" 	    nunmap <buffer> <Tab>
" 	else
" 	    imap <buffer> <Tab> <C-R>=atplib#TabCompletion(1,1)<CR>
" 	endif
	if l:map 
	    echo '<Tab> map turned on'
	else
	    echo '<Tab> map turned off'
	endif
    endif
endfunction
command! -buffer 	ToggleTab	 	:call <SID>ToggleTab()
nnoremap <silent> <Plug>ToggleTab		:ToggleTab
" }}}
endif
"}}}

" Tab Completion variables
" {{{ TAB COMPLETION variables
" ( functions are in autoload/atplib.vim )
"
let g:atp_completion_modes=[ 
	    \ 'commands', 		'labels', 		
	    \ 'tikz libraries', 	'environment names',
	    \ 'close environments' , 	'brackets',
	    \ 'input files',		'bibstyles',
	    \ 'bibitems', 		'bibfiles',
	    \ 'documentclass',		'tikzpicture commands',
	    \ 'tikzpicture',		'tikzpicture keywords',
	    \ 'package names',		'font encoding',
	    \ 'font family',		'font series',
	    \ 'font shape' ]
let g:atp_completion_modes_normal_mode=[ 
	    \ 'close environments' , 	'brackets' ]

" By defualt all completion modes are ative.
if !exists("g:atp_completion_active_modes")
    let g:atp_completion_active_modes=deepcopy(g:atp_completion_modes)
endif
if !exists("g:atp_completion_active_modes_normal_mode")
    let g:atp_completion_active_modes_normal_mode=deepcopy(g:atp_completion_modes_normal_mode)
endif

" Note: to remove completions: 'inline_math' or 'displayed_math' one has to
" remove also: 'close_environments' /the function atplib#CloseLastEnvironment can
" close math instead of an environment/.

" ToDo: make list of complition commands from the input files.
" ToDo: make complition fot \cite, and for \ref and \eqref commands.

" ToDo: there is second such a list! line 3150
	let g:atp_Environments=['array', 'abstract', 'center', 'corollary', 
		\ 'definition', 'document', 'description',
		\ 'enumerate', 'example', 'eqnarray', 
		\ 'flushright', 'flushleft', 'figure', 'frontmatter', 
		\ 'keywords', 
		\ 'itemize', 'lemma', 'list', 'notation', 'minipage', 
		\ 'proof', 'proposition', 'picture', 'theorem', 'tikzpicture',  
		\ 'tabular', 'table', 'tabbing', 'thebibliography', 'titlepage',
		\ 'quotation', 'quote',
		\ 'remark', 'verbatim', 'verse' ]

	let g:atp_amsmath_environments=['align', 'alignat', 'equation', 'gather',
		\ 'multiline', 'split', 'substack', 'flalign', 'smallmatrix', 'subeqations',
		\ 'pmatrix', 'bmatrix', 'Bmatrix', 'vmatrix' ]

	" if short name is no_short_name or '' then both means to do not put
	" anything, also if there is no key it will not get a short name.
	let g:atp_shortname_dict = { 'theorem' : 'thm', 
		    \ 'proposition' : 'prop', 	'definition' : 'defi',
		    \ 'lemma' : 'lem',		'array' : 'ar',
		    \ 'abstract' : 'no_short_name',
		    \ 'tikzpicture' : 'tikz',	'tabular' : 'table',
		    \ 'table' : 'table', 	'proof' : 'pr',
		    \ 'corollary' : 'cor',	'enumerate' : 'enum',
		    \ 'example' : 'ex',		'itemize' : 'it',
		    \ 'item'	: 'itm',
		    \ 'remark' : 'rem',		'notation' : 'not',
		    \ 'center' : '', 		'flushright' : '',
		    \ 'flushleft' : '', 	'quotation' : 'quot',
		    \ 'quot' : 'quot',		'tabbing' : '',
		    \ 'picture' : 'pic',	'minipage' : '',	
		    \ 'list' : 'list',		'figure' : 'fig',
		    \ 'verbatim' : 'verb', 	'verse' : 'verse',
		    \ 'thebibliography' : '',	'document' : 'no_short_name',
		    \ 'titlepave' : '', 	'align' : 'eq',
		    \ 'alignat' : 'eq',		'equation' : 'eq',
		    \ 'gather'  : 'eq', 	'multiline' : '',
		    \ 'split'	: 'eq', 	'substack' : '',
		    \ 'flalign' : 'eq',
		    \ 'part'	: 'prt',	'chapter' : 'chap',
		    \ 'section' : 'sec',	'subsection' : 'ssec',
		    \ 'subsubsection' : 'sssec', 'paragraph' : 'par',
		    \ 'subparagraph' : 'spar' }

	" ToDo: Doc.
	" Usage: \label{l:shorn_env_name . g:atp_separator
	if !exists("g:atp_separator")
	    let g:atp_separator=':'
	endif
	if !exists("g:atp_no_separator")
	    let g:atp_no_separator = 0
	endif
	if !exists("g:atp_no_short_names")
	    let g:atp_env_short_names = 1
	endif
	" the separator will not be put after the environments in this list:  
	" the empty string is on purpose: to not put separator when there is
	" no name.
	let g:atp_no_separator_list=['', 'titlepage']

" 	let g:atp_package_list=sort(['amsmath', 'amssymb', 'amsthm', 'amstex', 
" 	\ 'babel', 'booktabs', 'bookman', 'color', 'colorx', 'chancery', 'charter', 'courier',
" 	\ 'enumerate', 'euro', 'fancyhdr', 'fancyheadings', 'fontinst', 
" 	\ 'geometry', 'graphicx', 'graphics',
" 	\ 'hyperref', 'helvet', 'layout', 'longtable',
" 	\ 'newcent', 'nicefrac', 'ntheorem', 'palatino', 'stmaryrd', 'showkeys', 'tikz',
" 	\ 'qpalatin', 'qbookman', 'qcourier', 'qswiss', 'qtimes', 'verbatim', 'wasysym'])

	" the command \label is added at the end.
	let g:atp_Commands=["\\begin{", "\\end{", 
	\ "\\cite{", "\\nocite{", "\\ref{", "\\pageref{", "\\eqref{", "\\bibitem", "\\item",
	\ "\\emph{", "\\documentclass{", "\\usepackage{",
	\ "\\section{", "\\subsection{", "\\subsubsection{", "\\part{", 
	\ "\\chapter{", "\\appendix", "\\subparagraph", "\\paragraph",
	\ "\\textbf{", "\\textsf{", "\\textrm{", "\\textit{", "\\texttt{", 
	\ "\\textsc{", "\\textsl{", "\\textup{", "\\textnormal", "\\textcolor{",
	\ "\\bfseries", "\\mdseries",
	\ "\\tiny",  "\\scriptsize", "\\footnotesize", "\\small",
	\ "\\noindent", "\\normalfont", "\normalsize", "\\normalsize", "\\normal", 
	\ "\\large", "\\Large", "\\LARGE", "\\huge", "\\HUGE",
	\ "\\usefont{", "\\fontsize{", "\\selectfont", "\\fontencoding{", "\\fontfamiliy{", "\\fontseries{", "\\fontshape{",
	\ "\\rmdefault", "\\sfdefault", "\\ttdefault", "\\bfdefault", "\\mddefault", "\\itdefault",
	\ "\\sldefault", "\\scdefault", "\\updefault",  "\\renewcommand{", "\\newcommand{",
	\ "\\addcontentsline{", "\\addtocontents",
	\ "\\input", "\\include", "\\includeonly", "\\inlucegraphics",  
	\ "\\savebox", "\\sbox", "\\usebox", "\\rule", "\\raisebox{", 
	\ "\\parbox{", "\\mbox{", "\\makebox{", "\\framebox{", "\\fbox{",
	\ "\\bigskip", "\\medskip", "\\smallskip", "\\vfill", "\\vspace{", 
	\ "\\hspace", "\\hrulefill", "\\hfill", "\\dots", "\\dotfill",
	\ "\\thispagestyle", "\\mathnormal", "\\markright", "\\pagestyle", "\\pagenumbering",
	\ "\\author{", "\\date{", "\\thanks{", "\\title{",
	\ "\\maketitle", "\\overbrace{", "\\underbrace{",
	\ "\\marginpar", "\\indent", "\\par", "\\sloppy", "\\pagebreak", "\\nopagebreak",
	\ "\\newpage", "\\newline", "\\newtheorem{", "\\linebreak", "\\hyphenation{", "\\fussy",
	\ "\\enlagrethispage{", "\\clearpage", "\\cleardoublepage",
	\ "\\caption{",
	\ "\\opening{", "\\name{", "\\makelabels{", "\\location{", "\\closing{", "\\address{", 
	\ "\\signature{", "\\stopbreaks", "\\startbreaks",
	\ "\\newcounter{", "\\refstepcounter{", 
	\ "\\roman{", "\\Roman{", "\\stepcounter{", "\\setcounter{", 
	\ "\\usecounter{", "\\value{", 
	\ "\\newlength{", "\\setlength{", "\\addtolength{", "\\settodepth{", 
	\ "\\settoheight{", "\\settowidth{", 
	\ "\\width", "\\height", "\\depth", "\\totalheight",
	\ "\\footnote{", "\\footnotemark", "\\footnotetetext", 
	\ "\\bibliography{", "\\bibliographystyle{", 
	\ "\\flushbottom", "\\onecolumn", "\\raggedbottom", "\\twocolumn",  
	\ "\\alph{", "\\Alph{", "\\arabic{", "\\fnsymbol{", "\\reversemarginpar",
	\ "\\exhyphenpenalty",
	\ "\\topmargin", "\\oddsidemargin", "\\evensidemargin", "\\headheight", "\\headsep", 
	\ "\\textwidth", "\\textheight", "\\marginparwidth", "\\marginparsep", "\\marginparpush", "\\footskip", "\\hoffset",
	\ "\\voffset", "\\paperwidth", "\\paperheight", "\\theequation", "\\thepage", "\\usetikzlibrary{",
	\ "\\tableofcontents", "\\newfont{", 
	\ "\\DeclareRobustCommand", "\\show", "\\CheckCommand" ]
	
	let g:atp_picture_commands=[ "\\put", "\\circle", "\\dashbox", "\\frame{", 
		    \"\\framebox(", "\\line(", "\\linethickness{",
		    \ "\\makebox(", "\\\multiput(", "\\oval(", "\\put", 
		    \ "\\shortstack", "\\vector(" ]

	" ToDo: end writting layout commands. 
	" ToDo: MAKE COMMANDS FOR PREAMBULE.

	let g:atp_math_commands=["\\forall", "\\exists", "\\emptyset", "\\aleph", "\\partial",
	\ "\\nabla", "\\Box", "\\Diamond", "\\bot", "\\top", "\\flat", "\\sharp",
	\ "\\mathbf{", "\\mathsf{", "\\mathrm{", "\\mathit{", "\\mathbb{", "\\mathtt{", "\\mathcal{", 
	\ "\\mathop{", "\\limits", "\\text{", "\\leqslant", "\\leq", "\\geqslant", "\\geq",
	\ "\\gtrsim", "\\lesssim", "\\gtrless", "\\left", "\\right", 
	\ "\\rightarrow", "\\Rightarrow", "\\leftarrow", "\\Leftarrow", "\\iff", 
	\ "\\leftrightarrow", "\\Leftrightarrow", "\\downarrow", "\\Downarrow", "\\Uparrow",
	\ "\\Longrightarrow", "\\longrightarrow", "\\Longleftarrow", "\\longleftarrow",
	\ "\\overrightarrow{", "\\overleftarrow{", "\\underrightarrow{", "\\underleftarrow{",
	\ "\\uparrow", "\\nearrow", "\\searrow", "\\swarrow", "\\nwarrow", 
	\ "\\hookrightarrow", "\\hookleftarrow", "\\gets", 
	\ "\\sum", "\\bigsum", "\\cup", "\\bigcup", "\\cap", "\\bigcap", 
	\ "\\prod", "\\coprod", "\\bigvee", "\\bigwedge", "\\wedge",  
	\ "\\oplus", "\\otimes", "\\odot", "\\oint",
	\ "\\int", "\\bigoplus", "\\bigotimes", "\\bigodot", "\\times",  
	\ "\\smile", "\\frown", "\\subset", "\\subseteq", "\\supset", "\\supseteq",
	\ "\\dashv", "\\vdash", "\\vDash", "\\Vdash", "\\models", "\\sim", "\\simeq", 
	\ "\\prec", "\\preceq", "\\preccurlyeq", "\\precapprox",
	\ "\\succ", "\\succeq", "\\succcurlyeq", "\\succapprox", "\\approx", 
	\ "\\thickapprox", "\\conq", "\\bullet", 
	\ "\\lhd", "\\unlhd", "\\rhd", "\\unrhd", "\\dagger", "\\ddager", "\\dag", "\\ddag", 
	\ "\\ldots", "\\cdots", "\\vdots", "\\ddots", 
	\ "\\vartriangleright", "\\vartriangleleft", "\\trianglerighteq", "\\trianglelefteq",
	\ "\\copyright", "\\textregistered", "\\puonds",
	\ "\\big", "\\Big", "\\Bigg", "\\huge", 
	\ "\\bigr", "\\Bigr", "\\biggr", "\\Biggr",
	\ "\\bigl", "\\Bigl", "\\biggl", "\\Biggl",
	\ "\\hat", "\\grave", "\\bar", "\\acute", "\\mathring", "\\check", "\\dot", "\\vec", "\\breve",
	\ "\\tilde", "\\widetilde " , "\\widehat", "\\ddot", 
	\ "\\sqrt", "\\frac{", "\\binom{", "\\cline", "\\vline", "\\hline", "\\multicolumn{", 
	\ "\\nouppercase", "\\sqsubset", "\\sqsupset", "\\square", "\\blacksquare", "\\triangledown", "\\triangle", 
	\ "\\diagdown", "\\diagup", "\\nexists", "\\varnothing", "\\Bbbk", "\\circledS", 
	\ "\\complement", "\\hslash", "\\hbar", 
	\ "\\eth", "\\rightrightarrows", "\\leftleftarrows", "\\rightleftarrows", "\\leftrighrarrows", 
	\ "\\downdownarrows", "\\upuparrows", "\\rightarrowtail", "\\leftarrowtail", 
	\ "\\twoheadrightarrow", "\\twoheadleftarrow", "\\rceil", "\\lceil", "\\rfloor", "\\lfloor", 
	\ "\\bullet", "\\bigtriangledown", "\\bigtriangleup", "\\ominus", "\\bigcirc", "\\amalg", 
	\ "\\setminus", "\\sqcup", "\\sqcap", 
	\ "\\lnot", "\\notin", "\\neq", "\\smile", "\\frown", "\\equiv", "\\perp",
	\ "\\quad", "\\qquad", "\\stackrel", "\\displaystyle", "\\textstyle", "\\scriptstyle", "\\scriptscriptstyle",
	\ "\\langle", "\\rangle" ]

	" commands defined by the user in input files.
	" ToDo: to doc.
	" ToDo: this doesn't work with input files well enough. 
	
	" Returns a list of two lists:  [ commanad_names, enironment_names ]

	" The BufEnter augroup doesn't work with EditInputFile, but at least it works
	" when entering. Debuging shows that when entering new buffer it uses
	" wrong b:atp_MainFile, it is still equal to the bufername and not the
	" real main file. Maybe it is better to use s:mainfile variable.

	if !exists("g:atp_local_completion")
	    let g:atp_local_completion = 1
	endif


	let g:atp_math_commands_non_expert_mode=[ "\\leqq", "\\geqq", "\\succeqq", "\\preceqq", 
		    \ "\\subseteqq", "\\supseteqq", "\\gtrapprox", "\\lessapprox" ]
	 
	" requiers amssymb package:
	let g:atp_ams_negations=[ "\\nless", "\\ngtr", "\\lneq", "\\gneq", "\\nleq", "\\ngeq", "\\nleqslant", "\\ngeqslant", 
		    \ "\\nsim", "\\nconq", "\\nvdash", "\\nvDash", 
		    \ "\\nsubseteq", "\\nsupseteq", 
		    \ "\\varsubsetneq", "\\subsetneq", "\\varsupsetneq", "\\supsetneq", 
		    \ "\\ntriangleright", "\\ntriangleleft", "\\ntrianglerighteq", "\\ntrianglelefteq", 
		    \ "\\nrightarrow", "\\nleftarrow", "\\nRightarrow", "\\nLeftarrow", 
		    \ "\\nleftrightarrow", "\\nLeftrightarrow", "\\nsucc", "\\nprec", "\\npreceq", "\\nsucceq", 
		    \ "\\precneq", "\\succneq", "\\precnapprox" ]

	let g:atp_ams_negations_non_expert_mode=[ "\\lneqq", "\\ngeqq", "\\nleqq", "\\ngeqq", "\\nsubseteqq", 
		    \ "\\nsupseteqq", "\\subsetneqq", "\\supsetneqq", "\\nsucceqq", "\\precneqq", "\\succneqq" ] 

	" ToDo: add more amsmath commands.
	let g:atp_amsmath_commands=[ "\\boxed", "\\intertext", "\\multiligngap", "\\shoveleft", "\\shoveright", "\\notag", "\\tag", 
		    \ "\\raistag{", "\\displaybreak", "\\allowdisplaybreaks", "\\numberwithin{",
		    \ "\\hdotsfor{" , "\\mspace{",
		    \ "\\negthinspace", "\\negmedspace", "\\negthickspace", "\\thinspace", "\\medspace", "\\thickspace",
		    \ "\\leftroot{", "\\uproot{", "\\overset{", "\\underset{", "\\sideset{", 
		    \ "\\dfrac{", "\\tfrac{", "\\cfrac{", "\\dbinom{", "\\tbinom{", "\\smash",
		    \ "\\lvert", "\\rvert", "\\lVert", "\\rVert", "\\DeclareMatchOperator{",
		    \ "\\arccos", "\\arcsin", "\\arg", "\\cos", "\\cosh", "\\cot", "\\coth", "\\csc", "\\deg", "\\det",
		    \ "\\dim", "\\exp", "\\gcd", "\\hom", "\\inf", "\\injlim", "\\ker", "\\lg", "\\lim", "\\liminf", "\\limsup",
		    \ "\\log", "\\min", "\\max", "\\Pr", "\\projlim", "\\sec", "\\sin", "\\sinh", "\\sup", "\\tan", "\\tanh",
		    \ "\\varlimsup", "\\varliminf", "\\varinjlim", "\\varprojlim", "\\mod", "\\bmod", "\\pmod", "\\pod", "\\sideset",
		    \ "\\iint", "\\iiint", "\\iiiint", "\\idotsint",
		    \ "\\varGamma", "\\varDelta", "\\varTheta", "\\varLambda", "\\varXi", "\\varPi", "\\varSigma", 
		    \ "\\varUpsilon", "\\varPhi", "\\varPsi", "\\varOmega" ]
	
	" ToDo: integrate in TabCompletion (amsfonts, euscript packages).
	let g:atp_amsfonts=[ "\\mathfrak", "\\mathscr" ]

	" not yet supported: in TabCompletion:
	let g:atp_amsxtra_commands=[ "\\sphat", "\\sptilde" ]
	let g:atp_fancyhdr_commands=["\\lfoot{", "\\rfoot{", "\\rhead{", "\\lhead{", 
		    \ "\\cfoot{", "\\chead{", "\\fancyhead{", "\\fancyfoot{",
		    \ "\\fancypagestyle{", "\\fancyhf{}", "\\headrulewidth", "\\footrulewidth",
		    \ "\\rightmark", "\\leftmark", "\\markboth", 
		    \ "\\chaptermark", "\\sectionmark", "\\subsectionmark",
		    \ "\\fancyheadoffset", "\\fancyfootoffset", "\\fancyhfoffset"]


	" ToDo: remove tikzpicture from above and integrate the
	" tikz_envirnoments variable
	" \begin{pgfonlayer}{background} (complete the second argument as
	" well}
	"
	" Tikz command cuold be accitve only in tikzpicture and after \tikz
	" command! There is a way to do that.
	" 
	let g:atp_tikz_environments=['tikzpicture', 'scope', 'pgfonlayer', 'background' ]
	" ToDo: this should be completed as packages.
	let g:atp_tikz_libraries=sort(['arrows', 'automata', 'backgrounds', 'calc', 'calendar', 'chains', 'decorations', 
		    \ 'decorations.footprints', 'decorations.fractals', 
		    \ 'decorations.markings', 'decorations.pathmorphing', 
		    \ 'decorations.replacing', 'decorations.shapes', 
		    \ 'decorations.text', 'er', 'fadings', 'fit',
		    \ 'folding', 'matrix', 'mindmap', 'scopes', 
		    \ 'patterns', 'pteri', 'plothandlers', 'plotmarks', 
		    \ 'plcaments', 'pgflibrarypatterns', 'pgflibraryshapes',
		    \ 'pgflibraryplotmarks', 'positioning', 'replacements', 
		    \ 'shadows', 'shapes.arrows', 'shapes.callout', 'shapes.geometric', 
		    \ 'shapes.gates.logic.IEC', 'shapes.gates.logic.US', 'shapes.misc', 
		    \ 'shapes.multipart', 'shapes.symbols', 'topaths', 'through', 'trees' ])
	" tikz keywords = begin without '\'!
	" ToDo: add mote keywords: done until page 145.
	" ToDo: put them in a correct order!!!
	let g:atp_tikz_keywords=[ 'draw', 'node', 'matrix', 'anchor', 'top', 'bottom',  
		    \ 'west', 'east', 'north', 'south', 'at', 'thin', 'thick', 'semithick', 'rounded', 'corners',
		    \ 'controls', 'and', 'circle', 'step', 'grid', 'very', 'style', 'line', 'help',
		    \ 'color', 'arc', 'curve', 'scale', 'parabola', 'line', 'ellipse', 'bend', 'sin', 'rectangle', 'ultra', 
		    \ 'right', 'left', 'intersection', 'xshift', 'yshift', 'shift', 'near', 'start', 'above', 'below', 
		    \ 'end', 'sloped', 'coordinate', 'cap', 'shape', 'transition', 'place', 'label', 'every', 
		    \ 'edge', 'point', 'loop', 'join', 'distance', 'sharp', 'rotate', 'blue', 'red', 'green', 'yellow', 
		    \ 'black', 'white', 'gray',
		    \ 'text', 'width', 'inner', 'sep', 'baseline', 'current', 'bounding', 'box', 
		    \ 'canvas', 'polar', 'radius', 'barycentric', 'angle', 'opacity', 
		    \ 'solid', 'phase', 'loosly', 'dashed', 'dotted' , 'densly', 
		    \ 'latex', 'diamond', 'double', 'smooth', 'cycle', 'coordinates', 'distance',
		    \ 'even', 'odd', 'rule', 'pattern', 
		    \ 'stars', 'fivepointed', 'shading', 'ball', 'axis', 'middle', 'outer', 'transorm',
		    \ 'fading', 'horizontal', 'vertical', 'light', 'crosshatch', 'button', 'postaction', 'out',
		    \ 'circular', 'shadow', 'scope', 'borders', 'spreading', 'false', 'position' ]
	let g:atp_tikz_library_arrows_keywords=[ "reversed'", "stealth'", 'triangle', 'open', 
		    \ 'hooks', 'round', 'fast', 'cap', 'butt'] 
	let g:atp_tikz_library_automata_keywords=[ 'state', 'accepting', 'initial', 'swap', 'edge',
		    \ 'loop', 'nodepart', 'lower', 'output']  
	let g:atp_tikz_library_backgrounds_keywords=[ 'background', 'show', 'inner', 'frame', 'framed',
		    \ 'tight', 'loose', 'xsep', 'ysep']

	" NEW:
	let g:atp_tikz_library_calendar=[ '\calendar', '\tikzmonthtext' ]
	let g:atp_tikz_library_calendar_keywords=[ 'week list', 'dates', 'day', 'day list', 'month', 'year', 'execute', 
		    \ 'before', 'after', 'downward', 'upward' ]
	let g:atp_tikz_library_chain=[ '\chainin' ]
	let g:atp_tikz_library_chain_keywords=[ 'chain', 'start chain', 'on chain', 'continue chain', 
		    \ 'start branch', 'branch', 'going', 'numbers', 'greek' ]
" 	let g:atp_tikz_library_decoration=[]
	let g:atp_tikz_library_decoration_keywords=[ 'decorate', 'decoration', 'lineto', 'straight', 'zigzag',
		    \ 'saw', 'random steps', 'bent', 'aspect', 'bumps', 'coil', 'curveto', 'snake', 
		    \ 'border', 'brace', 'segment lenght', 'waves', 'ticks', 'expanding', 
		    \ 'crosses', 'triangles', 'dart', 'shape sep', 'shape backgrounds', 'between', 'text along path', 
		    \ 'Koch curve type 1', 'Koch curve type 1', 'Koch snowflake', 'Cantor set', 'footprints',
		    \ 'foot lenght',  'stride lenght', 'foot sep', 'foot angle', 'foot of', 'gnome', 'human', 
		    \ 'bird', 'felis silvestris' ]
	" for tikz keywords we can complete sentences, like 'matrix of
	" math nodes'!
	let g:atp_tikz_library_matrix_keywords=['matrix of nodes', 'matrix of math nodes', 'nodes', 'delimiter', 
		    \ 'rmoustache', 'column sep=', 'row sep=' ] 
	" ToDo: completion for arguments in brackets [] for tikz commands.
	let g:atp_tikz_commands=[ "\\begin", "\\end", "\\matrix", "\\node", "\\shadedraw", 
		    \ "\\draw", "\\tikz", "\\tikzset",
		    \ "\\path", "\\filldraw", "\\fill", "\\clip", "\\drawclip", "\\foreach", "\\angle", "\\coordinate",
		    \ "\\useasboundingbox", "\\tikztostart", "\\tikztotarget", "\\tikztonodes", "\\tikzlastnode",
		    \ "\\pgfextra", "\\endpgfextra", "\\verb", "\\coordinate", 
		    \ "\\pattern", "\\shade", "\\shadedraw", "\\colorlet", "\\definecolor" ]

" ToDo: to doc.
" adding commands to completion list whether to check or not if we are in the
" correct environment (for example \tikz or \begin{tikzpicture})
if !exists("g:atp_check_if_opened")
    let g:atp_check_if_opened=1
endif
" This is as the above, but works only if one uses \(:\), \[:\]
if !exists("g:atp_math_opened")
    if search('\%([^\\]\|^\)\$\$\?','wnc') != 0
	let g:atp_math_opened=0
    else
	let g:atp_math_opened=1
    endif
endif
" ToDo: Think about even better math modes patterns.
" \[ - math mode \\[ - not mathmode (this can be at the end of a line as: \\[3pt])
" \\[ - this is math mode, but tex will complain (now I'm not matching it,
" that's maybe good.) 
" How to deal with $:$ (they are usually in one line, we could count them)  and $$:$$ 
" matchpair

let g:atp_math_modes=[ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)(','\%([^\\]\|^\)\%(\\\|\\\{3}\)\zs)'],
	    \ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)\[','\%([^\\]\|^\)\%(\\\|\\\{3}\)\zs\]'],	
	    \ ['\\begin{align', '\\end{alig\zsn'], 	['\\begin{gather', '\\end{gathe\zsr'], 
	    \ ['\\begin{falign', '\\end{flagi\zsn'], 	['\\begin[multiline', '\\end{multilin\zse'],
	    \ ['\\begin{equation', '\\end{equatio\zsn'],
	    \ ['\\begin{\%(display\)\?math', '\\end{\%(display\)\?mat\zsh'] ] 
" ToDo: user command list, env list g:atp_Commands, g:atp_Environments, 
" }}}
"
" Some of the autocommands (Status Line, LocalCommands, Log File):
" {{{ Autocommands:


if !s:did_options

    if (exists("g:atp_statusline") && g:atp_statusline == '1') || !exists("g:atp_statusline")
	 au BufWinEnter 	*.tex 	call ATPStatus()
    endif

    if g:atp_local_completion == 2 
	au BufEnter *.tex call LocalCommands()
    endif

    au FileType *tex let b:atp_TexFlavour = &filetype
    
    " Experimental:
	" This doesn't work !
" 	    let g:debug=0
" 	    fun! GetSynStackI()
" 		let synstack=[]
" 		let synstackI=synstack(line("."), col("."))
" 		try 
" 		    let test =  synstackI == 0
" 		    let b:return 	= 1
" 		    catch /Can only compare List with List/
" 		    let b:return	= 0
" 		endtry
" 		if b:return == 0
" 		    return []
" 		else
" 		    let g:debug+= 1
" 		    return map(synstack, "synIDattr(v:val, 'name')")
" 		endif
" 	    endfunction

    " The first one is not working! (which is the more important of these two :(
"     au CursorMovedI *.tex let g:atp_synstackI	= GetSynStackI()
    " This has problems in visual mode:
"     au CursorMoved  *.tex let g:atp_synstack	= map(synstack(line('.'), col('.')), "synIDattr(v:val, 'name')")
    
endif
" }}}


" This function and the following autocommand toggles the textwidth option if
" editting a math mode. Currently, supported are $:$, \(:\), \[:\] and $$:$$.
" {{{  SetMathVimOptions

if !exists("g:atp_SetMathVimOptions")
    let g:atp_SetMathVimOptions 	= 1
endif

if !exists("g:atp_MathVimOptions")
"     { 'option_name' : [ val_in_math, normal_val], ... }
    let g:atp_MathVimOptions 		=  { 'textwidth' 	: [ 0, 	&textwidth],
						\ }
endif

let g:atp_MathZones	= [ 
	    		\ 'texMathZoneV', 	'texMathZoneW', 
	    		\ 'texMathZoneX', 	'texMathZoneY',
	    		\ 'texMathZoneA', 	'texMathZoneAS',
	    		\ 'texMathZoneB', 	'texMathZoneBS',
	    		\ 'texMathZoneC', 	'texMathZoneCS',
	    		\ 'texMathZoneD', 	'texMathZoneDS',
	    		\ 'texMathZoneE', 	'texMathZoneES',
	    		\ 'texMathZoneF', 	'texMathZoneFS',
	    		\ 'texMathZoneG', 	'texMathZoneGS',
	    		\ 'texMathZoneH', 	'texMathZoneHS',
	    		\ 'texMathZoneI', 	'texMathZoneIS',
	    		\ 'texMathZoneJ', 	'texMathZoneJS',
	    		\ 'texMathZoneK', 	'texMathZoneKS',
	    		\ 'texMathZoneL', 	'texMathZoneLS' 
			\ ]

" a:0 	= 0 check if in math mode
" a:1   = 0 assume cursor is not in math
" a:1	= 1 assume cursor stands in math  
function! s:SetMathVimOptions(...)

	if !g:atp_SetMathVimOptions
	    return "no setting to toggle" 
	endif

	let MathZones = copy(g:atp_MathZones)
	if b:atp_TexFlavour == 'plaintex'
	    call add(MathZones, 'texMathZoneY')
	endif
	    
	" Change the long values to numbers 
	let MathVimOptions = map(copy(g:atp_MathVimOptions),
			\ " v:val[0] =~ v:key ? [ v:val[0] =~ 'no' . v:key ? 0 : 1, v:val[1] ] : v:val " )
	let MathVimOptions = map(MathVimOptions,
			\ " v:val[1] =~ v:key ? [ v:val[0], v:val[1] =~ 'no' . v:key ? 0 : 1 ] : v:val " )

	" check if the current (and 3 steps back) cursor position is in math
	" or use a:1
	let check	= a:0 == 0 ? atplib#CheckSyntaxGroups(MathZones) + atplib#CheckSyntaxGroups(MathZones, line("."), max([ 1, col(".")-3])) : a:1

	if check
	    for option_name in keys(MathVimOptions)
		execute "let &l:".option_name. " = " . MathVimOptions[option_name][0]
	    endfor
	else
	    for option_name in keys(MathVimOptions)
		execute "let &l:".option_name. " = " . MathVimOptions[option_name][1]
	    endfor
	endif

endfunction

if !s:did_options

    " if leaving the insert mode set the non-math options
    au InsertLeave 	*.tex :call s:SetMathVimOptions(0)
    " if entering the insert mode or in the insert mode check if the cursor is in
    " math or not and set the options acrodingly
    au InsertEnter	*.tex :call s:SetMathVimOptions()
    au CursorMovedI 	*.tex :call s:SetMathVimOptions()
"   This makes vim slow down when moving cursor:
"     au CursorMoved 	*.tex :call s:SetMathVimOptions()

endif
"}}}

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/search.vim	[[[1
356
" Author:	Marcin Szamotulski
" Note:		This file is a part of ATP plugin for vim.



" Make a dictionary of definitions found in all input files.
" {{{ s:make_defi_dict
" Comparing with ]D, ]d, ]i, ]I vim maps this function deals with multiline
" definitions.
"
" The output dictionary is of the form: 
" 	{ input_file : [ [begin_line, end_line], ... ] }
" a:1 	= buffer name to search in for input files
" a:2	= 1 	skip searching for the end_line
"
" ToDo: it is possible to check for the end using searchpairpos, but it
" operates on a list not on a buffer.
function! s:make_defi_dict(...)

    if a:0 >= 1
	let l:bufname=a:1
    else
	let l:bufname=bufname("%")
    endif

    " pattern to match the definitions this function is also used to fine
    " \newtheorem, and \newenvironment commands  
    if a:0 >= 2	
	let l:pattern = a:2
    else
	let l:pattern = '\\def\|\\newcommand'
    endif

    if a:0 >= 3
	let l:preambule_only=a:3
    else
	let l:preambule_only=1
    endif

    " this is still to slow!
    if a:0 >= 4
	let l:only_begining=a:4
    else
	let l:only_begining=0
    endif

    let l:defi_dict={}

    let l:inputfiles=FindInputFiles(l:bufname,"0")
    let l:input_files=[]
    let b:input_files=l:input_files

    for l:inputfile in keys(l:inputfiles)
	if l:inputfiles[l:inputfile][0] != "bib"
	    let l:input_file=atplib#append(l:inputfile,'.tex')
	    if filereadable(atplib#append(b:atp_OutDir,'/') . l:input_file)
		let l:input_file=atplib#append(b:atp_OutDir,'/') . l:input_file
	    else
		let l:input_file=findfile(l:inputfile,g:texmf . '**')
	    endif
	    call add(l:input_files, l:input_file)
	endif
    endfor


    let l:input_files=filter(l:input_files, 'v:val != ""')
    if !count(l:input_files,b:atp_MainFile)
	call extend(l:input_files,[ b:atp_MainFile ])
    endif

    if len(l:input_files) > 0
    for l:inputfile in l:input_files
	let l:defi_dict[l:inputfile]=[]
	" do not search for definitions in bib files 
	"TODO: it skips lines somehow. 
	let l:ifile=readfile(l:inputfile)
	
	" search for definitions
	let l:lnr=1
	while (l:lnr <= len(l:ifile) && (!l:preambule_only || l:ifile[l:lnr-1] !~ '\\begin\s*{document}'))
" 	    echo l:lnr . " " . l:inputfile . " " . l:ifile[l:lnr-1] !~ '\\begin\s*{document}'

	    let l:match=0

	    let l:line=l:ifile[l:lnr-1]
	    if substitute(l:line,'%.*','','') =~ l:pattern

		let l:b_line=l:lnr

		let l:lnr+=1	
		if !l:only_begining
		    let l:open=atplib#count(l:line,'{')    
		    let l:close=atplib#count(l:line,'}')
		    while l:open != l:close
			"go to next line and count if the definition ends at
			"this line
			let l:line=l:ifile[l:lnr-1]
			let l:open+=atplib#count(l:line,'{')    
			let l:close+=atplib#count(l:line,'}')
			let l:lnr+=1	
		    endwhile
		    let l:e_line=l:lnr-1
		    call add(l:defi_dict[l:inputfile], [ l:b_line, l:e_line ])
		else
		    call add(l:defi_dict[l:inputfile], [ l:b_line ])
		endif
	    else
		let l:lnr+=1
	    endif
	endwhile
    endfor
    endif

    return l:defi_dict
endfunction
"}}}

" Find all names of locally defined commands, colors and environments. 
" Used by the completion function.
"{{{ LocalCommands 
function! LocalCommands(...)

    if a:0 == 0
	let l:pattern='\\def\>\|\\newcommand\>\|\\newenvironment\|\\newtheorem\|\\definecolor'
    else
	let l:pattern=a:1
    endif
    echo "Makeing lists of commands and environments found in input files ... "
" 	    call s:setprojectname()
    let l:CommandNames=[]
    let l:EnvironmentNames=[]
    let l:ColorNames=[]

    " ToDo: I need a simpler function here !!!
    " 		we are just looking for definition names not for
    " 		definition itself (this takes time).
    let l:ddict=s:make_defi_dict(b:atp_MainFile,l:pattern,1,1)
" 	    echomsg " LocalCommands DEBUG " . b:atp_MainFile
    let b:ddict=l:ddict
	for l:inputfile in keys(l:ddict)
	    let l:ifile=readfile(l:inputfile)
	    for l:range in l:ddict[l:inputfile]
		if l:ifile[l:range[0]-1] =~ '\\def\|\\newcommand'
		    " check only definitions which starts at 0 column
		    " definition name 
		    let l:name=matchstr(l:ifile[l:range[0]-1],
				\ '^\%(\\def\\\zs[^{#]*\ze[{#]\|\\newcommand{\?\\\zs[^\[{]*\ze[\[{}]}\?\)')
		    " definition
		    let l:def=matchstr(l:ifile[l:range[0]-1],
				\ '^\%(\\def\\[^{]*{\zs.*\ze}\|\\newcommand\\[^{]*{\zs.*\ze}\)') 
		    if l:name != ""
			" add a definition if it is not a lenght:
			" \def\myskip{2cm}
			" will not be added.
" 				echo l:name . " count: " . (!count(l:CommandNames, "\\".l:name)) . " pattern: " . (l:def !~ '^\s*\(\d\|\.\)*\s*\(mm\|cm\|pt\|in\|em\|ex\)\?$' || l:def == "") . " l:def " . l:def
			if !count(l:CommandNames, "\\".l:name) && (l:def !~ '^\s*\(\d\|\.\)*\s*\(mm\|cm\|pt\|in\|em\|ex\)\?$' || l:def == "")
			    call add(l:CommandNames, "\\".l:name)
			endif
" 				echomsg l:name
		    endif
		endif
		if l:ifile[l:range[0]-1] =~ '\\newenvironment\|\\newtheorem'
		    " check only definitions which starts at 0 column
		    let l:name=matchstr(l:ifile[l:range[0]-1],
				\ '^\\\%(newtheorem\*\?\|newenvironment\){\zs[^}]*\ze}')
		    if l:name != ""
			if !count(l:EnvironmentNames,l:name)
			    call add(l:EnvironmentNames,l:name)
			endif
		    endif
		endif
		if l:ifile[l:range[0]-1] =~ '\\definecolor'
		    let l:name=matchstr(l:ifile[l:range[0]-1],
				\ '^\s*\\definecolor\s*{\zs[^}]*\ze}')
		    if l:name != ""
			if !count(l:ColorNames,l:name)
			    call add(l:ColorNames,l:name)
			endif
		    endif
		endif
	    endfor
	endfor
    let s:atp_LocalCommands	= []
    let s:atp_LocalEnvironments	= []
    let s:atp_LocalColors	= l:ColorNames

    " remove double entries
    for l:type in ['Command', 'Environment']
" 		echomsg l:type
	for l:item in l:{l:type}Names
	    if index(g:atp_{l:type}s,l:item) == '-1'
		call add(s:atp_Local{l:type}s,l:item)
	    endif
	endfor
    endfor

    " Make shallow copies of the lists
    let b:atp_LocalCommands=s:atp_LocalCommands
    let b:atp_LocalEnvironments=s:atp_LocalEnvironments
    let b:atp_LocalColors=s:atp_LocalColors
    return [ s:atp_LocalEnvironments, s:atp_LocalCommands, s:atp_LocalColors ]
endfunction
command! -buffer LocalCommands		:call LocalCommands()
"}}}

" Search for Definition in the definition dictionary (s:make_defi_dict).
"{{{ DefiSearch
function! DefiSearch(...)

    if a:0 == 0
	let l:pattern=''
    else
	let l:pattern='\C' . a:1
    endif
    if a:0 >= 2 
	let l:preambule_only=a:2
    else
	let l:preambule_only=1
    endif

    let l:ddict	= s:make_defi_dict(bufname("%"),'\\def\|\\newcommand',l:preambule_only)
"     let b:dd=l:ddict

    " open new buffer
    let l:openbuffer=" +setl\\ buftype=nofile\\ nospell " . fnameescape("DefiSearch")
    if g:vertical ==1
	let l:openbuffer="vsplit " . l:openbuffer 
    else
	let l:openbuffer="split " . l:openbuffer 
    endif

    if len(l:ddict) > 0
	" wipe out the old buffer and open new one instead
	if bufloaded("DefiSearch")
	    exe "silent bd! " . bufnr("DefiSearch") 
	endif
	silent exe l:openbuffer
	map <buffer> q	:bd<CR>

	for l:inputfile in keys(l:ddict)
	    let l:ifile=readfile(l:inputfile)
	    for l:range in l:ddict[l:inputfile]

		if l:ifile[l:range[0]-1] =~ l:pattern
		    " print the lines into the buffer
		    let l:i=0
		    let l:c=0
		    " add an empty line if the definition is longer than one line
		    if l:range[0] != l:range[1]
			call setline(line('$')+1,'')
			let l:i+=1
		    endif
		    while l:c <= l:range[1]-l:range[0] 
			let l:line=l:range[0]+l:c
			call setline(line('$')+1,l:ifile[l:line-1])
			let l:i+=1
			let l:c+=1
		    endwhile
		endif
	    endfor
	endfor

	if getbufline("DefiSearch",'1','$') == ['']
	    :bw
	    echomsg "Definition not found."
	endif
    else
	echomsg "Definition not found."
    endif
    try
	setl filetype=tex
    catch /cannot redefine function defisearch/
    finally
	setl filetype=tex
    endtry
endfunction
command! -buffer -nargs=* DefiSearch		:call DefiSearch(<f-args>)
"}}}

" These are only variables and front end functions for Bib Search Engine of ATP.
" Search engine is define in autoload/atplib.vim script library.
"{{{ BibSearch
"-------------SEARCH IN BIBFILES ----------------------
" This function counts accurence of a:keyword in string a:line, 
" there are two methods keyword is a string to find (a:1=0)or a pattern to
" match, the pattern used to is a:keyword\zs.* to find the place where to cut.
" DEBUG:
" command -buffer -nargs=* Count :echo atplib#count(<args>)

let g:bibentries=['article', 'book', 'booklet', 'conference', 'inbook', 'incollection', 'inproceedings', 'manual', 'mastertheosis', 'misc', 'phdthesis', 'proceedings', 'techreport', 'unpublished']


"{{{ variables
let g:bibmatchgroup		='String'
let g:defaultbibflags		= 'tabejsyu'
let g:defaultallbibflags	= 'tabejfsvnyPNSohiuHcp'
let b:lastbibflags		= g:defaultbibflags	" Set the lastflags variable to the default value on the startup.
let g:bibflagsdict=atplib#bibflagsdict
" These two variables were s:... but I switched to atplib ...
let g:bibflagslist		= keys(g:bibflagsdict)
let g:bibflagsstring		= join(g:bibflagslist,'')
let g:kwflagsdict={ 	  '@a' : '@article', 	
	    		\ '@b' : '@book\%(let\)\@<!', 
			\ '@B' : '@booklet', 	
			\ '@c' : '@in\%(collection\|book\)', 
			\ '@m' : '@misc', 	
			\ '@M' : '@manual', 
			\ '@p' : '@\%(conference\)\|\%(\%(in\)\?proceedings\)', 
			\ '@t' : '@\%(\%(master)\|\%(phd\)\)thesis', 
			\ '@T' : '@techreport', 
			\ '@u' : '@unpublished' }    

" Set the g:{b:atp_Viewer}Options as b:atp_ViewerOptions for the current buffer
"}}}
fun! s:set_viewer_options()
    if exists("b:atp_Viewer") && exists("g:" . b:atp_Viewer . "Options")
	let b:atp_ViewerOptions=g:{b:atp_Viewer}Options
    endif
endfun
au BufEnter *.tex :call s:set_viewer_options()

" Hilighlting
hi link BibResultsFileNames 	Title	
hi link BibResultEntry		ModeMsg
hi link BibResultsMatch		WarningMsg
hi link BibResultsGeneral	Normal


hi link Chapter 		Normal	
hi link Section			Normal
hi link Subsection		Normal
hi link Subsubsection		Normal
hi link CurrentSection		WarningMsg

" Front End Function
" {{{ BibSearch
"  There are three arguments: {pattern}, [flags, [choose]]
function! s:BibSearch(...)
    if a:0 == 0
	let bib_results		= atplib#searchbib('')
	let b:listofkeys 	= atplib#showresults( bibresults, '', '')
    elseif a:0 == 1
	let bibresults		= atplib#searchbib(a:1)
	let b:listofkeys	= atplib#showresults( bibresults, '', a:1)
    else
	let bibresults		= atplib#searchbib(a:1)
	let b:listofkeys	= atplib#showresults( bibresults, a:2, a:1)
    endif
    let b:atp_LastBibPattern	= a:1
endfunction
command! -buffer -nargs=* BibSearch	:call <SID>BibSearch(<f-args>)
nnoremap <silent> <Plug>BibSearchLast	:call <SID>BibSearch(b:atp_LastBibPattern, b:atp_LastBibFlags)
" }}}
"}}}

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/ATP_files/various.vim	[[[1
803
"Author		Marcin Szamotulski	
"These are various tools.

" This is the wrap selection function.
" {{{ WrapSelection
function! s:WrapSelection(wrapper,...)

    let l:end_wrapper 	= ( a:0 >= 1 ? a:1 : '}' )
    let l:cursor_pos	= ( a:0 >= 2 ? a:2 : 'end' )
    let l:new_line	= ( a:0 >= 3 ? a:3 : 0 )

"     let b:new_line=l:new_line
"     let b:cursor_pos=l:cursor_pos
"     let b:end_wrapper=l:end_wrapper

    let l:begin=getpos("'<")
    " todo: if and on 'ą' we should go one character further! (this is
    " a multibyte character)
    let l:end=getpos("'>")
    let l:pos_save=getpos(".")

    " hack for that:
    let l:pos=deepcopy(l:end)
    keepjumps call setpos(".",l:end)
    execute 'normal l'
    let l:pos_new=getpos(".")
    if l:pos_new[2]-l:pos[2] > 1
	let l:end[2]+=l:pos_new[2]-l:pos[2]-1
    endif

    let l:begin_line=getline(l:begin[1])
    let l:end_line=getline(l:end[1])

    let b:begin=l:begin[1]
    let b:end=l:end[1]

    " ToDo: this doesn't work yet!
    let l:add_indent='    '
    if l:begin[1] != l:end[1]
	let l:bbegin_line=strpart(l:begin_line,0,l:begin[2]-1)
	let l:ebegin_line=strpart(l:begin_line,l:begin[2]-1)

	" DEBUG
	let b:bbegin_line=l:bbegin_line
	let b:ebegin_line=l:ebegin_line

	let l:bend_line=strpart(l:end_line,0,l:end[2])
	let l:eend_line=strpart(l:end_line,l:end[2])

	if l:new_line == 0
	    " inline
" 	    let b:debug=0
	    let l:begin_line=l:bbegin_line.a:wrapper.l:ebegin_line
	    let l:end_line=l:bend_line.l:end_wrapper.l:eend_line
	    call setline(l:begin[1],l:begin_line)
	    call setline(l:end[1],l:end_line)
	    let l:end[2]+=len(l:end_wrapper)
	else
" 	    let b:debug=1
	    " in seprate lines
	    let l:indent=atplib#CopyIndentation(l:begin_line)
	    if l:bbegin_line !~ '^\s*$'
		let l:begin_choice=1
		call setline(l:begin[1],l:bbegin_line)
		call append(l:begin[1],l:indent.a:wrapper) " THERE IS AN ISSUE HERE!
		call append(copy(l:begin[1])+1,l:indent.substitute(l:ebegin_line,'^\s*','',''))
		let l:end[1]+=2
	    elseif l:bbegin_line =~ '^\s\+$'
		let l:begin_choice=2
		call append(l:begin[1]-1,l:indent.a:wrapper)
		call append(l:begin[1],l:begin_line.l:ebegin_line)
		let l:end[1]+=2
	    else
		let l:begin_choice=3
		call append(copy(l:begin[1])-1,l:indent.a:wrapper)
		let l:end[1]+=1
	    endif
	    if l:eend_line !~ '^\s*$'
		let l:end_choice=4
		call setline(l:end[1],l:bend_line)
		call append(l:end[1],l:indent.l:end_wrapper)
		call append(copy(l:end[1])+1,l:indent.substitute(l:eend_line,'^\s*','',''))
	    else
		let l:end_choice=5
		call append(l:end[1],l:indent.l:end_wrapper)
	    endif
	    if (l:end[1] - l:begin[1]) >= 0
		if l:begin_choice == 1
		    let i=2
		elseif l:begin_choice == 2
		    let i=2
		elseif l:begin_choice == 3 
		    let i=1
		endif
		if l:end_choice == 5 
		    let j=l:end[1]-l:begin[1]+1
		else
		    let j=l:end[1]-l:begin[1]+1
		endif
		while i < j
		    " Adding indentation doesn't work in this simple way here?
		    " but the result is ok.
		    call setline(l:begin[1]+i,l:indent.l:add_indent.getline(l:begin[1]+i))
		    let i+=1
		endwhile
	    endif
	    let l:end[1]+=2
	    let l:end[2]=1
	endif
    else
	let l:begin_l=strpart(l:begin_line,0,l:begin[2]-1)
	let l:middle_l=strpart(l:begin_line,l:begin[2]-1,l:end[2]-l:begin[2]+1)
	let l:end_l=strpart(l:begin_line,l:end[2])
	if l:new_line == 0
	    " inline
	    let l:line=l:begin_l.a:wrapper.l:middle_l.l:end_wrapper.l:end_l
	    call setline(l:begin[1],l:line)
	    let l:end[2]+=len(a:wrapper)+1
	else
	    " in seprate lines
	    let b:begin_l=l:begin_l
	    let b:middle_l=l:middle_l
	    let b:end_l=l:end_l

	    let l:indent=atplib#CopyIndentation(l:begin_line)

	    if l:begin_l =~ '\S' 
		call setline(l:begin[1],l:begin_l)
		call append(copy(l:begin[1]),l:indent.a:wrapper)
		call append(copy(l:begin[1])+1,l:indent.l:add_indent.l:middle_l)
		call append(copy(l:begin[1])+2,l:indent.l:end_wrapper)
		if substitute(l:end_l,'^\s*','','') =~ '\S'
		    call append(copy(l:begin[1])+3,l:indent.substitute(l:end_l,'^\s*','',''))
		endif
	    else
		call setline(copy(l:begin[1]),l:indent.a:wrapper)
		call append(copy(l:begin[1]),l:indent.l:add_indent.l:middle_l)
		call append(copy(l:begin[1])+1,l:indent.l:end_wrapper)
		if substitute(l:end_l,'^\s*','','') =~ '\S'
		    call append(copy(l:begin[1])+2,l:indent.substitute(l:end_l,'^\s*','',''))
		endif
	    endif
	endif
    endif
    if l:cursor_pos == "end"
	let l:end[2]+=len(l:end_wrapper)-1
	call setpos(".",l:end)
    elseif l:cursor_pos =~ '\d\+'
	let l:pos=l:begin
	let l:pos[2]+=l:cursor_pos
	call setpos(".",l:pos)
    elseif l:cursor_pos == "current"
	keepjumps call setpos(".",l:pos_save)
    elseif l:cursor_pos == "begin"
	let l:begin[2]+=len(a:wrapper)-1
	keepjumps call setpos(".",l:begin)
    endif
endfunction
command! -buffer -nargs=? -range WrapSelection	:call <SID>WrapSelection(<args>)
vmap <Plug>WrapSelection			:<C-U>call <SID>WrapSelection('')<CR>i

"}}}
"{{{ Inteligent Wrap Selection 
" This function selects the correct font wrapper for math/text environment.
" the rest of arguments are the same as for WrapSelection (and are passed to
" WrapSelection function)
" a:text_wrapper	= [ 'begin_text_wrapper', 'end_text_wrapper' ] 
" a:math_wrapper	= [ 'begin_math_wrapper', 'end_math_wrapper' ] 
" if end_(math\|text)_wrapper is not given '}' is used (but neverthe less both
" arguments must be lists).
function! s:InteligentWrapSelection(text_wrapper, math_wrapper, ...)

    let cursor_pos	= ( a:0 >= 1 ? a:2 : 'end' )
    let new_line	= ( a:0 >= 2 ? a:3 : 0 )

    let MathZones = copy(g:atp_MathZones)
    if b:atp_TexFlavour == 'plaintex'
	call add(MathZones, 'texMathZoneY')
    endif

    " select the correct wrapper
    if atplib#CheckSyntaxGroups(MathZones)
	let begin_wrapper 	= a:math_wrapper[0]
	let end_wrapper 	= get(a:math_wrapper,1, '}')
    else
	let begin_wrapper	= a:text_wrapper[0]
	let end_wrapper		= get(a:text_wrapper,1, '}')
    endif

    " if the wrapper is empty return
    " useful for wrappers which are valid only in one mode.
    if begin_wrapper == ""
	return
    endif

    call s:WrapSelection(begin_wrapper, end_wrapper, cursor_pos, new_line) 
endfunction
command! -buffer -nargs=? -range InteligentWrapSelection	:call <SID>InteligentWrapSelection(<args>)
vmap <Plug>InteligentWrapSelection				:<C-U>call <SID>InteligentWrapSelection('')<CR>i
"}}}


" Editing Toggle Functions
"{{{ Variables
if !exists("g:atp_no_toggle_environments")
    let g:atp_no_toggle_environments=[ 'document', 'tikzpicture', 'picture']
endif
if !exists("g:atp_toggle_environment_1")
    let g:atp_toggle_environment_1=[ 'center', 'flushleft', 'flushright', 'minipage' ]
endif
if !exists("g:atp_toggle_environment_2")
    let g:atp_toggle_environment_2=[ 'enumerate', 'itemize', 'list', 'description' ]
endif
if !exists("g:atp_toggle_environment_3")
    let g:atp_toggle_environment_3=[ 'quotation', 'quote', 'verse' ]
endif
if !exists("g:atp_toggle_environment_4")
    let g:atp_toggle_environment_4=[ 'theorem', 'proposition', 'lemma' ]
endif
if !exists("g:atp_toggle_environment_5")
    let g:atp_toggle_environment_5=[ 'corollary', 'remark', 'note' ]
endif
if !exists("g:atp_toggle_environment_6")
    let g:atp_toggle_environment_6=[  'equation', 'align', 'array', 'alignat', 'gather', 'flalign'  ]
endif
if !exists("g:atp_toggle_environment_7")
    let g:atp_toggle_environment_7=[ 'smallmatrix', 'pmatrix', 'bmatrix', 'Bmatrix', 'vmatrix' ]
endif
if !exists("g:atp_toggle_environment_8")
    let g:atp_toggle_environment_8=[ 'tabbing', 'tabular']
endif
if !exists("g:atp_toggle_labels")
    let g:atp_toggle_labels=1
endif
"}}}
"{{{ ToggleStar
" this function adds a star to the current environment
" todo: to doc.
function! s:ToggleStar()

    " limit:
    let l:from_line=max([1,line(".")-g:atp_completion_limits[2]])
    let l:to_line=line(".")+g:atp_completion_limits[2]

    " omit pattern
    let l:omit=join(g:atp_no_star_environments,'\|')
    let l:open_pos=searchpairpos('\\begin\s*{','','\\end\s*{[^}]*}\zs','cbnW','getline(".") =~ "\\\\begin\\s*{".l:omit."}"',l:from_line)
    let b:open_pos=l:open_pos
    let l:env_name=matchstr(strpart(getline(l:open_pos[0]),l:open_pos[1]),'begin\s*{\zs[^}]*\ze}')
    let b:env_name=l:env_name
    if l:open_pos == [0, 0] || index(g:atp_no_star_environments,l:env_name) != -1
	return
    endif
    if l:env_name =~ '\*$'
	let l:env_name=substitute(l:env_name,'\*$','','')
	let l:close_pos=searchpairpos('\\begin\s*{'.l:env_name.'\*}','','\\end\s*{'.l:env_name.'\*}\zs','cnW',"",l:to_line)
	if l:close_pos != [0, 0]
	    call setline(l:open_pos[0],substitute(getline(l:open_pos[0]),'\(\\begin\s*{\)'.l:env_name.'\*}','\1'.l:env_name.'}',''))
	    call setline(l:close_pos[0],substitute(getline(l:close_pos[0]),
			\ '\(\\end\s*{\)'.l:env_name.'\*}','\1'.l:env_name.'}',''))
	    echomsg "Star removed from '".l:env_name."*' at lines: " .l:open_pos[0]." and ".l:close_pos[0]
	endif
    else
	let l:close_pos=searchpairpos('\\begin\s{'.l:env_name.'}','','\\end\s*{'.l:env_name.'}\zs','cnW',"",l:to_line)
	if l:close_pos != [0, 0]
	    call setline(l:open_pos[0],substitute(getline(l:open_pos[0]),
		    \ '\(\\begin\s*{\)'.l:env_name.'}','\1'.l:env_name.'\*}',''))
	    call setline(l:close_pos[0],substitute(getline(l:close_pos[0]),
			\ '\(\\end\s*{\)'.l:env_name.'}','\1'.l:env_name.'\*}',''))
	    echomsg "Star added to '".l:env_name."' at lines: " .l:open_pos[0]." and ".l:close_pos[0]
	endif
    endif
endfunction
command! -buffer 	ToggleStar   		:call <SID>ToggleStar()
nnoremap <silent> <Plug>ToggleStar		:ToggleStar
"}}}
"{{{ ToggleEnvironment
" this function toggles envrionment name.
" Todo: to doc.
" the argument specifies the speed (if -1 then toggle back)
" default is '1'
function! s:ToggleEnvironment(...)

    let l:add = ( a:0 >= 1 ? a:1 : 1 ) 

    " limit:
    let l:from_line=max([1,line(".")-g:atp_completion_limits[2]])
    let l:to_line=line(".")+g:atp_completion_limits[2]

    " omit pattern
    let l:omit=join(g:atp_no_toggle_environments,'\|')
    let l:open_pos=searchpairpos('\\begin\s*{','','\\end\s*{[^}]*}\zs','bnW','getline(".") =~ "\\\\begin\\s*{".l:omit."}"',l:from_line)
    let l:env_name=matchstr(strpart(getline(l:open_pos[0]),l:open_pos[1]),'begin\s*{\zs[^}]*\ze}')

    let l:label=matchstr(strpart(getline(l:open_pos[0]),l:open_pos[1]),'\\label\s*{\zs[^}]*\ze}')
    " DEBUG
    let b:line=strpart(getline(l:open_pos[0]),l:open_pos[1])
    let b:label=l:label
    let b:env_name=l:env_name
    if l:open_pos == [0, 0] || index(g:atp_no_toggle_environments,l:env_name) != -1
	return
    endif

    let l:env_name_ws=substitute(l:env_name,'\*$','','')
    let l:variable="g:atp_toggle_environment_1"
    let l:i=1
    while 1
	let l:env_idx=index({l:variable},l:env_name_ws)
	if l:env_idx != -1
	    break
	else
	    let l:i+=1
	    let l:variable="g:atp_toggle_environment_".l:i
	endif
	if !exists(l:variable)
	    return
	endif
    endwhile

    if l:add > 0 && l:env_idx > len({l:variable})-l:add-1
	let l:env_idx=0
    elseif ( l:add < 0 && l:env_idx < -1*l:add )
	let l:env_idx=len({l:variable})-1
    else
	let l:env_idx+=l:add
    endif
    let l:new_env_name={l:variable}[l:env_idx]
    if l:env_name =~ '\*$'
	let l:new_env_name.="*"
    endif

    " DEBUG
"     let b:i=l:i
"     let b:env_idx=l:env_idx
"     let b:env_name=l:env_name
"     let b:new_env_name=l:new_env_name

    let l:env_name=escape(l:env_name,'*')
    let l:close_pos=searchpairpos('\\begin\s*{'.l:env_name.'}','','\\end\s*{'.l:env_name.'}\zs','nW',"",l:to_line)
    if l:close_pos != [0, 0]
	call setline(l:open_pos[0],substitute(getline(l:open_pos[0]),'\(\\begin\s*{\)'.l:env_name.'}','\1'.l:new_env_name.'}',''))
	call setline(l:close_pos[0],substitute(getline(l:close_pos[0]),
		    \ '\(\\end\s*{\)'.l:env_name.'}','\1'.l:new_env_name.'}',''))
	echomsg "Environment toggeled at lines: " .l:open_pos[0]." and ".l:close_pos[0]
    endif

    if l:label != "" && g:atp_toggle_labels
	let l:new_env_name_ws=substitute(l:new_env_name,'\*$','','')
	let l:new_short_name=get(g:atp_shortname_dict,l:new_env_name_ws,"")
	let l:short_pattern=join(values(filter(g:atp_shortname_dict,'v:val != ""')),'\|')
	let l:short_name=matchstr(l:label,'^'.l:short_pattern)
	let l:new_label=substitute(l:label,'^'.l:short_name,l:new_short_name,'')

	" check if new label is in use!
	let l:pos_save=getpos(".")
	let l:n=search('\C\\\(label\|\%(eq\|page\)\?ref\)\s*{'.l:new_label.'}','nwc')
" 	let b:n=l:n

	if l:short_name != "" && l:n == 0 && l:new_label != l:label
	    silent! keepjumps execute '%substitute /\\\(eq\|page\)\?\(ref\s*\){'.l:label.'}/\\\1\2{'.l:new_label.'}/gIe'
	    silent! keepjumps execute l:open_pos[0].'substitute /\\label{'.l:label.'}/\\label{'.l:new_label.'}'
	    keepjumps call setpos(".",l:pos_save)
	elseif l:n != 0 && l:new_label != l:label
	    echohl WarningMsg
	    echomsg "Labels not changed, new label: ".l:new_label." is in use!"
	    echohl Normal
	endif
    endif
    return  l:open_pos[0]."-".l:close_pos[0]
endfunction
command! -buffer -nargs=? ToggleEnvironment   		:call <SID>ToggleEnvironment(<f-args>)
nnoremap <silent> <Plug>ToggleEnvForward		:call <SID>ToggleEnvironment(1)
nnoremap <silent> <Plug>ToggleEnvBackward		:call <SID>ToggleEnvironment(-1)
"}}}


"{{{ Help 
function! s:TeXdoc(...)
    let texdoc_arg	= ""
    for i in range(1,a:0)
	let texdoc_arg.=" " . a:{i}
    endfor
    if texdoc_arg == ""
	let texdoc_arg 	= "-m " . g:atp_TeXdocDefault
    endif
    " If the file is a text file texdoc is 'cat'-ing it into the terminal,
    " we use echo to capture the output. 
    " The rediraction prevents showing texdoc info messages which are not that
    " important, if a document is not found texdoc sends a message to the standard
    " output not the error.
    echo system("texdoc " . texdoc_arg . " 2>/dev/null")
endfunction

function! s:TeXdoc_complete(ArgLead, CmdLine, CursorPos)
    let texdoc_alias_files=split(system("texdoc -f"), '\n')
    call filter(texdoc_alias_files, "v:val =~ 'active'")
    call map(texdoc_alias_files, "substitute(substitute(v:val, '^[^/]*\\ze', '', ''), '\/\/\\+', '/', 'g')")
    let aliases = []
    for file in texdoc_alias_files
	call extend(aliases, readfile(file))
    endfor

    call filter(aliases, "v:val =~ 'alias'")
    call filter(map(aliases, "matchstr(v:val, '^\\s*alias\\s*\\zs\\S*\\ze\\s*=')"),"v:val !~ '^\\s*$'")

    return filter(copy(aliases), "v:val =~ '^' . a:ArgLead")
endfunction
command -buffer -nargs=* -complete=customlist,<SID>TeXdoc_complete TeXdoc 	:call <SID>TeXdoc(<f-args>)
nnoremap <silent> <buffer> <Plug>TeXdoc						:TeXdoc 
"}}}

" This function deletes tex specific output files (exept the pdf/dvi file, unless
" g:atp_delete_output is set to 1 - then also delets the current output file)
"{{{1 Delete
function! s:Delete()

    call atplib#outdir()

    let l:atp_tex_extensions=deepcopy(g:atp_tex_extensions)
    let error=0

    if g:atp_delete_output
	if b:atp_TexCompiler == "pdftex" || b:atp_TexCompiler == "pdflatex"
	    let l:ext="pdf"
	else
	    let l:ext="dvi"
	endif
	call add(l:atp_tex_extensions,l:ext)
    endif

    for l:ext in l:atp_tex_extensions
	if executable(g:rmcommand)
	    if g:rmcommand =~ "^\s*rm\p*" || g:rmcommand =~ "^\s*perltrash\p*"
		if l:ext != "dvi" && l:ext != "pdf"
		    let l:rm=g:rmcommand . " " . shellescape(b:atp_OutDir) . "*." . l:ext . " 2>/dev/null && echo Removed ./*" . l:ext . " files"
		else

		    let l:rm=g:rmcommand . " " . fnamemodify(b:atp_MainFile,":r").".".l:ext . " 2>/dev/null && echo Removed " . fnamemodify(b:atp_MainFile,":r").".".l:ext
		endif
	    endif
	    echo system(l:rm)
	else
	    let error=1
	    let l:file=b:atp_OutDir . fnamemodify(expand("%"),":t:r") . "." . l:ext
	    if delete(l:file) == 0
		echo "Removed " . l:file 
	    endif
	endif
    endfor

" 	if error
" 		echo "Please set g:rmcommand to clear the working directory"
" 	endif
endfunction
command -buffer	Delete		:call <SID>Delete()<CR>
nmap <buffer> <Plug>Delete	:call <SID>Delete()<CR>
"}}}1

"{{{1 OpenLog, TexLog, TexLog Buffer Options, PdfFonts, YesNoCompletion
"{{{2 s:Search function for Log Buffer
function! s:Search(pattern, flag)
    let @/	=a:pattern
    call search(a:pattern, a:flag)
endfunction
function! s:Searchpair(start, middle, end, flag)
    if getline(".")[col(".")-1] == ')' 
	let flag	= a:flag.'b'
    else
	let flag	= substitute(a:flag, 'b', '', 'g')
    endif
    call searchpair(a:start, a:middle, a:end, flag)
endfunction
"}}}
function! s:OpenLog()
    if filereadable(&l:errorfile)
	exe "rightbelow split +setl\\ nospell\\ ruler\\ syn=log_atp\\ autoread " . &l:errorfile
	map <buffer> q :bd<CR>
	map <silent> <buffer> w :call <SID>Search('Warning', 'W')<CR>
	map <silent> <buffer> W :call <SID>Search('Warning', 'bW')<CR>
	map <silent> <buffer> c :call <SID>Search('LaTeX Warning: Citation', 'W')<CR>
	map <silent> <buffer> C :call <SID>Search('LaTeX Warning: Citation', 'bW')<CR>
	map <silent> <buffer> r :call <SID>Search('LaTeX Warning: Reference', 'W')<CR>
	map <silent> <buffer> R :call <SID>Search('LaTeX Warning: Reference', 'bW')<CR>
	map <silent> <buffer> e :call <SID>Search('^!', 'W')<CR>
	map <silent> <buffer> E :call <SID>Search('^!', 'bW')<CR>
	map <silent> <buffer> f :call <SID>Search('Font \%(Info\\|Warning\)', 'W')<CR>
	map <silent> <buffer> F :call <SID>Search('Font \%(Info\\|Warning\)', 'bW')<CR>
	map <silent> <buffer> p :call <SID>Search('Package', 'W')<CR>
	map <silent> <buffer> P :call <SID>Search('Package', 'bW')<CR>
	map <silent> <buffer> i :call <SID>Search('Info', 'W')<CR>
	map <silent> <buffer> I :call <SID>Search('Info', 'bW')<CR>
	map <silent> <buffer> % :call <SID>Searchpair('(', '', ')', 'w')<CR>
	silent execute "%g/^\s*$/d"
    else
	echo "No log file"
    endif
endfunction
command! -buffer OpenLog			:call <SID>OpenLog()
nnoremap <silent> <buffer> <Plug>OpenLog	:call <SID>OpenLog()<CR>

" TeX LOG FILE
if &buftype == 'quickfix'
	setlocal modifiable
	setlocal autoread
endif	
function! s:TexLog(options)
    if executable("texloganalyser")
       let s:command="texloganalyser " . a:options . " " . &l:errorfile
       echo system(s:command)
    else	
       echo "Please install 'texloganalyser' to have this functionality. The perl program written by Thomas van Oudenhove."  
    endif
endfunction
command! -buffer TexLog			:call <SID>TexLog()
nnoremap <silent> <buffer> <Plug>TexLog	:call <SID>TexLog()<CR>

function! s:PdfFonts()
    if b:atp_OutDir !~ "\/$"
	b:atp_OutDir=b:atp_OutDir . "/"
    endif
    if executable("pdffonts")
	let s:command="pdffonts " . fnameescape(fnamemodify(b:atp_MainFile,":r")) . ".pdf"
	echo system(s:command)
    else
	echo "Please install 'pdffonts' to have this functionality. In 'gentoo' it is in the package 'app-text/poppler-utils'."  
    endif
endfunction	
command! -buffer PdfFonts			:call <SID>PdfFonts()
nnoremap <silent> <buffer> <Plug>PdfFonts	:call <SID>PdfFonts()<CR>

" function! s:setprintexpr()
"     if b:atp_TexCompiler == "pdftex" || b:atp_TexCompiler == "pdflatex"
" 	let s:ext = ".pdf"
"     else
" 	let s:ext = ".dvi"	
"     endif
"     let &printexpr="system('lpr' . (&printdevice == '' ? '' : ' -P' . &printdevice) . ' " . fnameescape(fnamemodify(expand("%"),":p:r")) . s:ext . "') . + v:shell_error"
" endfunction
" call s:setprintexpr()

fun! YesNoCompletion(A,P,L)
    return ['yes','no']
endfun
"}}}1

" Ssh printing tools
"{{{1 Print, Lpstat, ListPrinters
" This function can send the output file to local or remote printer.
" a:1   = file to print		(if not given printing the output file)
" a:2	= printer name		(if g:atp_ssh is non empty or different from
" 				'localhost' printer on remote server)
" a:3	= printing options	(give printing optinos or 'default' then use
" 				the variable g:printingoptions)
" a:4 	= printing command 	(default lpr)
 function! s:SshPrint(...)

    call atplib#outdir()

    " set the extension of the file to print
    " if prining the tex output file.
    if a:0 == 0 || a:0 >= 1 && a:1 == ""
	let l:ext = get(g:atp_CompilersDict, b:atp_TexCompiler, "not present")
	if l:ext == "not present"
	    echohl WarningMsg
	    echomsg b:atp_TexCompiler . " is not present in g:atp_CompilersDict"
	    echohl Normal
	    return "extension not found"
	endif
	if b:atp_TexCompiler =~ "lua"
	    if b:atp_TexOptions == "" || b:atp_TexOptions =~ "output-format=\s*pdf"
		let l:ext = ".pdf"
	    else
		let l:ext = ".dvi"
	    endif
	endif
    endif

    " set the file to print
    let l:pfile		= ( a:0 == 0 || (a:0 >= 1 && a:1 == "" ) ? b:atp_OutDir . fnamemodify(expand("%"),":t:r") . l:ext : a:1 )

    " set the printing command
    let l:lprcommand	= ( a:0 >= 4 ? a:4 : "lpr" )
    let l:print_options	= ( a:0 >= 3 ? a:3 : g:printingoptions )

    " print locally or remotely
    " the default is to print locally (g:atp_ssh=`whoami`@localhost)
    let l:server	= ( exists("g:atp_ssh") ? strpart(g:atp_ssh,stridx(g:atp_ssh,"@")+1) : "localhost" )
    " To which printer send the file:
    let l:printer	= ( a:0 >= 2 ? "-P " . a:2 : "" )
    " Set Printing Options
    let l:print_options .= " " . l:printer

    echomsg "Server " . l:server
    echomsg "File   " . l:pfile

    if l:server =~ 'localhost'
	let l:ok 		= confirm("Are the printing options set right?\n".l:print_options,"&Yes\n&No\n&Cancel")
	if l:ok == "2"
	    let l:print_options	= input("Give new printing options ")
	elseif l:ok == "3"
	    return "abandoned"
	endif

	let l:com	= l:lprcommand . " " . l:print_options . " " .  fnameescape(l:pfile)

	redraw!
	echomsg "Printing ...  " . l:com
" 	let b:com=l:com " DEBUG
	call system(l:com)
    " print over ssh on the server g:atp_ssh with the printer a:1 (or the
    " default system printer if a:0 == 0
    else 
	" TODO: write completion :).
	let l:ok = confirm("Are the printing options set right?\n".l:print_options,"&Yes\n&No\n&Cancel")
	if l:ok == "2"
	    let l:print_options=input("Give new printing options ")
	elseif l:ok == "3"
	    return "abandoned"
	endif
	redraw!
	let l:com="cat " . fnameescape(l:pfile) . " | ssh " . g:atp_ssh . " " . l:lprcommand . " " . l:print_options
	echomsg "Printing ...  " . l:com
" 	let b:com=l:com " DEBUG
	call system(l:com)
    endif
endfunction
" The command only prints the output file.
command! -complete=custom,<SID>ListPrinters  -buffer -nargs=* SshPrint 	:call <SID>SshPrint("", <f-args>)
nnoremap <silent> <buffer> <Plug>SshPrint				:SshPrint

fun! s:Lpstat()
    if exists("g:apt_ssh") 
	let l:server=strpart(g:atp_ssh,stridx(g:atp_ssh,"@")+1)
    else
	let l:server='locahost'
    endif
    if l:server == 'localhost'
	echo system("lpstat -l")
    else
	echo system("ssh " . g:atp_ssh . " lpstat -l ")
    endif
endfunction
command! -buffer Lpstat			:call <SID>Lpstat()
nnoremap <silent> <buffer> <Plug>Lpstat	:call <SID>Lpstat()<CR>

" it is used for completetion of the command SshPrint
function! s:ListPrinters(A,L,P)
    if exists("g:atp_ssh") && g:atp_ssh !~ '@localhost' && g:atp_ssh != ""
	let l:com="ssh -q " . g:atp_ssh . " lpstat -a | awk '{print $1}'"
    else
	let l:com="lpstat -a | awk '{print $1}'"
    endif
    return system(l:com)
endfunction
command! -buffer ListPrinters	:echo <SID>ListPrinters("", "", "")
"}}}1

" ToDo noto
" {{{1 ToDo
"
" TODO if the file was not found ask to make one.
function! ToDo(keyword,stop,...)

    if a:0 == 0
	let bufname	= bufname("%")
    else
	let bufname	= a:1
    endif

    " read the buffer
    let texfile=getbufline(bufname, 1, "$")

    " find ToDos
    let todo = {}
    let nr=1
    for line in texfile
	if line =~ '%.*' . a:keyword 
	    call extend(todo, { nr : line }) 
	endif
	let nr += 1
    endfor

    " Show ToDos
    echohl atp_Todo
    if len(keys(todo)) == 0
	echomsg " List for '%.*" . a:keyword . "' in '" . bufname . "' is empty."
	return
    endif
    echomsg " List for '%.*" . a:keyword . "' in '" . bufname . "':"
    let sortedkeys=sort(keys(todo),"atplib#CompareList")
    for key in sortedkeys
	" echo the todo line.
	echomsg key . " " . substitute(substitute(todo[key],'%','',''),'\t',' ','g')
	let true	= 1
	let a		= 1
	let linenr	= key
	" show all comment lines right below the found todo line.
	while true && texfile[linenr] !~ '%.*\c\<todo\>' 
	    let linenr=key+a-1
	    if texfile[linenr] =~ "\s*%" && texfile[linenr] !~ a:stop
		" make space of length equal to len(linenr)
		let space=""
		let j=0
		while j < len(linenr)
		    let space=space . " " 
		    let j+=1
		endwhile
		echomsg space . " " . substitute(substitute(texfile[linenr],'%','',''),'\t',' ','g')
	    else
		let true = 0
	    endif
	    let a += 1
	endwhile
    endfor
    echohl None
endfunction
command! -buffer -nargs=? -complete=buffer ToDo		:call ToDo('\c\<to\s*do\>','\s*%\c.*\<note\>',<f-args>)
command! -buffer -nargs=? -complete=buffer Note		:call ToDo('\c\<note\>','\s*%\c.*\<to\s*do\>',<f-args>)
" }}}1

" This functions reloads ATP (whole or just a function)
" {{{1  RELOAD

if !exists("g:debug_atp_plugin")
    let g:debug_atp_plugin=0
endif
if g:debug_atp_plugin==1 && !exists("*Reload")
" Reload() - reload all the tex_apt functions
" Reload(func1,func2,...) reload list of functions func1 and func2
fun! Reload(...)
    let l:pos_saved=getpos(".")
    let l:bufname=fnamemodify(expand("%"),":p")

    if a:0 == 0
	let l:runtime_path=split(&runtimepath,',')
	echo "Searching for atp plugin files"
	let l:file_list=['ftplugin/tex_atp.vim', 'ftplugin/fd_atp.vim', 
		    \ 'ftplugin/bibsearch_atp.vim', 'ftplugin/toc_atp.vim', 
		    \ 'autoload/atplib.vim', 'ftplugin/atp_LatexBox.vim',
		    \ 'indent/tex_atp.vim' ]
	let l:file_path=[]
	for l:file in l:file_list
		call add(l:file_path,globpath(&rtp,l:file))
	endfor
" 	if exists("b:atp_debug")
" 	    if b:atp_debug == "v" || b:atp_debug == "verbose"
" 		echomsg string(l:file_path)
" 	    endif
" 	endif
	for l:file in l:file_path
	    echomsg "deleting FUNCTIONS and VARIABLES from " . l:file
	    let l:atp=readfile(l:file)
	    for l:line in l:atp
		let l:function_name=matchstr(l:line,'^\s*fun\%(ction\)\?!\?\s\+\zs\<[^(]*\>\ze(')
		if l:function_name != "" && l:function_name != "Reload"
		    if exists("*" . l:function_name)
			if exists("b:atp_debug")
			    if b:atp_debug == "v" || b:atp_debug == "verbose"
				echomsg "deleting function " . l:function_name
			    endif
			endif
			execute "delfunction " . l:function_name
		    endif
		endif
		let l:variable_name=matchstr(l:line,'^\s*let\s\+\zsg:[a-zA-Z_^{}]*\ze\>')
		if exists(l:variable_name)
		    execute "unlet ".l:variable_name
		    if exists("b:atp_debug")
			if b:atp_debug == "v" || b:atp_debug == "verbose"
			    echomsg "unlet ".l:variable_name
			endif
		    endif
		endif
	    endfor
	endfor
    else
	if a:1 != "maps" && a:1 != "reload"
	    let l:f_list=split(a:1,',')
	    let g:f_list=l:f_list
	    for l:function in l:f_list
		execute "delfunction " . l:function
		if exists("b:atp_debug")
		    if b:atp_debug == "v" || b:atp_debug == "verbose"
			echomsg "delfunction " . l:function
		    endif
		endif
	    endfor
	endif
    endif
    au! CursorHold *.tex
    w
"   THIS IS THE SLOW WAY:
    bd!
    execute "edit " . fnameescape(l:bufname)
    keepjumps call setpos(".",l:pos_saved)
"   This could be faster: but aparently doesn't work.
"     execute "source " . l:file_path[0]
endfunction
endif
command -buffer -nargs=* -complete=function Reload	:call Reload(<f-args>)
" }}}1

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
ftplugin/plaintex_atp.vim	[[[1
5
" Maintainer:	Marcin Szamotulski
" Note:		This is a part of ATP plugin for (La)TeX files.

" b:atp_TexFlavour will be set to plaintex automatically
source $HOME/.vim/ftplugin/tex_atp.vim
ftplugin/bibsearch_atp.vim	[[[1
112
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 July 3
" Note:		This file is a part of ATP plugin.

"
" {{{ Load Once
if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1
" }}}

" Status Line:
function! ATPBibStatus() "{{{
    return "Bibsearch: " . substitute(expand("%"),"___","","g")
endfunction
setlocal statusline=%{ATPBibStatus()}
" }}}

" Maps:
" {{{ MAPPINGS 
if !exists("no_plugin_maps") && !exists("no_atp_bibsearch_maps")
    map <buffer> c :call BibChoose()<CR>
    map <buffer> y :call BibChoose()<CR>
    map <buffer> p :call BibChoose()<CR>
    map <buffer> q :hide<CR>
    command! -buffer -nargs=* BibChoose 	:call BibChoose(<f-args>)
endif
" }}}

" Functions:
function! BibChoose(...)" {{{
    let l:which=input("Which entry? ( <Number><reg name><Enter>, <Number><Enter> or <Enter> for none) ")
    if l:which == ""
	return
    endif
    if l:which =~ '^\d*$' 
	let l:start=stridx(b:listofkeys[l:which],'{')+1
	let l:choice=substitute(strpart(b:listofkeys[l:which],l:start),',','','')
	q
	let l:line=getline(".")
	let l:col=col(".")
	let l:line=strpart(l:line,0,l:col) . l:choice . strpart(l:line,l:col)
	call setline(line("."), l:line)
    elseif l:which =~ '^\d*\(\a\|+\| . "*" .\)$'
	    let l:letter=substitute(l:which,'\d','','g')
	    let l:which=substitute(l:which,'\a\|+\|' . "*",'','g')
	    let l:start=stridx(b:listofkeys[l:which],'{')+1
	    let l:choice=substitute(strpart(b:listofkeys[l:which],l:start),',','','')
	    silent if l:letter == 'a'
		let @a=l:choice
	    elseif l:letter == 'b'
		let @b=l:choice
	    elseif l:letter == 'c'
		let @c=l:choice
	    elseif l:letter == 'd'
		let @d=l:choice
	    elseif l:letter == 'e'
		let @e=l:choice
	    elseif l:letter == 'f'
		let @f=l:choice
	    elseif l:letter == 'g'
		let @g=l:choice
	    elseif l:letter == 'h'
		let @h=l:choice
	    elseif l:letter == 'i'
		let @i=l:choice
	    elseif l:letter == 'j'
		let @j=l:choice
	    elseif l:letter == 'k'
		let @k=l:choice
	    elseif l:letter == 'l'
		let @l=l:choice
	    elseif l:letter == 'm'
		let @m=l:choice
	    elseif l:letter == 'n'
		let @n=l:choice
	    elseif l:letter == 'o'
		let @o=l:choice
	    elseif l:letter == 'p'
		let @p=l:choice
	    elseif l:letter == 'q'
		let @q=l:choice
	    elseif l:letter == 'r'
		let @r=l:choice
	    elseif l:letter == 's'
		let @s=l:choice
	    elseif l:letter == 't'
		let @t=l:choice
	    elseif l:letter == 'u'
		let @u=l:choice
	    elseif l:letter == 'v'
		let @v=l:choice
	    elseif l:letter == 'w'
		let @w=l:choice
	    elseif l:letter == 'x'
		let @x=l:choice
	    elseif l:letter == 'y'
		let @y=l:choice
	    elseif l:letter == 'z'
		let @z=l:choice
	    elseif l:letter == '*'
		let @-=l:choice
	    elseif l:letter == '+'
		let @+=l:choice
	    elseif l:letter == '-'
		let @@=l:choice
	    endif
	    echohl WarningMsg | echomsg "Choice yanekd to the register '" . l:letter . "'" | echohl None
    endif
endfunction "}}}

ftplugin/toc_atp.vim	[[[1
404
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 May 31
" URL:		

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

function! ATP_TOC_StatusLine()
    if expand("%") == "__ToC__"
	return "Table of Contents"
    elseif expand("%") == "__Labels__"
	return "List of Labels"
    endif
endfunction
setlocal statusline=%{ATP_TOC_StatusLine()}

function! s:getlinenr(...)
    if a:0 == 0
	let l:line=getline('.')
    else
	let l:line=getline(a:1)
    endif
    let l:nr=substitute(matchstr(l:line,'^\s*\d\+'),'^\s*','','')
    return l:nr
endfunction

" Get the file name and its path from the LABELS/ToC list.
function! s:file()
    let l:true=1
    let l:linenr=line('.')
    while l:true == 1
	let l:line=s:getlinenr(l:linenr)
	if l:line != ""
	    let l:linenr-=1
	else
	    let l:true=0
	    " NOTE THAT FILE NAME SHOULD NOT INCLUDE '(' and ')' and SHOULD
	    " NOT BEGIN WITH A NUMBER.
	    let l:line=getline(l:linenr)
	    let l:bufname=strpart(l:line,0,stridx(l:line,'(')-1)
	    let l:path=substitute(strpart(l:line,stridx(l:line,'(')+1),')\s*$','','')
" 	    echomsg "BUFNAME " . l:bufname
" 	    echomsg "PATH " . l:path
	endif
    endwhile
    return [ l:path, l:bufname ]
endfunction
command! File	:echo s:file()
 
"---------------------------------------------------------------------
" Notes:
" 		(1) choose window with matching buffer name
" 		(2) choose among those choose the one which we eddited last
" Solution:
"        			       --N-> choose this window
"			 	       |
"			     --N-> ----|
"			     | 	       --Y-> choose that window		
" --go from where you come-->|         Does there exists another open window 
"  			     |	       with the right buffer name?
"			     |	
"  			     --Y-> use this window
"			   Does the window has
"			   a correct name?
"
" This function returns the window number to which we will eventually go.
function! s:gotowinnr()
    " This is the line number to which we will go.
    let l:nr=s:getlinenr()
    " t:atp_bufname
    " t:atp_winnr		were set by TOC(), they should also be set by
    " 			autocommands
    let l:buf=s:file()
    let l:bufname=l:buf[0] . "/" . l:buf[1]

    if t:atp_bufname == l:bufname
	" if t:atp_bufname agree with that found in ToC
	" if the t:atp_winnr is still open
	if bufwinnr(t:atp_bufname) != -1
	    let l:gotowinnr=t:atp_winnr
" 	    echomsg "DEBUG A"
	else
	    let l:gotowinnr=-1
" 	    echomsg "DEBUG B"
	endif
" 	echomsg "DEBUG C " . l:gotowinnr
    else
 	if bufwinnr("^" . l:bufname . "$") != 0
	    " if not but there is a window with buffer l:bufname
	    let l:gotowinnr=bufwinnr("^" . l:bufname . "$")
 	else
	    " if not and there is no window with buffer l:bufname
 	    let l:gotowinnr=t:atp_winnr
 	endif
    endif
    return l:gotowinnr
endif
endfunction

function! GotoLine(closebuffer)
    
    " if under help lines do nothing:
    let l:toc=getbufline("%",1,"$")
    let l:h_line=index(reverse(copy(l:toc)),'')+1
    if line(".") > len(l:toc)-l:h_line
	return ''
    endif

    let l:buf=s:file()

    " remember the ToC window number
    let l:tocbufnr=bufnr("")

    " line to go to
    let l:nr=s:getlinenr()

    " window to go to
    let l:gotowinnr=s:gotowinnr()

    if l:gotowinnr != -1
 	exe l:gotowinnr . " wincmd w"
    else
 	exe l:gotowinnr . " wincmd w"
	exe "e " . fnameescape(l:buf[0] . "/" . l:buf[1])
    endif
	
    "if we were asked to close the window
    if a:closebuffer == 1
	exe "bdelete " . l:tocbufnr
    endif

    "finally, set the position
    call setpos('.',[0,l:nr,1,0])
    exe "normal zt"
    
endfunction
" endif

function! s:yank(arg)

    let l:toc=getbufline("%",1,"$")
    let l:h_line=index(reverse(copy(l:toc)),'')+1
    if line(".") > len(l:toc)-l:h_line
	return ''
    endif

    let l:cbufnr=bufnr("")
    let l:buf=s:file()
    let l:bufname=l:buf[1]
    let l:filename=l:buf[0] . "/" . l:buf[1]

    if exists("t:atp_labels") || get(t:atp_labels,l:filename,"nofile") != "nofile"
	let l:choice=get(get(t:atp_labels,l:filename),s:getlinenr())
    else
	let l:choice="nokey"
    endif

    if l:choice=="nokey"
	" in TOC, if there is a key we will give it back if not:
	au! CursorHold __ToC__
	echomsg "There is no key."
	sleep 759m
	au CursorHold __ToC__ :call EchoLine()
	return ""
    else
	if a:arg == '@'
	    let l:letter=input("To which register? <reg name><Enter> or empty for none ")
	    silent if l:letter == 'a'
		let @a=l:choice
	    elseif l:letter == 'b'
		let @b=l:choice
	    elseif l:letter == 'c'
		let @c=l:choice
	    elseif l:letter == 'd'
		let @d=l:choice
	    elseif l:letter == 'e'
		let @e=l:choice
	    elseif l:letter == 'f'
		let @f=l:choice
	    elseif l:letter == 'g'
		let @g=l:choice
	    elseif l:letter == 'h'
		let @h=l:choice
	    elseif l:letter == 'i'
		let @i=l:choice
	    elseif l:letter == 'j'
		let @j=l:choice
	    elseif l:letter == 'k'
		let @k=l:choice
	    elseif l:letter == 'l'
		let @l=l:choice
	    elseif l:letter == 'm'
		let @m=l:choice
	    elseif l:letter == 'n'
		let @n=l:choice
	    elseif l:letter == 'o'
		let @o=l:choice
	    elseif l:letter == 'p'
		let @p=l:choice
	    elseif l:letter == 'q'
		let @q=l:choice
	    elseif l:letter == 'r'
		let @r=l:choice
	    elseif l:letter == 's'
		let @s=l:choice
	    elseif l:letter == 't'
		let @t=l:choice
	    elseif l:letter == 'u'
		let @u=l:choice
	    elseif l:letter == 'v'
		let @v=l:choice
	    elseif l:letter == 'w'
		let @w=l:choice
	    elseif l:letter == 'x'
		let @x=l:choice
	    elseif l:letter == 'y'
		let @y=l:choice
	    elseif l:letter == 'z'
		let @z=l:choice
	    elseif l:letter == '*'
		let @-=l:choice
	    elseif l:letter == '+'
		let @+=l:choice
	    elseif l:letter == '-'
		let @@=l:choice
	    endif
	    echohl WarningMsg | echomsg "Choice yanked to the register '" . l:letter . "'" | echohl None
	elseif a:arg =='p'

	    let l:gotowinnr=s:gotowinnr()
	    exe l:gotowinnr . " wincmd w"

	    " delete the buffer
	    exe "bdelete " . l:cbufnr

	    " set the line
	    let l:line=getline('.')
	    let l:colpos=getpos('.')[2]
	    let l:bline=strpart(l:line,0,l:colpos)
	    let l:eline=strpart(l:line,l:colpos)
	    call setline('.',l:bline . l:choice . l:eline)
	    call setpos('.',[getpos('.')[0],getpos('.')[1],getpos('.')[2]+len(l:choice),getpos('.')[3]])
	endif
    endif
endfunction

command! -buffer P :call Yank("p")

if !exists("*YankToReg")
function! YankToReg()
    call s:yank("@")
endfunction
endif

if !exists("*Paste")
function! Paste()
    call s:yank("p")
endfunction
endif
command! -buffer -nargs=1 Y :call YankToReg(<f-arg>)

if !exists("*ShowLabelContext")
function! ShowLabelContext()

    let l:toc=getbufline("%",1,"$")
    let l:h_line=index(reverse(copy(l:toc)),'')+1
    if line(".") > len(l:toc)-l:h_line
	return ''
    endif

    let l:cbufname=bufname('%')
    let l:bufname=s:file()[1]
    let l:bufnr=bufnr("^" . l:bufname . "$")
    let l:winnr=bufwinnr(l:bufname)
" 	echomsg "DEBUG bufname " . l:bufname
    let l:line=s:getlinenr()
    if !exists("t:atp_labels")
	let t:atp_labels=UpdateLabels(l:bufname)
    endif
    exe l:winnr . " wincmd w"
	if l:winnr == -1
	    exe "e #" . l:bufnr
	endif
    exe "12split "
    call setpos('.',[0,l:line,1,0])
endfunction
endif

if !exists("*EchoLine")
function! EchoLine()

    let l:toc=getbufline("%",1,"$")
    let l:h_line=index(reverse(copy(l:toc)),'')+1
    if line(".") > len(l:toc)-l:h_line
	return ''
    endif

    let l:bufname=s:file()[1]
    let l:bufnr=bufnr("^" . l:bufname . "$")
    if !exists("t:atp_labels")
	let t:atp_labels[l:bufname]=UpdateLabels(l:bufname)[l:bufname]
    endif
    let l:line=s:getlinenr()
    let l:sec_line=join(getbufline(l:bufname,l:line))
    let l:sec_type=""
    if l:sec_line =~ '\\subparagraph[^\*]'
	let l:sec_type="subparagraph"
    elseif l:sec_line =~ '\\subparagraph\*'
	let l:sec_type="subparagraph*"
    elseif l:sec_line =~ '\\paragraph[^\*]'
	let l:sec_type="paragraph"
    elseif l:sec_line =~ '\\paragraph\*'
	let l:sec_type="paragraph*"
    elseif l:sec_line =~ '\\subsubsection[^\*]'
	let l:sec_type="subsubsection"
    elseif l:sec_line =~ '\\subsubsection\*'
	let l:sec_type="subsubsection*"
    elseif l:sec_line =~ '\\subsection[^\*]'
	let l:sec_type="subsection"
    elseif l:sec_line =~ '\\subsection\*'
	let l:sec_type="subsection*"
    elseif l:sec_line =~ '\\section[^\*]'
	let l:sec_type="section"
    elseif l:sec_line =~ '\\section\*'
	let l:sec_type="section*"
    elseif l:sec_line =~ '\\chapter[^\*]'
	let l:sec_type="chapter"
    elseif l:sec_line =~ '\\chapter\*'
	let l:sec_type="chapter*"
    elseif l:sec_line =~ '\\part[^\*]'
	let l:sec_type="part"
    elseif l:sec_line =~ '\\part\*'
	let l:sec_type="part*"
    elseif l:sec_line =~ '\\bibliography'
	let l:sec_type="bibliography"
    elseif l:sec_line =~ '\\abstract'
	let l:sec_type="abstract"
    endif

    let l:label=matchstr(l:sec_line,'\\label\s*{\zs[^}]*\ze}')
    let g:sec_line=l:sec_line
    let g:label=l:label
    if l:label != ""
	echo l:sec_type . " : '" . strpart(l:sec_line,stridx(l:sec_line,'{')+1,stridx(l:sec_line,'}')-stridx(l:sec_line,'{')-1) . "'\t label : " . l:label
    else
	echo l:sec_type . " : '" . strpart(l:sec_line,stridx(l:sec_line,'{')+1,stridx(l:sec_line,'}')-stridx(l:sec_line,'{')-1) . "'"
    endif
    return 0
endfunction
endif

" function! s:bdelete()
"     call s:deletevariables()
"     bdelete
" endfunction
" command -buffer Bdelete 	:call s:bdelete()

" TODO:
" function! Update()
"     l:cbufname=bufname("")
"     let l:bufname=substitute(l:cbufname,'\C\%(-TOC\|-LABELS\)$','','')
"     let l:bufnr=bufnr("^" . l:bufname . "$")
"     let t:atp_labels[l:bufname]=UpdateLabels(l:bufname)[l:bufname]
"     if l:cbufname =~ "-TOC$"
" 	" TODO
"     elseif l:cbufname =~ "-LABELS$"
" 	" TODO
"     endif
" endfunction

" To DoC
function! Help()
    echo "Available Mappings:"
    echo "q 		close ToC window"
    echo "<CR>  		go to and close"
    echo "<space>		go to"
    echo "c or y		yank the label to a register"
    echo "p		yank and paste the label (in the source file)"
    echo "e		echo the title to command line"
    echo "h		this help message"
endfunction

" MAPPINGS
if !exists("no_plugin_maps") && !exists("no_atp_toc_maps")
    map <silent> <buffer> q 		:bdelete<CR>
    map <silent> <buffer> <CR> 		:call GotoLine(1)<CR>
    map <silent> <buffer> <space> 	:call GotoLine(0)<CR>
" This does not work: 
"   noremap <silent> <buffer> <LeftMouse> :call GotoLine(0)<CR>
"   when the cursor is in another buffer (and the option mousefocuse is not
"   set) it calles the command instead of the function, I could add a check if
"   mouse is over the right buffer. With mousefocuse it also do not works very
"   well.
    map <buffer> c 			:call YankToReg()<CR>
    map <buffer> y 			:call YankToReg()<CR>
    noremap <silent> <buffer> p 	:call Paste()<CR>
    noremap <silent> <buffer> s 	:call ShowLabelContext()<CR> 
    noremap <silent> <buffer> e 	:call EchoLine()<CR>
    noremap <silent> <buffer> <F1>	:call Help()<CR>
endif
setl updatetime=200 
au CursorHold __ToC__ :call EchoLine()
ftplugin/fd_atp.vim	[[[1
74
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 July 9
" URL:		
"{{{ Load Once
if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1
"}}}
"{{{ OpenFile
if !exists("*OpenFile")
function! OpenFile()
    let line	= max([line("."), '2'])-2
    let file	= g:fd_matches[line]

    " The list of fd files starts at second line.
    let openbuffer	= "topleft split! +setl\\ nospell\\ ft=fd_atp\\ noro " . fnameescape(file)
    silent exe openbuffer
    let b:atp_autex=0
endfunction
endif
"}}}
"{{{ ShowFonts
function! ShowFonts(fd_file)

    let font_commands	= atplib#ShowFont(a:fd_file)

    let message		= ""
    for fcom in font_commands
	let message	.= "\n".fcom
    endfor
    let message="Fonts Declared:".message
    call confirm(message)
endfunction
"}}}
"{{{ Autocommand
au CursorHold fd_list* :echo g:fd_matches[(max([line("."),'2'])-2)]
"}}}
"{{{ Preview
function! Preview(...)

    let keep_tex = ( a:0 == 0 ? 0 : a:1 )

    let b_pos	= getpos("'<")[1]
    let e_pos	= getpos("'>")[1]

    if getpos("'<") != [0, 0, 0, 0] && getpos("'<") != [0, 0, 0, 0]
	let fd_files	= filter(copy(g:fd_matches),'index(g:fd_matches,v:val)+1 >= b_pos-1 && index(g:fd_matches,v:val)+1 <= e_pos-1')
    else
	let fd_files	= [g:fd_matches[(max([line("."),'2'])-2)]]
    endif

    call atplib#Preview(fd_files, keep_tex)

endfunction
"}}}
"{{{ Commands
if bufname("%") =~ 'fd_list'
    command! -buffer -nargs=? -range Preview	:call Preview(<f-args>)
    command! -buffer ShowFonts			:call ShowFonts(g:fd_matches[(max([line("."),'2'])-2)])
    map <buffer> <Enter> 			:call OpenFile()<CR>
    map <buffer> <Tab>				:call ShowFonts(g:fd_matches[(max([line("."),'2'])-2)])<CR>
else
    command! -buffer -nargs=1 Preview		:call atplib#Preview(["buffer"],<f-args>)
endif
"}}}
"{{{ Maps
map 	<buffer> 	P :Preview 1<CR>
map 	<buffer> 	p :Preview 0<CR>
vmap 	<buffer> 	P :Preview 1<CR>
vmap 	<buffer> 	p :Preview 0<CR>
map 	<buffer> 	Q :bd!<CR>
map 	<buffer> 	q :q!<CR>R
"}}}
autoload/atplib.vim	[[[1
3307
" Vim library for atp filetype plugin
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Email:	mszamot [AT] gmail [DOT] com

" This function appends '/' to b:atp_OutDir if it is not present. 
"{{{ atplib#outdir
function! atplib#outdir()
    if b:atp_OutDir !~ "\/$"
	let b:atp_OutDir=b:atp_OutDir . "/"
    endif
endfunction
"}}}

" Find labels.
" {{{1 atplib#LABELS
" the argument should be: resolved full path to the file:
" resove(fnamemodify(bufname("%"),":p"))

" {{{2 --------------- atplib#generatelabels
function! atplib#generatelabels(filename)
    let s:labels	= {}
    let bufname		= fnamemodify(a:filename,":t")

    " getbufline reads only loaded buffers, unloaded can be read from file.
    if bufloaded("^" . bufname . "$")
	let texfile=getbufline("^" . bufname . "$","1","$")
    else
	let texfile=readfile(a:filename)
    endif
    let true=1
    let i=0

"     " remove the preambule
"     while true == 1
" 	if texfile[0] =~ '\\begin\s*{document}'
" 		let true=0
" 	endif
" 	call remove(texfile,0)
" 	let i+=1
"     endwhile

    let i=0
    while i < len(texfile)
	if texfile[i] =~ '\\label\s*{'
	    let lname 	= matchstr(texfile[i], '\\label\s*{.*', '')
	    let start 	= stridx(lname, '{')+1
	    let lname 	= strpart(lname, start)
	    let end	= stridx(lname, '}')
	    let lname	= strpart(lname, 0, end)
    "This can be extended to have also the whole environment which
    "could be shown.
	    call extend(s:labels, { i+1 : lname })
	endif
	let i+=1 
    endwhile

    if exists("t:atp_labels")
	call extend(t:atp_labels, { a:filename : s:labels }, "force")
    else
	let t:atp_labels	= { a:filename : s:labels }
    endif
    return t:atp_labels
endfunction
" }}}2
" {{{2 --------------- atplib#showlabels
" The argument is the dictionary generated by atplib#generatelabels.
function! atplib#showlabels(labels)
    " the argument a:labels=t:atp_labels[bufname("")] !
    let l:cline=line(".")
    let l:lines=sort(keys(a:labels),"atplib#CompareList")
    " Open new window or jump to the existing one.
    let l:bufname=bufname("")
"     let l:bufpath=fnamemodify(bufname(""),":p:h")
    let l:bufpath=fnamemodify(resolve(fnamemodify(bufname("%"),":p")),":h")
    let l:bname="__Labels__"
    let l:labelswinnr=bufwinnr("^" . l:bname . "$")
    let t:atp_labelswinnr=winnr()
    let t:atp_labelsbufnr=bufnr("^" . l:bname . "$") 
    let l:labelswinnr=bufwinnr(t:atp_labelsbufnr)
    if l:labelswinnr != -1
	" Jump to the existing window.
	exe l:labelswinnr . " wincmd w"
	if l:labelswinnr != t:atp_labelswinnr
	    silent exe "%delete"
	else
	    echoerr "ATP error in function s:showtoc, TOC/LABEL buffer 
		    \ and the tex file buffer agree."
	    return
	endif
    else
    " Open new window if its width is defined (if it is not the code below
    " will put lab:cels in the current buffer so it is better to return.
	if !exists("t:atp_labels_window_width")
	    echoerr "t:atp_labels_window_width not set"
	    return
	endif
	let l:openbuffer=t:atp_labels_window_width . "vsplit +setl\\ buftype=nofile\\ filetype=toc_atp\\ syntax=labels_atp __Labels__"
	silent exe l:openbuffer
	silent call atplib#setwindow()
	let t:atp_labelsbufnr=bufnr("")
    endif
    call setline(1,l:bufname . " (" . l:bufpath . ")")
    let l:ln=2
    for l:line in l:lines
	call setline(l:ln, l:line . "\t" . a:labels[l:line]) 
	let l:ln+=1
    endfor
    " set the cursor position on the correct line number.
    let l:number=1
    for l:line in l:lines
    if l:cline>=l:line
	keepjumps call setpos('.',[bufnr(bufname('%')),l:number+1,1,0])
    elseif l:number == 1 && l:cline<l:line
	keepjumps call setpos('.',[bufnr(bufname('%')),l:number+1,1,0])
    endif
    let l:number+=1
    endfor
endfunction
" }}}2
" }}}1

"{{{1 atplib#CompareList
function! atplib#CompareList(i1, i2)
   return str2nr(a:i1) == str2nr(a:i2) ? 0 : str2nr(a:i1) > str2nr(a:i2) ? 1 : -1
endfunction
"}}}1
" {{{1 atplib#CompareCoordinates
" Each list is an argument with two values!
" listA=[line_nrA,col_nrA] usually given by searchpos() function
" listB=[line_nrB,col_nrB]
" returns 1 iff A is smaller than B
fun! atplib#CompareCoordinates(listA,listB)
    if a:listA[0] < a:listB[0] || 
	\ a:listA[0] == a:listB[0] && a:listA[1] < a:listB[1] ||
	\ a:listA == [0,0]
	" the meaning of the last is that if the searchpos() has not found the
	" beginning (a:listA) then it should return 1 : the env is not started.
	return 1
    else
	return 0
    endif
endfun
"}}}1
" ReadInputFile function reads finds a file inn tex style and returns the list
" of its lines. 
" {{{1 atplib#ReadInputFile
" this function looks for an input file: in the list of buffers, under a path if
" it is given, then in the b:atp_OutDir.
" directory. The last argument if equal to 1, then look also
" under g:texmf.
function! atplib#ReadInputFile(ifile,check_texmf)

    let l:input_file = []

    " read the buffer or read file if the buffer is not listed.
    if buflisted(fnamemodify(a:ifile,":t"))
	let l:input_file=getbufline(fnamemodify(a:ifile,":t"),1,'$')
    " if the ifile is given with a path it should be tried to read from there
    elseif filereadable(a:ifile)
	let l:input_file=readfile(a:ifile)
    " if not then try to read it from b:atp_OutDir
    elseif filereadable(b:atp_OutDir . fnamemodify(a:ifile,":t"))
	let l:input_file=readfile(filereadable(b:atp_OutDir . fnamemodify(a:ifile,":t")))
    " the last chance is to look for it in the g:texmf directory
    elseif a:check_texmf && filereadable(findfile(a:ifile,g:texmf . '**'))
	let l:input_file=readfile(findfile(a:ifile,g:texmf . '**'))
    endif

    return l:input_file
endfunction
" }}}1

" These are all bibsearch realted variables and functions.
"{{{ BIBSEARCH
"{{{ atplib#variables
let atplib#bibflagsdict={ 't' : ['title', 'title        '] , 'a' : ['author', 'author       '], 
		\ 'b' : ['booktitle', 'booktitle    '], 'c' : ['mrclass', 'mrclass      '], 
		\ 'e' : ['editor', 'editor       '], 	'j' : ['journal', 'journal      '], 
		\ 'f' : ['fjournal', 'fjournal     '], 	'y' : ['year', 'year         '], 
		\ 'n' : ['number', 'number       '], 	'v' : ['volume', 'volume       '], 
		\ 's' : ['series', 'series       '], 	'p' : ['pages', 'pages        '], 
		\ 'P' : ['publisher', 'publisher    '], 'N' : ['note', 'note         '], 
		\ 'S' : ['school', 'school       '], 	'h' : ['howpublished', 'howpublished '], 
		\ 'o' : ['organization', 'organization '], 'I' : ['institution' , 'institution '],
		\ 'u' : ['url', 'url          '],
		\ 'H' : ['homepage', 'homepage     '], 	'i' : ['issn', 'issn         '],
		\ 'k' : ['key', 'key          ']}
" they do not work in the library script :(
" using g:bibflags... .
" let atplib#bibflagslist=keys(atplib#bibflagsdict)
" let atplib#bibflagsstring=join(atplib#bibflagslist,'')
"}}}
" This functions finds bibfiles defined in the tex source file. 
"{{{ atplib#searchbib
" ToDo should not search in comment lines.

" To make it work after kpsewhich is searching for bib path.
" let s:bibfiles=FindBibFiles(bufname('%'))
function! atplib#searchbib(pattern) 
" 	echomsg "DEBUG pattern" a:pattern
    call atplib#outdir()
    let s:bibfiles=keys(FindBibFiles(bufname('%')))
    
    " Make a pattern which will match for the elements of the list g:bibentries
    let l:pattern = '^\s*@\%(\<'.g:bibentries[0].'\>'
    for l:bibentry in g:bibentries['1':len(g:bibentries)]
	let l:pattern=l:pattern . '\|\<' . l:bibentry . '\>'
    endfor
    let l:pattern=l:pattern . '\)'
    let b:pattern=l:pattern
" This pattern matches all entry lines: author = \| title = \| ... 
    let l:pattern_b = '^\s*\%('
    for l:bibentry in keys(g:bibflagsdict)
	let l:pattern_b=l:pattern_b . '\|\<' . g:bibflagsdict[l:bibentry][0] . '\>'
    endfor
    let l:pattern_b.='\)\s*='
    let b:pattern_b=l:pattern_b

    unlet l:bibentry
    let b:bibentryline={} 
    
    " READ EACH BIBFILE IN TO DICTIONARY s:bibdict, WITH KEY NAME BEING THE bibfilename
    let s:bibdict={}
    let l:bibdict={}
    for l:f in s:bibfiles
	let s:bibdict[l:f]=[]

	" read the bibfile if it is in b:atp_OutDir or in g:atp_bibinputs directory
	" ToDo: change this to look in directories under g:atp_bibinputs. 
	" (see also ToDo in FindBibFiles 284)
	for l:path in g:atp_bibinputs 
	    " it might be problem when there are multiple libraries with the
	    " same name under different locations (only the last one will
	    " survive)
	    let s:bibdict[l:f]=readfile(fnameescape(findfile(atplib#append(l:f,'.bib'),atplib#append(l:path,"/") . "**")))
	endfor
	let l:bibdict[l:f]=copy(s:bibdict[l:f])
	" clear the s:bibdict values from lines which begin with %    
	" SPEED UP
	call filter(l:bibdict[l:f],' v:val !~ "^\\s*\\%(%\\|@\\cstring\\)"')
" 	let l:x=0
" 	for l:line in s:bibdict[l:f]
" 	    if l:line =~ '^\s*\%(%\|@\cstring\)' 
" 		call remove(l:bibdict[l:f],l:x)
" 	    else
" 		let l:x+=1
" 	    endif
" 	endfor
" 	unlet l:line
" 	END SPEED UP
    endfor
    let b:bibdict=deepcopy(l:bibdict)	" DEBUG

    " SPEED UP TODO: if a:pattern == "" there is no need to do that!
    " uncomment this when the rest will be ready!
    if a:pattern != ""
	for l:f in s:bibfiles
	    let l:list=[]
	    let l:nr=1
	    for l:line in l:bibdict[l:f]
		" if the line matches find the beginning of this bib field and add its
		" line number to the list l:list
		if substitute(l:line,'{\|}','','g') =~ a:pattern
		    let l:true=1
		    let l:t=0
		    while l:true == 1
			let l:tnr=l:nr-l:t
			" go back until the line will match l:pattern (which
			" should be the beginning of the bib field.
		       if l:bibdict[l:f][l:tnr-1] =~ l:pattern && l:tnr >= 0
			   let l:true=0
			   let l:list=add(l:list,l:tnr)
		       elseif l:tnr <= 0
			   let l:true=0
		       endif
		       let l:t+=1
		    endwhile
		endif
		let l:nr+=1
	    endfor
    " CLEAR THE l:list FROM ENTRIES WHICH APPEAR TWICE OR MORE --> l:clist
	    let l:pentry="A"		" We want to ensure that l:entry (a number) and l:pentry are different
	    for l:entry in l:list
		if l:entry != l:pentry
		    if count(l:list,l:entry) > 1
			while count(l:list,l:entry) > 1
			    let l:eind=index(l:list,l:entry)
			    call remove(l:list,l:eind)
			endwhile
		    endif 
		    let l:pentry=l:entry
		endif
	    endfor
	    let b:bibentryline=extend(b:bibentryline,{ l:f : l:list })
	endfor
    endif
"   CHECK EACH BIBFILE
    let l:bibresults={}

" NEW CODE: 
"     if the pattern was empty make it faster. 
    if a:pattern == ""
	for l:bibfile in keys(l:bibdict)
	    let l:bibfile_len=len(l:bibdict[l:bibfile])
	    let s:bibd={}
		let l:nr=0
		while l:nr < l:bibfile_len
		    let l:line=l:bibdict[l:bibfile][l:nr]
" 		    echomsg "LINE " . l:nr . "  " .  (l:line =~ l:pattern) . " line " . l:line
		    if l:line =~ l:pattern
			let s:lbibd={}
			let s:lbibd["bibfield_key"]=l:line
			let l:beg_line=l:nr+1
			let l:nr+=1
			let l:line=l:bibdict[l:bibfile][l:nr]
			let l:y=1
			while l:line !~ l:pattern && l:nr < l:bibfile_len
			    let l:line=l:bibdict[l:bibfile][l:nr]
			    let l:lkey=tolower(
					\ matchstr(
					    \ strpart(l:line,0,
						\ stridx(l:line,"=")
					    \ ),'\<\w*\>'
					\ ))
			" CONCATENATE LINES IF IT IS NOT ENDED
			    let l:y=1
			    if l:lkey != ""
				let s:lbibd[l:lkey]=l:line
	" IF THE LINE IS SPLIT ATTACH NEXT LINE									
" 				echomsg "l:nr=".l:nr. "       line=".l:line 
				let l:nline=get(l:bibdict[l:bibfile],l:nr+l:y)
" 				while l:line !~ '\%()\|}\|"\)\?\s*,\s*\%(%.*\)\?$' && 
" 					    \ l:nline !~ l:pattern
				while l:nline !~ '=' && 
					    \ l:nline !~ l:pattern &&
					    \ (l:nr+l:y) < l:bibfile_len
" 				THIS IS FAST BUT CAN BREAK FROM TIME TO TIME
" 				(for example if the title contains = )
" 				instead of l:nline !~ '=' it is better to
" 				check: 
" 				while l:nline !~ l:pattern_b &&
" 					    \ l:nline !~ l:pattern &&
" 					    \ (l:nr+l:y) < l:bibfile_len
" 			        (BUT IT TAKES 1s more!)  
				    let s:lbibd[l:lkey]=substitute(s:lbibd[l:lkey],'\s*$','','') . " ". substitute(get(l:bibdict[l:bibfile],l:nr+l:y),'^\s*','','')
				    let l:line=get(l:bibdict[l:bibfile],l:nr+l:y)
" 				    echomsg "l:nr=".l:nr. " l:y=".l:y." line=".l:line 
				    let l:y+=1
				    let l:nline=get(l:bibdict[l:bibfile],l:nr+l:y)
				    if l:y > 30
					echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry (check line " . l:nr . ") in " . l:f . " line=".l:line
					break
				    endif
				endwhile
				if l:nline =~ l:pattern 
" 				    echomsg "BREAK l:nr=".l:nr. " l:y=".l:y." nline=".l:nline 
				    let l:y=1
				endif
" 				OLD MECHANISM OF ATTACHING LINES
" 				echomsg "l:nr=".l:nr. "       line=".l:line 
" 				if l:line !~ '\%()\|}\|"\)\s*,\s*\%(%.*\)\?$'
" 				    let l:lline=substitute(l:line,'\\"\|\\{\|\\}\|\\(\|\\)','','g')
" 				    let l:pos=atplib#count(l:lline,"{")
" 				    let l:neg=atplib#count(l:lline,"}")
" 				    let l:m=l:pos-l:neg
" 				    let l:pos=atplib#count(l:lline,"(")
" 				    let l:neg=atplib#count(l:lline,")")
" 				    let l:n=l:pos-l:neg
" 				    let l:o=atplib#count(l:lline,"\"")
" 	    " this checks if bracets {}, and () and "" appear in pairs in the current line:  
" 				    if l:m>0 || l:n>0 || l:o>l:o/2*2 
" 					while l:m>0 || l:n>0 || l:o>l:o/2*2 
" 					    let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:nr+l:y),"{")
" 					    let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:nr+l:y),"}")
" 					    let l:m+=l:pos-l:neg
" 					    let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:nr+l:y),"(")
" 					    let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:nr+l:y),")")
" 					    let l:n+=l:pos-l:neg
" 					    let l:o+=atplib#count(get(l:bibdict[l:bibfile],l:nr+l:y),"\"")
" 	    " Let's append the next line: 
" 					    let s:lbibd[l:lkey]=substitute(s:lbibd[l:lkey],'\s*$','','') . " ". substitute(get(l:bibdict[l:bibfile],l:nr+l:y),'^\s*','','')
" 					    let l:y+=1
" 					    if l:y > 30
" 						echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry at line " . l:linenr . " (check line " . l:nr . ") in " . l:f
" 						break
" 					    endif
" 					endwhile
" 				    endif
" 				endif
" 			    END OF OLD MECHANISM OF ATTACHING LINES




			    endif
" 			    echomsg "SUB LINE l:nr" . l:nr . "  l:y" . l:y
			    let l:nr+=l:y
			    unlet l:y
			endwhile
			let l:nr-=1
" 			echomsg "END WHILE " . l:nr
" 			echo s:lbibd
			call extend(s:bibd, { l:beg_line : s:lbibd })
		    else
			let l:nr+=1
		    endif
" 		    echomsg "LINE END " . l:nr . " " .  l:bibdict[l:bibfile][l:nr]
		endwhile
	    let l:bibresults[l:bibfile]=s:bibd
	    let g:bibresults=l:bibresults
	endfor
	let g:bbibresults=l:bibresults
	return l:bibresults
    endif
    " END OF NEW CODE: (up)

    for l:bibfile in keys(b:bibentryline)
	let l:f=l:bibfile . ".bib"
"s:bibdict[l:f])	CHECK EVERY STARTING LINE (we are going to read bibfile from starting
"	line till the last matching } 
 	let s:bibd={}
 	for l:linenr in b:bibentryline[l:bibfile]
"
" 	new algorithm is on the way, using searchpair function
" 	    l:time=0
" 	    l:true=1
" 	    let b:pair1=searchpair('(','',')','b')
" 	    let b:pair2=searchpair('{','','}','b')
" 	    let l:true=b:pair1+b:pair2
" 	    while l:true == 0
" 		let b:pair1p=b:pair1	
" 		let b:pair1=searchpair('(','',')','b')
" 		let b:pair2p=b:pair2	
" 		let b:pair2=searchpair('{','','}','b')
" 		let l:time+=1
" 	    endwhile
" 	    let l:bfieldline=l:time

	    let l:nr=l:linenr-1
	    let l:i=atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),"{")-atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),"}")
	    let l:j=atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),"(")-atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),")") 
	    let s:lbibd={}
	    let s:lbibd["bibfield_key"]=get(l:bibdict[l:bibfile],l:linenr-1)
	    let l:x=1
" we go from the first line of bibentry, i.e. @article{ or @article(, until the { and (
" will close. In each line we count brackets.	    
            while l:i>0	|| l:j>0
		let l:tlnr=l:x+l:linenr
		let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),"{")
		let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),"}")
		let l:i+=l:pos-l:neg
		let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),"(")
		let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),")")
		let l:j+=l:pos-l:neg
		let l:lkey=tolower(
			    \ matchstr(
				\ strpart(get(l:bibdict[l:bibfile],l:tlnr-1),0,
				    \ stridx(get(l:bibdict[l:bibfile],l:tlnr-1),"=")
				\ ),'\<\w*\>'
			    \ ))
		if l:lkey != ""
		    let s:lbibd[l:lkey]=get(l:bibdict[l:bibfile],l:tlnr-1)
			let l:y=0
" IF THE LINE IS SPLIT ATTACH NEXT LINE									
			if get(l:bibdict[l:bibfile],l:tlnr-1) !~ '\%()\|}\|"\)\s*,\s*\%(%.*\)\?$'
" 				    \ get(l:bibdict[l:bibfile],l:tlnr) !~ l:pattern_b
			    let l:lline=substitute(get(l:bibdict[l:bibfile],l:tlnr+l:y-1),'\\"\|\\{\|\\}\|\\(\|\\)','','g')
			    let l:pos=atplib#count(l:lline,"{")
			    let l:neg=atplib#count(l:lline,"}")
			    let l:m=l:pos-l:neg
			    let l:pos=atplib#count(l:lline,"(")
			    let l:neg=atplib#count(l:lline,")")
			    let l:n=l:pos-l:neg
			    let l:o=atplib#count(l:lline,"\"")
    " this checks if bracets {}, and () and "" appear in pairs in the current line:  
			    if l:m>0 || l:n>0 || l:o>l:o/2*2 
				while l:m>0 || l:n>0 || l:o>l:o/2*2 
				    let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"{")
				    let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"}")
				    let l:m+=l:pos-l:neg
				    let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"(")
				    let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),")")
				    let l:n+=l:pos-l:neg
				    let l:o+=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"\"")
    " Let's append the next line: 
				    let s:lbibd[l:lkey]=substitute(s:lbibd[l:lkey],'\s*$','','') . " ". substitute(get(l:bibdict[l:bibfile],l:tlnr+l:y),'^\s*','','')
				    let l:y+=1
				    if l:y > 30
					echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry at line " . l:linenr . " (check line " . l:tlnr . ") in " . l:f
					break
				    endif
				endwhile
			    endif
			endif
		endif
" we have to go line by line and we could skip l:y+1 lines, but we have to
" keep l:m, l:o values. It do not saves much.		
		let l:x+=1
		if l:x > 30
			echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry at line " . l:linenr . " in " . l:f
			break
	        endif
		let b:x=l:x
		unlet l:tlnr
	    endwhile
	    
	    let s:bibd[l:linenr]=s:lbibd
	    unlet s:lbibd
	endfor
	let l:bibresults[l:bibfile]=s:bibd
    endfor
    let g:bibresults=l:bibresults
    return l:bibresults
endfunction
"}}}
" This is the main search engine.
" {{{ atplib#SearchBibItems
" the argument should be b:atp_MainFile but in any case it is made in this way.
" it specifies in which file to search for include files.
function! atplib#SearchBibItems(name)

    " we are going to make a dictionary { citekey : label } (see :h \bibitem) 
    let l:citekey_label_dict={}

    " make a list of include files.
    let l:inputfile_dict=FindInputFiles(a:name,0)
    let l:includefile_list=[]
    for l:key in keys(l:inputfile_dict)
	if l:inputfile_dict[l:key][0] =~ '^\%(include\|input\|includeonly\)$'
	    call add(l:includefile_list,atplib#append(l:key,'.tex'))
	endif
    endfor
    call add(l:includefile_list,b:atp_MainFile) 
"     let b:ifl=l:includefile_list

    " search for bibitems in all include files.
    for l:ifile in l:includefile_list

	let l:input_file = atplib#ReadInputFile(l:ifile,0)

	    " search for bibitems and make a dictionary of labels and citekeys
	    for l:line in l:input_file
		if l:line =~ '\\bibitem'
		    let l:label=matchstr(l:line,'\\bibitem\s*\[\zs[^]]*\ze\]')
		    let l:key=matchstr(l:line,'\\bibitem\s*\%(\[[^]]*\]\)\?\s*{\zs[^}]*\ze}') 
" 		    if l:label =~ 'bibitem'
" 			let l:label=''
" 		    endif
		    if l:key != ""
			call extend(l:citekey_label_dict, { l:key : l:label }, 'error') 
		    endif
		endif
	    endfor
    endfor
	
    return l:citekey_label_dict
endfunction
" }}}
" Showing results 
"{{{ atplib#showresults
" FLAGS:
" for currently supported flags see ':h atp_bibflags'
" All - all flags	
" L - last flag
" a - author
" e - editor
" t - title
" b - booktitle
" j - journal
" s - series
" y - year
" n - number
" v - volume
" p - pages
" P - publisher
" N - note
" S - school
" h - howpublished
" o - organization
" i - institution

function! atplib#showresults(bibresults,flags,pattern)
 
    "if nothing was found inform the user and return:
    if len(a:bibresults) == count(a:bibresults,{})
	echo "BibSearch: no bib fields matched."
	return 0
    endif


    function! s:showvalue(value)
	return substitute(strpart(a:value,stridx(a:value,"=")+1),'^\s*','','')
    endfunction

    let s:z=1
    let l:ln=1
    let l:listofkeys={}
"--------------SET UP FLAGS--------------------------    
	    let l:allflagon=0
	    let l:flagslist=[]
	    let l:kwflagslist=[]
    " flags o and i are synonims: (but refer to different entry keys): 
	if a:flags =~ '\Ci' && a:flags !~ '\Co'
	    let a:flags=substitute(a:flags,'i','io','') 
	elseif a:flags !~ '\Ci' && a:flags =~ '\Co'
	    let a:flags=substitute(a:flags,'o','oi','')
	endif
	if a:flags !~ 'All'
	    if a:flags =~ 'L'
 		if strpart(a:flags,0,1) != '+'
 		    let l:flags=b:atp_LastBibFlags . substitute(strpart(a:flags,0),'\CL','','g')
 		else
 		    let l:flags=b:atp_LastBibFlags . substitute(a:flags,'\CL','','g')
 		endif
	    else
		if a:flags == "" 
		    let l:flags=g:defaultbibflags
		elseif strpart(a:flags,0,1) != '+' && a:flags !~ 'All' 
		    let l:flags=a:flags
		elseif strpart(a:flags,0,1) == '+' && a:flags !~ 'All'
		    let l:flags=g:defaultbibflags . strpart(a:flags,1)
		endif
	    endif
	    let b:atp_LastBibFlags=substitute(l:flags,'+\|L','','g')
		if l:flags != ""
		    let l:expr='\C[' . g:bibflagsstring . ']' 
		    while len(l:flags) >=1
			let l:oneflag=strpart(l:flags,0,1)
    " if we get a flag from the variable g:bibflagsstring we copy it to the list l:flagslist 
			if l:oneflag =~ l:expr
			    let l:flagslist=add(l:flagslist,l:oneflag)
			    let l:flags=strpart(l:flags,1)
    " if we get '@' we eat ;) two letters to the list l:kwflagslist			
			elseif l:oneflag == '@'
			    let l:oneflag=strpart(l:flags,0,2)
			    if index(keys(g:kwflagsdict),l:oneflag) != -1
				let l:kwflagslist=add(l:kwflagslist,l:oneflag)
			    endif
			    let l:flags=strpart(l:flags,2)
    " remove flags which are not defined
			elseif l:oneflag !~ l:expr && l:oneflag != '@'
			    let l:flags=strpart(l:flags,1)
			endif
		    endwhile
		endif
	else
    " if the flag 'All' was specified. 	    
	    let l:flagslist=split(g:defaultallbibflags, '\zs')
	    let l:af=substitute(a:flags,'All','','g')
	    for l:kwflag in keys(g:kwflagsdict)
		if a:flags =~ '\C' . l:kwflag	
		    call extend(l:kwflagslist,[l:kwflag])
		endif
	    endfor
	endif
" 	let b:flagslist=l:flagslist			" DEBUG
" 	let b:kwflagslist=l:kwflagslist			" DEBUG

	"NEW: if there are only keyword flags append default flags
	if len(l:kwflagslist) > 0 && len(l:flagslist) == 0 
	    let l:flagslist=split(g:defaultbibflags,'\zs')
	endif

"   Open a new window.
    let l:bufnr=bufnr("___" . a:pattern . "___"  )
    if l:bufnr != -1
	let l:bdelete=l:bufnr . "bwipeout"
	exe l:bdelete
    endif
    unlet l:bufnr
    let l:openbuffer=" +setl\\ buftype=nofile\\ filetype=bibsearch_atp " . fnameescape("___" . a:pattern . "___")
    if g:vertical ==1
	let l:openbuffer="vsplit " . l:openbuffer 
	let l:skip=""
    else
	let l:openbuffer="split " . l:openbuffer 
	let l:skip="       "
    endif
    silent exe l:openbuffer

"     set the window options
    silent call atplib#setwindow()
" make a dictionary of clear values, which we will fill with found entries. 	    
" the default value is no<keyname>, which after all is matched and not showed
" SPEED UP:
    let l:values={'bibfield_key' : 'nokey'}	
    for l:flag in g:bibflagslist
	let l:values_clear=extend(l:values,{ g:bibflagsdict[l:flag][0] : 'no' . g:bibflagsdict[l:flag][0] })
    endfor

" SPEED UP: 
    let l:kwflag_pattern="\\C"	
    let l:len_kwflgslist=len(l:kwflagslist)
    let l:kwflagslist_rev=reverse(deepcopy(l:kwflagslist))
    for l:lkwflag in l:kwflagslist
	if index(l:kwflagslist_rev,l:lkwflag) == 0 
	    let l:kwflag_pattern.=g:kwflagsdict[l:lkwflag]
	else
	    let l:kwflag_pattern.=g:kwflagsdict[l:lkwflag].'\|'
	endif
    endfor
"     let b:kwflag_pattern=l:kwflag_pattern

    for l:bibfile in keys(a:bibresults)
	if a:bibresults[l:bibfile] != {}
	    call setline(l:ln, "Found in " . l:bibfile )	
	    let l:ln+=1
	endif
	for l:linenr in copy(sort(keys(a:bibresults[l:bibfile]),"atplib#CompareList"))
	    let l:values=deepcopy(l:values_clear)
	    let b:values=l:values
" fill l:values with a:bibrsults	    
	    let l:values["bibfield_key"]=a:bibresults[l:bibfile][l:linenr]["bibfield_key"]
" 	    for l:key in keys(l:values)
" 		if l:key != 'key' && get(a:bibresults[l:bibfile][l:linenr],l:key,"no" . l:key) != "no" . l:key
" 		    let l:values[l:key]=a:bibresults[l:bibfile][l:linenr][l:key]
" 		endif
" SPEED UP:
		call extend(l:values,a:bibresults[l:bibfile][l:linenr],'force')
" 	    endfor
" ----------------------------- SHOW ENTRIES -------------------------
" first we check the keyword flags, @a,@b,... it passes if at least one flag
" is matched
	    let l:check=0
" 	    for l:lkwflag in l:kwflagslist
" 	        let l:kwflagpattern= '\C' . g:kwflagsdict[l:lkwflag]
" 		if l:values['bibfield_key'] =~ l:kwflagpattern
" 		   let l:check=1
" 		endif
" 	    endfor
	    if l:values['bibfield_key'] =~ l:kwflag_pattern
		let l:check=1
	    endif
	    if l:check == 1 || len(l:kwflagslist) == 0
		let l:linenumber=index(s:bibdict[l:bibfile],l:values["bibfield_key"])+1
 		call setline(l:ln,s:z . ". line " . l:linenumber . "  " . l:values["bibfield_key"])
		let l:ln+=1
 		let l:c0=atplib#count(l:values["bibfield_key"],'{')-atplib#count(l:values["bibfield_key"],'(')

	
" this goes over the entry flags:
		for l:lflag in l:flagslist
" we check if the entry was present in bibfile:
		    if l:values[g:bibflagsdict[l:lflag][0]] != "no" . g:bibflagsdict[l:lflag][0]
" 			if l:values[g:bibflagsdict[l:lflag][0]] =~ a:pattern
			    call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " = " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
			    let l:ln+=1
" 			else
" 			    call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " = " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
" 			    let l:ln+=1
" 			endif
		    endif
		endfor
		let l:lastline=getline(line('$'))
		let l:c1=atplib#count(l:lastline,'{')-atplib#count(l:lastline,'}')
		let l:c2=atplib#count(l:lastline,'(')-atplib#count(l:lastline,')')
		let l:c3=atplib#count(l:lastline,'\"')
" 		echomsg "last line " . line('$') . "     l:ln=" l:ln . "    l:c0=" . l:c0		"DEBUG
		if l:c0 == 1 && l:c1 == -1
		    call setline(line('$'),substitute(l:lastline,'}\s*$','',''))
		    call setline(l:ln,'}')
		    let l:ln+=1
		elseif l:c0 == 1 && l:c1 == 0	
		    call setline(l:ln,'}')
		    let l:ln+=1
		elseif l:c0 == -1 && l:c2 == -1
		    call setline(line('$'),substitute(l:lastline,')\s*$','',''))
		    call setline(l:ln,')')
		    let l:ln+=1
		elseif l:c0 == -1 && l:c1 == 0	
		    call setline(l:ln,')')
		    let l:ln+=1
		endif
		let l:listofkeys[s:z]=l:values["bibfield_key"]
		let s:z+=1
	    endif
	endfor
    endfor
    call matchadd("Search",a:pattern)
    " return l:listofkeys which will be available in the bib search buffer
    " as b:listofkeys (see the BibSearch function below)
    return l:listofkeys
endfunction
"}}}
"}}}

" This function sets the window options common for toc and bibsearch windows.
"{{{1 atplib#setwindow
" this function sets the options of BibSearch, ToC and Labels windows.
function! atplib#setwindow()
" These options are set in the command line
" +setl\\ buftype=nofile\\ filetype=bibsearch_atp   
" +setl\\ buftype=nofile\\ filetype=toc_atp\\ nowrap
" +setl\\ buftype=nofile\\ filetype=toc_atp\\ syntax=labels_atp
	setlocal nonumber
 	setlocal winfixwidth
	setlocal noswapfile	
	setlocal window
	setlocal nobuflisted
	if &filetype == "bibsearch_atp"
" 	    setlocal winwidth=30
	    setlocal nospell
	elseif &filetype == "toc_atp"
" 	    setlocal winwidth=20
	    setlocal nospell
	endif
endfunction
" }}}1
" {{{1 atplib#count
function! atplib#count(line,keyword,...)
   
    if a:0 == 0 || a:1 == 0
	let l:method=0
    elseif a:1 == 1
	let l:method=1
    endif

    let l:line=a:line
    let l:i=0  
    if l:method==0
	while stridx(l:line,a:keyword) != '-1'
" 		if stridx(l:line,a:keyword) !='-1' 
	    let l:line=strpart(l:line,stridx(l:line,a:keyword)+1)
" 		endif
	    let l:i+=1
	endwhile
    elseif l:method==1
	let l:line=escape(l:line,'\\')
" 	let b:line=l:line " DEBUG
	while match(l:line,a:keyword . '\zs.*') != '-1'
	    let l:line=strpart(l:line,match(l:line,a:keyword . '\zs.*'))
	    let l:i+=1
	endwhile
    endif
    return l:i
endfunction
" }}}1
" {{{1 atplib#append 	/ at the end of a directory name
fun! atplib#append(where,what)
    return substitute(a:where,a:what . "\s*$",'','') . a:what
endfun
" }}}1
" This is used in several places to find all input files.
" {{{1 atplib#FindInputFiles
" this function is for completion of \bibliography and \input commands it returns a list
" of all files under a:dir and in g:outdir with a given extension.
function! atplib#FindInputFilesInDir(dir,in_current_dir,ext)
	let l:raw_files=split(globpath(atplib#append(a:dir,'/'),'**'))
	if a:in_current_dir
	    call extend(l:raw_files,split(globpath(b:atp_OutDir,'*')))
	endif
" 	let b:raw=l:raw_files " DEBUG
	let l:file_list=[]
	for l:key in l:raw_files
	    if l:key =~ a:ext . '$'
		call add(l:file_list,l:key)
	    endif
	endfor
	return l:file_list
endfunction
" }}}1

" This functions cheks if an environment is closed/opened.
" atplib#CheckClosed {{{1
" check if last bpat is closed.
" starting from the current line, limits the number of
" lines to search. It returns 0 if the environment is not closed or the line
" number where it is closed (an env is cannot be closed in 0 line)

" ToDo: the two function should only check not commented lines!
"
" Method 0 makes mistakes if the pattern is \begin:\end, if
" \begin{env_name}:\end{env_names} rather no (unless there are nested
" environments in the same name.
" Method 1 doesn't make mistakes and thus is preferable.
" after testing I shall remove method 0
function! atplib#CheckClosed(bpat,epat,line,limit,...)

"     NOTE: THIS IS MUCH FASTER !!! or SLOWER !!! ???            
"
"     let l:pos_saved=getpos(".") 
"     let l:cline=line(".")
"     if a:line != l:cline
" 	let l:col=len(getline(a:line))
" 	keepjumps call setpos(".",[0,a:line,l:col,0])
"     endif
"     let l:line=searchpair(a:bpat,'',a:epat,'nWr','',max([(a:line+a:limit),1]))
"     if a:line != l:cline
" 	keepjumps call setpos(".",l:pos_saved)
"     endif
"     return l:line


    if a:0 == 0 || a:1 == 0
	let l:method = 0
    else
	let l:method = a:1
    endif
"     echomsg "DEBUG METHOD " . l:method

    let l:len=len(getbufline(bufname("%"),1,'$'))
    let l:nr=a:line

    if a:limit == "$" || a:limit == "-1"
	let l:limit=l:len-a:line
    else
	let l:limit=a:limit
    endif

    if l:method==0
	while l:nr <= a:line+l:limit
	    let l:line=getline(l:nr)
" 	    if l:line =~ '\\def\|\%(re\)\?newcommand\>'
" 		let l:nr+=1
" 		continue
" 	    endif
" 	    echomsg "CC line " . l:nr . " " . l:line
	" Check if Closed
	    if l:nr == a:line
		if strpart(l:line,getpos(".")[2]-1) =~ '\%(' . a:bpat . '.*\)\@<!' . a:epat
" " 		    echo "CC 1 l:nr " . l:nr
		    return l:nr
		endif
	    else
		if l:line =~ '\%(' . a:epat . '.*\)\@<!' . a:bpat
		    return 0
		elseif l:line =~ '\%(' . a:bpat . '.*\)\@<!' . a:epat 
"     	    if l:line =~ a:epat 
		    return l:nr
		endif
	    endif
	    let l:nr+=1
	endwhile

    elseif l:method==1

	let l:bpat_count=0
	let l:epat_count=0
	let l:begin_line=getline(a:line)
	let l:begin_line_nr=line(a:line)
" 	echomsg "CC DEBUG ------------"
	while l:nr <= a:line+l:limit
	    let l:line=getline(l:nr)
" 	    if l:line =~ '\\def\|\%(re\)\?newcommand\>'
" 		let l:nr+=1
" 		continue
" 	    endif
	" I assume that the env is opened in the line before!
	    let l:bpat_count+=atplib#count(l:line,a:bpat,1)
	    let l:epat_count+=atplib#count(l:line,a:epat,1)
" 	    echomsg "cc line nr " . l:nr . " bpat " . l:bpat_count . " epat " . l:epat_count
	    if (l:bpat_count+1) == l:epat_count && l:begin_line !~ a:bpat
" 		echomsg "A"
		return l:nr
	    elseif l:bpat_count == l:epat_count && l:begin_line =~ a:bpat
" 		echomsg "B"
		return l:nr
	    endif 
	    let l:nr+=1
	endwhile
	return 0
    endif
endfunction
" }}}1
" atplib#CheckOpened {{{1
" Usage: By default (a:0 == 0 || a:1 == 0 ) it returns line number where the
" environment is opened if the environment is opened and is not closed (for
" completion), else it returns 0. However, if a:1 == 1 it returns line number
" where the environment is opened, if we are inside an environment (it is
" opened and closed below the starting line or not closed at all), it if a:1
" = 2, it just check if env is opened without looking if it is closed (
" cursor position is important).
" a:1 == 0 first non closed
" a:1 == 2 first non closed by counting.

" this function doesn't check if sth is opened in lines which begins with '\\def\>'
" (some times one wants to have a command which opens an environment.

" Todo: write a faster function using searchpairpos() which returns correct
" values.
function! atplib#CheckOpened(bpat,epat,line,limit,...)


"     this is almost good:    
"     let l:line=searchpair(a:bpat,'',a:epat,'bnWr','',max([(a:line-a:limit),1]))
"     return l:line

    if a:0 == 0 || a:1 == 0
	let l:check_mode = 0
    elseif a:1 == 1
	let l:check_mode = 1
    elseif a:1 == 2
	let l:check_mode = 2
    endif

"     let b:check_mode=l:check_mode

    let l:len=len(getbufline(bufname("%"),1,'$'))
    let l:nr=a:line

    if a:limit == "^" || a:limit == "-1"
	let l:limit=a:line-1
    else
	let l:limit=a:limit
    endif

    if l:check_mode == 0 || l:check_mode == 1
	while l:nr >= a:line-l:limit && l:nr >= 1
	    let l:line=getline(l:nr)
" 	    if l:line =~ '\\def\|\%(re\)\?newcommand\>'
" 		let l:nr-=1
" 		continue
" 	    endif
" 	echo "DEBUG A " . l:nr . " " . l:line
		if l:nr == a:line
" 		    let l:x= a:bpat . '.\{-}' . a:epat
" 		    echomsg " DEBUG CifO " . l:x
			if substitute(strpart(l:line,0,getpos(".")[2]), a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ a:bpat
" 			    let b:cifo_return=1
			    return l:nr
			endif
		else
		    if l:check_mode == 0
			if substitute(l:line, a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ a:bpat
			    " check if it is closed up to the place where we start. (There
			    " is no need to check after, it will be checked anyway
			    " b a serrate call in TabCompletion.
			    if !atplib#CheckClosed(a:bpat,a:epat,l:nr,a:limit,0)
					    " LAST CHANGE 1->0 above
" 				let b:cifo_return=2 . " " . l:nr 
				return l:nr
			    endif
			endif
		    elseif l:check_mode == 1
			if substitute(l:line, a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ '\%(\\def\|\%(re\)\?newcommand\)\@<!' . a:bpat
			    let l:check=atplib#CheckClosed(a:bpat,a:epat,l:nr,a:limit,1)
" 		    echo "DEBUG line nr: " l:nr . " line: " . l:line . " check: " . l:check
			    " if env is not closed or is closed after a:line
			    if  l:check == 0 || l:check >= a:line
" 				let b:cifo_return=2 . " " . l:nr 
				return l:nr
			    endif
			endif
		    endif
		endif
	    let l:nr-=1
	endwhile
    elseif l:check_mode == 2
	let l:bpat_count=0
	let l:epat_count=0
	let l:begin_line=getline(".")
	let l:c=0
	while l:nr >= a:line-l:limit  && l:nr >= 1
	    let l:line=getline(l:nr)
" 	    if l:line =~ '\\def\|\%(re\)\?newcommand\>'
" 		let l:nr-=1
" 		continue
" 	    endif
	" I assume that the env is opened in line before!
" 		let l:line=strpart(l:line,getpos(".")[2])
	    let l:bpat_count+=atplib#count(l:line,a:bpat,1)
	    let l:epat_count+=atplib#count(l:line,a:epat,1)
" 		echomsg "co " . l:c . " lnr " . l:nr . " bpat " . l:bpat_count . " epat " . l:epat_count
	    if l:bpat_count == (l:epat_count+1+l:c) && l:begin_line != line(".") 
		let l:env_name=matchstr(getline(l:nr),'\\begin{\zs[^}]*\ze}')
		let l:check=atplib#CheckClosed('\\begin{' . l:env_name . '}', '\\end{' . l:env_name . '}',1,a:limit,1)
" 			echomsg "co DEBUG " l:check . " env " . l:env_name
		if !l:check
		    return l:nr
		else
		    let l:c+=1
		endif
	    elseif l:bpat_count == l:epat_count && l:begin_line == line(".")
		return l:nr
	    endif 
	    let l:nr-=1
	endwhile
    endif
    return 0 
endfunction
" }}}1
" {{{ atplib#CheckSyntaxGroups
" This functions returns one if one of the environment given in the list
" a:zones is present in they syntax stack at line a:1 and column a:0.
" a:zones =	a list of zones
" a:1	  = 	line nr (default: current cursor line)
" a:2     =	column nr (default: column before the current cursor position)
" The function doesn't make any checks if the line and column supplied are
" valid /synstack() function returns 0 rather than [] in such a case/.
function! atplib#CheckSyntaxGroups(zones,...)
    let line		= a:0 >= 2 ? a:1 : line(".")
    let col		= a:0 >= 2 ? a:2 : max([1, col(".")-1])
    let zones		= copy(a:zones)

    let synstack	= map(synstack( line, col), 'synIDattr(v:val, "name")') 

    return max(map(zones, "count(synstack, v:val)"))
endfunction
"}}}
" atplib#CopyIndentation {{{1
function! atplib#CopyIndentation(line)
    let raw_indent	= split(a:line,'\s\zs')
    let indent		= ""
    for char in raw_indent
	if char =~ '^\%(\s\|\t\)'
	    let indent.=char
	else
	    break
	endif
    endfor
    return indent
endfunction
"}}}1

" Tab Completion Related Functions
" atplib#SearchPackage {{{1
"
" This function searches if the package in question is declared or not.
" Returns 1 if it is and 0 if it is not.
" It was inspired by autex function written by Carl Mueller, math at carlm e4ward c o m
" name = package name (tikz library name)
" a:1  = stop line (number of the line \\begin{document} 
" a:2  = pattern matching the command (without '^[^%]*\\', just the name)
" to match \usetikzlibrary{...,..., - 
function! atplib#SearchPackage(name,...)

    let l:possaved=getpos(".")

    if a:0 == 0
	keepjumps call setpos(".", [0,1,1,0])
	let l:stopline=search('\\begin\s*{document}','nW')
    else
	if a:1 != 0
	    let l:stopline=a:1
	else
	    keepjumps call setpos(".", [0,1,1,0])
	    let l:stopline=search('\\begin\s*{document}','nW')
	endif
    endif

    if a:0 == 2
	let l:command=a:2
    else
	let l:command='usepackage\s*\%(\[[^]]*]\)\?'
    endif

    if l:stopline != 0

	keepjumps call setpos(".",[0,1,1,0])
	let l:return=search('^[^%]*\\'.l:command."\s*{[^}]*".a:name,'ncW',l:stopline)

	keepjump call setpos(".",l:possaved)
	return l:return

    else

	keepjumps call setpos(".",[0,1,1,0])
	let l:return=search('^[^%]*\\'.l:command."\s*{[^}]*".a:name,'ncW')

	keepjump call setpos(".",l:possaved)
	return l:return

    endif

endfunction
" }}}1
" atplib#DocumentClass {{{1
function! atplib#DocumentClass()

    let l:bufnr=bufnr(b:atp_MainFile)

    let l:n=1
    let l:line=join(getbufline(l:bufnr,l:n))

    if l:line =~ '\\documentclass'
" 	let b:line=l:line " DEBUG
	return substitute(l:line,'.*\\documentclass\s*\%(\[.*\]\)\?{\(.*\)}.*','\1','')
    endif
    while l:line !~ '\\documentclass'
	if l:line =~ '\\documentclass'
	    return substitute(l:line,'.*\\documentclass\s*\%(\[.*\]\)\?{\(.*\)}.*','\1','')
	endif
	let l:n+=1
	let l:line=join(getbufline(l:bufnr,l:n))
    endwhile
endfunction
" }}}1
" {{{1 atplib#FindFiles 	/ find files: bst, cls, etc ... /
function! atplib#FindFiles(format,ext)
    let l:path=substitute(substitute(system("kpsewhich -show-path ".a:format ),'!!','','g'),'\/\/\+','\/','g')
"     let b:path=l:path
    let l:path=substitute(l:path,':\|\n',',','g')
    let l:list=split(globpath(l:path,"**/*.".a:ext),'\n') 
    call map(l:list,'fnamemodify(v:val,":t:r")')
    return l:list
endfunction
" }}}1
" atplib#Extend {{{1
" arguments are the same as for extend(), but it adds only the entries which
" are not present.
function! atplib#Extend(list_a,list_b,...)
    let l:list_a=deepcopy(a:list_a)
    let l:list_b=deepcopy(a:list_b)
    let l:diff=filter(l:list_b,'count(l:list_a,v:val) == 0')
    if a:0 == 0
	return extend(l:list_a,l:diff)
    else
	return extend(l:list_a,l:diff,a:1)
    endif
endfunction
" }}}1
" {{{1 atplib#Add
function! atplib#Add(list,what)
    let l:new=[] 
    for l:el in a:list
	call add(l:new,l:el . a:what)
    endfor
    return l:new
endfunction
"}}}1

" atplib#CloseLastEnvironment {{{1
" a:1 = i	(append before, so the cursor will  be after - the dafult)  
" 	a	(append after)
" a:2 = math 		the pairs \(:\), $:$, \[:\] or $$:$$ (if filetype is
" 						plaintex or b:atp_TexFlavour="plaintex")
" 	environment
" 			by the way, treating the math pairs together is very fast. 
" a:3 = environment name (if present and non zero sets a:2 = environment)	
" a:4 = line number where environment is opened
" ToDo: Ad a highlight to messages!!! AND MAKE IT NOT DISAPPEAR SOME HOW?
" (redrawing doesn't help) CHECK lazyredraw. 
" Note: this function tries to not double the checks what to close if it is
" given in the arguments, and find only the information that is not given
" (possibly all the information as all arguments can be omitted).
function! atplib#CloseLastEnvironment(...)

    if a:0 == 0 
	let l:com 	= 'i'
    elseif a:0 >= "1" 
	let l:com 	= a:1
    endif

    if a:0 >= 2 && a:2 != ""
	let l:close 	= a:2
    else
	let l:close 	= 0
    endif

    if a:0 >= 3
	let l:env_name=a:3
	let l:close 	= "environment"
    else
	let l:env_name 	= 0
    endif

    if a:0 >= 4
	let l:bpos_env	= a:4
    else
	let l:bpos_env	= [0, 0]
    endif


"   {{{2 find the begining line of environment to close (if we are closing
"   an environment)
    if l:env_name == 0 && ( l:close == "environment" || l:close == 0 ) && l:close != "math"

	let filter 	= 'strpart(getline(''.''), 0, col(''.'') - 1) =~ ''\\\@<!%'''

	" Check if and environment is opened (\begin:\end):
	" This is the slow part :( 0.4s)
	if l:bpos_env == [0, 0]
	    let l:bpos_env		= searchpairpos('\\begin\s*{', '', '\\end\s*{', 'bnW', 'searchpair("\\\\begin\s*{".matchstr(getline("."),"\\\\begin\s*{\\zs[^}]*\\ze\}"), "", "\\\\end\s*{".matchstr(getline("."), "\\\\begin\s*{\\zs[^}]*\\ze}"), "nW", "", "line(".")+g:atp_completion_limits[2]")',max([1,(line(".")-g:atp_completion_limits[2])]))
"      	let l:bpos_env		= searchpairpos('\\begin\s*{', '', '\\end\s*{', 'bnW', 'searchpair("\\\\begin\s*{".matchstr(getline("."),"\\\\begin\s*{\\zs[^}]*\\ze\}"), "", "\\\\end\s*{".matchstr(getline("."), "\\\\begin\s*{\\zs[^}]*\\ze}"), "nW", "\\%(\\%(\\\\\\@<!\\\\\\)\\@<!%.*\\)\\@<!", "line(".")+g:atp_completion_limits[2]")',max([1,(line(".")-g:atp_completion_limits[2])]))
	endif

	let l:env_name		= matchstr(strpart(getline(l:bpos_env[0]),l:bpos_env[1]-1), '^\\begin\s*{\s*\zs[^}]*\ze*\s*}')

    " if a:3 (environment name) was given:
    elseif l:env_name != "0" && l:close == "environment" 

	let l:bpos_env	= searchpairpos('\\begin\s*{'.l:env_name.'}', '', '\\end\s*{'.l:env_name.'}', 'bnW', '',max([1,(line(".")-g:atp_completion_limits[2])]))

    endif
"   }}}2
"   {{{2 if a:2 was not given (l:close = 0) we have to check math modes as
"   well.
    if l:close == "0" || l:close == "math" 

	let stopline 		= search('^\s*$\|\\par\>', 'bnW')

	" Check if one of \(:\), \[:\], $:$, $$:$$ is opened using syntax
	" file. If it is fined the starting position.

	let synstack		= map(synstack(line("."),col(".")-1), 'synIDattr(v:val, "name")')
	let math_1		= (index(synstack, 'texMathZoneV') != -1 ? 1  : 0 )   
	    if math_1
		let bpos_math_1	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\\(', 'bnW', stopline)
		let b:p1 = bpos_math_1
		let l:begin_line= bpos_math_1[0]
		let math_mode	= "texMathZoneV"
	    endif
	" the \[:\] pair:
	let math_2		= (index(synstack, 'texMathZoneW') != -1 ? 1  : 0 )   
	    if math_2
		let bpos_math_2	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\\[', 'bnW', stopline)
		let b:p2 = bpos_math_2
		let l:begin_line= bpos_math_2[0]
		let math_mode	= "texMathZoneW"
	    endif
	" the $:$ pair:
	let math_3		= (index(synstack, 'texMathZoneX') != -1 ? 1  : 0 )   
	    if math_3
		let bpos_math_3	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\$\{1,1}', 'bnW', stopline)
		let b:p3 = bpos_math_3
		let l:begin_line= bpos_math_3[0]
		let math_mode	= "texMathZoneX"
	    endif
	if &filetype == 'plaintex' || b:atp_TexFlavour == 'plaintex'
	    " the $$:$$ pair:
	    let math_4		= (index(synstack, 'texMathZoneY') != -1 ? 1  : 0 )   
	    if math_4
		let bpos_math_4	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\$\{2,2}', 'bnW', stopline)
		let b:p4 = bpos_math_4
		let l:begin_line= bpos_math_4[0]
		let math_mode	= "texMathZoneY"
	    endif
	else
	    let math_4 	= "0"
	endif
    endif
"}}}2
"{{{2 set l:close if a:1 was not given.
if a:0 <= 1
" 	let l:begin_line=max([ l:begin_line_env, l:begin_line_imath, l:begin_line_dmath ])
    " now we can set what to close:
    " the synstack never contains two of the math zones: texMathZoneV,
    " texMathZoneW, texMathZoneX, texMathZoneY.
    if math_1 + math_2 + math_3 + math_4 >= 1
	let l:close = 'math'
    elseif l:begin_line_env
	let l:close = 'environment'
    endif
endif

let l:env=l:env_name
"}}}2

if l:close == "0" 
    return "there was nothing to close"
elseif ( &filetype != "plaintex" && b:atp_TexFlavour != "plaintex" && exists("math_4") && math_4 != "0" )
    return " in latex your are not supposed to use $$:$$, if you realy want to please set g:atp_TexFlavaour == 'tex' "
endif

let l:cline=getline(".")
let l:pos=getpos(".")
if l:close == "math"
    let l:line=getline(l:begin_line)
elseif l:close == "environment"
    let l:line=getline(l:bpos_env[0])
endif
" Copy the indentation of what we are closing.
let l:eindent=atplib#CopyIndentation(l:line)
"{{{2 close environment
    if l:close == 'environment'

	" Rules:
	" env & \[ \]: close in the same line 
	" unless it starts in a serrate line,
	" \( \): close in the same line. 
	"{{{3 close environment in the same line
	if l:line !~ '^\s*\%(\$\|\$\$\|[^\\]\\(\|[^\\]\\\[\)\?\s*\\begin\s*{[^}]*}\s*\%(([^)]*)\s*\|{[^}]*}\s*\|\[[^\]]*\]\s*\)\{,3}\%(\\label{[^}]*}\s*\)\?$' 
	    " 		    THIS WILL BE NEEDED LATER!
" 		    \ (l:close == 'display_math' 	&& l:line !~ '^\s*[^\\]\\\[\s*$') ||
" 		    \ (l:close == 'inline_math' 	&& (l:line !~ '^\s*[^\\]\\(\s*$' || l:begin_line == line("."))) ||
" 		    \ (l:close == 'dolar_math' 		&& l:cline =~ '\$')

	    " the above condition matches for the situations when we have to
	    " complete in the same line in four cases:
	    " l:close == environment, display_math, inline_math or
	    " dolar_math. 

	    " do not complete environments which starts in a definition.
" let b:cle_debug= (getline(l:begin_line) =~ '\\def\|\%(re\)\?newcommand') . " " . (l:begin_line != line("."))
" 	    if getline(l:begin_line) =~ '\\def\|\%(re\)\?newcommand' && l:begin_line != line(".")
"  		let b:cle_return="def"
" 		return b:cle_return
" 	    endif
	    if index(g:atp_no_complete, l:env) == '-1' &&
		\ !atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{' . l:env,'\%(%.*\)\@<!\\end\s*{' . l:env,line("."),g:atp_completion_limits[2])
		if l:com == 'a'  
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\end{'.l:env.'}' . strpart(l:cline,getpos(".")[2]))
		    let l:pos=getpos(".")
		    let l:pos[2]=len(strpart(l:cline,0,getpos(".")[2]) . '\end{'.l:env.'}')+1
		    keepjumps call setpos(".",l:pos)
		else
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\end{'.l:env.'}' . strpart(l:cline,getpos(".")[2]-1))
		    let l:pos=getpos(".")
		    let l:pos[2]=len(strpart(l:cline,0,getpos(".")[2]-1) . '\end{'.l:env.'}')+1
		    keepjumps call setpos(".",l:pos)
		endif
	    endif "}}}3
	"{{{3 close environment in a new line 
	else 

		" do not complete environments which starts in a definition.

		let l:error=0
		let l:prev_line_nr="-1"
		let l:cenv_lines=[]
		let l:nr=line(".")
		
		let l:line_nr=line(".")
		" l:line_nr number of line which we complete
		" l:cenv_lines list of closed environments (we complete after
		" line number maximum of these numbers.

		let l:pos=getpos(".")
		let l:pos_saved=deepcopy(l:pos)

		while l:line_nr >= 0
			let l:line_nr=search('\%(%.*\)\@<!\\begin\s*{','bW')
		    " match last environment openned in this line.
		    " ToDo: afterwards we can make it works for multiple openned
		    " envs.
		    let l:env_name=matchstr(getline(l:line_nr),'\%(%.*\)\@<!\\begin\s*{\zs[^}]*\ze}\%(.*\\begin\s*{[^}]*}\)\@!')
		    if index(g:atp_long_environments,l:env_name) != -1
			let l:limit=3
		    else
			let l:limit=2
		    endif
		    let l:close_line_nr=atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{' . l:env_name, 
				\ '\%(%.*\)\@<!\\end\s*{' . l:env_name,
				\ l:line_nr,g:atp_completion_limits[l:limit],1)

		    if l:close_line_nr != 0
			call add(l:cenv_lines,l:close_line_nr)
		    else
			break
		    endif
		    let l:line_nr-=1
		endwhile

" 		if g:atp_close_after_last_closed == 1	
		    keepjumps call setpos(".",l:pos)
" 		endif
		    
" 		let b:cenv_lines=deepcopy(l:cenv_lines) " DEBUG
" 		let b:line_nr=l:line_nr " DEBUG
			
		if getline(l:line_nr) =~ '\%(%.*\)\@<!\%(\\def\|\%(re\)\?newcommand\)' && l:line_nr != line(".")
" 		    let b:cle_return="def"
		    return
		endif

		" get all names of environments which begin in this line
		let l:env_names=[]
		let l:line=getline(l:line_nr)
		while l:line =~ '\\begin\s*{' 
		    let l:cenv_begins = match(l:line,'\%(%.*\)\@<!\\begin{\zs[^}]*\ze}\%(.*\\begin\s{\)\@!')
		    let l:cenv_name = matchstr(l:line,'\%(%.*\)\@<!\\begin{\zs[^}]*\ze}\%(.*\\begin\s{\)\@!')
		    let l:cenv_len=len(l:cenv_name)
		    let l:line=strpart(l:line,l:cenv_begins+l:cenv_len)
		    call add(l:env_names,l:cenv_name)
			" DEBUG:
" 			let b:env_names=l:env_names
" 			let b:line=l:line
" 			let b:cenv_begins=l:cenv_begins
" 			let b:cenv_name=l:cenv_name
		endwhile
		" thus we have a list of env names.
		
		" make a dictionary of lines where they closes. 
		" this is a list of pairs (I need the order!)
		let l:env_dict=[]

		" list of closed environments
		let l:cenv_names=[]

		for l:uenv in l:env_names
		    let l:uline_nr=atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{' . l:uenv . '}', 
				\ '\%(%.*\)\@<!\\end\s*{' . l:uenv . '}', l:line_nr, g:atp_completion_limits[2])
		    call extend(l:env_dict,[ l:uenv, l:uline_nr])
		    if l:uline_nr != '0'
			call add(l:cenv_names,l:uenv)
		    endif
		endfor
		
		" close unclosed environment

		" check if at least one of them is closed
		if len(l:cenv_names) == 0
		    let l:str=""
		    for l:uenv in l:env_names
			if index(g:atp_no_complete,l:uenv) == '-1'
			    let l:str.='\end{' . l:uenv .'}'
			endif
		    endfor
		    " l:uenv will remain the last environment name which
		    " I use!
		    " Do not append empty lines (l:str is empty if all l:uenv
		    " belongs to the g:atp_no_complete list.
		    if len(l:str) == 0
			return 0
		    endif
		    let l:eindent=atplib#CopyIndentation(getline(l:line_nr))
		    let l:pos=getpos(".")
		    if len(l:cenv_lines) > 0 

			let l:max=max(l:cenv_lines)
			let l:pos[1]=l:max+1
			" find the first closed item below the last closed
			" pair (below l:pos[1]). (I assume every env is in
			" a seprate line!
			let l:end=atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{','\%(%.*\)\@<!\\end\s*{',l:line_nr,g:atp_completion_limits[2],1)
" 			let b:info= " l:max=".l:max." l:end=".l:end." line('.')=".line(".")." l:line_nr=".l:line_nr
			" if the line was found append just befor it.
			echohl WarningMsg 
			if l:end != 0 
				if line(".") <= l:max
				    if line(".") <= l:end
					call append(l:max, l:eindent . l:str)
					echomsg l:str . " appended after line " . l:end
" 					let b:cle_return="append cenv_lines 1.1.1 before line " . l:max 
					call setpos(".",[0,l:max+1,len(l:eindent.l:str)+1,0])
				    else
					call append(l:end-1, l:eindent . l:str)
					echomsg l:str . " appended after line " . l:end
" 					let b:cle_return="append cenv_lines 1.1.2 before line " . l:max 
					call setpos(".",[0,l:end,len(l:eindent.l:str)+1,0])
				    endif
				elseif line(".") < l:end
				    call append(line("."), l:eindent . l:str)
				    echomsg l:str . " appended after line " . line(".")
" 				    let b:cle_return="append cenv_lines 1.2 before line " . line(".")
				    call setpos(".",[0,line(".")+1,len(l:eindent.l:str)+1,0])
				elseif line(".") >= l:end
				    call append(l:end-1, l:eindent . l:str)
				    echomsg l:str . " appended after line " . (l:end-1)
" 				    let b:cle_return="append cenv_lines 1.3 before line " . (l:end-1)
				    call setpos(".",[0,l:end,len(l:eindent.l:str)+1,0])
				endif
			else
			    if line(".") >= l:max
				call append(l:pos_saved[1], l:eindent . l:str)
				keepjumps call setpos(".",l:pos_saved)
				echomsg l:str . " appended after line " . line(".")
" 				let b:cle_return="append cenv_lines 2.1 after line " . line(".")
				call setpos(".",[0,l:pos_saved[1]+1,len(l:eindent.l:str)+1,0])
			    elseif line(".") < l:max
				call append(l:max, l:eindent . l:str)
				echomsg l:str . " appended after line " . l:max
" 				let b:cle_return="append cenv_lines 2.2 after line " . l:max
				call setpos(".",[0,l:max+1,len(l:eindent.l:str)+1,0])
" 			    elseif line(".") >= l:end
"				If we are to far				
" 				call append(l:end-1, l:eindent . l:str)
" 				echomsg l:str . " appended after line " . (l:end-1)
" 				let b:cle_return="append cenv_lines 2.3 before line " . (l:end-1)
			    endif
			endif
			echohl None 
		    else
			let l:pos[1]=l:line_nr
			let l:pos[2]=1
			" put cursor at the end of the line where not closed \begin was
			" found
			keepjumps call setpos(".",[0,l:line_nr,len(getline(l:line_nr)),0])
			let l:cline=getline(l:pos_saved[1])
			let l:iline=searchpair('\\begin{','','\\end{','nW')
			if l:iline > l:line_nr && l:iline <= l:pos_saved[1]
			    call append(l:iline-1, l:eindent . l:str)
			    echomsg l:str . " appended before line " . l:iline
" 			    let b:cle_return="append before if 3.1 str " . l:str . " before line " . l:iline
			    let l:pos_saved[2]+=len(l:str)
			    call setpos(".",[0,l:iline,len(l:eindent.l:str)+1,0])
			else
			    if l:cline =~ '\\begin{\%('.l:uenv.'\)\@!'
				call append(l:pos_saved[1]-1, l:eindent . l:str)
				echomsg l:str . " appended before line " . l:pos_saved[1]
" 				let b:cle_return="append before if 3.2.1 str " . l:str . " before line " . l:pos_saved[1] 
				let l:pos_saved[2]+=len(l:str)
				call setpos(".",[0,l:pos_saved[1],len(l:eindent.l:str)+1,0])
			    else
				call append(l:pos_saved[1], l:eindent . l:str)
				echomsg l:str . " appended after line " . l:pos_saved[1]
" 				let b:cle_return="append after if 3.2.2 str " . l:str . " after line " . l:pos_saved[1] 
				let l:pos_saved[2]+=len(l:str)
				call setpos(".",[0,l:pos_saved[1]+1,len(l:eindent.l:str)+1,0])
			    endif
			endif 
			return 1
		    endif
		else
		    return "this is too hard?"
		endif
		unlet! l:env_names
		unlet! l:env_dict
		unlet! l:cenv_names
		unlet! l:pos 
		unlet! l:pos_saved
" 		if getline('.') =~ '^\s*$'
" 		    exec "normal dd"
		endif
    "}}}3
    "{{{2 close math: texMathZoneV, texMathZoneW, texMathZoneX, texMathZoneY 
    else
	"{{{3 Close math in the current line
	if    math_mode == 'texMathZoneV' && l:line !~ '^\s*\\(\s*$' 	||
	    \ math_mode == 'texMathZoneW' && l:line !~ '^\s*\\\[\s*$' 	||
	    \ math_mode == 'texMathZoneX' && l:line !~ '^\s*\$\s*$' 	||
	    \ math_mode == 'texMathZoneY' && l:line !~ '^\s*\$\{2,2}\s*$'
	    if math_mode == "texMathZoneW"
	 	if l:com == 'a' 
		    if getline(l:begin_line) =~ '^\s*\\\[\s*$'
			call append(line("."),atplib#CopyIndentation(getline(l:begin_line)).'\]')
		    else
			call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\]'. strpart(l:cline,getpos(".")[2]))
		    endif
		else
		    if getline(l:begin_line) =~ '^\s*\\\[\s*$'
			call append(line("."),atplib#CopyIndentation(getline(l:begin_line)).'\]')
		    else
			call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\]'. strpart(l:cline,getpos(".")[2]-1))
" TODO: This could be optional: (but the option rather
" should be an argument of this function rather than
" here!
		    endif
		    let l:pos=getpos(".")
		    let l:pos[2]+=2
		    keepjumps call setpos(("."),l:pos)
		    let b:cle_return="texMathZoneW"
		endif
	    elseif math_mode == "texMathZoneV"
		if l:com == 'a'
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\)'. strpart(l:cline,getpos(".")[2]))
		else
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\)'. strpart(l:cline,getpos(".")[2]-1))
		    call cursor(line("."),col(".")+2)
		    let b:cle_return="V"
		endif
	    elseif math_mode == "texMathZoneX" 
		call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '$'. strpart(l:cline,getpos(".")[2]-1))
		call cursor(line("."),col(".")+1)
		" Close $$:$$.
		if &filetype == "plaintex" || b:atp_TexFlavour == "plaintex"
		endif
" TODO: math_mode == "texMathZoneY"		
	    endif " }}}3
	"{{{3 Close math in a new line, preserv the indentation.
	else 	    
	    let l:eindent=atplib#CopyIndentation(l:line)
	    if math_mode == 'texMathZoneW'
		let l:iline=line(".")
		" if the current line is empty append before it.
		if getline(".") =~ '^\s*$' && l:iline > 1
		    let l:iline-=1
		endif
		call append(l:iline, l:eindent . '\]')
		echomsg "\[ closed in line " . l:iline
		let b:cle_return=2 . " dispalyed math " . l:iline  . " indent " . len(l:eindent)" DEBUG
	    elseif math_mode == 'texMathZoneV'
		let l:iline=line(".")
		" if the current line is empty append before it.
		if getline(".") =~ '^\s*$' && l:iline > 1
		    let l:iline-=1
		endif
		call append(l:iline, l:eindent . '\)')
		echomsg "\( closed in line " . l:iline
		let b:cle_return=2 . " inline math " . l:iline . " indent " .len(l:eindent) " DEBUG
	    elseif math_mode == 'texMathZoneX'
		let l:iline=line(".")
		" if the current line is empty append before it.
		if getline(".") =~ '^\s*$' && l:iline > 1
		    let l:iline-=1
		endif
		let sindent=atplib#CopyIndentation(getline(search('\$', 'bnW')))
		call append(l:iline, sindent . '$')
		echomsg "$ closed in line " . l:iline
" TODO: math_mode == "texMathZoneY"
	    endif
	endif "}}3
    endif
    "}}}2
endfunction
" imap <F7> <Esc>:call atplib#CloseLastEnvironment()<CR>
" }}}1
" {{{1 atplib#CloseLastBracket
"
" Note adding a bracket pair doesn't mean that it will be supported!
" This is to be done! and is quite easy.

" Notes: it first closes the most outer opened bracket:
" 	\left\{\Bigl( 
" 	will first close with \right\} and then \Bigl)
" it still doesn't work 100% well with nested brackets (the part that remains is to
" close in right place) But is doesn't close closed pairs !!! 

" ToDo: \\{ is not closing right, add support for {:}!
" {{{2 			atplib#CloseLastBracket	
" a:1 == 1 just return the bracket 
function! atplib#CloseLastBracket(...)
    
    if a:0 >= 1
	let l:only_return = a:1
    else
	let l:only_return = 0
    endif

    " the value to return 
"     let l:return=""

    " {{{3
    let l:pattern=""
    let l:size_patterns=[]
    for l:size in keys(g:atp_sizes_of_brackets)
	call add(l:size_patterns,escape(l:size,'\'))
    endfor
    let l:pattern_b	= '\C\%('.join(l:size_patterns,'\|').'\)'
    let l:pattern_o	= '\%('.join(map(keys(g:atp_bracket_dict),'escape(v:val,"\\[]")'),'\|').'\)'
"     let b:pat_b=l:pattern_b


    let l:limit_line=max([1,(line(".")-g:atp_completion_limits[1])])
        
    let l:pos_saved=getpos(".")


   " But maybe we shouldn't check if the bracket is closed sometimes one can
   " want to close closed bracket and delete the old one.
   "
   " just add check if the given b:pair_123 are closed or not and take the max
   " over not closed pairs. 
   
   let l:open_col_check_list=[]
"    let b:open_col_check_list=l:open_col_check_list

   "    change the position! and then: 
   "    check the flag 'r' in searchpair!!!
   let i=1
    for l:ket in keys(g:atp_bracket_dict)
" 	let b:ket_{i}=l:ket " DEBUG
	let l:pos=deepcopy(l:pos_saved)
	let l:pair_{i}=searchpairpos(escape(l:ket,'\[]'),'',escape(g:atp_bracket_dict[l:ket],'\[]'),'bnW',"",l:limit_line)
	let l:pos[1]=l:pair_{i}[0]
	let l:pos[2]=l:pair_{i}[1]
	keepjumps call setpos(".",l:pos)
	let l:check_{i}= atplib#CheckClosed(escape(l:ket,'\'),escape(g:atp_bracket_dict[l:ket],'\'),line("."),g:atp_completion_limits[0],1) == '0'
	call add(l:open_col_check_list,(l:check_{i}*l:pair_{i}[1]))
	keepjumps call setpos(".",l:pos_saved)
	let i+=1
    endfor
    keepjumps call setpos(".",l:pos_saved)
   
"    " DEBUG:
"    let j=1
"    while j<i
"        let b:check_{j}=l:check_{j}
"        let b:pair_{j}=l:pair_{j}
"        let j+=1
"    endwhile

    let l:open_col=max(l:open_col_check_list)
    let j=1
    while j<i
	if l:open_col == l:pair_{j}[1] && l:check_{j} != 0
	    let l:open_line=l:pair_{j}[0]
	endif
	let j+=1
    endwhile


   " Debug:
"        let b:open_line=l:open_line
"        let b:open_col=l:open_col 

    "}}}3
    " {{{3 main if
   if l:open_col 
	let l:line=getline(l:open_line)

	let l:bline=strpart(l:line,0,(l:open_col-1))
	let l:eline=strpart(l:line,l:open_col-1,2)
	let b:bline=l:bline


	let l:opening_size=matchstr(l:bline,'\zs'.l:pattern_b.'\ze\s*$')
	let l:closing_size=get(g:atp_sizes_of_brackets,l:opening_size,"")
	let l:opening_bracket=matchstr(l:eline,'^'.l:pattern_o)

	if l:opening_size =~ '\\' && l:opening_bracket != '(' && l:opening_bracket != '['
	    let l:bbline=strpart(l:bline,0,len(l:bline)-1)
	    let l:opening_size2=matchstr(l:bbline,'\zs'.l:pattern_b.'\ze\s*$')
	    let l:closing_size2=get(g:atp_sizes_of_brackets,l:opening_size2,"")
	    let l:closing_size=l:closing_size2.l:closing_size

	    " DEBUG
	    let b:bbline=l:bbline
	    let b:o_size2=l:opening_size2
	    let b:c_size2=l:closing_size2
	endif

	" DEBUG:
	let b:o_bra=l:opening_bracket
	let b:o_size=l:opening_size
	let b:bline=l:bline
	let b:line=l:line
	let b:eline=l:eline
	let b:opening_size=l:opening_size
	let b:closing_size=l:closing_size

" 	if l:line =~ escape(l:opening_size.l:opening_bracket,'\') . '\s*$' && col(".") == len(getline(line(".")))
" 	    let l:indent=atplib#CopyIndentation(getline(l:open_line))
" 	    call append(line("."),l:indent.l:closing_size.get(g:atp_bracket_dict,l:opening_bracket))
" 	    let b:clb_return='append end line'
" 	    return 'append end line'
" 	else
	    let l:cline=getline(line("."))
	    if mode() == 'i'
		if !l:only_return
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1).
			    \ l:closing_size.get(g:atp_bracket_dict,l:opening_bracket). 
			    \ strpart(l:cline,getpos(".")[2]-1))
		endif
		let l:return=l:closing_size.get(g:atp_bracket_dict,l:opening_bracket)
	    elseif mode() == 'n'
		if !l:only_return
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]).
			    \ l:closing_size.get(g:atp_bracket_dict,l:opening_bracket). 
			    \ strpart(l:cline,getpos(".")[2]))
		endif
		let l:return=l:closing_size.get(g:atp_bracket_dict,l:opening_bracket)
	    endif
	    let l:pos=getpos(".")
	    let l:pos[2]+=len(l:closing_size.get(g:atp_bracket_dict,l:opening_bracket))
	    keepjumps call setpos(".",l:pos)
" 	    let b:clb_return='close inline'
	    return l:return
" 	endif
   endif
   " }}}3
endfunction
" }}}2
" }}}1

" Tab Completion
" atplib#TabCompletion {{{1
" This is the main TAB COMPLITION function.
"
" expert_mode = 1 (on)  gives less completions in some cases (commands,...)
" 			the matching pattern has to match at the beginning and
" 			is case sensitive. Furthermode  in expert mode, if
" 			completing a command and found less than 1 match then
" 			the function tries to close \(:\) or \[:\] (but not an
" 			environment, before doing ToDo in line 3832 there is
" 			no sense to make it).
" 			<Tab> or <F7> (if g:atp_no_tab_map=1)
" expert_mode = 0 (off) gives more matches but in some cases better ones, the
" 			string has to match somewhare and is case in
" 			sensitive, for example:
" 			\arrow<Tab> will show all the arrows definded in tex,
" 			in expert mode there would be no match (as there is no
" 			command in tex which begins with \arrow).
" 			<S-Tab> or <S-F7> (if g:atp_no_tab_map=1)
"
" ToDo: \ref{<Tab> do not closes the '}', its by purpose, as sometimes one
" wants to add more than one reference. But this is not allowed by this
" command! :) I can add it.
" Completion Modes:
" 	documentclass (\documentclass)
" 	labels   (\ref,\eqref)
" 	packages (\usepackage)
" 	commands
" 	environments (\begin,\(:\),\[:\])
" 	brackets ((:),[:],{:}) preserves the size operators!
" 	bibitems (\cite\|\citep\|citet)
" 	bibfiles (\bibliography)
" 	bibstyle (\bibliographystyle)
" 	end	 (close \begin{env} with \end{env})
" 	font encoding
" 	font family
" 	font series
" 	font shape

"ToDo: the completion should be only done if the completed text is different
"from what it is. But it might be as it is, there are reasons to keep this.
"
"
"TODO: g:atp_check_if_opened problem: \(\right<Tab>
"TODO: cite when thepattern was not found it just closes the bracket, it
"should do nothing or warn that the pattern was not found.
"ToTHINK: g:atp_Commands add g:atp_math_commands, Do add g:atp_Commands in math mode /when g:atp_check_if_open=1/ ?) 
" Done: tikz commands and keywords are not working.
" Todo: \usetikzlibrary{ completion is not completing right.
" Todo: bracket closing \(( ... .
" Todo: sort labels in the appearing order in the tex file.


" IDEA: to make it work, 
" make a function which is run in insert mode, then goes to normal mode, 
" (using feedkeys() function) then goes to insert mode (not changing the
" cursor position setpos(), might be necessary for that) and then calls
" a function through a map (using <Plug> might be helpful). 
"
" function! atplib#Tab_Commp()
" 
" endfunction
" function! atplib#ATP_Complete(synstack,offset,completion_list)
"     let g:synstack_complete=copy(a:synstack)
"     call complete(a:offset,a:completion_lilst)
" endfunction
" imap <Plug>ATP_Complete	<C-R>=atplib#ATP_Complete(s:synstack,s:completion_list)

function! atplib#TabCompletion(expert_mode,...)
    " {{{2 Match the completed word 
    let l:normal_mode=0

    if a:0 >= 1
	let l:normal_mode=a:1
    endif

    " this specifies the default argument for atplib#CloseLastEnvironment()
    " in some cases it is better to append after than before.
    let l:append='i'

    let l:pos=getpos(".")
    let l:pos_saved=deepcopy(l:pos)
    let l:line=join(getbufline("%",l:pos[1]))
    let l:nchar=strpart(l:line,l:pos[2]-1,1)
"     let b:nchar=l:nchar " debug
    let l:l=strpart(l:line,0,l:pos[2]-1)
"     let b:l=l:l	" debug
    let l:n=strridx(l:l,'{')
    let l:m=strridx(l:l,',')
    let l:o=strridx(l:l,'\')
    let l:s=strridx(l:l,' ')
    let l:p=strridx(l:l,'[')
     
    let l:nr=max([l:n,l:m,l:o,l:s,l:p])

"     DEBUG:
"     let b:n=l:n
"     let b:o=l:o
"     let b:s=l:s
"     let b:p=l:p
"     let b:nr=l:nr

    " this matches for \...
    let l:begin=strpart(l:l,l:nr+1)
    let l:cbegin=strpart(l:l,l:nr)
    " and this for '\<\w*$' (beginning of last started word) -- used in
    " tikzpicture completion method 
    let l:tbegin=matchstr(l:l,'\zs\<\w*$')
    let l:obegin=strpart(l:l,l:o)


"     let b:line=l:line     " DEBUG
"     let b:tbegin=l:tbegin " DEBUG
"     let b:cbegin=l:cbegin " DEBUG
"     let b:obegin=l:obegin " DEBUG
"     let b:begin= l:begin  " DEBUG

    " what we are trying to complete: usepackage, environment.
    let l:pline=strpart(l:l,0,l:nr)
"     let b:pline=l:pline	"DEBUG

    let l:limit_line=max([1,(l:pos[1]-g:atp_completion_limits[1])])
" }}}2
" {{{2 SET COMPLETION METHOD
    " {{{3 command
    if l:o > l:n && l:o > l:s && 
	\ l:pline !~ '\%(input\|include\%(only\)\?\|[^\\]\\\\[^\\]$\)' &&
	\ l:pline !~ '\\\@<!\\$' &&
	\ l:begin !~ '{\|}\|,\|-\|\^\|\$\|(\|)\|&\|-\|+\|=\|#\|:\|;\|\.\|,\||\|?$' &&
	\ l:begin !~ '^\[\|\]\|-\|{\|}\|(\|)' &&
	\ l:cbegin =~ '^\\' && !l:normal_mode &&
	\ l:l !~ '\\\%(no\)\?cite[^}]*$'

	" in this case we are completing a command
	" the last match are the things which for sure do not ends any
	" command. The pattern '[^\\]\\\\[^\\]$' do not matches "\" and "\\\",
	" in which case the line contains "\\" and "\\\\" ( = line ends!)
	" (here "\" is one character \ not like in magic patterns '\\')
	" but matches "" and "\\" (i.e. when completing "\" or "\\\" [end line
	" + command].
	if index(g:atp_completion_active_modes, 'commands') != -1
	    let l:completion_method='command'
	    "DEBUG:
	    let b:comp_method='command'
	else
" 	    let b:comp_method='command fast return'
	    return ''
	endif
    "}}}3
    "{{{3 environment names
    elseif (l:pline =~ '\%(\\begin\|\\end\)\s*$' && l:begin !~ '}.*$' && !l:normal_mode)
	if index(g:atp_completion_active_modes, 'environment names') != -1 
	    let l:completion_method='environment_names'
	    "DEBUG:
	    let b:comp_method='environment_names'
	else
" 	    let b:comp_method='environment_names fast return'
	    return ''
	endif
    "}}}3
    "{{{3 close environments
    elseif !l:normal_mode && 
		\ ((l:pline =~ '\\begin\s*$' && l:begin =~ '}\s*$') || ( l:pline =~ '\\begin\s*{[^}]*}\s*\\label' ) ) || 
		\ l:normal_mode && l:pline =~ '\\begin\s*\({[^}]*}\?\)\?\s*$'
	if (!l:normal_mode && index(g:atp_completion_active_modes, 'close environments') != -1 ) ||
		    \ (l:normal_mode && index(g:atp_completion_active_modes_normal_mode, 'close environments') != -1 )
	    let l:completion_method='close environments'
	    "DEBUG:
	    let b:comp_method='colse environments'
	else
	    let b:comp_method='colse environments fast return'
	    return ''
	endif
    "}}}3
    "{{{3 colors
    elseif l:l =~ '\\textcolor{[^}]*$'
	let l:completion_method='colors'
	"DEBUG:
	let b:comp_method='colors'
    "}}}3
    "{{{3 label
    elseif l:pline =~ '\\\%(eq\)\?ref\s*$' && !l:normal_mode
	if index(g:atp_completion_active_modes, 'labels') != -1 
	    let l:completion_method='labels'
	    "DEBUG:
	    let b:comp_method='label'
	else
	    let b:comp_method='label fast return'
	    return ''
	endif
    "}}}3
    "{{{3 bibitems
    elseif l:pline =~ '\\\%(no\)\?cite' && !l:normal_mode
	if index(g:atp_completion_active_modes, 'bibitems') != -1
	    let l:completion_method='bibitems'
	    "DEBUG:
	    let b:comp_method='bibitems'
	    if l:begin =~ '}\s*$'
		return ''
	    endif 
	else
	    let b:comp_method='bibitems fast return'
	    return ''
	endif
    "}}}3
    "{{{3 tikzpicture
    elseif search('\%(\\def\>.*\|\\\%(re\)\?newcommand\>.*\|%.*\)\@<!\\begin{tikzpicture}','bnW') > search('[^%]*\\end{tikzpicture}','bnW') ||
	\ !atplib#CompareCoordinates(searchpos('[^%]*\zs\\tikz{','bnw'),searchpos('}','bnw'))
	"{{{4 tikzpicture keywords
	if l:l =~ '\%(\s\|\[\|{\|}\|,\|\.\|=\|:\)' . l:tbegin . '$' && !l:normal_mode
	    if index(g:atp_completion_active_modes, 'tikzpicture keywords') != -1 
		"DEBUG:
		let b:comp_method='tikzpicture keywords'
		let l:completion_method="tikzpicture keywords"
	    else
		let b:comp_method='tikzpicture keywords fast return'
		return ''
	    endif
	"}}}4
	"{{{4 tikzpicture commands
	elseif  l:l =~ '\\' . l:tbegin  . '$' && !l:normal_mode
	    if index(g:atp_completion_active_modes, 'tikzpicture commands') != -1
		"DEBUG:
		let b:comp_method='tikzpicture commands'
		let l:completion_method="tikzpicture commands"
	    else
		let b:comp_method='tikzpicture commands fast return'
		return ''
	    endif
	"}}}4
	"{{{4 close_env tikzpicture
	else
	    if (!normal_mode &&  index(g:atp_completion_active_modes, 'close environments') != -1 ) ||
			\ (l:normal_mode && index(g:atp_completion_active_modes_normal_mode, 'close environments') != -1 )
		"DEBUG:
		let b:comp_method='close_env tikzpicture'
		let l:completion_method="close_env"
	    else
		let b:comp_method='close_env tikzpicture fast return'
		return ''
	    endif
	endif
    "}}}3
    "{{{3 package
    elseif l:pline =~ '\\usepackage\%([.*]\)\?\s*' && !l:normal_mode
	if index(g:atp_completion_active_modes, 'package names') != -1
	    let l:completion_method='package'
	    "DEBUG:
	    let b:comp_method='package'
	else
	    let b:comp_method='package fast return'
	    return ''
	endif
    "}}}3
    "{{{3 tikz libraries
    elseif l:pline =~ '\\usetikzlibrary\%([.*]\)\?\s*' && !l:normal_mode
	if index(g:atp_completion_active_modes, 'tikz libraries') != -1
	    let l:completion_method='tikz libraries'
	    "DEBUG:
	    let b:comp_method='tikz libraries'
	else
	    let b:comp_method='tikz libraries fast return'
	    return ''
	endif
    "}}}3
    "{{{3 inputfiles
    elseif ((l:pline =~ '\\input' || l:begin =~ 'input') ||
	  \ (l:pline =~ '\\include' || l:begin =~ 'include') ||
	  \ (l:pline =~ '\\includeonly' || l:begin =~ 'includeonly') ) && !l:normal_mode 
	if l:begin =~ 'input'
	    let l:begin=substitute(l:begin,'.*\%(input\|include\%(only\)\?\)\s\?','','')
	endif
	if index(g:atp_completion_active_modes, 'input files') != -1
	    let l:completion_method='inputfiles'
	    " DEBUG:
	    let b:comp_method='inputfiles'
	else
	    let b:comp_method='inputfiles fast return'
	    return ''
	endif
    "}}}3
    "{{{3 bibfiles
    elseif l:pline =~ '\\bibliography\%(style\)\@!' && !l:normal_mode
	if index(g:atp_completion_active_modes, 'bibitems') != -1
	    let l:completion_method='bibfiles'
	    " DEBUG:
	    let b:comp_method='bibfiles'
	else
	    let b:comp_method='bibfiles fast return'
	    return ''
	endif
    "}}}3
    "{{{3 bibstyles
    elseif l:pline =~ '\\bibliographystyle' && !l:normal_mode 
	if (index(g:atp_completion_active_modes, 'bibstyles') != -1 ) 
	    let l:completion_method='bibstyles'
	    let b:comp_method='bibstyles'
	else
	    let b:comp_method='bibstyles fast return'
	    return ''
	endif
    "}}}3
    "{{{3 documentclass
    elseif l:pline =~ '\\documentclass\>' && !l:normal_mode 
	if index(g:atp_completion_active_modes, 'documentclass') != -1
	    let l:completion_method='documentclass'
	    let b:comp_method='documentclass'
	else
	    let b:comp_method='documentclass fast return'
	    return ''
	endif
    "}}}3
    "{{{3 font family
    elseif l:l =~ '\%(\\usefont{[^}]*}{\|\\DeclareFixedFont{[^}]*}{[^}]*}{\|\\fontfamily{\)[^}]*$' && !l:normal_mode 
	if index(g:atp_completion_active_modes, 'font family') != -1
	    let l:completion_method='font family'
	    let b:comp_method='font family'
	else
	    let b:comp_method='font family fast return'
	    return ''
	endif
    "{{{3 font series
    elseif l:l =~ '\%(\\usefont{[^}]*}{[^}]*}{\|\\DeclareFixedFont{[^}]*}{[^}]*}{[^}]*}{\|\\fontseries{\)[^}]*$' && !l:normal_mode 
	if index(g:atp_completion_active_modes, 'font series') != -1
	    let l:completion_method='font series'
	    let b:comp_method='font series'
	else
	    let b:comp_method='font series fast return'
	    return ''
	endif
    "{{{3 font shape
    elseif l:l =~ '\%(\\usefont{[^}]*}{[^}]*}{[^}]*}{\|\\DeclareFixedFont{[^}]*}{[^}]*}{[^}]*}{[^}]*}{\|\\fontshape{\)[^}]*$' && !l:normal_mode 
	if index(g:atp_completion_active_modes, 'font shape') != -1
	    let l:completion_method='font shape'
	    let b:comp_method='font shape'
	else
	    let b:comp_method='font shape fast return'
	    return ''
	endif
    "}}}3
    "{{{3 font encoding
    elseif l:l =~ '\%(\\usefont{\|\\DeclareFixedFont{[^}]*}{\|\\fontencoding{\)[^}]*$' && !l:normal_mode 
	if index(g:atp_completion_active_modes, 'font encoding') != -1
	    let l:completion_method='font encoding'
	    let b:comp_method='font encoding'
	else
	    let b:comp_method='font encoding fast return'
	    return ''
	endif
    "}}}3
    "{{{3 brackets
" TODO: make this dependent on g:atp_bracket_dict
    elseif index(g:atp_completion_active_modes, 'brackets') != -1 && 
		\ (searchpairpos('\%(\\\@<!\\\)\@<!(','', '\%(\\\@<!\\\)\@<!)', 'bnW', "", l:limit_line) != [0, 0] ||  
		\ searchpairpos('\%(\\begin\s*\|\\end\s*\)\@<!{',	  '', '}',	 'bnW', "", l:limit_line) != [0, 0] || 
		\ searchpairpos('\%(\\\@<!\\\)\@<!\[', '', '\%(\\\@<!\\\)\@<!\]', 'bnW', "", l:limit_line) != [0, 0] )
	if (!normal_mode &&  index(g:atp_completion_active_modes, 'brackets') != -1 ) ||
		    \ (l:normal_mode && index(g:atp_completion_active_modes_normal_mode, 'brackets') != -1 )
	    let b:comp_method='brackets'
	    call atplib#CloseLastBracket()
	    return '' 
	else
	    let b:comp_method='brackets fast return'
	    return ''
	endif
    "}}}3
    "{{{3 close environments
    else
	if (!normal_mode &&  index(g:atp_completion_active_modes, 'close environments') != '-1' ) ||
		    \ (l:normal_mode && index(g:atp_completion_active_modes_normal_mode, 'close environments') != '-1' )
	    let l:completion_method='close_env'
	    "DEBUG:
	    let b:comp_method='close_env a' 
	else
	    let b:comp_method='close_env a fast return' 
	    return ''
	endif
    endif
" if the \[ is not closed, first close it and then complete the commands, it
" is better as then automatic tex will have better file to operate on.
    " }}}3
" }}}2
" {{{2 close environments
    if l:completion_method=='close_env'

	" Close inline math
	if atplib#CheckSyntaxGroups(['texMathZoneV', 'texMathZoneW', 'texMathZoneX', 'texMathZoneY'])
	    call atplib#CloseLastEnvironment(l:append, 'math')
	else
" 	    let l:env_opened	= searchpair('\%(\\\@<!%.*\)\@<!\\begin','','\%(\\\@<!%.*\)\@<!\\end','bnW','searchpair("\\%(\\\\\\@<!%.*\\)\\@<!\\\\begin{".matchstr(getline("."),"\\\\begin{\\zs[^}]*\\ze}"),"","\\%(\\\\\\@<!%.*\\)\\@<!\\\\end{".matchstr(getline("."),"\\\\begin{\\zs[^}]*\\ze}"),"nW")',max([1,(line(".")-g:atp_completion_limits[2])]))

	    let stopline_forward	= line(".") + g:atp_completion_limits[2]
	    let stopline_backward	= line(".") - g:atp_completion_limits[2]

	    let line_nr=line(".")
	    while line_nr >= stopline_backward
		let line_nr 		= searchpair('\\begin\s*{', '', '\\end\s*{', 'bnW', '', stopline_backward)
		if line_nr >= stopline_backward
		    let env_name	= matchstr(getline(line_nr), '\\begin\s*{\zs[^}]*}\ze}')
		    let line_forward 	= searchpair('\\begin\s*{'.env_name.'}', '', '\\end\s*{'.env_name.'}', 
							\ 'nW', '', stopline_forward)
		    if line_forward == 0
			break
		    endif
			
		else
		    let line_nr = -1
		endif
	    endwhile

	    let l:env_opened 	= line_nr
	    if l:env_opened
	    " the l:env_name variable might have wrong value as it is
	    " looking using '\\begin' and '\\end' this might be not enough, 
		" however the function atplib#CloseLastEnv works perfectly and this
		" should be save:
	    call atplib#CloseLastEnvironment(l:append, 'environment', '', [l:env_opened, 0])
	    let b:tc_return="close_env opened:"
		return ''
	    endif
	endif
	return ''
    endif
" }}}2
" {{{2 SET COMPLETION LIST
    " generate the completion names
    " {{{3 ------------ ENVIRONMENT NAMES
    if l:completion_method == 'environment_names'
	let l:end=strpart(l:line,l:pos[2]-1)
	if l:end !~ '\s*}'
	    let l:completion_list=deepcopy(g:atp_Environments)
	    if g:atp_local_completion
		" Make a list of local envs and commands
		if !exists("s:atp_LocalEnvironments") 
		    let s:atp_LocalEnvironments=LocalCommands()[1]
		    endif
		let l:completion_list=atplib#Extend(l:completion_list,s:atp_LocalEnvironments)
	    endif
	    let l:completion_list=atplib#Add(l:completion_list,'}')
	else
	    let l:completion_list=deepcopy(g:atp_Environments)
	    if g:atp_local_completion
		" Make a list of local envs and commands
		if !exists("s:atp_LocalEnvironments") 
		    let s:atp_LocalEnvironments=LocalCommands()[1]
		    endif
		call atplib#Extend(l:completion_list,s:atp_LocalEnvironments)
	    endif
	endif
	" TIKZ
	keepjumps call setpos(".",[0,1,1,0])
	let l:stop_line=search('\\begin\s*{document}','cnW')
	keepjumps call setpos(".",l:pos_saved)
	if atplib#SearchPackage('tikz',l:stop_line) && 
		    \ ( !g:atp_check_if_opened || 
		    \ atplib#CheckOpened('\\begin\s*{tikzpicture}','\\end\s*{tikzpicture}',line('.'),80) || 
		    \ atplib#CheckOpened('\\tikz{','}',line("."),g:atp_completion_limits[2]) )
	    if l:end !~ '\s*}'
		call extend(l:completion_list,atplib#Add(g:atp_tikz_environments,'}'))
	    else
		call extend(l:completion_list,g:atp_tikz_environments)
	    endif
	endif
	" AMSMATH
	if atplib#SearchPackage('amsmath',l:stop_line) || g:atp_amsmath == 1 || atplib#DocumentClass() =~ '^ams'
	    if l:end !~ '\s*}'
		call extend(l:completion_list,atplib#Add(g:atp_amsmath_environments,'}'),0)
	    else
		call extend(l:completion_list,g:atp_amsmath_environments,0)
	    endif
	endif
    " }}}3
    "{{{3 ------------ PACKAGES
    elseif l:completion_method == 'package'
	let l:completion_list=atplib#FindFiles("tex","sty")
    "}}}3
    "{{{3 ------------ COLORS
    elseif l:completion_method == 'colors'
	" To Do: make a predefined lists of colors depending on package
	" options! 
	" Make a list of local envs and commands
	if !exists("s:atp_LocalColors") 
	    let s:atp_LocalColors=LocalCommands()[2]
	    endif
	let l:completion_list=s:atp_LocalColors
    "}}}3
    " {{{3 ------------ TIKZ LIBRARIES
    elseif l:completion_method == 'tikz libraries'
	let l:completion_list=deepcopy(g:atp_tikz_libraries)
    " }}}3
    " {{{3 ------------ TIKZPICTURE KEYWORDS
    elseif l:completion_method == 'tikzpicture keywords'

	keepjumps call setpos(".",[0,1,1,0])
	let l:stop_line=search('\\begin\s*{document}','cnW')
	keepjumps call setpos(".",l:pos_saved)

	let l:completion_list=deepcopy(g:atp_tikz_keywords)
	" ToDo: it can be faster to find once which libraries are defined and then
	" check them.
	if atplib#SearchPackage('.*arrows',l:stop_line,'usetikzlibrary')
	    call extend(l:completion_list,g:atp_tikz_library_arrows_keywords)
	endif   
	if atplib#SearchPackage('.*automata',l:stop_line,'usetikzlibrary')
	    call extend(l:completion_list,g:atp_tikz_library_automata_keywords)
	endif   
	if atplib#SearchPackage('.*background',l:stop_line,'usetikzlibrary')
	    call extend(l:completion_list,g:atp_tikz_library_backgrounds_keywords)
	endif   
	if atplib#SearchPackage('.*calendar',l:stop_line,'usetikzlibrary')
	    call extend(l:completion_list,g:atp_tikz_library_calendar_keywords)
	endif
	if atplib#SearchPackage('.*chain',l:stop_line,'usetikzlibrary')
	    call extend(l:completion_list,g:atp_tikz_library_chain_keywords)
	endif
	if atplib#SearchPackage('.*decorations',l:stop_line,'usetikzlibrary')
	    call extend(l:completion_list,g:atp_tikz_library_decoration_keywords)
	endif
	if atplib#SearchPackage('.*matrix',l:stop_line,'usetikzlibrary')
	    call extend(l:completion_list,g:atp_tikz_library_matrix_keywords)
	endif   
    " }}}3
    " {{{3 ------------ COMMANDS
    elseif l:completion_method == 'command'
	let l:tbegin=strpart(l:l,l:o+1)
	let l:completion_list=[]
	
	" Find end of the preambule.
	keepjumps call setpos(".",[0,1,1,0])
	let l:stop_line=search('\\begin\s*{document}','cnW')
	keepjumps call setpos(".",l:pos_saved)
	 
	" Are we in the math mode?
	let l:math_is_opened	= atplib#CheckSyntaxGroups(g:atp_MathZones)

   	"{{{4 picture
	if searchpair('\\begin\s*{picture}','','\\end\s*{picture}','bnW',"",max([1,(line(".")-g:atp_completion_limits[2])]))
	    call extend(l:completion_list,g:atp_picture_commands)
	endif "}}}4
	" TIKZ commands {{{4
	" if tikz is declared and we are in tikz environment.
	let l:in_tikz=searchpair('\\begin\s*{tikzpicture}','','\\end\s*{tikzpicture}','bnW',"",max([1,(line(".")-g:atp_completion_limits[2])])) || atplib#CheckOpened('\\tikz{','}',line("."),g:atp_completion_limits[0])
	if l:in_tikz
	    call extend(l:completion_list,g:atp_tikz_commands)
	    if atplib#SearchPackage('.*calendar',l:stop_line,'usetikzlibrary')
		call extend(l:completion_list,g:atp_tikz_library_calendar)
	    endif
	    if atplib#SearchPackage('.*chain','usetikzlibrary')
		call extend(l:completion_list,g:atp_tikz_library_chain)
	    endif
	    " if inside a bracket pair {:} add normal commands. 
	    if searchpair('\\\@<!{','','\\\@<!}','bnW',"",max([1,(line(".")-g:atp_completion_limits[0])]))
		call extend(l:completion_list,g:atp_Commands)
	    endif
	endif "}}}4
	" {{{4 MATH commands 
	" if we are in math mode or if we do not check for it.
	if g:atp_no_math_command_completion != 1 &&  ( !g:atp_math_opened || l:math_is_opened )
	    call extend(l:completion_list,g:atp_math_commands)
	    " amsmath && amssymb {{{5
	    if (g:atp_amsmath == 1 || atplib#DocumentClass() =~ '^ams') && !l:in_tikz
		call extend(l:completion_list,g:atp_amsmath_commands,0)
		call extend(l:completion_list,g:atp_ams_negations)
		if a:expert_mode == 0 
		    call extend(l:completion_list,g:atp_ams_negations_non_expert_mode)
		endif
	    elseif !l:in_tikz
		if atplib#SearchPackage('amsmath',l:stop_line)
		    call extend(l:completion_list,g:atp_amsmath_commands,0)
		endif
		if atplib#SearchPackage('amssymb',l:stop_line)
		    call extend(l:completion_list,g:atp_ams_negations)
		    if a:expert_mode == 0 
			call extend(l:completion_list,g:atp_ams_negations_non_expert_mode)
		    endif
		endif
	    endif " }}}5
	    " nicefrac {{{5
	    if atplib#SearchPackage('nicefrac',l:stop_line)
		call add(l:completion_list,"\\nicefrac{")
	    endif "}}}5
	    " math non expert mode {{{5
	    if a:expert_mode == 0
		call extend(l:completion_list,g:atp_math_commands_non_expert_mode)
	    endif "}}}5
	endif
	" }}}4
	" LOCAL commands {{{4
	if g:atp_local_completion
	    " Make a list of local envs and commands
	    if !exists("s:atp_LocalCommands") 
		if exists("b:atp_LocalCommands")
		    let s:atp_LocalCommands=b:atp_LocalCommands
		elseif exists("g:atp_local_commands")
		    let s:atp_LocalCommands=g:atp_local_commands
		else
		    let s:atp_LocalCommands=LocalCommands()[1]
		endif
	    endif
	    call extend(l:completion_list,s:atp_LocalCommands)
	endif
	"}}}4
	" {{{4 COMMANDS
"	if we are not in math mode or if we do not care about it or we are in non expert mode.
	if (!g:atp_math_opened || !l:math_is_opened ) && !l:in_tikz || a:expert_mode == 0
	    call extend(l:completion_list,g:atp_Commands)
	    " FANCYHDR
	    if atplib#SearchPackage('fancyhdr',l:stop_line) && !l:in_tikz
		call extend(l:completion_list,g:atp_fancyhdr_commands)
	    endif
	endif
	"}}}4
	" ToDo: add layout commands and many more packages. (COMMANDS FOR
	" PREAMBULE)
	"{{{4
	let l:env_name=substitute(l:pline,'.*\%(\\\%(begin\|end.*\){\(.\{-}\)}.*\|\\\%(\(item\)\s*\)\%(\[.*\]\)\?\s*$\)','\1\2','') 
	if l:env_name =~ '\\\%(\%(sub\)\?paragraph\|\%(sub\)*section\|chapter\|part\)'
	    let l:env_name=substitute(l:env_name,'.*\\\(\%(sub\)\?paragraph\|\%(sub\)*section\|chapter\|part\).*','\1','')
	endif
	let l:env_name=substitute(l:env_name,'\*$','','')
	" if the pattern did not work do not put the env name.
	" for example \item cos\lab<Tab> the pattern will not work and we do
	" not want env name. 
	if l:env_name == l:pline
	    let l:env_name=''
	endif
" 	let b:env_name=l:env_name " DEBUG

	if has_key(g:atp_shortname_dict,l:env_name)
	    if g:atp_shortname_dict[l:env_name] != 'no_short_name' && g:atp_shortname_dict[l:env_name] != '' 
		let l:short_env_name=g:atp_shortname_dict[l:env_name]
		let l:no_separator=0
	    else
		let l:short_env_name=''
		let l:no_separator=1
	    endif
	else
	    let l:short_env_name=''
	    let l:no_separator=1
	endif

" 	if index(g:atp_no_separator_list,l:env_name) != -1
" 	    let l:no_separator = 1
" 	endif

	if g:atp_env_short_names == 1
	    if l:no_separator == 0 && g:atp_no_separator == 0
		let l:short_env_name=l:short_env_name . g:atp_separator
	    endif
	else
	    let l:short_env_name=''
	endif

	call extend(l:completion_list, [ '\label{' . l:short_env_name ],0)
	"}}}4

    " }}}3
    " {{{3 ------------ LABELS
    elseif l:completion_method == 'labels'
	let l:completion_list=[]
	let l:completion_list=deepcopy(values(atplib#generatelabels(fnamemodify(bufname("%"),":p"))[fnamemodify(bufname("%"),":p")]))
	if l:nchar != '}'
	    call map(l:completion_list,'v:val."}"')
	endif
    " }}}3
    " {{{3 ------------ TEX INPUTFILES
    elseif l:completion_method ==  'inputfiles'
	let l:inputfiles=atplib#FindInputFilesInDir(g:texmf,1,".tex")
	let l:completion_list=[]
	for l:key in l:inputfiles
	    call add(l:completion_list,fnamemodify(l:key,":t:r"))
	endfor
	call sort(l:completion_list)
    " }}}3
    " {{{3 ------------ BIBFILES
    elseif l:completion_method ==  'bibfiles'
	let l:bibfiles=[]
	for l:dir in g:atp_bibinputs
	    let l:bibfiles=extend(l:bibfiles,atplib#FindInputFilesInDir(l:dir,0,".bib"))
	endfor
	let l:completion_list=[]
	for l:key in l:bibfiles
	    call add(l:completion_list,fnamemodify(l:key,":t:r"))
	endfor
	call sort(l:completion_list)
    " }}}3
    " {{{3 ------------ BIBSTYLES
    elseif l:completion_method == 'bibstyles'
	let l:completion_list=atplib#FindFiles("bst","bst")
    "}}}3
    "{{{3 ------------ DOCUMENTCLASS
    elseif l:completion_method == 'documentclass'
	let l:completion_list=atplib#FindFiles("tex","cls")
	if l:nchar != "}"
	    call map(l:completion_list,'v:val."}"')
	endif
    " }}}3
    "{{{3 ------------ FONT FAMILY
    elseif l:completion_method == 'font family'
	let l:bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let l:epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if l:epos == -1
	    let l:epos=len(l:line)
	endif
	let l:fline=strpart(l:line,l:bpos,l:epos-l:bpos)
	let l:encoding=matchstr(l:fline,'\\\%(usefont\|DeclareFixedFont\s*{[^}]*}\|fontencoding\)\s*{\zs[^}]*\ze}')
	if l:encoding == ""
	    let l:encoding=g:atp_font_encoding
	endif
" 	let b:encoding=l:encoding
	let l:completion_list=[]
	let l:fd_list=atplib#FdSearch('^'.l:encoding.l:begin)
" 	let b:fd_list=l:fd_list
	for l:file in l:fd_list
	    call extend(l:completion_list,map(atplib#ShowFonts(l:file),'matchstr(v:val,"usefont\\s*{[^}]*}\\s*{\\zs[^}]*\\ze}")'))
	endfor
	call filter(l:completion_list,'count(l:completion_list,v:val) == 1 ')
    " }}}3
    "{{{3 ------------ FONT SERIES
    elseif l:completion_method == 'font series'
	let l:bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let l:epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if l:epos == -1
	    let l:epos=len(l:line)
	endif
	let l:fline=strpart(l:line,l:bpos,l:epos-l:bpos)
" 	let b:fline=l:fline
	let l:encoding=matchstr(l:fline,'\\\%(usefont\|DeclareFixedFont\s*{[^}]*}\|fontencoding\)\s*{\zs[^}]*\ze}')
	if l:encoding == ""
	    let l:encoding=g:atp_font_encoding
	endif
	let l:font_family=matchstr(l:fline,'\\\%(usefont\s*{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\|fontfamily\)\s*{\zs[^}]*\ze}')
" 	let b:font_family=l:font_family
	let l:completion_list=[]
	let l:fd_list=atplib#FdSearch('^'.l:encoding.l:font_family)
	for l:file in l:fd_list
	    call extend(l:completion_list,map(atplib#ShowFonts(l:file),'matchstr(v:val,"usefont{[^}]*}{[^}]*}{\\zs[^}]*\\ze}")'))
	endfor
	call filter(l:completion_list,'count(l:completion_list,v:val) == 1 ')
    " }}}3
    "{{{3 ------------ FONT SHAPE
    elseif l:completion_method == 'font shape'
	let l:bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let l:epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if l:epos == -1
	    let l:epos=len(l:line)
	endif
	let l:fline=strpart(l:line,l:bpos,l:epos-l:bpos)
	let l:encoding=matchstr(l:fline,'\\\%(usefont\|DeclareFixedFont\s*{[^}]*}\|fontencoding\)\s*{\zs[^}]*\ze}')
	if l:encoding == ""
	    let l:encoding=g:atp_font_encoding
	endif
	let l:font_family=matchstr(l:fline,'\\\%(usefont{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\|fontfamily\)\s*{\zs[^}]*\ze}')
	let l:font_series=matchstr(l:fline,'\\\%(usefont\s*{[^}]*}\s*{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\s*{[^}]*}\|fontseries\)\s*{\zs[^}]*\ze}')
	let l:completion_list=[]
	let l:fd_list=atplib#FdSearch('^'.l:encoding.l:font_family)

	for l:file in l:fd_list
	    call extend(l:completion_list,map(atplib#ShowFonts(l:file),'matchstr(v:val,"usefont{[^}]*}{'.l:font_family.'}{'.l:font_series.'}{\\zs[^}]*\\ze}")'))
	endfor
	call filter(l:completion_list,'count(l:completion_list,v:val) == 1 ')
    " }}}3
    " {{{3 ------------ FONT ENCODING
    elseif l:completion_method == 'font encoding'
	let l:bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let l:epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if l:epos == -1
	    let l:epos=len(l:line)
	endif
	let l:fline=strpart(l:line,l:bpos,l:epos-l:bpos)
	let l:font_family=matchstr(l:fline,'\\\%(usefont\s*{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\|fontfamily\)\s*{\zs[^}]*\ze}')
	if l:font_family != ""
	    let l:fd_list=atplib#FdSearch(l:font_family)
	    let l:completion_list=map(copy(l:fd_list),'toupper(substitute(fnamemodify(v:val,":t"),"'.l:font_family.'.*$","",""))')
	else
" 	    let l:completion_list=[]
" 	    for l:fd_file in l:fd_list
" 		let l:enc=substitute(fnamemodify(l:fd_file,":t"),"\\d\\zs.*$","","")
" 		if l:enc != fnamemodify(l:fd_file,":t")
" 		    call add(l:completion_list,toupper(l:enc))
" 		endif
" 	    endfor
	    let l:completion_list=g:atp_completion_font_encodings
	endif
    " }}}3
    " {{{3 ------------ BIBITEMS
    elseif l:completion_method == 'bibitems'
	let l:col = col('.') - 1
	while l:col > 0 && line[l:col - 1] !~ '{\|,'
		let l:col -= 1
	endwhile
	let l:pat=strpart(l:l,l:col)
	let l:bibitems_list=values(atplib#searchbib(l:pat))
" 	let b:bibitmes_list=l:bibitems_list " DEBUG
	let l:pre_completion_list=[]
	let l:completion_dict=[]
" 	let b:completion_dict=l:completion_dict " DEBUG
	let l:completion_list=[]
	for l:dict in l:bibitems_list
	    for l:key in keys(l:dict)
		" ToDo: change l:dict[l:key][...] to get() to not get errors
		" if it is not present or to handle situations when it is not
		" present!
		call add(l:pre_completion_list, l:dict[l:key]['bibfield_key']) 
		let l:bibkey=l:dict[l:key]['bibfield_key']
		let l:bibkey=substitute(strpart(l:bibkey,max([stridx(l:bibkey,'{'),stridx(l:bibkey,'(')])+1),',\s*','','')
		if l:nchar != ',\|}'
		    let l:bibkey.="}"
		endif
		let l:title=get(l:dict[l:key],'title','notitle')
" 		if l:title == 'notitle'
" 		    let l:title=get(l:dict[l:key],'booktitle','')
" 		endif
		let l:title=substitute(matchstr(l:title,'^\s*title\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*\%(,\|$\)'),'{\|}','','g')
		let l:year=get(l:dict[l:key],'year',"")
		let l:year=matchstr(l:year,'^\s*year\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*\%(,\|$\)')
		let l:abbr=get(l:dict[l:key],'author',"noauthor")
		let l:author = matchstr(l:abbr,'^\s*author\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*,')
		if l:abbr=="noauthor" || l:abbr == ""
		    let l:abbr=get(l:dict[l:key],'editor',"")
		    let l:author = matchstr(l:abbr,'^\s*editor\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*,')
		endif
		if len(l:author) >= 40
		    if match(l:author,'\sand\s')
			let l:author=strpart(l:author,0,match(l:author,'\sand\s')) . ' et al.'
		    else
			let l:author=strpart(l:author,0,40)
		    endif
		endif
		let l:author=substitute(l:author,'{\|}','','g')
		if l:dict[l:key]['bibfield_key'] =~ 'article'
		    let l:type="[a]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'book\>'
		    let l:type="[B]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'booklet'
		    let l:type="[b]"
		elseif  l:dict[l:key]['bibfield_key'] =~ 'proceedings\|conference'
		    let l:type="[p]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'unpublished'
		    let l:type="[u]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'incollection'
		    let l:type="[c]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'phdthesis'
		    let l:type="[PhD]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'masterthesis'
		    let l:type="[M]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'misc'
		    let l:type="[-]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'techreport'
		    let l:type="[t]"
		elseif l:dict[l:key]['bibfield_key'] =~ 'manual'
		    let l:type="[m]"
		else
		    let l:type="   "
		endif

		let l:abbr=l:type." ".l:author." (".l:year.") "

		call add(l:completion_dict, { "word" : l:bibkey, "menu" : l:title, "abbr" : l:abbr }) 
	    endfor
	endfor
	for l:key in l:pre_completion_list
	    call add(l:completion_list,substitute(strpart(l:key,max([stridx(l:key,'{'),stridx(l:key,'(')])+1),',\s*','',''))
	endfor

	" add the \bibitems found in include files
	call extend(l:completion_list,keys(atplib#SearchBibItems(b:atp_MainFile)))
    elseif l:completion_method == 'colors'
	" ToDo:
	let l:completion_list=[]
    endif
    " }}}3
    if exists("l:completion_list")
	let b:completion_list=l:completion_list	" DEBUG
    endif
    " }}}2
" {{{2 make the list of matching completions
    "{{{3 if l:completion_method != close environments && != env_close
    if l:completion_method != 'close environments' && l:completion_method != 'env_close'
	let l:completions=[]
	    " Packages, environments, labels, bib and input files must match
	    " at the beginning (in expert_mode).
	    if (l:completion_method == 'package' 		||
			\ l:completion_method == 'environment_names' ||
			\ l:completion_method == 'colors' 	||
			\ l:completion_method == 'labels' 	||
			\ l:completion_method == 'bibfiles' 	||
			\ l:completion_method == 'bibstyles' 	||
			\ l:completion_method == 'font family' 	||
			\ l:completion_method == 'font series' 	||
			\ l:completion_method == 'font shape'	||
			\ l:completion_method == 'font encoding'||
			\ l:completion_method == 'documentclass' )
		if a:expert_mode == 1 
		    let l:completions	= filter(deepcopy(l:completion_list),' v:val =~ "\\C^".l:begin') 
		elseif a:expert_mode !=1
		    let l:completions	= filter(deepcopy(l:completion_list),' v:val =~ l:begin') 
		endif
	    " Bibitems match not only in the beginning!!! 
	    elseif (l:completion_method == 'tikz libraries' ||
			\ l:completion_method == 'inputfiles')
		let l:completions	= filter(deepcopy(l:completion_list),' v:val =~ l:begin') 
		if l:nchar != "}" && l:nchar != "," && l:completion_method != 'inputfiles'
		    call map(l:completions,'v:val."}"')
		endif
	    " Commands must match at the beginning (but in a different way)
	    " (only in expert_mode).
	    elseif l:completion_method == 'command' 
			if a:expert_mode == 1 
			    let l:completions	= filter(copy(l:completion_list),'v:val =~ "\\C^\\\\".l:tbegin')
			elseif a:expert_mode != 1 
			    let l:completions	= filter(copy(l:completion_list),'v:val =~ l:tbegin')
			endif
	    elseif l:completion_method == 'tikzpicture keywords'
		if a:expert_mode == 1 
		    let l:completions	= filter(deepcopy(l:completion_list),'v:val =~ "\\C^".l:tbegin') 
		elseif a:expert_mode != 1 
		    let l:completions	= filter(deepcopy(l:completion_list),'v:val =~ l:tbegin') 
		endif
	    elseif l:completion_method == 'tikzpicture commands'
		if a:expert_mode == 1 
		    let l:completions	= filter(deepcopy(l:completion_list),'v:val =~ "\\C^".l:tbegin') 
		elseif a:expert_mode != 1 
		    let l:completions	= filter(deepcopy(l:completion_list),'v:val =~ l:tbegin') 
		endif
	    endif
"     elseif l:completion_method == 'font family' ||
" 		\ l:completion_method == 'font series' ||
" 		\ l:completion_method == 'font shape'
" 	let l:completions=copy(l:completion_list)
    "}}}3
    "{{{3 else: try to close environment
    else
	call atplib#CloseLastEnvironment('a', 'environment')
	let b:tc_return="1"
	return ''
    endif
    "}}}3
    "{{{3 SORTING and TRUNCATION
    " ToDo: we will not truncate if completion method is specific, this should be
    " made by a variable! Maybe better is to provide a positive list !!!
    if g:atp_completion_truncate && a:expert_mode && 
		\ index(['bibfiles', 'bibitems', 'bibstyles', 'labels', 
		\ 'font family', 'font series', 'font shape', 'font encoding' ],l:completion_method) == -1
	call filter(l:completions,'len(substitute(v:val,"^\\","","")) >= g:atp_completion_truncate')
    endif
    if l:completion_method == "tikzpicture keywords"
	let l:bracket=atplib#CloseLastBracket(1)
	if l:bracket != ""
	    call insert(l:completions,l:bracket,0)
	endif
    endif
    " if the list is long it is better if it is sorted, if it short it is
    " better if the more used things are at the beginning.
    if len(l:completions) > 5 && l:completion_method != 'labels'
	let l:completions=sort(l:completions)
    endif
    " }}}3
    let b:completions=l:completions " DEBUG
    " }}}2
    " complete {{{2
    if l:completion_method == 'labels' 			|| 
		\ l:completion_method == 'package' 	|| 
		\ l:completion_method == 'tikz libraries'    || 
		\ l:completion_method == 'environment_names' ||
		\ l:completion_method == 'colors'	||
		\ l:completion_method == 'bibfiles' 	|| 
		\ l:completion_method == 'bibstyles' 	|| 
		\ l:completion_method == 'documentclass'|| 
		\ l:completion_method == 'font family'  ||
		\ l:completion_method == 'font series'  ||
		\ l:completion_method == 'font shape'   ||
		\ l:completion_method == 'font encoding'||
		\ l:completion_method == 'inputfiles' 
	call complete(l:nr+2,l:completions)
	let b:tc_return="labels,package,tikz libraries,environment_names,bibitems,bibfiles,inputfiles"
    elseif !l:normal_mode && l:completion_method == 'bibitems'
	call complete(l:col+1,l:completion_dict)
    elseif !l:normal_mode && (l:completion_method == 'command' || l:completion_method == 'tikzpicture commands')
	call complete(l:o+1,l:completions)
	let b:tc_return="command X"
    elseif !l:normal_mode && (l:completion_method == 'tikzpicture keywords')
	let l:t=match(l:l,'\zs\<\w*$')
	" in case '\zs\<\w*$ is empty
	if l:t == -1
	    let l:t=col(".")
	endif
	call complete(l:t+1,l:completions)
	let b:tc_return="tikzpicture keywords"
    endif
    " If the completion method was a command (probably in a math mode) and
    " there was no completion, check if environments are closed.
    " {{{ 3 Final call of CloseLastEnvrionment / CloseLastBracket
    let l:len=len(l:completions)
    if l:len == 0 && (!count(['package', 'bibfiles', 'bibstyles', 'inputfiles'], l:completion_method) || a:expert_mode == 1 )|| l:len == 1
	if (l:completion_method == 'command' || l:completion_method == 'tikzpicture commands') && 
	    \ (l:len == 0 || l:len == 1 && l:completions[0] == '\'. l:begin )

	    let filter 	= 'strpart(getline("."), 0, col(".") - 1) =~ ''\\\@<!%'''
	    let stopline 	= search('^\s*$\|\\par\>', 'bnW')

	    " Check inline math:
	    if atplib#CheckSyntaxGroups(['texMathZoneV', 'texMathZoneW', 'texMathZoneX', 'texMathZoneY'])
		call atplib#CloseLastEnvironment('i', 'math')
		let l:a	= "inline math"
	    " Check environments:
	    else
		let l:env_opened= searchpairpos('\\begin','','\\end','bnW','searchpair("\\\\begin{".matchstr(getline("."),"\\\\begin{\\zs[^}]*\\ze}"),"","\\\\end{".matchstr(getline("."),"\\\\begin{\\zs[^}]*\\ze}"),"nW")',max([1,(line(".")-g:atp_completion_limits[2])]))
		let l:env_name 	= matchstr(strpart(getline(l:env_opened[0]), l:env_opened[1]-1), '\\begin\s*{\zs[^}]*\ze}')
		if l:env_opened != [0, 0]
		    call atplib#CloseLastEnvironment('a', 'environment', l:env_name, l:env_opened)
		endif
	    endif
	    let b:comp_method.=' close_env end' "DEBUG
	    if exists("l:a")
		let b:tc_return.=" close_env end " . l:a
	    else
		let b:tc_return.=" close_env end"
	    endif
	elseif l:completion_method == 'package' || 
		    \  l:completion_method == 'bibstyles' || 
		    \ l:completion_method == 'bibfiles' ||
		    \ l:completion_method == 'bibitems'
	    let b:tc_return='close_bracket end'
	    call atplib#CloseLastBracket()
	endif
    endif
    "}}}3
""}}}2
"  ToDo: (a challenging one)  
"  Move one step after completion is done (see the condition).
"  for this one have to end till complete() function will end, and this can be
"  done using (g)vim server functions.
"     let b:check=0
"     if l:completion_method == 'environment_names' && l:end =~ '\s*}'
" 	let b:check=1
" 	let l:pos=getpos(".")
" 	let l:pos[2]+=1
" 	call setpos(".",l:pos) 
"     endif
"
    " unlet variables if there were defined.
    if exists("l:completion_list")
	unlet l:completion_list
    endif
    if exists("l:completions")
	unlet l:completions
    endif
    return ''
    "}}}2
endfunction
" }}}1

" Font Preview Functions
"{{{1 Font Preview Functions
" These functions search for fd files and show them in a buffer with filetype
" 'fd_atp'. There are additional function for this filetype written in
" fd_atp.vim ftplugin. Distributed with atp.
"{{{2 atplib#FdSearch
"([<pattern>,<method>])
function! atplib#FdSearch(...)

    if a:0 == 0
	let l:pattern=""
	let l:method=0
    else
	let l:pattern=a:1
	if a:0 == 1
	    let l:method=0
	else
	    let l:method=1
	endif
    endif
"     let b:a=a:0
"     let b:method=l:method

    " Find fd file
    let l:path=substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
    let l:path=substitute(l:path,':\|\n',',','g')
    let l:fd=split(globpath(l:path,"**/*.fd"),'\n') 

    " Match for l:pattern
    let s:fd_matches=[]
    for l:fd_file in l:fd
	if (l:method==0 && fnamemodify(l:fd_file,":t") =~ l:pattern) ||
		    \ (l:method==1 && l:fd_file =~ l:pattern)
	    call add(s:fd_matches,l:fd_file)
	endif
    endfor

    return s:fd_matches
endfunction
"{{{2 atplib#FontSearch
" atplib#FontSearch([<pattern>,<method>]) 
" method = 0 match for name of fd file
" method = 1 match against whole path
if !exists("*atplib#FontSearch")
function! atplib#FontSearch(...)
	
    if a:0 == 0
	let l:pattern=""
	let l:method=0
    else
	let l:pattern=a:1
	if a:0 == 1
	    let l:method=0
	else
	    let l:method=1
	endif
    endif
"     let b:a=a:0
"     let b:method=l:method

    let s:fd_matches=atplib#FdSearch(l:pattern,l:method)

    " Open Buffer and list fd files
    " set filetype to fd_atp
    let l:tmp_dir=tempname()
    call mkdir(l:tmp_dir)
    let l:fd_bufname="fd_list " . l:pattern
    let l:openbuffer="32vsplit! +setl\\ nospell\\ ft=fd_atp ". fnameescape(l:tmp_dir . "/" . l:fd_bufname )

    let g:fd_matches=[]
    if len(s:fd_matches) > 0
	echohl WarningMsg
	echomsg "Found " . len(s:fd_matches) . " files."
	echohl None
	" wipe out the old buffer and open new one instead
	if buflisted(fnameescape(l:tmp_dir . "/" . l:fd_bufname))
" 	    echomsg "DEBUG DELETE BUFFER"
	    silent exe "bd! " . bufnr(fnameescape(l:tmp_dir . "/" . l:fd_bufname))
	endif
	silent exe l:openbuffer
	" make l:tmp_dir available for this buffer.
" 	let b:tmp_dir=l:tmp_dir
	cd /tmp
	map <buffer> q	:bd<CR>

	" print the lines into the buffer
	let l:i=0
	call setline(1,"Press Enter to open a font definition file:")
	for l:fd_file in s:fd_matches
	    " we put in line the last directory/fd_filename:
	    " this is what we cut:
	    let l:path=fnamemodify(l:fd_file,":h:h")
	    let l:fd_name=substitute(l:fd_file,"^" . l:path . '/\?','','')
" 	    call setline(line('$')+1,fnamemodify(l:fd_file,":t"))
	    call setline(line('$')+1,l:fd_name)
	    call add(g:fd_matches,l:fd_file)
	    let l:i+=1
	endfor
	silent w
	setlocal nomodifiable
	setlocal ro
    else
	echohl WarningMsg
	echomsg "No fd file found."
	echohl None
    endif

endfunction
endif
"}}}2
"{{{2 atplib#Fd_completion /not needed/
" if !exists("*atplib#Fd_completion")
" function! atplib#Fd_completion(A,C,P)
"     	
"     " Find all files
"     let l:path=substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
"     let l:path=substitute(l:path,':\|\n',',','g')
"     let l:fd=split(globpath(l:path,"**/*.fd"),'\n') 
"     let l:fd=map(l:fd,'fnamemodify(v:val,":t:r")')
" 
"     let l:matches=[]
"     for l:fd_file in l:fd
" 	if l:fd_file =~ a:A
" 	    call add(l:matches,l:fd_file)
" 	endif
"     endfor
"     return l:matches
" endfunction
" endif
" }}}2
" {{{2 atplib#OpenFdFile /not working && not needed?/
" function! atplib#OpenFdFile(name)
"     let l:path=substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
"     let l:path=substitute(l:path,':\|\n',',','g')
"     let b:path=l:path
"     let l:fd=split(globpath(l:path,"**/".a:name.".fd"),'\n') 
"     let l:fd=map(l:fd,'fnamemodify(v:val,":t:r")')
"     let b:fd=l:fd
"     execute "split +setl\\ ft=fd_atp " . l:fd[0]
" endfunction
" }}}2
"{{{2 atplib#Preview
" keep_tex=1 open the tex file of the sample file, otherwise it is deleted (at
" least from the buffer list).
" To Do: fd_file could be a list of fd_files which we would like to see, every
" font should be done after \pagebreak[4]
" if a:fd_files=['buffer'] it means read the current buffer (if one has opened
" an fd file).
function! atplib#Preview(fd_files,keep_tex)
    if a:fd_files != ["buffer"]
	let l:fd_files={}
	for l:fd_file in a:fd_files
	    call extend(l:fd_files,{fd_file : readfile(l:fd_file)})
	endfor
    else
	let l:fd_files={bufname("%"):getline(1,"$")}
    endif
    unlet l:fd_file

    let l:declare_command='\C\%(DeclareFontShape\%(WithSizes\)\?\|sauter@\%(tt\)\?family\|EC@\%(tt\)\?family\|krntstexmplfamily\|HFO@\%(tt\)\?family\)'
    let b:declare_command=l:declare_command
    
    let l:font_decl_dict={}
    for l:fd_file in a:fd_files
	call extend(l:font_decl_dict, {l:fd_file : [ ]})
	for l:line in l:fd_files[l:fd_file]
	    if l:line =~ '\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'
		call add(l:font_decl_dict[l:fd_file],l:line)
	    endif
	endfor
    endfor
    let b:dict=l:font_decl_dict " DEBUG

"     let l:tmp_dir=tempname()
    if exists("b:tmp_dir")
	let l:tmp_dir=b:tmp_dir
    else
	let l:tmp_dir=tempname()
    endif
    if !isdirectory(l:tmp_dir)
	call mkdir(l:tmp_dir)
    endif
    if a:fd_files == ["buffer"]
	let l:testfont_file=l:tmp_dir . "/" . fnamemodify(bufname("%"),":t:r") . ".tex"
    else
	" the name could be taken from the pattern
	" or join(map(keys(deepcopy(a:fd_files)),'substitute(fnamemodify(v:val,":t:r"),".fd$","","")'),'_')
	" though it can be quite a long name.
	let l:testfont_file=l:tmp_dir . "/" . fnamemodify(a:fd_files[0],":t:r") . ".tex"
    endif
    call system("touch " . l:testfont_file)
    
    let l:fd_bufnr=bufnr("%")

    let s:text="On November 14, 1885, Senator \\& Mrs.~Leland Stanford called
		\ together at their San Francisco mansion the 24~prominent men who had
		\ been chosen as the first trustees of The Leland Stanford Junior University.
		\ They handed to the board the Founding Grant of the University, which they
		\ had executed three days before.\\\\
		\ (!`THE DAZED BROWN FOX QUICKLY GAVE 12345--67890 JUMPS!)"

"     let l:text="On November 14, 1885, Senator \\& Mrs.~Leland Stanford called
" 	\ together at their San Francisco mansion the 24~prominent men who had
" 	\ been chosen as the first trustees of The Leland Stanford Junior University.
" 	\ They handed to the board the Founding Grant of the University, which they
" 	\ had executed three days before. This document---with various amendments,
" 	\ legislative acts, and court decrees---remains as the University's charter.
" 	\ In bold, sweeping language it stipulates that the objectives of the University
" 	\ are ``to qualify students for personal success and direct usefulness in life;
" 	\ and to promote the public welfare by exercising an influence in behalf of
" 	\ humanity and civilization, teaching the blessings of liberty regulated by
" 	\ law, and inculcating love and reverence for the great principles of
" 	\ government as derived from the inalienable rights of man to life, liberty,
" 	\ and the pursuit of happiness.''\\
" 	\ (!`THE DAZED BROWN FOX QUICKLY GAVE 12345--67890 JUMPS!)\\par}}
" 	\ \\def\\\moretext{?`But aren't Kafka's Schlo{\\ss} and {\\AE}sop's {\\OE}uvres
" 	\ often na{\\"\\i}ve  vis-\\`a-vis the d{\\ae}monic ph{\\oe}nix's official r\\^ole
" 	\ in fluffy souffl\\'es? }
" 	\ \\moretext"

    if a:fd_files == ["buffer"]
	let l:openbuffer="edit "
    else
	let l:openbuffer="topleft split!"
    endif
    execute l:openbuffer . " +setlocal\\ ft=tex\\ modifiable\\ noro " . l:testfont_file 
    map <buffer> q :bd!<CR>

    call setline(1,'\documentclass{article}')
    call setline(2,'\oddsidemargin=0pt')
    call setline(3,'\textwidth=450pt')
    call setline(4,'\textheight=700pt')
    call setline(5,'\topmargin=-10pt')
    call setline(6,'\headsep=0pt')
    call setline(7,'\begin{document}')

    let l:i=8
    let l:j=1
    let l:len_font_decl_dict=len(l:font_decl_dict)
    let b:len_font_decl_dict=l:len_font_decl_dict
    for l:fd_file in keys(l:font_decl_dict) 
	if l:j == 1 
	    call setline(l:i,'\textsc\textbf{\Large Fonts from the file '.l:fd_file.'}\\[2em]')
	    let l:i+=1
	else
" 	    call setline(l:i,'\pagebreak[4]')
	    call setline(l:i,'\vspace{4em}')
	    call setline(l:i+1,'')
	    call setline(l:i+2,'\textsc\textbf{\Large Fonts from the file '.l:fd_file.'}\\[2em]')
	    let l:i+=3
	endif
	let l:len_font_decl=len(l:font_decl_dict[l:fd_file])
	let b:match=[]
	for l:font in l:font_decl_dict[l:fd_file]
	    " SHOW THE FONT ENCODING, FAMILY, SERIES and SHAPE
	    if matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') == "b" ||
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') == "bx"
		let b:show_font='\noindent{\large \textit{Font Encoding}: \textsf{' . 
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textit{Font Family}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^}#]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textit{Font Series}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') . '}' . 
			    \ ' \textit{Font Shape}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}') . '}}\\[2pt]'
	    else
		let b:show_font='\noindent{\large \textbf{Font Encoding}: \textsf{' . 
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textbf{Font Family}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^}#]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textbf{Font Series}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') . '}' . 
			    \ ' \textbf{Font Shape}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}') . '}}\\[2pt]'
	    endif
	    call setline(l:i,b:show_font)
	    let l:i+=1
	    " CHANGE THE FONT
	    call setline(l:i,'{' . substitute(
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'),
			\ l:declare_command,'usefont','') . 
			\ '\selectfont')
	    " WRITE SAMPLE TEXT
	    call add(b:match,matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'))
	    let l:i+=1
	    " END
	    if l:j<l:len_font_decl
		call setline(l:i,s:text . '}\\\\')
	    else
		call setline(l:i,s:text . '}')
	    endif
	    let l:i+=1
	    let l:j+=1
	endfor
    endfor
    call setline(l:i,'\end{document}')
    silent w
    if b:atp_TexCompiler =~ '^pdf'	
	let l:ext=".pdf"
    else
	let l:ext=".dvi"
    endif
    call system(b:atp_TexCompiler . " " . l:testfont_file . 
	    \ " && " . b:atp_Viewer . " " . fnamemodify(l:testfont_file,":p:r") . l:ext ." &")
    if !a:keep_tex
	silent exe "bd"
    endif
endfunction
" }}}2
"{{{2 atplib#FontPreview
" a:fd_file  pattern to find fd file (.fd will be appended if it is not
" present at the end),
" a:1 = encoding
" a:2 = l:keep_tex, i.e. show the tex source.
function! atplib#FontPreview(fd_file,...)
    if a:0 == 0
	let l:keep_tex=0
	let l:enc=""
    elseif a:0 == 1
	let l:enc=a:1
	let l:keep_tex=0
    elseif a:0 ==2
	let l:enc=a:1
	let l:keep_tex=a:2
    endif
    if filereadable(a:fd_file)
	let l:fd_file=a:fd_file
    else
	" Find fd file
	if a:fd_file !~ '.fd\s*$'
	    let l:fd_file=a:fd_file.".*.fd"
	else
	    let l:fd_file=a:fd_file
	endif
	let l:path=substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
	let l:path=substitute(l:path,':\|\n',',','g')
	let l:fd_all=split(globpath(l:path,"**/*.fd"),'\n') 
	let l:fd=filter(l:fd_all,'v:val =~ l:fd_file && fnamemodify(v:val,":t") =~ "^".l:enc')
	if len(l:fd) == 0
	    echo "FD file not found."
	    return
	elseif len(l:fd) == 1
	    let l:fd_file_list=l:fd
	else
	    let l:i=1
	    for l:f in l:fd
		echo l:i." ".substitute(f,'^'.fnamemodify(f,":h:h").'/\?','','')
		let l:i+=1
	    endfor
	    let l:choice=input('Which fd file? ')
	    if l:choice == "" 
		return
	    endif
	    let l:choice_list=split(l:choice,',')
	    let b:choice_list=l:choice_list
	    " if there is 1-4  --> a list of 1,2,3,4
	    let l:new_choice_list=[]
	    for l:ch in l:choice_list
		if l:ch =~ '^\d\+$'
		    call add(l:new_choice_list,l:ch)
		elseif l:ch =~ '^\d\+\s*-\s*\d\+$'
		    let l:b=matchstr(l:ch,'^\d\+')
		    let l:e=matchstr(l:ch,'\d\+$')
		    let l:k=l:b
		    while l:k<=l:e
			call add(l:new_choice_list,l:k)
			let l:k+=1
		    endwhile
		endif
	    endfor
	    let b:new_choice_list=l:new_choice_list
	    let l:fd_file_list=map(copy(l:new_choice_list),'get(l:fd,(v:val-1),"")')
	    let l:fd_file_list=filter(l:fd_file_list,'v:val != ""')
" 	    let l:fd_file=get(l:fd,l:choice-1,"return")
	    if len(l:fd_file_list) == 0
		return
	    endif
	endif
    endif
    call atplib#Preview(l:fd_file_list,l:keep_tex)
endfunction
"}}}2
" {{{2 atplib#ShowFonts
function! atplib#ShowFonts(fd_file)
    let l:declare_command='\C\%(DeclareFontShape\%(WithSizes\)\?\|sauter@\%(tt\)\?family\|EC@\%(tt\)\?family\|krntstexmplfamily\|HFO@\%(tt\)\?family\)'
    let b:declare_command=l:declare_command
    
    let l:font_decl=[]
    for l:line in readfile(a:fd_file)
	if l:line =~ '\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'
	    call add(l:font_decl,l:line)
	endif
    endfor
    let l:font_commands=[]
    for l:font in l:font_decl
	call add(l:font_commands,substitute(
		    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'),
		    \ l:declare_command,'usefont',''))
    endfor
    return l:font_commands
endfunction
"}}}2
" }}}1
"
" vim:fdm=marker:ff=unix:noet:ts=8:sw=4:fdc=1
doc/automatic-tex-plugin.txt	[[[1
2314
*atp* 		Automatic TeX Plugin

			An Introduction to AUTOMATIC (La)TeX PLUGIN
				by Marcin Szamotulski
			    mszamot [AT] gmail [DOT] com
			----------------------------------------

If you found this plugin useful or you have any kind of problems with running
it or some ideas to share, you are cordially invited to write to me:
mszamot@gmail.com. Voting at Vim site is also welcome ;) .

-----------------------------------------------------------------------------
					Abstract

This is a filetype plugin for Vim to comfortably write TeX (LaTeX, PdfLaTeX)
documents, which provides functionality not met in other such plugins. It
makes you FREE from compiling procedure, making this process automatic using
autocommands. It also provides useful mappings and other functions: to analyse
your .log file, to see the table contents, to search for a label, to search in
bib files or to find a macro definition matching a pattern, or even to find
and preview fonts in your tex distribution. The features include an extended
tab completion for: commands, environment names, packages, input files, bib
files, bst files, colors, closing brackets and environments (preserves
nesting),... etc. To have full functionality you need: pdffonts available in
the package app-text/poppler-utils (at least in Gentoo). Another good tool is
texdoc, which is a part of texlive - these days standard TeX distribution for
Linux, and MikTeX on Windows.
------------------------------------------------------------------------------

Features include:
-----------------
* completion for commands, closing environments (even nested), package names,
  citations and labels.
	See |atp-Completion|, 
* a powerful function to search in bibliographic files (bib files):
	See |atp-bibsearch|,
* table of contents which allows to switch between different '.tex' files:
	See |atp-command-TOC|,
* list of labels which allows to see the context of a label:
	See |atp-command-Labels|,
* a command to list ToDo lines:
	See |atp-command-ToDo|.
* a command to search for a macro definition (multi-line support):
 	See |atp-command-DefiSearch|
* a command to search and PREVIEW fonts in your latex distribution:
 	See |atp-command-FontSearch|, and |atp-command-FontPreview|


			Table of Contents
-----------------------------------------------------------------------------
                                                		*atp-help-toc*
	|atp-news|			News
	|atp-installation| 		Installation								
	|atp-functions| 		Functions and commands
	|atp-bibsearch|			Searching in bib files
	|atp-completion|       		How to use and configure completion
	|atp-omnicompletion|			and omnicompletion 	(*NEW*)
	|atp-configure| 		How to configure to your needs 
	|atp-mappings|  		Mappings and Commands
	|atp-errors|  			Error handling
	|atp-requirements|  		Requirements
	|atp-viewers| 			Note about viewers 
					(including inverse and reverse searching for xdvi)
	|atp-color-highlight|		Colors and syntax files
	|atp-remarks|  			Final remarks
	|atp-copy-rights|		Copy Rights

	
Note on usage: type :help atp<CTRL>d to see all the helptags. To see help tags
for all the defined functions :help atp*()<CTRL>d, mappings: :help atp-map<CTRL>d

================================================================================

							*atp-news*
News: >
	$HOME/.atprc.vim	
<				A configuration file for ATP. Now you do not
				have to use autocommands to set local-buffer
				options, just place them here.
>
	call back/debug mode	
<				|atp-debug-mode| This was rewritten so you
				should read it! There are NEW FEATURES !!!
				Note: you need use gvim or >
					vim --servername VIM
<				for this.   
>
	b:atp_TexFlavour	
<				if you are editting a plain tex file it is
				automatically set to 'plaintex', then you get
				hihglighting for $$:$$. Some other features
				are planned (you can also set this while
				editting a 'tex' file, i.e. latex document but
				using $$:$$ is latex is not recommended it is
				know to break some latex specific things).
>
	New Feature in version 7.1
	g:atp_MathVimOptions	= { 'textwidth' : '0' }
<				This variable is a dictionary of vim settings
				and its values which will be valid when you
				edit mathematics inside the pairs \(:\), $:$,
				\[:\], $$:$$ (only in plain tex files or if
				g:atp_TexFlavour = 'plaintex'). For example,
				the default value will toggle between your
				'textwidth' in non-math and 0 in math.
				The dictionary may contain short option names
				eqally well as long names.

				Note: the standard tex syntax file defines 
				other zones: for example for align and
				equation environments (and many others) but
				some how they are not accessible using
				synstack() function. 
				
				This feature can be turned off using setting
				variable >
					g:atp_SetMathVimOptions
<				to '0', the default is '1'.

	New Editing Tools	vie, viE, vae, vi$ and va$ or when in visual
	written by 		mode ie, iE, ae i$, a$.
	David Munger		They select the current environment in two
				ways:
				i 	- innner
				a 	- auter
				e 	- environment 
				$	- one of math zones \(:\), $:$, \[:\]
					  plain tex $$:$$ is not yet supported.

				'viE' selects a bit more than 'vie' but less
				than 'vae', it selects a bracket pair before
				the begining of the inner part of an
				environment, so it can be environment name or
				an option just after. 						


================================================================================
Installation                               		*atp-installation*
>
	 :filetype plugin on is required to run this plugin, see
	 |:filetype-plugin-on| and |:filetype-indent-on| if you want to have
	 automatic indentation for TeX files.
<
To install you just need to copy tex.Vim file to ~your ~/.Vim/ftplugin/
directory copy this help file to ~/.Vim/doc and then run :helptags ~/.Vim/doc
and that's all, now you can just type your story ... :)


================================================================================
Commands	                               		*atp-commands*
							

The main function is not seen by the user (it is called s:compiler, for those
who want to read the plugin). It executes tex compiler specified by the
variable b:atp_TexCompiler. It is executed
as an autocommand by the line:
	au! CursorHold $HOME*.tex silent call 's:auTeX()'
where s:auTeX() is a simple function which calls s:compiler if the file written
on the disk and the buffer differ.
As you can see it will run if a key is not pressed during time defined by
option 'updatetime' (see |CursorHold|) in the normal mode. If you type in
insert mode the file won't be compiled (and that's alright as you can be in the
middle of your very long formula). The value of 'updatetime' which works fine
is around 1000ms ('updatetime' is set in milliseconds). Tex compiler is run with
one options:
	-output-directory 
which points to a unique temporary file in Vim temporary directory (using the
function 'tempname()' (see |tempname()|. If you are concerned with security
reasons read also: |shelltemp|.

You can switch off/on the function s:auTeX by pressing <S-F5> or by letting
the local to buffer variable b:autex=1 (on) b:autex=0 (off). It is useful in
some situations turn automatic compiling off. The key <S-F5> calls the function
ToggleAuTex() which sets the variable b:autex and issue a message. You can also
set this variable to 0 for some files that are not supposed to be processed,
for example:
>
	au BufRead texmf/*.tex let b:atp_autex=0
<
On start up b:atp_autex is set to 1 if the path of opened file is not under any
tex directory ('kpsewhich -show-path tex', except the current dir). For example,
files located under your local texmf tree will have b:atp_autex=0.

The second important variable b:atp_TexCompiler (see |atp-variable-b:atp_TexCompiler|) configures
if you use TeX, PdfTeX, LaTeX, PdfLaTeX and it should point to the program
name so please do not use capital letters.

Next variable to set is b:atp_OutDir (see |atp-variable-b:atp_OutDir|). It configures where TeX
will put the output and where viewer and log analyzing tools can find
appropriate files. 

The last top most important variable is |atp-variable-g:keep| which is a list of extensions,
by default it is
	let g:keep = ["log","aux","toc","bbl"]
Files with this extension will be copied from b:atp_OutDir to the temporary
directory with appropriate name to be used when (La)TeX is compiling. (log file
will be only copied after it is created, other files will be copied back and
forth between you b:atp_OutDir and the temporary directory)

							|atp-callback|
							|atp-debug-mode|
	By default the call back mechanism is turned on (g:atp_callback=1)

	When call back mechanism is set, which is by default if you run gui
	version, if you invoke 'vim' from command line you need to add
	'servername' variable, it might be desirable to alias vim to to >
			vim --servername VIM 
< 	you have additinal funcionalities:

	* STATUS LINE NOTIFICATION: status line can show if tex is running >
		    let g:atp_status_notification = 1
<		If unset you will get a message when compiler ends.
		If set the number next to the name of your compiler indicates
		how many instances are currently running.
	* The LOG FILE will be automatically read after compilation.

	* if t:atp_DebugMode 	= 'silent'
			   You will not get any message from compilation. 

				= 'debug'
			   After the end of compilation (invoked by the user
			   or atocommand) you will get a message with the
			   return status of the compilator.

			   If you open the error window with :copen or with
			   the menu option ToggleDebugMode then it will be
			   automatically closed after first compilation with
			   exist status 0. 

			   	= 'verbose'
			   Every compilation which is invoked by the user will
			   be run in verbose mode as with <F5> key.

	Note: the 'verbose' mode in 'vim' (in console) needs to be run, when
	there is no other latex instance running. Now you get a message to
	wait until compelation ends. In future releases, a better solution
	will be worked out. Gui version 'gvim' works better (as it dosn't  
	susspend the edditor).

	 The background compilation is always done in g:atp_DefaultDebugMode.
	 Unless it is set to 'verbose' in which case 'debug' mode is used. 

	 You can invoke compiler in the 'debug' mode with '<LocalLeader>d',
	 '<LocalLeader>l' uses the default mode.

						*atp-variable-b:atp_ReloadOnError*
The variable b:atp_ReloadOnError if set to 1 (the default) reload the file
even when the exit status of compiler was non zero. If set to 0, then the file
will not be reloaded [actually for viewers other than xpdf it will not be
copied from the temporary directory, for xpdf it will be copied but not
reloaded). 

There is also a variable which stores the last command which executed
your tex compiler, see |atp-variable-b:texcommand|.   

Below I explain commands (functions) which are accesible: 

:TEX [mode]						*atp-command-TEX*
map \l, map \d,imap \l
	If anyway you want to run TeX yourself but you do not want to see the
	output this is the right tool. This runs TeX in 'nonstopmode'. You can
	specify an argument <runs> which tells how many consecutive runs of
	TeX you need (this is important if you want to compile Table of
	Contents, or index, or the bibliography (see |atp-command-Bibtex|)

	If b:atp_OpenViewer=1 and there current viewer (b:Viewer) is not
	running on the output file then this function will open a viewer. By
	default b:atp_OpenViewer=0 and this feature is disabled. 

	The command :2TEX will run :call TEX(2), :TEX 3 do :call TEX(3), and
	:2TEX3 will resolve to :call TEX(3).

	It is useful when you want to make the outline (using hyperref
	package) of your article in pdf files, the tex file has to be
	'sourced' twice. To make the bibliography you can use |atp-command-Bibtex|.

	If <runs> > 5 it will be reduced to 5, to avoid running tex for hundreds
	(or event thousands) of times (what could happen otherwise by
	a mistake giving the range of the command to be the current line
	number).

	mode is one of '', 'silent', 'debug', 'verbose'. When '' the value of
	g:atp_DefaultDebugMode is used. See descriptino of |atp-debug-mode|.

	\d is mapped to :TEX debug and \l to :TEX (thus it uses your default
	debug mode).

:VTEX 							*atp-command-VTEX*
map <F5>,imap <F5> 
	This is equivalent to ':TEX verbose'.

:ShowErrors <flag>					*atp-command-ShowErrors*
	This command shows error/warning messages. It sets the |'errorformat'|
	variable accordingly to the flag, which is a word made of letters:
>
		e		- include errors
		w		- include all warning messages
		r		- include all reference warnings
		c		- include all citations warnings
		f		- include all font warnings
		fi		- include font info massages
		F		- show files listed in the log
				    (messages which start with 'File: ')
				    shows the files loaded by tex
					for example fd files that LaTeX is using
		p		- show packages loaded by tex 
				    (messages which start with 'Package: ')
		all		- show all the log file		    
		o		- open the log file in a new buffer (split).
<
	If none flag is given 'e' is used.  If 'o' flag is uesd the split
	buffer with log message has a map 'q' to ':bd'.  
	Example: >
		:ShowErrors rc
<	will show all reference and citation warnings.

ShowErrors maps:					*atp-command-ShowErrors-maps* 

<F6>+e			to see all errors 	(:ShowErrors e)
<F6>+w			to see all warnings	(:ShowErrors w)
<F6>+r			to see warnings coming	(:ShowErrors rc) 
			from references or citations  
<F6>+f			to see font warnings 	(:ShowErrors f)

this is not a texloganalyzer mapping but it is a good place to mention it:
<F6>+l			to open log file in a new split window
			this is a mapping to the |atp-command-OpenLog|.

	
						*atp-command-SetErrorFormat* 
:SetErrorFormat <flag> 	
	This command has the same syntax as :ShowErrors. It only sets the
	|'erroformat'| variable.
	
						*atp-command-Bibtex* 
map \b, :Bibtex, :Bibtex v
	This function will call bibtex to produce the bibliography file
	(.bbl). If in |atp-variable-b:atp_OutDir| there is no 'aux' file it first calls tex
	compiler. After the 'bbl' file is produced two consecutive runs of tex
	compiler are called to make the bibliography.

	If you specify any value to the <debug> option then then this function
	will be called in verbose mode (only the last time tex compiler will
	run in errorstop mode). This gives you the chance to see the output of
	bibtex command for a second. The command :Bibtex v is associated to
	this behaviour. If you want to just run bibtex see the next function.

	The command :Bibtex  will :call Bibtex(), while :Bibtex v
	(and :Bibtex <anything>) will :call Bibtex(1)

							*atp-variable-g:atp_bibinputs*
	Tex is looking for the date base files in the path: `kpsewhich
	-show-path bib`. The variable g:atp_bibinputs contains Vim list of
	these direcrories. Thus if atp cannot find your bib file, tex also
	won't be able. 

							*atp-command-SimpleBibtex*
map \sb, :SBibtex
	This calls bibtex on the aux file in your |atp-variable-b:atp_OutDir| directory and
	shows you the output. It is useful if you are debugging your
	bibliography database. 
	
							*atp-command-ViewOutput*
map \v,map <F3>, imap \v, imap <F3>  
	You would like to see what you are editing use this function. It will
	use the program defined in the variable b:Viewer. See |atp-variable-b:Viewer|,
	|atp-variable-g:atp_XpdfServer|, |atp-xpdfOptions|. When there is no output file it will run
	TeX and open the file. Read more about particular viewers
	(inverse/reverse searching) in |atp-viewers|. 

:SetXdvi						*atp-command-SetXdvi*
	This command sets the options for xdvi viewer, which enables inverse
	and reverse searching. It sets the command
		:IS[earch]
	and the map '<LocalLeader>is' for inverse searching. For reverse
	searching hold CTRL and click with left mouse button on the text in
	xdvi viewer. It will read the xdvi options from the variable
	g:xdviOptions (to the variable b:atp_ViewerOptions).

:SetXpdf						*atp-command-SetXpdf*
	This command sets the options for xpdf viewer (as for now the
	inverse/reverse searching in pdf files is not implemented)
	It will read the xpdf viewer options from the variable g:xpdfOptions
	(to the varaible b:atp_ViewerOptions).

							see |atp-command-BibSearch|
:BibSearch
	This function finds bib entries in bib files defined in your tex file
	and in the variable b:bibfiles (see |atp-variable-b:bibfiles|), which match the
	<pattern> (a vim regular expression). The output is configurable by
	the <flag> argument, see |atp-bibflags|.

							see |atp-command-BibChoose|
:BibChoose, map c, map y, map p
	This function is defined in the window with results of BibSearch
	command. It is mapped to 'y' and 'c' and let you copy the bib entry key
	to a register (see |atp-command-BibChoose|) or directly to last opened
	buffer (after the last cursor position). When you chose to paste, it
	will close the BibSearch window.

						*atp-command-FindBibFiles*
:FindBibFiles
	This updates the variables s:bibfiles, s:allbibfiles,
	s:notreadablebibfiles (showed by ShowOptions command). Finds all bib
	files defined in all '\bibliography' commands. For more about the
	above variables read |atp-variables-bib|. This function is called
	internally be the script functions BibSearch/BibChoose/ShowOptions.
	The command :FindBibFiles finds bib files in the current buffer. 

	If a readable bib file was not found in any of g:atp_bibinputs
	directories (see |atp-variable-g:atp_bibinputs|) directories it is classified as
	not readable.  

						*atp-command-FindInputFiles*
:FindInputFiles [bufname]
	This function finds all the input files, i.e. files put after the
	tex commands: '\input', '\include' or '\includeonly'. And prints the
	result. 
	The function and the command have one optional argument is the buffer
	name as given by |bufname|. By default it searches for input files in the current buffer.
	The bufername completion is set, hence you can use <Tab> to choose the
	bufername.

							*atp-command-EditInputFile*
:EditInputFile [input_file_name] [bufname]
	This function finds input files (using the function FindInputFiles()
	in the current buffer and let you choose one to edit.

	The first as well as the second argument are optional. There is
	completion for the <input_file_name> argument, so that you can just
	press <Tab> to switch between input files. If you want to list the
	first and then choose do not pass any arguments.

	The <input_file_name> may contain white spaces, but then has to be quoted
	with '"' (this is a latex requirement).

	Input files are searched in the |atp-variable-b:atp_OutDir| directory, if not found
	there then in the directory |atp-variable-g:texmf|. The default value is 
	g:texmf=$HOME/texmf, which is the default value of local texmf tree in
	texlive.

	The bibliographis declared are also listed. The command searches for
	them in the any of g:atp_bibinputs directory (see |atp-variable-g:atp_bibinputs|.

							*atp-command-OpenLog*
:OpenLog, map <F6>l, imap <F6>l
	Opens log file in a new split window with two options (which are set
	locally): ruler, nospell, and a map 'q' to ':bd'.	

	You can also use the command ':Explore' to see log,aux,... files
	(which is a part of 'netrw' vim plugin).

							*atp-command-Delete*
map <F6>d
	Deletes all the files with an extension which belongs to
	g:atp_tex_extensions in the directory b:atp_OutDir. By default
	g:atp_tex_extensions does not contain '.tex', '.pdf', '.dvi' so none
	of your important files will be deleted. If you set
	g:atp_delete_output=1 the function will delete also the current output
	file (but not any other!).

Print([<printer>, <printer_options>])			*atp-command-Print*
map \p, :SshPrint
	It will run 'lpr' command and append to it the options defined in the
	variable 'g:printeroptions' + options given in the second argument. It
	prints the pdf or dvi depending on the value of 'b:atp_TexCompiler' (see
	|atp-variable-b:atp_TexCompiler|).  If you specify the variable
	'g:atp_ssh=<user>@<host>' it will print via ssh on the <host> using
	the <printer>. The command ':SshPrint' has a completion set for the
	printers available on your local system or in the host. All the
	arguments of the command SshPrint are |<f-agrs>|. 
	
	Both arguments are optional (the default system printer is used, and
	only the options 'g:printeroptions' applay).

	The map '\p' will print on the default printer.

	The command has completion for the names of printers (also remote
	printers), press <Tab> to cycle through printers, or type first
	letters of the printers name and press <Tab> to complete it.

							*atp-command-Lpstat*
:Lpstat
	Sends "lpstat -l" remotly (using the 'g:atp_ssh' value) or locally and
	echoes the output.

							*atp-command-ShowOptions* 
:ShowOptions, :ShowOptions v 
	This will show values of variables that are currently set. If you specify any
	argument the deafult values will be shown is square brackets.
>
		:ShowOptions v
<		
							*atp-command-WrapSelection*
:WrapSelection <begin_wrapper>,[<end_wrapper>,<cursor_pos>,<new_lines>]

	Puts selected text inside begin_wrapper:<end_wrapper> and sets the
	cursor position according to the variables <cursor_pos>. Possible values
	are: a number (indicates the character of <begin_wrapper> to put the
	cursor on (see and check vmap \c below), or 'end' put the cursor at
	the end of <end_wrapper> or 'begin' leave the cursor at the begining
	(to be precise at the end of the starting wrapper).  
	The default <end_wrapper> is '}'.  The last argument <new_lines>
	0/1 (defult is 0): if 1 then the begin_wrapper and end_wrapper are put
	in seperate lines (the begin line and end line are split), this is
	useful for puting text into and evinronment \begin{}:\end{}. 

	The command arguments should be separated with commas (see |<args>|).

							*atp-command-InteligentWrapSelection*
:InteligentWrapSelection <math_wrapper_pair>,<text_wrapper_pair>,[<cursor_pos>,<new_lines>]

	Puts the selected text inside <math_wrapper_pair> if the cursor stands
	in mathematics otherwise inside <text_wrapper_pair>.
	<math_wrapper_pair> <text_wrapper_pair> are vim lists of length at
	least 1, the first wrapper is the opening and the second is the
	closing one (if not given the default '}' is used. The other arguments
	are as for |atp-command-WrapSelection|. If the opening leader in is not
	given then this command is not wrapping the text (see below for the
	suggested map '\tx') 

	The command arguments should be separated with commas (see |<args>|).

	These are the provided maps in visual mode: >
	    vmap <buffer> \rm	:<C-U>InteligentWrapSelection ['\\textrm{'],	['\\mathrm{']<CR>
	    vmap <buffer> \em	:<C-U>InteligentWrapSelection ['\\emph{'],	['\\mathit{']<CR>
	    vmap <buffer> \it	:<C-U>InteligentWrapSelection ['\\textit{'],	['\\mathit{']<CR>
	    vmap <buffer> \sf	:<C-U>InteligentWrapSelection ['\\textsf{'],	['\\mathsf{']<CR>
	    vmap <buffer> \tt	:<C-U>InteligentWrapSelection ['\\texttt{'], 	['\\mathtt{']<CR>
	    vmap <buffer> \bf	:<C-U>InteligentWrapSelection ['\\textbf{'],	['\\mathbf{']<CR>
	    vmap <buffer> \bb	:<C-U>InteligentWrapSelection ['\\textbf{'],	['\\mathbb{']<CR>
	    vmap <buffer> \sl	:<C-U>WrapSelection '\\textsl{'<CR>
	    vmap <buffer> \sc	:<C-U>WrapSelection '\\textsc{'<CR>
	    vmap <buffer> \up	:<C-U>WrapSelection '\\textup{'<CR>
	    vmap <buffer> \md	:<C-U>WrapSelection '\\textmd{'<CR>
	    vmap <buffer> \n	:<C-U>InteligentWrapSelection ['\\textnormal{'],['\\mathnormal{']<CR>
	    vmap <buffer> \cal	:<C-U>InteligentWrapSelection [''],['\\mathcal{']<CR>
	    vmap <LocalLeader>f					:WrapSelection '{\usefont{'.g:atp_font_encoding.'}{}{}{}\selectfont ', '}',(len(g:atp_font_encoding)+11)<CR>
<   	Suggested maps: >
	    vmap <buffer> \tx	:<C-U>InteligentWrapSelection [''],['\\text{']<CR>
	    vmap <buffer> \in	:<C-U>InteligentWrapSelection [''],['\\intertext{']<CR>"
<	The leader '\' in above commands is configurable: the value of
	g:atp_vmap_text_font_leader is used (the default is '\').

	Another provided wrapper: >
	    vmap <buffer> \f :WrapSelection 
	         \ '{\\usefont{".g:atp_font_encoding."}{}{}{}\\selectfont ', '}', '(len(g:atp_font_encoding)+11)'<CR>
<	Where the variable: >
	    g:atp_font_encoding
<	stores the default encoding which is 'OT1', unless you use fontenc
	package, then the default for fontenc is used (the last defined in
	\usepackage[...]{fontenc} see the 'Latex2e font selection'
	/font user guide/ available on CTAN).

	Other wrappers: >
	    vmap m			:WrapSelection '\(', 	'\)'<CR>
	    vmap M			:WrapSelection '\[', 	'\]'<CR>
	    vmap <LocalLeader>(		:WrapSelection '(', 	')', 	'begin'<CR>
	    vmap <LocalLeader>[		:WrapSelection '[', 	']', 	'begin'<CR>
	    vmap <LocalLeader>{		:WrapSelection '{', 	'}', 	'begin'<CR>
	    vmap <LocalLeader>)		:WrapSelection '(', 	')', 	'end'<CR>
	    vmap <LocalLeader>]		:WrapSelection '[', 	']', 	'end'<CR>
	    vmap <LocalLeader>}		:WrapSelection '{', 	'}', 	'end'<CR>
	    vmap <LocalLeader>b(	:WrapSelection '\left(', '\right)', 'begin'<CR>
	    vmap <LocalLeader>b[	:WrapSelection '\left[', '\right]', 'begin'<CR>
	    vmap <LocalLeader>b(	:WrapSelection '\left(', '\right)', 'begin'<CR>
	    vmap <LocalLeader>b[	:WrapSelection '\left[', '\right]', 'end'<CR>
	    vmap <LocalLeader>b{	:WrapSelection '\left{', '\right}', 'end'<CR>
	    vmap <LocalLeader>b{	:WrapSelection '\left{', '\right}', 'end'<CR>
< 	And the maps to put the selected text into an environment: >
	    vmap <LocalLeader>C	:WrapSelection '\begin{center}',	'\end{center}',		'0','1'<CR>
	    vmap <LocalLeader>R	:WrapSelection '\begin{flushright}',	'\end{flushright}',	'0','1'<CR>
	    vmap <LocalLeader>L	:WrapSelection '\begin{flushleft}',	'\end{flushleft}',	'0','1'<CR>
<	(note that the arguments for this command must be put in ':' or ":")
   	the highlighted text will put inside \textbf{ }. 

	You can also use a wrapper which was yanked into register 'a': >
		:WrapSelection @a
<  	This will work not only in visual mode. It will operate on last
	selected text. So if you accidentaly lost the selection you can still
	use this commad (but not the maps)!

:TOC [skip] 							*atp-command-TOC*
map \t
	Shows Table of Contents of your document. It do not yet support the
	started version of chapter, section,... environemnts. 

	The optional argument skip controls if the table of contents data base
	must be generated: by default map \t doesn't regenerate the toc data
	base (unless if it doesn't exist), and :TOC command regenerate the
	data base.
	
	The function opens new window in which you can use the mappings: 

		'e' 	to echo the line from your tex file
		'y' 	to yank the label of the chapter under the curosor
				to a register, if it exists,
	 	'p' 	to paste it directly to your tex file (just after the
				current cursor position), 
		's'	it splits the window with your tex source file and
			sets the current line to the beginning of the
			chapter/section under the cursor,
		'q' 	to quit, and finaly, 
		<Enter> to go to the chapter under the cursor and close ToC.
		<space> to go to the chapter under the cursor but leave ToC
			open.

	There are also commands: ':C' and ':P', which do the same as 'c' and
	'p' mappings. They all call the function 'Yank(<where>)', the argument
	<where> can be one of: '@<register name>' or 'p'.  

	TOC() supports many edited files. For example if you have in your
	buffer list two files a.tex and b.tex this command will produce table
	of contents of both of them. If you have just one opened window
	(excluding the ToC window) then pressing <space>, <enter>, p and q
	will take you to the right buffer (which will be read if is unloaded
	or hidden). If you split a window then <space>, <enter>,
	p, q will take you to the window from which you are comming. However,
	if you have two windows with two different buffers loaded they will
	act on the window with the matching buffer name.

	The variable t:toc_window_width sets the width of table of contents
	window. By default t:toc_window_width=30. You can set a global
	variable g:toc_window_width to overide the default value.

							*atp-command-CTOC*
:CTOC	
	This function returns the name of the currently edited chapter/
	section/subsection/subsubsection. Use ':echo CTOC()' or just ':CTOC' to
	see the returned value. If you added a section unit the function will
	not update the database, run ':TOC' to do that (map \t).

:Labels							*atp-command-Labels*
map \L 
	Shows labels defined in your file. You can also use the mappings
	'e','c','p','s','q' and <Enter> as above.

	If you forget what are these mappings, write ':map' in the TOC or
	LABELS window.

	The key 's' shows the context of the label under the cursor (your
	current window splits).

	The variable t:labels_window_width sets the width of labels window. By
	default t:labels_window_width=30. You can set a global
	variable g:labels_window_width to overide the default value.

						*atp-move-command-NEnv*
:NEnv <environment>
	Move to next environment, for example ':NEnv definition'. Completion
	is set, which finds environemts defined in current tex source file.
	This function omits environments in comment lines.

						*atp-move-command-PEnv*
:PEnv <environment>
	Move to previous environment, for example ':NEnv definition'. Completion
	is set, which finds environments defined in current tex source file.
	This function omits environments in comment lines.


						*atp-move-command-NextSection*
:NPart, :NChap, NSec, map \NP, map \NS, map \NS
	Goes to next <section>, the commands need not to be explained.

						*atp-move-command-PrevSection*
:PPart, :PChap, PSec, map \PP, map \PC, map PS
	Goes to previous <section>.

ToDo(<keyword>,<stop>,[bufname])		*atp-function-ToDo*
:ToDo [bufname]					*atp-command-ToDo*
:Note [bufname]					*atp-command-Note*
	The function list all the lines of the buffer [bufname] which match
	for the pattern '%.*<keyword>'. The <stop> argument is the pattern to
	before which to stop. The optional argument is the buffer
	name (the buffer name completion is set on). If not given the
	current buffer is assumed.
	You can set highlighting for this command by:
		highlight atp-Todo ctermfg=... 	guifg=...
	The command :ToDo sets keyowrd='\c\<todo\>' and
	stop='\s*%.*\c\<note\>', and the command :Note
	sets keyowrd='\c\<note\>' and stop='\s*%.*\c\<todo\>'. This prevent
	from listing ToDo lines with Notes and vice versa. 
	 
:ToggleSpace, map <F2>				*atp-command-ToggleSpace*
	This function (command) sets, if it is undefined or removes if it is
	defined, the mapping:
>
		:cmap <Space> \_s\+
<
	which is useful when searching by the command '/', especially if
	|'textwidth'| or |'wrapmargin'| is non zero (and |'formatoptions'|
	contains one of the flags 't', 'v' or 'b'). Then each <Space> will
	match for a space which also might end of the line.

							*atp-command-ToggleStar*
:ToggleStar 	 		adds/removes a star from the current 
map <LocalLeader>s		environment (if it is not one belonging 
				to the list: >
					g:atp_no_star_environments
<				
							*atp-command-ToggleEnvironment*
:ToggleEnvironment 	mapped to <F4> and <S-F4>, switches environment
map <F4>, map <S-F4>    name. See (i.e. echo ) g:atp_toggle_environments_1...7 
			(you can change or add your own variables,
			just add numbers - they must be consecutive).

							*atp-command-ToggleLabels*
							*atp-variable-g:atp_toggle_labels*
	It also changes the prefixes of labels (if there is one, which belongs
	to g:atp_shortnames_dict) and all ref's (\ref, \eqref and \pageref).
	You have to turn on this feature by putting g:atp_toggle_labels=1 (by
	default it is 0).  Check if it works for you!  If there is a label or
	reference to which it wants to change it doesn't change labels and
	issue a Warning Message, thus I believe it should work for every one,
	but as this changes your file it is turned off by default.) 


:SetOutDir						*atp-command-SetOutDir*
	This is a command which sets the 'b:atp_OutDir' variable and the |'errorfile'| option.
	See |atp-variable-b:atp_OutDir| for the default value.

:SetErrorFile						*atp-command-SetErrorFile*
	If you change |atp-variable-b:atp_OutDir| variable and you want to update the
	|'errorfile'| option use this command. It will show you the value to
	which |'errorfile'| was set. 

:ATPStatus						*atp-command-ATPStatus*
	This function (command) sets the status line, which include: the name
	of currently eddited   chapter (or section) the value of 'b:atp_OutDir' and
	it will warn you if 'b:atp_OutDir' variable is not set. This function is
	called at startup unless the variable 'g:atp_statusline=0' is set (for
	example in you $VIMRC file). The status is set by the autocommand: >
		au BufWinEnter *.tex :call ATPStatus()
<	In this way every opened window with a '*.tex' file will get the correct
	status line.


:FontSearch [<pattern> <match_option>]			*atp-command-FontSearch*
	
	For example:
	:FontSearch ^t1
		will list all the fd files in your tex distribution which
		names starts with t1 (i.e. which describes fonts in encoding
		'T1')
	:FontSearch bookman 1
		will list all fd files which full path mathes 'bookman'.

	The <match_option> argument has two values 0/1, if it is not given
	0 is assumed.

	In the opened window there are several mappings defined:
	    <Enter>   	open the fd file under the cursor
	    <Tab>	list fonts defined in the fd file (shows the command
			that you can use in your tex file to use this font)
	    p		preview the fonts defined in the fd file under the
			cursor, do not shows the latex source. 	
	    P		preview fonts and show the latex source 
			(then you can see the errors why the preview was not
			produced; in many cases there are no encoding files or
			fonts themselves for which you have to look in CTAN
			archive yourself; or YOU CAN just SEE HOW TO USE 
			FONTS :) )
	    q 		close the window (actually, delete the buffer using
		       :bd, it will be still in the list if you type ":ls!",
		       so even then you can reload previous searches.)  

	In addition to 'p' and 'P' maps there is a :Preview command. 
	Note: the maps 'p' and 'P' work in both normal and visual mode.
	You can select a part of the text and use this maps or the command
	:Preview to make one preview file for all the font files.


	The same mappings are defined in the window with fd file opened
	(except <Enter>, <Tab>).  

	Additionally, read |font-lowlevelcommands| to learn how to use
	|\usefont|, |\fontsize|, |\selectfont| and other such commands.
	The 'Latex 2e font selection' by 'LeTeX3 Project Team' might be very
	useful. It is available on the net (you probably have it in your tex
	distriution if it is installed with the documentation, if not check
	the CTAN archive).

	ATP also has a very nice completion for various font declaration
	commands, see |atp-completion-fontdeclaration|.

	Hopefully, this will help your documents to become beautiful :)

							*atp-command-FontPreview*
:FontPreview <fd_file> [<encoding> <keep_tex>]
	Previews all fonts defined in fd file matching the pattern <fd_file>
	with encoding <encoding> (optional). If <keep_tex> is 1 (defualt is 0)
	it will keep the latex source file for debuging purpoposes.

	It returns a list of fonts which fd files matches the <fontname> and
	<encoding>. You will be asked to chose for which files make a preview,
	possible answers are: >
			1,2,3
< 	which is equivalent to >
			1-3
<	you can also mix this notation: >
			1,2-5,7	
<	As in FontSearch command the <keep_tex> variable specifies if
	the source file will be shown (for debugging purposes, or just to look how
	to use the font :).

:PID 							*atp-command-PID*
	Prints PIDs of all running instances of |atp-variable-b:atp_TexComopiler|.

================================================================================
Searching in bib files 		                        *atp-bibsearch*

		___________________________
		Tabel of Contents:
		|atp-BibSearch|
		|atp-bibpatterns|
		|atp-bibflags|
			|atp-bibflags:default|
			|atp-bibsearch-show-only-keys|
			|atp-bibflags:+|
			|atp-bibflags:output|
			|atp-bibflags:all|
			|atp-bibflags:last|
			|atp-bibflags:add-flag|	
		|atp-command-BibChoose|	
		|atp-bibsearch-highlight|
		|atp-command-BibSearch|
		|atp-bibflags:examples|
		|atp-bibsearch-variables|
			|atp-variable-b:bibfiles|

		____________________________
		Naming Conventions:	

		@article{<label>,					\	
			author = { .... },		<-- bib entry    | 
			title  = { .... },				 > bib field
			journal= " .... ",				|
		}							/	

			article 		<-- bib field keyword 
			author,title,...	<-- bib entry label 	
			<label>			<-- bib field label 	


One more function is provided which searches the bib files for bib fields, 
and for the bib field labels for the latex command \cite{}.

BibSearch([<pattern>,<flags>)				*atp-BibSearch* 
	The function BibSearch allows you to search for the <pattern> in bib
	files and opens a new window with results. For the command, please read
	|atp-bibsearch-command|.

	The function BibSearch takes two arguments (the last one is optional).
	The first one is the <pattern> to match against each line of the
	bibliographic files supplied in the commands \bibliography (if there
	are several names,please do not add a white space ' ' after ',' unless
	the file name begins with a space, i.e.
>
 	\bibliography(Mathematics, Physics,/home/user/Bibliography)
< 
	then the plugin will understand that the names of the bib files are
	'Mathematics.bib', ' Physics.bib' and '/home/user/Bibliography.bib'.

								*atp-bibpatterns*
	Each line of every bib file found in your tex document will be
	matched against the pattern, for example if the pattern is:
>
 		'author.*Grothendieck'
<
	the BibSearch function will find all the bibliographic fields
	which in one line have the words 'author' and 'Grothendieck' (in most
	cases it means that you will see only works of Grothendieck). Another
	example:
>
 	'^\(\s*author.*Joayl\)\|Galois Theory'
<
	will result in all bib fields which author is Joyal or 
	which includes the words 'Galois Theory' (which by the way apear in
	many article/book titles), yet another example:	
>
 	'author.*Joayl\|title\p*Galois Theory'
<
	This will match against all bib entries written by Joyal or which title
	includes the word 'Galois Theory'.
>
 		:call BibSearch('author.*Joyal\&.*Tirney')	
<	
	will find all the bib entries which were written by Joyal and Tirney
	(and maybe somebody else). 

	For now, there is no possibility to filter bibliographic entries which
	both match a pattern in separate lines, i.g. to show all bib entries
	written by Joyal on 'Descent Theory'.

	Before a match, all '{', and '}' are deleted from the line of the bib file.
	But you will see them in the output (what can be useful for debugging
	errors in bib files)

	Note that in Vim patterns should be quoted using '...' not "...".   

	Further examples are supplied after the next section
	|atp-bibflags:examples|, which describes other functionalities
	of the BibSearch/BibChoos functions.

								*atp-bibpattern:last*
								*atp-bib-b:atp_LastBibPattern*
	The variable 'b:atp_LastBibPattern' stores the last pattern used by
	bib search.

								*atp-bibflags*
	The first optional argument [flags] chooses what and in which order
	you want to see the  bib entries found (entries are listed in
	the order they appear in bib file).  Flag is a word made of letters.
	There are three kinds of flags: entry flags which matches against
	labels of bib entries, like author, title, etc..., and keyword flags: which
	matches against keywords of bib fields: @article, @book, @techreport,
	etc...  and two special flags 'All' and 'L'. A flag is a word on
	letters:
>
		a  - author
 		e  - editor
 		t  - title
 		b  - booktitle
 		j  - journal
 		s  - series
 		y  - year
 		n  - number
 		v  - volume
 		p  - pages
 		P  - Publisher
 		N  - Note
 		S  - School
 		h  - howpublished
 		o  - organization
		u  - url	
		H  - Homepage	
  any other letter - do not show anything but the first line of bib entry 
		@a - article 						/@article/
		@b - book or booklet 					/@book,@booklet/
		@B - Booklet 						/@booklet/	
		@c - incollection 					/@incollection,@inbook/
		@p - proceedings, inproceedings, conference   		/@proceedings,@inproceedings,@conference/
		@m - misc 						/@misc/
		@M - Manual 						/@manual/
		@t - master or PhD thesis  				/@masterthesis,@phdthesis/
		@T - Techreport 					/@techreport/
		@u - unpublished  					/@unpublished/		
		All - all flags						(see |atp-bibflags:all|)		
		L   - last flags					(see |atp-bibflags:last|)		
<

	Examples:
>
		tayu@a		--> show the entries: tile, author, year, url of matching articles.
		baeP@b		--> show the entries: booktitle, author, editor, 
 							publisher of matching books (@book,@booklet).
<
	Flags '@.' are filtered out, if one does not belong to the one above
	then it is deleted. You can see which flags are defined using
	ShowOptions function/command (they are listed as Available
	KeyWordFlags).
								*atp-bibflags:default*
	The default flag is stored in the global variable g:defaultbibflags and is
	equal to 'tabejsyu'. This means that the output for each bib field found 
	will include the 
		title
		author
		booktitle
		editor
		journal 
		series
		year
	if title,author,... are specified in the bibliography for the given
	position. If there are many position which match you can set flags to
	be as simple as possible to include more lines on the screen. For
	example 'tabe' is quite reasonable (note that all bib entries are
	matched separately, i.e. if a bib field has both 'title' and 'booktitle'
	bib entries it will give you both of them.

								*atp-bibsearch-show-only-keys*
	If you just want to list just the lines with bib fields keywords:
	@article{, @book{, etc. supply a flag which do not belongs to
	'g:defaultallbibflags', for example 'X', or 'X@a'
	
								*atp-bibflags:+*
	You can also specify flags with '+', for example: 
>
	flags='+p'
	flags='+@b'
<
	This feature ADDS FLAGS TO THE DEFAULT VALUE defined in the variable
	g:defaultbibflags (see |atp-defaulbibflags|). The first will result in
	showing the default entries and the page number, the second will
	result in showing only books with the default bib entries. You can
	specify as many additional flags as you wish.  *atp-bibflags:output*
	Note that the function shows the line with database file name if there
	are entries in this bibliography which match the pattern thus,for
	example, if you specify the flag '@a' and you see the line with
	database file name, but you do not see any bib entry, then in this
	database there are bib fields which match but these are not articles.
	
								*atp-bibflags:all*
	The flags='All' is a synonim of flag=g:defaultallbibflags which by default is
	equal to'tabejfsvnyPNSohiuHcp' i.e. all flags in this order. If you
	add your own flag you should change this global variable. You can add to
	this flag any flag which contains '@' (see |atp-bibflags|) by
	the plus operator, i.e. All+@a@b or +@aAll will give the same result.

								*atp-bibflags:last*
								*atp-bib-b:atp_LastBibFlags*	
	The variable 'b:atp_LastBibFlags' stores the recently used flags. The flag
	'L' sets the flags for this search to the value of 'b:atp_LastBibFlags'.
	You can write '+L@a', '+L@a', 'L@a' or '@aL' but not '+@La', if you
	want to add some flags to previous searches. Next time the flag 'L'
	will change the meaning (i.e. it is really the last time not earlier
	:) However, there is no '-' :( '-@aL' could be helpful.
	 
	The variable 'b:atp_LastBibFlags' is not changed when you use the 'All'
	flag.

								*atp-bibflags:add-flag*
	You can add your own flags but not keyword flags (i.e. @a,@b,...).
	Just add an entry to the dictionary g:bibflagsdict. (:ShowOptions v to
	see its current value), For example
>
	let g:bibflagsdict=extend(g:bibflagsdict, { '<flags_name>' : [
	'<bib_entry_name>': '<how_to_show>'] })
< 
	where, <flags_name> is the flag to use (it should be one letter), it
	must be different from the defined flags, <bib_entry_name> is a
	lowercase bib entry name, like 'title', 'url', etc., <how_to_show> if
	you want to have a nice output put the bib entry name and that much of
	white spaces to get 13 strings.
		
BibChoose							*atp-command-BibChoose*
map c, map y, map p
	This function/command is only available in the window with BibSearch results
	and allows to copy a bib entry key to a register or directly to the
	last opened buffer (after the cursor position). It is mapped to 'c'
	and 'y'. You will be asked to give the number of bib entry to yank:
>
	    <bib entry number><register name><Enter>	- to copy it to a register
	    <bib entry number><Enter>			- to paste it to 'tex' file
	    <Enter>					- to skip the choice
<	
	When you paste the bib entry key the bib search window will close.

								*atp-bibsearch-highlight*
	The colours of the output are set by the syntax file
	'syntax/bibsearch_atp.Vim'. All groups except one are the same as in
	the syntax file for bib files ('syntax/bib.Vim' in your $VIMRUNTIME
	directory). Their names are 'bibsearchEntryKw' instead 'bibEntryKw'.
	The one that is differently defined 'bibsearchComment'.  Which is
	changed in that way to highlight the bib file names.  One additional
	highlight group is: 'bibsearchInfo'. It highlights the number of
	entry and its line number in the bib file. By default all bibsearch
	groups are linked to the corresponding bib group, the bibsearchInfo
	group is not set.
	
	In a colour file (~/.Vim/color/*.Vim) you can use these groups to set
	colours.  See |highlight| or just read a colour file. For example,
	this is a nice set of colours for dark background 
		
							 	
:BibSearch [pattern] [flag] 					*atp-command-BibSearch*
	which do what you expect. The arguments should not be quoted and
	separated by a white spaces (if you want to include a white space use
	'\ '), for more see |f-args|. If you do not provide any argument then
	all entries of all bib files will be shown. Examples:

	Some examples:						*atp-bibflags:examples*
>
 	:BibSearch 
<				Above command shows all bib fields with
				the default flags
>
 	:BibSearch @ yt	
<				and this is a tip how to show all bib fields with
				different flags than the default ones(the '@'
				will match at every bib field!). It is
				equivalent to:
>
	:call BibSearch('','yt')

 	:BibSearch title[\s.]*Galois Theory  aetb
<
	The next one shows all bib fields which were written by Joyal and
	Tirney (and may by somebody else).
>
	:BibSearch 'author.*Joyal\&.*Tirney'
<

:DefiSearch [pattern] [preambule_only]				*atp-command-DefiSearch*
	Both argument are optional.
	Finds all definitions which matches the pattern. It looks in the main
	file (only in the preambule, unless the optional argument is equal
	0, the default is 1) and all the input files (except bib files).

	The pattern is case sensitive (i.e. the function appends '\C' at the
	beginning of the pattern by default, if you want to make case
	insensitive matching put '\c' at the beginning of your pattern).

								*atp-bibsearch-variables*
								*atp-variables-bib*	
SOME VARIABLES:
	All the values of important variables can be shown by ShowOption
	command.

								
b:bibfiles							*atp-variable-b:bibfiles*
	This variable is a list and you can put here additional bib files.
	They will be parsed by the BibSearch/BibChoose functions.

	The following variables you can see using the ShowOptions command (see
	|atp-ShowOptions|).
s:bibfiles
	This variable is a list which stores bib files found in your tex
	files and which are readable. It is set up when you first run of the commands:
	BibSearch/BibChoose/ShowOptions. Its value is shown by the
	functions FindBibFiles({bufname}).
s:allbibfiles 
	this is a sum of found bib files the locally defined b:bibfiles, which
	not necessarily are readable.
s:notreadablebibfiles
	guess what :)

-----------------------------------------------------------------------------------
								*atp-bibsearch-comments*
	Please do not hesitate to report any bug to me:
	mszamot@gmail.com 							
	
	The algorithm will work only if all kind of bib entries of your bib
	file are included in the list g:bibentries. However, changing just
	this variable is not enough. In that case the search engine (function
	s:search) will produce correct output, but the function which displays
	found entries, will not know how to work with the new entries. One
	would have to add an entry to the dictionary 'g:bibflagsdict'. If
	it is the case, please let me know: mszamot [AT] gmail [DOT] com  

	As you can see entries of the type '@string' which can be used in bib
	files are not supported (i.e. there is no function which substitutes
	the variables defined in @string to their values), but it is doable.
			@string{ Name = Value }
			

================================================================================
Completion			                        *atp-completion*

The completion is by default mapped to <Tab>. For example if you type
\math<Tab> you will get a list of choices which completes this command. (See
':h popupmenu-completion' and ':h completion' for more).

There are two completion algorithm: expert mode  and non expert mode: the
keyword for completion in expert mode must match at the beginning, in non
expert mode any where. Also in expert mode the list of possible completions is
smaller (for example there is no '\lneqq', there is only '\lneq').

If you prefer to not map <Tab> key (you can use '>' '<' to make the tabulation
in visual mode - see ':h >') then you can define g:atp_no_tab_map=1 in your
Vimrc file. 

You can switch a completion mode adjusting the variable
'g:atp_completion_active_modes', all names of completion modes are stored in the
variable 'g:atp_tab_completion_modes'.

If 'g:atp_local_completion' is set to non zero value, then input files
will be scanned for \def, \newcommand, \newnevironment and \newtheorem
commands and they will be used for completion. (if its value is 1 then this
will be done during first completion - this is the default, if it is set to
2 then this will be done at start up.

	NOTE: if you press <Tab> but then you changed your mind, the
	completion pop-up menu allows to cancel completion: press ctrl+p (i.e.
	go up - some times more than once) and then press <space>.

							*atp-command-ToggleTab*
There is a command to toggle the tab map off/on: :ToggleTab, it is also mapped
to `<Tab>.

Completion modes are:

	commands					*atp-completion-commands*
		if g:atp_check_if_math_mode = 1 then the pull of commands
		contains math commands only if there you are inside a math
		environment. This works perfectly if you switch from $:$ and
		$$:$$ to their equivalent (and more up-to-date) \(:\) and \[:\].
		The list of math environment in which ATP will think you are
		editing a math mode is stored in the variable:
		'g:atp_math_modes'. Its entries are two element list of
		patterns which matches the beginning and end of a math mode.
		The '0' entry have to provide the beginning and end pattern of
		in line math '\(:\)', the second for displayed math '\[:\]'.
		Its default value is given below.
	
		If you add a package (like tikz or amsmath, or amssymb) then
		the pull of completions will contain extra
		commands/environment names defined in these packages.  
		Because some classes calls amsmath package silently 
		setting the variable 'g:atp_amsmath=1' will ensure that 
		you will get completions for these commands. The algorithm
		checks if you have this package declared or if you use some of
		the standard ams class (actually checks if the document class
		name matches '^ams'). 

		If you do not want math commands completions at all define
		':let g:atp_no_math_command_completion=1' (you can put it in your
		~/.vimrc, or define while writing, both will work, so you can
		switch off the math completions temporarily)

		The label command completes in a special way: for example in
		a line like:
			\begin{theorem}\lab<Tab>
		will complete to 
			\begin{theorem}\label{thm:
		The dictionary of short names is 'g:atp_shortname_dict'. If
		you do not likes this idea (however it can help you to
		correctly write \ref{ - to avoid referring to lemmas as
		propositions, and also it makes completion for \ref{ nicer
		- you can list only labels for theorems), so if you do not
		want it anyway: 'let g:atp_no_short_names=1' will make the
		work.

							*atp-variable-local_completion*
							*atp-variable-b:local_commands*
							*atp-variable-g:local_commands*
							*atp-variable-b:local_environments*
							*atp-variable-g:local_environments*
		By default the first time you are completing an environment
		name or a command a list of locally defined environments and
		commands is made (it takes a few seconds). If you do not want
		to completions for them define "let g:atp_local_completion=0",
		if g:atp_local_completion=2" then the search for local
		definitions and commands will be done on startup. If you
		added a command or an environment the function
		"LocalCommands()" will update the list of local definitions.
		The output is stored in two variables: >
				b:atp_local_commands
				b:atp_local_environments
<
		If you use the same set of definitions in every tex file
		you can set >
				g:atp_local_commands 
				g:atp_local_environments
<
		which if defined are used instead of b:atp_local_commands and
		b:atp_local_environments (use the function LocalCommands()
		to generate the list and then use the command: 
		    :let @a= b:atp_local_commands 
		i.e. copy the variable to register a and paste it in your Vimrc file.)

		There is an extended support for tikz picture environment both
		inline \tikz{:} and displayed
		\begin{tikzpicture}:\end{tikzpicture}. The completion works
		for commands and keywords. The pull of them is enlarged if you
		add a tikz library. Yet not all the libraries are
		supported but this is just the matter of my time. Normal
		commands are added if you are inside {:}.


	ref/label/cite					*atp-completion-ref*
							*atp-completion-label*
							*atp-completion-cite*
		for label completion puts short names, for ref and eqref
		commands the completions are the labels found in all files
		associated to the main file (the plugin searches the input
		and include files for them). The same for cite: which searches
		also in bib files defined in the main file.

		There is also omnicompletion (CTRL-X CTRL-O, see
		|i_CTRL-X_CTRL-O|) for \cite command. Check it out, as it is
		very nice (especially in gvim!) and very fast. 

		For both completion and omnicompletion for the cite command,
		the text after \cite{  [ or after a comma after \cite{ ] is
		treated as a regular expression. Thus you can wirte:

		\cite{.*author1\&.*author2<Tab>

		to get the completions for things written by both author 1 and
		2 (regardless of the order they appear in bib files).

		BibTeX omni completion is triggered by '\cite{', '\citep{' or '\citet{'.
		For example, assume you have in your .bib files an entry looking like: >

		@book {	knuth1981,
				author = "Donald E. Knuth",
				title = "Seminumerical Algorithms",
				publisher = "Addison-Wesley",
				year = "1981" }

		Then, try: >

			\cite{Knuth 1981<CTRL-X><CTRL-O>
			\cite{algo<CTRL-X><CTRL-O>
<

    You can also use regular expressions (or vim patterns) after '\cite{'.

	brackets					*atp-completion-brackets*
		Closing of brackets {:},{:},[:],(:) (also closes math modes \(:\) and
		\[:\]). 
		Relelvant variables are: g:atp_bracket_dict a dictionary of
		brackets by default it consists of pairs '(' : ')', '{' : '}',
		'[' : ']'. There is a second dictionary g:atp_sizes_of_brackets
		which contains all the sizes of brackets in latex plus a pair
		'\' : '\', for closing the math modes: \(:\), \[:\] and the
		brackets \{:\}.
															
	environments					*atp-closing-environments*
							*atp-completion-env*
		Completes after '\begin{' and '\end{'. For example
		'\begin{cen<Tab>' will give '\begin{center}' 
		But '\begin{theorem}<Tab>' or
		'\begin{theorem}\label{thm:1}<Tab> will close the environment.
		The algorithm tries to close environment in many natural
		situations: for example when it did found less than one command
		completion. It closes the right environment when they are
		nested (however not in right place!) and it preserves the
		indention. When after \begin{center}\label{...} XXX is
		something (in place of XXX) it will close the environment in
		after the cursor position otherwise in next line.

		The environments opened in tex definitions ('\def',
		'\newcommand', '\renewcommand') will not be closed unless the
		current cursor position is in that line (sometimes one might
		want to have a definition which only opens an environment).

		EXAMPLES:
				
			(the <Tab> indicates in which position the
			<Tab> can be pressed to get the described
			behaviour).
>
			    \begin{theorem}
				    \begin{enumerate}
				    \item .....
				    \item .....
					\begin{minipage} 	
					    ......
					\end{minipage}
					    ......
					    ......<Tab>
					    XXXXXX
					    ......
			    \end{theorem}
<			Usually the closing comes in the next line,
			unless we are inside an environment which is opened
			after the non closed environment: 
>
			    \begin{theorem}
				    \begin{enumerate}
				    \item .....
				    \item .....
					\begin{minipage}<Tab> 	
					    ......<Tab>
					\end{minipage}<Tab>
					    XXXXXX
					    ......
					    ......
					    ......
			    \end{theorem}
<			Then the closing will be put just after the last
			opened environment closes, or
>
			    \begin{theorem}
				    \begin{enumerate}
				    \item .....
				    \item .....
					\begin{minipage}
					    ......
					\end{minipage}
					    ......
					    ......
					    ......
					    XXXXXX
			    \end{theorem}<Tab>
			    ....<Tab>
<			If we are outside the theorem environment,
			'\end{enumerate}' will be placed just above
			'\end{theorem}', and 	
>
			    \begin{theorem}[Joyal\&Tirney]\label{thm:jt}
				    \begin{enumerate}
				    \item .....
				    \item .....
					\begin{minipage} 	
					    ......
					    ......
					    XXXXXX
				    \end{enumerate}<Tab>
			    \end{theorem}<Tab>
<			will put \end{minipage} just above
			\begin{enumerate}. Furthermore, if:
>
			    \begin{theorem}
				    \begin{enumerate}\label{enu:1}
				    \item .....
				    \item .....
					\begin{minipage} 	
					    ......
					    \begin{itemize}
						    ......
					    \end{itemize}
					    ......
					    ......
					    XXXXXX
				    \end{enumerate}<Tab>
			    \end{theorem}<Tab>
<			'\end{minipage}' will be put just above
			'\end{enumerate}'.  Furthermore,
>
			\begin{theorem}[...]\label{...} Let \(C\) be a ....
			......
			......<Tab> XXXXX
<	
		That is, if you like to write \begin{}:\end{} in the beginning
		and end of a line this will be preserved. However, there is no
		support for nested environments then!

	font declarations				*atp-completion-fontdeclaration*
		This is completion for the commands 
		    \usefont{<encoding>}{<font_familly>}{<font_series>}{<font_shape>},
		    \fontencoding{<encoding>},
		    \fontfamily{<font_family>},
		    \fontseries{<font_series>},
		    \fontshape{<font_shape>},
		    \DeclareFixedFont{<cmd>}{<encoding>}{<font_familly>}{<font_series>}{<font_shape>}{<size>}
		
		It first splits the line and take a part between the commands
		\selectfont (if there is not \selectfont command this step is
		omitted).

		Then if the <encoding> is declared the font families for the
		completion will only come from this <encoding>.

		If <font_family> is defined, <font_series> and <font_shape>
		come from the particular font definition file (the declared
		encoding is used if not the value of
		|atp-variable-g:atp_font_encoding| is used).

		If <font_family> and <font_series> are defined then the
		<font_shape> for this font (in the above encoding) is found.
		

	bibstyle
		Completion for the command '\bibliographystyle{'. Finds all
		"bst" files avaiable in your tex distribution. 


	documentclass
		Completion for the command '\documentclass'. Returns list of
		all classes available in your distribution.

------------------------------------------------------------------
							*atp-completion-variables*
These are all variables which can help to customise the completion:
(if the value is given it is the default, if it is not means it is too long to
put it here).
>
 	g:atp_completion_limits		= [ '40', '60', '80', '100' ]
<
				The above variable specifies how long should
				atp plugin search for closed/unclosed environments:
				the first value	 - search for \(:\)  [ in line math ]
				the second	 - search for \[:\]  [ displayed math ]
				the third	 - search for \begin{<env>:\end{<env>	
				the fourth	 - for environments defined in
						   the variable g:atp_long_environments

				You can also put "-1" as the values of
				g:atp_completion_limits, then the search
				forward/backward will last till first/last
				line. However, this makes it run slower.
>
 	g:atp_long_environments 	= []
<	
				If some of your environments are very long put
				ther names in this list. Do not forget that is
				environment <env> is long and is put inside
				environment <center> then <center> is alos
				long!
				 
				However, this will not close long environments
				(for that you have to change the third
				argument of g:atp_completion_limits !). This
				just prevents closiong environments which are
				closed and to long to see that.
>
  	g:atp_completion_modes		= [ 
				\ 'commands', 		'inline_math', 
				\ 'displayed_math', 	'package_names', 
				\ 'tikz_libraries', 	'environment_names', 
				\ 'close_environments' ,'labels', 
				\ 'bibitems', 		'input_files',
				\ 'bibfiles',		'bibstyles',
				\ 'documentclass' ] 	
<				
				This is the list of completion modes.

g:atp_completion_active_modes	= g:atp_completion_modes
				This is the list of completion modes which are
				active, by default all modes are active. Remove
				a value from this list to make it inactive (You can
				use remove() command, see ':h remove()').  
>
    	g:atp_environments
    	g:atp_amsmath_environments
    	g:atp_shortname_dict
    	g:atp_separator			= ':'
    	g:atp_no_separator 		= 0
    	g:atp_env_short_names 		= 1
    	g:atp_no_separator_list		= ['', 'titlepage']
    	g:atp_commands
    	g:atp_math_commands
    	g:atp_ams_negations
    	g:atp_math_commands_non_expert_mode
    	g:atp_ams_negations_non_expert_mode
<				The two list of commands will be add only in
				ttphe non expert mode.
>
    	g:atp_amsmath_commands
    	g:atp_fancyhdr_commands
    	g:atp_tikz_environments
    	g:atp_tikz_libraries
    	g:atp_tikz_commands
	g:atp_completion_truncate	= 4
<
				do not complete commands less than
				4 characters (not counting the leading '\' if
				present). If 0 then complete all the defined
				commands. This only works in the expert mode.
								*atp-variable-g:atp_check_if_opened*
>
     	g:atp_check_if_opened	= 1     
<
				     (this checks if your are in some environments,
				     for example inside \begin{tikzpicture} or
					 \tikz{}, if 1 then the tikz commands will be
					 available only inside this environments)
								*atp-variable-g:atp_math_opened*
>
     	g:atp_math_opened		
<
				(the default is 1 if in your tex file there was no
				$ or $$ excluding \$, thus it will be one in any newly
				edited file!)
								*atp-variable-g:atp_math_modes*
>
     	let g:atp_math_modes	=[ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)(','\%([^\\]\|^\)\%(\\\|\\\{3}\))'],
				\ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)\[','\%([^\\]\|^\)\%(\\\|\\\{3}\)\]'], 	
				\ ['\\begin{align', '\end{align'], 		['\\begin{gather', '\\end{gather'], 
				\ ['\\begin{falign', '\\end{flagin'], 	['\\begin[multiline', '\\end{multiline'],
				\ ['\\begin{tikz', '\\end{tikz'],		['\begin{equation', '\end{equation'] ]
<				The first pattern of the 0th item matches '\(' and '\\\(' but
				not '\\('. Similarly for '\)', '\[', '\]'.  Remember
				that if you change this list the first term should
				correspond to '\(:\)' and the second to '\[:\]'. If
				you want to switch checking if cursors stands in math
				mode use g:atp_math_opened variable.
>
	g:atp_no_tab_map
	g:atp_no_complete		=['document']
<
				List of environments which is not closed by
				<tab> completion. (The document environment in longer
				documents can be not seen by the algorithm as closed,
				because it searches only in a part of the text, see
				g:atp_completion_limits variable above).
>
	g:atp_bracket_dict  	= { '(' : ')', '{' : '}', '[' : '] }
	g:atp_sizes_of_brackets = {'\left': '\right', 		'\bigl' : '\bigr', 
				 \ '\Bigl' : '\Bigr', 		'\biggl' : '\biggr' , 
				 \ '\Biggl' : '\Biggr', 	'\' : '\' }
<

================================================================================
OmniCompletion by David Munger (LatexBox plugin)	*atp-omnicompletion*

Completion is achieved through omni completion |compl-omni|, with default
bindings <CTRL-X><CTRL-O>. There are four types of completion:



------------------------------------------------------------------------------

							*atp-omnicompletion-commands*
Commands ~

Command completion is triggered by the '\' character.  For example, >
	\beg<CTRL-X><CTRL-O>
completes to >
	\begin{

Associated settings:
	|atp-variable-g:LatexBox_completion_commands|
	|atp-variable-g:LatexBox_completion_close_braces|


------------------------------------------------------------------------------

							*atp-omnicompletion-environments*
Environments ~

Environment completion is triggered by '\begin{'.  For example, >
	\begin{it<CTRL-X><CTRL-O>
completes to >
	\begin{itemize}

Completion of '\end{' automatically closes the last open environment.

Associated settings:
	|atp-variable-g:LatexBox_completion_environments|
	|atp-variable-g:LatexBox_completion_close_braces|


------------------------------------------------------------------------------

							*atp-omnicompletion-labels*
Labels ~

Label completion is triggered by '\ref{' or '\eqref{'.  For example, >
	\ref{sec:<CTRL-X><CTRL-O>
offers a list of all matching labels, with their associated value and page number.
Labels are read from the aux file, so label completion works only after
complilation.

It matches:
	1. labels: >
		\ref{sec:<CTRL-X><CTRL-O>
<	2. numbers: >
		\eqref{2<CTRL-X><CTRL-O>
<	3. labels and numbers together (separated by whitespace): >
		\eqref{eq 2<CTRL-X><CTRL-O>
	

Associated settings:
	|atp-variable-g:LatexBox_ref_pattern|
	|atp-variable-g:LatexBox_completion_close_braces|


------------------------------------------------------------------------------

							*atp-omnicompletion-bibtex*
BibTeX entries ~

BibTeX completion is triggered by '\cite{', '\citep{' or '\citet{'.
For example, assume you have in your .bib files an entry looking like: >

	@book {	knuth1981,
		author = "Donald E. Knuth",
		title = "Seminumerical Algorithms",
		publisher = "Addison-Wesley",
		year = "1981" }

Then, try: >

	\cite{Knuth 1981<CTRL-X><CTRL-O>
	\cite{algo<CTRL-X><CTRL-O>

You can also use regular expressions (or vim patterns) after '\cite{'.

Associated settings:
*atp-variable-g:LatexBox_cite_pattern*		Default: '\\cite\(p\|t\)\?\*\?\_\s*{'
*atp-variable-g:LatexBox_ref_pattern*		Default: '\\v\?\(eq\|page\)\?ref\*\?\_\s*{'

	Patterns to match \cite and \ref commands for BibTeX and label completion.
	Must include the trailing '{'.
	To match all commands that contain 'cite' (case insensitive), use: >
		let g:LatexBox_cite_pattern = '\c\\\a*cite\a*\*\?\_\s*{'
<	To match all commands that end with 'ref' (case insensitive): >
		let g:LatexBox_ref_pattern = '\c\\\a*ref\*\?\_\s*{'
<	Both examples match commands with a trailing star too.

*atp-variable-g:LatexBox_bibtex_wild_spaces*		Default: 1

	If nonzero, spaces act as wildcards ('.*') in completion.
	For example, if nonzero, >
		\cite{Knuth 1981
<	is equivalent to >
		\cite{Knuth.*1981

*atp-variable-g:LatexBox_completion_close_braces*	Default: 1

	If nonzero, omni completion will add closing brackets where relevant.
	For example, if nonzero, >
		\begin{itemize
<	completes to >
		\begin{itemize}

*atp-variable-g:LatexBox_completion_environments*
*atp-variable-g:LatexBox_completion_commands*

	Static completion lists for environments
	|atp-omnicompletion-environments| and commands
	|atp-omnicompletion-commands|.

================================================================================
How to configure ATP to your needs                      *atp-configure*
							*atp-variables*

There are several options you can set, and they might be set in your Vimrc
file. The default values are given below (except the completion setup and
bibtex documented above).

Tip: If you want to see (almost) all the variables, type ':let g:atp-<CTRL+d>',
and ':let b:<CTRL+d>'.

All buffer variables (see |b:var|), i.e. these which name begins with "b:",
should be set in your Vimrc file. The best way to do that is by using
autocommand:
>
	au BufReadPre *.tex let b:atp_TexCompiler="latex"
<
If you put just let b:atp_TexCompiler, this will also work but not always: for
example when you open a new buffer in existing Vim session.

let b:atp_TexCompiler="pdflatex" 			*atp-variable-b:atp_TexCompiler*
	Used by functions: TEX() (map \l, imap \l), VTEX() (map <F5>, imap <F5>)

	You can set it to latex, tex, luatex, and so on and possibly to
	lilypond as well. 

let b:atp_TexOptions=""
	If you want to set some additional options to your tex compiler you can
	use this variable, note that -output-directory, and -mode, are
	already used. You can use this to make reverse searching with xdvi see
	|atp-xdvi|.

							*atp-variable-b:atp_OutDir*
let b:atp_OutDir=fnameescape(fnamemodify(resolve(expand("%:p")),":h")) . "/"
Used by ViewOutput(), TEX(), VTEX(), BibTeX(), TexLog(), Pdffonts(), Delete() 
			i.e. in all functions.

	This is the directory in which tex will put the output files. If the
	open file is not a symbolic link it is equal to the directory in which
	the tex file is located. If the open file is a symbolic link it points
	to the directory in which the real file is located. 
	
	If you set this variable to './' (or '.') and change the current
	working directory for example to /tmp (:cd /tmp) then the latex output
	will be placed in /tmp, i.e. it will move with with cd. However, the
	default value of b:atp_OutDir is not affected by :cd command.

	White spaces and other characters should not be escaped. It will be
	quoted in '...' using the |shellescape()| function.

	You can see the current output directory in the status (it is in the
	short notation) to see it whole type:
		:echo b:atp_OutDir
	or use the function ShowOptions() (see |apt-command-ShowOptions|).		

	If in your environment the variable $TEXMFOUTDIR is set the value of
	b:atp_OutDir will be set to its value.

let b:atp_auruns=1					*atp-variable-b:atp_auruns*
	This variable control how many times the automatic function calls tex
	compiler (consecutively). It is useful if you are working with PDF
	files and you want to have bookmarks (you can get them using hyperref
	package with the option: bookmarks. Then set b:atp_auruns to '2'.

let g:atp_autex_check_if_closed=1  			*atp-variable-g:atp_autex_check_if_closed*
	This feature is not implemented.
	tex run if all environments \begin:\end, \(:\) and \[:\] are closed.
	Set g:atp_autex_check_if_closed=0 in order to not make the checks.

let g:texmf=$HOME/texmf					*atp-variable-g:texmf*
	This variable configures where input files are placed. See
	|atp-command-EditInputFile|.

let g:askforoutdir=0					*atp-variable-g:askforoutdir*
	Its values are 1 and 0.  When it is set to 1 you will be asked for the
	name of a directory where tex will put output files, note that this
	name should end with a "/".

let b:atp_Viewer="xpdf"					*atp-variable-b:atp_Viewer*
    							*atp-variable-g:{b:Viewer}Options*
	it was tested with xpdf, evince, epdfviewer, kpdf, okular, xdvi and
	they all works fine. I'm using xpdf and the xpdf server options are
	supported so that the file is automatically reloaded (other viewers,
	except epdfview, have this functionality as well. This do not works
	for acroread. Read more about viewers in |atp-viewers|. 

	If you use program b:Viewer then you can use the variable
	g:{b:Viewer}Options to set the options, for example if b:Viewer="xpdf"
	then you might use:

    g:xpdfOptions					*atp-variable-g:xpdfOptions*
	Used by function: ViewOutput() (map \v, map <F3>, imap <F3>)

	For example, if you want to have different look of one document you can
	set it to "-bg gray20". Other example:
>
 	let g:XpdfOptions="-bg Grey30 -mattecolor SlateBlue2 -papercolor White"
<
	This variable will be copied to b:atp_ViewerOptions, so if you want to
	make changes on the fly use this variable. You can also set it
	directly using autocommand:
>
    au BufRead *.tex let b:atp_ViewerOptions="-bg NavajoWhite4 -fg black -mattecolor burlywood"
<
 
let b:XpdfServer=fnamemodify(expand("%"),":t")		*atp-variable-g:atp_XpdfServer*	
	Used by function: ViewOutput() (map \v, map <F3>, imap <F3>)

	It is equal to the name of the source file. You do not need escape
	spaces in the name (shellescape() function is used before it is send
	to the shell).

let b:atp_OpenViewer=1					*atp-variable-b:atp_OpenViewer*
	If the function which calls TeX compiler do not see that you are
	viewing the output file it will open it for you if b:atp_OpenViewer=1.
	Otherwise, this feature is disabled.

let g:rmcommand="perltrash"				*atp-variable-g:rmcommand*
	Used by function: Delete() (map <F6>d imap <F6>d)	

	If you have another 'trash can' program you can use it here, if you do
	not have it you can use "rm" (at your own risk). It is used to delete
	the files produced by (La)TeX (see |apt-Delete()|). The function
	Delete() will remove all files in the output directory (see
	|atp-variable-b:atp_OutDir|), which ends with an extension defined in the list
	|atp-variable-g:atp_tex_extensions|. If you set:
>
	let g:rmcommand=''
<
	then the function Delete() (see |apt-Delete()|) will use the Vim
	|delete()| command, and will delete only the files produced by the
	current '.tex' file. The temporary directory is cleared by rm command.

	The program 'perltrash' is in the package app-misc/perltrash (at least
	for Gentoo).

let g:atp_delete_output=0
	If set to 1 then Delete function (map <F6>d) will delete also the
	output file.	

							*atp-variable-g:atp_tex_extensions*	
let g:atp_tex_extensions=["aux", "log", "bbl", "blg", "spl", "snm", "nav", "thm", "brf", "out", "toc", "mpx", "idx", "maf", "blg", "glo", "mtc[0-9]", "mtc1[0-9]"]	
	 This list is used by the function Delete() (see |apt-Delete()|) which
	 deletes all the files with the specified extension in the directory
	 b:atp_OutDir, unless g:rmcommand="" (see |atp-variable-g:rmcommand|) in which case
	 Delete() deletes only the output files for the current buffer.
									
let g:keep=["log","aux","toc","bbl"]			*atp-variable-g:keep*
	Files with an extension belonging to this list will be copied from
	'b:atp_OutDir' to the temporary directory with appropriate name. Then it
	will be used by (La)TeX. (log file will be copied after it is created,
	other files will be copied back and forth between 'b:atp_OutDir' and the
	temporary directory). These four elements: log,aux,toc,bbl are
	essentially minimum to work with: table of contents, pdf-bookmarks and
	bibtex. There are possibly other classes, like beamer, or packages
	like theorem (produce .thm files) which will need to configure this
	variable.

	You can change this variable by the command:
		:let g:keep+=["thm","spl"]
								
let g:printeroptions=""					*atp-variable-g:printeroptions*
	You can set the printer options. These are options for the 'lpr'
	command, which will print the output file (pdf or dvi) this depends on
	the b:atp_TexCompiler that you use.

b:texcommand						*atp-variable-b:texcommand*
	This variable is for debugging. It stores the last executed command to
	compile your document. This changes also when your compiler was run
	automatically.
>
		:TEX
		:echo b:texcommand
		:TEX 2
		:echo b:texcommand
<		
		
g:defaultbibflags		see |atp-bibflags:default|
g:defaultallbibflags		see |atp-bibflags:all|
b:atp_LastBibFlags		see |atp-bibflags:last|

b:bibfiles			see |atp-variables-bib|
s:bibfiles
s:allbibfiles
s:notreadablebibfiles
	For more on bib flags see |atp-bibflags|.
	
let t:toc_window_width=30
	g:toc_window_width (by default not set, if set overrides
	t:toc_window_width)
	Configures the initial width of the window with table of contents.

let t:labels_window_width=30
	g:labels_window_width (by default not set, if set overrides
	t:labels_window_width)
	Configures the initial width of the window with labels.

g:atp_statusline
	by default it is not set, put the line
>
	let g:atp_statusline=0
<
	in your $VIMRC file if you do not want the status line provided by this
	plugin. (See |atp-command-ATPStatus|).

let b:atp_TruncateStatuSection=40
	This variable sets how many characters of the section/subsection title
	(or chapter/section titles if you write a book) should be shown in the
	status line.  Section title and subsection title gets equal amount of
	characters.

================================================================================
Maps and Commands                     			*atp-maps*

Lots of mappings which are given here uses #. This is a convenient map on
British keyboards, but not in the US layout, you can change them for '`' or
some other key that it is not used in Vim (there are not many of them though).
The most commonly used latex-suite plugin uses similar set of mappings (but
there might be some differences). The easy way to change imap leaders is by
using the variables:

	    g:atp_imap_first_leader == "#"
	    	for Greak letters,
	    g:atp_imap_second_leader == "##"
	    	for font commands,
	    g:atp_imap_third_leader == "]"
	    	for environments,  
	    g:atp_imap_fourth_leader == "["
	    	for extra environments in the old layout.
All other mappings (map, vmap, nmap, ...) are using <LocalLeader> which can be
altered with the option |maplocalleader|. A good alternate solution is to use "_"
instead of "##".

Maps are using the <buffer> option thus are local to the buffer. To unmap you
also have to use this option, for example to unmap \l issue the command:
>
	:unmap <buffer> <LocalLeader>l
<

The maps are loaded unless you set one of variables: 'g:no_plugin_maps' or
'g:no_atp_maps' (disables maps defined in tex_atp.Vim), 'g:no_atp_toc_maps'
(disables maps defined in 'toc_atp.Vim'),  'g:atp_no_env_maps' (disables the
environment maps '[*', ']*') or 'g:atp_no_tab_map' (disables the tab map
for completion, then completion is mapped to <F7> and <S-F7> (for the non
expert mode) but there is no map for 'WrapSelection()', you have to provide 
one by your self).

Note: in all mappings "\" is set to your <LocalLeader> (and thus, in fact, the
map can differ).

ShowOptions()						
:ShowOptions, :ShowOptions v

:TEX
map \l,imap \l

:VTEX
map <F5>,imap <F5>, :VTEX 

ViewOutput
map \v,map <F3>, imap \v, imap <F3>  

:Bibtex		/run also latex/
map \b

:SBibtex 	/Simple Bibtex - run only bibtex/
map \sb

:OpenLog
map <F6>l, imap <F6>l

:Delete
map <F6>d
	Deletes all the files with an extension which belongs to
	g:atp_tex_extensions in the directory b:atp_OutDir. By default
	g:atp_tex_extensions does not contain '.tex', '.pdf', '.dvi' so none
	of your important files will be deleted. If you set
	g:atp_delete_output=1 the function will delete also the current output
	file (but not any other!).

:SShPrint
map \p
	This calls the function Printer(g:printeroptions) (see |Print()|).


:TOC
map \t
	This is a mapping to the command ':TOC'


:Labels
map \L
	This is a mapping to the command ':Labels'


TeXdoc:
<F1> 	   
	This is both map and imap. Then you have to type what you are looking
	for and press enter. The option 'keywordprg' is set to 'texdoc -m',
	i.e when your cursor is over a package name and you press 'K' key
	then you should see the package document file (if it is named
	after the package).

	Without any argument it will open "g:atp_TeXdocDefault", by default it
	is eqaul to "-a lshort", i.e. "The not so short introduction to LaTeX
	2e" by Tobias Oetiker. You can change the default for something that
	you use more often, for example you can set it to "-a faq", i.e. 'The
	UK TeX FAQ' (or even to "-a lshort faq" if you want them both :). 

pdffonts is mapped to <F6>g	
There is also a command ':PdfFonts' which does the same. 

" FONT COMMANDS
imap ##rm \textrm{}<Left>
imap ##it \textit{}<Left>
imap ##sl \textsl{}<Left>
imap ##sf \textsf{}<Left>
imap ##bf \textbf{}<Left>
	
imap ##mit \mathit{}<Left>
imap ##mrm \mathrm{}<Left>
imap ##msf \mathsf{}<Left>
imap ##mbf \mathbf{}<Left>

							*atp-map-greek-letters*
" GREEK LETTERS
imap #a \alpha
imap #b \beta
imap #c \chi
imap #d \delta
imap #e \epsilon
imap #f \phi
imap #y \psi
imap #g \gamma
imap #h \eta
imap #k \kappa
imap #l \lambda
imap #i \iota
imap #m \mu
imap #n \nu
imap #p \pi
imap #o \theta
imap #r \rho
imap #s \sigma
imap #t \tau
imap #u \upsilon
imap #vs \varsigma
imap #vo \vartheta
imap #w \omega
imap #x \xi
imap #z \zeta

Not all upper Greek letters are in LaTeX:
imap #D \Delta
imap #Y \Psi
imap #F \Phi
imap #G \Gamma
imap #L \Lambda
imap #M \Mu
imap #N \Nu
imap #P \Pi
imap #O \Theta
imap #S \Sigma
imap #T \Tau
imap #U \Upsilon
imap #V \Varsigma
imap #W \Omega

								*atp-map-environments*
imap ]b \begin{}<Left>
imap ]e \end{}<Left>
imap ]c \begin{center}<Cr>\end{center}<Esc>O

imap ]d \begin{definition}<Cr>\end{definition}<Esc>O
imap ]t \begin{theorem}<Cr>\end{theorem}<Esc>O
imap ]P \begin{proposition}<Cr>\end{proposition}<Esc>O
imap ]l \begin{lemma}<Cr>\end{lemma}<Esc>O
imap ]r \begin{remark}<Cr>\end{remark}<Esc>O
imap ]o \begin{corollary}<Cr>\end{corollary}<Esc>O
imap ]p \begin{proof}<Cr>\end{proof}<Esc>O
imap ]x \begin{example}<Cr>\end{example}<Esc>O
imap ]n \begin{note}<Cr>\end{note}<Esc>O

imap ]e \begin{enumerate}<Cr>\end{enumerate}<Esc>O
imap ]i \begin{itemize}<Cr>\end{itemize}<Esc>O
imap ]I \item

imap ]a \begin{align}<Cr>\end{align}<Esc>O
imap ]q \begin{equation}<Cr>\end{equation}<Esc>O

imap ]l \begin{flushleft}<Cr>\end{flushleft}<Esc>O
imap ]R \begin{flushright}<Cr>\end{flushright}<Esc>O

imap ]z \begin{center}<CR>\begin{tikzpicture}<CR><CR>\end{tikzpicture}<CR>\end{center}<Up><Up>
imap ]f \begin{frame}<Cr>\end{frame}<Esc>O

								*atp-map-other*
These are very useful mappings for typing mathematics:
imap __ _{}<Left>
imap ^^ ^{}<Left>
imap ]m \[\]<Left><Left>

================================================================================
Error Handling							*atp-errors*
	
This plugins sets the option 
>
	set errorfile=  log file in your b:atp_OutDir directory
<
This allows you to use |quickfix| commands, for example to read the error file
use :cg (see |cg|), to jump to the first error :cf (see |cg|), to list all
errors :cl (see |cl|), read |errorformat| if you want to change the output of
this commands.

								*atp-errors-bibsearch*
A possible error which may occur using the :BibSearch commands has a simple
cause: we count number of brackets '()', '{}' and '"' (but nor '\"') to see
where the bib entry ends and where to join lines. The message error is echoed
when more than 30 lines where processed and the matching bracket was not found
(or the number of '"' is odd). Look at your bib file at the specified
position.  Syntax highlighting for bib files can help you finding where such
an error is located. (After reading the bib file comment lines and lines which
begin with @string are removed, so that the brackets in comment lines do not
count.)

================================================================================
Requirements							*atp-requirements*

This plugin requires Vim version higher than 7. 

It is nice to have 'texdoc' program. This plugin maps <F1> to a function which
calls it. This allows to speed up searches for documentation. Also the option
'keywordprg' has the value "texdoc -m", thus pressing 'K' (see |K|) over a tex
package should open you the package documentation. The same applies to this
help file.

Another good programs are texloganalyzer (which is now not used) and pdffonts
There is a map to use pdffonts, see: |pdffonts|.

================================================================================
Notes on Viewers                               			*atp-viewers*

xpdf								*atp-viewers-xpdf*
	It is fully supported. It is configured in the way that when your tex
	file have errors, xpdf viewer will not reload your file, which I found
	useful. 

	You can set your own options of xpdf using b:XpdfOptions, for example
>
	    let b:XpdfOptions="-bg NavajoWhite4 -fg black -mattecolor burylwood"
<
	will make xpdf view different. This is helpful when you edit to
	files, and do not want to xpdf mix them. Another example:
>
	    let b:XpdfOptions="-bg Grey30 -mattecolor SlateBlue2 -papercolor White"
<
evince
	Works fine.
okular
kpdf
	Works fine (moves page a little bit when updates a file).
epdfview
	This viewer does not support automatic reloads when the file changes
	(but it seems that the work is in progress). You have to issue CTRL-R
	yourself when the file is changed.
acroread
	As with epdfview (with the difference that it supports automatic
	updates, but it do not works somehow)
								
xdvi							*atp-viewers-xdvi*
							*atp-viewers-xdvi-reverse/inverse-searching*
	Works fine. The file will be updated after a click (or use the xdvi
	options '-watchfile 1' see man xdvi for explanations). You can set
	inverse/reverse searching by the command |SetXdvi|. It is recommended
	to use gVim rather than Vim, if you want to use Vim you have to run it
	with the command:
>
 "	Vim --servername xdvi <file>
<
	You can pick any server name.
	
	The command SetXdvi defines a new function:

RevSearch()							*atp-command-RevSearch*
map \is, :RevSearch
	Which makes an reverse search (sets the xdvi position according to the
	cursor position in gVim).

	Here I describe how inverse/reverse searching is done. 
	
    (1) Inverse searching
	(i.e. position Vim's cursor after xdvi event:
	usually CTRL+Left Mouse) with this options:
>
	let b:atp_TexCompiler="latex"
	let b:atp_TexOptions="-src-specials"
	let b:Viewer="xdvi -editor 'gVim --remote-wait +%l %f'"
<
	See Vim tip at: http://Vim.wikia.com/wiki/Vim_can_interact_with_xdvi 	

    (2) Reverse searching
	For reverse searching (position xdvi according to the Vim's cursor
	position) you can set:
>
	let b:reverse_search="xdvi -sourceposition " . line(".") . ":" . col(".") . fnamemodify(expand("%"),":p") . " " . fnamemodify(expand("%"),":p:r") . ".dvi"
<
	To make an reverse search:
>
		:call system(b:reverse_search)
<
	And xdvi will place itself at the current cursor position in the 'tex'
	source file. You can make a map for this. 

	To use this with Vim one have to add server name. Run
	Vim as:
>
 	Vim --servername Vimtex
 	let b:Viewer="xdvi -editor 'Vim --servername " . v:servername . " --remote-wait +%l %f'"
 	let b:reverse_search="xdvi -editdor 'Vim --servername " . v:servername "' -sourceposition " . line(".") . ":" . col(".") . fnamemodify(expand("%"),":p") . " " . fnamemodify(expand("%"),":p:r") . ".dvi"
<

	In case of troubles:

	Reverse Searching:
	    If reverse searching do not works for you, and you get an error, that
	    there is no reference to your tex file in the dvi file, then open
	    your dvi file in an editor (Vim :) and check what is the name after
	    'src:<line number>' (this are the source specials put by 'latex
	    -src-specials' command). It should refer to your tex file. Xdvi
	    will not recognize if you specify the full name of the tex file (i.e.
	    with path) in the b:reverse_search (as we do above using the
	    modifier :p and the function fnamemodify in
	    'fnamemodify(expand("%"),":p")' the other one with ":p:r" is OK!)
	    and in the dvi file there is written just the name without the
	    path (and vice versa, if you give just the name in
	    b:reverse_search and in the file there is full path).

================================================================================
Color highliting and syntax groups				*atp-color-highlight*

There is a color scheme included: coots-beauty-256 you need 256 colors to use
it. These are the highlights groups defined for various files:

1) ToC file
    highlight atp_linenumber
    highlight atp_number
    highlight atp_chapter
    highlight atp_section
    highlight atp_subsection
    highlight atp_abstract
	*this group highlights abstrac, all the unnubered chapters and
	bibliography.

    The chapter group highlights or chapters, or sections, or parts, depending
    what is your top level section in your latex document. This applies,
    accordingly, to other groups.

2) Labels file
    highlight atp_label_filename
    highlight atp_label_linenr
    highlight atp_label_name 

3) BibSearch file
    this is very much the same as the standard syntax for bib files. Groups
    are named bibsearch<NAME> instead of bib<NAME>. There is one more group
    added:
>
	    bibsearchInfo
<
    which highlights the line number of the bib entry in the bib file.  All
    bibsearch groups are by default linked to the bib groups.

    Yet, there is no default highlighting, try coots-beauty-256 color scheme.
    If you like it, I'm glad, if you have a nice (non standard) color scheme,
    I'm happy to get it, if you like to share it.

================================================================================
Final Remarks                               			*atp-remarks*
	
	To see some messages that are issued you can use :messages command
	(see |:mes|).

	If you find this plugin useful and have some comments you are
	cordially invited to write to the author: <mszamot@gmail.com>.

	There are other ways to make such an automatic plugin. The crucial
	step is to make the plugin know that tex is already in use (if you run
	to tex compilers on the same file at the same time the effect won't be
	good).

	For several month I was using a different code which was not using
	temporary copies but was checking if the tex is running or not. It was
	working very good but I didn't understand why (silly isn't it), and
	when I started making changes it stopped working: the issue is that
	it is difficult to make a function sleep until tex stops working not
	putting whole Vim into sleep - this is the time that we want to save.
	However, the advantage of using temporary files is smoothness (changes
	appear faster in the output file). 

	Best regards, and hopefully you will find this useful :) 
	Marcin Szamotulski
	
	
================================================================================
Copy Rights							*atp-copy-rights*


" Copyright:    Copyright (C) 2010 Marcin Szamotulski Permission is hereby
"		granted to use and distribute this code, with or without
"		modifications, provided that this copyright notice is copied
"		with it. Like anything else that's free, Automatic TeX Plugin
"		is provided *as is* and comes with no warranty of any kind,
"		either expressed or implied. By using this plugin, you agree
"		that in no event will the copyright holder be liable for any
"		damages resulting from the use of this software.


vim:tw=75:ts=8:ft=help:norl:
doc/latexhelp.txt	[[[1
2430
*latexhelp.txt*    For Vim version 6.0.  Last change: 2001 Dec 20


				LATEX HELP 1.6  
		   translated (with minor changes) for vim
			     by Mikolaj Machowski

This file documents LaTeX2e, a document preparation system. LaTeX2e is a
macro package for TeX.

  This is edition 1.6 of the LaTeX2e documentation, and is for the Texinfo
that is distributed as part of Version 19 of GNU Emacs. It uses version
2.134 or later of the texinfo.tex input file.

  This is translated from LATEX.HLP v1.0a in the VMS Help Library.  The
pre-translation version was written by George D. Greenwade of Sam Houston
State University.

  The LaTeX 2.09 version was written by Stephen Gilmore <stg@dcs.ed.ac.uk>.

  The LaTeX2e version was adapted from this by Torsten Martinsen
<bullestock@dk-online.dk>.

  Version for vim of this manual was written by Mikolaj Machowski
<mikmach@wp.pl>

  Copyright 1988,1994 Free Software Foundation, Inc.  Copyright 1994-1996
Torsten Martinsen. Copyright for `translation' for vim Mikolaj Machowski 2001.

  Permission is granted to make and distribute verbatim copies of this manual
provided the copyright notice and this permission notice are preserved on
all copies.

  Permission is granted to copy and distribute modified versions of this
manual under the conditions for verbatim copying, provided that the entire
resulting derived work is distributed under the terms of a permission
notice identical to this one.

  Permission is granted to copy and distribute translations of this manual
into another language, under the above conditions for modified versions,
except that the sections entitled "Distribution" and "General Public
License" may be included in a translation approved by the author instead of
in the original English.

==============================================================================
*LaTeX* *latex*

The LaTeX command typesets a file of text using the TeX program and the LaTeX
Macro package for TeX. To be more specific, it processes an input file
containing the text of a document with interspersed commands that describe how
the text should be formatted.

1.  Commands					|latex-commands|
2.  Counters					|latex-counters|
3.  Cross References				|latex-references|
4.  Definitions					|latex-definitions|
5.  Document Classes				|latex-classes|
6.  Layout					|latex-layout|
7.  Environments				|latex-environments|
8.  Footnotes					|latex-footnotes|
9.  Lengths					|latex-lengths|
10. Letters					|latex-letters|
11. Line & Page Breaking			|latex-breaking|
12. Making Paragraphs				|latex-paragraphs|
13. Margin Notes				|latex-margin-notes|
14. Math Formulae				|latex-math|
15. Modes					|latex-modes|
16. Page Styles					|latex-page-styles|
17. Sectioning					|latex-sectioning|
18. Spaces & Boxes				|latex-spaces-boxes|
19. Special Characters				|latex-special-char|
20. Splitting the Input				|latex-inputting|
21. Starting & Ending				|latex-start-end|
22. Table of Contents				|latex-toc|
23. Terminal Input/Output			|latex-terminal|
24. Typefaces					|latex-typefaces|
25. Parameters					|latex-parameters|

==============================================================================
1. Commands					*latex-commands*

A LaTeX command begins with the command name, which consists of a \ followed
by either
	(a) a string of letters or
	(b) a single non-letter.

Arguments contained in square brackets, [], are optional while arguments
contained in braces, {}, are required.

NOTE: LaTeX is case sensitive. Enter all commands in lower case unless
explicitly directed to do otherwise.

==============================================================================
2. Counters					*latex-counters*

|\addtocounter|		Add a quantity to a counter
|\alph|			Print value of a counter using letters
|\arabic|		Print value of a counter using numerals
|\fnsymbol|		Print value of a counter using symbols
|\newcounter|		Define a new counter
|\refstepcounter|	Add to counter, resetting subsidiary counters
|\roman|		Print value of a counter using roman numerals
|\setcounter|		Set the value of a counter
|\stepcounter|		Add to counter, resetting subsidiary counters
|\usecounter|		Use a specified counter in a list environment
|\value|		Use the value of a counter in an expression

Everything LaTeX numbers for you has a counter associated with it. The name of
the counter is the same as the name of the environment or command that
produces the number, except with no |\\|. (|lc-enumi| - |lc-enumiv| are used
for the nested |\enumerate| environment.) Below is a list of the counters
used in LaTeX's standard document classes to control numbering.

 |part|          |paragraph|     |figure|      |enumi|    |itemi|
 |chapter|       |subparagraph|  |table|       |enumii|   |itemii|
 |section|       |page|          |footnote|    |enumiii|  |itemiii|
 |subsection|    |equation|      |mpfootnote|  |enumiv|   |itemiv|
 |subsubsection|


\addtocounter{counter}{value}			*\addtocounter*
		Increments the {counter} by the amount specified by the
		{value} argument. The {value} argument can be negative.

\alph{counter}					*\alph* *\Alph*
\Alph{counter}
		This command causes the value of the counter to be printed in
		alphabetic characters. |\alph| command uses lower case
		alphabetic alphabetic characters, i.e., a, b, c... while the
		|\Alph| command uses upper case alphabetic characters, i.e.,
		A, B, C....

\arabic{counter} 				*\arabic*
		Causes the value of the {counter} to be printed in Arabic
		numbers, i.e., 3.

\fnsymbol{counter} 				*\fnsymbol*
		Causes the value of the {counter} to be printed in a specific
		sequence of nine symbols that can be used for numbering
		footnotes.
		Note: counter must have a value between 1 and 9 inclusive.

\newcounter{foo}[counter] 			*\newcounter*
		Defines a new counter named {foo}. The counter is initialized
		to zero.  The optional argument [counter] causes the counter
		{foo} to be reset whenever the counter named in the optional
		argument is incremented.

\refstepcounter{counter}			*\refstepcounter*
		Command works like |\stepcounter|, except it also defines the
		current |\ref| value to be the result of \thecounter.

\roman{counter} 				*\roman* *\Roman*
\Roman{counter}
		Causes the value of the {counter} to be printed in Roman
		numerals.  The |\roman| command uses lower case Roman numerals,
		i.e., i, ii, iii..., while the |\Roman| command uses upper case
		Roman numerals, i.e., I, II, III....

\stepcounter{counter}				*\stepcounter*
		Adds one to the {counter} and resets all subsidiary counters.

\setcounter{counter}{value}			*\setcounter*
		Sets the value of the {counter} to that specified by the
		{value} argument.

\usecounter{counter} 				*\usecounter*
		Command is used in the second argument of the |list|
		environment to allow the {counter} specified to be used to
		number the list items.

\value{counter} 				*\value*
		Produces the value of the {counter} named in the mandatory
		argument. It can be used where LaTeX expects an integer or
		number, such as the second argument of a |\setcounter| or
		|\addtocounter| command, or in: >
			\hspace{\value{foo}\parindent}
<		It is useful for doing arithmetic with counters.

==============================================================================
3. Cross References				*latex-references*

One reason for numbering things like figures and equations is to refer the
reader to them, as in "See Figure 3 for more details."

|\label|		Assign a symbolic name to a piece of text
|\pageref|		Refer to a page number
|\ref|			Refer to a section, figure or similar


\label{key} 					*\label*
		Command appearing in ordinary text assigns to the {key} the
		number of the current sectional unit; one appearing inside a
		numbered environment assigns that number to the {key}.

		A {key} can consist of any sequence of letters, digits, or
		punctuation characters. Upper and lowercase letters are
		different.

		To avoid accidentally creating two labels with the same name,
		it is common to use labels consisting of a prefix and a suffix
		separated by a colon. The prefixes conventionally used are
			* 'cha' for chapters
			* 'sec' for lower-level sectioning commands
			* 'fig' for figures
			* 'tab' for tables
			* 'eq'  for equations
		Thus, a label for a figure would look like: >
			\label{fig:bandersnatch}

\pageref{key} 					*\pageref*
		Command produces the page number of the place in the text
		where the corresponding |\label| command appears.  ie. where
		\label{key} appears.

\ref{key}					*\ref*
		Command produces the number of the sectional unit, equation
		number, ... of the corresponding |\label| command.

==============================================================================
4. Definitions					*latex-definitions*

|\newcommand| 		Define a new command
|\newenvironment| 	Define a new environment
|\newtheorem| 		Define a new theorem-like environment
|\newfont| 		Define a new font name


\newcommand{cmd}[args]{definition}		*\newcommand* *\renewcommand*
\newcommand{cmd}[args][default]{definition}
\renewcommand{cmd}[args]{definition}
\renewcommand{cmd}[args][default]{definition}

These commands define (or redefine) a command.

{cmd}		A command name beginning with a |\\|. For |\newcommand| it must
		not be already defined and must not begin with |\end|; for
		|\renewcommand| it must already be defined.

{args}		An integer from 1 to 9 denoting the number of arguments of the
		command being defined. The default is for the command to have
		no arguments.

{default}	If this optional parameter is present, it means that the
		command's first argument is optional. The default value of the
		optional argument is default.

{definition}	The text to be substituted for every occurrence of {cmd}; a
		parameter of the form #n in {cmd} is replaced by the text of
		the nth argument when this substitution takes place.

       					*\newenvironment* *\renewenvironment*
\newenvironment{nam}[args]{begdef}{enddef}
\newenvironment{nam}[args][default]{begdef}{enddef}
\renewenvironment{nam}[args]{begdef}{enddef}

These commands define or redefine an environment.

{nam} 		The name of the environment. For |\newenvironment| there must
		be no currently defined environment by that name, and the
		command \nam must be undefined.  For |\renewenvironment| the
		environment must already be defined.

{args}		An integer from 1 to 9 denoting the number of arguments of
		the newly-defined environment. The default is no arguments.

{default} 	If this is specified, the first argument is optional, and
		default gives the default value for that argument.

{begdef} 	The text substituted for every occurrence of \begin{nam}; a
		parameter of the form #n in {cmd} is replaced by the text of
		the nth argument when this substitution takes place.

{enddef} 	The text substituted for every occurrence of \end{nam}. It
		may not contain any argument parameters.


\newtheorem{envname}{caption}[within]			*\newtheorem*
\newtheorem{envname}[numberedlike]{caption}

This command defines a theorem-like environment.

{envname}	The name of the environment to be defined. A string of
		letters. It must not be the name of an existing environment or
		counter.

{caption}	The text printed at the beginning of the environment, right
		before the number. This may simply say "Theorem", for example.

{within}	The name of an already defined counter, usually of a sectional
		unit. Provides a means of resetting the new theorem counter
		within the sectional unit.

{numberedlike}	The name of an already defined theorem-like environment.

The |\newtheorem| command may have at most one optional argument.


\newfont{cmd}{fontname} 				*\newfont*
		Defines the command name {cmd}, which must not be currently
		defined, to be a declaration that selects the font named
		{fontname} to be the current font.

==============================================================================
5. Document Classes				*latex-classes*


\documentclass[options]{class}			*\documentclass*

Valid LaTeX document classes include:
	*article		*article-class*
	*report			*report-class*
	*letter			*letter-class*
	*book			*book-class*
	*slides			*slides-class*

All the standard classes (except slides) accept the following options for
selecting the typeface size (10 pt is default):

10pt, 11pt, 12pt

All classes accept these options for selecting the paper size (default is
letter):

a4paper, a5paper, b5paper, letterpaper, legalpaper, executivepaper

Miscellaneous options:

landscape 					*landscape*
	Selects landscape format. Default is portrait.

titlepage, notitlepage				*notitlepage*
		Selects if there should be a separate title page.

leqno						*leqno* *rqno*
		Equation number on left side of equations.  Default is
		right side.

fleqn						*fleqn*
		Displayed formulas flush left.  Default is centred.

openbib						*openbib*
		Use "open" bibliography format.

draft, final					*draft* *final*
		Mark/do not mark overfull boxes with a rule. Default is
		final.

These options are not available with the slides class:

oneside, twoside				*oneside* *twoside*
		Selects one- or twosided layout. Default is oneside,
		except for the book class.

openright, openany				*openright* *openany*
		Determines if a chapter should start on a right-hand page.
		Default is openright for book.

onecolumn, twocolumn				*onecolumn* *twocolumn*
		One or two columns.  Defaults to one column.

The slides class offers the option clock for printing the time at the bottom
of each |\note|.

If you specify more than one option, they must be separated by a comma.

\usepackage[options]{pkg} 			*\usepackage*
		Additional packages are loaded by this. If you
		specify more than one package, they must be separated by a
		comma.

Any options given in the |\documentclass| command that are unknown by the
selected document class are passed on to the packages loaded with |\usepackage|.

==============================================================================
6. Layout					*latex-layout*

Miscellaneous commands for controlling the general layout of the page.

|\flushbottom|		Make all text pages the same height.
|\onecolumn| 		Use one-column layout.
|\raggedbottom| 	Allow text pages of differing height.
|\twocolumn| 		Use two-column layout.

\flushbottom					*\flushbottom*
		Makes all text pages the same height, adding extra vertical
		space when necessary to fill out the page.  This is the
		standard if twocolumn mode is selected.

\onecolumn					*\onecolumn*
		Starts a new page and produces single-column output.

\raggedbottom					*\raggedbottom*
		Makes all pages the height of the text on that page.  No extra
		vertical space is added.

\twocolumn[text]				*\twocolumn*
		Starts a new page and produces two-column output.  If the
		optional [text] argument is present, it is typeset in
		one-column mode.

==============================================================================
7. Environments					*latex-environments*

						*\begin* *\end*
LaTeX provides a number of different paragraph-making environments. Each
environment begins and ends in the same manner: >

	\begin{environment-name}
	.
	.
	.
	\end{environment-name}
<
a. |array| 		Math arrays
b. |center| 		Centred lines
c. |description| 	Labelled lists
d. |enumerate|		Numbered lists
e. |eqnarray| 		Sequences of aligned equations
f. |equation| 		Displayed equation
g. |figure| 		Floating figures
h. |flushleft| 		Flushed left lines
i. |flushright| 	Flushed right lines
j. |itemize| 		Bulleted lists
k. |letter| 		Letters
l. |list| 		Generic list environment
m. |minipage| 		Miniature page
n. |picture| 		Picture with text, arrows, lines and circles
o. |quotation| 		Indented environment with paragraph indentation
p. |quote-l| 		Indented environment with no paragraph indentation
q. |tabbing| 		Align text arbitrarily
r. |table| 		Floating tables
s. |tabular| 		Align text in columns
t. |thebibliography| 	Bibliography or reference list
u. |theorem| 		Theorems, lemmas, etc
v. |titlepage| 		For hand crafted title pages
x. |verbatim| 		Simulating typed input
y. |verse| 		For poetry and other things

==============================================================================
 a. array					*array*
>
	\begin{array}{col1col2...coln}
		column 1 entry & column 2 entry ... & column n entry \\
		.
		.
		.
	\end{array}

Math arrays are produced with the |array| environment. It has a single mandatory
argument describing the number of columns and the alignment within them. Each
column, coln, is specified by a single letter that tells how items in that row
should be formatted.
	* c -- for centred
	* l -- for flush left
	* r -- for flush right
Column entries must be separated by an |&|. Column entries may include other
LaTeX commands. Each row of the array must be terminated with the string |\\|.

Note that the |array| environment can only be used in |math-mode|, so normally
it is used inside an |equation| environment.

==============================================================================
b. center					*center*
>
	\begin{center}
		Text on line 1 \\
		Text on line 2 \\
		.
		.
		.
	\end{center}

The |\center| environment allows you to create a paragraph consisting of lines
that are centred within the left and right margins on the current page. Each
line must be terminated with the string |\\|.

\centering					*\centering*
		This declaration corresponds to the |center| environment. This
		declaration can be used inside an environment such as
		|quote-l| or in a |\parbox|. The text of a |figure| or |table|
		can be centred on the page by putting a |\centering| command
		at the beginning of the |figure| or |table| environment.
		Unlike the |center| environment, the |\centering| command does
		not start a new paragraph; it simply changes how LaTeX formats
		paragraph units. To affect a paragraph unit's format, the
		scope of the declaration must contain the blank line or |\end|
		command (of an environment like |quote-l|) that ends the
		paragraph unit.

==============================================================================
c. description					*description*
>
	\begin{description}
		\item [label] First item
		\item [label] Second item
		.
		.
		.
	\end{description}

The |description| environment is used to make labelled lists. The label is
bold face and flushed right.

==============================================================================
d. enumerate					*enumerate*
>
	\begin{enumerate}
		\item First item
		\item Second item
		.
		.
		.
	\end{enumerate}

The |enumerate| environment produces a numbered list.  Enumerations can be
nested within one another, up to four levels deep.  They can also be nested
within other paragraph-making environments.

\item		Each item of an enumerated list begins with an |\item|
		command. There must be at least one |\item| command
		within the environment.

The |enumerate| environment uses the |\enumi| through |\enumiv| counters (see
section |latex-counters|). The type of numbering can be changed by redefining
\theenumi etc.

==============================================================================
e. eqnarray					*eqnarray*
>
	\begin{eqnarray}
		math formula 1 \\
		math formula 2 \\
		.
		.
		.
	\end{eqnarray}

The |eqnarray| environment is used to display a sequence of equations or
inequalities. It is very much like a three-column |array| environment, with
consecutive rows separated by |\\| and consecutive items within a row separated
by an |&|.

\nonumber					*\nonumber*
		An equation number is placed on every line unless that
		line has a |\nonumber| command.

\lefteqn					*\lefteqn*
		The command |\lefteqn| is used for splitting long
		formulas across lines.  It typesets its argument in
		display style flush left in a box of zero width.

==============================================================================
f. equation	 				*equation*
>
	\begin{equation}
		math formula
	\end{equation}

The |equation| environment centres your equation on the page and places the
equation number in the right margin.

==============================================================================
g. figure					*figure*
>
	\begin{figure}[placement]
		body of the figure
		\caption{figure title}
	\end{figure}

Figures are objects that are not part of the normal text, and are usually
"floated" to a convenient place, like the top of a page. Figures will not be
split between two pages.

The optional argument [placement] determines where LaTeX will try to place
your figure. There are four places where LaTeX can possibly put a float:

h (Here)		at the position in the text where the figure
			environment appears.
t (Top)			at the top of a text page.
b (Bottom)		at the bottom of a text page.
p (Page of floats)	on a separate float page, which is a page containing
			no text, only floats.

The standard |report-class| and |article-class| use the default placement
[tbp].

The body of the |figure| is made up of whatever text, LaTeX commands, etc.  you
wish.

The \caption command allows you to title your figure.

==============================================================================
h. flushleft					*flushleft*
>
	\begin{flushleft}
		Text on line 1 \\
		Text on line 2 \\
		.
		.
		.
	\end{flushleft}

The |flushleft| environment allows you to create a paragraph consisting of
lines that are flushed left, to the left-hand margin. Each line must be
terminated with the string |\\|.

\raggedright					*\raggedright*
		This declaration corresponds to the |flushleft| environment.
		This declaration can be used inside an environment such as
		|quote-l| or in a |\parbox|.  Unlike the |flushleft|
		environment, the |\raggedright| command does not start a new
		paragraph; it simply changes how LaTeX formats paragraph
		units. To affect a paragraph unit's format, the scope of the
		declaration must contain the blank line or |\end| command (of
		an environment like |quote-l|) that ends the paragraph unit.

==============================================================================
i. flushright					*flushright*
>
	\begin{flushright}
		Text on line 1 \\
		Text on line 2 \\
		.
		.
		.
 	\end{flushright}

The |flushright| environment allows you to create a paragraph consisting of
lines that are flushed right, to the right-hand margin. Each line must be
terminated with the string |\\|.

\raggedleft					*\raggedleft*
		This declaration corresponds to the |flushright| environment.
		This declaration can be used inside an environment such as
		|quote-l| or in a |\parbox|.  Unlike the |flushright|
		environment, the |\raggedleft| command does not start a new
		paragraph; it simply changes how LaTeX formats paragraph
		units. To affect a paragraph unit's format, the scope of the
		declaration must contain the blank line or |\end| command (of
		an environment like |quote-l|) that ends the paragraph unit.

==============================================================================
j. itemize					*itemize*
>
	\begin{itemize}
		\item First item
		\item Second item
		.
		.
		.
	\end{itemize}

The |itemize| environment produces a "bulleted" list.  Itemizations can be
nested within one another, up to four levels deep.  They can also be nested
within other paragraph-making environments.

\item						*\item*
		Each item of an itemized list begins with an |\item| command.
		There must be at least one |\item| command within the
		environment.

The itemize environment uses the |\itemi| through |\itemiv| counters (see
section |latex-counters|). The type of numbering can be changed by redefining
\theitemi etc.

==============================================================================
k. letter					*\letter*

This environment is used for creating letters. See section |latex-letters|.

==============================================================================
l. list						*list*

The |list| environment is a generic environment which is used for defining many
of the more specific environments. It is seldom used in documents, but often
in macros.
>
	\begin{list}{label}{spacing}
		\item First item
		\item Second item
		.
		.
		.
	\end{list}

'label'		The {label} argument specifies how items should be labelled.
		This argument is a piece of text that is inserted in a box to
		form the {label}.  This argument can and usually does contain
		other LaTeX commands.

'spacing'	The {spacing} argument contains commands to change the spacing
		parameters for the |list|. This argument will most often be
		null, i.e., {}. This will select all default spacing which
		should suffice for most cases.

==============================================================================
m. minipage					*minipage*
>
	\begin{minipage}[position]{width}
		text
	\end{minipage}

The |minipage| environment is similar to a |\parbox| command. It takes the
same optional [position] argument and mandatory {width} argument. You may use
other paragraph-making environments inside a |minipage|.  Footnotes in a
minipage environment are handled in a way that is particularly useful for
putting footnotes in figures or tables. A |\footnote| or |\footnotetext|
command puts the footnote at the bottom of the minipage instead of at the
bottom of the page, and it uses the |\mpfootnote| counter instead of the
ordinary footnote counter. See sections |latex-counters| and
|latex-footnotes|.

NOTE: Don't put one |minipage| inside another if you are using footnotes; they
may wind up at the bottom of the wrong minipage.

==============================================================================
n. picture					*picture*
>
		 	   size		  position
	\begin{picture}(width,height)(x offset,y offset)
		.
		.
		picture commands
		.
		.
	\end{picture}

The |picture| environment allows you to create just about any kind of picture
you want containing text, lines, arrows and circles. You tell LaTeX where to
put things in the picture by specifying their coordinates. A coordinate is a
number that may have a decimal point and a minus sign -- a number like 5, 2.3
or -3.1416. A coordinate specifies a length in multiples of the unit length
|\unitlength|, so if |\unitlength| has been set to 1cm, then the coordinate
2.54 specifies a length of 2.54 centimetres. You can change the value of
|\unitlength| anywhere you want, using the |\setlength| command, but strange
things will happen if you try changing it inside the |picture| environment.

A position is a pair of coordinates, such as (2.4,-5), specifying the point
with x-coordinate 2.4 and y-coordinate -5. Coordinates are specified in the
usual way with respect to an origin, which is normally at the lower-left
corner of the |picture|.
Note that when a position appears as an argument, it is not enclosed in
braces; the parentheses serve to delimit the argument.

The |picture| environment has one mandatory argument, which is a position.  It
specifies the size of the picture. The environment produces a rectangular box
with width and height determined by this argument's x- and y-coordinates.

The |picture| environment also has an optional position argument, following
the size argument, that can change the origin. (Unlike ordinary optional
arguments, this argument is not contained in square brackets.) The optional
argument gives the coordinates of the point at the lower-left corner of the
picture (thereby determining the origin).  For example, if |\unitlength| has
been set to 1mm, the command: >
	\begin{picture}(100,200)(10,20)
>
produces a picture of width 100 millimetres and height 200 millimetres, whose
lower-left corner is the point (10,20) and whose upper-right corner is
therefore the point (110,220). When you first draw a picture, you will omit
the optional argument, leaving the origin at the lower-left corner. If you
then want to modify your picture by shifting everything, you just add the
appropriate optional argument.

The environment's mandatory argument determines the nominal size of the
picture. This need bear no relation to how large the picture really is; LaTeX
will happily allow you to put things outside the picture, or even off the
page. The picture's nominal size is used by LaTeX in determining how much room
to leave for it.

Everything that appears in a picture is drawn by the |\put| command. The
command: >
	\put (11.3,-.3){...}

puts the object specified by ... in the picture, with its
reference point at coordinates (11.3,-.3). The reference points for various
objects will be described below.

The |\put| creates an LR box (|lrbox|). You can put anything in the text
argument of the |\put| that you'd put into the argument of an |\mbox| and
related commands. When you do this, the reference point will be the lower left
corner of the box.

Picture commands:
|\circle|		Draw a circle
|\dashbox|		Draw a dashed box
|\frame|		Draw a frame around an object
|\framebox(picture)|	Draw a box with a frame around it
|\line|			Draw a straight line
|\linethickness|	Set the line thickness
|\makebox(picture)|	Draw a box of the specified size
|\multiput|		Draw multiple instances of an object
|\oval|			Draw an ellipse
|\put|			Place an object at a specified place
|\shortstack|		Make a pile of objects
|\vector|		Draw a line with an arrow

\circle[*]{diameter}				*\circle*
		Command produces a circle with a {diameter} as close to the
		specified one as possible. If the *-form of the command is
		used, LaTeX draws a solid circle.
		Note: only circles up to 40 pt can be drawn.


\dashbox{dashlength}(width,height){...} 	*\dashbox*
		Draws a box with a dashed line.  The |\dashbox| has an extra
		argument which specifies the width of each dash.  A dashed box
		looks best when the width and height are multiples of the
		{dashlength}.

\frame{...} 					*\frame*
		Puts a rectangular frame around the object specified in the
		argument. The reference point is the bottom left corner of the
		frame. No extra space is put between the frame and the object.

\framebox(width,height)[position]{...}		*\picture-framebox*
		The |\framebox| command is exactly the same as the
		|picture-makebox| command, except that it puts a frame around
		the outside of the box that it creates.  The |\framebox|
		command produces a rule of thickness |\fboxrule|, and leaves a
		space |\fboxsep| between the rule and the contents of the box.

\line(x slope,y slope){length} 			*\line*
		Draws a line of the specified length and slope.
		Note: LaTeX can only draw lines with slope = x/y, where x and
		y have integer values from -6 through 6.

\linethickness{dimension}			*\linethickness*
		Declares the thickness of horizontal and vertical lines in a
		|picture| environment to be dimension, which must be a
		positive length. It does not affect the thickness of slanted
		lines (|\line|) and circles (|circle|), or the quarter circles
		drawn by |\oval| to form the corners of an oval.

\makebox(width,height)[position]{...} 		*picture-makebox*
		The makebox command for the |picture| environment is similar
		to the normal |\makebox| command except that you must specify
		a width and height in multiples of |\unitlength|.
		The optional argument, [position], specifies the quadrant that
		your text appears in. You may select up to two of the
		following:
			t - Moves the item to the top of the rectangle
			b - Moves the item to the bottom
			l - Moves the item to the left
			r - Moves the item to the right

						*\multiput*
\multiput(x coord,y coord)(delta x,delta y){no of copies}{object}
		This command can be used when you are putting the same
		object in a regular pattern across a picture.

\oval(width,height)[portion] 			*\oval*
		Produces a rectangle with rounded corners. The optional
		argument, [portion], allows you to select part of the oval.
			t - top portion
			b - bottom portion
			r - right portion
			l - left portion
		Note: the "corners" of the oval are made with quarter circles
		with a maximum radius of 20 pt, so large "ovals" will look
		more like boxes with rounded corners.

\put(x coord,y coord){ ... } 			*\put*
		Places the item specified by the mandatory argument at the
		given coordinates.

\shortstack[position]{... \\ ... \\ ...} 	*\shortstack*
		The |\shortstack| command produces a stack of objects.
		The valid positions are:
			r - right of the stack
			l - left of the stack
			c - centre of the stack (default)

\vector(x slope,y slope){length} 		*\vector*
		Draws a line with an arrow of the specified length and slope.
		The x and y values must lie between -4 and +4, inclusive.

==============================================================================
o. quotation					*quotation*
 >
	\begin{quotation}
		text
	\end{quotation}

The margins of the |quotation| environment are indented on the left and the
right. The text is justified at both margins and there is paragraph
indentation. Leaving a blank line between text produces a new paragraph.

==============================================================================
p. quote					*quote-l*
>
	\begin{quote}
		text
	\end{quote}

The margins of the |quote-l| environment are indented on the left and the right.
The text is justified at both margins.  Leaving a blank line between text
produces a new paragraph.

==============================================================================
q. tabbing					*tabbing*
>
	\begin{tabbing}
	text \= more text \= still more text \= last text \\
	second row \>  \> more \\
	.
	.
	.
	\end{tabbing}

The |tabbing| environment provides a way to align text in columns. It works by
setting tab stops and tabbing to them much the way you do with an ordinary
typewriter.

It is best suited for cases where the width of each column is constant and
known in advance.

This environment can be broken across pages, unlike the |tabular| environment.
The following commands can be used inside a tabbing environment:

				*tab=*
\= 		Sets a tab stop at the current position.

						*tab>*
\> 		Advances to the next tab stop.

						*tab<*
\< 		This command allows you to put something to the left of the
		local margin without changing the margin. Can only be used at
		the start of the line.

						*tab+*
\+ 		Moves the left margin of the next and all the following
		commands one tab stop to the right.

						*tab-*
\- 		Moves the left margin of the next and all the following
		commands one tab stop to the left.

						*tab'*
\' 		Moves everything that you have typed so far in the current
		column, i.e.  everything from the most recent \> (|tab>|), \<
		(|tab<|), \' (|tab'|), |\\|, or |\kill| command, to the right
		of the previous column, flush against the current column's tab
		stop.

						*tab`*
\`		Allows you to put text flush right against any tab stop,
		including tab stop 0. However, it can't move text to the right
		of the last column because there's no tab stop there. The \`
		(|tab`|) command moves all the text that follows it, up to the
		|\\| or \end{tabbing} command that ends the line, to the right
		margin of the tabbing environment. There must be no \>
		(|tab>|) or \' (|tab'|) command between the \` (|tab`|) and
		the command that ends the line.

						*\kill*
\kill 		Sets tab stops without producing text. Works just like |\\|
		except that it throws away the current line instead of
		producing output for it. The effect of any \= (|tab=|), \+
		(|tab+|) or \- (|tab-|) commands in that line remain in
		effect.

						*\pushtabs*
\pushtabs	Saves all current tab stop positions. Useful for temporarily
		changing tab stop positions in the middle of a tabbing
		environment. Also restores the tab stop positions saved by the
		last |\pushtabs|.

						*taba*
\a		In a tabbing environment, the commands \= (|tab=|), \'
		(|tab'|) and \` (|tab`|) do not produce accents as normal.
		Instead, the commands \a=, \a' and \a` are used.

This example typesets a Pascal function in a traditional format:
>
        \begin{tabbing}
        function \= fact(n : integer) : integer;\\
                 \> begin \= \+ \\
                       \> if \= n $>$ 1 then \+ \\
                                fact := n * fact(n-1) \- \\
                          else \+ \\
                                fact := 1; \-\- \\
                    end;\\
        \end{tabbing}

==============================================================================
r. table					*\table*
>
	\begin{table}[placement]
		body of the table
		\caption{table title}
	\end{table}

Tables are objects that are not part of the normal text, and are usually
"floated" to a convenient place, like the top of a page. Tables will not be
split between two pages.

The optional argument [placement] determines where LaTeX will try to place
your table. There are four places where LaTeX can possibly put a float:

	h (Here)		at the position in the text where the table
				environment appears.
	t (Top)			at the top of a text page.
	b (Bottom)		at the bottom of a text page.
	p (Page of floats)	on a separate float page, which is a page
				containing no text, only floats.

The standard |report-class| and |article-class| use the default placement [tbp].

The body of the table is made up of whatever text, LaTeX commands, etc., you
wish.

The \caption command allows you to title your table.

==============================================================================
s. tabular					*tabular*
>
	\begin{tabular}[pos]{cols}
		column 1 entry & column 2 entry ... & column n entry \\
		.
		.
		.
	\end{tabular}

or
>
	\begin{tabular*}{width}[pos]{cols}
		column 1 entry & column 2 entry ... & column n entry \\
		.
		.
		.
	\end{tabular*}

These environments produce a box consisting of a sequence of rows of items,
aligned vertically in columns. The mandatory and optional arguments consist
of:

{width}	Specifies the width of the tabular* environment. There must be
	rubber space between columns that can stretch to fill out the
	specified width.

[pos]	Specifies the vertical position; default is alignment on the
	centre of the environment.
		t - align on top row
		b - align on bottom row

{cols}	Specifies the column formatting. It consists of a sequence of
	the following specifiers, corresponding to the sequence of
	columns and intercolumn material.
		l - A column of left-aligned items.

		r - A column of right-aligned items.

		c - A column of centred items.

		| - A vertical line the full height and depth of the
		environment.

		@{text} - This inserts text in every row. An @-expression
		suppresses the intercolumn space normally inserted
		between columns; any desired space between the
		inserted text and the adjacent items must be included
		in text. An \extracolsep{wd} command in an
		@-expression causes an extra space of width {wd} to
		appear to the left of all subsequent columns, until
		countermanded by another |\extracolsep| command. Unlike
		ordinary intercolumn space, this extra space is not
		suppressed by an @-expression. An |\extracolsep|
		command can be used only in an @-expression in the
		cols argument.

		p{wd} - Produces a column with each item typeset in a |\parbox|
		of width {wd}, as if it were the argument of a
		\parbox[t]{wd} command. However, a |\\| may not appear
		in the item, except in the following situations:
		1. inside an environment like |minipage|, |array|, or
		|tabular|.
		2. inside an explicit |\parbox|.
		3. in the scope of a |\centering|, |\raggedright|, or
		|\raggedleft| declaration. The latter declarations must
		appear inside braces or an environment when used in a
		p-column element.

		{num}{cols} - Equivalent to num copies of cols, where num is any positive
		integer and cols is any list of column-specifiers,
		which may contain another -expression.

These commands can be used inside a tabular environment:

|\cline|		Draw a horizontal line spanning some columns.
|\hline|		Draw a * horizontal line spanning all columns.
|\multicolumn|		Make an item spanning * several columns.
|\vline|		Draw a vertical line.


\cline{i-j}					*\cline*
		The |\cline| command draws horizontal lines across the columns
		specified, beginning in column i and ending in column j,
		which are identified in the mandatory argument.

\hline						*\hline*
		The |\hline| command will draw a horizontal line the width of
		the table.  It's most commonly used to draw a line at the top,
		bottom, and between the rows of the table.

\multicolumn{cols}{pos}{text} 			*\multicolumn*
		The |\multicolumn| is used to make an entry that spans several
		columns.  The first mandatory argument, {cols}, specifies the
		number of columns to span. The second mandatory argument,
		{pos}, specifies the formatting of the entry:
			c - centered
			l - flushleft
			r - flushright.
		The third mandatory argument, {text}, specifies what text is
		to make up the entry.

\vline						*\vline*
		The |\vline| command will draw a vertical line extending the
		full height and depth of its row. An |\hfill| command can be
		used to move the line to the edge of the column. It can also
		be used in an @-expression.

==============================================================================
t. thebibliography				*\thebibliography*
>
	\begin{thebibliography}{widestlabel}
		\bibitem[label]{cite_key}
		.
		.
		.
	\end{thebibliography}

The |\thebibliography| environment produces a bibliography or reference list.

In the |article-class|, this reference list is labelled "References"; in the
|report-class|, it is labelled "Bibliography".

{widestlabel}	Text that, when printed, is approximately as wide as the
 		widest item label produces by the |\bibitem| commands.

|\bibitem|		Specify a bibliography item.
|\cite|			Refer to a bibliography item.
|\nocite|		Include an item in the bibliography.
|BibTeX|		Automatic generation of bibliographies.

\bibitem					*\bibitem*
\bibitem[label]{citekey}
		The |\bibitem| command generates an entry labelled by [label].
		If the [label] argument is missing, a number is generated as
		the label, using the |\enumi| counter.  The {citekey} is any
		sequence of letters, numbers, and punctuation symbols not
		containing a comma. This command writes an entry on the `.aux'
		file containing {citekey} and the item's label. When this
		`.aux' file is read by the \begin{document} command, the
		item's label is associated with {citekey}, causing the
		reference to {citekey} by a |\cite| command to produce the
		associated label.

\cite						*\cite*
\cite[text]{keylist}
		The {keylist} argument is a list of citation keys.  This
		command generates an in-text citation to the references
		associated with the keys in {keylist} by entries on the `.aux'
		file read by the \begin{document} command.
		The optional text argument will appear after the
		citation, i.e.: >
			\cite[p.2]{knuth}
<		might produce `[Knuth, p. 2]'.

\nocite						*\nocite*
\nocite{keylist}
		The |\nocite| command produces no text, but writes
		{keylist}, which is a list of one or more citation
		keys, on the `.aux' file.

BibTeX						*BibTeX* *bibtex*
						*\bibliographystyle*
If you use the BibTeX program by Oren Patashnik (highly recommended if you
need a bibliography of more than a couple of titles) to maintain your
bibliography, you don't use the |thebibliography| environment.  Instead, you
include the lines:
>
	\bibliographystyle{style}
	\bibliography{bibfile}

where {style} refers to a file style.bst, which defines how your citations
will look. The standard styles distributed with BibTeX are:

{alpha}	Sorted alphabetically. Labels are formed from name of author and year
	of publication.
{plain} Sorted alphabetically. Labels are numeric.
{unsrt} Like plain, but entries are in order of citation.
{abbrv} Like plain, but more compact labels.

In addition, numerous other BibTeX style files exist tailored to the demands
of various publications.

						*\bibliography*
The argument to |\bibliography| refers to the file bibfile.bib, which should
contain your database in BibTeX format. Only the entries referred to via
|\cite| and |\nocite| will be listed in the bibliography.

==============================================================================
u. theorem					*theorem*
>
	\begin{theorem}
		theorem text
	\end{theorem}

The |theorem| environment produces "Theorem x" in boldface followed by your
theorem text.

==============================================================================
v. titlepage					*titlepage*
>
	\begin{titlepage}
		text
	\end{titlepage}

The |titlepage| environment creates a title page, i.e. a page with no printed
page number or heading. It also causes the following page to be numbered page
one. Formatting the title page is left to you. The |\today| command comes in
handy for title pages.

Note that you can use the |\maketitle| to produce a standard title page.

==============================================================================
x. verbatim					*verbatim*
>
	\begin{verbatim}
		text
	\end{verbatim}

The |verbatim| environment is a paragraph-making environment that gets LaTeX
to print exactly what you type in. It turns LaTeX into a typewriter with
carriage returns and blanks having the same effect that they would on a
typewriter.

\verb						*\verb*
\verb char literal_text char
\verb*char literal_text char
		Typesets literal_text exactly as typed, including
		special characters and spaces, using a typewriter |\tt|
		type style. There may be no space between |\verb| or
		|\verb|* and char (space is shown here only for
		clarity).  The *-form differs only in that spaces are
		printed as `\verb*| |\'.

==============================================================================
y. verse					*verse*
>
	\begin{verse}
		text
	\end{verse}

The |verse| environment is designed for poetry, though you may find other uses
for it.

The margins are indented on the left and the right. Separate the lines of each
stanza with |\\|, and use one or more blank lines to separate the stanzas.

==============================================================================
8. Footnotes					*latex-footnotes*

Footnotes can be produced in one of two ways. They can be produced with one
command, the |\footnote| command. They can also be produced with two commands,
the |\footnotemark| and the |\footnotetext| commands. See the specific command for
information on why you would use one over the other.

|\footnote| 	Insert a footnote
|\footnotemark|	Insert footnote mark only
|\footnotetext|	Insert footnote text only

\footnote[number]{text}				*\footnote*
		Command places the numbered footnote text at the bottom of the
		current page. The optional argument, number, is used to change
		the default footnote number.  This command can only be used in
		outer paragraph mode; i.e., you cannot use it in sectioning
		commands like |\chapter|, in |\figure|, |\table| or in a
		|\tabular| environment.

\footnotemark					*\footnotemark*
		Command puts the footnote number in the text. This command can
		be used in inner paragraph mode. The text of the footnote is
		supplied by the |\footnotetext| command.
		This command can be used to produce several consecutive
		footnote markers referring to the same footnote by using
>
			\footnotemark[\value{footnote}]
<
		after the first |\footnote| command.

\footnotetext[number]{text}			*\footnotetext*
		Command produces the text to be placed at the bottom of the
		page. This command can come anywhere after the |\footnotemark|
		command. The |\footnotetext| command must appear in outer
		paragraph mode.  The optional argument, number, is used to
		change the default footnote number.

==============================================================================
9. Lengths					*latex-lengths*

A length is a measure of distance. Many LaTeX commands take a length as an
argument.

|\newlength|	Define a new length.
|\setlength|	Set the value of a length.
|\addtolength|	Add a quantity to a length.
|\settodepth|	Set a length to  the depth of something.
|\settoheight|	Set a length to the height of  something.
|\settowidth|	Set a length to the width of something.
|pre-lengths|	Lengths that are, like, predefined.

\newlength{\gnat}				*\newlength*
		The |\newlength| command defines the mandatory argument, \gnat,
		as a length command with a value of 0in. An error occurs if a
		\gnat command already exists.

\setlength{\gnat}{length}			*\setlength*
		The |\setlength| command is used to set the value of a \gnat
		command. The {length} argument can be expressed in any terms
		of length LaTeX understands, i.e., inches (in), millimetres
		(mm), points (pt), etc.

\addtolength{\gnat}{length} 			*\addtolength*
		The |\addtolength| command increments a \gnat by the amount
		specified in the {length} argument. It can be a negative
		amount.

\settodepth{\gnat}{text} 			*\settodepth*
		The |\settodepth| command sets the value of a \gnat command
		equal to the depth of the {text} argument.

\settoheight{\gnat}{text} 			*\settoheight*
		The |\settoheight| command sets the value of a \gnat command
		equal to the height of the {text} argument.

\settowidth{\gnat}{text}			*\settowidth*
		The |\settowidth| command sets the value of a \gnat command
		equal to the width of the {text} argument.

Predefined lengths				*pre-lengths*

\width 						*\width*
\height						*\height*
\depth						*\depth*
\totalheight					*\totalheight*
		These length parameters can be used in the arguments of the
		box-making commands See section Spaces & Boxes. They specify
		the natural width etc.  of the text in the box.
		\totalheight equals \height + \depth.
		To make a box with the text stretched to double the natural
		size, e.g., say: >
			\makebox[2\width]{Get a stretcher}

==============================================================================
10. Letters					*latex-letters*

You can use LaTeX to typeset letters, both personal and business. The letter
document class is designed to make a number of letters at once, although you
can make just one if you so desire.

Your `.tex' source file has the same minimum commands as the other document
classes, i.e., you must have the following commands as a minimum: >
	\documentclass{letter}
	\begin{document}
		...
		letters
		...
	\end{document}

Each letter is a letter environment, whose argument is the name and address of
the recipient. For example, you might have: >
	\begin{letter}
		{Mr. Joe Smith\\
		2345 Princess St.  \\
		Edinburgh, EH1 1AA}
		...
	\end{letter}

The letter itself begins with the |\opening| command.  The text of the letter
follows. It is typed as ordinary LaTeX input.  Commands that make no sense in
a letter, like |\chapter|, do not work. The letter closes with a |\closing|
command.

After the closing, you can have additional material. The |\cc| command produces
the usual "cc: ...". There's also a similar |\encl| command for a list of
enclosures. With both these commands, use|\\| to separate the items.

These commands are used with the letter class:
|\address|	Your return address.
|\cc|		Cc list.  closing Saying goodbye.
|\encl|		List of enclosed material.
|\location|	Your organisation's address.
|\makelabels|	Making address labels.
|\name|		Your name, for the return address.
|\opening|	Saying hello.
|\ps|		Adding a postscript.
|\signature|	Your signature.
|\startbreaks|	Allow page breaks.
|\stopbreaks|	Disallow page breaks.
|\telephone|	Your phone number.

\address{Return address}			*\address*
		The return address, as it should appear on the letter and the
		envelope.  Separate lines of the address should be separated
		by |\\| commands. If you do not make an |\address| declaration,
		then the letter will be formatted for copying onto your
		organisation's standard letterhead. (See section Overview of
		LaTeX and Local Guide, for details on your local
		implementation). If you give an |\address| declaration, then
		the letter will be formatted as a personal letter.

\cc{Kate Schechter\\Rob McKenna}		*\cc*
		Generate a list of other persons the letter was sent to. Each
		name is printed on a separate line.

\closing{text}					*\closing*
		The letter closes with a |\closing| command, i.e., >
			\closing{Best Regards,} \encl{CV\\Certificates}
<		Generate a list of enclosed material.

\location{address}				*\location*
		This modifies your organisation's standard address. This only
		appears if the firstpage pagestyle is selected.

\makelabels{number}				*\makelabels*
		If you issue this command in the preamble, LaTeX will create a
		sheet of address labels. This sheet will be output before the
		letters.

\name{June Davenport}				*\name*
		Your name, used for printing on the envelope together with the
		return address.

\opening{text}					*\opening*
		The letter begins with the |\opening| command. The mandatory
		argument, text, is whatever text you wish to start your
		letter, i.e., >
			\opening{Dear Joe,}

\ps						*\ps*
		Use this command before a postscript.

\signature{Harvey Swick}			*\signature*
		Your name, as it should appear at the end of the letter
		underneath the space for your signature. Items that should go
		on separate lines should be separated by |\\| commands.

\startbreaks					*\startbreaks*
		Used after a |\stopbreaks| command to allow page breaks again.

\stopbreaks					*\stopbreaks*
		Inhibit page breaks until a |\startbreaks| command occurs.

\telephone{number}				*\telephone*
		This is your telephone number. This only appears if the
		firstpage pagestyle is selected.

==============================================================================
11. Line & Page Breaking			*latex-breaking*

The first thing LaTeX does when processing ordinary text is to translate your
input file into a string of glyphs and spaces. To produce a printed document,
this string must be broken into lines, and these lines must be broken into
pages. In some environments, you do the line breaking yourself with the |\\|
command, but LaTeX usually does it for you.

|\\| 			Start a new line
|hyph-| 		Insert explicit hyphenation
|\cleardoublepage| 	Start a new right-hand page
|\clearpage| 		Start a new page
|\enlargethispage| 	Enlarge the current page a bit
|\fussy| 		Be fussy about line breaking
|\hyphenation| 		Tell LaTeX how to hyphenate a word
|\linebreak| 		Break the line
|\newline| 		Break the line prematurely
|\newpage| 		Start a new page
|\nolinebreak| 		Don't break the current line
|\nopagebreak| 		Don't make a page break here
|\pagebreak| 		Please make a page break here
|\sloppy| 		Be sloppy about line breaking

\\[*][extraspace]				*\\* *\\\\*
		The |\\| command tells LaTeX to start a new line. It has an
		optional argument, [extraspace], that specifies how much extra
		vertical space is to be inserted before the next line. This
		can be a negative amount.
		The \\* command is the same as the ordinary |\\| command
		except that it tells LaTeX not to start a new page after the
		line.

\-						*hyph-*
		The \- command tells LaTeX that it may hyphenate the word at
		that point.  LaTeX is very good at hyphenating, and it will
		usually find all correct hyphenation points. The \- command is
		used for the exceptional cases.
		Note: when you insert \- commands in a word, the word will
		only be hyphenated at those points and not at any of the
		hyphenation points that LaTeX might otherwise have chosen.

\cleardoublepage				*\cleardoublepage*
		The |\cleardoublepage| command ends the current page and causes
		all figures and tables that have so far appeared in the input
		to be printed.  In a two-sided printing style (|twoside|), it
		also makes the next page a right-hand (odd-numbered) page,
		producing a blank page if necessary.

\clearpage					*\clearpage*
		The |\clearpage| command ends the current page and causes all
		figures and tables that have so far appeared in the input to
		be printed.

\enlargethispage{size} 				*\enlargethispage*
\enlargethispage*{size}
		Enlarge the textheight for the current page by the
		specified amount; e.g.: >

			\enlargethispage{\baselineskip}
<
		will allow one additional line.  The starred form
		tries to squeeze the material together on the page as
		much as possible. This is normally used together with
		an explicit |\pagebreak|.

\fussy						*\fussy*
		This declaration (which is the default) makes TeX more fussy
		about line breaking. This can avoids too much space between
		words, but may produce overfull boxes.  This command cancels
		the effect of a previous |\sloppy| command.

\hyphenation{words}				*\hyphenation*
		The |\hyphenation| command declares allowed hyphenation points,
		where words is a list of words, separated by spaces, in which
		each hyphenation point is indicated by a - character.

\linebreak[number]				*\linebreak*
		The |\linebreak| command tells LaTeX to break the current line
		at the point of the command. With the optional argument,
		number, you can convert the |\linebreak| command from a demand
		to a request. The [number] must be a number from 0 to 4. The
		higher the number, the more insistent the request is.  The
		|\linebreak| command causes LaTeX to stretch the line so it
		extends to the right margin.

\newline					*\newline*
		The |\newline| command breaks the line right where it is. It
		can only be used in paragraph mode.

\newpage					*\newpage*
		The |\newpage| command ends the current page.

\nolinebreak[number]				*\nolinebreak*
		The |\nolinebreak| command prevents LaTeX from breaking the
		current line at the point of the command. With the optional
		argument, [number], you can convert the |\nolinebreak| command
		from a demand to a request. The [number] must be a number from 0
		to 4. The higher the number, the more insistent the request
		is.

\nopagebreak[number]				*\nopagebreak*
		The |\nopagebreak| command prevents LaTeX from breaking the
		current page at the point of the command. With the optional
		argument, [number], you can convert the |\nopagebreak| command
		from a demand to a request. The [number] must be a number from
		0 to 4. The higher the number, the more insistent the request
		is.

\pagebreak[number]				*\pagebreak*
		The |\pagebreak| command tells LaTeX to break the current page
		at the point of the command. With the optional argument,
		[number], you can convert the |\pagebreak| command from a
		demand to a request. The [number] must be a number from 0 to
		4. The higher the number, the more insistent the request is.

\sloppy						*\sloppy*
		This declaration makes TeX less fussy about line breaking.
		This can prevent overfull boxes, but may leave too much space
		between words.
		Lasts until a |\fussy| command is issued.

==============================================================================
12. Making Paragraphs				*latex-paragraphs*

A paragraph is ended by one or more completely blank lines -- lines not
containing even a |\%|. A blank line should not appear where a new paragraph
cannot be started, such as in math mode or in the argument of a sectioning
command.

|\indent| 	Indent this paragraph.
|\noindent| 	Do not indent this paragraph.
|\par| 		Another way of writing a blank line.

\indent 					*\indent*
		This produces a horizontal space whose width equals the width
		of the paragraph indentation. It is used to add paragraph
		indentation where it would otherwise be suppressed.

\noindent 					*\noindent*
		When used at the beginning of the paragraph, it suppresses the
		paragraph indentation. It has no effect when used in the
		middle of a paragraph.

\par						*\par*
		Equivalent to a blank line; often used to make command or
		environment definitions easier to read.

==============================================================================
13. Margin Notes				*latex-margin-notes*

\marginpar[left]{right}				*\marginpar*
		This command creates a note in the margin. The first line will
		be at the same height as the line in the text where the
		|\marginpar| occurs.

		When you only specify the mandatory argument {right}, the text
		will be placed:
		* in the right margin for one-sided layout
		* in the outside margin for two-sided layout (|twoside|)
		* in the nearest margin for two-column layout (|twocolumn|)

\reversemarginpar				*\reversemarginpar*
		By issuing the command |\reversemarginpar|, you can force the
		marginal notes to go into the opposite (inside) margin.

When you specify both arguments, left is used for the left margin, and right
is used for the right margin.

The first word will normally not be hyphenated; you can enable hyphenation by
prefixing the first word with a \hspace{0pt} command (|hspace|).

==============================================================================
14. Math Formulae				*latex-math*
						*displaymath*
There are three environments (|latex-environments|) that put LaTeX in math
mode:
|math|  	For Formulae that appear right in the text.
|displaymath|  	For Formulae that appear on their own line.
|equation|  	The same as the displaymath environment except that it adds an
		equation number in the right margin.

The |math| environment can be used in both paragraph and LR mode, but the
|displaymath| and |equation| environments can be used only in paragraph mode. The
|math| and |displaymath| environments are used so often that they have the
following short forms:
	\(...\)    instead of    \begin{math}...\end{math}
	\[...\]    instead of    \begin{displaymath}...\end{displaymath}

In fact, the math environment is so common that it has an even shorter form:
	$ ... $    instead of     \(...\)

|sub-sup|	Also known as exponent or index.
|math-symbols|	Various mathematical squiggles.
|math-spacing|	Thick, medium, thin and negative spaces.
|math-misc|	Stuff that doesn't fit anywhere else.

==========
Subscripts & Superscripts			*sub-sup*
						*subscripts* *superscripts*

To get an expression exp to appear as a subscript, you just type _{exp}.  To
get exp to appear as a superscript, you type ^{exp}. LaTeX handles
superscripted superscripts and all of that stuff in the natural way. It even
does the right thing when something has both a subscript and a superscript.

==========
Math Symbols					*math-symbols*

LaTeX provides almost any mathematical symbol you're likely to need. The
commands for generating them can be used only in math mode. For example, if
you include >
	$\pi$
in your source, you will get the symbol in your output.

==========
Spacing in Math Mode				*math-spacing*

In a math environment, LaTeX ignores the spaces you type and puts in the
spacing that it thinks is best. LaTeX formats mathematics the way it's done in
mathematics texts. If you want different spacing, LaTeX provides the following
four commands for use in math mode:
	\; - a thick space			*math;*
	\: - a medium space			*math:*
	\, - a thin space			*math,*
	\! - a negative thin space		*math!*

==========
Math Miscellany					*math-misc*

\cdots						*\cdots*
		Produces a horizontal ellipsis where the dots are raised to
		the centre of the line.
\ddots						*\ddots*
		Produces a diagonal ellipsis.
\frac{num}{den}					*\frac*
		Produces the fraction num divided by den.
\ldots						*\ldots*
		Produces an ellipsis. This command works in any mode, not just
		math mode.
\overbrace{text}				*\overbrace*
		Generates a brace over text.
\overline{text}					*\overline*
		Causes the argument text to be overlined.
\sqrt[root]{arg}				*\sqrt*
		Produces the square root of its argument.  The optional
		argument, [root], determines what root to produce, i.e., the
		cube root of x+y would be typed as: >
			$\sqrt[3]{x+y}$.
\underbrace{text}				*\underbrace*
		Generates text with a brace underneath.
\underline{text}				*\underline*
		Causes the argument text to be underlined. This command can
		also be used in paragraph and LR mode.
\vdots						*\vdots*
		Produces a vertical ellipsis.

==============================================================================
15. Modes					*latex-modes*

When LaTeX is processing your input text, it is always in one of three modes:
	Paragraph mode					*paragraph-mode*
	Math mode					*math-mode*
	Left-to-right mode, called LR mode for short.	*lr-mode*

LaTeX changes mode only when it goes up or down a staircase to a different
level, though not all level changes produce mode changes. Mode changes occur
only when entering or leaving an environment, or when LaTeX is processing the
argument of certain text-producing commands.

|paragraph-mode| is the most common; it's the one LaTeX is in when processing
ordinary text. In that mode, LaTeX breaks your text into lines and breaks the
lines into pages. LaTeX is in |math-mode| when it's generating a mathematical
formula. In |lr-mode|, as in |paragraph-mode|, LaTeX considers the output that
it produces to be a string of words with spaces between them. However, unlike
|paragraph-mode|, LaTeX keeps going from left to right; it never starts a new
line in |lr-mode|. Even if you put a hundred words into an |\mbox|, LaTeX would
keep typesetting them from left to right inside a single box, and then
complain because the resulting box was too wide to fit on the line.

LaTeX is in |lr-mode| when it starts making a box with an |\mbox| command.  You
can get it to enter a different mode inside the box - for example, you can
make it enter |math-mode| to put a formula in the box. There are also several
text-producing commands and environments for making a box that put LaTeX in
|paragraph-mode|. The box make by one of these commands or environments will be
called a |\parbox|. When LaTeX is in |paragraph-mode| while making a box, it is
said to be in "inner paragraph mode". Its normal |paragraph-mode|, which it
starts out in, is called "outer paragraph mode".

==============================================================================
16. Page Styles					*latex-page-styles*

The |\documentclass| command determines the size and position of the page's head
and foot. The page style determines what goes in them.

|\maketitle| 	Generate a title page.
|\pagenumbering| Set the style used for page numbers.
|\pagestyle| 	Change the headings/footings style.
|\thispagestyle| Change the headings/footings style for this page.

\maketitle					*\maketitle*
		The |\maketitle| command generates a title on a separate title
		page - except in the |\article| class, where the title normally
		goes at the top of the first page.  Information used to
		produce the title is obtained from the following declarations:

		|\author|	Who wrote this stuff?
		|\date|		The date the document was created.
		|\thanks|	A special form of footnote.
		|\title|		How to set the document title.

		\author{names}				*\author* *\and*
			The |\author| command declares the author(s), where
			names is a list of authors separated by \and commands.
			Use |\\| to separate lines within a single author's
			entry -- for example, to give the author's institution
			or address.

		\date{text}				*\date*
			The |\date| command declares text to be the document's
			date.  With no |\date| command, the current date is
			used.

		\thanks{text}				*\thanks*
			The |\thanks| command produces a |\footnote| to the
			title.

		\title{text}				*\title*
			The |\title| command declares text to be the title. Use
			|\\| to tell LaTeX where to start a new line in a long
			title.

\pagenumbering{numstyle}			*\pagenumbering*
		Specifies the style of page numbers. Possible values of
		'numstyle' are:
			arabic - Arabic numerals		*arabic*
			roman  - Lowercase Roman numerals 	*roman*
			Roman  - Uppercase Roman numerals 	*Roman*
			alph   - Lowercase letters 		*alph*
			Alph   - Uppercase letters 		*Alph*

\pagestyle{option}				*\pagestyle*
						*plain* *empty* *headings*
		The |\pagestyle| command changes the style from the current
		page on throughout the remainder of your document.
		The valid options are:
		plain      - Just a plain page number.
		empty      - Produces empty heads and feet no page numbers.
		headings   - Puts running headings on each page. The document
			     style specifies what goes in the headings.
		myheadings - You specify what is to go in the heading with the
			     |\markboth| or the |\markright| commands.

		|\markboth| 	Set left and right headings.
		|\markright| 	Set right heading only.

		\markboth{left head}{right head}	*\markboth*
			The |\markboth| command is used in conjunction with the
			page style myheadings for setting both the left and
			the right heading.
			Note that a "left-hand heading" is generated by the
			last |\markboth| command before the end of the page,
			while a "right-hand heading" is generated by the first
			|\markboth| or |\markright| that comes on the page if
			there is one, otherwise by the last one before the
			page.


		\markright{right head}			*\markright*
			The |\markright| command is used in conjunction with
			the page style |\myheadings| for setting the right
			heading, leaving the left heading unchanged.
			Note that a "left-hand heading" is generated by the
			last |\markboth| command before the end of the page,
			while a "right-hand heading" is generated by the first
			|\markboth| or |\markright| that comes on the page if
			there is one, otherwise by the last one before the
			page.

\thispagestyle{option}				*\thispagestyle*
		The |\thispagestyle| command works in the same manner as the
		|\pagestyle| command except that it changes the style for the
		current page only.

==============================================================================
17. Sectioning					*latex-sectioning*

Sectioning commands provide the means to structure your text into units.
|\part|
|\chapter| (report and book class only)
|\section|
|\subsection|
|\subsubsection|
|\paragraph|
|\subparagraph|

All sectioning commands take the same general form, i.e.,

					*\part*
					*\chapter* (report and book class only)
					*\section* *\subsection* *\subsubsection*
					*\paragraph* *\subparagraph*
\chapter[optional]{title}
		In addition to providing the heading in the text, the
		mandatory argument of the sectioning command can appear in two
		other places:
		1. The table of contents
		2. The running head at the top of the page. You may not want
		   the same thing to appear in these other two places as
		   appears in the text heading. To handle this situation, the
		   sectioning commands have an optional argument that provides
		   the text for these other two purposes.

All sectioning commands have *\-forms that print a title, but do not include a
number and do not make an entry in the table of contents.

\appendix 					*\appendix*
		The |\appendix| command changes the way sectional units are
		numbered. The |\appendix| command generates no text and does
		not affect the numbering of parts. The normal use of this
		command is something like: >
			\chapter{The First Chapter}
			...
			\appendix \chapter{The First Appendix}


==============================================================================
18. Spaces & Boxes				*latex-spaces-boxes*

All the predefined length parameters See section Predefined lengths can be
used in the arguments of the box-making commands.

 Horizontal space:

|\dotfill|	Stretchable horizontal dots.
|\hfill|	Stretchable horizontal space.
|\hrulefill|	Stretchable horizontal rule.
|\hspace|	Fixed horizontal space.

 Vertical space:

|\addvspace|	Fixed vertical space.
|\bigskip|	Fixed vertical space.
|\medskip|	Fixed vertical space.
|\smallskip|	Fixed vertical space.
|\vfill|	Stretchable vertical space.
|\vspace|	Fixed vertical space.

 Boxes:

|\fbox|		Framebox.
|\framebox|	Framebox, adjustable position.
|\lrbox|	An environment like |\sbox|.
|\makebox|	Box, adjustable position.
|\mbox|		Box.
|\newsavebox|	Declare a name for saving a box.
|\parbox|	Box with text in paragraph mode.
|\raisebox|	Raise or lower text.
|\rule|		Lines and squares.
|\savebox|	Like |\makebox|, but save the text for later use.
|\sbox|		Like |\mbox|, but save the text for later use.
|\usebox|	Print saved text.

Horizontal space:				*latex-hor-space*

LaTeX removes horizontal space that comes at the end of a line. If you don't
want LaTeX to remove this space, include the optional * argument.  Then the
space is never removed.

\dotfill					*\dotfill*
		The |\dotfill| command produces a "rubber length" that produces
		dots instead of just spaces.

\hfill						*\hfill*
		The |\hfill| fill command produces a "rubber length" which can
		stretch or shrink horizontally. It will be filled with spaces.

\hrulefill					*\hrulefill*
		The |\hrulefill| fill command produces a "rubber length" which
		can stretch or shrink horizontally. It will be filled with a
		horizontal rule.

\hspace[*]{length}				*\hspace*
		The |\hspace| command adds horizontal space. The length of the
		space can be expressed in any terms that LaTeX understands,
		i.e., points, inches, etc. You can add negative as well as
		positive space with an |\hspace| command. Adding negative space
		is like backspacing.


Vertical space:					*latex-ver-space*

LaTeX removes vertical space that comes at the end of a page. If you don't
want LaTeX to remove this space, include the optional * argument.  Then the
space is never removed.

\addvspace{length}				*\addvspace*
		The |\addvspace| command normally adds a vertical space of
		height length.  However, if vertical space has already been
		added to the same point in the output by a previous
		|\addvspace| command, then this command will not add more space
		than needed to make the natural length of the total vertical
		space equal to length.

\bigskip					*\bigskip*
		The |\bigskip| command is equivalent to \vspace{bigskipamount}
		where bigskipamount is determined by the document class.

\medskip					*\medskip*
		The |\medskip| command is equivalent to \vspace{medskipamount}
		where medskipamount is determined by the document class.

\smallskip					*\smallskip*
		The |\smallskip| command is equivalent to
		\vspace{smallskipamount} where smallskipamount is determined
		by the document class.

\vfill						*\vfill*
		The |\vfill| fill command produces a rubber length which can
		stretch or shrink vertically.

\vspace[*]{length}				*\vspace*
		The |\vspace| command adds vertical space. The length of the
		space can be expressed in any terms that LaTeX understands,
		i.e., points, inches, etc. You can add negative as well as
		positive space with an |\vspace| command.


Boxes:						*latex-boxes*

\fbox{text}					*\fbox*
		The |\fbox| command is exactly the same as the |\mbox| command,
		except that it puts a frame around the outside of the box that
		it creates.

\framebox[width][position]{text}		*\framebox*
		The |\framebox| command is exactly the same as the |\makebox|
		command, except that it puts a frame around the outside of the
		box that it creates.
		The |\framebox| command produces a rule of thickness
		|\fboxrule|, and leaves a space |\fboxsep| between the rule and
		the contents of the box.

lrbox						*\lrbox*
\begin{lrbox}{cmd} text \end{lrbox}
		This is the environment form of |\sbox|.
		The text inside the environment is saved in the box cmd, which
		must have been declared with |\newsavebox|.

\makebox[width][position]{text} 		*\makebox*
		The |\makebox| command creates a box just wide enough to
		contain the text specified. The width of the box is specified
		by the optional [width] argument.  The position of the text
		within the box is determined by the optional [position]
		argument.
			c -- centred (default)
			l -- flushleft
			r -- flushright
			s -- stretch from left to right margin. The text must
			     contain stretchable space for this to work.
		See section |\picture-makebox|.

\mbox{text}					*\mbox*
		The |\mbox| command creates a box just wide enough to hold the
		text created by its argument.
		Use this command to prevent text from being split across
		lines.

\newsavebox{cmd}				*\newsavebox*
		Declares {cmd}, which must be a command name that is not
		already defined, to be a bin for saving boxes.


\parbox[position][height][innerpos]{width}{text} 	*\parbox*
		A parbox is a box whose contents are created in
		|\paragraph-mode|. The |\parbox| has two

	Mandatory arguments:
'width'		specifies the width of the parbox
'text'		the text that goes inside the parbox.

	Optional arguments:
'position'	LaTeX will position a parbox so its centre lines up with the
		centre of the text line. The optional position argument allows
		you to line up either the top or bottom line in the parbox
		(default is top).

'height'        If the height argument is not given, the box will have the
		natural height of the text.

'innerpos'	The inner-pos argument controls the placement of the text
		inside the box. If it is not specified, position is used.
			t -- text is placed at the top of the box
			c -- text is centred in the box
			b -- text is placed at the bottom of the box
			s -- stretch vertically. The text must contain
			     vertically stretchable space for this to work.

		A |\parbox| command is used for a parbox containing a small
		piece of text, with nothing fancy inside. In particular, you
		shouldn't use any of the paragraph-making environments inside
		a |\parbox| argument. For larger pieces of text, including ones
		containing a paragraph-making environment, you should use a
		|\minipage| environment.

\raisebox{distance}[extendabove][extendbelow]{text}   *\raisebox*
		The |\raisebox| command is used to raise or lower text. The
		first mandatory argument specifies how high the text is to be
		raised (or lowered if it is a negative amount). The text
		itself is processed in LR mode.
		Sometimes it's useful to make LaTeX think something has a
		different size than it really does - or a different size than
		LaTeX would normally think it has.  The |\raisebox| command
		lets you tell LaTeX how tall it is.
		The first optional argument, extend-above, makes LaTeX think
		that the text extends above the line by the amount specified.
		The second optional argument, extend-below, makes LaTeX think
		that the text extends below the line by the amount specified.

\rule[raiseheight]{width}{thickness} 		*\rule*
		The |\rule| command is used to produce horizontal lines. The
		arguments are defined as follows:
'raiseheight'	specifies how high to raise the rule (optional)
'width'		specifies the length of the rule (mandatory)
'thickness'	specifies the thickness of the rule (mandatory)

\savebox{cmd}[width][pos]{text} 		*\savebox*
		This command typeset text in a box just as for |\makebox|.
		However, instead of printing the resulting box, it saves it in
		bin cmd, which must have been declared with |\newsavebox|.

\sbox{text}					*\sbox*
		This commands typeset text in a box just as for |\mbox|.
		However, instead of printing the resulting box, it saves it in
		bin cmd, which must have been declared with |\newsavebox|.

\usebox{cmd}					*\usebox*
		Prints the box most recently saved in bin cmd by a |\savebox|
		command.

==============================================================================
19. Special Characters				*latex-special*

The following characters play a special role in LaTeX and are called "special
printing characters", or simply "special characters". >
			 #  $  %  &  ~  _  ^  \  {  }
Whenever you put one of these special characters into your file, you are doing
something special. If you simply want the character to be printed just as any
other letter, include a \ in front of the character. For example, \$ will
produce $ in your output.

One exception to this rule is the \ itself because |\\| has its own special
meaning. A \ is produced by typing $\backslash$ in your file.

Also, \~ means `place a tilde accent over the following letter', so you will
probably want to use |\verb| instead.
						*\symbol*
In addition, you can access any character of a font once you know its number
by using the |\symbol| command. For example, the character used for displaying
spaces in the |\verb|* command has the code decimal 32, so it can be typed as
\symbol{32}.

You can also specify octal numbers with ' or hexadecimal numbers with ", so
the previous example could also be written as \symbol{'40} or \symbol{"20}.

==============================================================================
20. Splitting the Input				*latex-inputting*

A large document requires a lot of input. Rather than putting the whole input
in a single large file, it's more efficient to split it into several smaller
ones. Regardless of how many separate files you use, there is one that is the
root file; it is the one whose name you type when you run LaTeX.

|\include| 		Conditionally include a file
|\includeonly| 		Determine which files are included
|\input| 		Unconditionally include a file

\include{file}					*\include*
		The \include command is used in conjunction with the
		|\includeonly| command for selective inclusion of
		files. The file argument is the first name of a file,
		denoting `file.tex' . If file is one the file names in
		the file list of the |\includeonly| command or if there
		is no |\includeonly| command, the \include command is
		equivalent to: >
			\clearpage \input{file} \clearpage
<
		except that if the file `file.tex' does not exist,
		then a warning message rather than an error is
		produced. If the file is not in the file list, the
		\include command is equivalent to |\clearpage|.

		The |\include| command may not appear in the preamble or in a
		file read by another |\include| command.

\includeonly{filelist}	 			*\includeonly*
		The |\includeonly| command controls which files will be read in
		by an |\include| command. {filelist} should be a
		comma-separated list of filenames. Each filename must match
		exactly a filename specified in a |\include| command. This
		command can only appear in the preamble.

\input{file} 					*\input*
		The |\input| command causes the indicated file to be read and
		processed, exactly as if its contents had been inserted in the
		current file at that point. The file name may be a complete
		file name with extension or just a first name, in which case
		the file `file.tex' is used.
==============================================================================
21. Starting & Ending				*latex-start-end*

Your input file must contain the following commands as a minimum:
\documentclass{class} 		|\documentclass|
\begin{document} 		|\begin|
... your text goes here ...
\end{document} 			|\end|

where the class selected is one of the valid classes for LaTeX.
See |\classes|for details of the various document classes.

You may include other LaTeX commands between the |\documentclass| and the
\begin{document} commands (i.e., in the `preamble').
==============================================================================
22. Table of Contents				*latex-toc*

						*\tableofcontents*
A table of contents is produced with the |\tableofcontents| command. You put
the command right where you want the table of contents to go; LaTeX does the
rest for you. It produces a heading, but it does not automatically start a new
page. If you want a new page after the table of contents, include a |\newpage|
command after the |\tableofcontents| command.

						*\listoffigures* *\listoftables*
There are similar commands |\listoffigures| and |\listoftables| for producing a
list of figures and a list of tables, respectively.  Everything works exactly
the same as for the table of contents.

						*\nofiles*
NOTE: If you want any of these items to be generated, you cannot have the
\nofiles command in your document.

|\addcontentsline|	Add an entry to table of contents etc.
|\addtocontents|		Add text directly to table of contents file etc.

\addcontentsline{file}{secunit}{entry}		*\addcontentsline*
		The |\addcontentsline| command adds an entry to the specified
		list or table where:
{file}		is the extension of the file on which information is to be
        	written:
        		toc (table of contents),
        		lof (list of figures),
        		lot (list of tables).
{secunit}	controls the formatting of the entry. It should be one of the
		following, depending upon the value of the file argument:
			toc -- the name of the sectional unit, such as part or
				subsection.
			lof -- figure
			lot -- table
{entry}		is the text of the entry.

\addtocontents{file}{text}				*\addtocontents*
		The |\addtocontents| command adds text (or formatting commands)
		directly to the file that generates the table of contents or
		list of figures or tables.
{file}		is the extension of the file on which information is to be written:
			toc (table of contents),
			lof (list of figures),
			lot (list of tables).
{text}		is the information to be written.

==============================================================================
23. Terminal Input/Output				*latex-terminal*

|\typein|		Read text from the terminal.
|\typeout|		Write text to the terminal.

\typein[cmd]{msg}					*\typein*
		Prints {msg} on the terminal and causes LaTeX to stop and wait
		for you to type a line of input, ending with return. If the
		[cmd] argument is missing, the typed input is processed as if
		it had been included in the input file in place of the
		|\typein| command. If the [cmd] argument is present, it must be
		a command name. This command name is then defined or redefined
		to be the typed input.

\typeout{msg}						*\typeout*
		Prints {msg} on the terminal and in the `.log' file. Commands
		in {msg} that are defined with |\newcommand| or |\renewcommand|
		are replaced by their definitions before being printed.

							*\space*
LaTeX's usual rules for treating multiple spaces as a single space and
ignoring spaces after a command name apply to {msg}. A |\space| command in {msg}
causes a single space to be printed. A ^^J in {msg} prints a newline.

==============================================================================
24. Typefaces					*latex-typefaces*

The typeface is specified by giving the "size" and "style". A typeface is also
called a "font".
|font-styles|		Select roman, italics etc.
|font-size|		Select point size.
|font-lowlevelcommands|	Commands for wizards.

Styles						*font-styles*

The following type style commands are supported by LaTeX.

These commands are used like: >
	\textit{italics text}.
The corresponding command in parenthesis is the "declaration form", which
takes no arguments. The scope of the declaration form lasts until the next
type style command or the end of the current group.

The declaration forms are cumulative; i.e., you can say: >
	\sffamily\bfseries
to get sans serif boldface.

You can also use the environment form of the declaration forms; e.g.: >
	\begin{ttfamily}...\end{ttfamily}.
<
\textrm (\rmfamily)		*\textrm* *\rmfamily*
		Roman

\textit (\itshape)		*\textit* *\itshape* *\emph*
		Emphasis (toggles between |\textit| and |\textrm|).

\textmd (\mdseries)		*\textmd* *\mdseries*
		Medium weight (default). The opposite of boldface.

\textbf (\bfseries)		*\textbf* *\bfseries*
		Boldface.

\textup (\upshape)		*\textup* *\upshape*
		Upright (default).  The opposite of slanted.

\textsl (\slshape)		*\textsl* *\slshape*
		Slanted.

\textsf (\sffamily)		*\textsf* *\sffamily*
		Sans serif.

\textsc (\scshape)		*\textsc* *\scshape*
		Small caps.

\texttt (\ttfamily)		*\texttt* *\ttfamily*
		Typewriter.

\textnormal (\normalfont)	*\textnormal* *\normalfont*
		Main document font.

\mathrm				*\mathrm*
		Roman, for use in math mode.

\mathbf  			*\mathbf*
		Boldface, for use in math mode.

\mathsf				*\mathsf*
		Sans serif, for use in math mode.

\mathtt				*\mathtt*
		Typewriter, for use in math mode.

\mathit				*\mathit*
		Italics, for use in math mode, e.g. variable names with
		several letters.

\mathnormal			*\mathnormal*
		For use in math mode, e.g. inside another type style
		declaration.

\mathcal			*\mathcal*
 `Calligraphic' letters, for use in math mode.

				*\mathversion*
In addition, the command \mathversion{bold} can be used for switching to bold
letters and symbols in formulas. \mathversion{normal} restores the default.

==========
Sizes						*font-size*

The following standard type size commands are supported by LaTeX.

The commands as listed here are "declaration forms". The scope of the
declaration form lasts until the next type style command or the end of the
current group.

You can also use the environment form of these commands; e.g. >
	\begin{tiny}...\end{tiny}

\tiny			        *\tiny*
\scriptsize		        *\scriptsize*
\footnotesize		        *\footnotesize*
\small			        *\small*
\normalsize(default)	        *\normalsize*
\large			        *\large*
\Large			        *\Large*
\LARGE			        *\LARGE*
\huge			        *\huge*
\Huge			        *\Huge*

==========
Low-level font commands				*font-lowlevelcommands*

These commands are primarily intended for writers of macros and packages. The
commands listed here are only a subset of the available ones. For full
details, you should consult Chapter 7 of The LaTeX Companion.

\fontencoding{enc}				*\fontencoding*
		Select font encoding. Valid encodings include OT1 and T1.

\fontfamily{family}  				*\fontfamily*
		Select font family. Valid families include:
			cmr  for Computer Modern Roman
			cmss for Computer Modern Sans Serif
			cmtt for Computer Modern Typewriter
		and numerous others.

\fontseries{series}				*\fontseries*
		Select font series. Valid series include:
			m Medium (normal)
			b Bold
			c Condensed
			bc Bold condensed
			bx Bold extended
		and various other combinations.

\fontshape{shape}				*\fontshape*
		Select font shape. Valid shapes are:
			n Upright (normal)
			it Italic
			sl Slanted (oblique)
			sc Small caps
			ui Upright italics
			ol Outline
		The two last shapes are not available for most font families.

\fontsize{size}{skip}				*\fontsize*
		Set font size. The first parameter is the font size to switch
		to; the second is the \baselineskip to use. The unit of both
		parameters defaults to pt. A rule of thumb is that the
		baselineskip should be 1.2 times the font size.

\selectfont					*\selectfont*
		The changes made by calling the four font commands described
		above do not come into effect until |\selectfont| is called.

\usefont{enc}{family}{series}{shape}		*\usefont*
		Equivalent to calling |\fontencoding|, |\fontfamily|,
		|\fontseries| and |\fontshape| with the given parameters,
		followed by |\selectfont|.

==============================================================================
25. Parameters					*latex-parameters*

The input file specification indicates the file to be formatted; TeX uses
`.tex' as a default file extension. If you omit the input file entirely, TeX
accepts input from the terminal. You specify command options by supplying a
string as a parameter to the command; e.g. >

	latex "\scrollmode\input foo.tex"

will process `foo.tex' without pausing after every error.

Output files are always created in the current directory. When you fail to
specify an input file name, TeX bases the output names on the file
specification associated with the logical name TEX_OUTPUT, typically
texput.log.

 vim:tw=78:ts=8:ft=help:norl:
syntax/bibsearch_atp.vim	[[[1
99
" Vim syntax file
" Language:	bibsearchTeX (bibsearchliographic database format for (La)TeX)
" Author:	Bernd Feige <Bernd.Feige@gmx.net>
" Modified:	Marcin Szamotulski
" Last Change:	Feb 19, 2010

" This is a modification of syntax/bibsearch.vim

" Initialization
" ==============
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
" First we source syntax/bib.vim file
source $VIMRUNTIME/syntax/bib.vim
" Ignore case
syn case ignore

" Keywords
" ========
syn keyword bibsearchType contained	article book booklet conference inbook
syn keyword bibsearchType contained	incollection inproceedings manual
syn keyword bibsearchType contained	mastersthesis misc phdthesis
syn keyword bibsearchType contained	proceedings techreport unpublished
syn keyword bibsearchType contained	string
syn keyword bibsearchEntryKw contained	address author booktitle annote chapter
syn keyword bibsearchEntryKw contained	crossref edition editor issn howpublished
syn keyword bibsearchEntryKw contained	institution fjournal journal key month mrclass 
syn keyword bibsearchEntryKw contained	note number organization pages publisher
syn keyword bibsearchEntryKw contained	school series type title volume year
" Non-standard:
syn keyword bibsearchNSEntryKw contained	abstract isbn issn keywords url

" Clusters
" ========
syn cluster bibsearchVarContents	contains=bibsearchUnescapedSpecial,bibsearchBrace,bibsearchParen
" This cluster is empty but things can be added externally:
"syn cluster bibsearchCommentContents

" Matches
" =======
syn match bibsearchUnescapedSpecial contained /[^\\][%&]/hs=s+1
syn match bibsearchKey contained /\s*[^ \t}="]\+,/hs=s,he=e-1 nextgroup=bibsearchField
syn match bibsearchVariable contained /[^{}," \t=]/
syn region bibsearchQuote contained start=/"/ end=/"/  contains=@bibsearchVarContents
syn region bibsearchBrace contained start=/{/ end=/}/  contains=@bibsearchVarContents
syn region bibsearchParen contained start=/(/ end=/)/  contains=@bibsearchVarContents
syn region bibsearchField contained start="\S\+\s*=\s*" end=/[}),]/me=e-1 contains=bibsearchEntryKw,bibsearchNSEntryKw,bibsearchBrace,bibsearchParen,bibsearchQuote,bibsearchVariable
syn region bibsearchEntryData contained start=/[{(]/ms=e+1 end=/[})]/me=e-1 contains=bibsearchKey,bibsearchField
" Actually, 5.8 <= Vim < 6.0 would ignore the `fold' keyword anyway, but Vim<5.8 would produce
" an error, so we explicitly distinguish versions with and without folding functionality:
if version < 600
  syn region bibsearchEntry start=/@\S\+[{(]/ end=/^\s*[})]/ transparent contains=bibsearchType,bibsearchEntryData nextgroup=bibsearchComment
else
  syn region bibsearchEntry start=/@\S\+[{(]/ end=/^\s*[})]/ transparent fold contains=bibsearchType,bibsearchEntryData nextgroup=bibsearchComment
endif
syn region bibComment2 start=/@Comment[{(]/ end=/^\s*@/me=e-1 contains=@bibsearchCommentContents nextgroup=bibsearchEntry

" Synchronization
" ===============
syn sync match All grouphere bibsearchEntry /^\s*@/
syn sync maxlines=200
syn sync minlines=50

" Bibsearch
" =========
syn region bibsearchComment start=/./ end=/\s*@/me=e-1 contains=@bibsearchCommentContents,@bibsearchSearchInfo nextgroup=bibsearchEntry
syn region bibsearchInfo start=/\s*\d/ end=/\s@/me=e-1 contained containedin=bibsearchComment

" Highlighting defaults
" =====================
" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_bib_syn_inits")
  if version < 508
    let did_bib_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink bibsearchType		bibType
  HiLink bibsearchEntryKw	bibEntryKw
  HiLink bibsearchNSEntryKw	bibNSEntryKw
  HiLink bibsearchKey		bibKey
  HiLink bibsearchVariable	bibVariable
  HiLink bibsearchUnescapedSpecial	bibUnescapedSpecial
  HiLink bibsearchComment	bibComment
  HiLink bibsearchComment2	bibsearchComment
  HiLink bibsearchQuote		bibQuote        
  HiLink bibsearchBrace		bibBrace        
  HiLink bibsearchParen		bibParen        
  delcommand HiLink
endif
let b:current_syntax = "bibsearch"
syntax/toc_atp.vim	[[[1
31
" Vim syntax file
" Language:	toc_atp
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Feb 7
" URL:		

syntax match  atp_filename /^\s*\D.*$/
syntax match  atp_linenumber /^\s*\d\+/ skipwhite nextgroup=atp_number,atp_abstract
syntax match  atp_number /\t\%(\d\+\.\?\)\+/ms=b+1,me=e contained nextgroup=atp_sectiontitle,atp_subsectiontitle 

syntax match atp_abstract /\t\+\s\s\(\S\&\D\).*$/ 

syntax match  atp_chapter /^\s*\d\+\t\+\d\+\s.*/ contains=atp_linenumber,atp_number,atp_chaptertitle
" syntax region atp_chaptertitle matchgroup=atp_chaptertitle start=/\d\s\(\S\&\D\)/ms=e-1 end=/$/me=e contained oneline

syntax match  atp_section /^\s*\d\+\t\+\(\d\+\.\d\+\|\s\{3,}\)\s.\+/ contains=atp_linenumber,atp_number,atp_sectiontitle 
" syntax region atp_sectiontitle matchgroup=atp_sectiontitle start=/\d\s\t\@<!/ms=e+1,ms=e+1 end=/$/me=e contained oneline

syntax match  atp_subsection /^\s*\d\+\t\+\(\d\+\.\d\+\.\d\+\|\s\{5,}\)\s.\+/ contains=atp_linenumber,atp_number,atp_subsectiontitle 
" syntax region atp_subsectiontitle matchgroup=atp_subsectiontitle start=/\d\s\t\@<!/ms=e+1,ms=e+1 end=/$/me=e contained oneline

hi link atp_filename Title
hi link atp_linenumber LineNr
hi link atp_number Number
hi link atp_abstract Label
hi link atp_chapter Label
hi link atp_section Label 
hi link atp_subsection Label
" hi link atp_chaptertitle Title
" hi link atp_sectiontitle Title 
" hi link atp_subsectiontitle Title
syntax/labels_atp.vim	[[[1
15
" Vim syntax file
" Language:	toc_atp
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Feb 7
" URL:		

syntax region atp_label_line start=/^/ end=/$/ transparent contains=atp_label_linenr,atp_label_tab,atp_label_name oneline 
syntax match  atp_label_linenr /^\d\+/ contained nextgroup=atp_label_tab
syntax match  atp_label_tab /\t\+/ contained nextgroup=atp_label_name
syntax region atp_label_name start=/\D\S/ end=/$/ oneline 
syntax match atp_label_filename /^\D.*$/	

hi link atp_label_filename Title
hi link atp_label_linenr LineNr
hi link atp_label_name 	Label
syntax/log_atp.vim	[[[1
104
" syntax clear


syntax keyword texlogKeyword 		LuaTeX LaTeX2e Babel pdfTeXk pdfTeX Web2C pdflatex latex teTeX TeXLive ON OFF 
" Does not work:
" syntax keyword texlogLatexKeyword	LaTeX 			contained

syntax match texlogBrackets		'(\|)\|{\|}\|\[\|\]\|<\|>'

syntax match texlogOpenOut		'\\openout\d\+'

syntax match texlogDate			'\%(\s\|<\)\zs\%(\d\d\s\w\w\w\s\d\d\d\d\s\d\d:\d\d\|\d\d\d\d\/\d\d\/\d\d\)\ze\%(\s\|>\|$\)' 	
syntax match texlogVersion		'\%(v\|ver\)\s*\d*\.\d*\w'
syntax keyword texlogWarningKeyword	obsolete undefined 

syntax match texlogLatexInfo		'LaTeX Info:' 		contains=NONE
syntax match texlogLatexFontInfo	'LaTeX Font Info:' 	contains=NONE
syntax match texlogEndInfo		'Output written on\s\+\%(\S\|\.\|\\\s\|\n\)*' contains=texlogOutputWritten,texlogFileName transparent
syntax match texlogOutputWritten	'Output written on' 	contained 
syntax match texlogPages		'(\zs\_d\+\_s\+pages\ze,\_s*\_d\+\_s\+bytes)'

syntax match texlogPath			'\%(\/\%(\w\|-\|\\\s\|\n\)\+\)\+\%(\.\w\+\)\+'
syntax match texlogFont			'\%(OT\d\|T\d\|OMS\|OML\|U\|OMX\|PD\d\)\n\?\%(\/\_w\+\)\+'
syntax match texlogFontB		'\%(OT\d\|T\d\|OMS\|OML\|U\|OMX\|PD\d\)\n\?+\_w\+'
syntax match texlogFontSize		'<\d\+\%(\.\d\+\)\?>'

syntax match texlogLatexWarning		'LaTeX Warning:' 	contains=NONE
" is visible in synstack but is not highlighted.
syntax match texlogLatexFontWarning	'LaTeX Font Warning:' 	contains=NONE

syntax match texlogPdfTeXWarning	'pdfTeX warning'	contains=NONE

syntax match texlogPackageWarning	'Package\s\+\_w\+\_s\+Warning'
syntax match texlogPackageInfo		'Package\s\+\_w\+\_s\+Info'

syntax match texlogError		'^!.*$'

syntax match texlogOverfullBox		'Overfull\_s\\[hv]box'
syntax match texlogUnderfullBox		'Underfull\_s\\[hv]box'
syntax match texlogTooWide		'too\_swide'

syntax match texlogLineNr		'\%(^l\.\|input\_sline\_s*\)\_d\+\|lines\_s\+\_d\+--\_d\+'
syntax match texlogPageNr		'\[\_d\+\%(\_s*{[^}]*}\)\?\]\|page\_s\_d\+'

syntax match texlogDocumentClass	'Document Class:\s\+\S*'
syntax match texlogPackage		'Package:\s\+\S*'
syntax match texlogFile			'File:\s\+\S*'
syntax match texlogFileName		'\/\zs\%(\w\|\\\s\|-\|\n\)\+\.\%(dvi\|pdf\|ps\)' contained
syntax match texlogCitation		'Citation\s\+\`[^']*\'' 	contains=texlogScope
syntax match texlogReference		'Reference\s\+\`[^']*\'' 	contains=texlogScope
syntax match texlogScope		'\%(\`\|\'\)[^']*\''

syntax match texlogFontShapes		'\Cfont shapes' 
syntax match texlogNotAvailable		'not available'

" syntax match texlogDelimiter		'(\|)'
"
" This works only with 'sync fromstart' which is slow for long log files.
syntax region texlogBracket	start="(" skip="\\[()]"	end=")" transparent contains=texlogPath,texlogLatexInfo,texlogFontInfo,texlogEndInfo,texlogOutputWritten,texlogLatexWarning,texlogLatexFontInfo,texlogLatexFontWarning,texlogPackageWarning,texlogPackageInfo,texlogError,texlogLineNr,texlogPageNr,texlogPackage,texlogDocumentClass,texlogFile,texlogCitation,texlogReference,texlogKeyword,texlogLatexKeyword,texlogScope,texlogFont,texlogFontB,texlogFontSize,texlogOverfullBox,texlogUnderfullBox,texlogTooWide,texlogDate,texlogVersion,texlogWarningKeyword,texlogPages,texlogFontShapes,texlogNotAvailable,texlogOpenOut,texlogPdfTeXWarning,texlogBrackets

syntax sync fromstart 

hi def link texlogKeyword		Keyword
hi def link texlogLatexKeyword		Keyword
hi def link texlogBrackets		Special
hi def link texlogOpenOut		Statement
hi def link texlogWarningKeyword	Identifier
hi def link texlogPath			Include

hi def link texlogLatexInfo 		String

hi def link texlogOutputWritten		String
hi def link texlogFileName		Identifier
hi def link texlogPages			Identifier

hi def link texlogLatexFontInfo 	String
hi def link texlogLatexWarning 		Identifier
hi def link texlogLatexFontWarning 	Identifier
hi def link texlogPackageWarning	Identifier
hi def link texlogPdfTeXWarning		Identifier
hi def link texlogPackageInfo 		String
hi def link texlogError 		Error

hi def link texlogLineNr		Number
hi def link texlogPageNr		Number

hi def link texlogDocumentClass		String
hi def link texlogPackage		String
hi def link texlogFile			String
hi def link texlogCitation		String
hi def link texlogReference		String
hi def link texlogOverfullBox		Function
hi def link texlogUnderfullBox		Function
hi def link texlogTooWide		Function
hi def link texlogScope			Label 

hi def link texlogFont			Label
hi def link texlogFontB			Label
hi def link texlogFontSize		Label
hi def link texlogFontShapes		String
hi def link texlogNotAvailable		Identifier

hi def link texlogDate			Number
hi def link texlogVersion		Number
colors/coots-beauty-256.vim	[[[1
238
" Vim color file
" Maintainer:	Marcin Szamotulski  <mszamot at gmail dot com>
" Last Change:	2009 Feb 19
" Version:	1.0.0
" URL:		http://www.axisym3.net/jdany/vim-the-editor/#ocean237256
"
" These are the colors of the "Ocean237" theme by Chris Vertonghen modified
" to work on 256-color xterms.
"
set background=dark

highlight clear
if exists("syntax_on")
    syntax reset
endif

"let g:colors_name = "coot-256"

highlight Normal         cterm=none           ctermfg=250 ctermbg=233	guifg=white	guibg=#1c1c1c
highlight NonText        cterm=none           ctermfg=105 ctermbg=233	guifg=#1c1c1c	guibg=#1c1c1c 

highlight Visual         		      ctermbg=238				guibg=gray35
highlight VisualNOS      cterm=bold,underline ctermfg=57  ctermbg=233

highlight Cursor         cterm=none           ctermfg=15  ctermbg=93	guifg=#000000	guibg=#8A4C98
highlight CursorIM       cterm=bold           ctermfg=15  ctermbg=93	guifg=#000000	guibg=#8A4C98
"highlight CursorColumn
"highlight CursorLine

highlight Directory      			ctermfg=5   ctermbg=233	guifg=DarkViolet	guibg=#1c1c1c

highlight DiffAdd        cterm=none           	ctermfg=15  ctermbg=56  guifg=white	guibg=SlateBlue4 gui=bold
highlight DiffDelete     cterm=none           	ctermfg=19  ctermbg=56	guifg=VioletRed	guibg=SlateBlue4
highlight DiffChange     cterm=none           	ctermfg=173 ctermbg=125	guifg=salmon	guibg=DeepPink4
highlight DiffText       cterm=bold           	ctermfg=white ctermbg=125  guifg=white	guibg=DeepPink4

highlight Question       cterm=bold           	ctermfg=33  ctermbg=233 guifg=#0087ff	guibg=#1c1c1c
highlight ErrorMsg       cterm=bold            	ctermfg=160 ctermbg=233 guifg=#d70000	guibg=#1c1c1c
highlight ModeMsg              			ctermfg=33  ctermbg=233 guifg=#0087ff	guibg=#1c1c1c
highlight MoreMsg        	           	ctermfg=39  ctermbg=233 guifg=#00afff	guibg=#1c1c1c
highlight WarningMsg    cterm=bold           	ctermfg=161 ctermbg=233 guifg=#d7005f	guibg=#1c1c1c

highlight LineNr                              	ctermfg=57 ctermbg=233	guifg=#837598	guibg=#1c1c1c
highlight Folded  				ctermfg=57 ctermbg=233	guifg=#837598	guibg=#1a1a1a
highlight FoldColumn     cterm=none           	ctermfg=green ctermbg=233 guifg=#5CB80C guibg=#1c1c1c
"highlight SignColumn

highlight Search         cterm=bold           	ctermfg=black  	ctermbg=226	guifg=black guibg=yellow
highlight IncSearch      cterm=bold        	ctermfg=black  	ctermbg=red	guifg=gold guibg=#1c1c1c
highlight MatchParen     			ctermfg=233	ctermbg=226	guifg=#1c1c1c guibg=gold

highlight PMenu          ctermbg=18 ctermfg=39  
highlight PMenuSel       ctermbg=39 ctermfg=18
highlight PMenuSBar      ctermbg=white ctermfg=33
highlight PMenuThumb     ctermbg=white ctermfg=33

highlight SpecialKey     ctermfg=129    ctermbg=233			 guifg=DarkViolet
highlight StatusLine     cterm=none     ctermfg=226 ctermbg=232		 guifg=#111111 guibg=wheat1
highlight StatusLineNC   cterm=none     ctermfg=245 ctermbg=232		 guifg=#111111 guibg=snow4	 
highlight default User1		 cterm=bold	ctermfg=226 ctermbg=232	gui=bold guifg=DeepPink4  	guibg=#111111	 
highlight default User2		 cterm=none	ctermfg=red ctermbg=232		 guifg=Khaki1  		guibg=#111111
highlight default User3		 cterm=none	ctermfg=226 ctermbg=232		 guifg=BlueViolet   	guibg=#111111

highlight VertSplit      cterm=none     ctermfg=green   ctermbg=233	 guifg=#1c1c1c	  guibg=DeepSkyBlue4
highlight WildMenu       cterm=bold     ctermfg=0   ctermbg=118

highlight Title          cterm=bold           	ctermfg=226 	ctermbg=232

"highlight Menu
"highlight Scrollbar
"highlight Tooltip

"          Syntax         Groups
highlight Comment        cterm=none           	ctermfg=90 ctermbg=233		guifg=Magenta4

highlight Constant       ctermfg=125          	ctermbg=233			guifg=DeepPink3
highlight String         cterm=none           	ctermfg=27   ctermbg=233	guifg=RoyalBlue1
"highlight Character
highlight Number         cterm=none           	ctermfg=161  ctermbg=233	guifg=DeepPink2
highlight Boolean        cterm=none           	ctermfg=161  ctermbg=233	guifg=DeepPink1
"highlight Float

highlight Identifier     		      	ctermfg=39			guifg=DodgerBlue
highlight Function       cterm=none           	ctermfg=51   ctermbg=233	guifg=Turquoise1

highlight Statement      cterm=none           	ctermfg=135			guifg=MediumOrchid
"248
highlight Conditional    cterm=none           	ctermfg=27   ctermbg=233	guifg=SlateBlue2
highlight Repeat         cterm=none           	ctermfg=82   ctermbg=233
"highlight Label
highlight Operator       cterm=none	      	ctermfg=40   ctermbg=233	guifg=Chartreuse1
highlight Keyword        cterm=none           	ctermfg=197  ctermbg=233	guifg=DeepPink1
highlight Exception      cterm=none           	ctermfg=82   ctermbg=233	guifg=Chartreuse1

highlight PreProc        ctermfg=82						guifg=DeepPink1
highlight Include        cterm=none           	ctermfg=130  ctermbg=233
highlight Define         cterm=none           	ctermfg=39   ctermbg=233
highlight Macro          cterm=none           	ctermfg=39   ctermbg=233
highlight PreCondit      cterm=bold           	ctermfg=125  ctermbg=233

"jak mutt odpala vima i \bf,\textrm itd:
highlight Type           cterm=none           	ctermfg=82               	guifg=LawnGreen
highlight StorageClass   cterm=none           	ctermfg=21   ctermbg=233
highlight Structure      cterm=none           	ctermfg=21   ctermbg=233
highlight Typedef        cterm=none           	ctermfg=21   ctermbg=233

" $, $$:
highlight Special        cterm=none	      	ctermfg=93			guifg=BlueViolet
"249
"tex math mode
"highlight SpecialChar
"highlight Tag:
"highlight Delimiter
"highlight SpecialComment
"highlight Debug

highlight Underlined     cterm=underline      	ctermfg=102 ctermbg=233		gui=underline
highlight Ignore         ctermfg=67

"highlight SpellBad       ctermfg=21           	ctermbg=233
"highlight SpellCap       ctermfg=19           	ctermbg=233
"highlight SpellRare      ctermfg=18           	ctermbg=233
"highlight SpellLocal     ctermfg=17           	ctermbg=233

highlight Todo           ctermfg=21           ctermbg=233	guifg=DeepPink guibg=#1c1c1c	gui=underline,bold

highlight helpNormal		ctermbg=235
highlight helpHyperTextJump 	ctermfg=57
highlight helpBar 		ctermfg=57
highlight helpStar		ctermfg=27	

highlight TabLine	cterm=none	ctermfg=white 	ctermbg=240
highlight TabLineFill 	cterm=none	ctermfg=white 	ctermbg=240
highlight TabLineSel	cterm=bold	ctermfg=white	ctermbg=57
"highlight TabLineSel	cterm=bold	ctermfg=white	ctermbg=197
" \command
highlight texDelimiter			ctermfg=161	ctermbg=233	guifg=MediumVioletRed
" \begin, \end:
highlight texSectionMarker		ctermfg=238	ctermbg=233	guifg=FireBrick		gui=bold
highlight texSection	cterm=bold	ctermfg=242	ctermbg=233	guifg=FireBrick2	gui=bold
" highlight texSectionName						guifg=FireBrick
highlight texDocType			ctermfg=90	ctermbg=233	guifg=DeepPink4
highlight texInputFile			ctermfg=90	ctermbg=233	guifg=DeepPink4
highlight texDocTypeArgs		ctermfg=204	ctermbg=233	guifg=DeepPink2
highlight texInputFileopt		ctermfg=204	ctermbg=233	guifg=DeepPink2
highlight texType			ctermfg=40	ctermbg=233	guifg=green3
highlight texMath			ctermfg=245	ctermbg=233	guifg=DarkKhaki
highlight texStatement 			ctermfg=245	ctermbg=233	guifg=DeepPink3
highlight texString			ctermfg=39	ctermbg=233	guifg=DodgerBlue
highlight texSpecialChar		ctermfg=39	ctermbg=233	guifg=DodgerBlue
highlight texRefZone							guifg=DeepPink2		gui=bold
highlight texCite							guifg=DeepPink4
highlight texRefOption							guifg=HotPink4

" Testing:
hi texBfText 		gui=bold
hi texItText 		gui=italic
hi texUnderlineText 	gui=underline

" \chapter, \section, ... {theorem} {definition}

highlight Error          ctermfg=196         	ctermbg=233
highlight SpellErrors  	 cterm=underline      	ctermfg=darkred ctermbg=233
highlight SpellBad       ctermfg=196         	ctermbg=233
highlight SpellCap       ctermfg=202         	ctermbg=233
highlight SpellRare      ctermfg=203         	ctermbg=233
highlight SpellLocal     ctermfg=202         	ctermbg=233

hi bibsearchInfo 	ctermfg=33			guifg=DeepPink
hi bibsearchComment	cterm=bold 	ctermfg=27	guifg=blue		gui=bold 
hi bibComment2		cterm=bold 	ctermfg=30	guifg=SeaGreen4		gui=bold
hi bibsearchCommentContents cterm=none	ctermfg=30	guifg=SeaGreen4		gui=none
hi bibsearchType			ctermfg=24	guifg=MediumVioletRed
" hi bibsearchEntryData						ctermfg=magenta
hi bibsearchKey		cterm=bold 		ctermfg=white	guifg=white	gui=bold
hi bibsearchEntry 				ctermfg=33	guifg=DeepSkyBlue
    hi bibsearchField 				ctermfg=green	guifg=SlateBlue4
	hi bibsearchEntryKw			ctermfg=white	guifg=SlateBlue3
	hi bibsearchBrace			cterm=bold	guifg=white gui=bold
	hi bibsearchVariable 			ctermfg=white	guifg=white

" ATP toc file
highlight atp_filename						guifg=FireBrick
highlight atp_linenumber	cterm=bold	ctermfg=27	guifg=PeachPuff4
highlight atp_number 				ctermfg=33	guifg=sienna
highlight atp_chapter 		cterm=bold 	ctermfg=white	guifg=seashell2		gui=bold
highlight atp_section				ctermfg=30	guifg=seashell4
highlight atp_subsection			ctermfg=24	guifg=seashell4
highlight atp_abstract	cterm=bold	ctermfg=gray		guifg=seashell2		gui=bold

" ATP label file
highlight atp_label_filename					guifg=DeepPink4		gui=bold
highlight atp_label_linenr cterm=bold	ctermfg=white		guifg=maroon
" highlight atp_label_name 		ctermfg=green		guifg=chartreuse
highlight atp_label_name 					guifg=DeepPink2		gui=bold

highlight atp_statusline 	cterm=bold	ctermfg=green 	ctermbg=233

highlight atp_statustitle 	cterm=bold	ctermfg=grey 	ctermbg=233  
highlight atp_statussection 	cterm=bold	ctermfg=yellow 	ctermbg=233  
highlight atp_statusoutdir 			ctermfg=grey 	ctermbg=233 

highlight link atp_Todo Normal

" hi texlogKeyword		Keyword
hi texlogPath			guifg=gray38 gui=bold

hi texlogLatexInfo 		guifg=DeepPink4
hi texlogEndInfo 		guifg=DeepPink4
hi texlogLatexFontInfo 		guifg=DeepPink4
hi texlogLatexWarning 		guifg=white gui=bold
hi texlogLatexFontWarning 	guifg=white gui=bold
hi texlogPackageWarning		guifg=white gui=bold
hi texlogPackageInfo 		guifg=DeepPink3
" hi texlogError 		Error
hi texlogLineNr			guifg=SlateBlue3
hi texlogPageNr			guifg=SlateBlue3

hi texlogDocumentClass		guifg=DeepPink3
hi texlogPackage		guifg=DeepPink3
hi texlogFile			guifg=DeepPink3
hi texlogCitation		guifg=DeepPink3
hi texlogReference		guifg=DeepPink3
hi texlogScope			guifg=green1 



highlight ywtxt_todo	guifg=yellow gui=bold
highlight ywtxt_note	guifg=yellow gui=bold

highlight ywtxt_heading1 guifg=slateblue1 gui=bold 
highlight ywtxt_heading2 guifg=slateblue gui=bold 
highlight ywtxt_heading3 guifg=slateblue4 gui=bold 
highlight ywtxt_heading4 guifg=darkslateblue gui=bold 
highlight ywtxt_bold 		cterm=bold 	gui=bold
highlight ywtxt_italic 		cterm=italic  	gui=italic
highlight ywtxt_underline 	cterm=underline gui=underline
highlight ywtxt_comment guifg=honeydew4
