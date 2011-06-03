" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/tex_LatexBox.vim	[[[1
21
" LaTeX Box plugin for Vim
" Maintainer: David Munger
" Email: mungerd@gmail.com
" Version: 0.3.3

if !exists('s:loaded')

	let prefix = expand('<sfile>:p:h') . '/latex-box/'

	execute 'source ' . fnameescape(prefix . 'common.vim')
	execute 'source ' . fnameescape(prefix . 'complete.vim')
	execute 'source ' . fnameescape(prefix . 'motion.vim')
	execute 'source ' . fnameescape(prefix . 'latexmk.vim')

	let s:loaded = 1

endif

execute 'source ' . fnameescape(prefix . 'mappings.vim')

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/common.vim	[[[1
160
" LaTeX Box common functions

" Settings {{{

" Compilation {{{
let g:LatexBox_latexmk_options = ''
let g:LatexBox_output_type = 'pdf'
let g:LatexBox_viewer = 'xdg-open'
" }}}

" Completion {{{
let g:LatexBox_completion_close_braces = 1
let g:LatexBox_bibtex_wild_spaces = 1

let g:LatexBox_completion_environments = [
	\ {'word': 'itemize',		'menu': 'bullet list' },
	\ {'word': 'enumerate',		'menu': 'numbered list' },
	\ {'word': 'description',	'menu': 'description' },
	\ {'word': 'center',		'menu': 'centered text' },
	\ {'word': 'figure',		'menu': 'floating figure' },
	\ {'word': 'table',			'menu': 'floating table' },
	\ {'word': 'equation',		'menu': 'equation (numbered)' },
	\ {'word': 'align',			'menu': 'aligned equations (numbered)' },
	\ {'word': 'align*',		'menu': 'aligned equations' },
	\ ]

let g:LatexBox_completion_commands = [
	\ {'word': '\begin{' },
	\ {'word': '\end{' },
	\ {'word': '\item' },
	\ {'word': '\label{' },
	\ {'word': '\ref{' },
	\ {'word': '\eqref{' },
	\ {'word': '\cite{' },
	\ {'word': '\nonumber' },
	\ {'word': '\bibliography' },
	\ {'word': '\bibliographystyle' },
	\ ]
" }}}

" Templates {{{
let g:LatexBox_templates = {
				\ 'document':	{},
				\ 'abstract':	{},
				\ 'itemize':	{'template': "\<Tab>\\item "},
				\ 'enumerate':	{'template': "\<Tab>\\item "},
				\ 'figure':	 	{'label': 'fig:', 'options': '[htb]'},
				\ 'table':		{'label': 'tab:', 'options': '[htb]'},
				\ 'tabular':	{'options': '[cc]'},
				\ 'center':	 	{},
				\ 'equation':	{'label': 'eq:'},
				\ 'align':		{'label': 'eq:'},
				\ 'gather':		{'label': 'eq:'},
				\ }
" }}}

" }}}

" Filename utilities {{{
"
function! LatexBox_GetMainTexFile()

	" 1. check for the b:main_tex_file variable
	if exists('b:main_tex_file') && glob(b:main_tex_file, 1) != ''
		return b:main_tex_file
	endif

	" 2. scan current file for "\begin{document}"
	if &filetype == 'tex' && search('\\begin\_\s*{document}', 'nw') != 0
		return expand('%:p')
	endif

	" 3. prompt for file with completion
	let b:main_tex_file = s:PromptForMainFile()
	return b:main_tex_file
endfunction

function! s:PromptForMainFile()
	let saved_dir = getcwd()
	execute 'cd ' . expand('%:p:h')
	let l:file = ''
	while glob(l:file, 1) == ''
		let l:file = input('main LaTeX file: ', '', 'file')
		if l:file !~ '\.tex$'
			let l:file .= '.tex'
		endif
	endwhile
	execute 'cd ' . saved_dir
	return l:file
endfunction

" Return the directory of the main tex file
function! LatexBox_GetTexRoot()
	return fnamemodify(LatexBox_GetMainTexFile(), ':h')
endfunction

"!function! LatexBox_GetTexFile()
"!	if &filetype != 'tex'
"!		echomsg 'not a tex file'
"!		return ''
"!	endif
"!	return expand("%:p")
"!endfunction

function! LatexBox_GetTexBasename(with_dir)
	if a:with_dir
		return fnamemodify(LatexBox_GetMainTexFile(), ':r')
	else
		return fnamemodify(LatexBox_GetMainTexFile(), ':t:r')
	endif
endfunction

function! LatexBox_GetAuxFile()
	return LatexBox_GetTexBasename(1) . '.aux'
endfunction

function! LatexBox_GetLogFile()
	return LatexBox_GetTexBasename(1) . '.log'
endfunction

function! LatexBox_GetOutputFile()
	return LatexBox_GetTexBasename(1) . '.' . g:LatexBox_output_type
endfunction
" }}}

" FIXME: remove this
"!" GetAuxIncludedFiles {{{
"!function! LatexBox_GetAuxIncludedFiles(auxfile)
"!
"!	let files = []
"!	let prefix = fnamemodify(a:auxfile, ':p:h')
"!
"!	for line in readfile(a:auxfile)
"!		let newaux = matchstr(line, '^\\@input{\zs[^}]*\ze}')
"!		if newaux != ''
"!			call add(files, prefix . '/' . newaux)
"!		endif
"!	endfor
"!
"!	return files
"!
"!endfunction

" }}}

" View {{{
function! LatexBox_View()
	let outfile = LatexBox_GetOutputFile()
	if !filereadable(outfile)
		echomsg fnamemodify(outfile, ':.') . ' is not readable'
		return
	endif
	silent execute '!' . g:LatexBox_viewer ' ' . shellescape(LatexBox_GetOutputFile()) . ' &'
endfunction

command! LatexView			call LatexBox_View()
" }}}


" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/complete.vim	[[[1
344
" LaTeX Box completion

" <SID> Wrap {{{
function! s:GetSID()
	return matchstr(expand('<sfile>'), '\zs<SNR>\d\+_\ze.*$')
endfunction
let s:SID = s:GetSID()
function! s:SIDWrap(func)
	return s:SID . a:func
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

		if line[0:pos-1] =~ '\\begin\_\s*{$'
			let s:completion_type = 'begin'
		elseif line[0:pos-1] =~ '\\end\_\s*{$'
			let s:completion_type = 'end'
		elseif line[0:pos-1] =~ '\\\(eq\)\?ref\_\s*{$'
			let s:completion_type = 'ref'
		elseif line[0:pos-1] =~ '\\cite\(p\|t\)\?\_\s*{$'
			let s:completion_type = 'bib'
		else
			let s:completion_type = 'command'
			if line[pos-1] == '\'
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
					if g:LatexBox_completion_close_braces
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
			let env = s:GetLastUnclosedEnv()
			if env != ''
				if g:LatexBox_completion_close_braces
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
			return s:CompleteLabels(a:base)
		elseif s:completion_type == 'bib'
			" suggest BibTeX entries
			return LatexBox_BibComplete(a:base)
		endif
		return suggestions
	endif
endfunction
" }}}


" BibTeX search {{{

" find the \bibliography{...} command
function! s:FindBibData()
    "FIXME: use main tex file
	for line in readfile(LatexBox_GetMainTexFile())
    	let bibdata = matchstr(line, '\\bibliography\_\s*{\zs[^}]*\ze}')
		if !empty(bibdata)
			return bibdata
		endif
	endfor
	return ''
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
	let tmpbase = LatexBox_GetTexRoot() . '/_LatexBox_BibComplete'
    let auxfile = tmpbase . '.aux'
    let bblfile = tmpbase . '.bbl'
    let blgfile = tmpbase . '.blg'

    call writefile(['\citation{*}', '\bibstyle{' . s:bstfile . '}', '\bibdata{' . bibdata . '}'], auxfile)
    silent execute '! cd ' shellescape(LatexBox_GetTexRoot()) . ' ; bibtex -terse ' . fnamemodify(auxfile, ':t')

    let res = []
    let curentry = ''
    for l:line in readfile(bblfile)
        if l:line =~ '^\s*$'

            " process current entry
			
            if empty(curentry) || curentry !~ a:regexp
				" skip entry if void or doesn't match
				let curentry = ''
                continue
            endif
            let matches = matchlist(curentry, '^{\(.*\)}{\(.*\)}{\(.*\)}{\(.*\)}{\(.*\)}.*')
            if !empty(matches) && !empty(matches[1])
                call add(res, {'key': matches[1], 'type': matches[2],
							\ 'author': matches[3], 'year': matches[4], 'title': matches[5]})
            endif
            let curentry = ''
        else
            let curentry .= l:line
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
		let regexp = substitute(a:regexp, '\s\+', '.*', 'g')
	else
		let regexp = a:regexp
	endif

    let res = []
    for m in LatexBox_BibSearch(regexp)
        let w = {'word': m['key'],
					\ 'abbr': '[' . m['type'] . '] ' . m['author'] . ' (' . m['year'] . ')',
					\ 'menu': m['title']}
		if g:LatexBox_completion_close_braces
			" add trailing '}'
			let w.word = w.word . '}'
		endif
        call add(res, w)
    endfor
    return res
endfunction
" }}}


" Complete Labels {{{
function! s:CompleteLabels(regex, ...)

	let suggestions = []

	if a:0 == 0
		let auxfile = LatexBox_GetAuxFile()
	else
		let auxfile = a:1
	endif

	let prefix = fnamemodify(auxfile, ':p:h')

	" search for the target equation number
	for line in readfile(auxfile)
		let matches = matchlist(line, '^\\newlabel{\(' . a:regex . '[^}]*\)}{{\([^}]*\)}{\([^}]*\)}.*}')
		if !empty(matches)

			let entry = {'word': matches[1], 'menu': '(' . matches[2] . ') [p.' . matches[3] . ']'}

			if g:LatexBox_completion_close_braces
				" add trailing '}'
				let entry = copy(entry)
				let entry.abbr = entry.word
				let entry.word = entry.word . '}'
			endif
			call add(suggestions, entry)
		endif

		let included_auxfile = matchstr(line, '^\\@input{\zs[^}]*\ze}')
		if included_auxfile != ''
			let included_auxfile = prefix . '/' . included_auxfile
			call extend(suggestions, s:CompleteLabels(a:regex, included_auxfile))
		endif
	endfor

	return suggestions

endfunction
" }}}



" Find tex label by number {{{
function! LatexBox_FindLabelByNumber(regex, number)

	let auxfiles = [LatexBox_GetAuxFile()]

	while !empty(auxfiles)

		let auxfile = auxfiles[0]

		" search for the target equation number
		for line in readfile(auxfile)
			let label = matchstr(line, '^\\newlabel{\zs' . a:regex . '[^}]*\ze}{{' . a:number . '}')
			if label != ''
				return label
			endif
		endfor

		call extend(auxfiles, LatexBox_GetAuxIncludedFiles(auxfile))
		call remove(auxfiles, 0)

	endwhile

	" no match found; return the empty string
	return ''

endfunction
" }}}

" Find tex label by number with prompt {{{
function! LatexBox_FindLabelByNumberPrompt()

	let regex = input('label prefix: ', '', 'customlist,' . s:SIDWrap('GetLabelTypes'))
	let number = input('label number: ')
	return LatexBox_FindLabelByNumber(regex, number)
endfunction

function! s:GetLabelTypes(lead, cmdline, pos)
	let l:label_types = ['eq:', 'fig:', 'tab:']
	let suggestions = []
	for l:w in l:label_types
		if l:w =~ '^' . a:lead
			call add(suggestions, l:w)
		endif
	endfor
	return suggestions
endfunction
" }}}

" Templates {{{

function! s:GetTemplateList(lead, cmdline, pos)
	let suggestions = []
	for env in keys(g:LatexBox_templates)
		if env =~ '^' . a:lead
			call add(suggestions, env)
		endif
	endfor
	return suggestions
endfunction


function! LatexBox_Template(env, close)
	let envdata = get(g:LatexBox_templates, a:env, {})

	let text = '\begin{' . a:env . '}'

	if has_key(envdata, 'options')
		let text .= envdata.options
	endif
	if a:close
		let text .= "\<End>\n" . '\end{' . a:env . '}' . "\<Up>\<End>"
	endif
	if has_key(envdata, 'template')
		let text .= "\n" . envdata.template
	endif
	if has_key(envdata, 'label')
		let text .= "\n" . '\label{' . envdata.label . '}' . "\<Left>"
	endif

	return text
endfunction

function! LatexBox_TemplatePrompt(close)
	let env = input('Environment: ', '', 'customlist,' . s:SIDWrap('GetEnvList'))
	return LatexBox_Template(env, a:close)
endfunction

function! s:GetLastUnclosedEnv()
	let begin_line = searchpair('\\begin\_\s*{[^}]*}', '', '\\end\_\s*{[^}]*}', 'bnW')
	if begin_line
		let env = matchstr(getline(begin_line), '\\begin\_\s*{\zs[^}]*\ze}')
		return env
	else
		return ''
	endif
endfunction

" }}}

" Close Last Environment {{{
function! LatexBox_CloseLastEnv()
	let env = s:GetLastUnclosedEnv()
	if env != ''
		return '\end{' . env . '}'
	else
		return ''
	endif
endfunction
" }}}

" Wrap Selection {{{
function! LatexBox_WrapSelection(wrapper)
	normal `>a}
	exec 'normal `<i\'.a:wrapper.'{'
endfunction
" }}}


" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/latexmk.vim	[[[1
97
" LaTeX Box latexmk functions


" <SID> Wrap {{{
function! s:GetSID()
	return matchstr(expand('<sfile>'), '\zs<SNR>\d\+_\ze.*$')
endfunction
let s:SID = s:GetSID()
function! s:SIDWrap(func)
	return s:SID . a:func
endfunction
" }}}


" list of log files for which latexmk is running
let s:latexmk_running_list = []

" Callback {{{
function! s:LatexmkCallback(status, log)
	"let pos = getpos('.')
	execute 'cgetfile ' . a:log
	if a:status
		echomsg "latexmk exited with status " . a:status
	else
		echomsg "latexmk finished"
	endif
	call remove(s:latexmk_running_list, index(s:latexmk_running_list, a:log))
	"call setpos('.', pos)
endfunction
" }}}

" Latexmk {{{
function! LatexBox_Latexmk(force)

	let log = LatexBox_GetLogFile()

	if index(s:latexmk_running_list, log) >= 0
		echomsg "latexmk is already running (" . fnamemodify(log, ':.') . ")"
		return
	endif

	let l:callback = s:SIDWrap('LatexmkCallback')

	let l:options = '-' . g:LatexBox_output_type . ' -quiet ' . g:LatexBox_latexmk_options
	if a:force
		let l:options .= ' -g'
	endif
	let l:options .= " -e '$pdflatex =~ s/ / -file-line-error /'"
	let l:options .= " -e '$latex =~ s/ / -file-line-error /'"

	let l:cmd = 'cd ' . LatexBox_GetTexRoot() . ' ; latexmk ' . l:options . ' ' . LatexBox_GetMainTexFile()
	let l:vimcmd = v:progname . ' --servername ' . v:servername . ' --remote-expr ' . 
				\ shellescape(l:callback) . '\($?,\"' . log . '\"\)'

	call add(s:latexmk_running_list, log)
	silent execute '! ( ( ' . l:cmd . ' ) ; ' . l:vimcmd . ' ) &'
endfunction
" }}}

" LatexmkClean {{{
function! LatexBox_LatexmkClean(cleanall)

	if a:cleanall
		let l:options = '-C'
	else
		let l:options = '-c'
	endif

	let l:cmd = 'cd ' . LatexBox_GetTexRoot() . ' ; latexmk ' . l:options . ' ' . LatexBox_GetMainTexFile()

	silent execute '! ( ' . l:cmd . ' )'
	echomsg "latexmk clean finished"
endfunction
" }}}

" LatexmkStatus {{{
function! LatexBox_LatexmkStatus()

	let log = LatexBox_GetLogFile()

	if index(s:latexmk_running_list, log) >= 0
		echo "latexmk is running (" . fnamemodify(log, ':.') . ")"
	else
		echo "latexmk is not running"
	endif
endfunction
" }}}

" Commands {{{
command! Latexmk			call LatexBox_Latexmk(0)
command! LatexmkForce		call LatexBox_Latexmk(1)
command! LatexmkClean		call LatexBox_LatexmkClean(0)
command! LatexmkCleanAll	call LatexBox_LatexmkClean(1)
command! LatexmkStatus		call LatexBox_LatexmkStatus()
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/mappings.vim	[[[1
49
" LaTeX Box mappings

" latexmk {{{
map <buffer> <LocalLeader>ll :Latexmk<CR>
map <buffer> <LocalLeader>lL :LatexmkForce<CR>
map <buffer> <LocalLeader>lc :LatexmkClean<CR>
map <buffer> <LocalLeader>lC :LatexmkCleanAll<CR>
map <buffer> <LocalLeader>lg :LatexmkStatus<CR>
" }}}

" View {{{
map <buffer> <LocalLeader>lv :LatexView<CR>
" }}}

" Error Format {{{
" This assumes we're using the -file-line-error with [pdf]latex.
setlocal efm=%E%f:%l:%m,%-Cl.%l\ %m,%-G
" }}}

" TOC {{{
command! LatexTOC call LatexBox_TOC()
map <silent> <buffer> <LocalLeader>lt :LatexTOC<CR>
" }}}

setlocal omnifunc=LatexBox_Complete

finish

" Suggested mappings:

" Motion {{{
map <silent> <buffer> ¶ :call LatexBox_JumpToNextBraces(0)<CR>
map <silent> <buffer> § :call LatexBox_JumpToNextBraces(1)<CR>
imap <silent> <buffer> ¶ <C-R>=LatexBox_JumpToNextBraces(0)<CR>
imap <silent> <buffer> § <C-R>=LatexBox_JumpToNextBraces(1)<CR>
" }}}

" begin/end {{{
imap <buffer> <silent> <F5> <C-R>=LatexBox_TemplatePrompt(1)<CR>
imap <buffer> <silent> [[ \begin{
imap <buffer> <silent> ]] <C-R>=LatexBox_CloseLastEnv()<CR>
vmap <buffer> <silent> <F7> <Esc>:call LatexBox_WrapSelection('')<CR>i
" }}}

" Other commands {{{
imap <buffer> <F11> <C-R>=LatexBox_FindLabelByNumberPrompt()<CR>
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/motion.vim	[[[1
137
" LaTeX Box motion functions

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
	if next =~ '[]}]' && prev !~ '[[{]'
		return "\<Right>"
	else
		return ''
	endif
endfunction
" }}}

" Table of Contents {{{
function! s:ReadTOC(auxfile)

	let toc = []

	let prefix = fnamemodify(a:auxfile, ':p:h')

	for line in readfile(a:auxfile)

		let included = matchstr(line, '^\\@input{\zs[^}]*\ze}')
		
		if included != ''
			call extend(toc, s:ReadTOC(prefix . '/' . included))
			continue
		endif

		let m = matchlist(line,
					\ '^\\@writefile{toc}{\\contentsline\s*' .
					\ '{\([^}]*\)}{\\numberline {\([^}]*\)}\(.*\)')

		if !empty(m)
			let str = m[3]
			let nbraces = 0
			let istr = 0
			while nbraces >= 0 && istr < len(str)
				if str[istr] == '{'
					let nbraces += 1
				elseif str[istr] == '}'
					let nbraces -= 1
				endif
				let istr += 1
			endwhile
			let text = str[:(istr-2)]
			let page = matchstr(str[(istr):], '{\([^}]*\)}')

			call add(toc, {'file': fnamemodify(a:auxfile, ':r') . '.tex',
						\ 'level': m[1], 'number': m[2], 'text': text, 'page': page})
		endif

	endfor

	return toc

endfunction

function! LatexBox_TOC()
	let toc = s:ReadTOC(LatexBox_GetAuxFile())
	let calling_buf = bufnr('%')

	30vnew +setlocal\ buftype=nofile LaTeX\ TOC

	for entry in toc
		call append('$', entry['number'] . "\t" . entry['text'])
	endfor
	call append('$', ["", "<Esc>/q: close", "<Space>: jump", "<Enter>: jump and close"])

	0delete
	syntax match Comment /^<.*/

	map <buffer> <silent> q			:bdelete<CR>
	map <buffer> <silent> <Esc>		:bdelete<CR>
	map <buffer> <silent> <Space> 	:call <SID>TOCActivate(0)<CR>
	map <buffer> <silent> <CR> 		:call <SID>TOCActivate(1)<CR>
	setlocal cursorline nomodifiable tabstop=8 nowrap

	let b:toc = toc
	let b:calling_win = bufwinnr(calling_buf)

endfunction

" TODO
"!function! s:FindClosestSection(toc, pos)
"!	let saved_pos = getpos('.')
"!	for entry in toc
"!	endfor
"!
"!	call setpos(saved_pos)
"!endfunction

function! s:TOCActivate(close)
	let n = getpos('.')[1] - 1

	if n >= len(b:toc)
		return
	endif

	let entry = b:toc[n]

	let toc_bnr = bufnr('%')
	let toc_wnr = winnr()

	execute b:calling_win . 'wincmd w'

	let bnr = bufnr(entry['file'])
	if bnr >= 0
		execute 'buffer ' . bnr
	else
		execute 'edit ' . entry['file']
	endif
	call search('\\' . entry['level'] . '\_\s*{' .
				\ substitute(entry['text'], ' ', '\\_\\s\\+', 'g') . '}', 'w')
	normal zt

	if a:close
		execute 'bdelete ' . toc_bnr
	else
		execute toc_wnr . 'wincmd w'
	endif
endfunction
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/vimcomplete.bst	[[[1
299
ENTRY
  { address author booktitle chapter doi edition editor eid howpublished institution isbn issn journal key month note number organization pages publisher school series title type volume year }
  {}
  { label }
STRINGS { s t}

FUNCTION {output}
{ 's :=
  %purify$
  "}{" * write$
  s
}
FUNCTION {fin.entry}
{ "}" * write$
  newline$
}

FUNCTION {not}
{   { #0 }
    { #1 }
  if$
}
FUNCTION {and}
{   'skip$
    { pop$ #0 }
  if$
}
FUNCTION {or}
{   { pop$ #1 }
    'skip$
  if$
}
FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}

FUNCTION {capitalize}
{ "u" change.case$ "t" change.case$ }

FUNCTION {space.word}
{ " " swap$ * " " * }

FUNCTION {bbl.and}      { "&"}
FUNCTION {bbl.etal}     { "et al." }

INTEGERS { nameptr namesleft numnames }

STRINGS  { bibinfo}

FUNCTION {format.names}
{ duplicate$ empty$ 'skip$ {
  's :=
  "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      %"{vv~}{ll}{, f.}{, jj}"
      "{vv }{ll}{}{}"
      format.name$
      't :=
      nameptr #1 >
        {
          namesleft #1 >
            { ", " * t * }
            {
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              t "others" =
                {
                  " " * bbl.etal *
                }
                {
                  bbl.and
                  space.word * t *
                }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
  } if$
}
FUNCTION {format.authors}
{ author format.names }

FUNCTION {format.title}
{ title
  duplicate$ empty$ 'skip$
    { "t" change.case$ }
  if$
}
FUNCTION {output.label}
{ newline$
  "{" cite$ * write$
  ""
}


FUNCTION {format.date}
{
  ""
  duplicate$ empty$
  year  duplicate$ empty$
    { swap$ 'skip$
        { "there's a month but no year in " cite$ * warning$ }
      if$
      *
    }
    { swap$ 'skip$
        {
          swap$
          " " * swap$
        }
      if$
      *
    }
  if$
}

FUNCTION {output.entry}
{ 's :=
  output.label
  s output
  format.authors output
  format.date output
  format.title output
  fin.entry
}

FUNCTION {default.type}     {"?" output.entry}

FUNCTION {article}          {"a" output.entry}
FUNCTION {book}             {"B" output.entry}
FUNCTION {booklet}          {"k" output.entry}
FUNCTION {conference}       {"f" output.entry}
FUNCTION {inbook}           {"b" output.entry}
FUNCTION {incollection}     {"c" output.entry}
FUNCTION {inproceedings}    {"p" output.entry}
FUNCTION {manual}           {"m" output.entry}
FUNCTION {mastersthesis}    {"Master" output.entry}
FUNCTION {misc}             {"-" output.entry}
FUNCTION {phdthesis}        {"PhD" output.entry}
FUNCTION {proceedings}      {"P" output.entry}
FUNCTION {techreport}       {"r" output.entry}
FUNCTION {unpublished}      {"u" output.entry}


READ
FUNCTION {sortify}
{ purify$
  "l" change.case$
}
INTEGERS { len }
FUNCTION {chop.word}
{ 's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    's
  if$
}
FUNCTION {sort.format.names}
{ 's :=
  #1 'nameptr :=
  ""
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{vv{ } }{ll{ }}{  f{ }}{  jj{ }}"
      format.name$ 't :=
      nameptr #1 >
        {
          "   "  *
          namesleft #1 = t "others" = and
            { "zzzzz" * }
            { t sortify * }
          if$
        }
        { t sortify * }
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {sort.format.title}
{ 't :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  sortify
  #1 global.max$ substring$
}
FUNCTION {author.sort}
{ author empty$
    { key empty$
        { "to sort, need author or key in " cite$ * warning$
          ""
        }
        { key sortify }
      if$
    }
    { author sort.format.names }
  if$
}
FUNCTION {author.editor.sort}
{ author empty$
    { editor empty$
        { key empty$
            { "to sort, need author, editor, or key in " cite$ * warning$
              ""
            }
            { key sortify }
          if$
        }
        { editor sort.format.names }
      if$
    }
    { author sort.format.names }
  if$
}
FUNCTION {author.organization.sort}
{ author empty$
    { organization empty$
        { key empty$
            { "to sort, need author, organization, or key in " cite$ * warning$
              ""
            }
            { key sortify }
          if$
        }
        { "The " #4 organization chop.word sortify }
      if$
    }
    { author sort.format.names }
  if$
}
FUNCTION {editor.organization.sort}
{ editor empty$
    { organization empty$
        { key empty$
            { "to sort, need editor, organization, or key in " cite$ * warning$
              ""
            }
            { key sortify }
          if$
        }
        { "The " #4 organization chop.word sortify }
      if$
    }
    { editor sort.format.names }
  if$
}
FUNCTION {presort}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.sort
    { type$ "proceedings" =
        'editor.organization.sort
        { type$ "manual" =
            'author.organization.sort
            'author.sort
          if$
        }
      if$
    }
  if$
  "    "
  *
  year field.or.null sortify
  *
  "    "
  *
  title field.or.null
  sort.format.title
  *
  #1 entry.max$ substring$
  'sort.key$ :=
}
ITERATE {presort}
SORT
ITERATE {call.type$}
doc/latex-box.txt	[[[1
258
*latex-box.txt*  	LaTeX Tool Box
*latex-box*

This plugin provides:
- Background compilation using latexmk;
- Completion for commands, environments, labels, and bibtex entries;
- A simple table of contents;
- Motion through brackets/braces.


|latex-box-completion|			COMPLETION
|latex-box-completion-commands|		Commands
|latex-box-completion-environments|	Environments
|latex-box-completion-labels|		Labels
|latex-box-completion-bibtex|		Bibtex

|latex-box-commands|			COMMANDS
|latex-box-commands-compilation|	Compilation
|latex-box-commands-motion|		Motion

|latex-box-motion|			MOTION

|latex-box-mappings|			MAPPINGS
|latex-box-mappings-compilation|	Compilation
|latex-box-mappings-templates|		Templates
|latex-box-mappings-motion|		Motion

|latex-box-settings|			SETTINGS
|latex-box-settings-compilation|	Compilation
|latex-box-settings-completion|		Completion
|latex-box-settings-templates|		Templates


COMPLETION						*latex-box-completion*

Completion is achieved through omni completion |compl-omni|, with default
bindings <CTRL-X><CTRL-O>. There are four types of completion:



						*latex-box-completion-commands*
Commands ~

Command completion is triggered by the '\' character.  For example, >
	\beg<CTRL-X><CTRL-O>
completes to >
	\begin{

Associated settings:
	|g:LatexBox_completion_commands|
	|g:LatexBox_completion_close_braces|


						*latex-box-completion-environments*
Environments ~

Environment completion is triggered by '\begin{'.  For example, >
	\begin{it<CTRL-X><CTRL-O>
completes to >
	\begin{itemize}

Completion of '\end{' automatically closes the last open environment.

Associated settings:
	|g:LatexBox_completion_environments|
	|g:LatexBox_completion_close_braces|


						*latex-box-completion-labels*
Labels ~

Label completion is triggered by '\ref{' or '\eqref{'.  For example, >
	\ref{sec:<CTRL-X><CTRL-O>
offers a list of all matching labels, with their associated value and page number.

Associated settings:
	|g:LatexBox_completion_close_braces|


						*latex-box-completion-bibtex*
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

Associated settings:
	|g:LatexBox_bibtex_wild_spaces|
	|g:LatexBox_completion_close_braces|


							*latex-box-templates*
Templates ~

Associated settings:
	|g:LatexBox_templates|


COMMANDS						*latex-box-commands*

							*latex-box-commands-compilation*
Compilation ~

*:Latexmk*
	Compile with latexmk in background.
	See |g:LatexBox_latexmk_options|.
*:LatexmkForce*
	Force compilation with latexmk in background.
*:LatexmkClean*
	Clean temporary output from LaTeX.
*:LatexmkCleanAll*
	Clean all output from LaTeX.

Viewing ~

*:LatexView*
	Launch viewer on output file.
	See |g:LatexBox_output_type| and |g:LatexBox_viewer|.

Motion ~

*:LatexTOC*
	Open a table of contents.
	Use Enter to navigate to selected entry.



MOTION							*latex-box-motion*

The function LatexBox_JumpToNextBraces({backward}) allows to jump outside of
the current brace/bracket pair, or inside of the next opening braces/brackets.


MAPPINGS						*latex-box-mappings*

							*latex-box-mappings-compilation*
Compilation ~

<Leader>ll		|:Latexmk|
	Compile with latexmk in background.
<Leader>lL		|:LatexmkForce|
	Force compilation with latexmk in background.
<Leader>lc		|:LatexmkClean|
	Clean temporary output from LaTeX.
<Leader>lC		|:LatexmkCleanAll|
	Clean all output from LaTeX.

Viewing ~

<Leader>lv		|:LatexView|
	View output file.

							*latex-box-mappings-templates*
Templates ~

Suggested binding: >
	imap <buffer> <silent> <F5> <C-R>=LatexBox_TemplatePrompt(1)<CR>
<
							*latex-box-mappings-motion*
Motion ~

<Leader>lt		|:LatexTOC|
	Open a table of contents.
	Use Enter to navigate to selected entry.


Suggested bindings: >
	map <silent> <buffer> ¶ :call LatexBox_JumpToNextBraces(0)<CR>
	map <silent> <buffer> § :call LatexBox_JumpToNextBraces(1)<CR>
	imap <silent> <buffer> ¶ <C-R>=LatexBox_JumpToNextBraces(0)<CR>
	imap <silent> <buffer> § <C-R>=LatexBox_JumpToNextBraces(1)<CR>
<

SETTINGS						*latex-box-settings*


						*latex-box-settings-completion*
Completion ~

*g:LatexBox_completion_close_braces*	Default: 1

	If nonzero, omni completion will add closing brackets where relevant.
	For example, if nonzero, >
		\begin{itemize
<	completes to >
		\begin{itemize}

*g:LatexBox_bibtex_wild_spaces*		Default: 1

	If nonzero, spaces act as wildcards ('.*') in completion.
	For example, if nonzero, >
		\cite{Knuth 1981
<	is equivalent to >
		\cite{Knuth.*1981

*g:LatexBox_completion_environments*
*g:LatexBox_completion_commands*

	Static completion lists for environments
	|latex-box-completion-environments| and commands
	|latex-box-completion-commands|.
	See |complete-items|.


						*latex-box-settings-templates*
Templates ~

*g:LatexBox_templates*

	Dictionary of environment templates |latex-box-templates|.
	FIXME! The final structure for this is not decided yet.


						*latex-box-settings-compilation*
Compilation ~

*g:LatexBox_latexmk_options*	Default: ""

	Additional options to pass to latexmk during compilation, e.g, "-d".

*g:LatexBox_output_type*	Default: "pdf"

	Extension of the output file. One of "pdf", "dvi" or "ps".

*g:LatexBox_viewer*		Default: "xdg-open"

	Viewer application for the output file, e.g., "xpdf".


TODO						*latex-box-todo*

Document templates.
Document wrap selection.
Document find label by number.
Document close last environment.
Errors: just document with :cc etc., no need to create bindings


Suggested bindings: >
	imap <buffer> <silent> [[ \begin{
	imap <buffer> <silent> ]] <C-R>=LatexBox_CloseLastEnv()<CR>
	imap <buffer> <F11> <C-R>=LatexBox_FindLabelByNumberPrompt()<CR>
	vmap <buffer> <silent> <F7> <Esc>:call LatexBox_WrapSelection('')<CR>i
<
Commands for LatexBox_TemplatePrompt({close}) and LatexBox_FindLabelByNumberPrompt().


vim:tw=78:ts=8:sw=8:ft=help:norl:noet:
