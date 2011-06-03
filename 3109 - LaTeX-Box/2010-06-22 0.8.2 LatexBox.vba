" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/tex_LatexBox.vim	[[[1
21
" LaTeX Box plugin for Vim
" Maintainer: David Munger
" Email: mungerd@gmail.com
" Version: 0.8.2

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
230
" LaTeX Box common functions

" Settings {{{

" Compilation {{{

" g:vim_program {{{
if !exists('g:vim_program')

	" attempt autodetection of vim executable
	let g:vim_program = ''
	let tmpfile = tempname()
	silent execute '!ps -o command= -p $PPID > ' . tmpfile
	for line in readfile(tmpfile)
		let line = matchstr(line, '^\S\+\>')
		if !empty(line) && executable(line)
			let g:vim_program = line . ' -g'
			break
		endif
	endfor
	call delete(tmpfile)

	if empty(g:vim_program)
		if has('gui_macvim')
			let g:vim_program = '/Applications/MacVim.app/Contents/MacOS/Vim -g'
		else
			let g:vim_program = v:progname
		endif
	endif
endif
" }}}

if !exists('g:LatexBox_latexmk_options')
	let g:LatexBox_latexmk_options = ''
endif
if !exists('g:LatexBox_output_type')
	let g:LatexBox_output_type = 'pdf'
endif
if !exists('g:LatexBox_viewer')
	let g:LatexBox_viewer = 'xdg-open'
endif
" }}}

" Completion {{{
if !exists('g:LatexBox_completion_close_braces')
	let g:LatexBox_completion_close_braces = 1
endif
if !exists('g:LatexBox_bibtex_wild_spaces')
	let g:LatexBox_bibtex_wild_spaces = 1
endif

if !exists('g:LatexBox_cite_pattern')
	let g:LatexBox_cite_pattern = '\\cite\(p\|t\)\?\*\?\_\s*{'
endif
if !exists('g:LatexBox_ref_pattern')
	let g:LatexBox_ref_pattern = '\\v\?\(eq\|page\)\?ref\*\?\_\s*{'
endif

if !exists('g:LatexBox_completion_environments')
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
		\ {'word': 'document' },
		\ {'word': 'abstract' },
		\ ]
endif

if !exists('g:LatexBox_completion_commands')
	let g:LatexBox_completion_commands = [
		\ {'word': '\begin{' },
		\ {'word': '\end{' },
		\ {'word': '\item' },
		\ {'word': '\label{' },
		\ {'word': '\ref{' },
		\ {'word': '\eqref{eq:' },
		\ {'word': '\cite{' },
		\ {'word': '\chapter{' },
		\ {'word': '\section{' },
		\ {'word': '\subsection{' },
		\ {'word': '\subsubsection{' },
		\ {'word': '\paragraph{' },
		\ {'word': '\nonumber' },
		\ {'word': '\bibliography' },
		\ {'word': '\bibliographystyle' },
		\ ]
endif
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

" View Output {{{
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

" Get Current Environment {{{
" LatexBox_GetCurrentEnvironment([with_pos])
" Returns:
" - environment													if with_pos is not given
" - [envirnoment, lnum_begin, cnum_begin, lnum_end, cnum_end]	if with_pos is nonzero
function! LatexBox_GetCurrentEnvironment(...)

	if a:0 > 0
		let with_pos = a:1
	else
		let with_pos = 0
	endif

	let pos = getpos('.')
	let begin_pat = '\\begin\_\s*{[^}]*}\|\\\[\|\\('
	let end_pat = '\\end\_\s*{[^}]*}\|\\\]\|\\)'
	let filter = 'strpart(getline("."), 0, col(".") - 1) =~ ''^%\|[^\\]%'''

	" match begin/end pairs but skip comments
	let flags = 'bnW'
	if strpart(getline('.'), col('.') - 1) =~ '^\%(' . begin_pat . '\)'
		let flags .= 'c'
	endif
	let [lnum, cnum] = searchpairpos(begin_pat, '', end_pat, flags, filter)

	let env = ''

	if lnum

		let line = strpart(getline(lnum), cnum - 1)

		if empty(env)
			let env = matchstr(line, '^\\begin\_\s*{\zs[^}]*\ze}')
		endif
		if empty(env)
			let env = matchstr(line, '^\\\[')
		endif
		if empty(env)
			let env = matchstr(line, '^\\(')
		endif

	endif

	if with_pos == 1

		let flags = 'nW'
		if !(lnum == pos[1] && cnum == pos[2])
			let flags .= 'c'
		endif

		let [lnum2, cnum2] = searchpairpos(begin_pat, '', end_pat, flags, filter)
		return [env, lnum, cnum, lnum2, cnum2]
	else
		return env
	endif


endfunction
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/complete.vim	[[[1
393
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

		let line_start = line[:pos-1]
		if line_start =~ '\\begin\_\s*{$'
			let s:completion_type = 'begin'
		elseif line_start =~ '\\end\_\s*{$'
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
		let file = LatexBox_GetMainTexFile()
	else
		let file = a:1
	endif

	if empty(glob(file), 1)
		return ''
	endif

	let lines = readfile(file)

	let bibdata_list = []

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\\bibliography\s*{[^}]\+}'''),
				\ 'matchstr(v:val, ''\\bibliography\s*{\zs[^}]\+\ze}'')')

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\\\%(input\|include\)\s*{[^}]\+}'''),
				\ 's:FindBibData(LatexBox_kpsewhich(matchstr(v:val, ''\\\%(input\|include\)\s*{\zs[^}]\+\ze}'')))')

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\\\%(input\|include\)\s\+\S\+'''),
				\ 's:FindBibData(LatexBox_kpsewhich(matchstr(v:val, ''\\\%(input\|include\)\s\+\zs\S\+\ze'')))')

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
	let tmpbase = LatexBox_GetTexRoot() . '/_LatexBox_BibComplete'
    let auxfile = tmpbase . '.aux'
    let bblfile = tmpbase . '.bbl'
    let blgfile = tmpbase . '.blg'

    call writefile(['\citation{*}', '\bibstyle{' . s:bstfile . '}', '\bibdata{' . bibdata . '}'], auxfile)

    silent execute '! cd ' shellescape(LatexBox_GetTexRoot()) .
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


" Close Last Environment {{{
function! s:CloseLastEnv()
	" first, try with \left/\right pairs
	let filter = 'strpart(getline("."), 0, col(".") - 1) =~ ''^%\|[^\\]%'''
	let [lnum, cnum] = searchpairpos('\\left\>', '', '\\right\>', 'bnW', filter)
	if lnum
		let line = strpart(getline(lnum), cnum - 1)
		let bracket = matchstr(line, '^\\left\zs\((\|\[\|\\{\||\|\.\)\ze')
		for [open, close] in [['(', ')'], ['\[', '\]'], ['\\{', '\\}'], ['|', '|'], ['\.', '|']]
			let bracket = substitute(bracket, open, close, 'g')
		endfor
		return '\right' . bracket
	endif
	
	" second, try witn environments
	let env = LatexBox_GetCurrentEnvironment()
	if env == '\['
		return '\]'
	elseif env == '\('
		return '\)'
	elseif env != ''
		return '\end{' . env . '}'
	endif
	return ''
endfunction

" }}}

" Wrap Selection {{{
function! s:WrapSelection(wrapper)
	normal `>a}
	exec 'normal `<i\' . a:wrapper . '{'
endfunction
" }}}

" Change Environment {{{
function! s:ChangeEnv()

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
	for entry in g:LatexBox_completion_environments
		let env = entry.word
		if env =~ '^' . a:lead
			call add(suggestions, env)
		endif
	endfor
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
imap <Plug>LatexCloseLastEnv	<C-R>=<SID>CloseLastEnv()<CR>
vmap <Plug>LatexWrapSelection	:call <SID>WrapSelection('')<CR>i
nmap <Plug>LatexChangeEnv		:call <SID>ChangeEnv()<CR>
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/latexmk.vim	[[[1
201
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


" dictionary of latexmk PID's (basename: pid)
let s:latexmk_running_pids = {}

" Set PID {{{
function! s:LatexmkSetPID(basename, pid)
	let s:latexmk_running_pids[a:basename] = a:pid
endfunction
" }}}

" Callback {{{
function! s:LatexmkCallback(basename, status)
	"let pos = getpos('.')
	if a:status
		echomsg "latexmk exited with status " . a:status
	else
		echomsg "latexmk finished"
	endif
	call remove(s:latexmk_running_pids, a:basename)
	call LatexBox_LatexmkErrors(0)
	"call setpos('.', pos)
endfunction
" }}}

" Latexmk {{{
function! LatexBox_Latexmk(force)

	if empty(v:servername)
		echoerr "cannot run latexmk in background without a VIM server"
		return
	endif

	let basename = LatexBox_GetTexBasename(1)

	if has_key(s:latexmk_running_pids, basename)
		echomsg "latexmk is already running for `" . fnamemodify(basename, ':t') . "'"
		return
	endif

	let callsetpid = s:SIDWrap('LatexmkSetPID')
	let callback = s:SIDWrap('LatexmkCallback')

	let l:options = '-' . g:LatexBox_output_type . ' -quiet ' . g:LatexBox_latexmk_options
	if a:force
		let l:options .= ' -g'
	endif
	let l:options .= " -e '$pdflatex =~ s/ / -file-line-error /'"
	let l:options .= " -e '$latex =~ s/ / -file-line-error /'"

	" callback to set the pid
	let vimsetpid = g:vim_program . ' --servername ' . v:servername . ' --remote-expr ' .
				\ shellescape(callsetpid) . '\(\"' . basename . '\",$$\)'

	" latexmk command
	let cmd = 'cd ' . LatexBox_GetTexRoot() . ' ; latexmk ' . l:options . ' ' . LatexBox_GetMainTexFile()

	" callback after latexmk is finished
	let vimcmd = g:vim_program . ' --servername ' . v:servername . ' --remote-expr ' . 
				\ shellescape(callback) . '\(\"' . basename . '\",$?\)'

	silent execute '! ( ' . vimsetpid . ' ; ( ' . cmd . ' ) ; ' . vimcmd . ' ) &'
endfunction
" }}}

" LatexmkStop {{{
function! LatexBox_LatexmkStop()

	let basename = LatexBox_GetTexBasename(1)

	if !has_key(s:latexmk_running_pids, basename)
		echomsg "latexmk is not running for `" . fnamemodify(basename, ':t') . "'"
		return
	endif

	call s:kill_latexmk(s:latexmk_running_pids[basename])

	call remove(s:latexmk_running_pids, basename)
	echomsg "latexmk stopped for `" . fnamemodify(basename, ':t') . "'"
endfunction
" }}}

" kill_latexmk {{{
function! s:kill_latexmk(gpid)

	" This version doesn't work on systems on which pkill is not installed:
	"!silent execute '! pkill -g ' . pid

	" This version is more portable, but still doesn't work on Mac OS X:
	"!silent execute '! kill `ps -o pid= -g ' . pid . '`'

	" Since 'ps' behaves differently on different platforms, we must use brute force:
	" - list all processes in a temporary file
	" - match by process group ID
	" - kill matches
	let pids = []
	let tmpfile = tempname()
	silent execute '!ps x -o pgid,pid > ' . tmpfile
	for line in readfile(tmpfile)
		let pid = matchstr(line, '^\s*' . a:gpid . '\s\+\zs\d\+\ze')
		if !empty(pid)
			call add(pids, pid)
		endif
	endfor
	call delete(tmpfile)
	if !empty(pids)
		silent execute '! kill ' . join(pids)
	endif
endfunction
" }}}

" kill_all_latexmk {{{
function! s:kill_all_latexmk()
	for gpid in values(s:latexmk_running_pids)
		call s:kill_latexmk(gpid)
	endfor
	let s:latexmk_running_pids = {}
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

	silent execute '! ' . l:cmd
	echomsg "latexmk clean finished"
endfunction
" }}}

" LatexmkStatus {{{
function! LatexBox_LatexmkStatus(detailed)

	if a:detailed
		if empty(s:latexmk_running_pids)
			echo "latexmk is not running"
		else
			let plist = ""
			for [basename, pid] in items(s:latexmk_running_pids)
				if !empty(plist)
					plist .= '; '
				endif
				let plist .= fnamemodify(basename, ':t') . ':' . pid
			endfor
			echo "latexmk is running (" . plist . ")"
		endif
	else
		let basename = LatexBox_GetTexBasename(1)
		if has_key(s:latexmk_running_pids, basename)
			echo "latexmk is running"
		else
			echo "latexmk is not running"
		endif
	endif

endfunction
" }}}

" LatexErrors {{{
function! LatexBox_LatexErrors(jump)
	let log = LatexBox_GetLogFile()
	if (a:jump)
		execute 'cfile ' . log
	else
		execute 'cgetfile ' . log
	endif
endfunction
" }}}

" Commands {{{
command! Latexmk				call LatexBox_Latexmk(0)
command! LatexmkForce			call LatexBox_Latexmk(1)
command! LatexmkClean			call LatexBox_LatexmkClean(0)
command! LatexmkCleanAll		call LatexBox_LatexmkClean(1)
command! LatexmkStatus			call LatexBox_LatexmkStatus(0)
command! LatexmkStatusDetailed	call LatexBox_LatexmkStatus(1)
command! LatexmkStop			call LatexBox_LatexmkStop()
command! LatexErrors			call LatexBox_LatexErrors(1)
" }}}

autocmd VimLeavePre * call <SID>kill_all_latexmk()

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
ftplugin/latex-box/mappings.vim	[[[1
65
" LaTeX Box mappings

if exists("g:LatexBox_no_mappings")
	finish
endif

" latexmk {{{
map <buffer> <LocalLeader>ll :Latexmk<CR>
map <buffer> <LocalLeader>lL :LatexmkForce<CR>
map <buffer> <LocalLeader>lc :LatexmkClean<CR>
map <buffer> <LocalLeader>lC :LatexmkCleanAll<CR>
map <buffer> <LocalLeader>lg :LatexmkStatus<CR>
map <buffer> <LocalLeader>lG :LatexmkStatusDetailed<CR>
map <buffer> <LocalLeader>lk :LatexmkStop<CR>
map <buffer> <LocalLeader>le :LatexErrors<CR>
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

" begin/end pairs {{{
nmap <buffer> % <Plug>LatexBox_JumpToMatch
xmap <buffer> % <Plug>LatexBox_JumpToMatch
vmap <buffer> ie <Plug>LatexBox_SelectCurrentEnvInner
vmap <buffer> ae <Plug>LatexBox_SelectCurrentEnvOuter
omap <buffer> ie :normal vie<CR>
omap <buffer> ae :normal vae<CR>
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
228
" LaTeX Box motion functions

" begin/end pairs {{{
function! s:JumpToMatch(mode)

	" selection is lost upon function call, reselect
	if a:mode == 'v'
		normal! gv
	endif

	let open_pats = ['{', '\[', '(', '\\begin\>', '\\left\>']
	let close_pats = ['}', '\]', ')', '\\end\>', '\\right\>']

	"for [open_pat, close_pat] in [['\\begin\>', '\\end\>'], ['\\left\>', '\\right\>']]

	let filter = 'strpart(getline("."), 0, col(".") - 1) =~ ''^%\|[^\\]%'''

	" move to the left until not on alphabetic characters
	let [bufnum, lnum, cnum, off] = getpos('.')
	let line = getline(lnum)
	while cnum > 1 && line[cnum - 1] =~ '\a'
		let cnum -= 1
	endwhile
	call cursor(lnum, cnum)

	" go to next opening/closing pattern
	call search(join(open_pats + close_pats, '\|'), 'cW', filter)

	let rest_of_line = strpart(line, col('.') - 1)

	for i in range(len(open_pats))
		let open_pat = open_pats[i]
		let close_pat = close_pats[i]

		if rest_of_line =~ '^\%(' . open_pat . '\)'
			" if on opening pattern, go to closing pattern
			call searchpair(open_pat, '', close_pat, 'W', filter)
			return
		elseif rest_of_line =~ '^\%(' . close_pat . '\)'
			" if on closing pattern, go to opening pattern
			let flags = 'bW'
			call searchpair(open_pat, '', close_pat, 'bW', filter)
			return
		endif

	endfor

endfunction
nnoremap <silent> <Plug>LatexBox_JumpToMatch :call <SID>JumpToMatch('n')<CR>
vnoremap <silent> <Plug>LatexBox_JumpToMatch :<C-U>call <SID>JumpToMatch('v')<CR>
" }}}

" select current environment {{{
function! s:SelectCurrentEnv(seltype)
	let [env, lnum, cnum, lnum2, cnum2] = LatexBox_GetCurrentEnvironment(1)
	call cursor(lnum, cnum)
	if a:seltype == 'inner'
		if env =~ '^\'
			normal! 2l
		else
			call search('}\_\s*', 'eW')
			normal l
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
			normal! 2l
		else
			call search('}', 'eW')
		endif
	endif
endfunction
vnoremap <silent> <Plug>LatexBox_SelectCurrentEnvInner :<C-U>call <SID>SelectCurrentEnv('inner')<CR>
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

	let titlestr = entry['text']

	" Credit goes to Marcin Szamotulski for the following fix. It allows to match through
	" commands added by TeX.
	let titlestr = substitute(titlestr, '\\\w*\>\s*\%({[^}]*}\)\?', '.*', 'g')

	let titlestr = escape(titlestr, '\')
	let titlestr = substitute(titlestr, ' ', '\\_\\s\\+', 'g')

	call search('\\' . entry['level'] . '\_\s*{' . titlestr . '}', 'w')
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
303
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
{ author empty$
    {editor format.names} {author format.names}
  if$
}

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
372
*latex-box.txt*  	LaTeX Tool Box
*latex-box*

This plugin provides:
- Background compilation using latexmk;
- Completion for commands, environments, labels, and bibtex entries;
- A simple table of contents;
- Smart indentation (activated with "set smartindent");
- Motion between \begin/\end pairs with the % key;
- Motion through brackets/braces (with user-defined keys).
- Environment objects (e.g., select environement with "vie" or "vae")

==============================================================================

|latex-box-completion|			COMPLETION
|latex-box-completion-commands|		Commands
|latex-box-completion-environments|	Environments
|latex-box-completion-labels|		Labels
|latex-box-completion-bibtex|		Bibtex

|latex-box-commands|			COMMANDS
|latex-box-commands-compilation|	Compilation
|latex-box-commands-viewing|		Viewing
|latex-box-commands-motion|		Motion

|latex-box-motion|			MOTION

|latex-box-mappings|			MAPPINGS
|latex-box-mappings-compilation|	Compilation
|latex-box-mappings-insertion|		Insertion
|latex-box-mappings-viewing|		Viewing
|latex-box-mappings-motion|		Motion

|latex-box-settings|			SETTINGS
|latex-box-settings-compilation|	Compilation
|latex-box-settings-completion|		Completion


==============================================================================

COMPLETION						*latex-box-completion*

Completion is achieved through omni completion |compl-omni|, with default
bindings <CTRL-X><CTRL-O>. There are four types of completion:



------------------------------------------------------------------------------

						*latex-box-completion-commands*
Commands ~

Command completion is triggered by the '\' character.  For example, >
	\beg<CTRL-X><CTRL-O>
completes to >
	\begin{

Associated settings:
	|g:LatexBox_completion_commands|
	|g:LatexBox_completion_close_braces|


------------------------------------------------------------------------------

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


------------------------------------------------------------------------------

						*latex-box-completion-labels*
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
	|g:LatexBox_ref_pattern|
	|g:LatexBox_completion_close_braces|


------------------------------------------------------------------------------

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

You can also use regular expressions (or vim patterns) after '\cite{'.

Associated settings:
	|g:LatexBox_cite_pattern|
	|g:LatexBox_bibtex_wild_spaces|
	|g:LatexBox_completion_close_braces|


==============================================================================

COMMANDS						*latex-box-commands*

------------------------------------------------------------------------------

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
*:LatexmkStop*
	Stop latexmk if it is running.
*:LatexmkStatus*
	Show the running status of latexmk for the current buffer.
*:LatexmkStatusDetailed*
	Show the running status of latexmk for all buffers with process group
	ID's.
*:LatexErrors*
	Load the log file for the current document and jump to the first error.

When latexmk terminates, it reports its success or failure (with status
number). To navigate through the errors, you can use the |:cc|, |:cn| and
|:cp| commands, as well as the |:clist| command to list the errors.

------------------------------------------------------------------------------

							*latex-box-commands-viewing*
Viewing ~

*:LatexView*
	Launch viewer on output file.
	See |g:LatexBox_output_type| and |g:LatexBox_viewer|.

------------------------------------------------------------------------------

							*latex-box-commands-motion*
Motion ~

*:LatexTOC*
	Open a table of contents.
	Use Enter to navigate to selected entry.



==============================================================================

MOTION							*latex-box-motion*

The function LatexBox_JumpToNextBraces({backward}) allows to jump outside of
the current brace/bracket pair, or inside of the next opening braces/brackets.


==============================================================================

MAPPINGS						*latex-box-mappings*

------------------------------------------------------------------------------

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
<Leader>lk		|:LatexmkStop|
	Stop latexmk if it is running.
<Leader>lg		|:LatexmkStatus|
	Show the running status of latexmk for the current buffer.
<Leader>lG		|:LatexmkStatusDetailed|
	Show the running status of latexmk for all buffers with process group
	ID's.
<Leader>le		|:LatexErrors|
	Load the log file for the current document and jump to the first error.


------------------------------------------------------------------------------

							*latex-box-mappings-viewing*
Viewing ~

<Leader>lv		|:LatexView|
	View output file.

------------------------------------------------------------------------------

							*latex-box-mappings-insertion*
Insertion ~


<Plug>LatexCloseLastEnv
	Close the last matching open environment. Use with imap, e.g.: >
	imap ]]		<Plug>LatexCloseLastEnv
<
<Plug>LatexChangeEnv
	Change the current environment. Use with nmap, e.g.: >
	nmap <F5>		<Plug>LatexChangeEnv
<
<Plug>LatexWrapSelection
	Wrap the current selection in a LaTeX command. Use with vmap, e.g.: >
	vmap <F7>		<Plug>LatexWrapSelection
<
Suggested mappings to put in ~/.vim/ftplugin/tex.vim: >
	imap <buffer> [[ 		\begin{
	imap <buffer> ]]		<Plug>LatexCloseLastEnv
	nmap <buffer> <F5>		<Plug>LatexChangeEnv
	vmap <buffer> <F7>		<Plug>LatexWrapSelection
	imap <buffer> (( 		\eqref{
<
------------------------------------------------------------------------------

							*latex-box-mappings-motion*
Motion ~

<Leader>lt		|:LatexTOC|
	Open a table of contents.
	Use Enter to navigate to selected entry.


Suggested bindings: >
	map  <silent> <buffer> ¶ :call LatexBox_JumpToNextBraces(0)<CR>
	map  <silent> <buffer> § :call LatexBox_JumpToNextBraces(1)<CR>
	imap <silent> <buffer> ¶ <C-R>=LatexBox_JumpToNextBraces(0)<CR>
	imap <silent> <buffer> § <C-R>=LatexBox_JumpToNextBraces(1)<CR>
<

==============================================================================

SETTINGS						*latex-box-settings*


------------------------------------------------------------------------------

Mappings ~

*g:LatexBox_no_mappings*
	If this variable is defined, the default keyboard mappings will not be
	loaded.


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

*g:LatexBox_cite_pattern*		Default: '\\cite\(p\|t\)\?\*\?\_\s*{'
*g:LatexBox_ref_pattern*		Default: '\\v\?\(eq\|page\)\?ref\*\?\_\s*{'

	Patterns to match \cite and \ref commands for BibTeX and label completion.
	Must include the trailing '{'.
	To match all commands that contain 'cite' (case insensitive), use: >
		let g:LatexBox_cite_pattern = '\c\\\a*cite\a*\*\?\_\s*{'
<	To match all commands that end with 'ref' (case insensitive): >
		let g:LatexBox_ref_pattern = '\c\\\a*ref\*\?\_\s*{'
<	Both examples match commands with a trailing star too.


------------------------------------------------------------------------------

Templates (DEPRECATED) ~

*g:LatexBox_templates*

	Dictionary of environment templates |latex-box-templates|.
	
	DEPRECATED!
	I think it is better to leave this task to plug-ins oriented to do
	this well, like snipMate:
	http://www.vim.org/scripts/script.php?script_id=2540

	


------------------------------------------------------------------------------

						*latex-box-settings-compilation*
Compilation ~

*g:vim_program*			Default: autodetected

	Vim program to use on command line for callbacks.
	If autodetect fails, defaults to >
	'/Applications/MacVim.app/Contents/MacOS/Vim -g'
<	on MacVim, or to |v:progname| on other systems.

*g:LatexBox_latexmk_options*	Default: ""

	Additional options to pass to latexmk during compilation, e.g, "-d".

*g:LatexBox_output_type*	Default: "pdf"

	Extension of the output file. One of "pdf", "dvi" or "ps".

*g:LatexBox_viewer*		Default: "xdg-open"

	Viewer application for the output file, e.g., "xpdf".


==============================================================================

TODO						*latex-box-todo*

- Fix bugs?

==============================================================================

vim:tw=78:ts=8:sw=8:ft=help:norl:noet:
