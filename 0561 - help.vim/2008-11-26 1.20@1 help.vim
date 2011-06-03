" vim:fdm=marker:commentstring="\ %s:
" Name: help.vim ("help subsystem")
" Version: 1.19
" Authors: Slava Gorbanev (author  1.16 version) and 
"          Nikolay Panov  (author >1.16 version)
" Date: 09/03/2003 (21:03)
" Description: call man or perldoc -f or many other help system in dependent from context
" Changes:
" * 1.20  now pydoc is supported
" * 1.19  several bugfixes, impruvements and other...
" * 1.18  several bugfixes, added call apropos if man page not found.
" * 1.17  support fvwm, muttrc filetype, many fixes...
" * 1.17g new, extended implementation of help.vim (http://www.vim.org/scripts/script.php?script_id=561)
" * 1.17b forked by help.vim,v 1.16 2002/01/05 19:58:38 from Slava Gorbanev, added support tcl/tk and many fixes
" Installation: put this into your plugin directory (~/.vim/plugin)
" Usage:
" 	use <F1> by default or other key (if you remap it) to call Help(expand("<cword>"))
"	this function creating in half window buffer with contex-dependent
"	manual (or other) page about word under corsor.
"	You can use new commands now:
"	 Man 	 	 something		the same as man into your system
"	 Perldoc 	 something		the same as perldoc into your system
"	 GoToSection something		try to find <something> section into window
"	 Help		 something		try to show context-dependent help
"	 Dict		 something		the same as dict into your system 
" 
"    By default in help-buffer set key-mapping:
"    q           for exit (and ``Esc'' in GUI mode)
"    o           for command :only
"    D           for go to DESCRIPTION section
"    S           for go to SYN section
"    (and other --- see source code by detail)
"
"	You can mapping go to any other section:
"	for example for go to NAME section by N key call next command:
"	map N :call GoToSection('NAME')
"
" 	Now support: sh, vim, perl, python, tcl/tk, C/C++, fvwm, muttrc and many other.
"
" TODO nicier documentation (oh - my terrible english)...
" TODO support many other language
" {{{ Builtin function (sh, perl)
let sh_builtin='^\(alias\|bg\|bind\|break\|builtin\|case\|cd\|co\(mmand\|ntinue\)\|declare\|dirs\|echo\|enable\|eval\|ex\(ec\|it\|port\)\|fc\|fg\|for\|function\|getopts\|hash\|help\|history\|if\|jobs\|kill\|let\|lo\(cal\|gout\)\|popd\|pushd\|pwd\|read\(\|only\)\|return\|se\(lect\|t\)\|shift\|source\|suspend\|test\|times\|trap\|ty\(pe\|peset\)\|ulimit\|umask\|un\(alias\|set\|til\)\|variables\|wait\|while\)$'
let perl_builtin='^\(abs\|accept\|alarm\|atan2\|bind\|binmode\|bless\|caller\|chdir\|chmod\|chom\=p\|chown\|chr\|chroot\|close\|closedir\|connect\|continue\|cos\|crypt\|dbmclose\|dbmopen\|defined\|delete\|die\|do\|dump\|each\|end\(grent\|hostent\|netent\|protoent\|pwent\|servent\)\|eof\|eval\|exec\|exp\|exists\|exit\|fcntl\|fileno\|flock\|fork\|format\|formline\|getc\|getgrent\|getgrgid\|getgrnam\|gethostbyaddr\|gethostbyname\|gethostent\|getlogin\|getnetbyaddr\|getnetbyname\|getnetent\|getpeername\|getpgrp\|getppid\|getpriority\|getprotobyname\|getprotobynumber\|getprotoent\|getpwent\|getpwnam\|getpwuid\|getservbyname\|getservbyport\|getservent\|getsockname\|getsockopt\|glob\|gmtime\|goto\|grep\|hex\|import\|index\|int\|ioctl\|join\|keys\|kill\|last\|lc\|lcfirst\|length\|link\|listen\|local\|localtime\|log\|lstat\|map\|mkdir\|msgctl\|msgget\|msgrcv\|msgsnd\|my\|next\|no\|open\|opendir\|ord\|pack\|package\|pipe\|po[ps]\|printf\=\|push\|quotemeta\|rand\|read\(\|dir\|link\)\|recv\|redo\|ref\|rename\|require\|reset\|return\|reverse\|rewinddir\|rindex\|rmdir\|scalar\|seek\|seekdir\|select\|semctl\|semget\|semop\|send\|set\(gr\|host\|net\)ent\|setp\(grp\|riority\|rotoent\|went\)\|setservent\|setsockopt\|shift\|shmctl\|shmget\|shmread\|shmwrite\|shutdown\|sin\|sleep\|socket\(\|pair\)\|sort\|splice\|split\|sprintf\|sqrt\|srand\|stat\|study\|sub\|substr\|symlink\|sys\(call\|read\|seek\|write\|tem\)\|tell\|telldir\|tied\=\|times\=\|truncate\|uc\|ucfirst\|umask\|undef\|unlink\|unpack\|unshift\|untie\|use\|utime\|values\|vec\|wait\(\|pid\)\|wa\(ntarray\|rn\)\|write\)$'
" }}}
" {{{ Global definition variables and command
let $MANPL='1100i' " no page breaks inside man pages

command! -nargs=* Man			call Man(<f-args>)
command! -nargs=1 Perldoc 		call Perldoc(<f-args>)
command! -nargs=1 Pydoc 		call Pydoc(<f-args>)
command! -nargs=1 GoToSection	call GoToSection(<f-args>)
" }}}
" {{{ Mappings
nmap <F1>			:call Help(expand("<cword>"))<CR>
nmap <F1><F1>		:call Dict(expand("<cword>"))<CR>
imap <F1><F1> 		<ESC>:call Dict(expand("<cword>"))<CR>
imap <F1> 			<ESC>:call Help(expand("<cword>"))<CR>
" }}}
" {{{ Funcitions definition 
" {{{ The GoToSection(section) function close all fond and go to...
fun! GoToSection(search)
	let search = a:search
	if search =~ '\/'
		let cmd = search
	else
		let search = substitute(search, "^un", "", "")
		let cmd = '/^\s*\(\[un\]\)\='.search.'.\{-,50}/'
	endif
	normal zM
	silent exec cmd
	normal zvjzvztk0
endfun
" }}}
" {{{ The OpenHelpWin(cmd, ft, ...) function get manual page and creating window
fun! OpenHelpWin(cmd, ft, ...)
    if a:0
		let buf_name = a:1
    else
		let buf_name = 'Help'
    endif
    exe 'silent new' escape(buf_name, '\ ')
    setlocal modifiable buftype=nofile noswapfile
    endif
    let &ft = a:ft
    exe "0r!".a:cmd
	if line('$') == 1
		exe "0r! man ".a:1." 2>/dev/null"
		if line('$') == 1
			exe "0r! apropos ".a:1." 2>/dev/null"
		endif
	endif
	let helpsize = line('$')
	if helpsize > &helpheight
		let helpsize = &helpheight
	endif
	set nomod
    if winheight(2) != -1
		exe 'resize' helpsize
    endif
    1
    " {{{ key-mapping and definition local parameter
    noremap <buffer> <Space> <C-F>
    noremap <buffer> <Backspace> <C-B>
    noremap <buffer> o :only<CR>
    noremap <buffer> q :bdel<CR>
    noremap <buffer> D :call GoToSection('DESCRIPTION')<CR>
    noremap <buffer> S :call GoToSection('SYN')<CR>
    noremap <buffer> <C-Up> zM?^[A-Z]\+<CR>jzvztk0
    noremap <buffer> <C-Down> zM/^[A-Z]\+<CR>jzvztk0
    if has("gui_running")
        noremap <buffer> <Esc> :bdel<CR>
    endif

    setlocal foldmethod=indent
    setlocal nohlsearch
    setlocal nomodifiable
    " }}}
    endif
endfun
" }}}
" {{{ The Man(page, ...) function gets a man page
fun! Man(page, ...)
    if a:0
		let page = a:1
		let section = '-S '.a:page.' '
		if a:0 > 1
			let go_to = a:2
		else
			let go_to = ''
		endif
    else
		let page = a:page
		let section = ''
		let go_to = ''
    endif
    call OpenHelpWin("man ".section.page." 2>/dev/null \|col -b\|uniq", 'man', page)
	if go_to != ''
		call GoToSection(go_to)
	endif
endfun
" }}}
" {{{ The Dict(word) function call a dict command for word
fun! Dict(word)
    call OpenHelpWin("dict ".a:word." 2>/dev/null \|col -b\|uniq", 'man', a:word)
	normal G2kzvztk0
endfun
" }}}
" {{{ The Pydoc(word) function gets a python documentation for word
fun! Pydoc(word)
    let move_to_pattern = ''
    let filetype = 'man'
    let cmd = 'pydoc '.a:word
    call OpenHelpWin(cmd." 2>/dev/null", filetype, a:word)
    if move_to_pattern != ''
        call GoToSection(move_to_pattern)
    endif
endfun
" }}}
" {{{ The Perldoc(word) function gets a perl documentation for word
fun! Perldoc(word)
    let move_to_pattern = ''
    let filetype = 'man'
    if a:word =~ g:perl_builtin
		let cmd = 'perldoc -f '.a:word
    elseif a:word =~# '^\(s\|m\|qr\)$'
		let cmd = 'man perlop'
		let move_to_pattern = '/^ \+'.a:word.'\/PATTERN\//'
    elseif a:word =~# '^\(tr\|y\)$'
		let cmd = 'man perlop'
		let move_to_pattern = '/^ \+'.a:word.'\/SEARCHLIST\//'
    elseif a:word =~# '^q[qxw]\=$'
		let cmd = 'man perlop'
		let move_to_pattern = '/^ \+'.a:word.'\/STRING\//'
    elseif a:word =~# '^\(y\|tr\)$'
		let cmd = 'man perlop'
		let move_to_pattern = '/^ \+'.a:word.'\/SEARCHLIST\//'
    else
		let cmd = 'man -S 3perl:3pm:3 '.a:word
    endif
    call OpenHelpWin(cmd." 2>/dev/null \|col -b\|uniq", filetype, a:word)
    if move_to_pattern != ''
		call GoToSection(move_to_pattern)
    endif
endfun
" }}}
" {{{ The ShBuiltin(word) function gets a help for sh builin function
fun! ShBuiltin(word)
	let WORD = expand("<cword>")
	if a:word =~ g:sh_builtin
		let cmd = "bash -c 'help ".a:word."'"
	elseif WORD == '\[' || WORD =~ '^-[a-z]$'
		let cmd = "bash -c 'help test'"
	elseif WORD == ':' || WORD == '.' || WORD == '{'
		let cmd = "bash -c 'help ".WORD."'"
	else
		call Man('1:5:8', a:word)
		return
	endif
	call OpenHelpWin(cmd, 'man', WORD)
	normal zR
endfun
" }}}
" {{{ The Help(word) function gets a help for word (in depend on context)
fun! Help(word)
    if &ft == 'vim' || &ft == 'help'
		exe "help" a:word
    elseif &ft == 'c' || &ft == 'cpp'
		call Man('2:3', a:word)
    elseif &ft =~ 'perl'
        call Perldoc(a:word)
    elseif &ft =~ 'python'
		call Pydoc(a:word)
	elseif &ft =~ 'tcl'
		call Man('3tcl:3tk:3', a:word)
	elseif &ft =~ '^\(fvwm\|muttrc\)$'
		call Man('1:5',&ft, a:word)
    elseif &ft =~ '^z\=sh'
		call ShBuiltin(a:word)
    elseif a:word =~ '^\i\+'
		call Man(a:word)
    else
		echohl Error | echo 'No identifier under cursor' | echohl None
    endif

	normal zv
endfun
" }}}
" }}}
