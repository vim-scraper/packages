""""""""""""""""""""""" mty's vimrc """""""""""""""""""""""""""""""
"
" I took tips from here and there and made a lot of stuff myself.
" This vimrc misses many things (python support for example...).
" Take the time to make it you're own, steal it, modify it, delete
" it, I just don't give a shit... 
" I'd just be glad if you would send me any mod you make so I can
" learn from the stuff you other guys do (thanks and gifts are 
" also happily taken... ;D). 
" My mail is: mtylty@gmail.com 
"
"P.s.
"You require vim's latest version and full support for scripts and
"stuff. If vim gives you errors you either have to update vim or 
"you have to install some plugins...
"I don't have time to document it properly so you have to read it 
"yourself... To use this vimrc you just have to read at the end of 
"the file, where there are all the bindings...(Also look at function
"CommentIt that contains many programming-language specific bindings) 
"If you would like to use all the features, you have to create a dir 
"~/.vimstuff containing a ~/.vimstuff/viminfo file, a directory named 
"~/.vimstuff/backups and a file ~/.vimstuff/bashvimrc ; this will give 
"you viminfo out of the way, costant backups and a bashrc that you can 
"use while in :shell command (this gets me less confused since I added 
"to my $PS1 a |_insidevim_| string).
"Another thing, I don't give a fuck if you use hjkl to move around,
"I just don't like it, so don't go complaining about this, I just 
"you could trash this vimrc... just do it...
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


let osys=system('uname -s')
let vinvoke=fnamemodify($_, ":p")
let fullp=substitute(vinvoke, '^\(.*[/]\).*$', '\1', "")
filetype plugin indent on
"
" File Backups, various info and restore last editing position
"
if (isdirectory($HOME.'/.vimstuff'))
	if (isdirectory($HOME.'/.vimstuff/backups'))
		let vimtdir=$HOME . '/.vimstuff/backups'
		set backup
		let &backupdir=vimtdir
	else
		set nobackup
	endif
	if (filereadable($HOME.'/.vimstuff/viminfo'))
		set viminfo+=n$HOME/.vimstuff/viminfo
	endif
	if (filereadable($HOME.'/.vimstuff/bashvimrc'))
		set shell=bash\ --rcfile\ ~/.vimstuff/bashvimrc
	endif
endif
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
"
" Setup a proper include file path
"
if $INCLUDE == ""
	let &path="/usr/include,/usr/X11R6/include,/usr/local/include,/opt/local/include"
endif
set history=100
set nonumber
set tabstop=4
set shiftwidth=4
set statusline=%<%F%h%m%r%=\[%B\]\ %l,%c%V\ %P
set laststatus=2
set showcmd
set gcr=a:blinkon0
set errorbells
set visualbell
set nowarn
set ignorecase
set smartcase

"
" Use F10 to switch between hex and ASCII editing
"
function HexConverter()
	let c=getline(".")
	if c =~ '^[0-9a-f]\{7}:'
		:%!xxd -r
	else
		:%!xxd -g4
	endif
endfunction

"
" Function to make del and return work as they should
"
function! Delete_key(...)
	let line=getline (".")
	if line=~'^\s*$'
		execute "normal dd"
		return
	endif
	let column = col(".")
	let line_len = strlen (line)
	let first_or_end=0
	if column == 1
		let first_or_end=1
	else
		if column == line_len
			let first_or_end=1
		endif
	endif
	execute "normal i\<DEL>\<ESC>"
	if first_or_end == 0
		execute "normal l"
	endif
endfunction

"
"Close and open folded code
"
function SuperFold ()
	let pos = line(".") 
	let lev = foldclosed(pos)
	let isfold = foldlevel(pos)
	if (isfold == 0)
		echo "This is not a fold." | sleep 700 m | redraw | echo ""
	else
		if lev == -1
			execute "normal zc"
		else
			execute "normal zo"
		endif
	endif
endfunction

"
"Auto complete for html tags
" 
function Tags ()
	let to_copy = ""
	let line = getline(".")
	if (stridx(line, "/") == -1 && stridx(line, "<") != -1)
		if stridx(line, " ") != -1
			let start = stridx(line, "<") + 1
			let end = stridx(line, " ")
			let to_copy = strpart(line, start, end - start)
			let new_line = line."></".to_copy.">"
			call setline(line("."), new_line)
		else
			let start = stridx(line, "<") + 1
			let to_copy = strpart(line, start)
			let new_line = line."></".to_copy.">"
			call setline(line("."), new_line)
		endif
	else
		let new_line = line.">"
		call setline(line("."), new_line)
	endif
endfunction

"
" shortcuts for particular languages, add yours...
"
function CommentIt ()
	if &filetype == "c" || &filetype == "cpp"
		"
		" Some comments: ca comments a line, cd decomments it, cc big comment,
		" in for local includes .h, gi for global includes
		"
		map <silent> ca I/*<ESC>A*/<ESC><left>
		map <silent> cd :s/\/\*//g<cr><ESC>:s/\*\///g<cr><ESC>
		vmap <silent> ca <ESC>`<I/*<ESC>`>A*/<ESC>
		vmap <silent> cd <ESC>`<:s/\/\*//g<cr><ESC>`><ESC>:s/\*\///g<cr><ESC>
		map cc i/***********************************************************************<cr>*********************************************************************/<up><ESC>o
		map gi i#include <.h><ESC><Left><Left>i
		map li i#include ".h"<ESC><Left><Left>i
		"
		" This adds gcc support for the quickfix cycle
		"
		set makeprg=gcc\ -Wall\ -Wextra\ %
		set errorformat=%f:%l:\ %m
		"
		" Auto parenthesis...
		"
		"imap ( ()<Left>
		"imap [ []<Left>
		imap { {<return>}<up><ESC>o

		"
		" This adds some coders' shortcuts...
		"
		map <F3> :wa<cr>:make<cr>
		map <F4> :cn<cr>
		map <F5> :cp<cr>
		map <F6> :clist<cr>
		map <F7> :wa<cr>:!gcc -Wall -Wextra -g -o main %<cr>:!./main<cr>
		map <F8> :wa<cr>:!gcc -Wall -Wextra -g -o main %<cr>:!cgdb main<cr>
	endif

	if &filetype == "html" || &filetype == "jsp"
		imap <silent> > <ESC>:call Tags()<cr>i<right><right><cr><ESC><up>o
		set filetype=html
	endif

	if &filetype == "java"
		map <silent> ca I/*<ESC>A*/<ESC><left>
		map <silent> cd :s/\/\*//g<cr><ESC>:s/\*\///g<cr><ESC>
		map cc i/***********************************************************************<cr>*********************************************************************/<up><ESC>o
		set makeprg=javac\ %
		"set errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
		set errorformat=%A%f:%l:\ %m,%+Z%p^,%+C%.%#,%-G%.%#
		map <F3> :wa<cr>:make<cr>
		map <F4> :cn<cr>
		map <F5> :cp<cr>
		map <F6> :clist<cr>
	endif
endfunction

"
" custom hotkeys... see above...
"
autocmd BufEnter * call CommentIt ()

"
" I can't rember why I need it...
"
set nomodeline

"
" Auto fold opened files and apply syntax
"
set foldmethod=indent
syntax on

"
" Set decent colors, in-sun visibility :D
"
colorscheme desert
hi Folded ctermfg=yellow ctermbg=none cterm=bold
hi LineNr ctermfg=red ctermbg=none 

"
" Set search highlight
"
set hlsearch


"
" Mouse control
"
set mouse=a
"set t_ku=[A
"set t_kd=[B
"set t_kr=[C
"set t_kl=[D

"
" Decent backspace
"
set backspace=indent,eol,start

"
"Call the delete_key function
"
nnoremap <silent> <DEL> :call Delete_key()<CR>
nnoremap <silent> <CR> i<CR><ESC>
nnoremap <silent> <BS> i<BS><RIGHT><ESC>

"
" Do not backup!
"
"set nobackup
"

"
"Convert everything to Hex with F10
"
map <F10> :call HexConverter()

"
" Set decent open and close for folds
"
map fc zM
map fo zR
map <silent> ff :call SuperFold() <CR>

"
" automatic parenteses
"
"imap ( ()<Left>
"imap [ []<Left>
"imap { {<return>}<up><ESC>o
"

"
" modify splitted window size
"
map + <C-W>10+
map - <C-W>10-
map > <C-W>10>
map < <C-W>10<

"
" Set decent tab shortcuts
"
map tn <ESC>:tabedit 
map st <ESC>:tabs<cr>
map ] <ESC>:tabnext<cr>
map [ <ESC>:tabprevious<cr>

"
" Split shortcuts (rember... ctrl-w to change split)
"
"
map hs <ESC>:split 
map vs <ESC>:vsplit 

"
" Man pages shortcut
"
runtime ftplugin/man.vim

map k <ESC>\K
map man <ESC>:Man 

"
" Press sh and get a shell for free...
"
map sh <ESC>:shell<cr>
