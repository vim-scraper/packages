" My vim settings file
" Author: Mayuresh Kadu <mskadu@gmail.com>

" Settings {{{
set secure nocompatible
if version >= 600
	syntax enable
	filetype on
	filetype plugin on
	filetype indent on
else
	:finish
endif
" }}}

" Terminal Specific Stuff {{{

"if has("terminfo")
"	set t_Co=16
"	set t_Sf=3p1%dm
"	set t_Sb=%p1%dm
"	set t_vb=
"else
"	set t_Co=16
"	set t_Sf= %dm
"	set t_Sb= dm
"	set t_vb=
"endif

" }}}

" General Settings {{{
"
"Make autoindent happen
set autoindent
"Show actual cursor position
set ruler
set showmatch
set showmode
set showcmd

"Misc
set shiftwidth=3
set tabstop=3
set number
set smartindent
set formatoptions=croq
set backspace=indent,eol,start

"Fix findstr for Win32
if has("win32")
	set grepprg=findstr\ /R\ /S\ /N
endif

" Choose right syntax highlighting with tab completion
map <F2> :source $VIM/syntax/
" F9 toggles highlighting
map <F9> :if has("syntax_items")<CR>syntax off<CR>else<CR>syntax on<CR>endif<CR><CR>

" show status line
set ls=2
"Make no *.bak
set nobackup
" keep backup while we are editing
set writebackup
"Do not wrap text
set nowrap
" do not highlight searches
set nohlsearch
"Turn this ON when we want to debug
"set verbose=9

"General Options ends }}}

"Java settings {{{
let java_highlight_functions=1

autocmd BufRead BufNewFile *.java set makeprg=javac\ %
"autocmd BufRead BufNewFile *.java set makeprg=ant\ -emacs

" Mark Klips as XML files (17/2/2005)
autocmd BufRead,BufNewFile *.klip set filetype=xml

" Reload settings file everytime its written 
autocmd! bufwritepost _vimrc source $VIM/_vimrc

"Java Settings End }}}

"My color settings {{{
"
"Reset the default ones before we begin loading ours
highlight Constant 	NONE
highlight Delimiter	NONE
highlight Directory 	NONE
highlight Error		NONE
highlight ErrorMsg	NONE
highlight Identifier	NONE
highlight LineNr		NONE
highlight ModeMsg		NONE
highlight MoreMsg		NONE
highlight Normal		NONE
highlight NonText		NONE
highlight PreProc		NONE
highlight Question	NONE
highlight Search		NONE
highlight Special		NONE
highlight SpecialKey	NONE
highlight Statement	NONE
highlight StatusLine	NONE
highlight Title		NONE
highlight Todo			NONE
highlight Type			NONE
highlight Visual		NONE
highlight WarningMsg	NONE

"Now put in our own colors
highlight Comment		term=bold ctermfg=5 ctermbg=0 guifg=#FF005F guibg=gray
highlight Constant	term=underline ctermfg=6 guifg=#FF2F8F
highlight Delimiter 	term=bold cterm=bold ctermfg=1 gui=bold guifg=red
highlight Directory	term=bold ctermfg=DarkBlue guifg=Blue
highlight Error		term=standout cterm=bold ctermbg=1 ctermfg=1 gui=bold guifg=red
highlight ErrorMsg	term=standout cterm=bold ctermfg=1 gui=bold guifg=red
highlight Identifier	term=underline ctermfg=3 guifg=Yellow3
highlight LineNr		term=underline cterm=bold ctermfg=3 guifg=Brown
highlight ModeMsg		term=bold cterm=bold ctermfg=3 ctermbg=1 guifg=yellow2 guibg=red
highlight MoreMsg		term=bold cterm=bold ctermfg=2 gui=bold guifg=green
highlight NonText		term=bold ctermfg=2 guifg=green3
highlight Normal		ctermfg=white ctermbg=black guifg=grey90 guibg=#000020
highlight PreProc		term=underline ctermfg=14 guifg=cyan
highlight Question	term=standout cterm=bold ctermfg=2 gui=bold guifg=Green
highlight Search		term=reverse ctermbg=2 guibg=Yellow
highlight Special		term=bold ctermfg=5 guifg=SlateBlue
highlight SpecialKey	term=bold ctermfg=DarkBlue guifg=Blue

"set foldmethod=indent
"colorscheme darkblue
colorscheme desert

"use our custom font
if has("gui") 
	set guifont=Courier_New:h10:cANSI
endif
