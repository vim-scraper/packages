"---------------------------------------------------------------------------
"    Filename:     auto.vim
"    Description:  Programming Automation
"    Author:       ywgx , vimperator@163.com
"    Created:      Sat Nov 13 01:29:55 CST 2010
"    Division:     Software College Northeastern University
"    Version:      1.0
"---------------------------------------------------------------------------
syntax on
syntax enable
colors koehler
filetype plugin on
filetype indent on

set undolevels=100
set nocompatible
set autoindent
set smartindent

set magic
set showmode
set hlsearch
set incsearch
set nu
set cin

set exrc
set backspace=2
set ts=4
set sw=4

set wildmenu

map <Up>   gk
map <Down> gj

map  ,, <ESC>
map  ;; <ESC>
imap ,, <ESC>
imap ;; <ESC>
map <silent> ;; :q!<CR>
map <silent> ,, :wqa<CR>
map <silent> mm <ESC>
imap<silent> mm <ESC>
noremap! <M-k> <Up>
noremap! <M-j> <Down>
noremap! <M-h> <left>
noremap! <M-l> <Right>
inoremap <C-j> <C-o>gj
inoremap <C-k> <C-o>gk
map <F5> <Esc>:1,$g/^$/d<CR><Esc>gg=G<Esc>gg
map <F7>  :%s/\r/<CR>

set enc=utf-8
set fileencodings=utf-8,gbk,ucs-bom,cp936,gb2312,gb18030,big5,euc-jp,latin1,usc

au FileType cpp,c,cc,python map  mm :call CR1()<CR>
au FileType cpp,c,cc,python map  <F2> :call CR()<CR>
au FileType cpp,c,cc,python imap <F2> <ESC> :call CR()<CR>
au FileType cpp,c,cc,python :call YW()

func YW()
 inoremap , ,<space>
 inoremap ; ;<space>
 inoremap <= <space><=<space>
 inoremap >> <space>>><space>
 inoremap << <space><<<space>
 inoremap >= <space>>=<space>
 inoremap == <space>==<space>
 inoremap += <space>+=<space>
 inoremap && <space>&&<space>
 inoremap != <space>!=<space>
 map <silent> # :<esc>i<esc>i#
endf

func CR()
	exec "w"
	if &filetype == 'c'
	    exec "!gcc -Wall % -o %<"
	    exec "!clear;./%< 2>/dev/null && rm -f %<"
    elseif &filetype == 'cpp'
	    exec "!g++ -Wall % -o %<"
	    exec "!clear;./%< 2>/dev/null && rm -f %<"
    elseif &filetype == 'python'
	    exec "!clear;python %"
	elseif &filetype == 'sh'
	    exec "!clear;bash %"
	elseif &filetype == 'pl'
		exec "!clear;perl %"
	endif
endf

func CR1()
	exec "w"
	if &filetype == 'c'
	    exec "!gcc -Wall % -o %<"
	    exec "!./%< 2>/dev/null && rm -f %<"
    elseif &filetype == 'cpp'
	    exec "!g++ -Wall % -o %<"
	    exec "!./%< 2>/dev/null && rm -f %<"
    elseif &filetype == 'python'
	    exec "!python %"
	elseif &filetype == 'sh'
	    exec "!bash %"
	elseif &filetype == 'pl'
		exec "!perl %"
	endif
endf
