"---------------------------------------------------------------------------
"    Description:  Programming automation
"    Author:       ywgx , vimperator@163.com
"    Division:     Software College Northeastern University
"    Version:      1.2
"---------------------------------------------------------------------------
syntax on
syntax enable
filetype plugin indent on

set enc=utf-8
set fencs=utf-8,gbk,ucs-bom,cp936,gb2312,gb18030

set autoindent smartindent nocompatible

set et ts=4 sw=4 exrc backspace=2 softtabstop=4

set nu cin magic showmode hlsearch wildmenu incsearch

map <Up>   gk
map <Down> gj

im ,, <ESC>
im ;; <ESC>
no! <M-k> <Up>
no! <M-j> <Down>
no! <M-h> <left>
no! <M-l> <Right>
nm <silent> ;; :q!<CR>
nm <silent> ,, :wqa<CR>

au FileType cpp,c,cc,python :call YW()
func YW()
 ino , ,<space>
 ino ; ;<space>
 ino <= <space><=<space>
 ino *= <space>*=<space>
 ino /= <space>/=<space>
 ino >> <space>>><space>
 ino << <space><<<space>
 ino >= <space>>=<space>
 ino == <space>==<space>
 ino += <space>+=<space>
 ino && <space>&&<space>
 ino != <space>!=<space>
 im<silent> mm <ESC>A;<ESC>
 nm <F2> :call CR()<CR><CR>
 nm<silent> mm :call CR1()<CR>
 im <F2> <ESC> :call CR()<CR><CR>
 nm<silent> ca I/*<ESC><left>A<ESC><left>
 nm<silent> cd :s/\/\*//g<cr><ESC>:s/\*\///g<cr><ESC>
endf

func CR()
	exe "w"
	if &filetype == 'c'
	    exec "!gcc -Wall % -o %<"
	    exec "!clear;./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'cpp'
	    exe "!g++ -Wall % -o %<"
	    exe "!clear;./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'python'
	    exe "!clear;python %"
	elsei &filetype == 'sh'
	    exe "!clear;bash %"
	elsei &filetype == 'pl'
		exe "!clear;perl %"
	en
endf

func CR1()
	exe "w"
	if &filetype == 'c'
	    exe "!gcc  % -o %<"
	    exe "!./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'cpp'
	    exe "!g++  % -o %<"
	    exe "!./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'python'
	    exe "!python %"
	elsei &filetype == 'sh'
	    exe "!bash %"
	elsei &filetype == 'pl'
		exe "!perl %"
	en
endf
