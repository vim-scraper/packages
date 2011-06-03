"---------------------------------------------------------------------------
"    Description:  Programming automation
"    Author:       ywgx , vimperator@163.com
"    Division:     Software College Northeastern University
"    Version:      1.3
"---------------------------------------------------------------------------
syntax on
syntax enable
filetype plugin indent on

set enc=utf-8
set fileencodings=utf-8,gbk,ucs-bom,cp936,gb2312,gb18030
set autoindent smartindent nocompatible
set exrc ts=4 sw=4 backspace=2 softtabstop=4
set nu cin magic showmode hlsearch incsearch wildmenu

im  ,, <ESC>
im  ;; <ESC>
map <Up>   gk
map <Down> gj
no! <M-k> <Up>
no! <M-j> <Down>
no! <M-h> <left>
no! <M-l> <Right>
nm <silent> ;; :q!<CR>
nm <silent> ,, :wqa<CR>

au FileType python map  <F2> :call CR()<CR>
au FileType python imap <F2> <ESC> :call CR()<CR>
au FileType cpp,c,cc,python map  mm :call CR1()<CR>

au FileType cpp,c,cc,h :call YW()
func YW()
 ino , ,<space>
 ino ( (<space>
 ino ) )<ESC>i<space><ESC>la
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
 nm <Space> i <Esc>l
 nm <silent> # :<esc>i<esc>i#
 nm <F2> :call CR()<CR><CR>
 im <F2> <ESC> :call CR()<CR><CR>
endf

func CR()
	exe "w"
	if &filetype == 'c'
	    exe "!gcc -Wall % -o %<"
	    exe "!clear;./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'cpp'
	    exe "!g++ -Wall % -o %<"
	    exe "!clear;./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'python'
	    exe "!clear;python %"
	elsei &filetype == 'sh'
	    exe "!clear;bash %"
	elsei &filetype == 'pl'
		exe "!clear;perl %"
	endif
endf

func CR1()
	exe "w"
	if &filetype == 'c'
	    exe "!gcc -Wall % -o %<"
	    exe "!./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'cpp'
	    exe "!g++ -Wall % -o %<"
	    exe "!./%< 2>/dev/null && rm -f %<"
    elsei &filetype == 'python'
	    exe "!python %"
	elsei &filetype == 'sh'
	    exe "!bash %"
	elsei &filetype == 'pl'
		exe "!perl %"
	endif
endf
