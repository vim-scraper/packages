"---------------------------------------------------------------------------
"    Description:  Programming automation
"    Author:       kk , ywgx , vimperator@163.com
"    Division:     Software College Northeastern University
"    Version:      1.6
"---------------------------------------------------------------------------

map <c-f> :/<c-v><cr>n
imap <c-z> <esc>ua
nmap <c-z> <esc>u

map <Up>   gk
map <Down> gj
no! <M-k> <Up>
no! <M-j> <Down>
no! <M-h> <left>
no! <M-l> <Right>
nm <silent> tt :!ctags -R .<CR>
au FileType python,ruby :call Py()
au FileType cpp,c,cc,h :call Cc()
func Py()
	nm <c-e> :call CR1()<CR>
	im <c-e> <ESC>:call CR1()<CR>
	im <F2> <ESC>
	if exists("$DISPLAY")
		nm <F2> :call CR()<CR>
	else
		nm <F2> :call CR1()<CR>
	en
endf
func Cc()
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
	nm<Space> i <Esc>l
	im<silent> <c-e> <ESC>A;<ESC>
	im<silent> nn <ESC>A;<ESC>o
	if exists("$DISPLAY")
		nm<F2> :call CR()<CR><CR>
		im<F2> <ESC> :call CR()<CR><CR>
		nm <c-e> :call CR()<CR><CR>
	else
		nm<F2> :call CR1()<CR>
		im<F2> <ESC> :call CR1()<CR>
		nm <c-e> :call CR1()<CR>
	en
	nm<silent> ca I/*<ESC><left>A<ESC><left>
	nm<silent> cd :s/\/\*//g<cr><ESC>:s/\*\///g<cr><ESC>
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
	elsei &filetype == 'ruby'
		exe "!clear;ruby -w %"
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
	elsei &filetype == 'ruby'
		exe "!ruby -w %"
	elsei &filetype == 'sh'
		exe "!bash %"
	elsei &filetype == 'pl'
		exe "!perl %"
	endif
endf
