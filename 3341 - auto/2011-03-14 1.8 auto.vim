"---------------------------------------------------------------------------
"    DESCription:  Programming Automation
"    Help:         F2 key or "mm" to compile and debug
"    Author:       ywgx.ch@gmail.com
"    Version:      1.8
"    License:      GPL v2.0
"---------------------------------------------------------------------------
se ai si nocp
se exrc ts=4 sw=4 bs=2 sts=4
se nu cin magic smd hls is wmnu
im  ,, <ESC>
im  ;; <ESC>
map <Up>   gk
map <Down> gj
no! <M-k> <Up>
no! <M-j> <Down>
no! <M-h> <Left>
no! <M-l> <Right>
nm <silent> ;; :q!<CR>
nm <silent> ,, :wqa<CR>
au BufRead,BufNewFile *.html setf html
au FileType python,ruby :call Py()
au FileType cpp,c,cc,h :call Cc()
func Py()
	nm mm :call CR1()<CR>
	im mm <ESC>$
	im nn <ESC>o
	im <F2> <ESC>
	if exists("$DISPLAY")
		nm <F2> :call CR()<CR>
	el
		nm <F2> :call CR1()<CR>
	en
endf
func Cc()
	ino , ,<SPACE>
	ino ; ;<SPACE>
	ino <= <SPACE><=<SPACE>
	ino *= <SPACE>*=<SPACE>
	ino /= <SPACE>/=<SPACE>
	ino >> <SPACE>>><SPACE>
	ino << <SPACE><<<SPACE>
	ino >= <SPACE>>=<SPACE>
	ino == <SPACE>==<SPACE>
	ino += <SPACE>+=<SPACE>
	ino && <SPACE>&&<SPACE>
	ino != <SPACE>!=<SPACE>
	nm<SPACE> i<SPACE> <ESC>l
	im<silent> mm <ESC>A;<ESC>
	im<silent> nn <ESC>A;<ESC>o
	nm<silent> # :<ESC>i<ESC>i#
	if exists("$DISPLAY")
		nm mm :call CR1()<CR><CR>
		nm<F2> :call CR()<CR><CR>
		im<F2> <ESC> :call CR()<CR><CR>
	el
		nm mm :call CR1()<CR>
		nm<F2> :call CR1()<CR>
		im<F2> <ESC> :call CR1()<CR>
	en
	nm<silent> ca I/*<ESC><Left>A<ESC><Left>
	nm<silent> cd :s/\/\*//g<CR><ESC>:s/\*\///g<CR><ESC>
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
		exe "!clear;ruby  %"
	elsei &filetype == 'sh'
		exe "!clear;bash %"
	elsei &filetype == 'pl'
		exe "!clear;perl %"
	en
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
		exe "!ruby  %"
	elsei &filetype == 'sh'
		exe "!bash %"
	elsei &filetype == 'pl'
		exe "!perl %"
	elsei &filetype =='html'
		exe "!firefox %"
	en
endf
