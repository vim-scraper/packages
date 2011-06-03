"---------------------------------------------------------------------------
"    DESCription:  Programming Automation
"    Help:         F2 key or "mm" to compile and debug
"    Author:       ywgx.ch@gmail.com
"    Version:      1.9
"    License:      GPL v2.0
"---------------------------------------------------------------------------
se ai si nocp
se nu is cin smd wmnu magic
se exrc ts=4 sw=4 sts=4 backspace=2
im  ,, <ESC>
im  ;; <ESC>
map<Up>   gk
map<Down> gj
no!<M-k> <Up>
no!<M-j> <Down>
no!<M-h> <Left>
no!<M-l> <Right>
nm<silent> ;; :q!<CR>
nm<silent> ,, :wq<CR>
au BufRead,BufNewFile *.html setf html
au FileType python,ruby :call Py()
au FileType cpp,c,cc,h,html,perl :call Cc()
au FileType c,cc,cpp,h,html,python :call Cp()
func Cp()
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
endf
func Py()
	nm mm :call CrText()<CR>
	im mm <ESC>$
	im nn <ESC>o
	im <F2> <ESC>
	if exists("$DISPLAY")
		nm <F2> :call Cr()<CR>
	el
		nm <F2> :call CrText()<CR>
	en
endf
func Cc()
	no != <SPACE>!=<SPACE>
    im mm <ESC>A;<ESC>
	im nn <ESC>A;<ESC>o
	nm<silent> # :<ESC>i<ESC>i#
	if exists("$DISPLAY")
		if &filetype == 'perl'
		   nm mm :call CrText()<CR>
		   nm<F2> :call Cr()<CR>
		   im<F2> <ESC> :call Cr()<CR>
	    el
		   nm mm :call CrText()<CR><CR>
		   nm mm :call CrText()<CR><CR>
		   nm<F2> :call Cr()<CR><CR>
		   im<F2> <ESC> :call Cr()<CR><CR>
		en
	el
		nm<F2> :call CrText()<CR>
		im<F2> <ESC> :call CrText()<CR>
		nm mm :call CrText()<CR>
	en
endf
func Cr()
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
	elsei &filetype == 'perl'
		exe "!clear;perl %"
	en
endf
func CrText()
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
	elsei &filetype == 'perl'
		exe "!perl %"
	elsei &filetype =='html'
		exe "!firefox %"
	en
endf
