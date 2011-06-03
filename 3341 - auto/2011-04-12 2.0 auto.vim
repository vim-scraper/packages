"---------------------------------------------------------------------------
"    DESCription:  Programming Automation
"    Help:         F2 key or "mm" to compile and debug
"    Author:       ywgx.ch@gmail.com
"    Version:      2.0
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
au FileType python,ruby,sh :call Py()
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
	nm mm :call Ct()<CR>
	im mm <ESC>$
	im nn <ESC>o
	im <F2> <ESC>
	if exists("$DISPLAY")
		nm <F2> :call Cv()<CR>
	el
		nm <F2> :call Ct()<CR>
	en
endf
func Cc()
	no != <SPACE>!=<SPACE>
    im mm <ESC>A;<ESC>
	im nn <ESC>A;<ESC>o
	if exists("$DISPLAY")
		if &filetype == 'perl'
		   nm mm :call Ct()<CR>
		   nm<F2> :call Cv()<CR>
		   im<F2> <ESC> :call Cv()<CR>
	    el
		   nm mm :call Ct()<CR><CR>
		   nm mm :call Ct()<CR><CR>
		   nm<F2> :call Cv()<CR><CR>
		   im<F2> <ESC> :call Cv()<CR><CR>
		en
	el
		nm<F2> :call Ct()<CR>
		im<F2> <ESC> :call Ct()<CR>
		nm mm :call Ct()<CR>
	en
endf
func Cv()
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
func Ct()
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
