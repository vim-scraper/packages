" run it on command line
" vim +"so ~/.vim/SlideShow.vim" `grep -liR "Cary" * | grep -v .svn | grep ".java" | sort`

if exists("SlideShowSourced")
	finish
endif
let SlideShowSourced = "true"

" This is a neat tool that has the same effect as putting the source code
" file in a slide show.

" Pause number of seconds before doing the next slide.
let s:interval = 2

" Scroll a number of lines if needed.
function! Scroll()
	echo line(".") . " of " . line("$") . " " . expand("%:p")
	if (line(".") != line("$")) 
		" let diff = (line("$") - line(".") - 1) / 2
		let diff = line("$") - line(".") - 2
		let scrollLine = winheight(0) - winline() - &scrolloff
		if (diff > scrollLine)
			exe "normal ".scrollLine."\<c-e>"
			exe "sleep " . s:interval
		endif 
	endif

endfun

" Sleep some time
function! Sleep(force)
	echo line(".") . " of " . line("$") . " " . expand("%:p")
	let last = line("$")
	let height = winheight(0)
	if (a:force)
		exe "sleep " . s:interval
	else
		if (last > height)
			exe "sleep " . s:interval
		endif
	endif
endfunction

let @b=':call Scroll()'
let @a=":nextgg:call Sleep(1)50@b:call Sleep(0)"
nmap <silent> <space> gg50@b:call Sleep(0)10000@a
noremap <Left> :previous<cr>
noremap <Right> :next<cr>
