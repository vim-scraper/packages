" Vim plugin for previewing definitions of functions or variables
" Maintainer: Wang Danqi<beyondwdq[At]gmail.com>
" Last Change: 2008 May 12
" Version: v0.1

" Has this already been loaded?
if exists ("loaded_previewtag")
	finish
endif

let loaded_previewtag = 1

" detect the msg
command! -nargs=0 -bar Pshow call s:PreviewWord()
command! -nargs=0 -bar Pdelete call s:DeletePreBuffer()
command! -nargs=0 -bar PshowORdelete call s:ShowORDelete()
command! -nargs=0 -bar Pswitch call s:SwitchBufferToWindow()


" Delete the buffer when in previewwindow, preview a tag when in other windows
func s:ShowORDelete()
	if &previewwindow
		try
			exe "bd %"
		catch
			return
		endtry
	else
		call s:PreviewWord()
	endif
endfunc

" close the preview window, and display the buffer in a new window
func s:SwitchBufferToWindow()
	silent! wincmd P
	if &previewwindow
		let cw = bufnr("%")
		try
			exe "pclose"
			exe "buffer! " . cw
		catch
			return
		endtry
	endif
endfunc

" Delete buffer in previewwindow
func s:DeletePreBuffer()
	silent! wincmd P
	if &previewwindow
		try
			exe "bd %"
		catch
			return
		endtry
	endif
endfunc


" Show the ptag window
" some code of this function is copied from vim doc, try :help previewwindow.
" Since I am using a vim doc in Chinese, some Chinese commnents are left in
" the code
func s:PreviewWord()
	if &previewwindow	" not do this in the preview window
		return
	else
		let w = expand("<cword>")		" get the word under cursor
		if w =~ '\a'			" if the word contains a letter

		    " 在显示下一个标签之前，删除所有现存的语法高亮
		    silent! wincmd P                 " 跳转至预览窗口
		    if &previewwindow                " 如果确实转到了预览窗口……
		      match none                     " 删除语法高亮
		      wincmd p                       " 回到原来的窗口
		    endif

			try  " Try displaying a matching tag for the word under the cursor
				exe "ptag " . w
			catch
				return
			endtry

			silent! wincmd P                 " 跳转至预览窗口
    		if &previewwindow                " 如果确实转到了预览窗口……
    		  if has("folding")
    		    silent! .foldopen            " 展开折叠的行
    		  endif
    		  call search("$", "b")          " 到前一行的行尾
    		  let w = substitute(w, '\\', '\\\\', "")
    		  call search('\<\V' . w . '\>') " 定位光标在匹配的单词上
    		  " 给在此位置的单词加上匹配高亮
    		  hi previewWord term=bold ctermbg=green guibg=green
    		  exe 'match previewWord "\%' . line(".") . 'l\%' . col(".") . 'c\k*"'
    		  wincmd p                       " 返回原来的窗口
    		endif

		endif
	endif
endfun

"if you want to open the tag preview winodw automatically when you put cursor
"on a word, please uncomment the following line
"au! CursorHold *.[ch],*.cpp,*.cxx,*.cc nested call s:PreviewWord()
