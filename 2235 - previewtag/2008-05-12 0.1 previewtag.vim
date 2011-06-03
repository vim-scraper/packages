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

		    " ����ʾ��һ����ǩ֮ǰ��ɾ�������ִ���﷨����
		    silent! wincmd P                 " ��ת��Ԥ������
		    if &previewwindow                " ���ȷʵת����Ԥ�����ڡ���
		      match none                     " ɾ���﷨����
		      wincmd p                       " �ص�ԭ���Ĵ���
		    endif

			try  " Try displaying a matching tag for the word under the cursor
				exe "ptag " . w
			catch
				return
			endtry

			silent! wincmd P                 " ��ת��Ԥ������
    		if &previewwindow                " ���ȷʵת����Ԥ�����ڡ���
    		  if has("folding")
    		    silent! .foldopen            " չ���۵�����
    		  endif
    		  call search("$", "b")          " ��ǰһ�е���β
    		  let w = substitute(w, '\\', '\\\\', "")
    		  call search('\<\V' . w . '\>') " ��λ�����ƥ��ĵ�����
    		  " ���ڴ�λ�õĵ��ʼ���ƥ�����
    		  hi previewWord term=bold ctermbg=green guibg=green
    		  exe 'match previewWord "\%' . line(".") . 'l\%' . col(".") . 'c\k*"'
    		  wincmd p                       " ����ԭ���Ĵ���
    		endif

		endif
	endif
endfun

"if you want to open the tag preview winodw automatically when you put cursor
"on a word, please uncomment the following line
"au! CursorHold *.[ch],*.cpp,*.cxx,*.cc nested call s:PreviewWord()
