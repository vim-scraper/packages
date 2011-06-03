" Vim plugin for previewing definitions of functions or variables
" automatically like source insight, This could be helpful when insight codes
" Maintainer: Yang Baohua <yangbaohua[At]gmail.com>
" Last Change: 2008 May 07
" Version: v0.1
" Thanks to Vim help and thinelephant[At]newsmth

" Has this already been loaded?
if exists ("loaded_autopreview")
	finish
endif

let loaded_autopreview = 1

" detect the msg
command! -nargs=0 -bar Pwindow call s:PreviewStatusChange()

" Change the Preview_Window_Open variable to open pwindow or not "
func s:PreviewStatusChange()
    if g:Preview_Window_Open ==1
       let g:Preview_Window_Open =0
    try  " Try displaying a matching tag for the word under the cursor
        exe "pclose"
    catch
        return
    endtry
else 
    let g:Preview_Window_Open =1
    silent! call s:PreviewWord()
endif
endfunc

" auto refresh the ptag window
au! CursorHold *.[ch],*.cpp nested call s:PreviewWord()
func s:PreviewWord()
    if g:Preview_Window_Open ==0
        return
elseif &previewwindow	" not do this in the preview window
    return
else
    let w = expand("<cword>")		" get the word under cursor
    if w =~ '\a'			" if the word contains a letter
        try  " Try displaying a matching tag for the word under the cursor
            exe "ptag " . w
        catch
            return
        endtry
    endif
endif
endfun
