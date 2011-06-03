
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" HungarianToEnglish()
"
" A function to strip Hungarian decorations from C and C++ 
" identifier names.  In some cases, this can make code 
" easier to read.  It might also change how your program 
" runs or make it not compile, so use with caution!
"
" The author makes no representations, express or implied,
" about the usefulness or applicability of this code and
" assumes no responsibility for any damage it may cause.
" Enjoy!
"
" Issac Trotts
" June 25, 2002
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! HungarianToEnglish() 
    " convert uiBlah    (unsigned int)
    exe ":%s/\\<ui\\(\\u\\S*\\)\\>/\\1"
    " convert bBlah     (boolean or byte, who knows?)
    exe ":%s/\\<b\\(\\u\\S*\\)\\>/\\1"    
    " convert iBlah     (integer)
    exe ":%s/\\<i\\(\\u\\S*\\)\\>/\\1"    
    " convert pBlah     (pointer)
    exe ":%s/\\<p\\(\\u\\S*\\)\\>/\\1"    
    " convert szBlah    (string terminated by a zero)
    exe ":%s/\\<sz\\(\\u\\S*\\)\\>/\\1"    
    " convert lpszBlah  (long pointer to a string terminated by a zero)
    exe ":%s/\\<lpsz\\(\\u\\S*\\)\\>/\\1"    
endfunction

" Change <F9> to your favorite Hungarian-removal key combo!
map <F9> :call HungarianToEnglish()<CR> 

