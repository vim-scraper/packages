""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" HungarianToEnglish:
"
" A function to strip Hungarian decorations from C and C++ 
" identifier names.  In some cases, this can make code 
" easier to read.  It might also change how your program 
" runs or make it not compile, so use with caution!
"
" INSTALLATION:
" Put in your $VIM/vimfiles/plugin/ directory and change the 
" keybinding (at the bottom of this file) if you want it to be
" something other than F4.
" 
" USAGE:
" Highlight a region you wish to translate.  Then press <F4>.
"
" LEGAL STUFF:
" The author makes no representations, express or implied,
" about the usefulness or applicability of this code and
" assumes no responsibility for any damage it may cause.
" Enjoy!
"
" Issac Trotts
" Created: June 25, 2002
"
" MODIFICATIONS:
" June 26, 2002 -> ver 1.0.1 
" - fixed a bug with capitalization
" - added removal of the m_blah notation
" - changed to only operate on the selected range
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! HungarianToEnglish() range
    let rstr = a:firstline . "," . a:lastline 
    " convert uiBlah    (unsigned int)
    exe rstr .  "s/\\<ui\\(\\u\\S*\\)\\>/\\l\\1"
    " convert bBlah     (boolean or byte, who knows?)
    exe rstr .  "s/\\<b\\(\\u\\S*\\)\\>/\\l\\1"    
    " convert iBlah     (integer)
    exe rstr .  "s/\\<i\\(\\u\\S*\\)\\>/\\l\\1"    
    " convert pBlah     (pointer)
    exe rstr .  "s/\\<p\\(\\u\\S*\\)\\>/\\l\\1"    
    " convert szBlah    (string terminated by a zero)
    exe rstr .  "s/\\<sz\\(\\u\\S*\\)\\>/\\l\\1"    
    " convert lpszBlah  (long pointer to a string terminated by a zero)
    exe rstr .  "s/\\<lpsz\\(\\u\\S*\\)\\>/\\l\\1"    
    " convert m_blah    (member variable)
    exe rstr .  "s/\\<m_\\(\\a\\S*\\)\\>/_\\l\\1"    
endfunction

" Change <F4> to your favorite Hungarian-removal key combo!
map <F4> :call HungarianToEnglish()<CR> 


