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
" June 26, 2002 -> ver 1.0.2
" - removal of aBlah, eBlah, _blah, chBlah
" - better handling of m_bBlah and its friends
" - fixed it so it works not just on the first offending instance
"   of each line
" - corrected incorrectly changing identifiers like PRINTF when
"   ignorecase is turned on
"
" TODO:
" - add all the most obnoxious instances of this curious coding
"   convention.  See the Microsoft site for details!
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! HungarianToEnglish() range
    let rstr = a:firstline . "," . a:lastline " haha, this looks hungarian ;)
    " convert m_bBlah   (member variable) <- do this first so we also get rid
    "                                        of other decorations on member variables
    exe rstr .  "s/\\C\\<m_\\(\\a\\S*\\)\\>/\\l\\1/g"    
    " convert _blah     (member variable) <- Maybe this is going too far...
    exe rstr .  "s/\\C\\<_\\(\\a\\S*\\)\\>/\\l\\1/g"    
    " convert eBlah     (enumerated type)
    exe rstr .  "s/\\C\\<e\\(\\u\\S*\\)\\>/\\l\\1/g"
    " convert aBlah     (array)
    exe rstr .  "s/\\C\\<a\\(\\u\\S*\\)\\>/\\l\\1/g"
    " convert uiBlah    (unsigned int)
    exe rstr .  "s/\\C\\<ui\\(\\u\\S*\\)\\>/\\l\\1/g"
    " convert bBlah     (boolean or byte, who knows?)
    exe rstr .  "s/\\C\\<b\\(\\u\\S*\\)\\>/\\l\\1/g"    
    " convert iBlah     (integer)
    exe rstr .  "s/\\C\\<i\\(\\u\\S*\\)\\>/\\l\\1/g"    
    " convert pBlah     (pointer)
    exe rstr .  "s/\\C\\<p\\(\\u\\S*\\)\\>/\\l\\1/g"    
    " convert szBlah    (string terminated by a zero)
    exe rstr .  "s/\\C\\<sz\\(\\u\\S*\\)\\>/\\l\\1/g"    
    " convert lpszBlah  (long pointer to a string terminated by a zero)
    exe rstr .  "s/\\C\\<lpsz\\(\\u\\S*\\)\\>/\\l\\1/g"    
    " convert chBlah    (character)
    exe rstr .  "s/\\C\\<ch\\(\\u\\S*\\)\\>/\\l\\1/g"    
endfunction

" Change <F4> to your favorite Hungarian-removal key combo!
map <F4> :call HungarianToEnglish()<CR> 


