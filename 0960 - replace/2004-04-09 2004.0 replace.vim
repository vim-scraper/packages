" replace.vim
" version 2004.0
" by Helder Correia <helder (dot) correia (at) netcabo (dot) pt>
" 
" Vim plugin that asks the user for a string to replace, as well as the string
" he wants to replace with. By default, the script is called by the <F4>
" shortcut.
" 
" To install it, simply copy the script to $HOME/.vim/plugin.
" 
" This file is distributed under the GPL license Version 2, June 1991.
" Copyright (C) 1989, 1991 Free Software Foundation, Inc.  
" 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
" Everyone is permitted to copy and distribute verbatim copies
" of this license document, but changing it is not allowed.


function Replace()
    let s = input("Enter expression to replace: ")
    let r = input("Enter expression for replacing \"" . s . "\" with: ")
    execute "%s/" . s . "/" . r . "/gc"
endfunction


nmap <F4> :call Replace()<CR>
imap <F4> <C-o>:call Replace()<CR>

