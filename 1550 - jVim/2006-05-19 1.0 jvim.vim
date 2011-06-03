" File: jvim.vim
" Author: P.I.Julius (julius AT solutions-i DOT org)
" Version: 1.0
" Last Modified: May 19, 2006
"
" The "jVim plugin is a try to make vim a bit compatible with other source 
" editors. For e.g. in insert mode you'll have CTRL+C, CTRL+V, CTRL+Y, 
" SHIFT+ARROWS to select texts, and the best :) the auto complete popup menu
" which is shown as you type. I like this feature, this one was the only
" one which kept me from using vim, but now I'm a big vim fan :)
"
" You can get more info about the plugin here:
"       http://pijulius.blogspot.com/2006/05/jvimvim.html
"
" Installation
" ------------
" Just copy the jvim.vim to your .vim/plugin/ directory and or restart vim or type:
" :source ~/.vim/plugin/jvim.vim
" Thats it. Enjoy and don't forget this is my first attempt to write a plugin, so any
" feedback is more than welcome.

" NOTE: I use a BMP font because I prefer to have sharp fonts when I code, so you may
" have to change the guifont= below.

" Startup settings
" ----------------

set laststatus=2
set guifont=BmpCour\ 12
set foldcolumn=2
set foldenable
set foldmethod=indent
set nu
set nowrap
set ignorecase
set lines=63
set columns=120
set guioptions-=T
set spell

" Load our color scheme
" ---------------------

"color jvim
color blackdust

" Some function definitions
" -------------------------

function JNextBuffer()
    set hidden
    bn
endfunction

function JPrevBuffer()
    set hidden
    bp
endfunction

function JNewBuffer()
    set hidden
    enew
endfunction

function JVisual(command, mode)
    if a:mode == "i"
        normal v
	
        if a:command == "home"
	    normal g0
	
	elseif a:command == "end" 
	    normal g$
	
	elseif a:command == "up"
	    normal k
	
	elseif a:command == "down"
	    normal j
	
	elseif a:command == "right"
	    normal l
	
	elseif a:command == "left"
	    normal h
	
	elseif a:command == "pageup"
	    normal H
	
	elseif a:command == "pagedown"
	    normal L
	
	endif
	
    elseif a:mode == "v"
	if a:command == "up"
	    normal gv
	    normal gk
	
	elseif a:command == "down"
	    normal gv
	    normal gj
	
	elseif a:command == "right"
	    normal gv
	    normal l
	
	elseif a:command == "left"
	    normal gv
	    normal h
	
	endif
    endif
endfunction

function JVisualCopy()
    normal gv
    normal "+y
endfunction

function JVisualPaste()
    normal p
endfunction

function JVisualDelete()
    normal gv
    normal x
endfunction

" Map some keys for better navigation
" -----------------------------------

map <C-Right> :call JNextBuffer()<CR>
map <C-Left> :call JPrevBuffer()<CR>
map <C-n> :call JNewBuffer()<CR>
map <C-x> :bdelete<CR>

imap <C-y> <C-o>:d<CR>

map <C-z> <Nop>
map <S-Up> <Nop>
map <S-Down> <Nop>

imap <C-z> <C-o>:u<CR>
imap <C-r> <C-o>:red<CR>

imap <C-c> <Nop>
vmap <C-c> :<C-u>call JVisualCopy()<CR>
imap <C-v> <C-o>:call JVisualPaste()<CR>

vmap <BS> :<C-u>call JVisualDelete()<CR>
vmap <Del> :<C-u>call JVisualDelete()<CR>

nmap <Space> :normal za<CR>

" Maps for selection with shift
" -----------------------------

imap <S-Home> <C-o>:call JVisual("home", "i")<CR>
imap <S-End> <C-o>:call JVisual("end", "i")<CR>
imap <S-Up> <C-o>:call JVisual("up", "i")<CR>
imap <S-Down> <C-o>:call JVisual("down", "i")<CR>
imap <S-Left> <C-o>:call JVisual("left", "i")<CR>
imap <S-Right> <C-o>:call JVisual("right", "i")<CR>
imap <S-PageUp> <C-o>:call JVisual("pageup", "i")<CR>
imap <S-PageDown> <C-o>:call JVisual("pagedown", "i")<CR>

vmap <S-Up> :<C-u>call JVisual("up", "v")<CR>
vmap <S-Down> :<C-u>call JVisual("down", "v")<CR>
vmap <S-Left> :<C-u>call JVisual("left", "v")<CR>
vmap <S-Right> :<C-u>call JVisual("right", "v")<CR>

" Map all keys to Auto complete
" -----------------------------

set complete=.,w,b,u,t

let letter = "0"
while letter <= "9"
    execute "imap " letter letter . "<C-n><C-p>"
    let letter = nr2char(char2nr(letter)+1)
endwhile

let letter = "a"
while letter <= "z"
    execute "imap " letter letter . "<C-n><C-p>"
    let letter = nr2char(char2nr(letter)+1)
endwhile
