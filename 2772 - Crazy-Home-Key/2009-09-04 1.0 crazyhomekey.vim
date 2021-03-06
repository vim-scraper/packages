" crazyhomekey.vim - Finetune Home key for Programmer
" Maintainer:   Kent Chen
" Version:      1.0
"
" Usage:
"    a. Normal mode
"        1. Press <HOME> key in the middle of a line
"        2. Press <HOME> key again
"    b. Insert mode
"       do the same as above. It works well ^_^
"
" Installation:
" Place in ~/.vim/plugin (to load at start up)
"
" Recommanded: 
" Add below mapping to your .vimrc or uncomment below 
" Use <Alt-H> move to home, <Alt-L> move to the end
"     nmap h      :call ToggleHomeActionN()<CR>
"     imap h <ESC>:call ToggleHomeActionI()<CR>
"     map l $
"

if v:version < 700
    finish
endif

" finetune <Home> key action to fit programmer
function! GetPrevChar()
    if col('.') == 1
        return "\0"
    endif
    return strpart(getline('.'), col('.')-2, 1)
endfunction

function! ToggleHomeActionN()
    if (GetPrevChar() == " " || GetPrevChar() == "\t") 
        call feedkeys("0", 'n')
        " This works too!
        " call setpos('.', [0, line('.'), 1])
    else   
        call feedkeys("^", 'n')
    endif
endfunction

function! ToggleHomeActionI()
    if (GetPrevChar() == " " || GetPrevChar() == "\t") 
        " call setpos('.', [0, line('.'), 1])
        call feedkeys("\<Home>i")
    else   
        call feedkeys("\<Esc>^i")
    endif
endfunction

nmap [1~      :call ToggleHomeActionN()<CR>
imap [1~ <ESC>:call ToggleHomeActionI()<CR>

" vim:set ft=vim et sw=4 sts=4:
