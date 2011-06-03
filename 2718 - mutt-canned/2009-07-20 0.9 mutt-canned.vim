" my mutt canned response function(s)
" 
" To install this, add it to ~/.vim/scripts/ or somewhere
" and add something like the following:
"
"   au BufRead /tmp/mutt-* source ~/.vim/scripts/mutt-canned.vim
"   au BufRead ./example.file source ./mutt-canned.vim
"
" 0) echo lol, canned response > ~/.canned/lol
"
" 1.a) insert canned response at current position:
"      hammer CTRL-X (twice or more) to hop through them
"
" 1.b) insert canned response, replacing selected text:
"      highlight something with V and then hammer CTRL-X to replace it
"
" 1.c) build new canned response:
"      highlight something with V and hammer CTRL-V (twice) to build a new canned response
"
" NOTE: This may actually work nicely in gvim, I wouldn't know, but mean to try
"       that at some point.   Feedback welcome.
"
" Author: Paul Miller <jettero@cpan.org>
" Copyright: Paul Miller
" License: Public Domain
" Repository: http://github.com/jettero/mutt-vim/
" Issue Tracking: http://github.com/jettero/mutt-vim/issues
" VERSION: 0.90
"

fun! Read_Canned() 
    let canned = split(glob("~/.canned/*"), "\n")
    if len(canned)
        for file in canned
            let ftok = split(file, "/")
            let bnam = substitute(ftok[-1], '\s', '_', 'g')

            exec  "menu Canned." . bnam . " :r " . file . ""
        endfor
    endif
endfun

fun! Save_Canned() range
    let lines = getline(a:firstline, a:lastline)

    echo "slurped" len(lines) "lines"
    let response = input("Response Name: ")

    call writefile(lines, expand("~/.canned/" . response))
    call Read_Canned()
endfun

set wildmenu
set cpo-=<
set wcm=<C-X>

call Read_Canned()

 map <C-X><C-X> o<esc>:emenu Canned.<C-X>
vmap <C-X><C-X> dk:emenu Canned.<C-X>
vmap <C-V><C-V> :call Save_Canned()
