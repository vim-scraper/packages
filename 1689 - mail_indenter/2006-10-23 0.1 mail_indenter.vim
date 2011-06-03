""""""""""""""""""""""""""""""""""""""""""""""""
"  Author:  Mauro David Sauco <mauro@sauco.net>
"  Date:    Aug 10, 2006
"  Version: 0.1
"  License: GPL2
""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""
" <F2> will set the Recipient's Name 
" <F3> toggle start and end of block to be indented (will ask you for
"      Recipient's name if not set yet).
" <F4> Marks the end of the block to be indented and asks for a new
"      Recipient's name. (sometimes you want to indent many different blocks
"      with different names block of text)
" <F5> Indent everything from the current cursor position to the end of the
"      message (will ask you for Recipient name if not set yet)
""""""""""""""""""""""""""""""""""""""""""

map <F2> :call SetRcp()<CR>
map <F3> :call Marker()<CR>
map <F4> :call Newrcp()<CR>
map <F5> :call DoAll()<CR>
set tw=72
set fo=tcqrm

""""""""""""""""""""""""""""""""""""""""""

fun! Marker()
    if ! exists("g:mstat")
        let g:mstat = 0
    endif
    if g:mstat == 0
        echo "Started Selection"
        exe "normal ms"
        let g:mstat = 1
    else
        exe "normal me"
        let g:mstat = 0
        if ! exists("g:rcpname")
            call SetRcp()
        endif
        exe "normal `s\<C-v>`e"
        exe "normal I".g:rcpname."\<ESC>\<ESC>"
        exe "normal `s\<C-v>`e"
        exe "normal gq"
    endif
endfun

fun! SetRcp()
    let g:rcpname = input("Who? ")
    exe ":set com+=n:".g:rcpname."> "
endfun

fun! DoAll()
    if ! exists("g:rcpname")
        call SetRcp()
    endif
    exe "normal ma"
    exe "normal G"
    let line = getline(".")
    while line == ""
        let line = getline(".")
        exe "normal dd"
    endwhile
    exe "normal `a"
    exe "normal \<C-v>G"
    exe "normal I".g:rcpname."\<ESC>\<ESC>"
    exe "normal `a"
    exe "normal \<C-v>G"
    exe "normal gq"
endfun

fun! Newrcp()
    call SetRcp()
    call Marker()
endfun


