if &cp || exists("b:loaded_mantip")
 finish
endif

let b:loaded_mantip = "0.1"
let s:keepcpo = &cpo
set cpo&vim

let g:mantip_what = ""
let g:mantip_where = ""
let g:mantip_buffer = []
let g:mantip_pos = -1
let g:mantip_section = ""

fun! s:show()
    echomsg g:mantip_section . " " . (g:mantip_pos + 1 ). '/' . len(g:mantip_buffer) . ': ' g:mantip_buffer[g:mantip_pos]
endfun

fun! s:ManTipNext()
    let g:mantip_pos=g:mantip_pos + 1
    if len(g:mantip_buffer) == g:mantip_pos
        let g:mantip_pos = 0
    endif
    call s:show()
endfun

fun! s:ManTipPrev()
    let g:mantip_pos = g:mantip_pos - 1
    if g:mantip_pos < 0
        let g:mantip_pos = len(g:mantip_buffer) - 1
    endif
    call s:show()
endfun

fun! s:focus()
    let i = 0
    let g:mantip_pos = 0
    while i < len(g:mantip_buffer)

        let s = g:mantip_buffer[i]

        if s =~ g:mantip_what
            let g:mantip_pos = i
            break
        endif

        let i = i + 1
    endwhile

    call s:show()
endfun

fun! s:ManTip(what,where)

    if a:what == "" 
        return
    endif

    if a:what == g:mantip_what && a:where == g:mantip_where
        call s:ManTipNext()
    else
        let g:mantip_what = a:what
        let g:mantip_where = a:where

        let where=""
        if a:where != ""
            let where="-S ".a:where
        endif

        let cmd=
             \ "/bin/sh -c \"/usr/bin/man " . where . " ". a:what . " " .
             \ "| sed -n -e '1 p' -e '/^SYNOPSIS/,$ p'" .
             \ "| sed -e '2 d' -e '/^D.*/,$ d' -e '/^$/ d' -e '/ *#/ d' -e 's/^ *//g' \""
        let res=system(cmd)
        let lines=split(res, "\n")

        if len(lines) > 1

            let g:mantip_buffer=lines[1:]
            let g:mantip_pos=0
            let g:mantip_section=split(lines[0])[0]

            call s:focus()
        else
            echohl ErrorMsg
            echomsg lines[0]
            echohl None
        endif

    endif

endfun

fun! s:CWordManTip(where)
    call s:ManTip(expand('<cword>'),a:where)
endfun

command! -nargs=? ManTip call s:ManTip(<q-args>)
command! -nargs=1 CWordManTip call s:CWordManTip(<q-args>)
command! -nargs=0 ManTipNext call s:ManTipNext()
command! -nargs=0 ManTipPrev call s:ManTipPrev()

if exists('g:mantip_bindkeys') && g:mantip_bindkeys
    nmap <leader>d <Esc>:CWordManTip ''<CR>
    nmap <leader>D <Esc>:ManTipPrev<CR>
    nmap <leader>~ <Esc>:!whatis <cword><CR>

    nmap <leader>1 <Esc>:CWordManTip "1"<CR>
    nmap <leader>2 <Esc>:CWordManTip "2"<CR>
    nmap <leader>3 <Esc>:CWordManTip "3"<CR>
    nmap <leader>4 <Esc>:CWordManTip "4"<CR>
    nmap <leader>5 <Esc>:CWordManTip "5"<CR>
    nmap <leader>6 <Esc>:CWordManTip "6"<CR>
    nmap <leader>7 <Esc>:CWordManTip "7"<CR>
    nmap <leader>8 <Esc>:CWordManTip "8"<CR>
    nmap <leader>9 <Esc>:CWordManTip "9"<CR>
endif

let &cpo=s:keepcpo
unlet s:keepcpo

" printf
