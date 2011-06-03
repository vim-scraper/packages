if &cp || exists("b:loaded_htip")
 finish
endif

let b:loaded_htip = "0.1"
let s:keepcpo = &cpo
set cpo&vim

let g:htip_what = ""
let g:htip_how = ""
let g:htip_buffer = []
let g:htip_pos = -1

fun! s:show()
    let s = g:htip_buffer[g:htip_pos]
    if s =~ "^\<interactive\>"
        echohl ErrorMsg
    endif
    echomsg (g:htip_pos + 1 ). '/' . len(g:htip_buffer) . ': ' . s
    echohl None
endfun

fun! s:next()
    let g:htip_pos=g:htip_pos + 1
    if len(g:htip_buffer) == g:htip_pos
        let g:htip_pos = 0
    endif
    call s:show()
endfun

fun! s:prev()
    let g:htip_pos = g:htip_pos - 1
    if g:htip_pos < 0
        let g:htip_pos = len(g:htip_buffer) - 1
    endif
    call s:show()
endfun

fun! s:htip(what,how)

    if a:what == ""
        return
    endif

    if a:what == g:htip_what && a:how == g:htip_how
        call s:next()
    else
        let g:htip_what = a:what
        let g:htip_how = a:how

        let cmd=
             \ "/bin/sh -c \"echo '" . a:how . " ".a:what."' | /usr/bin/ghci -v0 ".expand("%").
             \ " | sed -e 's/^ *//g' -e '/^$/ d' -e 's/\t/ /g' \""
        let res=system(cmd)
        let lines=split(res, "\n")

        let g:htip_buffer=lines
        let g:htip_pos=0

        call s:show()

    endif

endfun

fun! s:cword_htip(how)
    call s:htip(expand('<cword>'),a:how)
endfun

command! -nargs=? HTip call s:htip(<q-args>)
command! -nargs=1 CWordHTip call s:cword_htip(<q-args>)
command! -nargs=0 HTipNext call s:next()
command! -nargs=0 HTipPrev call s:prev()

if exists('g:htip_bindkeys') && g:htip_bindkeys
    nmap <leader>ht <Esc>:CWordHTip :type<CR>
    nmap <leader>hi <Esc>:CWordHTip :info<CR>
    nmap <leader>hk <Esc>:CWordHTip :kind<CR>
    nmap <leader>hb <Esc>:CWordHTip :browse<CR>
    nmap <leader>hh <Esc>:HTipNext<CR>
    nmap <leader>hH <Esc>:HTipPrev<CR>
endif

let &cpo=s:keepcpo
unlet s:keepcpo

