" Name:     svnst.vim (ctags and cscope vim extends script)
" Brief:    Usefull tools reading code or coding
" Version:  1.0.0
" Date:     2011/06/23 19:35:21
" Author:   Chen Zuopeng (EN: Daniel Chen)
" Email:    chenzuopeng@gmail.com
"
" License:  Public domain, no restrictions whatsoever
"
" Copyright:Copyright (C) 2009-2010 Chen Zuopeng {{{
"           Permission is hereby granted to use and distribute this code,
"           with or without modifications, provided that this copyright
"           notice is copied with it. Like anything else that's free,
"           svnst.vim is provided *as is* and comes with no
"           warranty of any kind, either expressed or implied. In no
"           event will the copyright holder be liable for any damages
"           resulting from the use of this software. 
"           }}}
" Todo:     Easy way using svn status in vim
"           
" Usage:    This file should reside in the plugin directory and be {{{
"           automatically sourced.
"           You may use the default keymappings of
"             <Leader>st         - svn st from current directory
"           }}}
" UPDATE:   1.0.0 {{{
"             Descript:
"           }}}
" Check for Vim version 600 or greater {{{
if exists("g:svnst_version")
	"finish
endif

let g:svnst_version = "1.0.0"

if v:version < 600
    echo "Sorry, svnst" . g:svnst_version. "\nONLY runs with Vim 6.0 and greater."
    finish
endif

let s:svn_exe_name   = "svn"
let s:svn_status_arg = "status"
"}}}
"SubversionStatus {{{
function! SubversionStatus (dirname)
	let s:svn_pip = system (s:svn_exe_name . " " . s:svn_status_arg . " " . a:dirname)

    let l:bname = "Subversion status result"

    let l:winnum =  bufwinnr (l:bname)
    "If the list window is open
    if l:winnum != -1
        if winnr() != winnum
            " If not already in the window, jump to it
            exe winnum . 'wincmd w'
        endif
        "Focuse alread int the list window
        "Close window and start a new
        :q!
    endi
    
    setlocal modifiable
    " Open a new window at the bottom
    exe 'silent! botright ' . 'split ' . l:bname
	let str_help = "--------------------------> Press m ? s to grep the result below <-----------------------------\n"
	let str_help = str_help . s:svn_pip
    0put = str_help

    " Mark the buffer as scratch
    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal noswapfile
    setlocal nowrap
    setlocal nobuflisted
    normal! gg

    nmap <buffer><silent>m :call GrepStartWith_M ()<CR>
    nmap <buffer><silent>? :call GrepStartWith_Q ()<CR>
    nmap <buffer><silent>s :call GrepStartWith_S ()<CR>
endfunction
"}}}
"GrepStartWith_Q {{{
function! GrepStartWith_Q ()
	exec ":g/^[^?]/d"
endfunction
"}}}
"GrepStartWith_S {{{
function! GrepStartWith_S ()
	exec ":g/^[^ ]/d"
endfunction
"}}}
"GrepStartWith_M {{{
function! GrepStartWith_M ()
	exec ":g/^[^M]/d"
endfunction
"}}}
"--maping--- {{{
:map <Leader>st :call SubversionStatus (getcwd ()) <CR>

" vim600:fdm=marker:fdc=4:cms=\ "\ %s:
