if &cp || exists("g:auto_wc_loaded")
    finish
endif
let g:auto_wc_loaded = 1

augroup WC
    autocmd!
    autocmd BufUnload,FileWritePre,BufWritePre * call <SID>WC()
augroup END

function! s:WC()
    if &modifiable
        let l:current = 0
        let l:last = line('$')
        let l:charcount = s:CharCount()
        while l:current <= l:last
            let l:line = getline(l:current)
            call s:SearchAndReplace(l:line, l:current, l:charcount)
            let l:current += 1
        endwhile
    endif
endfunction

function! s:CharCount()
    let l:count = 0
    let l:current = 0
    let l:last = line('$')
    while l:current <= l:last
        let l:line = getline(l:current)
        let l:count += strlen(substitute(l:line, ".", "x", "g"))
        let l:current += 1
    endwhile
    return l:count
endfunction

function! s:SearchAndReplace(linetext, lineno, charcount)
    let l:found = match(a:linetext, 'WC:\[\d\{1,}/\d\{1,}]:')
    if l:found >= 0
        let l:pre  = substitute(a:linetext, '\(^.*WC:\[\)\d\{1,}\(/\d\{1,}]:.*$\)', '\1', '')
        let l:post = substitute(a:linetext, '\(^.*WC:\[\)\d\{1,}\(/\d\{1,}]:.*$\)', '\2', '')
        let l:newline = l:pre . a:charcount . l:post
        call setline(a:lineno, l:newline)
    endif
endfunction

finish

==============================================================================
auto_wc.vim : update total character count of current editing file automatically.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/auto_wc.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2008/04/25 15:00:00
==============================================================================

For writers.

Write your document "WC:[0/5000]",
and so,
left number that is total char count of current editing file
will be updated when you are saving file.

Right number is free number space.
For example,
you use right number space to write max char count.

==============================================================================

1. Copy the auto_wc.vim script to
   $HOME/vimfiles/plugin or $HOME/.vim/plugin directory.
   Refer to ':help add-plugin', ':help add-global-plugin' and
   ':help runtimepath' for more details about Vim plugins.

2. Restart Vim.

==============================================================================
" vim: set ff=unix et ft=vim nowrap :
