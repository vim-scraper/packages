
" Description: open N files in each tab each one.
" Usage:       :NTABS file1 file2 ...  fileN
" Synonyms:    :NTABS, :ETABS, TABS, OPENTABS
         
" Author:      Yakov Lerner iler.ml@gmail.com

if exists("g:newtabs") | finish | endif
let g:newtabs= 1

function! OpenTabs(...)
    let argc = a:0
    let iarg = 0
    if argc == 0 | echoerr "Missing argument(s)" | return | endif
    while iarg < argc 
        let iarg = iarg + 1
        " if buffer is unnamed and empty and not modified, do not create new tab
        if expand('%') == '' && getline('.') == '' && !&modified
            exe ':edit '.a:{iarg}
        else
            exe ':tabnew '.a:{iarg}
        endif
    endwhile
endfun

:command! -nargs=+ TABS     call OpenTabs(<f-args>)
:command! -nargs=+ OPENTABS call OpenTabs(<f-args>)
:command! -nargs=+ ETABS    call OpenTabs(<f-args>)
:command! -nargs=+ NTABS    call OpenTabs(<f-args>)
