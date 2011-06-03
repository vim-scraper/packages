" Vim global plugin which allows navigation via bisection
" Last Change: 2010 oct 25
" Author:      Christophe-Marie Duquesne <chm.duquesne@gmail.com>
" License:     Public domain
" Version:     0.0.1

if exists("loaded_toggle")
    finish
endif
let g:loaded_toggle = 1

function! s:Toggle(opt)
    try
        exec 'set ' . a:opt . '! ' . a:opt . '?'
    catch E518
        try
            exec 'let varvalue = ' . a:opt
            exec 'let ' . a:opt . ' = ' ( 1 - varvalue )
            exec 'let ' . a:opt
        catch E121
            echoerr "Trying to toggle an undefined variable"
        endtry
    endtry
endfunction

function! s:ToggleAll(...)
    try
        for arg in a:000
            call <SID>Toggle(arg)
        endfor
    catch E170
        echoerr "invalid arguments"
    endtry
endfunction

command! -nargs=+ -complete=option Toggle call <SID>ToggleAll(<f-args>)
