" Vim indent file
" Language:     TTCN-3
" Maintainer:   Stefan Karlsson <stefan.74@comhem.se>
" Last Change:  15 April 2004

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1


setlocal indentexpr=Get_ttcn_indent(v:lnum)

setlocal indentkeys=0{,0},0),!^F,o,O,e
setlocal cinwords=

setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://


if exists("*Get_ttcn_indent")
    finish
endif

function! Get_previous_code_line(lnum)
    let i = a:lnum - 1
    while i > 0
        let i = prevnonblank(i)
        if getline(i) =~ '\*/\s*$'
            while getline(i) !~ '/\*' && i > 1
                let i = i - 1
            endwhile
            if getline(i) =~ '^\s*/\*'
                let i = i - 1
            else
                break
            endif
        elseif getline(i) =~ '^\s*//'
            let i = i - 1
        else
            break
        endif
    endwhile

    return i
endfunction

let s:w0 = '^\s*\i\+\s*:='

let s:w1 = '^\s*\(\<module\>\|\<group\>\|\<type\s\+component\>\|\<function\>'
           \ . '\|\<testcase\>\|\<control\>\|\<alt\(step\)\?\>\|\<while\>'
           \ . '\|\<do\>\|\<for\>\|\<if\>\|\<else\>\|\<interleave\>'
           \ . '\|\(\(var\|const\|template\)\s\+\i\+\s\+\)\i\+\s\+:='
           \ . '\|\<template\s\+\i\+\s\+\i\+\s*\(:=\|(\)\?\s*$\)'

function Get_ttcn_indent(lnum)

    let n = Get_previous_code_line(a:lnum)

    let prevl = getline(n)
    let curl  = getline(a:lnum)

    if prevl =~# s:w0
        if curl =~ '^\s*}'
            let ind = indent(n) - &sw
        else
            let ind = indent(n)
        endif
    elseif curl =~# s:w0
        let n   = searchpair('{','','}','Wbn')

        while (n > 0) && getline(n) !~# s:w1
            let n = Get_previous_code_line(n)
        endwhile

        if getline(n) =~# '\<alt\(step\)\?\>\|\<interleave\>'
            let ind = indent(n) + 2*&sw
        else
            let ind = indent(n) + &sw
        endif
    else
        let ind = cindent(a:lnum)
    endif

    return ind
endfunction
