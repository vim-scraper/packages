" File:         TimeIt.vim
" Author:       Yakov Lerner <iler.ml@gmail.com>
" Last changed: 2006-07-29
"
" Time execution of one vim command(s) with subsecond resolution, 
" For example:
"      :TIM sleep 100m
" prnits
"      Execution took   0.100301 sec.
" With counter, TIMEIT times that many repetitions of the command:
"      :10TIM sleep 100m
" prints:
"      10 repetitions took   1.039352 sec.
" You can use sequence of |-separated commands under TIM:
"      :10TIM syntax off | syntax on 
" This will time ten repetitions of (syntax off | syntax on)
" sequence, not 10 repetitions of "syntax off" followed by
" one untimes "syntax on".

if exists("g:g:timeit_plugin") | finish | endif
let g:timeit_plugin= 1

command! -nargs=* -range TIMEIT :call TIMEIT(<count>,<q-args>)

function! TIMEIT(count, args) range
    let repeat = (a:count <= 0 ? 1 : a:count)
    let k = 0
    let start=reltime()
    while k < repeat
        exe a:args
        let k = k + 1
    endwh
    let time = reltimestr(reltime(start))

    redraw

    if repeat == 1
        echo "Execution took " . time ." sec."
    else
        echo repeat . " repetitions took ". time ." sec."
    endif
endfu
