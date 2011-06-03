" File:           TimeIt.vim
" URL:            http://www.vim.org/scripts/script.php?script_id=1622
" Author:         Yakov Lerner <iler.ml@gmail.com>
" Last Changed:   2007 Oct 14
" Contributions:  Andy Wokula, anwoku*yahoo#de (*# -> @.)
"
" Time execution of one vim command(s) with subsecond resolution,
" For example:
"      :TIM sleep 100m
" prints
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

if exists("g:loaded_timeit") | finish | endif
let g:loaded_timeit = 1

if !has("reltime")
   echo "TimeIt: Your Vim is compiled without '+reltime' feature"
   finish
endif

command! -nargs=* -count TIMEIT :call TIMEIT(<count>,<q-args>)

function! TIMEIT(count, args)
    let repeat = (a:count <= 0 ? 1 : a:count)
    let k = 0
    let start = reltime()
    while k < repeat
        exe a:args
        let k = k + 1
    endwh
    let time = reltimestr(reltime(start))

    redraw

    if repeat == 1
       echomsg "Execution took " . time ." sec."
    else
       echomsg repeat . " repetitions took ". time ." sec."
    endif
endfu
