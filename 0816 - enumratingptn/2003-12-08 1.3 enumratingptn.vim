" Enumerate rating pattern {{{1
"  Maintainer: hira@users.sourceforge.jp
" Last Change: 2003/12/09 (Tue) 15:19:40.
"     Version: 1.3

" example {{{1
" $ vim enumratingptn.vim
" :so %
" :call Rating(1, 6)
" :call Rating(42, 37)
" :call Rating(1864, 570)

function! Rating(p, v) "{{{1
    if a:v < 0
        echo "ERROR: vote must be not minus"
        echo "usage: Rating(point, vote)"
        return 0
    endif
    let l:v = a:v
    let l:a = 0
    let l:l = 0
    while 1
        if l:v == 0
            if a:p == l:a
                echo ">>>L<".l:l."> H<0> U<0>"
            endif
            return 0
        endif
        let l:dst   = a:p - l:a
        let l:apdst = s:Abs(l:dst)
        let l:vddst = s:Abs(l:v - l:apdst)
        let l:rem   = (vddst / 2) + (s:Even(vddst) ? 0 : 1)
        "echo "l:dst<".l:dst."> l:apdst<".l:apdst."> l:vddst<".l:vddst."> l:rem<".l:rem.">"
        if l:apdst <= l:v && ((s:Even(l:apdst) && s:Even(l:v)) || (s:Odd(l:apdst) && s:Odd(l:v)))
            if 0 < l:dst
                echo ">>>L<".l:l."> H<".(l:apdst + l:rem)."> U<".l:rem.">"
            else
                echo ">>>L<".l:l."> H<".l:rem."> U<".(l:apdst + l:rem).">"
            endif
        endif
        let l:v = l:v - 1
        let l:a = l:a + 4
        let l:l = l:l + 1
    endwhile
endfunction

" local functions {{{1
function! s:Abs(n)
    return a:n < 0 ?  0 - a:n : a:n
endfunction
function! s:Even(n)
    return a:n % 2 == 0
endfunction
function! s:Odd(n)
    return a:n % 2 == 1
endfunction

" in scheme {{{1
"(define (rate? point vote)
"    (if (< vote 0)
"        (display "error")
"        (let plus4 ((vote   vote)
"                    (answer 0)
"                    (p4     0))
"            (if (= 0 vote)
"                (if (= point answer)
"                    (begin (display (list 0 0 p4)) (newline)))
"                (let* ((dst   (- point answer))
"                       (apdst (abs dst))
"                       (vddst (abs (- vote apdst)))
"                       (rem   (+ (/ vddst 2) (if (even? vddst) 0 1))))
"                    (and (<= apdst vote)
"                         (or (and (even? apdst) (even? vote))
"                             (and (odd?  apdst) (odd?  vote)))
"                         (if (< 0 dst)
"                             (display (list p4 (+ apdst rem) rem))
"                             (display (list p4 rem (+ apdst rem)))))
"                    (plus4 (- vote 1) (+ answer 4) (+ p4 1)))))))
"}}}1
" vim:set nowrap foldmethod=marker expandtab:
