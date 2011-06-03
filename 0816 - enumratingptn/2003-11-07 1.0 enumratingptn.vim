" Enumerate rating pattern {{{1
"  Maintainer: hira@users.sourceforge.jp
" Last Change: 2003/11/08 (Sat) 07:45:45.
"     Version: 1.0

" example {{{1
" call Rating(1, 6)
" > L<1> H<1> U<4>
" 
" call Rating(42, 37)
" > L<15> H<2> U<20>
" > L<13> H<7> U<17>
" > L<11> H<12> U<14>
" > L<9> H<17> U<11>
" > L<7> H<22> U<8>
" > L<5> H<27> U<5>
" > L<3> H<32> U<2>
" 
" call Rating(1864, 570)
" > E169
" 
" call FastRating(1864, 570)
" > L<486> H<2> U<82>
" > L<484> H<7> U<79>
" > L<482> H<12> U<76>
" > ..................
" > L<436> H<127> U<7>
" > L<434> H<132> U<4>
" > L<432> H<137> U<1>

function! Rating(p, n) "{{{1
    call RatingR(0, 0, a:p, a:n, "L")
endfunction

function! RatingR(l, h, p, n, mode) "{{{1
    let l:p = 4*a:l+a:h
    if a:n == 0
        return
    elseif l:p - a:n > a:p
        return
    elseif l:p - a:n == a:p
        echo "L<".a:l."> H<".a:h."> U<".a:n.">"
        return
    endif
    if a:mode == "L"
        call RatingR(a:l+1, a:h, a:p, a:n-1, a:mode)
    endif
    call RatingR(a:l, a:h+1, a:p, a:n-1, "H")
endfunction

function! FastRating(p, n) "{{{1
    let l:l = (a:p+a:n)/5
    let l:subn = a:n   - l:l
    let l:subp = l:l*4 - a:p
    let l:big = l:subn > l:subp ? l:subn : l:subp
    let l:sml = l:subn < l:subp ? l:subn : l:subp
    let l:h = (l:big - l:sml) / 2
    let l:u = l:subn - l:h
    while 1
        if l:l < 0 || l:u < 0
            return
        end
        echo "L<".l:l."> H<".l:h."> U<".l:u.">"
        let l:l = l:l - 2
        let l:h = l:h + 5
        let l:u = l:u - 3
    endwhile
endfunction

"}}}1
" vim:set nowrap foldmethod=marker expandtab:
