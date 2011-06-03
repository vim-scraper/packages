let s:pattern = "\\C[-/+,A-Za-z0-9]\\+"
let s:decoder =                                 "332 332 333 --- 333 "
\ . "310 311 312 313 320 321 322 323 330 331 --- --- --- --- --- --- "
\ . "--- 122 123 130 131 132 133 200 201 202 203 210 211 212 213 220 "
\ . "221 222 223 230 231 232 233 300 301 302 303 --- --- --- --- --- "
\ . "--- 000 001 002 003 010 011 012 013 020 021 022 023 030 031 032 "
\ . "033 100 101 102 103 110 111 112 113 120 121 "

inoremap <M-F10> <C-R>=<SID>CueCat("")<Left><Left>
inoremap <A-F10> <C-R>=<SID>CueCat("")<Left><Left>
function! s:CueCat(x)
    let start = 0
    let ret = ""
    while 1
	let i = match(a:x, s:pattern, start)
	if i == -1
	    let ret = ret . strpart(a:x, start)
	    if ret =~# "^\.\\d\\{18}\.[A-Z0-9]\\{3}\.\\d\\{13,18}\.$"
		silent ! artsplay /usr/share/sounds/pop.wav; true
		let ret = strpart(ret, 24, strlen(ret) - 25)
		      \ . " #" . strpart(ret, 20, 3)
	    else
		silent ! artsplay /usr/share/sounds/error.wav; true
	    endif
	    return ret . "\n"
	endif
	let ret = ret . strpart(a:x, start, i - start)
	let start = matchend(a:x, s:pattern, start)
	let y = ""
	while i < start
	    let y = y . strpart(s:decoder, (char2nr(a:x[i]) - 43) * 4, 3)
	    let i = i + 1
	endwhile
	let i = 0
	while i + 4 <= strlen(y)
	    let ret = ret . nr2char(64 - y[i  ] * 64
			\              + y[i+1] * 16
			\              + y[i+2] *  4
			\	   + 3 - y[i+3]     )
	    let i = i + 4
	endwhile
    endwhile
endfunction
