let n = 32
while n < 127
    if n == 38
	silent! exec '%s/&#38;/\&amp;/g'
    elseif n == 47
	silent! exec '%s/&#47;/\//g'
    else
	silent! exec '%s/&#' . n . ';/' . nr2char(n) . '/g'
    endif
    let n = n + 1
endwhile
