function WC() range
	echo line2byte(a:lastline+1)-line2byte(a:firstline) . " bytes"
endfunction
"command -range WC <line1>,<line2>call WC ()
map <M-x> :call WC()<CR>
