" Script: DumpStr.vim
" Description: dump string (variable) in such format that allows to clearly
"  distinguish between byte-codes and multibyte character codes. Useful when
"  debugging scripts, to see what's inside unprintable or multibyte strings.
" Usage: source DumpStr.vim
"        call DumpStr(variable)
"        call TestDumpStr()      " test/demo
" Maintainer: Yakov Lerner <jilerner@yahoo.com>

fu! s:DumpChar(ch)
    if a:ch=="\n" || a:ch=="\r" || a:ch=="\t"
        echohl SpecialKey
        echon "^".nr2char(64+char2nr(a:ch))
    else
	" echohl Search
        echon a:ch
    endif
    echohl None 
endfu

fu! DumpStr(str)
    let str=a:str
    let blen = strlen(str)

    echo "String length:   ".strlen(str)." bytes"

    let mblen=strlen(substitute(str, ".", "x", "g"))

    let has_mb = (mblen != strlen(str))
    if mblen == strlen(str)
        echon " (no multibyte characters)"
	echon  " ( encoding=".&encoding." )"
    else
	echon ", ".mblen." chars "
        echo  "                  "
	echon " (string has multibyte characters)"
	echon  " ( encoding=".&encoding." )"
    endif

    if has_mb 
        echo "Char dump:       "
        let r=str
        while r != ""
	    let ch=substitute(r, '^\(.\).*', '\1', "")
	    let r=substitute(r, "^.", "", "")
	    echon char2nr(ch)."("
	    call s:DumpChar(ch)
	    echon ") "
        endw
    endif

    echo "Bytes:          \""
    let i=0
    while i<strlen(str)
        call s:DumpChar(str[i])
	let i=i+1
    endw
    echon "\""
    

    echo "Byte dump:       "
    let i=0
    while i<strlen(str)
        let ch=str[i]
        echon char2nr(ch)."("
	call s:DumpChar(ch)
	echon ") "
	let i=i+1
    endw
    echo "\n"
endfu

function! TestDumpStr()
    " useful demo/testcases fr DumpStr()
    let saved_encoding=&encoding
    echo "\n"
    call DumpStr("abc\e\t\r\n")
    set encoding=utf-8
    call DumpStr("\u1234\x80abc\n")
    let &encoding=saved_encoding
endfu
