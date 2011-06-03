"====================================================================
" What Is This: Dictionary for Korean
" Author: SungTae Moon <munhoney@gmail.com, http://munhoney.blogspot.com>
" Last Change: Sun, 01 July 2007
" Version: 0.5
" Usage:
"  :Dic
"     Show dictionary 
"==================================================================== 

command! -nargs=* Dic call Dictionary(0,<f-args>)

"**************************************************************
"* Dictionary : show meaning of word 
"*-------------------------------------------------------------
"*  Not yet 
"**************************************************************
function! Dictionary(...)

    let vheight = 5
    let word = expand("<cword>")

    " Init 
    if ( word == "" )
	return
    endif

    " Build Window 

    let vwinnum=bufnr('__Dictionary')

    if getbufvar(vwinnum, 'Dictionary') == 'Dictionary'
	let vwinnum = bufwinnr(vwinnum)
    else
	let vwinnum = -1
    endif

    if ( vwinnum >= 0 )
	if vwinnum != bufwinnr('%')
	    execute "normal \<c-w>".vwinnum."w"
	endif
	setlocal modifiable
	silent %d _
    else

	" Make Title
	if (!exists('s:bufautocommandsset'))
	    auto BufEnter *Dictionary let b:sav_titlestring = &titlestring | let &titlestring = '%{strftime("%c")}' | let b:sav_wrapscan = &wrapscan
	    auto BufLeave *Dictionary let &titlestring = b:sav_titlestring | let &wrapscan = b:sav_wrapscan
	    let s:bufautocommandsset=1
	endif

	execute 'bo '.vheight.'split __Dictionary'
	setlocal noswapfile
	setlocal buftype=nowrite
	setlocal bufhidden=delete
	setlocal nonumber
	setlocal nowrap
	setlocal norightleft
	setlocal foldcolumn=0
	setlocal modifiable
	setlocal nolist
	set nowrapscan

	let b:Dictionary = 'Dictionary' 
    endif

    "call Parsing_Naver(word)
    call Parsing_Naver_EEDic(word)
    let curpos = ['.', 0, 0, 0]
    call setpos('.', curpos)
    setlocal nomodifiable
    execute "normal \<c-w>wk"


endfunction

"**************************************************************
"*  Parsing_Naver_EEDic : English-English Dictionary
"*-------------------------------------------------------------
"*  word : a word 
"**************************************************************
function Parsing_Naver_EEDic(word)

    let temp_mean = ""
    let isPart = 0
    let lines = []
    
    " Search Dictionary Hompage
    call system("wget -O output eedic.naver.com/search.naver?dic_where=eedic\\&query=" . a:word)
    
    " Check file open 
    if ( !filereadable("output") )
	echo "Cannot file "
	return ''	
    endif
 
    " Read & Catch source 
    for line in  readfile("output", '', 1024)
	if ( match(line, "source_contents") > 0 )
	    "let temp_mean =  line
	    break
	endif
    endfor
    
    " Parsing Html Source 
    let line =  substitute(line, "&lt;", "<", "g")
    let line =  substitute(line, "&gt;", ">", "g")
    let line =  substitute(line, "&quot;", "\"", "g")
    let line =  substitute(line, "<br>", "\n", "g")
    let line =  substitute(line, "nbsp;", " ", "g")
    let line =  substitute(line, "&amp;", "", "g")
    let line =  substitute(line, "<img[^>]*>", "", "g")
    let line =  substitute(line, "<strong>", "", "g")
    let line =  substitute(line, "</strong>", "", "g")
    let line =  substitute(line, "<b>", "", "g")
    let line =  substitute(line, "</b>", "", "g")
    let line =  substitute(line, "<emdash>", "", "g")
    let line =  substitute(line, "\n", "", "g")
    let line =  substitute(line, "<span", "\n<span", "g")

    " Add List
    let index = 0
    let pos = 0
    while ( pos  < strlen(line) )
	let pos = match(line, "\n")
	let temp_line = strpart(line, 0, pos)
	if ( match(temp_line, "span" ) > 0 )
	    call add(lines, temp_line) 
	    let index = index + 1
	endif
	let pos = pos + 1	
	let line = strpart(line,  pos, strlen(line))
    endwhile
    
    execute 'syn match CalNavi "'.a:word.'"'
    
    " Display
    for line in lines
	if ( match(line, "hwme2") > 0 )
	    let line =  substitute(line, "<span[^>]*>", "", "g")
	    let line =  substitute(line, "</span>", "", "g")
	    exec "normal! i".line."\<cr>"
	elseif ( match(line, "dnum") > 0 )
	    let line =  substitute(line, "<span[^>]*>", "", "g")
	    let line =  substitute(line, "</span>", "", "g")
	    "execute 'syn match CalNavi "'.line.'"'
	    exec "normal! i".line
	elseif ( match(line, "posp") > 0 )
	    let line =  substitute(line, "<span[^>]*>", "", "g")
	    let line =  substitute(line, "</span>", "", "g")
	    "execute 'syn match CalNavi "'.line.'"'
	    exec "normal! i".line."\<cr>"
	elseif ( match(line, "denf") > 0 )
	    let line =  substitute(line, "<span[^>]*>", "", "g")
	    let line =  substitute(line, "</span>", "", "g")
	    exec "normal! i".line."\<cr>"
	elseif ( match(line, "egph") > 0 )
	    let line =  substitute(line, "<span[^>]*>", "", "g")
	    let line =  substitute(line, "</span>", "", "g")
	    exec "normal! i"."ex) ".line."\<cr>"
	elseif ( match(line, "c_word") > 0 )
	    let line =  substitute(line, "<span[^>]*>", "", "g")
	    let line =  substitute(line, "</span>", "", "g")
	    exec "normal! i"."SEE ".line."\<cr>"
	endif
    endfor
endfunction

hi def link CalNavi     Search
hi def link CalSaturday Statement
hi def link CalSunday   Type
