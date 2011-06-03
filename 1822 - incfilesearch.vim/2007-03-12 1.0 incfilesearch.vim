"============================================================================
"        File: incfilesearch.vim
"
" Description: Find a file to edit with incremental search inspired by
"              incbufsearch.vim by Michael Denio (mike at imagine-sw.com).
"              http://www.vim.org/scripts/script.php?script_id=685
"
"      Author: Greg McIntyre (vim at puyo.cjb.net)
"
"     Version: 1.0
"
"     Install: Copy to Vim's plugin directory
"
"       Usage: (1) Define a normal key mapping to :IncFileSearch or leave the
"                  default of <C-O>
"              (2) When activated, start typing the name of the buffer you want
"                  to switch to.
"              (3) Press <BS> if you make an error
"              (4) Press <CR> to edit the first buffer in the remaining match
"                  list
"              (5) Press <ESC> to quit and continue editing the current buffer
"
" $Id$
" 
"     History:
"     
" 2007/03/13 puyo Created
"
"============================================================================

" Install command and default key mapping
if !exists(":IncFileSearch")
  command IncFileSearch :call <SID>IncFileSearch()
endif

if !hasmapto("<Plug>IncBufferSwitch")
    nmap <silent> <unique> <C-O> :IncFileSearch<CR>
endif

"
" Switch to a file in the current directory that matches the partial name we have thus far
" 
function! <SID>PartialFileSearch(fileList, partialName, first)
    let flag = 0
	let s:buflist = []
	if strlen(a:partialName) == 0
		return
	endif
	for filename in a:fileList
		if (match(filename, a:partialName) > -1)
			if flag == s:tabStop
				if a:first == 0
					"execute "silent edit " filename
					"redraw
				endif
			endif
			let s:buflist += [filename]
			"add(s:buflist, filename)
			let flag = flag + 1
		endif
    endfor

    if flag == s:tabStop + 1
        let s:tabStop = - 1
    endif
endfunction

function! <SID>PrintList()
	let fullStr = join(s:buflist, ',')
	let cropPos = max([0, winwidth(0) - 10])
	let croppedStr = fullStr[0:cropPos]
	if strlen(croppedStr) < strlen(fullStr)
		let croppedStr = croppedStr[0:-3] . "..."
	endif
	echon croppedStr
endfunction

"
" Perform an incremental buffer switch
"
function! <SID>IncFileSearch()
    let origBuffer = bufname("%")
    let files = split(glob("**"), "\n")
    let partialFileName = ""
    let s:tabStop = 0
    let s:buflist = []

    call s:PartialFileSearch(files, '', 1)
	call s:PrintList()
    echon "\nFiles matching: "

    while 1 == 1
        let flag  =0
        let rawChar = getchar()
        if rawChar == 13 " <CR>
			echo s:buflist[0]
			execute "silent edit " s:buflist[0]
            break
        endif
        if rawChar == 27 " <ESC>
            break
        endif
        if rawChar == "\<BS>"
             let s:tabStop = 0
             if strlen(partialFileName) > 0
                let partialFileName = strpart(partialFileName, 0, strlen(partialFileName) - 1)
                if strlen(partialFileName) == 0
                   let flag = 1
                endif
             else
                continue
             endif
        elseif rawChar == 9 " TAB -- find next matching buffer
            let s:tabStop = (s:tabStop == -1) ? 0 : s:tabStop + 1
        else
            let nextChar = nr2char(rawChar)
            let partialFileName = partialFileName . nextChar
        endif

        call s:PartialFileSearch(files, partialFileName, flag)
        redraw
		call s:PrintList()
		echon "\nFiles matching: " . partialFileName
    endwhile
endfunction
