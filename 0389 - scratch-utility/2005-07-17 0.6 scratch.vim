" scratch.vim
" Author: Abhilash Koneri <abhilash_koneri at hotmail.com>
" Last Change: 06-Jan-2003 @ 12:47
" Created: 17-Aug-2002
" Version: 0.6
" Download From:
"     http://www.vim.org/script.php?script_id=389
"----------------------------------------------------------------------
" This is a simple plugin that creates a scratch buffer for your
" vim session and helps to access it when you need it.  
"
" If you like the custom mappings provided in the script - hitting
" <F8> should create a new scratch buffer. You can do your scribes
" here and if you want to get rid of it, hit <F8> again inside scratch buffer
" window. If you want to get back to the scratch buffer repeat <F8>.
"
"
" Custom mappings
" ---------------
" The ones defined below are not very ergonomic!
"----------------------------------------------------------------------
"Standard Inteface:  <F8> to make a new ScratchBuffer, <F8>-again to hide one
if !hasmapto('<Plug>ShowScratchBuffer',"n")
  nmap <unique> <silent> <F8> <Plug>ShowScratchBuffer
endif
if !hasmapto('<Plug>InsShowScratchBuffer',"i")
  imap <unique> <silent> <F8> <Plug>InsShowScratchBuffer
endif

" User Overrideable Plugin Interface
nmap <script> <silent> <Plug>ShowScratchBuffer
      \ :silent call <SID>ShowScratchBuffer()<cr>
imap <script> <silent> <Plug>InsShowScratchBuffer
      \ <c-o>:silent call <SID>ShowScratchBuffer()<cr>

command! -nargs=0 Scratch :call <SID>ShowScratchBuffer()

let s:SCRATCH_BUFFER_NAME="[Scratch]"
let s:buffer_number = -1

"----------------------------------------------------------------------
" Diplays the scratch buffer. Creates one if it is an already 
" present
"----------------------------------------------------------------------
function! <SID>ShowScratchBuffer()
    if(s:buffer_number == -1 || bufexists(s:buffer_number) == 0)
	" Temporarily modify isfname to avoid treating the name as a pattern.
	    let _isf = &isfname
	    set isfname-=\
	    set isfname-=[
	    exec "sp \\". s:SCRATCH_BUFFER_NAME
	    let &isfname = _isf
        let s:buffer_number = bufnr('%')
    else
	    let buffer_win=bufwinnr(s:buffer_number)
	    if(buffer_win == -1)
	        exec('sb '. s:buffer_number)
	    else
	        call <SID>GotoWindow(buffer_win)
	    endif
    endif
    " Do setup always, just in case.
    set buftype=nofile
    set bufhidden=hide
    set noswapfile
    set noro
    nmap <buffer> <silent> <Plug>ShowScratchBuffer :hide<cr>
    imap <buffer> <silent> <Plug>InsShowScratchBuffer <c-o>:hide<cr>
    command! -buffer -nargs=0 Scratch :hide
endfunction

"-----------------------------------------------------------------------
" Moves the cursor to the scratch buffer if it is already
" open. (stole this from WinManager.vim)
"-----------------------------------------------------------------------
function! <SID>GotoWindow(reqdWinNum)
	let startWinNum = bufwinnr("")
	while bufwinnr("") != a:reqdWinNum
		wincmd w
		if bufwinnr("") == startWinNum
			let v:errmsg = "Couldn't find window ".a:reqdWinNum
			return 0
		endif
	endwhile
	return 1
endfunction

" vim6: sw=4
