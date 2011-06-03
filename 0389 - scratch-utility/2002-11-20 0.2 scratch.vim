"----------------------------------------------------------------------
" This is a simple plugin that creates a scratch buffer for your
" vim session and helps to access it when you need it.  
"
" If you like the custom mappings provided in the script - hitting
" <F5> should create a new scratch buffer. You can do your scribes
" here and if you want to get rid of it, hit <F6>. If you want to 
" get back to the scratch buffer hit <F5> again.
"
"
" Custom mappings
" ---------------
" The ones defined below are not very ergonomic!
"----------------------------------------------------------------------
"Standard Inteface:  <F8> to make a new ScratchBuffer, shift-<F8> to hide one
if !hasmapto('<Plug>ShowScratchBuffer',"n")
  nmap <unique> <F8> <Plug>ShowScratchBuffer
endif
if !hasmapto('<Plug>InsShowScratchBuffer',"i")
  imap <unique> <F8> <Plug>InsShowScratchBuffer
endif
if !hasmapto('<Plug>HideScratchBuffer',"n")
  nmap <unique> <S-F8> <Plug>HideScratchBuffer
endif
if !hasmapto('<Plug>InsHideScratchBuffer',"i")
  imap <unique> <S-F8> <Plug>InsHideScratchBuffer
endif

" User Overrideable Plugin Interface
nmap <silent> <Plug>ShowScratchBuffer    :silent call      <SID>ShowScratchBuffer()<cr>
nmap <silent> <Plug>HideScratchBuffer    :silent call      <SID>HideScratchBuffer()<cr>
imap <silent> <Plug>InsShowScratchBuffer <c-o>:silent call <SID>ShowScratchBuffer()<cr>
imap <silent> <Plug>InsHideScratchBuffer <c-o>:silent call <SID>HideScratchBuffer()<cr>

"----------------------------------------------------------------------
" Diplays the scratch buffer. Creates one if it is an already 
" present
"----------------------------------------------------------------------
let s:SCRATCH_BUFFER_NAME="_scratch_"

function! <SID>ShowScratchBuffer()
    let buffer_number = bufnr(s:SCRATCH_BUFFER_NAME)
    if(buffer_number == -1)
        echo "buffer does not exist"
        exec('badd '. s:SCRATCH_BUFFER_NAME)
        let buffer_number = bufnr(s:SCRATCH_BUFFER_NAME)
    endif    
    let buffer_win=bufwinnr(s:SCRATCH_BUFFER_NAME)
    if(buffer_win == -1)
        exec('sb '.buffer_number)
        "sbuffer [buffer_number]
        set buftype=nofile
        set bufhidden=hide
        set noswapfile
    else
        call <SID>GotoWindow(buffer_win)
        echohl WarningMsg
        echo "Buffer already open!"
        echohl None
    endif
endfunction

"----------------------------------------------------------------------
" Hides the scratch buffer
"----------------------------------------------------------------------
function! <SID>HideScratchBuffer()
    if(bufname("%") == s:SCRATCH_BUFFER_NAME)
        hide 
    else
        echohl WarningMsg
        echo "This is not a scratch buffer!"
        echohl None
    endif
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
