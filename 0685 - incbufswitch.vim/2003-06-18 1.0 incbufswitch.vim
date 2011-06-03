"============================================================================
"        File: incbufswitch.vim
" Description: Switch the buffer in the current window by incrementally typing
"              the name.
"      Author: Michael Denio (mike at imagine-sw.com)
"     Version: 1.0
"
"       Usage: (1) Define a normal key mapping to :IncBufSwitch or leave the
"                  default of <C-S>
"              (2) Start typing the name of the buffer you want to switch to.
"              (3) Press <BS> if make an error
"              (4) Press either <CR> or <ESC> when you are done
"
"     History:
"     
" 2003/06/18 mjd Created
"
"============================================================================


" Install command and default key mapping
if !exists(":IncBufSwitch")
  command IncBufSwitch :call <SID>IncBufferSwitch()
endif

if !hasmapto("<Plug>IncBufferSwitch")
    nmap <silent> <unique> <C-S> :IncBufSwitch<CR>
endif


"
" Switch to a buffer that matches the partial name we have thus far
" 
function! <SID>PartialBufSwitch(partialName)
    let lastBuffer = bufnr("$")
    let currentBuffer = 1
    while currentBuffer <= lastBuffer
        if (bufexists(currentBuffer) != 0 && buflisted(currentBuffer))
            let filename = expand("#" . currentBuffer . ":t")
            if (match(filename, "^" . a:partialName) > -1)
                execute "silent buffer " currentBuffer
                redraw
                break
            endif
        endif
        let currentBuffer = currentBuffer + 1
    endwhile
    " Inform user if no buffer matches pattern (beep)
    if (currentBuffer > lastBuffer)
        execute "silent normal \<Esc>"
        return 0
    endif
    return 1
endfunction


"
" Perform an incremental buffer switch
"
function! <SID>IncBufferSwitch()
    echon "IncBufSwitch: "
    let partialBufName = ""
    let bufFound = 0
    while 1 == 1
        let rawChar = getchar()
        let trySwitching = 0
        if rawChar == 13
            break
        endif
        if rawChar == 27
            echon "\r               "
            break
        endif
        if rawChar == "\<BS>"
            if strlen(partialBufName) > 0
                let partialBufName = strpart(partialBufName, 0, strlen(partialBufName) - 1)
                echon "\r               "
                " if we found a buffer before typing the backspace revert back
                " to it
                if bufFound == 1
                    let trySwitching = 1
                endif
            endif
        else
            let nextChar = nr2char(rawChar)
            let partialBufName = partialBufName . nextChar
            let trySwitching = 1
        endif
        echon "\rIncBufSwitch: " . partialBufName
        if trySwitching == 1
            let bufFound = s:PartialBufSwitch(partialBufName)
        endif
    endwhile
endfunction
