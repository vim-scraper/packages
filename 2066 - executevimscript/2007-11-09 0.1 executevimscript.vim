" executevimscript.vim - Execute selected vimscript lines (and hopefully later
" selected vimscript text)
"
" Author: Sébastien Rocca-Serra
" Version: 0.1
"

" Create a scratch buffer that can be used as a vimscript sandbox
function! <SID>CreateVimscriptScratchBuffer()
    new
    set buftype=nofile
    set bufhidden=hide
    setlocal noswapfile
    set syntax=vim
endfunction
command! CreateVimscriptScratchBuffer call <SID>CreateVimscriptScratchBuffer()

" Can be used to select and execute vimscript lines. Default is current line.
function! <SID>ExecuteVimscriptLines() range
    execute a:firstline . "," . a:lastline . 'yank'
    exec '@"'
endfunction
command! -range ExecuteVimscriptLines <line1>,<line2>call <SID>ExecuteVimscriptLines()
