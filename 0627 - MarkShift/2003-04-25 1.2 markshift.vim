" Function MarkShift
"
" Usefull for editing indented source files which have been
" edited with the expand tab setting. When activated the
" first space out of every N spaces is highlighted. N is
" worked out automaticlly from the shiftwidth setting of the
" buffer.
"
fu! MarkShift() 
    if !exists("s:MarkShiftEnabled") 
        let s:MarkShiftEnabled = 0 
    end
    if s:MarkShiftEnabled==0
        syn match leadspace /^\s\+/ contains=shiftregion
        exe "syn match shiftregion /\\s\\{" . &ts . "}/hs=s,he=s+1 contained"
        hi shiftregion guibg=Grey
        let s:MarkShiftEnabled=1
    else
        syn clear leadspace
        syn clear shiftregion
        let s:MarkShiftEnabled=0
    end
endfunc 
com! -nargs=0 MarkShift :call MarkShift()

