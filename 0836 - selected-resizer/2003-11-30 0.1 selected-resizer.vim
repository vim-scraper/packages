"Title: selected-resizer.vim 
"Version: 0.1
"Author: Ryan (gt3) Kulla
"Date: Nov 30 2003
"Descripion: resizes a window to the selected block of text 
"Usage: highlight text and hit Ctrl-r
"Tested: gvim 6.2


vmap <C-r> :call SelectedResizer("v")<CR>


function! SelectedResizer(mode)
    if a:mode == "v" 
        norm! gvy 
        let s:i = 0
        let s:newline_count = 0
        let s:selected_text_len = strlen(@")
        while s:i < s:selected_text_len
            if @"[s:i] == "\n"
                let s:newline_count = s:newline_count + 1
            endif
            let s:i = s:i + 1	
        endwhile
        if s:newline_count == 0 && s:selected_text_len < winwidth(0)
            "selected text line
            execute "normal z1\<CR>" 
        elseif s:newline_count == 0 && s:selected_text_len > winwidth(0)
            "selected spans more than 1 line, but doesn't contain \n
            execute "normal z" . (s:selected_text_len / winwidth(0) + 1) . "\<CR>" 
        else
            "change based off how many lines are highted
            execute "normal z" . (s:newline_count + 1) . "\<CR>"
        endif	
    endif
endfunction
