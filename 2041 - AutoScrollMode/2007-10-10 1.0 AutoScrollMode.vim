" AutoSrollMode.vim - Let Vim scroll text for You
" Maintainer:	onemda at gmail dot com
" Last Change:	12.1.2006
" Version:	1.0
" License:	Vim License
"
" Usage:
"
" This commands are used to change how Vim will scroll text
"
"        0-9        : change type of movement to:
"
"        "1"        : character, default
"        "2"        : word
"        "3"        : WORD
"        "4"        : line
"        "5"        : half page
"        "6"        : full page
"        "7"        : full screen
"        "8"        : sentence
"        "9"        : paragraph        see 'paragraphs'
"        "0"        : section        see 'sections'
"
"
"
" This commands are used to change how much time will Vim
" sleep until the cursor is moved, default: Vim will sleep forever
"
"
"        "h"        : decrease sleep period for 1 millisecond
"
"        "l"        : increase sleep period for 1 millisecond
"
"
"        "k"        : divides sleep period for 2
"
"        "j"        : multiplies sleep period for 2
"
"
"        "`"        : set sleep period to zero, default
"
"
"
"
" other useful commands:
"
"        "r"        : clean and redraw screen
"
"        <Space>    : toggle pause, text stop scrolling
"        <Enter>    : toggle backward/forward movement
"
"        <Esc>      : exit, enters Normal Vim mode
"        CTRL-C     : immediately exit in Normal Vim mode
"
"
" Note:
"
" paragraph and section movements should be useful only in nroff
" documents, and maybe not useful at all :)
"
" pressing "h" and after that "l" and after that "4": nothing happens,
" same with "j" and "k", but pressing "l" and after that "4":
" text will scroll up, in the same way as typing "j" in Normal mode.
"
"


function! AutoScrollMode()

    let period = 0
    let move = 1
    let key = 0

    while 1
        if key != 0
            if key == 32 && period!=0 
                while 1
                    let pause = getchar()
                    if pause == 27
                        return
                    elseif pause == 32
                        break
                    elseif pause == 13
                        let period = period * (-1)
                        break
                    elseif pause == 114
                        silent! execute "redraw!"
                        silent! execute "sleep 100m"
                    else
                        silent! execute    "sleep 10m"
                    endif
                endwhile
            elseif key == 48
                let move = 0
            elseif key == 49
                let move = 1
            elseif key == 50
                let move = 2
            elseif key == 51
                let move = 3
            elseif key == 52
                let move = 4
            elseif key == 53
                let move = 5
            elseif key == 54
                let move = 6
            elseif key == 55
                let move = 7
            elseif key == 56
                let move = 8
            elseif key == 57
                let move = 9
            elseif key == 96
                let period = 0
            elseif key == 107
                let period = period/2
            elseif key == 104
                let period = period-1
            elseif key == 106
                let period = period*2
            elseif key == 108
                let period = period+1
            elseif key == 114
                silent! execute "redraw!"
            elseif key == 13
                let period = period * (-1)
            elseif key == 27
                return
            endif
        else
            if period > 0 && line(".") < line ("$")
                if move == 1
                    silent! execute "normal \<Right>"
                elseif move == 2
                    normal w
                elseif move == 3
                    normal W
                elseif move == 4
                    silent! execute "normal \<Down>"
                elseif move == 5
                    silent! execute "normal \<c-d>"
                elseif move == 6
                    silent! execute "normal \<c-f>"
                elseif move == 7
                    silent! execute "normal z+"
                elseif move == 8
                    silent! execute "normal )"
                elseif move == 9
                    silent! execute "normal }"
                elseif move == 0
                    silent! execute "normal ]]"
                endif
            elseif period < 0 && line(".") > 1
                if move == 1
                    silent! execute "normal \<Left>"
                elseif move == 2
                    normal b
                elseif move == 3
                    normal B
                elseif move == 4
                    silent! execute "normal \<Up>"
                elseif move == 5
                    silent! execute "normal \<c-u>"
                elseif move ==6
                    silent! execute "normal \<c-b>"
                elseif move ==7
                    silent! execute "normal z^"
                elseif move == 8
                    silent! execute "normal ("
                elseif move == 9
                    silent! execute "normal {"
                elseif move == 0
                    silent! execute "normal [["
                endif
            endif
"            echo getbufline(bufname("%"), line("w$") + 1)
            redraw
        endif

        if period > 0
            let time = period
        elseif period < 0
            let time = period * (-1)
        else
            let time = 100
        endif

        if time > 1000
            let tt = time / 100
            let t1 = 0
            let t2 = time % 100
            let t3 = time % 10
            silent! execute "sleep ".t3."m"
            while t1 < tt
                silent! execute "sleep 100m"

                let t1 = t1 + 1
                let key = getchar(0)
                if key != 0
                    break
                endif
            endwhile
            silent! execute "sleep ".t2."m"
        else
            silent! execute "sleep ".time."m"
            let key = getchar(0)
        endif
    endwhile
endfunction

nmap <silent><F21> :call AutoScrollMode()<CR>
