" $VIMRUNTIME/plugin/ro-positioning.vim
    if exists("g:ro_positioning_loaded") | finish | endif
    let g:ro_positioning_loaded=1

    function! s:RoBufEnter()
        if &readonly
             if line(".") == 1
                 let s:x=winheight(winnr())/2
                 :exe ":" . s:x . "\n"
             endif
             set so=999
        endif
        if &so != 0 && &readonly 
            " noremap j :call s:SoRo_Down()<cr>
            noremap <silent> <Down> :call SoRo_Down()<cr>
            noremap <silent> <Up>   :call SoRo_Up()<cr>
        endif
    endfunction
    function! SoRo_Down()
        if &so != 0 && &readonly
            if line('.') < winheight(winnr())/2
                let skip=winheight(winnr())/2 - line('.')
                :exe "normal " skip . "j"
            else
                normal j
            endif
        else
            normal j
        endif
        echo ""
    endfunction
    function! SoRo_Up()
        if &so != 0 && &readonly
            if line('.') > line('$') - winheight(winnr())/2
                let skip=line('.') - (line('$') - winheight(winnr())/2)
                :exe "normal " skip ."k"
            else
                normal k
            endif
        else
            normal k
        endif
        echo ""
    endfunction

    au BufEnter * :call s:RoBufEnter()
" $VIMRUNTIME/plugin/ro-positioning.vim

