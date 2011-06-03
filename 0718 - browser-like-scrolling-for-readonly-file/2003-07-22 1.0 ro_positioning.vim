" $VIMRUNTIME/plugin/ro-positioning.vim
    if exists("g:ro_positioning_loaded") | finish | endif
    let g:ro_positioning_loaded=1

    function! s:RoPositioning()
        if &readonly
             let s:x=winheight(winnr())/2
             :exe ":" . s:x . "\n"
             set so=999
        endif
        if &so != 0 && &readonly 
            " noremap j :call s:SoRo_Down()<cr>
            noremap <Down> :call SoRo_Down()<cr>
            noremap <Up>   :call SoRo_Up()<cr>
        endif
    endfunction
    function! SoRo_Down()
        if &so != 0 && &readonly
            " XXX correct cmp involves &so
            if line('.') < winheight(winnr())/2
                " XXX local var or global ?
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
                " XXX local var or global ?
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

    au BufEnter * :call s:RoPositioning()
" $VIMRUNTIME/plugin/ro-positioning.vim

