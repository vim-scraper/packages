"color_peruse.vim
"Peruse Color Schemes
"
"Walter Hutchins
"Last Change July 14 2006
"Version 1.1
"
"Setup: copy to ~/.vim/plugin
"
"maybe do these mappings
"map <Leader>co :ColorPeruse<Space><CR>       - startup / reset to original
"map <Leader>cp :ColorPeruse<Space>0<CR>      - change to color under cursor
"map <Leader>c= :ColorPeruse<Space>1<CR>      - change to next color
"map <Leader>c- :ColorPeruse<Space>-1<CR>     - change to previous color
"
"You can edit items you don't like from the perusal list (temporary file)
"so you can narrow down the the list to your preferred items.
"Or, you could cut/paste in a previously prepared perusal list.

command -nargs=* ColorPeruse call s:ColorPeruse(<f-args>)

function s:ColorPeruseList() 
    let x=globpath(&rtp, "colors/*.vim")
    let y=substitute(x."\n","\\(^\\|\n\\)[^\n]*[/\\\\]", "\n", "g")
    let z=substitute(y,"\\.vim\n", "\n", "g")
    let s:csl=z
    let s:ocolor=g:colors_name
    return z
endfun

function s:ColorPeruse(...)
    if bufloaded('ColorPeruse')
        exec 'drop  ColorPeruse'
        if a:0 == 1
            let apply=1
            let move=a:1 + 0
            let cl=getline(".")
            let tl=getline(1)
            let la=getline("$")
            let lm=""
            if move > 0
                let lm=getline(".+" . move)
                if lm == la
                    let move=0
                    1
                endif
            elseif move < 0
                let lm=getline(".-" . move)
                if lm == tl
                    let move=0
                    $
                endif
            endif
        else
            let apply=0
            let move=0
        endif
        exec '.' . move
        yank a
        if !apply
            let @a=s:ocolor
        endif
        hi clear
        if exists("syntax_on")
            syntax reset
        endif
        let cmd='let scheme=substitute("' . @a . '","\n","","")'
        exec cmd
        exec 'color ' . scheme
    else
        call s:ColorPeruseList()
        vnew
        exec 'edit ColorPeruse'
        vertical resize 18
        1,$d
        put =s:csl
        g/^$/d
" the following trick avoids the "Press RETURN ..." prompt
0 append
.
        set nomodified
    endif
endfunction
