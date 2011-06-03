"color_peruse.vim
"Peruse Color Schemes
"
"Walter Hutchins
"Last Change July 13 2006
"Version 1.0
"
"Setup: copy to ~/.vim/plugin
"
"maybe do these mappings
"map <Leader>co :ColorPeruse<Space>
"map <Leader>cp :ColorPeruse<Space>0<CR>
"map <Leader>c= :ColorPeruse<Space>1<CR>
"map <Leader>c- :ColorPeruse<Space>-1<CR>
"
" for:
"      cp - ColorPeruse startup
"           :ColorPeruse<Space>0<CR>
"      co - type in number, hit enter: jump number distance to color & show
"           :ColorPeruse<Space>
"      c= - goto next color & show (= is same key as +)
"           :ColorPeruse<Space>1<CR>
"      c- - goto previous color & show
"           :ColorPeruse<Space>-1<CR>

command -nargs=* ColorPeruse call s:ColorPeruse(<f-args>)

function s:ColorPeruseList() 
    "lifted from ScrollColors.vim
    let x=globpath(&rtp, "colors/*.vim")
    let y=substitute(x."\n","\\(^\\|\n\\)[^\n]*[/\\\\]", "\n", "g")
    let z=substitute(y,"\\.vim\n", "\n", "g")
    "let sorted = s:SortVar(z, "\n")
    return z "s:RemoveDuplicates(sorted)
endfun

function s:ColorPeruse(...)
    let csl=s:ColorPeruseList()
    if bufloaded('ColorPeruse')
        exec 'drop  ColorPeruse'
        if a:0 == 1
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
            let move=0
        endif
        exec '.' . move
        yank a
        let cmd='let scheme=substitute("' . @a . '","\n","","")'
        exec cmd
        exec 'color ' . scheme
    else
        vnew
        exec 'edit ColorPeruse'
        vertical resize 25
        1,$d
        put =csl
        g/^$/d
" the following trick avoids the "Press RETURN ..." prompt
0 append
.
        set nomodified
    endif
endfunction
