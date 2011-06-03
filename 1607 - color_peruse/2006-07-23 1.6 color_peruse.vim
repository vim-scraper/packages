"color_peruse.vim
"Peruse Color Schemes
"
"Walter Hutchins
"Last Change July 24 2006
"Version 1.6
"
"Setup: copy to ~/.vim/plugin
"
"Put this mapping in .vimrc:
"    map <Leader>cp :ColorPeruse<Space>0<CR>:echo g:colors_name<CR>
"Then \cp will start it up.
"
"Type ? for help

"You are in vim when in the ColorPeruse window, so you can edit items you 
"don't like from the list, or place markers to return to re-view particular
"colorschemes.
"You can even cut/paste to put favorites together to compare more closely.

command -nargs=* ColorPeruse call <SID>ColorPeruse(<f-args>)

function <SID>ColorPeruseHelp()
    let helpmsg=""
    let helpmsg=helpmsg . "When the cursor is in the ColorPeruse window: \n"   
    let helpmsg=helpmsg . "   <kPlus>  :ColorPeruse 1         (next color) \n"
    let helpmsg=helpmsg . "  <kMinus>  :ColorPeruse -1        (prev color) \n"
    let helpmsg=helpmsg . "<kMultiply> :ColorPeruse           (original color) \n"
    let helpmsg=helpmsg . " <kDivide>  :ColorPeruse 0         (color under cursor) \n"
    let helpmsg=helpmsg . "     =      :call ColorPeruseDel() (remove duplicate lines) \n"
    let helpmsg=helpmsg . "     ?      Help \n"
    let helpmsg=helpmsg . "     :q     quit \n"
    echo helpmsg
endfunction

function <SID>ColorPeruseList() 
    let x=globpath(&rtp, "colors/*.vim")
    let y=substitute(x."\n","\\(^\\|\n\\)[^\n]*[/\\\\]", "\n", "g")
    let z=substitute(y,"\\.vim\n", "\n", "g")
    let s:csl=z
    let s:ocolor=g:colors_name
    vnew
    exec 'edit ColorPeruse'
    vertical resize 18
    1,$d
    put =s:csl
    g/^$/d
" the following trick avoids the "Press RETURN ..." prompt
0 append
.
    if exists("g:colors_name")
        let gcline=search(g:colors_name, "w")
    endif
    set nomodified
    map <buffer> <silent> <kMinus> :ColorPeruse -1<CR>:echo exists("g:colors_name") ? g:colors_name : "missing g:colors_name"<CR>
    map <buffer> <silent> <kPlus> :ColorPeruse 1<CR>:echo exists("g:colors_name") ? g:colors_name : "missing g:colors_name"<CR>
    map <buffer> <silent> <kMultiply> :ColorPeruse<CR>:echo exists("g:colors_name") ? g:colors_name : "missing g:colors_name"<CR>
    map <buffer> <silent> <kDivide> :ColorPeruse 0<CR>:echo exists("g:colors_name") ? g:colors_name : "missing g:colors_name"<CR>
    map <buffer> <silent> = :call <SID>ColorPeruseDel()<CR>
    map <buffer> ? :call <SID>ColorPeruseHelp()<CR>
    return z
endfun

" function to delete duplicate lines
function <SID>ColorPeruseDel()
    let lnc=line(".")
    exec 'let one=1'
    exec 'let dollar=line("$")'
    let loop=1
    while loop <= dollar
        exec loop
        exec 'let ll=getline(".")'
        y a
        exec '%s/^\('.ll.'\)$//'
        let putback=loop
        exec putback . 'put a'
        let loop=loop + 1
        g/^$/d
        exec 'let dollar=line("$")'
    endwhile
    if lnc > dollar
        let lnc=dollar
    endif
    exec lnc
    set nomodified
endfunction

function <SID>ColorPeruse(...)
    if bufloaded('ColorPeruse')
        exec 'drop  ColorPeruse'
        if a:0 == 1
            let apply=1
            let move=a:1 + 0
            let lnb=1
            let lnc=line(".")
            let lne=line("$")
            let lm=""
            if move > 0
                if lnc == lne
                    let move=0
                    1
                endif
            elseif move < 0
                if lnc == lnb
                    let move=0
                    $
                endif
            endif
        else
            let apply=0
            let move=0
        endif
        exec "." . move
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
        call <SID>ColorPeruseList()
    endif
endfunction
