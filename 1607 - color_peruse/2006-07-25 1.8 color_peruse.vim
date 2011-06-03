"color_peruse.vim
"Peruse Color Schemes
"
"Walter Hutchins
"Last Change July 25 2006
"Version 1.8
"
"Setup: copy to ~/.vim/plugin
"
"Put this mapping in .vimrc:
"    map <Leader>cp :ColorPeruse<Space>Mylist<CR>:echo g:colors_name<CR>
"Then \cp will start it up.
"If you once create this 'Mylist' file, it is suggested that you move it
"into a directory in vim's runtimepath. Type :set rtp to see this path.
"This 'Mylist' file does not have to exist.
"In such a list, you may have comments beginning with " which will be ignored,
"and you can sort the list in any order you choose.
"
"You can substitute a different filename path for the 'Mylist' above, 
"to load a list of color names. If you remove the 'Mylist' between the 
"<Space> and <CR> above, the default buffer name 'ColorPeruse' will be used.
"
"If filename path contains slashes and does not start with a dollar sign $,
"it will be taken literally.
"If the filename path starts with a dollar sign, it should be one of the
" vim paths: $HOME, $VIM, or $VIMRUNTIME.
"A filename path beginning with ~ is regarded as $HOME.
"A filename path without any slashes or ~, or $, would potentially read from
"and/or write to the current directory.
"
"A resulting filename path that is not readable, or an empty filename path 
"would potentially read from and/or write to a file named 'ColorPeruse'
"in the current directory.
"
"Please make any slashes in the above filename path forward slashes /,
"but if you use back slashes, use two of them.

"You are in vim when in the ColorPeruse window, so you can edit items you 
"don't like from the list, or place markers to return to re-view particular
"colorschemes.
"You can even cut/paste to put favorites together to compare more closely.
"
"If you want to sort the list, see :help eval-examples.

"
"Type ? for help
"
command -nargs=* ColorPeruse call <SID>ColorPeruse(<f-args>)
let s:xcsl=""

function <SID>ColorPeruseHelp()
    let helpmsg=""
    let helpmsg=helpmsg . "When the cursor is in the ColorPeruse window: \n"   
    let helpmsg=helpmsg . "   <kPlus>  :ColorPeruse 1           (next color) \n"
    let helpmsg=helpmsg . "  <kMinus>  :ColorPeruse -1          (prev color) \n"
    let helpmsg=helpmsg . "<kMultiply> :ColorPeruse             (original color) \n"
    let helpmsg=helpmsg . " <kDivide>  :ColorPeruse 0           (color under cursor) \n"
    let helpmsg=helpmsg . "     =      :call ColorPeruseDel()   (remove duplicate lines) \n"
    let helpmsg=helpmsg . "     +      :call ColorPeruseMerge() (append all :set rtp colors to list) \n"
    let helpmsg=helpmsg . "     ?      Help \n"
    let helpmsg=helpmsg . "     :q     quit \n"
    echo helpmsg
endfunction

function <SID>ColorPeruseList(...) 
    let s:xcsl=""
    if a:0 > 0
        let s:xcsl=a:1
        "let s:xcsl=substitute(s:xcsl, '\\', '/', '')
        let chkreadable="let isitlist=filereadable('".s:xcsl."')"
        exec chkreadable
        if !isitlist
            let me=match(s:xcsl, '/')
            if me == -1
                let gxcsl=globpath(&rtp, s:xcsl)
                let me=match(gxcsl, '\S\+'.nr2char(10))
                let mee=matchend(gxcsl, '\S\+'.nr2char(10))
                if me != -1
                    let gxcsl=strpart(gxcsl, me, mee - 1)
                endif
            endif
            let s:xcsl=substitute(s:xcsl, '\~', '$HOME', '')
            let dollarmatch=match(s:xcsl, '\$.\+/')
            let dollarmatche=matchend(s:xcsl, '\$.\+/')
            if dollarmatch != -1
                let dm1=strpart(s:xcsl, dollarmatch, dollarmatche - 1)
                exec 'let dm1='.dm1
                let dm2=strpart(s:xcsl, dollarmatche)
                let s:xcsl=dm1.'/'.dm2
            elseif gxcsl != ""
                let s:xcsl=gxcsl
            endif
            let chkreadable="let isitlist=filereadable('".s:xcsl."')"
            exec chkreadable
        endif
        if !isitlist
            let s:xcsl=""
        endif
    endif
    let x=globpath(&rtp, "colors/*.vim")
    let y=substitute(x."\n","\\(^\\|\n\\)[^\n]*[/\\\\]", "\n", "g")
    let z=substitute(y,"\\.vim\n", "\n", "g")
    let s:csl=z
    let s:ocolor=g:colors_name
    vnew
    vertical resize 18
    if s:xcsl != ""
        exec 'edit '.s:xcsl
    else
        let s:xcsl='ColorPeruse'
        exec 'edit '.s:xcsl
        1,$d
        put =s:csl
        g/^$/d
        set nomodified
    endif
" the following trick avoids the "Press RETURN ..." prompt
0 append
.
    if exists("g:colors_name")
        let gcline=search("^".g:colors_name."$", "w")
    endif
    map <buffer> <silent> <kMinus> :ColorPeruse -1<CR>:echo g:colors_name_message<CR>
    map <buffer> <silent> <kPlus> :ColorPeruse 1<CR>:echo g:colors_name_message<CR>
    map <buffer> <silent> <kMultiply> :ColorPeruse<CR>:echo g:colors_name_message<CR>
    map <buffer> <silent> <kDivide> :ColorPeruse 0<CR>:echo g:colors_name_message<CR>
    map <buffer> <silent> = :call <SID>ColorPeruseDel()<CR>
    map <buffer> <silent> + :call <SID>ColorPeruseMerge()<CR>
    map <buffer> ? :call <SID>ColorPeruseHelp()<CR>
    " when this buffer is exited it will be wiped out
    if v:version >= 602
         setlocal bh=delete
    endif
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
        let ll=escape(ll, '"/')
        y a
        exec 'silent %s/^\('.ll.'\)$//'
        let putback=loop
        exec putback . 'put a'
        let loop=loop + 1
        silent g/^$/d
        exec 'let dollar=line("$")'
    endwhile
    if lnc > dollar
        let lnc=dollar
    endif
    exec lnc
endfunction

function <SID>ColorPeruseMerge()
    $
    put =s:csl
endfunction

function <SID>ColorPeruse(...)
    let g:colors_name_message=""
    if s:xcsl == ""
        let s:xcsl='ColorPeruse'
    endif
    if bufloaded(s:xcsl)
        exec 'drop '.s:xcsl
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
        let curline=getline(".")
        if match(curline, '^\s*"') != -1 || match(curline, '^\s*$') == 0
            let skipline=1
        else
            let skipline=0
        endif
        if !skipline
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
            let cmd ='color ' . scheme
            exec cmd
            if exists("g:colors_name") 
                 if g:colors_name == scheme
                     let g:colors_name_message=g:colors_name
                 else
                     let g:colors_name_message=scheme." !wrong g:colors_name! ".g:colors_name
                 endif
            else
                 let g:colors_name_message="missing g:colors_name"
           endif
        endif
    else
        let nxtarg=0
        let remargs=a:0
        let argstr=""
        while remargs > 0
            let nxtarg=nxtarg + 1
                let argstr=<SID>ColorPeruseAddarg(argstr, a:{nxtarg})
            let remargs=remargs - 1
        endwhile
        exec 'call <SID>ColorPeruseList('.argstr.')'
    endif
endfunction
 
function <SID>ColorPeruseAddarg(argstr, arg)
    let dquot='"'
    let comma=""
    if a:argstr != ""
        let comma=","
    endif
    let argstr=a:argstr . comma . dquot . a:arg . dquot
    return argstr
endfunction

