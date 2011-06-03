" Simple SVN utility for vim.  0.002
" tag@cpan.org

" This is basically a very simple wrapper to calling svn commands.
" I mostly use it so I can :Commit instead of :w :!svn commit :!svn update
" I use 'ed' as my editor when doing svn commit, since I do it from gvim and
" I dont know if you've ever tried gvim -c ':!vim' but it aint pretty :)
" Yes I know I could use vi but its actually harder to use than ed, in that
" enviroment.  %p is so much easier than the hell elvis puts me through when it
" has no terminal.

let g:MidCommit = 0

if exists("$SVN_EDITOR")
    let g:SvnEditor = $SVN_EDITOR
else     
    let g:SvnEditor = "ed"
endif

com! -nargs=* SVN       call SvnMain(<f-args>)
com! -nargs=* Svn       call SvnMain(<f-args>)
com! -nargs=* Commit    call SvnCommit()
com! -nargs=* Update    call SvnUpdate()    
com! -nargs=* Add       call SvnAdd()
com! -nargs=* Log       call SvnLog()    
com! -nargs=* Complete  call SvnComplete()    

fu! SvnLog()
    windo new
    r!svn log
    0
endfunction

fu! SvnAdd(...)
    w
    if a:0 == 0
        exe "! svn add %"
    else
        let filename = a:1
        exe "! svn add " . filename
    endif
endfunction    

fu! SvnCommit()
    windo new

    silent! r!svn status | grep -vE '^\?'

    call append(0,"")
    call append(1,"--This line, and those below, will be ignored--")
    0

    echo "use :Complete to finish"
    let g:MidCommit = 1
endfunction

fu! SvnComplete()
    call append(0,"#!/bin/sh")
    call append(1, "cat <<SVN > $1")
    call cursor(5000000, 0)
    call append(line("."), "SVN")
    w svnedit.sh
    silent! ! chmod 755 svnedit.sh;EDITOR=$PWD/svnedit.sh svn commit;rm svnedit.sh;svn update
    close!

    let g:MidCommit = 0
endfunction
    

fu! SvnUpdate()
    ! svn update
endfunction    

fu! SvnMain(...)
    if a:0 != 0
        let svn_c = a:1

        if svn_c == "commit"
            call SvnCommit()
        
        elseif svn_c == "checkout"
            if a:0 > 1
                let svn_rs = a:2
                exe "!svn checkout " . svn_rs
            else
                echomsg "Usage: SVN checkout <repository>"
            endif    
            
        elseif svn_c == "update"
            call SvnUpdate()
     
        elseif svn_c == "log"
            call SvnLog()

        elseif svn_c == "add"
            if a:0 > 1
                call SvnAdd(a:2)
            else
                call SvnAdd()
            endif

        else
            let i = a:0
            let svn_c = ""

            while i
                exe "let svn_c = a:" . i . " . \" \" . svn_c"
                let i = i - 1
            endwhile

            exe "!svn " . svn_c
        endif    
    else
        echomsg "Usage: SVN <command> same as bin/svn"
    endif
endfunction    
