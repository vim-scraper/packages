" Simple SVN utility for vim.  0.001001 
" tag@cpan.org

" This is basically a very simple wrapper to calling svn commands.
" I mostly use it so I can :Commit instead of :w :!svn commit :!svn update
" I use 'ed' as my editor when doing svn commit, since I do it from gvim and
" I dont know if you've ever tried gvim -c ':!vim' but it aint pretty :)
" Yes I know I could use vi but its actually harder to use than ed, in that
" enviroment.  %p is so much easier than the hell elvis puts me through when it
" has no terminal.

if exists("$SVN_EDITOR")
    let g:SvnEditor = $SVN_EDITOR
else     
    let g:SvnEditor = "ed"
endif

com! -nargs=* SVN call SvnMain(<f-args>)
com! -nargs=* Svn call SvnMain(<f-args>)
com! -nargs=* Commit call SvnCommit()
com! -nargs=* Update call SvnUpdate()    

fu! SvnCommit()
    w
    exe "! EDITOR=" . g:SvnEditor . " svn commit"
    ! svn update
endfunction

fu! SvnUpdate()
    ! svn update
endfunction    

fu! SvnMain(...)
    if a:0 != 0
        let svn_c = a:1
        echomsg "You entered " . svn_c 
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
