" loadtags 0.1
" use correct tags files
" Miroslav Tynovsky

function! FindCorrectTags()
    let fullpath = expand("%:p")
    let pathparts = split(fullpath, '/')
    while len(pathparts) > 0
        call remove(pathparts, -1)
        let tagsfile = "/" . join(pathparts + ["tags"], '/')
        if filereadable(tagsfile)
            execute 'setlocal tags+=' . tagsfile
        endif
    endwhile
endfunction

autocmd BufNewFile,BufRead * call FindCorrectTags()
