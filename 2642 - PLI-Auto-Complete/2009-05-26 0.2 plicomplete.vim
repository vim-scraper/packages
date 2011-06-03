" Vim completion script
" Language: PL/I
" Maintainer: Ricky Wu <richiewu at live dot com>
" Last Change: 2009-05-26
" Version: 0.2
 
" This function is used for the 'completefunc' option.
function! plicomplete#Complete(findstart, base)
    if a:findstart
        " Locate the start of the item, including "."
        let line  = getline('.')
        let start = col('.') - 1
        let end   = -1
        while start > 0
            if line[start - 1] =~ '\w'
                let start -= 1
            elseif line[start - 1] =~ '\.'
                if end == -1
                    let end = start
                endif
                let start -= 1
            else
                break
            endif
        endwhile
 
        " Return the column of the last word, which is going to be changed.
        " Remember the text that comes before it in s:prepended.
        if end == -1
            let s:prepended = ''
            return start
        endif
        let s:prepended = strpart(line, start, end - start - 1)
        return end 
    endif
 
    " Return list of matches.
    let base = s:prepended
 
    " Don't do anything for an empty base, would result in all the tags in the
    " tags file.
    if base == ''
        return []
    endif
    if searchdecl(base) != 0
        return []
    endif
 
    let baseline = line('.')
    let lnum     = baseline
    let endflag  = 1
    let s:items  = []
    
    while endflag == 1
        let lnum  = lnum + 1
        let line  = getline(lnum)

        "Skip if current line is 
        if line =~ '\s\+\/\*\s\+\d\+\s\+\w\+\s\+.*(.*).*\*\/'
            continue
        endif
        
        "Copybooks include
        if line =~ '\s\+%INCLUDE\s\+.*(.*)\s*;'
            "Swith of CpyComplete
            if g:CpyCompleteInd == 0 
                continue
            endif
            if exists("g:CpyDir") == 0 
                continue
            endif
            let CpyName = s:CpyTrim(line)

            "Asign file path for different include method
            if line =~ g:CpyIdentification
                let IncludeDir = g:CpyDir
            elseif line =~ g:MapIdentification
                let IncludeDir = g:MapDir
            endif
            let filepath = IncludeDir.CpyName
            call s:CpyComplete(filepath)

            if getline(lnum + 1) !~ '\s\+\d\+\s\+.*(.*).*,'
                let endflag = -1
                break
            endif
        "Normal definition line
        elseif line =~ '\s\+\d\+\s\+\w\+\s\+.*(.*)'
            let item = s:ItemTrim(line) 
            call add(s:items, item)
        endif

        "Last line in definition
        if line =~ '\s\+\d\+\s\+.*;'
            let endflag = -1
        endif
 
    endwhile
    return s:items
 
endfunc

"Get variable item in definition
function! s:ItemTrim(line)
        
    let line     = a:line
    let column   = 1
    let findhead = -1
        
    while column < 72 
        let column = column + 1
        if line[column] =~ '\s'
            if findhead == 1
                let end = column 
                break
            endif
        elseif line[column] =~ '\h'
            if findhead == -1
                let start = column 
            endif
            let findhead = 1
        endif
    endwhile
        
    let item = strpart(line, start, end - start)
    return item
        
endfunc

"Get copybooks name
function! s:CpyTrim(line)

    let line   = a:line
    let column = 1

    while column < 72
        let column = column + 1
        if line[column] =~ '('
            let start = column + 1
        elseif line[column] =~ ')'
            let end = column - 1
            break
        endif
    endwhile

    let CpyName = strpart(line, start, end - start + 1)
    return CpyName

endfunc

"Get variables in copybooks
function! s:CpyComplete(filepath)
    let filepath = a:filepath
    if filereadable(filepath) == 0
        return
    endif

    for line in readfile(filepath, '')
        if line =~ '\s\+\/\*\s\+\d\+\s\+\w\+\s\+.*(.*).*\*\/'
            continue
        endif

        if line =~ '\s\+\d\+\s\+\w\+\s\+.*(.*)'
            let item = s:ItemTrim(line) 
            call add(s:items, item)
        endif
    endfor

endfunc

" vim:ft=vim:fdm=marker
