"   Author: Adam Bellaire
"  Version: 0.1
"     Date: 2/17/2009

function! SearchCols(pat)
    let g:ScCurCol = 0
    let g:ScStartPos = col('.')
    let g:ScEndPos = 0
    "execute 'match Search /'. a:pat .'/'
    call ScGetColWidths()
    while col('.') > g:ScEndPos
        call ScSetNextCol()
        if g:ScEndPos == ''
            break
        endif
    endwhile
    let g:ScSearchResult = ScSearchPat(a:pat,g:ScStartPos,g:ScEndPos)
    while !g:ScSearchResult && g:ScEndPos != ''
        call ScSetNextCol()
        call cursor(1,1)
        let g:ScSearchResult = ScSearchPat(a:pat,g:ScStartPos,g:ScEndPos)
        if g:ScEndPos == ''
            break
        endif
    endwhile
    if !g:ScSearchResult
        call cursor(g:ScOrigLine,g:ScOrigCol)
        echo 'Search string "'. a:pat .'" not found in remaining columns.'
    endif
endfunction

function! ScSearchPat(pat,startPos,endPos)
    return search(a:pat . (a:startPos > 1 ? ('\%>' . (a:startPos - 1) . 'c') : '') . (a:endPos > 1 ? ('\%<' .( a:endPos + 1) . 'c') : ''), 'We')
endfunction

function! ScSetNextCol()
        let g:ScStartPos = g:ScEndPos + 1
        let g:ScCurCol = g:ScCurCol + 1
        let g:ScEndPos =  MvElementAt(g:ScColWidths,':',g:ScCurCol)
endfunction

function! ScGetColWidthCur()
   let g:ScColWidthFound = search('\S\+\s\+','We',1)
   if g:ScColWidthFound == 1
       let g:ScColWidths = MvAddElement(g:ScColWidths,':',col('.'))
   endif
   return g:ScColWidthFound
endfunction

function! ScGetColWidths()
    let g:ScOrigCol = col('.')
    let g:ScOrigLine = line('.')
    call cursor(1,1)
    let g:ScColWidths = ":"
    while ScGetColWidthCur()
    endwhile
    call cursor(g:ScOrigLine,g:ScOrigCol)
endfunction

com! -nargs=1 SearchCols :call SearchCols('<args>')
