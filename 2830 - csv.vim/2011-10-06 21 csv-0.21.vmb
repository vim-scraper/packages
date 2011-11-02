" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/csv.vim	[[[1
1356
" Filetype plugin for editing CSV files. "{{{1
" Author:  Christian Brabandt <cb@256bit.org>
" Version: 0.21
" Script:  http://www.vim.org/scripts/script.php?script_id=2830
" License: VIM License
" Last Change: Thu, 06 Oct 2011 19:50:31 +0200
" Documentation: see :help ft_csv.txt
" GetLatestVimScripts: 2830 21 :AutoInstall: csv.vim
"
" Some ideas are take from the wiki http://vim.wikia.com/wiki/VimTip667
" though, implementation differs.

" Plugin folklore "{{{2
if v:version < 700 || exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

" Function definitions: "{{{2
fu! <sid>Warn(mess) "{{{3
    echohl WarningMsg
    echomsg "CSV: " . a:mess
    echohl Normal
endfu

fu! <sid>Init() "{{{3
    " Hilight Group for Columns
    if exists("g:csv_hiGroup")
        let s:hiGroup = g:csv_hiGroup
    else
        let s:hiGroup="WildMenu"
    endif
    if !exists("g:csv_hiHeader")
        let s:hiHeader = "Title"
    else
        let s:hiHeader = g:csv_hiHeader
    endif
    exe "hi link CSVHeaderLine" s:hiHeader

    " Determine default Delimiter
    if !exists("g:csv_delim")
        let b:delimiter=<SID>GetDelimiter()
    else
        let b:delimiter=g:csv_delim
    endif

    if empty(b:delimiter) && !exists("b:csv_fixed_width")
        call <SID>Warn("No delimiter found. See :h csv-delimiter to set it manually!")
    endif

    let s:del='\%(' . b:delimiter . '\|$\)'
    " Pattern for matching a single column
    if !exists("g:csv_strict_columns") && !exists("g:csv_col")
        \ && !exists("b:csv_fixed_width")
        " - Allow double quotes as escaped quotes only insides double quotes
        " - Allow linebreaks only, if g:csv_nl isn't set (this is
        "   only allowed in double quoted strings see RFC4180), though this
        "   does not work with :WhatColumn and might mess up syntax
        "   highlighting.
        " - optionally allow whitespace in front of the fields (to make it
        "   work with :ArrangeCol (that is actually not RFC4180 valid))
        " - Should work with most ugly solutions that are available
        let b:col='\%(\%(\%(' . (b:delimiter !~ '\s' ? '\s*' : '') .
                \ '"\%(' . (exists("g:csv_nl") ? '\_' : '' ) .
                \ '[^"]\|""\)*"\)' . s:del . '\)\|\%(' .
                \  '[^' .  b:delimiter . ']*' . s:del . '\)\)'
    elseif !exists("g:csv_col") && exists("g:csv_strict_columns")
        " strict columns
        let b:col='\%([^' . b:delimiter . ']*' . s:del . '\)'
    elseif exists("b:csv_fixed_width")
        " Fixed width column
        let b:col=''
        " Check for sane default
        if b:csv_fixed_width =~? '[^0-9,]'
            call <sid>Warn("Please specify the list of character columns" .
                \ "like this: '1,3,5'. See also :h csv-fixedwidth")
            return
        endif
        let b:csv_fixed_width_cols=split(b:csv_fixed_width, ',')
        " Force evaluating as numbers
        call map(b:csv_fixed_width_cols, 'v:val+0')
    else
        " User given column definition
        let b:col = g:csv_col
    endif
    " Check Header line
    " Defines which line is considered to be a header line
    call <sid>CheckHeaderLine()

    " define buffer-local commands
    call <SID>CommandDefinitions()
    " CSV specific mappings
    call <SID>CSVMappings()

    " Highlight column, on which the cursor is?
    if exists("g:csv_highlight_column") && g:csv_highlight_column =~? 'y' &&
        \ !exists("#CSV_HI#CursorMoved")
        aug CSV_HI
            au!
            au CursorMoved <buffer> HiColumn
        aug end
        " Set highlighting for column, on which the cursor is currently
        HiColumn
    elseif exists("#CSV_HI#CursorMoved")
        aug CSV_HI
            au! CursorMoved <buffer>
        aug end
        aug! CSV_HI
        " Remove any existing highlighting
        HiColumn!
    endif

    " force reloading CSV Syntax Highlighting
    if exists("b:current_syntax")
        unlet b:current_syntax
        " Force reloading syntax file
    endif
    if !exists("#CSV#ColorScheme")
        " Make sure, syntax highlighting is applied
        " after changing the colorscheme
        augroup CSV
            au!
            au ColorScheme *.csv,*.dat do Syntax
        augroup end
    endif
    call <sid>DisableFolding()
    silent do Syntax

    " undo when setting a new filetype
    let b:undo_ftplugin = "setlocal sol< tw< wrap<"
        \ . "| setl fen< fdm< fdl< fdc< fml<"
        \ . "| unlet! b:delimiter b:col b:csv_fixed_width_cols b:csv_filter"
        \ . "| unlet! b:csv_fixed_width b:csv_list b:col_width"
        \ . "| unlet! b:CSV_SplitWindow b:csv_headerline"

    for com in ["WhatColumn", "NrColumns", "HiColumn", "SearchInColumn",
            \ "DeleteColumn",  "ArrangeColumn", "InitCSV", "Header",
            \ "VHeader", "HeaderToggle", "VHeaderToggle", "Sort",
            \ "Column", "MoveColumn", "SumCol", "ConvertData",
            \ "Filters", "Analyze", "UnArrangeColumn", "CSVFixed",
            \ "VertFold" ]
        let b:undo_ftplugin .= "| sil! delc " . com
    endfor


    " CSV local settings
    setl nostartofline tw=0 nowrap

    if has("conceal")
        setl cole=2 cocu=nc
        let b:undo_ftplugin .= '|setl cole< cocu<'
    endif
endfu

fu! <sid>GetPat(colnr, maxcolnr, pat) "{{{3
    if a:colnr > 1 && a:colnr < a:maxcolnr
        if !exists("b:csv_fixed_width_cols")
            return '^' . <SID>GetColPat(a:colnr-1,0) . '\%([^' .
                \ b:delimiter . ']*\)\?\zs' . a:pat . '\ze' .
                \ '\%([^' . b:delimiter .']*\)\?' .
                \ b:delimiter . <SID>GetColPat(a:maxcolnr - a:colnr, 0) .
                \ '$'
        else
            return '\%' . b:csv_fixed_width_cols[(a:colnr - 1)] . 'c\zs'
                \ . a:pat . '.\{-}\ze\%'
                \ . (b:csv_fixed_width_cols[a:colnr]) . 'c\ze'
        endif
    elseif a:colnr == a:maxcolnr
        if !exists("b:csv_fixed_width_cols")
            return '^' . <SID>GetColPat(a:colnr - 1,0) .
                \ '\%([^' . b:delimiter .
                \ ']*\)\?\zs' . a:pat . '\ze'
        else
            return '\%' . b:csv_fixed_width_cols[-1] .
                \ 'c\zs' . a:pat . '\ze'
        endif
    else " colnr = 1
        if !exists("b:csv_fixed_width_cols")
            return '^' . '\%([^' . b:delimiter . ']*\)\?\zs' . a:pat .
            \ '\ze\%([^' . b:delimiter . ']*\)\?' . b:delimiter .
            \ <SID>GetColPat(a:maxcolnr -1 , 0) . '$'
        else
            return a:pat . '\ze.\{-}\%' . b:csv_fixed_width_cols[1] . 'c'
        endif
    endif
    return ''
endfu

fu! <sid>SearchColumn(arg) "{{{3
    try
        let arglist=split(a:arg)
        if len(arglist) == 1
            let colnr=<SID>WColumn()
            let pat=substitute(arglist[0], '^\(.\)\(.*\)\1$', '\2', '')
            if pat == arglist[0]
                throw "E684"
            endif
        else
            let colnr=arglist[0]
            let pat=substitute(arglist[1], '^\(.\)\(.*\)\1$', '\2', '')
            if pat == arglist[1]
                throw "E684"
            endif
        endif
    "catch /^Vim\%((\a\+)\)\=:E684/
    catch /E684/	" catch error index out of bounds
        call <SID>Warn("Error! Usage :SearchInColumn [<colnr>] /pattern/")
        return 1
    endtry
    let maxcolnr = <SID>MaxColumns()
    if colnr > maxcolnr
        call <SID>Warn("There exists no column " . colnr)
        return 1
    endif
    let @/ = <sid>GetPat(colnr, maxcolnr, pat)
    try
        norm! n
    catch /^Vim\%((\a\+)\)\=:E486/
        " Pattern not found
        echohl Error
        echomsg "E486: Pattern not found in column " . colnr . ": " . pat
        if &vbs > 0
            echomsg substitute(v:exception, '^[^:]*:', '','')
        endif
        echohl Normal
    endtry
endfu


fu! <sid>DelColumn(colnr) "{{{3
    let maxcolnr = <SID>MaxColumns()
    let _p = getpos('.')

    if empty(a:colnr)
       let colnr=<SID>WColumn()
    else
       let colnr=a:colnr
    endif

    if colnr > maxcolnr
        call <SID>Warn("There exists no column " . colnr)
        return
    endif

    if colnr != '1'
        if !exists("b:csv_fixed_width_cols")
            let pat= '^' . <SID>GetColPat(colnr-1,1) . b:col
        else
            let pat= <SID>GetColPat(colnr,0)
        endif
    else
        " distinction between csv and fixed width does not matter here
        let pat= '^' . <SID>GetColPat(colnr,0)
    endif
    if &ro
       setl noro
    endif
    exe ':%s/' . escape(pat, '/') . '//'
    call setpos('.', _p)
endfu

fu! <sid>HiCol(colnr, bang) "{{{3
    if a:colnr > <SID>MaxColumns() && !a:bang
        call <SID>Warn("There exists no column " . a:colnr)
        return
    endif
    if !a:bang
        if empty(a:colnr)
            let colnr=<SID>WColumn()
        else
            let colnr=a:colnr
        endif

        if colnr==1
            let pat='^'. <SID>GetColPat(colnr,0)
        elseif !exists("b:csv_fixed_width_cols")
            let pat='^'. <SID>GetColPat(colnr-1,1) . b:col
        else
            let pat=<SID>GetColPat(colnr,0)
        endif
    endif

    if exists("*matchadd")
        if exists("s:matchid")
            " ignore errors, that come from already deleted matches
            sil! call matchdelete(s:matchid)
        endif
        " Additionally, filter all matches, that could have been used earlier
        let matchlist=getmatches()
        call filter(matchlist, 'v:val["group"] !~ s:hiGroup')
        call setmatches(matchlist)
        if a:bang
            return
        endif
        let s:matchid=matchadd(s:hiGroup, pat, 0)
    elseif !a:bang
        exe ":2match " . s:hiGroup . ' /' . pat . '/'
    endif
endfu

fu! <sid>GetDelimiter() "{{{3
    if !exists("b:csv_fixed_width_cols")
        let _cur = getpos('.')
        let Delim={0: ';', 1:  ',', 2: '|', 3: '	'}
        let temp={}
        for i in  values(Delim)
            redir => temp[i]
            exe "silent! %s/" . i . "/&/nge"
            redir END
        endfor
        let Delim = map(temp, 'matchstr(substitute(v:val, "\n", "", ""), "^\\d\\+")')
        let Delim = filter(temp, 'v:val=~''\d''')
        let max   = max(values(temp))

        let result=[]
        call setpos('.', _cur)
        for [key, value] in items(Delim)
            if value == max
                return key
            endif
        endfor
        return ''
    else
        " There is no delimiter for fixedwidth files
        return ''
    endif
endfu

fu! <sid>WColumn(...) "{{{3
    " Return on which column the cursor is
    let _cur = getpos('.')
    if !exists("b:csv_fixed_width_cols")
        let line=getline('.')
        " move cursor to end of field
        "call search(b:col, 'ec', line('.'))
        call search(b:col, 'ec')
        let end=col('.')-1
        let fields=(split(line[0:end],b:col.'\zs'))
        let ret=len(fields)
        if exists("a:1") && a:1 > 0
            " bang attribute
            let head  = split(getline(1),b:col.'\zs')
            " remove preceeding whitespace
            let ret   = substitute(head[ret-1], '^\s\+', '', '')
            " remove delimiter
            let ret   = substitute(ret, b:delimiter. '$', '', '')
        endif
    else
        let temp=getpos('.')[2]
        let j=1
        let ret = 1
        for i in sort(b:csv_fixed_width_cols, "<sid>SortList")
            if temp >= i
                let ret = j
            endif
            let j += 1
        endfor
    endif
    call setpos('.',_cur)
    return ret
endfu

fu! <sid>MaxColumns() "{{{3
    "return maximum number of columns in first 10 lines
    if !exists("b:csv_fixed_width_cols")
        let l=getline(1,10)
        let fields=[]
        let result=0
        for item in l
            let temp=len(split(item, b:col.'\zs'))
            let result=(temp>result ? temp : result)
        endfor
        return result
    else
        return len(b:csv_fixed_width_cols)
    endif
endfu

fu! <sid>ColWidth(colnr) "{{{3
    " Return the width of a column
    " Internal function
    let width=20 "Fallback (wild guess)
    let tlist=[]

    if !exists("b:csv_fixed_width_cols")
        if !exists("b:csv_list")
            let b:csv_list=getline(1,'$')
            call map(b:csv_list, 'split(v:val, b:col.''\zs'')')
        endif
        try
            for item in b:csv_list
                call add(tlist, item[a:colnr-1])
            endfor
            " we have a list of the first 10 rows
            " Now transform it to a list of field a:colnr
            " and then return the maximum strlen
            " That could be done in 1 line, but that would look ugly
            "call map(list, 'split(v:val, b:col."\\zs")[a:colnr-1]')
            call map(tlist, 'substitute(v:val, ".", "x", "g")')
            call map(tlist, 'strlen(v:val)')
            return max(tlist)
        catch
            return width
        endtry
    elseif a:colnr > 0
        return b:csv_fixed_width_cols[a:colnr] - b:csv_fixed_width_cols[(a:colnr - 1)]
    endif
endfu

fu! <sid>ArrangeCol(first, last, bang) range "{{{3
    "TODO: Why doesn't that work?
    " is this because of the range flag?
    " It's because of the way, Vim works with
    " a:firstline and a:lastline parameter, therefore
    " explicitly give the range as argument to the function
    if exists("b:csv_fixed_width_cols")
        " Nothing to do
        call <sid>Warn("ArrangeColumn does not work with fixed width column!")
        return
    endif
    let cur=winsaveview()
    if a:bang || !exists("b:col_width")
        " Force recalculation of Column width
        call <sid>CalculateColumnWidth()
    endif

    if &ro
       " Just in case, to prevent the Warning
       " Warning: W10: Changing read-only file
       setl noro
    endif
    exe a:first . ',' . a:last .'s/' . (b:col) .
    \ '/\=<SID>Columnize(submatch(0))/' . (&gd ? '' : 'g')
    " Clean up variables, that were only needed for <sid>Columnize() function
    unlet! s:columnize_count s:max_cols s:prev_line
    setl ro
    call winrestview(cur)
endfu

fu! <sid>PrepUnArrangeCol(first, last) "{{{3
    " Because of the way, Vim works with
    " a:firstline and a:lastline parameter,
    " explicitly give the range as argument to the function
    if exists("b:csv_fixed_width_cols")
        " Nothing to do
        call <sid>Warn("UnArrangeColumn does not work with fixed width column!")
        return
    endif
    let cur=winsaveview()

    if &ro
       " Just in case, to prevent the Warning
       " Warning: W10: Changing read-only file
       setl noro
    endif
    exe a:first . ',' . a:last .'s/' . (b:col) .
    \ '/\=<SID>UnArrangeCol(submatch(0))/' . (&gd ? '' : 'g')
    " Clean up variables, that were only needed for <sid>Columnize() function
    call winrestview(cur)
endfu

fu! <sid>UnArrangeCol(match) "{{{3
    " Strip leading white space, also trims empty records:
    return substitute(a:match, '^\s\+', '', '')
    " only strip leading white space, if a non-white space follows:
    "return substitute(a:match, '^\s\+\ze\S', '', '')
endfu

fu! <sid>CalculateColumnWidth() "{{{3
    " Internal function, not called from external,
    " does not work with fixed width columns
    let b:col_width=[]
    " Force recalculating the Column width
    unlet! b:csv_list
    let s:max_cols=<SID>MaxColumns()
    for i in range(1,s:max_cols)
        call add(b:col_width, <SID>ColWidth(i))
    endfor
    " delete buffer content in variable b:csv_list,
    " this was only necessary for calculating the max width
    unlet! b:csv_list
endfu

fu! <sid>Columnize(field) "{{{3
    " Internal function, not called from external,
    " does not work with fixed width columns
    if !exists("s:columnize_count")
        let s:columnize_count = 0
    endif


    if !exists("s:max_cols")
        let s:max_cols = len(b:col_width)
    endif

    if exists("s:prev_line") && s:prev_line != line('.')
        let s:columnize_count = 0
    endif

    let s:prev_line = line('.')
    " convert zero based indexed list to 1 based indexed list,
    " Default: 20 width, in case that column width isn't defined
    " Careful: Keep this fast! Using
    "let width=get(b:col_width,<SID>WColumn()-1,20)
    " is too slow, so we are using:
    let width=get(b:col_width, (s:columnize_count % s:max_cols), 20)

    let s:columnize_count += 1
    if !exists("g:csv_no_multibyte") &&
        \ match(a:field, '[^ -~]') != -1
        " match characters outside the ascii range
        let a = split(a:field, '\zs')
        let add = eval(join(map(a, 'len(v:val)'), '+'))
        let add -= len(a)
    else
        let add = 0
    endif

    if width + add + 1 == strlen(a:field)
        " Column has correct length, don't use printf()
        return a:field
    endif

    " Add one for the frame
    " plus additional width for multibyte chars,
    " since printf(%*s..) uses byte width!
    let width = width + add  + 1

    return printf("%*s", width ,  a:field)
endfun

fu! <sid>GetColPat(colnr, zs_flag) "{{{3
    " Return Pattern for given column
    if a:colnr > 1
        if !exists("b:csv_fixed_width_cols")
            let pat=b:col . '\{' . (a:colnr) . '\}'
        else
            if a:colnr >= len(b:csv_fixed_width_cols)
            " Get last column
                let pat='\%' . b:csv_fixed_width_cols[-1] . 'c.*'
            else
            let pat='\%' . b:csv_fixed_width_cols[(a:colnr - 1)] .
            \ 'c.\{-}\%' .   b:csv_fixed_width_cols[a:colnr] . 'c'
            endif
        endif
    elseif !exists("b:csv_fixed_width_cols")
        let pat=b:col
    else
        let pat='\%' . b:csv_fixed_width_cols[0] . 'c.\{-}' .
            \ len(b:csv_fixed_width_cols) > 1 ?
            \ '\%' . b:csv_fixed_width_cols[1] . 'c' :
            \ ''
    endif
    return pat . (a:zs_flag ? '\zs' : '')
endfu

fu! <sid>SplitHeaderLine(lines, bang, hor) "{{{3
    if exists("b:csv_fixed_width_cols")
        call <sid>Warn("Header does not work with fixed width column!")
        return
    endif
    call <sid>CheckHeaderLine()
    if !a:bang
        " A Split Header Window already exists,
        " first close the already existing Window
        if exists("b:CSV_SplitWindow")
            call <sid>SplitHeaderLine(a:lines, 1, a:hor)
        endif
        " Split Window
        let _stl = &l:stl
        let _sbo = &sbo
        if a:hor
            setl scrollopt=hor scrollbind
            let lines = empty(a:lines) ? s:csv_fold_headerline : a:lines
            abo sp
            1
            exe "resize" . lines
            setl scrollopt=hor scrollbind winfixheight
            "let &l:stl=repeat(' ', winwidth(0))
            let &l:stl="%#Normal#".repeat(' ',winwidth(0))
            " Highlight first row
            let win = winnr()
        else
            setl scrollopt=ver scrollbind
            0
            let b=b:col
            let a=[]
            let a=<sid>CopyCol('',1)
            " Force recalculating columns width
            unlet! b:csv_list
            let width = <sid>ColWidth(1)
            let b=b:col
            abo vsp +enew
            let b:col=b
            call append(0, a)
            $d _
            sil %s/.*/\=printf("%.*s", width, submatch(0))/eg
            0
            exe "vert res" width
            setl scrollopt=ver scrollbind winfixwidth
            setl buftype=nowrite bufhidden=hide noswapfile nobuflisted
            let win = winnr()
        endif
        call matchadd("CSVHeaderLine", b:col)
        exe "wincmd p"
        let b:CSV_SplitWindow = win
    else
        " Close split window
        if !exists("b:CSV_SplitWindow")
            return
        endif
        exe b:CSV_SplitWindow . "wincmd w"
        if exists("_stl")
            let &l_stl = _stl
        endif
        if exists("_sbo")
            let &sbo = _sbo
        endif
        setl noscrollbind
        wincmd c
        unlet! b:CSV_SplitWindow
    endif
endfu

fu! <sid>SplitHeaderToggle(hor) "{{{3
    if !exists("b:CSV_SplitWindow")
        :call <sid>SplitHeaderLine(1,0,a:hor)
    else
        :call <sid>SplitHeaderLine(1,1,a:hor)
    endif
endfu

" TODO: from here on add logic for fixed-width csv files!
fu! <sid>MoveCol(forward, line) "{{{3
    " Move cursor position upwards/downwards left/right
    let colnr=<SID>WColumn()
    let maxcol=<SID>MaxColumns()
    let cpos=getpos('.')[2]
    if !exists("b:csv_fixed_width_cols")
        call search(b:col, 'bc', line('.'))
    endif
    let spos=getpos('.')[2]

    " Check for valid column
    " a:forward == 1 : search next col
    " a:forward == -1: search prev col
    " a:forward == 0 : stay in col
    if colnr - v:count1 >= 1 && a:forward == -1
        let colnr -= v:count1
    elseif colnr - v:count1 < 1 && a:forward == -1
        let colnr = 0
    elseif colnr + v:count1 <= maxcol && a:forward == 1
        let colnr += v:count1
    elseif colnr + v:count1 > maxcol && a:forward == 1
        let colnr = maxcol + 1
    endif

    let line=a:line
    if line < 1
        let line=1
    elseif line > line('$')
        let line=line('$')
    endif

    " Generate search pattern
    if colnr == 1
        let pat = '^' . <SID>GetColPat(colnr-1,0)
        "let pat = pat . '\%' . line . 'l'
    elseif (colnr == 0) || (colnr == maxcol + 1)
        if !exists("b:csv_fixed_width_cols")
            let pat=b:col
        else
            if a:forward > 0
                " Move forwards
                let pat=<sid>GetColPat(1, 0)
            else
                " Move backwards
                let pat=<sid>GetColPat(maxcol, 0)
            endif
        endif
    else
        if !exists("b:csv_fixed_width_cols")
            let pat='^'. <SID>GetColPat(colnr-1,1) . b:col
        else
            let pat=<SID>GetColPat(colnr,0)
        endif
        "let pat = pat . '\%' . line . 'l'
    endif

    " Search
    " move left/right
    if a:forward > 0
        call search(pat, 'W')
    elseif a:forward < 0
        call search(pat, 'bWe')
        " Moving upwards/downwards
    elseif line >= line('.')
        call search(pat . '\%' . line . 'l', '', line)
        " Move to the correct screen column
        " This is a best effort approach, we might still
        " leave the column (if the next column is shorter)
        if !exists("b:csv_fixed_width_cols")
            let a    = getpos('.')
            let a[2]+= cpos-spos
        else
            let a    = getpos('.')
            let a[2] = cpos
        endif
        call setpos('.', a)
    elseif line < line('.')
        call search(pat . '\%' . line . 'l', 'b', line)
        " Move to the correct screen column
        if !exists("b:csv_fixed_width_cols")
            let a    = getpos('.')
            let a[2]+= cpos-spos
        else
            let a    = getpos('.')
            let a[2] = cpos
        endif
        call setpos('.', a)
    endif
endfun

fu! <sid>SortComplete(A,L,P) "{{{3
    return join(range(1,<sid>MaxColumns()),"\n")
endfun

fu! <sid>SortList(a1, a2) "{{{3
    return a:a1+0 == a:a2+0 ? 0 : a:a1+0 > a:a2+0 ? 1 : -1
endfu

fu! <sid>Sort(bang, line1, line2, colnr) range "{{{3
    let wsv=winsaveview()
    if a:colnr =~? 'n'
        let numeric = 1
    else
        let numeric = 0
    endif
    let col = (empty(a:colnr) || a:colnr !~? '\d\+') ? <sid>WColumn() : a:colnr+0
    if col != 1
        if !exists("b:csv_fixed_width_cols")
            let pat= '^' . <SID>GetColPat(col-1,1) . b:col
        else
            let pat= '^' . <SID>GetColPat(col,0)
        endif
    else
        let pat= '^' . <SID>GetColPat(col,0)
    endif
    exe a:line1 ',' a:line2 . "sort" . (a:bang ? '!' : '') .
        \' r ' . (numeric ? 'n' : '') . ' /' . pat . '/'
    call winrestview(wsv)
endfun

fu! CSV_WCol(...) "{{{3
    if exists("a:1") && (a:1 == 'Name' || a:1 == 1)
        return printf("%s", <sid>WColumn(1))
    else
        return printf(" %d/%d", <SID>WColumn(), <SID>MaxColumns())
    endif
endfun

fu! <sid>CopyCol(reg, col) "{{{3
    " Return Specified Column into register reg
    let col = a:col == "0" ? <sid>WColumn() : a:col+0
    let mcol = <sid>MaxColumns()
    if col == '$' || col > mcol
        let col = mcol
    endif
    let a=getline(1, '$')
    if !exists("b:csv_fixed_width_cols")
        call map(a, 'split(v:val, ''^'' . b:col . ''\zs'')[col-1]')
    else
        call map(a, 'matchstr(v:val, <sid>GetColPat(col, 0))')
    endif
    if a:reg =~ '[-"0-9a-zA-Z*+]'
        "exe  ':let @' . a:reg . ' = "' . join(a, "\n") . '"'
        " set the register to blockwise mode
        call setreg(a:reg, join(a, "\n"), 'b')
    else
        return a
    endif
endfu

fu! <sid>MoveColumn(start, stop, ...) range "{{{3
    " Move column behind dest
    " Explicitly give the range as argument,
    " cause otherwise, Vim would move the cursor
    let wsv = winsaveview()

    let col = <sid>WColumn()
    let max = <sid>MaxColumns()

    " If no argument is given, move current column after last column
    let source=(exists("a:1") && a:1 > 0 && a:1 <= max ? a:1 : col)
    let dest  =(exists("a:2") && a:2 > 0 && a:2 <= max ? a:2 : max)

    " translate 1 based columns into zero based list index
    let source -= 1
    let dest   -= 1

    if source >= dest
        call <sid>Warn("Destination column before source column, aborting!")
        return
    endif

    " Swap line by line, instead of reading the whole range into memory

    for i in range(a:start, a:stop)
        if !exists("b:csv_fixed_width_cols")
            let fields=split(getline(i), b:col . '\zs')
        else
            let fields=[]
            for j in range(1, max, 1)
                call add(fields, matchstr(getline(i), <sid>GetColPat(j,0)))
            endfor
        endif

        " Add delimiter to destination column, in case there was none,
        " remove delimiter from source, in case destination did not have one
        if matchstr(fields[dest], '.$') !~? b:delimiter
            let fields[dest] = fields[dest] . b:delimiter
            if matchstr(fields[source], '.$') =~? b:delimiter
            let fields[source] = substitute(fields[source],
                \ '^\(.*\).$', '\1', '')
            endif
        endif

        let fields= (source == 0 ? [] : fields[0 : (source-1)])
                \ + fields[ (source+1) : dest ]
                \ + [ fields[source] ] + fields[(dest+1):]

        call setline(i, join(fields, ''))
    endfor

    call winrestview(wsv)

endfu

fu! <sid>SumColumn(list) "{{{3
    return eval(join(a:list, '+'))
endfu

fu! csv#EvalColumn(nr, func, first, last) range "{{{3
    let save = winsaveview()
    let col = (empty(a:nr) ? <sid>WColumn() : a:nr)
    let start = a:first - 1
    let stop  = a:last  - 1

    let column = <sid>CopyCol('', col)[start : stop]
    " Delete delimiter
    call map(column, 'substitute(v:val, b:delimiter, "", "g")')
    try
        let result=call(function(a:func), [column])
        return result
    catch
        " Evaluation of expression failed
        echohl Title
        echomsg "Evaluating" matchstr(a:func, '[a-zA-Z]\+$')
        \ "failed for column" col . "!"
        echohl Normal
        return ''
    finally
        call winrestview(save)
    endtry
endfu


fu! <sid>DoForEachColumn(start, stop, bang) range "{{{3
    " Do something for each column,
    " e.g. generate SQL-Statements, convert to HTML,
    " something like this
    " TODO: Define the function
    " needs a csv_pre_convert variable
    "         csv_post_convert variable
    "         csv_convert variable
    "         result contains converted buffer content
    let result = []

    if !exists("g:csv_convert")
        call <sid>Warn("You need to define how to convert your data using" .
                \ "the g:csv_convert variable, see :h csv-convert")
        return
    endif

    if exists("g:csv_pre_convert") && !empty(g:csv_pre_convert)
        call add(result, g:csv_pre_convert)
    endif

    for item in range(a:start, a:stop, 1)
        let t = g:csv_convert
        let line = getline(item)
        let context = split(g:csv_convert, '%s')
        let columns = len(context)
        if columns > <sid>MaxColumns()
            let columns = <sid>MaxColumns()
        elseif columns == 1
            call <sid>Warn("No Columns defined in your g:csv_convert variable, Aborting")
            return
        endif

        if !exists("b:csv_fixed_width_cols")
            let fields=split(line, b:col . '\zs')
            if a:bang
                call map(fields, 'substitute(v:val, b:delimiter .
                    \ ''\?$'' , "", "")')
            endif
        else
            let fields=[]
            for j in range(1, columns, 1)
                call add(fields, matchstr(line, <sid>GetColPat(j,0)))
            endfor
        endif
        for j in range(1, columns, 1)
            let t=substitute(t, '%s', fields[j-1], '')
        endfor
        call add(result, t)
    endfor

    if exists("g:csv_post_convert") && !empty(g:csv_post_convert)
        call add(result, g:csv_post_convert)
    endif

    new
    call append('$', result)
    1d _

endfun

fu! <sid>PrepareDoForEachColumn(start, stop, bang) range"{{{3
    let pre = exists("g:csv_pre_convert") ? g:csv_pre_convert : ''
    let g:csv_pre_convert=input('Pre convert text: ', pre)
    let post = exists("g:csv_post_convert") ? g:csv_post_convert : ''
    let g:csv_post_convert=input('Post convert text: ', post)
    let convert = exists("g:csv_convert") ? g:csv_convert : ''
    let g:csv_convert=input("Converted text, use %s for column input:\n", convert)
    call <sid>DoForEachColumn(a:start, a:stop, a:bang)
endfun
fu! <sid>CSVMappings() "{{{3
    noremap <silent> <buffer> W :<C-U>call <SID>MoveCol(1, line('.'))<CR>
    noremap <silent> <buffer> E :<C-U>call <SID>MoveCol(-1, line('.'))<CR>
    noremap <silent> <buffer> K :<C-U>call <SID>MoveCol(0, line('.')-v:count1)<CR>
    noremap <silent> <buffer> J :<C-U>call <SID>MoveCol(0, line('.')+v:count1)<CR>
    nnoremap <silent> <buffer> <CR> :<C-U>call <SID>PrepareFolding(1)<CR>
    nnoremap <silent> <buffer> <BS> :<C-U>call <SID>PrepareFolding(0)<CR>
    " Remap <CR> original values to a sane backup
    noremap <silent> <buffer> <LocalLeader>J J
    noremap <silent> <buffer> <LocalLeader>K K
    noremap <silent> <buffer> <LocalLeader>W W
    noremap <silent> <buffer> <LocalLeader>E E
    noremap <silent> <buffer> <LocalLeader>H H
    noremap <silent> <buffer> <LocalLeader>L L
    nnoremap <silent> <buffer> <LocalLeader><CR> <CR>
    nnoremap <silent> <buffer> <LocalLeader><BS> <BS>
    " Map C-Right and C-Left as alternative to W and E
    map <silent> <buffer> <C-Right> W
    map <silent> <buffer> <C-Left>  E
    map <silent> <buffer> H E
    map <silent> <buffer> L W
    map <silent> <buffer> <Up> K
    map <silent> <buffer> <Down> J
endfu

fu! <sid>EscapeValue(val) "{{{3
    return '\V' . escape(a:val, '\')
endfu

fu! <sid>FoldValue(lnum, val) "{{{3
    call <sid>CheckHeaderLine()

    if (a:lnum == s:csv_fold_headerline)
        " Don't fold away the header line
        return 0
    endif

    " Match literally, don't use regular expressions for matching
    if (getline(a:lnum) =~ a:val)
        return 0
    else
        return 1
    endif
endfu

fu! <sid>PrepareFolding(add)  "{{{3
    if !has("folding")
        return
    endif

    if !exists("b:csv_filter")
        let b:csv_filter = {}
    endif
    if !exists("s:filter_count") || s:filter_count < 1
        let s:filter_count = 0
    endif

    if !a:add
        " remove last added item from filter
        if len(b:csv_filter) > 0
            call <sid>RemoveLastItem(s:filter_count)
            let s:filter_count -= 1
            if len(b:csv_filter) == 0
                call <sid>DisableFolding()
                return
            endif
        else
            " Disable folding, if no pattern available
            call <sid>DisableFolding()
            return
        endif
    else

        let col = <sid>WColumn()
        let max = <sid>MaxColumns()
        let a   = <sid>GetColumn(line('.'), col)

        try
            " strip leading whitespace
            if (a !~ '\s\+'. b:delimiter . '$')
                let b = split(a, '^\s\+\ze\S')[0]
            else
                let b = a
            endif
        catch /^Vim\%((\a\+)\)\=:E684/
            " empty pattern - should match only empty columns
            let b = a
        endtry

        " strip trailing delimiter
        try
            let a = split(b, b:delimiter . '$')[0]
        catch /^Vim\%((\a\+)\)\=:E684/
            let a = b
        endtry

        " Make a column pattern
        let b= '\%(' .
            \ (exists("b:csv_fixed_width") ? '.*' : '') .
            \ <sid>GetPat(col, max, <sid>EscapeValue(a) . '\m') .
            \ '\)'

        let s:filter_count += 1
        let b:csv_filter[col] = { 'pat': b, 'id': s:filter_count,
            \ 'col': col, 'orig': a }

    endif
    " Put the pattern into the search register, so they will also
    " be highlighted
    let @/ = ''
    for val in sort(values(b:csv_filter), '<sid>SortFilter')
        let @/ .= val.pat . (val.id == s:filter_count ? '' : '\&')
    endfor
    let sid = <sid>GetSID()
    " Don't put spaces between the arguments!
    exe 'setl foldexpr=' . sid . '_FoldValue(v:lnum,@/)'
    "setl foldexpr=s:FoldValue(v:lnum,@/)
    " Be sure to also fold away single screen lines
    setl fen fdm=expr fdl=0 fdc=2 fml=0
    setl foldtext=substitute(v:folddashes,'-','\ ','g')
    setl fillchars-=fold:-
endfu

fu! <sid>OutputFilters() "{{{3
    call <sid>CheckHeaderLine()
    if s:csv_fold_headerline
        let  title="Nr\tCol\t      Name\tValue"
    else
        let  title="Nr\tCol\tValue"
    endif
    echohl "Title"
    echo   printf("%s", title)
    echo   printf("%s", repeat("=",strdisplaywidth(title)))
    echohl "Normal"
    if !exists("b:csv_filter") || len(b:csv_filter) == 0
        echo printf("%s", "No active filter")
    else
        let items = values(b:csv_filter)
        call sort(items, "<sid>SortFilter")
        for item in items
            if s:csv_fold_headerline
                echo printf("%02d\t%02d\t%10.10s\t%s",
                    \ item.id, item.col, <sid>GetColumn(1, item.col),
                    \ item.orig)
            else
                echo printf("%02d\t%02d\t%s",
                    \ item.id, item.col, item.orig)
            endif
        endfor
    endif
endfu

fu! <sid>SortFilter(a, b) "{{{3
    return a:a.id == a:b.id ? 0 :
        \ a:a.id > a:b.id ? 1 : -1
endfu

fu! <sid>GetColumn(line, col) "{{{3
    " Return Column content at a:line, a:col
    let a=getline(a:line)
    if !exists("b:csv_fixed_width_cols")
        return split(a, '^' . b:col . '\zs')[a:col - 1]
    else
        return matchstr(a, <sid>GetColPat(a:col, 0))
    endif
endfu

fu! <sid>RemoveLastItem(count) "{{{3
    for [key,value] in items(b:csv_filter)
        if value.id == a:count
            call remove(b:csv_filter, key)
        endif
    endfor
endfu

fu! <sid>DisableFolding() "{{{3
    setl nofen fdm=manual fdc=0 fdl=0 fillchars+=fold:-
endfu

fu! <sid>GetSID() "{{{3
    if v:version > 703 || v:version == 703 && has("patch032")
        return '<SNR>' . maparg('W', "", "", 1).sid
    else
        return substitute(maparg('W'), '\(<SNR>\d\+\)_', '\1', '')
    endif
endfu

fu! <sid>CheckHeaderLine() "{{{3
    if !exists("b:csv_headerline")
        let s:csv_fold_headerline = 1
    else
        let s:csv_fold_headerline = b:csv_headerline
    endif
endfu

fu! <sid>AnalyzeColumn(...) "{{{3
    let maxcolnr = <SID>MaxColumns()
    if len(a:000) == 1
        let colnr = a:1
    else
        let colnr = <sid>WColumn()
    endif

    if colnr > maxcolnr
        call <SID>Warn("There exists no column " . colnr)
        return 1
    endif

    " Initialize s:fold_headerline
    call <sid>CheckHeaderLine()
    let data = <sid>CopyCol('', colnr)[s:csv_fold_headerline : -1]
    let qty = len(data)
    let res = {}
    for item in data
        if !get(res, item)
            let res[item] = 0
        endif
        let res[item]+=1
    endfor

    let max_items = reverse(sort(values(res)))
    if len(max_items) > 5
        call remove(max_items, 5, -1)
        call filter(res, 'v:val =~ ''^''.join(max_items, ''\|'').''$''')
    endif

    if has("float")
        let  title="Nr\tCount\t % \tValue"
    else
        let  title="Nr\tCount\tValue"
    endif
    echohl "Title"
    echo printf("%s", title)
    echohl "Normal"
    echo printf("%s", repeat('=', strdisplaywidth(title)))

    let i=1
    for val in max_items
        for key in keys(res)
            if res[key] == val && i <= len(max_items)
                if !empty(b:delimiter)
                    let k = substitute(key, b:delimiter . '\?$', '', '')
                else
                    let k = key
                endif
                if has("float")
                    echo printf("%02d\t%02d\t%2.0f%%\t%.50s", i, res[key],
                        \ ((res[key] + 0.0)/qty)*100, k)
                else
                    echo printf("%02d\t%02d\t%.50s", i, res[key], k)
                endif
                call remove(res,key)
                let i+=1
            else
                continue
            endif
        endfor
    endfor
    unlet max_items
endfunc


fu! <sid>Vertfold(bang, col) "{{{3
    if a:bang
        do Syntax
        return
    endif
    if !has("conceal")
        call <sid>Warn("Concealing not supported in your Vim")
        return
    endif
    if empty(b:delimiter) && !exists("b:csv_fixed_width_cols")
        call <sid>Warn("There are no columns defined, can't hide away anything!")
        return
    endif
    let pat=<sid>GetPat(a:col, <sid>MaxColumns(), '.*')
    if exists("b:csv_fixed_width_cols") &&
        \ pat !~ '^\^\.\*'
        " Make the pattern implicitly start at line start,
        " so it will be applied by syntax highlighting (:h :syn-priority)
        let pat='^.*' . pat
    endif
    let pat=substitute(pat, '\\zs\(\.\*\)\@=', '', '')
    if !empty(pat)
        exe "syn match CSVFold /" . pat . "/ conceal cchar=+"
    endif
endfu

fu! <sid>InitCSVFixedWidth() "{{{3
    if !exists("+cc")
        " TODO: make this work with a custom matchadd() command for older
        " Vims, that don't have 'colorcolumn'
        call <sid>Warn("'colorcolumn' option not available")
        return
    endif
    " Turn off syntax highlighting
    syn clear
    let _cc  = &l:cc
    let &l:cc = 1
    redraw!
    let list = []
    let tcc  = &l:cc
    echo "<Cursor>, <Space>, <ESC>, <BS>, <CR>..."
    let char=getchar()
    while 1
        if char == "\<Left>" || char == "\<Right>"
            let tcc = eval('tcc'.(char=="\<Left>" ? '-' : '+').'1')
        elseif char == 32 " Space
            call add(list, tcc)
        elseif char == "\<BS>"
            call remove(list, -1)
        elseif char == 27 "<ESC>
            let &l:cc=_cc
            redraw!
            return
        else
            break
        endif
        let &l:cc=tcc . (!empty(list)? ',' . join(list, ','):'')
        redraw!
        echo "<Cursor>, <Space>, <ESC>, <BS>, <CR>..."
        let char=getchar()
    endw
    if tcc > 0
        call add(list,tcc)
    endif
    let b:csv_fixed_width_cols=[]
    let tcc=0
    if !empty(list)
        call Break()
        " Remove duplicate entries
        for val in sort(list, "<sid>SortList")
            if val==tcc
                continue
            endif
            call add(b:csv_fixed_width_cols, val)
            let tcc=val
        endfor
        let b:csv_fixed_width=join(sort(b:csv_fixed_width_cols,
            \ "<sid>SortList"), ',')
        call <sid>Init()
    endif
    let &l:cc=_cc
    redraw!
endfu

fu! Break()
    return
endfu

fu! <sid>LocalCmd(name, definition, args) "{{{3
    if !exists(':'.a:name)
        exe "com! -buffer " a:args a:name a:definition
    endif
endfu

fu! <sid>CommandDefinitions() "{{{3
    " When adding new commands, be sure, to also update
    " b:undo_ftplugin
    call <sid>LocalCmd("WhatColumn", ':echo <sid>WColumn(<bang>0)',
        \ '-bang')
    call <sid>LocalCmd("NrColumns", ':echo <sid>MaxColumns()', '')
    call <sid>LocalCmd("HiColumn", ':call <sid>HiCol(<q-args>,<q-bang>)',
        \ '-bang -nargs=?')
    call <sid>LocalCmd("SearchInColumn",
        \ ':call <sid>SearchColumn(<q-args>)', '-nargs=*')
    call <sid>LocalCmd("DeleteColumn", ':call <sid>DelColumn(<q-args>)',
        \ '-nargs=? -complete=custom,<sid>SortComplete')
    call <sid>LocalCmd("ArrangeColumn",
        \ ':call <sid>ArrangeCol(<line1>, <line2>, <bang>0)',
        \ '-range')
    call <sid>LocalCmd("UnArrangeColumn",
        \':call <sid>PrepUnArrangeCol(<line1>, <line2>)',
        \ '-range')
    call <sid>LocalCmd("InitCSV", ':call <sid>Init()', '')
    call <sid>LocalCmd('Header',
        \ ':call <sid>SplitHeaderLine(<q-args>,<bang>0,1)',
        \ '-nargs=? -bang')
    call <sid>LocalCmd('VHeader',
        \ ':call <sid>SplitHeaderLine(<q-args>,<bang>0,0)',
        \ '-nargs=? -bang')
    call <sid>LocalCmd("HeaderToggle",
        \ ':call <sid>SplitHeaderToggle(1)', '')
    call <sid>LocalCmd("VHeaderToggle",
        \ ':call <sid>SplitHeaderToggle(0)', '')
    call <sid>LocalCmd("Sort",
        \ ':call <sid>Sort(<bang>0, <line1>,<line2>,<q-args>)',
        \ '-nargs=* -bang -range=% -complete=custom,<sid>SortComplete')
    call <sid>LocalCmd("Column",
        \ ':call <sid>CopyCol(empty(<q-reg>)?''"'':<q-reg>,<q-count>)',
        \ '-count -register')
    call <sid>LocalCmd("MoveColumn",
        \ ':call <sid>MoveColumn(<line1>,<line2>,<f-args>)',
        \ '-range=% -nargs=* -complete=custom,<sid>SortComplete')
    call <sid>LocalCmd("SumCol",
        \ ':echo csv#EvalColumn(<q-args>, "<sid>SumColumn", <line1>,<line2>)',
        \ '-nargs=? -range=% -complete=custom,<sid>SortComplete')
    call <sid>LocalCmd("ConvertData",
        \ ':call <sid>PrepareDoForEachColumn(<line1>,<line2>,<bang>0)',
        \ '-bang -nargs=? -range=%')
    call <sid>LocalCmd("Filters", ':call <sid>OutputFilters()',
        \ '-nargs=0')
    call <sid>LocalCmd("Analyze", ':call <sid>AnalyzeColumn(<args>)',
        \ '-nargs=?')
    call <sid>LocalCmd("VertFold", ':call <sid>Vertfold(<bang>0,<args>)',
        \ '-bang -nargs=? -range=% -complete=custom,<sid>SortComplete')
    call <sid>LocalCmd("CSVFixed", ':call <sid>InitCSVFixedWidth()', '')
endfu
" end function definition "}}}2
" Initialize Plugin "{{{2
call <SID>Init()
let &cpo = s:cpo_save
unlet s:cpo_save

" Vim Modeline " {{{2
" vim: set foldmethod=marker et:
doc/ft-csv.txt	[[[1
1160
*ft-csv.txt*	For Vim version 7.3	Last Change: 
Author:		Christian Brabandt <cb@256bit.org>
Version:	0.20
Homepage:	http://www.vim.org/scripts/script.php?script_id=2830

The VIM LICENSE applies to the CSV filetype plugin (see |copyright|).
NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.

                                                           *csv-toc*
1. Introduction.................................|csv-intro|
2. Installation.................................|csv-installation|
3. CSV Commands.................................|csv-commands|
    3.1 WhatColumn..............................|WhatColumn_CSV|
    3.2 NrColumns...............................|NrColumns_CSV|
    3.3 SearchInColumn..........................|SearchInColumn_CSV|
    3.4 HiColumn................................|HiColumn_CSV|
    3.5 ArrangeColumn...........................|ArrangeColumn_CSV|
    3.6 UnArrangeColumn.........................|UnArrangeColumn_CSV|
    3.7 DeleteColumn............................|DeleteColumn_CSV|
    3.8 InitCSV.................................|InitCSV|
    3.9 Header..................................|Header_CSV|
    3.10 Sort...................................|Sort_CSV|
    3.11 CopyColumn.............................|Copy_CSV|
    3.12 MoveColumn.............................|MoveCol_CSV|
    3.13 Sum of a column........................|SumCol_CSV|
    3.14 Normal mode commands...................|csv-mapping|
    3.15 Convert CSV file.......................|csv-convert|
    3.16 Dynamic filters........................|csv-filter|
    3.17 Analyze a column.......................|csv-analyze|
    3.18 Vertical Folding.......................|csv-vertfold|
4. CSV Filetype configuration...................|csv-configuration|
    4.1 Delimiter...............................|csv-delimiter|
    4.2 Column..................................|csv-column|
    4.3 HiGroup.................................|csv-higroup|
    4.4 Strict Columns..........................|csv-strict|
    4.5 Multibyte Chars.........................|csv-mbyte|
    4.6 Concealing..............................|csv-conceal|
    4.7 Newlines................................|csv-newline|
    4.8 Highlight column automatically..........|csv-hicol|
    4.9 Fixed width columns.....................|csv-fixedwidth|
        4.9.1 Manual setup
        4.9.2 Setup using a Wizard
    4.10 CSV Header lines.......................|csv-header|
5. CSV Tips and Tricks..........................|csv-tips|
    5.1 Statusline..............................|csv-stl|
    5.2 Slow ArrangeCol.........................|csv-slow|
    5.3 Defining custom aggregate functions.....|csv-aggregate-functions|
6. CSV Changelog................................|csv-changelog|

==============================================================================
1. Introduction                                                    *csv-intro*

This plugin is used for handling column separated data with Vim. Usually those
files are called csv files and use the ',' as delimiter, though sometimes they
use e.g. the '|' or ';' as delimiter and there also exists fixedwidth columns.
The aim of this plugin is to ease handling these kinds of files.

This is a filetype plugin for CSV files. It was heavily influenced by
the Vim Wiki Tip667 (http://vim.wikia.com/wiki/VimTip667), though it
works differently. For instructions on installing this file, type
:help add-local-help |add-local-help| inside Vim. For a screenshot, of
how the plugin can be used, see http://www.256bit.org/~chrisbra/csv.gif

==============================================================================
2. Installation						*csv-installation*

In order to have vim automatically detect csv files, you need to have
|ftplugins| enabled (e.g. by having this line in your |.vimrc| file: >

   :filetype plugin on

<
The plugin already sets up some logic, to detect CSV files. By default,
the plugin recognizes *.csv and *.dat files as csv filetype. In order that the
csv filetype plugin is loaded correctly, vim needs to be enabled to load
|filetype-plugins|. This can be ensured, by putting a line like this into your
|.vimrc|: >
    :filetype plugin on
<
(see also |filetype-plugin-on|).

In case this did not work, you need to setup vim like this:

To have Vim automatically detect csv files, you need to do the following.

   1) Create your user runtime directory if you do not have one yet. This 
      directory needs to be in your 'runtime' path. In Unix this would
      typically the ~/.vim directory, while in Windows this is usually your
      ~/vimfiles directory. Use :echo expand("~") to find out, what Vim thinks
      your user directory is. 
      To create this directory, you can do: >

      :!mkdir ~/.vim 
<
      for Unix and >

      :!mkdir ~/vimfiles 
<
      for Windows.

   2) In that directory you create a file that will detect csv files. >

    if exists("did_load_csvfiletype")
      finish
    endif
    let did_load_csvfiletype=1

    augroup filetypedetect
      au! BufRead,BufNewFile *.csv,*.dat	setfiletype csv
    augroup END
<
      You save this file as "filetype.vim" in your user runtime diretory: >

        :w ~/.vim/filetype.vim
<
   3) To be able to use your new filetype.vim detection, you need to restart
      Vim. Vim will then  load the csv filetype plugin for all files whose
      names end with .csv.

==============================================================================
3. Commands							*csv-commands*

The CSV ftplugin provides several Commands:

3.1 WhatColumn                                                *WhatColumn_CSV*
--------------

If you would like to know, on which column the cursor is, use >
    :WhatColumn

Use the bang attribute, if you have a heading in the first line and you want
to know the name of the column in which the cursor is: >
    :WhatColumn!
<

3.2 NrColumns                                                 *NrColumns_CSV*
--------------

:NrColumns outputs the maximum number of columns available. It does this by
testing the first 10 lines for the number of columns. This usually should be
enough.

3.3 SearchInColumn                                          *SearchInColumn_CSV*
------------------

Use :SearchInColumn to search for a pattern within a specific column. The
usage is: >
    :SearchInColumn [<nr>] /{pat}/
<

So if you would like to search in Column 1 for the word foobar, you enter >

    :SearchInColumn 1 /foobar/

Instead of / as delimiter, you can use any other delimiter you like. If you
don't enter a column, the current column will be used.

3.4 HiColumn                                                    *HiColumn_CSV*
------------

:HiColumn <nr> can be used to highlight Column <nr>. Currently the plugin uses
the WildMenu Highlight Group. If you would like to change this, you need to
define the variable |g:csv_hiGroup|.

If you do not specify a <nr>, HiColumn will highlight the column on which the
cursor is. Use >

    :HiColumn!

to remove any highlighting.

If you want to automatically highlight a column, see |csv-hicol|

                                                          *:ArrangeColumn*
3.5 ArrangeColumn                                          *ArrangeColumn_CSV*
-----------------

If you would like all columns to be visually arranged, you can use the >
    :ArrangeColumn[!]

command. Beware, that this will change your file and depending on the size of
your file may slow down Vim significantly. This is highly experimental.
:ArrangeCommand will try to vertically align all columns by their maximum
column size.

Use the bang attribute to force recalculating the column width. This is
slower, but especially if you have modified the file, this will correctly
calculate the width of each column so that they can be correctly aligned. If
no column width has been calculated before, the width will be calculated, even
if the '!' has not been given.

Note, this can be very slow on large files or many columns (see
|csv-slow| on how to increase performance for this command). To prevent you
from accidently changing your csv file, the buffer will be set 'readonly'
afterwards. Note: this command does not work for fixed width columns
|csv-fixedwidth|

3.6 UnArrangeColumn                                    *UnArrangeColumn_CSV*
-----------------

If you would like to undo a previous :ArrangeColumn command, you can use this
command: >

    :UnArrangeColumn

Beware, that is no exact undo of the :ArrangeColumn command, since it strips
away all leading blanks for each column. So if previously a column contained
only some blanks, this command will strip all blanks.

3.7 DeleteColumn                                           *DeleteColumn_CSV*
----------------

The command :DeleteColumn can be used to delete a specific column. >

    :DeleteColumn 2

will delete column 2.

If you don't specify a column number, it will delete the column on which the
cursor is.

3.8 InitCSV							*InitCSV*
-----------

Reinitialize the Plugin. Use this, if you have changed the configuration
of the plugin (see |csv-configuration| ).

3.9 Header lines						 *Header_CSV*
----------------

This command splits the csv-buffer and adds a window, that holds a small
fraction of the csv file. This is useful, if the first line contains some kind
of a heading and you want always to display it. This works similar to fixing a
certain line at the top. As optional argument, you can give the number of
columns from the top, that shall be displayed. By default, 1 is used (You can
define youre own default by setting the b:csv_headerline variable, see
|csv-header|). Use the '!' to close this window. So this >

    :Header 3

opens at the top a split window, that holds the first 3 lines, is fixed
and horizontally 'scrollbind'ed to the csv window and highlighted using the
CSVHeaderLine highlighting.
To close the header window, use >

    :Header!

Note, this won't work with linebreaks in the column.

Note also, that if you already have a horizontal header window (|VHeader_CSV|),
this command will close the horizontal Header window. This is because of a
limitation of Vim itsself, which doesn't allow to sync the scrolling between
two windows horizontally and at the same time have another window only sync
its scrolling vertically.

Note: this command does not work for fixed width columns |csv-fixedwidth|

							    *VHeader_CSV*

If you want a vertical header line, use :VHeader. This works similar to the
|Header_CSV| command, except that it will open a vertical split window with
the first column always visible. It will always open the first column in the
new split window. Use the '!' to close the window.

Note, this won't work with linebreaks in the column.
Note also: this command does not work for fixed width columns |csv-fixedwidth|


					*VHeaderToggle_CSV* *HeaderToggle_CSV*

Use the :HeaderToggle and :VHeaderToggle command to toggle displaying the
horizontal or vertical header line.


3.10 Sort							*Sort_CSV*
--------

This command can be used to sort the csv file on a certain column. If no range
is given, is sorts the whole file. Specify the column number to sort on as
argument. Use the '!' attribute to reverse the sort order. For example, the
following command sorts line 1 til 10 on the 3 column >

    :1,10Sort 3

While this command >

    :1,10Sort! 3

reverses the order based on column 3.

Instead of a column, you can give the flag 'n' to have it sort numerically.
When no column number is given, it will sort by the column, on which the
cursor is currently.

3.11 Copy Column        					 *Copy_CSV*
----------------

If you need to copy a specific column, you can use the command :Column >

    :[N]Column [a]

Copy column N into register a. If you don't specify N, the column of the
current cursor position is used. If no register is given, the default register
|quotequote| is used. 


3.12 Move A Column        					 *MoveCol_CSV*
------------------

You can move one column behind another column by using the :MoveColumn
command >

    :[range]MoveColumn [source] [dest]

This moves the column number source behind column nr destination.
If both arguments are not given, move the column on which the cursor is behind
the last column. If [range] is not given, MoveColumn moves all columns,
otherwise, it moves the columns only for the lines within the range, e.g.
given that your first line is a header line, which you don't want to change >

    :2,$MoveColumn 1 $

this would move column 1 behind the last column, while keeping the header line
as is.


3.13 Sum of a Column        					 *SumCol_CSV*
--------------------

You can let Vim output the sum of a column using the :SumCol command >

    :[range]SumCol [nr]

This outputs the result of the column <nr> within the range given. If no range
is given, this will calculate the sum of the whole column. If <nr> is not
given, this calculate the sum for the column the cursor is on. See also
|csv-aggregate-functions|


3.14 Normal mode commands					 *csv-mapping*
-------------------------

The csv filetype plugin redefines the following keys as:

<C-Right> or L or W	Move [count] field forwards

<C-Left> or E or H	Move [count] field backwards

<Up> or K		Move [count] lines upwards within the same column

<Down> or J		Move [count] lines downwards within the same column

<Enter>                 Dynamically fold all lines away, that don't match
                        the value in the current column. See |csv-filter|

<BS>                    Remove last item from the dynamic filter.
                        See |csv-filter|

Note, that the <BS>, <CR>, K and J overlap Vim's default mapping for |<CR>|,
|<BS>|, |J| and |K| respectively. Therefore, this functionality has been
mapped to a sane default of <Localleader>J and <LocalLeader>K. If you haven't
changed the |<Leader>| or |<LocalLeader>| variables, those the <Localleader>
is equival to a single backslash '\', e.g. \K would run the lookup function on
the word under the cursor and \J would join this line with the previous line.


                                                            *ConvertData_CSV*
3.15 Converting a CSV File					 *csv-convert*
--------------------------

You can convert your CSV file to a different format with the command >

    ConvertData

Use the the ! attribute, to convert your data without the delimiter.

This command will interactively ask you for the definition of 3 variables.
After which it will convert your csv file into a new format, defined by those
3 variables and open the newly created file in a new window. Those 3 variables
define how the text converted.

First, You need to define what has to be done, before converting your column
data. That is done with the "pre convert" variable. The content of this
variable will be put in front of the new document.

Second, you define, what has to be put after the converted content of your
column data. This happens with the "post convert" variable. Basically the
contents of this variable will be put after processing the columns.

Last, the columns need to be converted into your format. For this you can
specify a printf() format like string, that defines how your data will be
converted. You can use '%s' to specify placeholders, which will later be
replaced by the content of the actual column. 

For example, suppose you want to convert your data into HTML, then you first
call the >

    ConvertData

At this point, Vim will ask you for input. First, you need to specify, what
needs to be done before processing the data:

Pre convert text: <html><body><table> `

This would specify to put the HTML Header before the actual data can be
processed. If the variable g:csv_pre_convert is already defined, Vim will
already show you its' content as default value. Simply pressing Enter will use
this data. After that, Vim asks, what the end of the converted file needs to
look like:

Post convert text: </table></body></html>

So here you are defining how to finish up the HTML file. If the variable
g:csv_post_convert is already defined, Vim will already show you its' content
as default value which you can confirm by pressing Enter. Last, you define,
how your columns need to be converted. Again, Vim asks you for how to do that:

Converted text, use %s for column input:
<tr><td>%s</td><td>%s</td><td>%s</td></tr>

This time, you can use '%s' expandos. They tell Vim, that they need to be
replaced by the actual content of your file. It does by going from the first
column in your file and replacing it with the corresponding %s in that order.
If there are less '%s' expandos then columns in your file, Vim will skip the
columns, that are not used. Again If the variable g:csv__convert is already
defined, Vim will already show you its' content as default value which you can
confirm by pressing Enter. 

After you hit Enter, Vim will convert your data and put it into a new window.
It may look like this:

    <html><body><table> `
    <tr><td>1,</td><td>2,</td><td>3,</td></tr> `
    <tr><td>2,</td><td>2,</td><td>4,</td></tr> `
    </table></body></html> `

Note, this is only a proof of concept. A better version of converting your
data to HTML is bundled with Vim (|:TOhtml|).

But may be you want your data converted into SQL-insert statements. That could
be done like this: >
    
    ConvertData!
    Pre convert text: `

(Leave this empty. It won't be used).

    Post convert text: Commit;`

After inserting the data, commit it into the database.

    Converted text, use %s for column input: `
    Insert into table foobar values ('%s', '%s', %s); `

Note, that the last argument is not included within single quotation marks,
since in this case the data is assumed to be integer and won't need to be
quoted for the database.

After hitting Enter, a new Window will be opened, which might look like this:

Insert into table foobar values('Foobar', '2', 2011);
Insert into table foobar values('Bar', '1', 2011);
...
Commit;           

Since the command was used with the bang attribute (!), the converted data
doesn't include the column delimiters.

Now you can copy it into your database, or further manipulate it.

3.16 Dynamic filters      					 *csv-filter*
--------------------      

If you are one a value and only want to see lines, that have the same value in
this column, you can dynamically filter the file and fold away all lines not
matching the value in the current column. To do so, simply press <CR> (Enter).
Now Vim will fold away all lines, that don't have the same value in this
particular row. Note, that leading blanks and the delimiter is removed and the
value is used literally when comparing with other values.

The way this is done is, that the value from the column is extracted and a
regular expression for that column is generated from it. In the end this
regular expression is put into the search register (|quote_/|) and used for
folding the file.

A subsequent <CR> on another value, will add this value to the current applied
filter (this is like using the logical AND between the currently active filter
and the new value). To remove the last item from the filter, press <BS>
(backspace). If all items from the filter are removed, folding will be
disabled.

If some command messes up the folding, you can use |zX| to have the folding
being reinitialized. (If in the meantime you searched for something, the folds
will be updated to only fold away non-matching lines.)

By default, the first line is assumed to be the header and won't be folded
away. See also |csv-header|.

                                                         *:Filter* *Filter_CSV*
To see the active filters, you can use the :Filter command. This will show you
a small summary, of what filters are active and looks like this: 

Nr      Col           Name              Value `
============================================= `
01      07          Price;              23.10 `

This means, there is only one filter active. The current active filter is on
column 7 (column name is Price) and the active value for the filter is 23.10.
When removing one item from the filter by pressing <BS>, it will always remove
the last item (highest number in NR column) from the active filter values.

Note, that depending on your csv file and the number of filters you used,
applying the filter might actually slow down vim, because a complex regular
expression is generated that is applied by the fold expression. Look into the
@/ (|quote_/|) register to see its value.

                                                                *Analyze_CSV*
3.17 Analyze a Column       					 *csv-analyze*
---------------------

If you'd like to know, how the values are distributed among a certain column,
you can use the :Analyze command. So >

    :Analyze 3

outputs the the distribution of the top 5 values in column 3. This looks like
this: 

Nr      Count    %      Value `
============================= `
01      20      50%     10    `
02      10      25%     2     `
03      10      25%     5     `

This tells you, that the the value '10' in column 3 occurs 50% of the time
(exactly 20 times) and the other 2 values '2' and '5' occur only 10 times, so
25% of the time.

                                                                *VertFold_CSV*
3.18 Vertical Folding       					 *csv-vertfold*
---------------------

Sometimes, you want to hide away certain columns to better view only certain
columns without having to horizontally scroll. You can use the :VertFold
command to hide certain columns: >

    :VertFold <nr>
<
This will hide all columns from the first until the number entered. It
currently can't hide single columns, because of the way, syntax highlighting
is used. This command uses the conceal-feature |:syn-conceal| to hide away
those columns.

Use >
    :VertFold!

to display all hidden columns again.

==============================================================================
4. CSV Configuration					 *csv-configuration*

The CSV plugin tries to automatically detect the field delimiter for your
file, cause although often the file is called CSV (comma separated values), a
semicolon is actually used. The column separator is stored in the buffer-local
variable b:delimiter. This delimiter is heavily used, because you need
it to define a column. Almost all commands use this variable therefore.

4.1 Delimiter							*csv-delimiter*
-------------

To override the automatic detection of the plugin and define the separator
manually, use: >

    :let g:csv_delim=','

to let the comma be the delimiter. This sets the buffer local delimiter
variable b:delimiter.

If your file does not consist of delimited columns, but rather is a fixed
width csv file, see |csv-fixedwidth| for configuring the plugin appropriately.

If you changed the delimiter, you should reinitiliaze the plugin using
|InitCSV|

4.2 Column							*csv-column*
----------

The definition, of what a column is, is defined as buffer-local variable
b:col. By default this variable is initialized to: >

    let b:col='\%(\%([^' . b:delimiter . ']*"[^"]*"[^' . b:delimiter . ']*'
    \. b:delimiter . '\)\|\%([^' . b:delimiter . ']*\%(' . b:delimiter 
    \. '\|$\)\)\)'

This should take care of quoted delimiters within a column. Those should
obviously not count as a delimiter. This regular expression is quite
complex and might not always work on some complex cases (e.g. linebreaks
within a field, see RFC4180 for some ugly cases that will probably not work
with this plugin).

If you changed the b:delimiter variable, you need to redefine the b:col
variable, cause otherwise it will not reflect the change. To change the
variable from the comma to a semicolon, you could call in your CSV-Buffer
this command: >

    :let b:col=substitute(b:col, ',', ';', 'g')

Check with :echo b:col, if the definition is correct afterwards.

You can also force the plugin to use your own defined regular expression as
column. That regular expression should include the delimiter for the columns. 
To define your own regular expression, set the g:csv_col variable: >

    let g:csv_col='[^,]*,'

This defines a column as a field delimited by the comma (where no comma can be
contained inside a field), similar to how |csv-strict| works.

You should reinitialize the plugin afterwards |InitCSV|

4.3 Highlighting Group                                         *csv-higroup*
----------------------

By default the csv ftplugin uses the WildMenu highlighting Group to define how
the |HiColumn| command highlights columns. If you would like to define a
different highlighting group, you need to set this via the g:csv_hiGroup
variable. You can e.g. define it in your |.vimrc|: >

    :let g:csv_hiGroup = "IncSearch"

You need to restart Vim, if you have changed this variable or use |InitCSV|

The |hl-Title| highlighting is used for the Header line that is created by the
|Header_CSV| command. If you prefer a different highlighting, set the
g:csv_hiHeader variable to the prefered highlighting: >

    let g:csv_hiHeader = 'Pmenu'
<
This would set the header window to the |hl-Pmenu| highlighting, that is used
for the popup menu. To disable the custom highlighting, simply |unlet| the
variable: >

    unlet g:csv_hiHeader

You should reinitialize the plugin afterwards |InitCSV|

4.4 Strict Columns						*csv-strict*
------------------

The default regular expression to define a column is quite complex
|csv-column|. This slows down the processing and makes Vim use more memory and
it could still not fit to your specific use case.

If you know, that in your data file, the delimiter cannot be contained inside
the fields quoted or escaped, you can speed up processing (this is quite
noticeable when using the |ArrangeColumn_CSV| command) by setting the
g:csv_strict_columns variable: >

    let g:csv_strict_columns = 1

This would define a column as this regex: >

    let b:col = '\%([^' . b:delimiter . ']*' . b:delimiter . '\|$\)'

Much simpler then the default column definition, isn't it?
See also |csv-column| and |csv-delimiter|

You can disable the effect if you |unlet| the variable: >

    unlet g:csv_strict_columns

You should reinitialize the plugin afterwards |InitCSV|

4.5 Multibyte Chars						*csv-mbyte*
-------------------

Unfortunately, when using the |ArrangeColumn_CSV| command, multibyte chars
make some trouble, because internally Vim uses bytes to specify the width of a
column. The CSV plugin tries to workaround that, by calculating the byte width
of each column, before aligning them. This is quite expensive and can slow
down processing. If you know, your data file only contains pure ASCII chars
(or you simply don't care, if some lines a off a little bit), set the
g:csv_no_multibyte variable: >

    let g:csv_no_multibyte = 1

And to force calculating the byte width of each column |unlet| the variable: >

    unlet g:csv_no_multibyte

You should reinitialize the plugin afterwards |InitCSV|

4.6 Concealing					*csv-syntax*	*csv-conceal*
-------------------

The CSV plugin comes with a function to syntax highlight csv files. Basically
allt it does is highlight the columns and the header line.

By default, the delimiter will not be displayed, if Vim supports |conceal| of
syntax items and instead draws a vertical line. If you don't want that, simply
set the g:csv_noconceal variable in your .vimrc >
    
    let g:csv_no_conceal = 1

and to disable it, simply unlet the variable >

    unlet g:csv_no_conceal

You should reinitialize the plugin afterwards |InitCSV|
Note: You can also set the 'conceallevel' option to control how the concealed
chars will be displayed.

If you want to customize the syntax colors, you can define your own groups.
The CSV plugin will use already defined highlighting groups, if they are
already defined, otherwise it will define its own defaults which should be
visible with 8, 16, 88 and 256 color terminals. For that it uses the
CSVColumnHeaderOdd and CSVColumnHeaderEven highlight groups for syntax
coloring the first line. All other lines get either the CSVColumnOdd or
CSVColumnEven highlighting.

In case you want to define your own highlighting groups, you can define your
own syntax highlighting like this in your |.vimrc| >

    hi CSVColumnEven term=bold ctermbg=4 guibg=DarkBlue
    hi CSVColumnOdd  term=bold ctermbg=5 guibg=DarkMagenta
    hi CSVColumnHeaderEven ...
    hi CSVColumnHeaderOdd ...

<
Note, these changes won't take effect, until you restart Vim.


4.7 Newlines						*csv-newline*
------------

RFC4180 allows newlines in double quoted strings. By default, the csv-plugin
won't recognize newlines inside fields. It is however possible to make the
plugin aware of newlines within quoted strings. To enbale this, set >

    let g:csv_nl = 1

and to disable it again, simply unset the variable >

    unlet g:csv_nl

It is a good idea to reinitialize the plugin afterwards |InitCSV|

Note, this might not work correctly in all cases. The syntax highlighting
seems to change on cursor movements. This could possibly be a bug in the
syntax highlighting engine of Vim. Also, |WhatColumn_CSV| can't handle
newlines inside fields and will most certainly be wrong. 

4.8 Highlight column automatically				*csv-hicol* 
----------------------------------

You can let vim automatically highlight the column on which the cursor is.
This works by defining an |CursorMoved| autocommand to always highlight the
column, when the cursor is moved in normal mode. Note, this does not update
the highlighting, if the Cursor is moved in Insert mode. To enable this, 
define the g:csv_highlight_column variable like this >

    let g:csv_highlight_column = 'y'

and to disable it again, simply unset the variable >

    unlet g:csv_highlight_column

It is a good idea to reinitialize the plugin afterwards |InitCSV|

4.9 Fixed width columns                                        *csv-fixedwidth*
-----------------------

Sometimes there are no real columns, but rather the file is fixed width with
no distinct delimiters between each column. The CSV plugin allows you to
handle such virtual columns like csv columns, if you define where each column
starts.

Note: Except for |ArrangeColumn_CSV| and the |Header_CSV| commands, all
commands work in either mode. Those two commands won't do anything in the case
of fixedwidth columns, since they don't really make sense here.

4.9.1 Manual setup
------------------
You can do this, by setting the buffer-local variable
b:csv_fixed_width like this >

    let b:csv_fixed_width="1,5,9,13,17,21"

This defines that each column starts at multiples of 4. Be sure, to issue
this command in the buffer, that contains your file, otherwise, it won't
have an effect, since this is a buffer-local option (|local-option|)

After setting this variable, you should reinitialize the plugins using
|InitCSV|

                                                                    *CSVFixed*
4.9.2 Setup using a Wizard
--------------------------
Alternatively, you can setup the fixed width columns using the :CSVFixed
command. This provides a simple wizard to select each column. If you enter
the command: >
    :CSVFixed 
<
The first column will be highlighted and Vim outputs:
<Cursor>, <Space>, <ESC>, <BS>, <CR>...
This means, you can now use those 5 keys to configure the fixed-width columns:

   <Cursor> Use Cursor Left (<Left>) and Cursor Right (<Right>) to move the
            highlighting bar.
   <Space>  If you press <Space>, this column will be fixed and remain
            highlighted and there will be another bar, you can move using
            the Cursor keys. This means this column will be considered to be
            the border between 2 fixed with columns.
   <ESC>    Abort
   <BS>     Press the backspace key, to remove the last column you fixed with
            the <Space> key.
   <CR>     Use Enter to finish the wizard. This will use all fixed columns
            to define the fixed width columns of your csv file. The plugin
            will be initialized and syntax highlighting should appear.

Note: This only works, if your Vim has the 'colorcolumn' option available
(This won't work with Vim < 7.3 and also not with a Vim without +syntax
feature).


4.10 CSV Header lines                                        *csv-header*
---------------------

By default, dynamic filtering |csv-filter| will not fold away the first line.
If you don't like that, you can define your header line using the variable
b:csv_fold_headerline, e.g. >
    
    let b:csv_headerline = 0

to disable, that a header line won't be folded away. If your header line
instead is on line 5, simply set this variable to 5. This also applies to the
|Header_CSV| command.

==============================================================================
5. CSV Tips and Tricks						*csv-tips*

Here, there you'll find some small tips and tricks that might help when
working with CSV files.

5.1 Statusline							*csv-stl*
--------------
Suppose you want to include the column, on which the cursor is, into your
statusline. You can do this, by defining in your .vimrc the 'statusline' like
this: >

    function MySTL()
	if has("statusline")
	    hi User1 term=standout ctermfg=0 ctermbg=11 guifg=Black guibg=Yellow
	    let stl = ...
	    if exists("*CSV_WCol")
		let csv = '%1*%{&ft=~"csv" ? CSV_WCol() : ""}%*'
	    else
		let csv = ''
	    endif
	    return stl.csv
	endif
    endfunc
    set stl=%!MySTL()
<

This will draw in your statusline right aligned the current column and max
column (like 1/10), if you are inside a CSV file. The column info will be
drawn using the User1 highlighting (|hl-User1|), that has been defined in the
second line of the function. In the third line of your function, put your
desired 'statusline' settings as |expression|. Note the section starting with
'if exists(..)' guards against not having loaded the filetype plugin.

                                                                 *CSV_WCol*

The CSV_WCol() function controls, what will be outputed. In the simplest case,
when no argument is given, it simply returns on which column the cursor is.
This would look like '1/10' which means the cursor is on the first of 10
columns. If you rather like to know the name of the column, simply give as
parameter to the function the string "Name". This will return the column name
as it is printed on the first line of that column. This can be adjusted, to
have the column name printed into the statusline (see |csv-stl| above) by
replacing the line >

    let csv = '%1*%{&ft=~"csv" ? CSV_WCol() : ""}%*'
<
by e.g.

    let csv = '%1*%{&ft=~"csv" ? CSV_WCol("Name") . " " . CSV_WCol() : ""}%*'

which will output "Name 2/10" if the cursor is in the second column
which is named "Name".

5.2 Slow ArrangeCol						*csv-slow*
-------------------

Processing a csv file using |ArrangeColumn_CSV| can be quite slow, because Vim
needs to calculate the width for each column and then replace each column by
itself widened by spaces to the optimal length. Unfortunately, csv files tend
to be quite big. Remember, for a file with 10,000 lines and 50 columns Vim
needs to process each cell, which accumulates to 500,000 substitutions. It
might take some time, until Vim is finished.

You can speed up things a little bit, if you omit the '!' attribute to the
|ArrangeColumn| (but this will only work, if the width has been calculated
before, e.g. by issuing a :1ArrangeColumn command to arrange only the first
line. Additionally you can also configure how this command behaves by setting
some configuration variables.

Here are some performance meassurements on how the various
configuration settings influence the |ArrangeColumn_CSV| command on a file
with 34 columns (on a 2.2GHz Core2Duo processor):

    Lines   |   default |   strict¹     |   multibyte²  | strict + multibyte³
    ------------------------------------------------------------------------
     1000   |    6.93 s |     5.53 s    |     6.76 s    |   5.66 s
     5000   |   15.2  s |     8.52 s    |    14.27 s    |   8.56 s
    10000   |   36.2  s |    24.67 s    |    36.11 s    |  24.26 s
    50000   |  162,23 s |    93.36 s    |   152.25 s    | 141.18 s

¹ setting the g:csv_strict_columns variable (|csv-strict|)
² setting the g:csv_no_multibyte variable (|csv-mbyte|)
³ setting the g:csv_no_multibyte variable and g:csv_strict_columns variable

Note, this was performed on a quite fast processor. If you need to work with
large files, be sure to have enough memory available, cause Vim needs to read
in the whole file into memory. You can also try the LargeFile plugin available
at http://www.vim.org/scripts/script.php?script_id=1506 which tunes several
Vim options (like |syn-off|, 'undolimits', 'fdm' and others).



5.3 Defining custom aggregate functions		    *csv-aggregate-functions*
---------------------------------------

The CSV plugin already defines the |SumCol_CSV|command, to let you calculate
the sum of all values of a certain column within a given range. But it may be,
that you don't need the sum, but would rather want to have the average of all
values within a certain column. You can define your own function and let the
plugin call it for a column like this:

    1) You define your own custom function in the after directory of your
       vimruntime path |after-directory| (see also #2 below) >

	fun! My_CSV_Average(col)
	    let sum=0
	    for item in a:col
		let sum+=item
	    endfor
	    return sum/len(a:col)
	endfun
<
       This function takes a list as argument, and calculates the average for
       all items in the list. You could also make use of Vim's |eval()|
       function and write your own Product function like this >

	fun! My_CSV_Product(col)
	    return eval(join(a:col, '*'))
	endfun
<

    2) Now define your own custom command, that calls your custom function for
    a certain column >
       
	    command! -buffer -nargs=? -range=% AvgCol
	    \ :echo csv#EvalColumn(<q-args>,
	    \ "My_CSV_Average", <line1>,<line2>)
<
        This command should best be put into a file called csv.vim and put
	into your ~/.vim/after/ftplugin/ directory. Create directories that
	don't exist yet. For Windows, this would be the
	$VIMRUNTIME/vimfiles/after/ftplugin directory.

    3) Make sure, your |.vimrc| includes a filetype plugin setting like this >

	filetype plugin on
<
       This should make sure, that all the necessary scripts are loaded by
       Vim.

    After restarting Vim, you can now use your custom command definition
    :AvgCol. Use a range, for the number of lines you want to evaluate and
    optionally use an argument to specify which column you want to be
    evaluated >

	:2,$AvgCol 7
<
    This will evaluate the average of column seven (assuming, line 1 is the
    header line, which should not be taken into account).


==============================================================================
6. CSV Changelog					       *csv-changelog*

0.20 Oct 06, 2011 {{{1

- Implement a wizard for initializing fixed-width columns (|CSVFixed|)
- Vertical folding (|VertFold_CSV|)
- fix plugin indentation (by Daniel Karl, thanks!)
- fixed missing bang parameter for HiColumn function (by Daniel Karl, thanks!)

0.19 Sep 26, 2011 {{{1

- Make |:ArrangeColumn| more robust
- Link CSVDelimiter to the Conceal highlighting group for Vim's that have
  +conceal feature (suggested by John Orr, thanks!)
- allow the possibility to return the Column name in the statusline |csv-stl|
  (suggested by John Orr, thanks!)
- documentation updates
- Allow to dynamically add Filters, see |csv-filter|
- Also display what filters are active, see |:Filter|
- Analyze a column for the distribution of a value |csv-analyze|
- Implement UnArrangeColumn command |UnArrangeColumn_CSV|
  (suggested by Daniel Karl in https://github.com/chrisbra/csv.vim/issues/7)

0.18 Aug 30, 2011 {{{1

- fix small typos in documentation
- document, that 'K' and 'J' have been remapped and the originial function is
  available as \K and \J
- Delimiters should not be highlighted within a column, only when used
  as actual delimiters (suggested by Peng Yu, thanks!)
- Performance improvements for |:ArrangeColumn|

0.17 Aug 16, 2011 {{{1

- small cosmetic changes
- small documentation updates
- fold away changelog in help file
- Document, that |DeleteColumn_CSV| deletes the column on which the cursor
  is, if no column number has been specified
- Support csv fixed width columns (|csv-fixedwidth|)
- Support to interactively convert your csv file to a different
  format (|csv-convert|)

0.16 Jul 25, 2011 {{{1

- Sort on the range, specified (reported by Peng Yu, thanks!)
- |MoveCol_CSV| to move a column behind another column (suggested by Peng Yu,
  thanks!)
- Document how to use custom functions with a column
  (|csv-aggregate-functions|)
- Use g:csv_highlight_column variable, to have Vim automatically highlight the
  column on which the cursor is (|csv-hicol|)
- Header/VHeader command should work better now (|Header_CSV|, |VHeader_CSV|)
- Use setreg() for setting the register for the |Column_CSV| command and make
  sure it is blockwise.
- Release 0.14 was not correctly uploaded to vim.org

0.14 Jul 20, 2011 {{{1

- really use g:csv_no_conceal variable (reported by Antonio Ospite, thanks!)
- Force redrawing before displaying error messages in syntax script (reported
  by Antonio Ospite, thanks!)
- Make syntax highlighting work better with different terminals (Should work
  now with 8, 88 and 256 color terminals, tested with linux konsole, xterm and
  rxvt) (https://github.com/chrisbra/csv.vim/issues/4)
- Automatically detect '|' as field separator for csv files

0.13 Mar 14, 2011 {{{1

- documentation update
- https://github.com/chrisbra/csv.vim/issues#issue/2 ('splitbelow' breaks
  |Header_CSV|, fix this; thanks lespea!)
- https://github.com/chrisbra/csv.vim/issues#issue/3 ('gdefault' breaks
  |ArrangeColumn_CSV|, fix this; thanks lespea!)
- https://github.com/chrisbra/csv.vim/issues#issue/1 (make syntax highlighting
  more robust, thanks lespea!)
- fix some small annoying bugs
- WhatColumn! displays column name

0.12 Feb 24, 2011 {{{1 

- bugfix release:
- don't use |:noa| when switching between windows
- make sure, colwidth() doesn't throw an error

0.11 Feb 24, 2011 {{{1

- new command |Copy_CSV|
- |Search_CSV| did not find anything in the last column if no delimiter
  was given (reported by chroyer)
- |VHeader_CSV| display the first column as Header similar to how
  |Header_CSV| works
- |HeaderToggle_CSV| and |VHeaderToggle_CSV| commands that toggle displaying
  the header lines/columns

0.10 Feb 23, 2011 {{{1

- Only conceal real delimiters
- document g:csv_no_conceal variable
- document g:csv_nl variable
- document conceal feature and syntax highlighting
- Normal mode command <Up>/<Down> work like K/J
- More robust regular expression engine, that can also handle newlines inside
  quoted strings.
- Slightly adjusted syntax highlighting

0.9 Feb 19, 2011 {{{1

- use conceal char depending on encoding
- Map normal mode keys also for visual/select and operator pending mode

0.8 Feb 17, 2011 {{{1

- Better Error handling 
- HiColumn! removes highlighting
- Enable NrColumns, that was deactivated in v.0.7
- a ColorScheme autocommand makes sure, that the syntax highlighting is
  reapplied, after changing the colorscheme.
- SearchInColumn now searches in the current column, if no column has been
  specified
- A lot more documentation
- Syntax Highlighting conceales delimiter
- small performance improvements for |ArrangeColumn_CSV|

0.7 Feb 16, 2011 {{{1

- Make the motion commands 'W' and 'E' work more reliable
- Document how to setup filetype plugins
- Make |WhatColumn_CSV| work more reliable (report from
  http://vim.wikia.com/Script:3280)
- DeleteColumn deletes current column, if no argument given
- |ArrangeColumn_CSV| handles errors better
- Code cleanup
- Syntax highlighting
- 'H' and 'L' move forward/backwards between csv fields
- 'K' and 'J' move upwards/downwards within the same column
- |Sort_CSV| to sort on a certain column
- |csv-tips| on how to colorize the statusline

0.6 Feb 15, 2011 {{{1

- Make |ArrangeColumn_CSV| work more reliable (had problems with multibyte
  chars before)
- Add |Header_CSV| function
- 'W' and 'E' move forward/backwards between csv fields
- provide a file ftdetect/csv.vim to detect csv files

0.5  Apr 20 2010 {{{1

- documentation update
- switched to a public repository: http://github.com/chrisbra/csv.vim
- enabled GLVS (see |GLVS|)

0.4a Mar 11 2010 {{{1

- fixed documentation

0.4  Mar 11 2010 {{{1

- introduce |InitCSV|
- better Error handling
- HiColumn now by default highlights the current column, if no argument is
  specified.

0.3  Oct, 28 2010 {{{1

- initial Version

vim:tw=78:ts=8:ft=help:norl:et:fdm=marker:fdl=0
syntax/csv.vim	[[[1
163
" A simple syntax highlighting, simply alternate colors between two
" adjacent columns
" Init {{{2
scriptencoding utf8
if version < 600
    syn clear
elseif exists("b:current_syntax")
    finish
endif

" Helper functions "{{{2
fu! <sid>Warning(msg) "{{{3
    " Don't redraw, so we are not overwriting messages from the ftplugin
    " script
    "redraw!
    echohl WarningMsg
    echomsg "CSV Syntax:" . a:msg
    echohl Normal
endfu

fu! <sid>CheckSaneSearchPattern() "{{{3
    " First: 
    " Check for filetype plugin. This syntax script relies on the filetype plugin,
    " else, it won't work properly.
    redir => s:a |sil filetype | redir end
    let s:a=split(s:a, "\n")[0]
    if match(s:a, '\cplugin:off') > 0
	call <sid>Warning("No filetype support, only simple highlighting using\n" .
		    \ s:del_def . " as delimiter! See :h csv-installation")
    endif

    " Second: Check for sane defaults for the column pattern
    " Not necessary to check for fixed width columns
    if exists("b:csv_fixed_width_cols")
	return
    endif

    let s:del_def = ','
    let s:col_def = '\%([^' . s:del_def . ']*' . s:del_def . '\|$\)'

    " Try a simple highlighting, if the defaults from the ftplugin
    " don't exist
    let s:col = exists("b:col") && !empty("b:col") ? b:col
		\ : s:col_def
    let s:del = exists("b:delimiter") && !empty("b:delimiter") ? b:delimiter
		\ : s:del_def
    try
	let _p = getpos('.')
	let _s = @/ 
	exe "sil norm! /" . b:col . "\<CR>"
    catch
	" check for invalid pattern, for simplicity,
	" we catch every exception
	let s:col = s:col_def
	let s:del = s:del_def
	call <sid>Warning("Invalid column pattern, using default pattern " . s:col_def)
    finally
	let @/ = _s
	call setpos('.', _p)
    endtry
endfu

" Syntax rules {{{2
fu! <sid>DoHighlight() "{{{3
    if has("conceal") && !exists("g:csv_no_conceal") &&
		\ !exists("b:csv_fixed_width_cols")
	" old val
	    "\ '\%(.\)\@=/ms=e,me=e contained conceal cchar=' .
	    " Has a problem with the last line!
	exe "syn match CSVDelimiter /" . s:col . 
	    \ '\n\=/ms=e,me=e contained conceal cchar=' .
	    \ (&enc == "utf-8" ? "│" : '|')
	"exe "syn match CSVDelimiterEOL /" . s:del . 
	"    \ '\?$/ contained conceal cchar=' .
	"    \ (&enc == "utf-8" ? "│" : '|')
	hi def link CSVDelimiter Conceal
	hi def link CSVDelimiterEOL Conceal
    elseif !exists("b:csv_fixed_width_cols")
	" The \%(.\)\@<= makes sure, the last char won't be concealed,
	" if it isn't a delimiter
	"exe "syn match CSVDelimiter /" . s:col . '\%(.\)\@<=/ms=e,me=e contained'
	exe "syn match CSVDelimiter /" . s:col . '\n\=/ms=e,me=e contained'
	"exe "syn match CSVDelimiterEOL /" . s:del . '\?$/ contained'
	if has("conceal")
	    hi def link CSVDelimiter Conceal
	    hi def link CSVDelimiterEOL Conceal
	else
	    hi def link CSVDelimiter Ignore
	    hi def link CSVDelimiterEOL Ignore
	endif
    endif " There is no delimiter for csv fixed width columns


    if !exists("b:csv_fixed_width_cols")
	exe 'syn match CSVColumnEven nextgroup=CSVColumnOdd /'
		    \ . s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'
	exe 'syn match CSVColumnOdd nextgroup=CSVColumnEven /'
		    \ . s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'

	exe 'syn match CSVColumnHeaderEven nextgroup=CSVColumnHeaderOdd /\%1l'
		    \. s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'
	exe 'syn match CSVColumnHeaderOdd nextgroup=CSVColumnHeaderEven /\%1l'
		    \. s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'
    else
	for i in range(len(b:csv_fixed_width_cols))
	    let pat = '/\%' . b:csv_fixed_width_cols[i] . 'c.*' .
			\ ((i == len(b:csv_fixed_width_cols)-1) ? '/' : 
			\ '\%' . b:csv_fixed_width_cols[i+1] . 'c/')

	    let group  = "CSVColumn" . (i%2 ? "Odd"  : "Even" )
	    let ngroup = "CSVColumn" . (i%2 ? "Even" : "Odd"  )
	    exe "syn match " group pat " nextgroup=" . ngroup
	endfor
    endif
endfun

fu! <sid>DoSyntaxDefinitions() "{{{3
    syn spell toplevel

    " Not really needed
    syn case ignore

    if &t_Co < 88
	if !exists("b:csv_fixed_width_cols")
	    hi default CSVColumnHeaderOdd ctermfg=DarkRed ctermbg=15
		\ guibg=grey80 guifg=black term=underline cterm=standout,bold
		\ gui=bold,underline 
	endif
	hi default CSVColumnOdd	ctermfg=DarkRed ctermbg=15 guibg=grey80
		\ guifg=black term=underline cterm=bold gui=underline
    else
	if !exists("b:csv_fixed_width_cols")
	    hi default CSVColumnHeaderOdd ctermfg=darkblue ctermbg=white
		\ guibg=grey80 guifg=black cterm=standout,underline
		\ gui=bold,underline
	endif
	hi default CSVColumnOdd ctermfg=darkblue ctermbg=white guibg=grey80
		\ guifg=black cterm=reverse,underline gui=underline
    endif
	
    " ctermbg=8 should be safe, even in 8 color terms
    if !exists("b:csv_fixed_width_cols")
	hi default CSVColumnHeaderEven ctermfg=white ctermbg=darkgrey
		\ guibg=grey50 guifg=black term=bold cterm=standout,underline
		\ gui=bold,underline 
    endif
    hi default CSVColumnEven ctermfg=white ctermbg=darkgrey guibg=grey50
		\ guifg=black term=bold cterm=underline gui=bold,underline 
endfun

" Main: {{{2 
" Make sure, we are using a sane, valid pattern for syntax
" highlighting
call <sid>CheckSaneSearchPattern()

" Define all necessary syntax groups
call <sid>DoSyntaxDefinitions()

" Highlight the file
call <sid>DoHighlight()

" Set the syntax variable {{{2
let b:current_syntax="csv"
ftdetect/csv.vim	[[[1
2
" Install Filetype detection for CSV files
au BufRead,BufNewFile *.csv,*.dat	set filetype=csv
