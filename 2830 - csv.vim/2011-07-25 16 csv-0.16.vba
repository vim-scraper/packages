" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/csv.vim	[[[1
733
" Filetype plugin for editing CSV files. "{{{1
" Author:  Christian Brabandt <cb@256bit.org>
" Version: 0.16
" Script:  http://www.vim.org/scripts/script.php?script_id=2830
" License: VIM License
" Last Change: Tue, 26 Jul 2011 00:03:36 +0200
" Documentation: see :help ft_csv.txt
" GetLatestVimScripts: 2830 16 :AutoInstall: csv.vim
"
" Some ideas are take from the wiki http://vim.wikia.com/wiki/VimTip667
" though, implementation differs.

" Plugin folclore "{{{2
if v:version < 700 || exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

" Function definitions: "{{{2
fu! <SID>Warn(mess) "{{{3
    echohl WarningMsg
    echomsg "CSV: " . a:mess
    echohl Normal
endfu

fu! <SID>Init() "{{{3
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

    if empty(b:delimiter)
	call <SID>Warn("No delimiter found. See :h csv-delimiter to set it manually!")
    endif
    
    let s:del='\%(' . b:delimiter . '\|$\)'
    " Pattern for matching a single column
    if !exists("g:csv_strict_columns") && !exists("g:csv_col")
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
		    \ '[^"]\|""\)*"\)' . s:del . 
		    \	'\)\|\%(' . 
		    \  '[^' .  b:delimiter . ']*' . s:del . '\)\)'
    elseif !exists("g:csv_col") " g:csv_strict_columns is set
	let b:col='\%([^' . b:delimiter . ']*' . s:del . '\)'
    else
	let b:col = g:csv_col
    endif
    

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
    silent do Syntax

    " undo when setting a new filetype
    let b:undo_ftplugin = "setlocal sol< tw< wrap<"
	\ . "| unlet b:delimiter b:col"

    " CSV local settings
    setl nostartofline tw=0 nowrap
    if has("conceal")
	setl cole=2 cocu=nc
	let b:undo_ftplugin .= '|setl cole< cocu<'
    endif
endfu 

fu! <SID>SearchColumn(arg) "{{{3
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
    "let @/=<SID>GetColPat(colnr) . '*\zs' . pat . '\ze\([^' . b:delimiter . ']*' . b:delimiter .'\)\?' . <SID>GetColPat(maxcolnr-colnr-1)
    " GetColPat(nr) returns a pattern containing '\zs' if nr > 1,
    " therefore, we need to clear that flag again ;(
    " TODO:
    " Is there a better way, than running a substitute command on '\zs', may be using a flag
    " with GetColPat(zsflag, colnr)?
    if colnr > 1 && colnr < maxcolnr
	"let @/=<SID>GetColPat(colnr-1,0) . '*\zs' . pat . '\ze\([^' . b:delimiter . ']*' . b:delimiter .'\)\?' . <SID>GetColPat(maxcolnr-colnr-1,0)
	"let @/= '^' . <SID>GetColPat(colnr-1,0) . '[^' . b:delimiter . ']*\zs' . pat . '\ze[^' . b:delimiter . ']*'.b:delimiter . <SID>GetColPat(maxcolnr-colnr,0) . '$'
	"let @/= '^' . <SID>GetColPat(colnr-1,0) . b:col1 . '\?\zs' . pat . '\ze' . b:col1 .'\?' . <SID>GetColPat(maxcolnr-colnr,0) " . '$'
	let @/= '^' . <SID>GetColPat(colnr-1,0) . '\%([^' . b:delimiter .
		\ ']*\)\?\zs' . pat . '\ze' . '\%([^' . b:delimiter .']*\)\?' . 
		\ b:delimiter . <SID>GetColPat(maxcolnr-colnr,0)  . '$'
    elseif colnr == maxcolnr
	let @/= '^' . <SID>GetColPat(colnr-1,0) . '\%([^' . b:delimiter .
		\ ']*\)\?\zs' . pat . '\ze' 
    else " colnr = 1
	"let @/= '^\zs' . pat . '\ze' . substitute((<SID>GetColPat(maxcolnr - colnr)), '\\zs', '', 'g')
	"let @/= '^\zs' . b:col1 . '\?' . pat . '\ze' . b:col1 . '\?' .  <SID>GetColPat(maxcolnr,0) . '$'
	let @/= '^' . '\%([^' . b:delimiter . ']*\)\?\zs' . pat .
		\ '\ze\%([^' . b:delimiter . ']*\)\?' . b:delimiter .
		\ <SID>GetColPat(maxcolnr-1,0) . '$'
    endif
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


fu! <SID>DelColumn(colnr) "{{{3
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

    if a:colnr != '1'
	let pat= '^' . <SID>GetColPat(a:colnr-1,1) . b:col
    else
	let pat= '^' . <SID>GetColPat(a:colnr,0) 
    endif
    if &ro
       setl noro
    endif
    exe ':%s/' . escape(pat, '/') . '//'
    call setpos('.', _p)
endfu

fu! <SID>HiCol(colnr) "{{{3
    if a:colnr > <SID>MaxColumns() && a:colnr[-1:] != '!'
	call <SID>Warn("There exists no column " . a:colnr)
	return
    endif
    if a:colnr[-1:] != '!'
	if empty(a:colnr)
	   let colnr=<SID>WColumn()
	else
	   let colnr=a:colnr
	endif

	if colnr==1
	    let pat='^'. <SID>GetColPat(colnr,0)
	else
	    let pat='^'. <SID>GetColPat(colnr-1,1) . b:col
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
	if a:colnr[-1:] == '!'
	    return
	endif
	let s:matchid=matchadd(s:hiGroup, pat, 0)
    else
	if a:colnr[-1:] != '!'
	    exe ":2match " . s:hiGroup . ' /' . pat . '/'
	endif
    endif
endfu

fu! <SID>GetDelimiter() "{{{3
    let _cur = getpos('.')
    let Delim={0: ';', 1:  ',', 2: '|'}
    let temp={}
    for i in  values(Delim)
	redir => temp[i]
	    exe "silent! %s/" . i . "/&/nge"
	redir END
    endfor
    let Delim = map(temp, 'matchstr(substitute(v:val, "\n", "", ""), "^\\d\\+")')

    let result=[]
    for [key, value] in items(Delim)
	if get(result,0) < value
	    call add(result, key)
	    call add(result, value)
	endif
    endfor
    call setpos('.', _cur)
    if !empty(result)
	return result[0]
    else
	return ''
    endif
endfu

fu! <SID>WColumn(...) "{{{3
    " Return on which column the cursor is
    let _cur = getpos('.')
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
    call setpos('.',_cur)
    return ret
endfu 

fu! <SID>MaxColumns() "{{{3
    "return maximum number of columns in first 10 lines
    let l=getline(1,10)
    let fields=[]
    let result=0
    for item in l
	let temp=len(split(item, b:col.'\zs'))
	let result=(temp>result ? temp : result)
    endfor
    return result
endfu

fu! <SID>ColWidth(colnr) "{{{3
    " Return the width of a column
    if !exists("b:buffer_content")
	let b:csv_list=getline(1,'$')
	let b:buffer_content=1
    endif
    let width=20 "Fallback (wild guess)
    let list=copy(b:csv_list)
    try
	" we have a list of the first 10 rows
	" Now transform it to a list of field a:colnr
	" and then return the maximum strlen
	" That could be done in 1 line, but that would look ugly
	call map(list, 'split(v:val, b:col."\\zs")[a:colnr-1]')
	call map(list, 'substitute(v:val, ".", "x", "g")')
	call map(list, 'strlen(v:val)')
	return max(list)
    catch
        return width
    endtry
endfu

fu! <SID>ArrangeCol() range "{{{3
    "TODO: Why doesn't that work?
    " is this because of the range flag?
    "let cur=winsaveview()
    " Force recalculation of Column width
    if exists("b:col_width")
      unlet b:col_width
    endif
    " Force recalculation of column width
    if exists("b:buffer_content")
      unlet b:buffer_content
    endif

   if &ro
       " Just in case, to prevent the Warning 
       " Warning: W10: Changing read-only file
       setl noro
   endif
   exe a:firstline . ',' . a:lastline .'s/' . (b:col) .
  \ '/\=<SID>Columnize(submatch(0))/' . (&gd ? '' : 'g')
   setl ro
   "call winrestview(cur)
endfu

fu! <SID>Columnize(field) "{{{3
   if !exists("b:col_width")
	let b:col_width=[]
	let max_cols=<SID>MaxColumns()
	for i in range(1,max_cols)
	    call add(b:col_width, <SID>ColWidth(i))
	endfor
	if exists("b:csv_list")
	    " delete buffer content in variable b:csv_list,
	    " this was only necessary for calculating the max width
	    unlet b:csv_list
	endif
   endif
   " convert zero based indexed list to 1 based indexed list,
   " Default: 20 width, in case that column width isn't defined
   let width=get(b:col_width,<SID>WColumn()-1,20)
   if !exists("g:csv_no_multibyte")
       let a = split(a:field, '\zs')
       let add = eval(join(map(a, 'len(v:val)'), '+'))
       let add -= len(a)
   else
       let add = 0
   endif
   
   " Add one for the frame
   " plus additional width for multibyte chars,
   " since printf(%*s..) uses byte width!
   let width = width + add  + 1

   return printf("%*s", width ,  a:field)
endfun

fu! <SID>GetColPat(colnr, zs_flag) "{{{3
    " Return Pattern for given column
    if a:colnr > 1
	let pat=b:col . '\{' . (a:colnr) . '\}' 
    else
        let pat=b:col 
    endif
    return pat . (a:zs_flag ? '\zs' : '')
endfu

fu! <SID>SplitHeaderLine(lines, bang, hor) "{{{3
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
	    let lines = empty(a:lines) ? 1 : a:lines
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
	    unlet! b:buffer_content
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

fu! <SID>SplitHeaderToggle(hor) "{{{3
    if !exists("b:CSV_SplitWindow")
	:call <sid>SplitHeaderLine(1,0,a:hor)
    else
	:call <sid>SplitHeaderLine(1,1,a:hor)
    endif
endfu

fu! <SID>MoveCol(forward, line) "{{{3
    let colnr=<SID>WColumn()
    let maxcol=<SID>MaxColumns()
    let cpos=getpos('.')[2]
    call search(b:col, 'bc', line('.'))
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
	let pat=b:col
    else
	let pat='^'. <SID>GetColPat(colnr-1,1) . b:col
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
	let a=getpos('.')
	let a[2]+=cpos-spos
	call setpos('.', a)
    elseif line < line('.')
	call search(pat . '\%' . line . 'l', 'b', line)
	" Move to the correct screen column
	let a=getpos('.')
	let a[2]+=cpos-spos
	call setpos('.', a)
    endif
endfun

fu! <SID>SortComplete(A,L,P) "{{{3
    return join(range(1,<sid>MaxColumns()),"\n")
endfun 

fu! <SID>Sort(bang, line1, line2, colnr) range "{{{3
    let wsv=winsaveview()
    if a:colnr =~? 'n'
	let numeric = 1
    else
	let numeric = 0
    endif
    let col = (empty(a:colnr) || a:colnr !~? '\d\+') ? <sid>WColumn() : a:colnr+0
    if col != 1
	let pat= '^' . <SID>GetColPat(col-1,1) . b:col
    else
	let pat= '^' . <SID>GetColPat(col,0) 
    endif
    exe a:line1 ',' a:line2 . "sort" . (a:bang ? '!' : '') .
		\' r ' . (numeric ? 'n' : '') . ' /' . pat . '/'
    call winrestview(wsv)
endfun

fu! CSV_WCol() "{{{3
    return printf(" %d/%d", <SID>WColumn(), <SID>MaxColumns())
endfun

fu! <sid>CopyCol(reg, col) "{{{3
    " Return Specified Column into register reg
    let col = a:col == "0" ? <sid>WColumn() : a:col+0
    let mcol = <sid>MaxColumns()
    if col == '$' || col > mcol
	let col = mcol
    endif
    let a=getline(1, '$')
    call map(a, 'split(v:val, ''^'' . b:col . ''\zs'')[col-1]')
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

    " Swap lines by lines, instead of reading the whole range into memory

    for i in range(a:start, a:stop)
	let fields=split(getline(i), b:col . '\zs')

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
	echomsg "Evaluating" a:func "failed for column" col "!"
	echohl Normal
	return ''
    finally
	call winrestview(save)
    endtry
endfu


fu! <SID>CommandDefinitions() "{{{3
    if !exists(":WhatColumn") "{{{4
	command! -buffer -bang WhatColumn :echo <SID>WColumn(<bang>0)
    endif
    if !exists(":NrColumns") "{{{4
	command! -buffer NrColumns :echo <SID>MaxColumns()
    endif
    if !exists(":HiColumn") "{{{4
	command! -buffer -bang -nargs=? HiColumn :call <SID>HiCol(<q-args>.<q-bang>)
    endif
    if !exists(":SearchInColumn") "{{{4
	command! -buffer -nargs=* SearchInColumn :call <SID>SearchColumn(<q-args>)
    endif
    if !exists(":DeleteColumn") "{{{4
	command! -buffer -nargs=? DeleteColumn :call <SID>DelColumn(<q-args>)
    endif
    if !exists(":ArrangeColumn") "{{{4
	command! -buffer -range ArrangeColumn :let s:a=winsaveview()
		    \|:<line1>,<line2>call <SID>ArrangeCol()|call winrestview(s:a)
    endif
    if !exists(":InitCSV") "{{{4
	command! -buffer InitCSV :call <SID>Init()
    endif
    if !exists(":Header") "{{{4
	command! -buffer -bang -nargs=? Header :call <SID>SplitHeaderLine(<q-args>,<bang>0,1)
    endif
    if !exists(":VHeader") "{{{4
	command! -buffer -bang -nargs=? VHeader :call <SID>SplitHeaderLine(<q-args>,<bang>0,0)
    endif
    if !exists(":HeaderToggle") "{{{4
	command! -buffer HeaderToggle :call <SID>SplitHeaderToggle(1)
    endif
    if !exists(":VHeaderToggle") "{{{4
	command! -buffer VHeaderToggle :call <SID>SplitHeaderToggle(0)
    endif
    if !exists(":Sort") "{{{4
	command! -buffer -nargs=* -bang -range=% -complete=custom,
		    \<SID>SortComplete Sort :call <SID>Sort(<bang>0,<line1>,<line2>,<q-args>)
    endif
    if !exists(":Column") "{{{4
	command! -buffer -count -register Column :call <SID>CopyCol(
		    \ empty(<q-reg>) ? '"' : <q-reg>,<q-count>)
    endif
    if !exists(":MoveCol") "{{{4
	command! -buffer -range=% -nargs=* -complete=custom,<SID>SortComplete
		    \ MoveCol :call <SID>MoveColumn(<line1>,<line2>,<f-args>)
    endif
    if !exists(":SumCol") "{{{4
	command! -buffer -nargs=? -range=% -complete=custom,<SID>SortComplete
		    \ SumCol :echo csv#EvalColumn(<q-args>, "<sid>SumColumn", <line1>,<line2>)
    endif
endfu

fu! <SID>CSVMappings() "{{{3
    noremap <silent> <buffer> W :<C-U>call <SID>MoveCol(1, line('.'))<CR>
    noremap <silent> <buffer> E :<C-U>call <SID>MoveCol(-1, line('.'))<CR>
    noremap <silent> <buffer> K :<C-U>call <SID>MoveCol(0, line('.')-v:count1)<CR>
    noremap <silent> <buffer> J :<C-U>call <SID>MoveCol(0, line('.')+v:count1)<CR>
    " Map C-Right and C-Left as alternative to W and E
    map <silent> <buffer> <C-Right> W
    map <silent> <buffer> <C-Left>  E
    map <silent> <buffer> H E
    map <silent> <buffer> L W
    map <silent> <buffer> <Up> K
    map <silent> <buffer> <Down> J
endfu


" end function definition "}}}2
" Initialize Plugin "{{{2
call <SID>Init()
let &cpo = s:cpo_save
unlet s:cpo_save

" Vim Modeline " {{{2
" vim: set foldmethod=marker: 
doc/ft-csv.txt	[[[1
739
*ft-csv.txt*	For Vim version 7.3	Last change: 2011 Feb 17

Author:		Christian Brabandt <cb@256bit.org>
Version:	0.16
Homepage:	http://www.vim.org/scripts/script.php?script_id=2830

The VIM LICENSE applies to the CSV filetype plugin (see |copyright|).
NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.

This is a filetype plugin for CSV files. It was heavily influenced by
the Vim Wiki Tip667 (http://vim.wikia.com/wiki/VimTip667), though it
works differently. For instructions on installing this file, type
:help add-local-help |add-local-help| inside Vim. For a screenshot, of
how the plugin can be used, see http://www.256bit.org/~chrisbra/csv.gif

                                                           *csv-toc*
1. Installation.................................|csv-installation|
2. CSV Commands.................................|csv-commands|
    2.1 WhatColumn..............................|WhatColumn_CSV|
    2.2 NrColumns...............................|NrColumns_CSV|
    2.3 SearchInColumn..........................|SearchInColumn_CSV|
    2.4 HiColumn................................|HiColumn_CSV|
    2.5 ArrangeColumn...........................|ArrangeColumn_CSV|
    2.6 DeleteColumn............................|DeleteColumn_CSV|
    2.7 InitCSV.................................|InitCSV|
    2.8 Header..................................|Header_CSV|
    2.9 Sort....................................|Sort_CSV|
    2.10 CopyColumn.............................|Copy_CSV|
    2.11 MoveCol................................|MoveCol_CSV|
    2.12 Sum of a column........................|SumCol_CSV|
    2.13 Normal mode commands...................|csv-motion|
3. CSV Filetype configuration...................|csv-configuration|
    3.1 Delimiter...............................|csv-delimiter|
    3.2 Column..................................|csv-column|
    3.3 HiGroup.................................|csv-higroup|
    3.4 Strict Columns..........................|csv-strict|
    3.5 Multibyte Chars.........................|csv-mbyte|
    3.6 Concealing..............................|csv-conceal|
    3.7 Newlines................................|csv-newline|
    3.8 Highlight column automatically..........|csv-hicol|
4. CSV Tips and Tricks..........................|csv-tips|
    4.1 Statusline..............................|csv-stl|
    4.2 Slow ArrangeCol.........................|csv-slow|
5. CSV Changelog................................|csv-changelog|

==============================================================================
1. Installation						*csv-installation*

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
2. Commands							*csv-commands*

The CSV ftplugin provides several Commands:

2.1 WhatColumn                                                *WhatColumn_CSV*
--------------

If you would like to know, on which column the cursor is, use >
    :WhatColumn

Use the bang attribute, if you have a heading in the first line and you want
to know the name of the column in which the cursor is: >
    :WhatColumn!
<

2.2 NrColumns                                                 *NrColumns_CSV*
--------------

:NrColumns outputs the maximum number of columns available. It does this by
testing the first 10 lines for the number of columns. This usually should be
enough.

2.3 SearchInColumn                                          *SearchInColumn_CSV*
------------------

Use :SearchInColumn to search for a pattern within a specific column. The
usage is: >
    :SearchInColumn [<nr>] /{pat}/
<

So if you would like to search in Column 1 for the word foobar, you enter >

    :SearchInColumn 1 /foobar/

Instead of / as delimiter, you can use any other delimiter you like. If you
don't enter a column, the current column will be used.

2.4 HiColumn                                                    *HiColumn_CSV*
------------

:HiColumn <nr> can be used to highlight Column <nr>. Currently the plugin uses
the WildMenu Highlight Group. If you would like to change this, you need to
define the variable |g:csv_hiGroup|.
If you do not specify a <nr>, HiColumn will hilight the column on which the
cursor is. Use >
    :HiColumn!
to remove any highlighting.

If you want to automatically highlight a column, see |csv-hicol|

2.5 ArrangeColumn                                          *ArrangeColumn_CSV*
-----------------

If you would like all columns to be visually arranged, you can use the >
    :ArrangeColumn

command. Beware, that this will change your file and depending
on the size of your file may slow down Vim significantly. This is highly
experimental.
:ArrangeCommand will try to vertically align all columns by their maximum
column size. Note, this can be very slow on large files or many columns (see
|csv-slow| on how to increase performance for this command).
To prevent you from accidently changing your csv file, the buffer will be set
'readonly' afterwards.

2.6 DeleteColumn                                           *DeleteColumn_CSV*
----------------

The command :DeleteColumn can be used to delete a specific column. >

    :DeleteColumn 2

will delete column 2.

2.7 InitCSV							*InitCSV*
-----------

Reinitialize the Plugin. Use this, if you have changed the configuration
of the plugin (see |csv-configuration| ).

2.8 Header lines						 *Header_CSV*
----------------

This command splits the csv-buffer and adds a window, that holds a small
fraction of the csv file. This is useful, if the first line contains
some kind of a heading and you want always to display it. This works
similar to fixing a certain line at the top. As optional argument, you
can give the number of columns from the top, that shall be displayed. By
default, 1 is used. Use the '!' to close this window. So this >

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


							    *VHeader_CSV*

If you want a vertical header line, use :VHeader. This works similar to the
|Header_CSV| command, except that it will open a vertical split window with
the first column always visible. It will always open the first column in the
new split window. Use the '!' to close the window.
Note, this won't work with linebreaks in the column.


					*VHeaderToggle_CSV* *HeaderToggle_CSV*

Use the :HeaderToggle and :VHeaderToggle command to toggle displaying the
horizontal or vertical header line.


2.9 Sort							*Sort_CSV*
--------

This command can be used to sort the csv file on a certain column. If no range
is given, is sorts the whole file. Specify the column number to sort on as
argument. Use the '!' attribute to reverse the sort order.
For example, the following command sorts line 1 til 10 on the 3 column >

    :1,10Sort 3

While this command >

    :1,10Sort! 3

reverses the order based on column 3.

Instead of a column, you can give the flag 'n' to have it sort numerically.
When no column number is given, it will sort by the column, on which the
cursor is currently.

2.10 Copy Column        					 *Copy_CSV*
----------------

If you need to copy a specific column, you can use the command :Column >

    :[N]Column [a]

Copy column N into register a. If you don't specify N, the column of the
current cursor position is used. If no register is given, the default register
|quotequote| is used. 


2.11 Move A Column        					 *MoveCol_CSV*
------------------

You can move one column behind another column by using the :MoveCol command >

    :[range]MoveCol [source] [dest]

This moves the column number source behind column nr destination.
If both arguments are not given, move the column on which the cursor is behind
the last column. If [range] is not given, MoveCol moves all columns,
otherwise, it moves the columns only for the lines within the range, e.g.
given that your first line is a header line, which you don't want to change >

    :2,$MoveCol 1 $

this would move column 1 behind the last column, while keeping the header line
as is.


2.12 Sum of a Column        					 *SumCol_CSV*
--------------------

You can let Vim output the sum of a column using the :SumCol command >

    :[range]SumCol [nr]

This outputs the result of the column <nr> within the range given. If no range
is given, this will calculate the sum of the whole column. If <nr> is not
given, this calculate the sum for the column the cursor is on. See also
|csv-aggregate-functions|


2.13 Normal mode commands					 *csv-motion*
-------------------------

The csv filetype plugin redefines the following keys as:

<C-Right> or L or W	Move [count] field forwards

<C-Left> or E or H	Move [count] field backwards

<Up> or K		Move [count] lines upwards within the same column

<Down> or J		Move [count] lines downwards within the same column


==============================================================================
3. CSV Configuration					 *csv-configuration*

The CSV plugin tries to automatically detect the field delimiter for your
file, cause although often the file is called CSV (comma separated values), a
semicolon is actually used. The column separator is stored in the buffer-local
variable b:delimiter. This delimiter is heavily used, because you need
it to define a column. Almost all commands use this variable therefore.

3.1 Delimiter							*csv-delimiter*
-------------

So to override the automatic detection of the plugin and define the separator
manually, use: >

    :let g:csv_delim=','

to let the comma be the delimiter. This sets the buffer local delimiter
variable b:delimiter.

If you changed the delimiter, you should reinitiliaze the plugin using
|InitCSV|

3.2 Column							*csv-column*
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

3.3 Hilighting Group                                         *csv-higroup*
--------------------

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

3.4 Strict Columns						*csv-strict*
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

3.5 Multibyte Chars						*csv-mbyte*
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

3.6 Concealing					*csv-syntax*	*csv-conceal*
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


3.7 Newlines						*csv-newline*
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

3.8 Highlight column automatically				*csv-hicol* 
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

==============================================================================
4. CSV Tips and Tricks						*csv-tips*

Here, there you'll find some small tips and tricks that might help when
working with CSV files.

4.1 Statusline							*csv-stl*
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


4.2 Slow ArrangeCol						*csv-slow*
-------------------

Processing a csv file using |ArrangeColumn_CSV| can be quite slow, because Vim
needs to calculate the width for each column and then replace each column by
itself widened by spaces to the optimal length. Unfortunately, csv files tend
to be quite big. Remember, for a file with 10,000 lines and 50 columns Vim
needs to process each cell, which accumulates to 500,000 substitutions. It
might take some time, until Vim is finished.

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



4.3 Defining custom aggregate functions		    *csv-aggregate-functions*
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

    2) Now define your own custom command, which should makes sure, this
       function will be called for a certain column >
       
	    command! -buffer -nargs=? -range=% AvgCol :echo csv#EvalColumn(<q-args>,
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
5. CSV Changelog					       *csv-changelog*

0.16 Jul 25, 2011       -Sort on the range, specified (reported by
                         Peng Yu, thanks!)
			-|MoveCol_CSV| to move a column behind another column
			 (suggested by Peng Yu, thanks!)
			-Document how to use custom functions with a column
			 (|csv-aggregate-functions|)
			-Use g:csv_highlight_column variable, to have Vim
			 automatically highlight the column on which the
			 cursor is (|csv-hicol|)
			-Header/VHeader command should work better now
			 (|Header_CSV|, |VHeader_CSV|)
			-Use setreg() for setting the register for the
		 	 |Column_CSV| command and make sure it is blockwise.
			-Release 0.14 was not correctly uploaded to vim.org
0.14 Jul 20, 2011	-really use g:csv_no_conceal variable
			 (reported by Antonio Ospite, thanks!)
			-Force redrawing before displaying error messages in
			 syntax script (reported by Antonio Ospite, thanks!)
			-Make syntax highlighting work better with different
			 terminals (Should work now with 8, 88 and 256 color
			 terminals, tested with linux konsole, xterm and rxvt)
			 (https://github.com/chrisbra/csv.vim/issues/4)
			-Automatically detect '|' as field separator for csv
			 files
0.13 Mar 14, 2011       -documentation update
			-https://github.com/chrisbra/csv.vim/issues#issue/2
			 ('splitbelow' breaks |Header_CSV|, fix this; thanks
			 lespea!)
			-https://github.com/chrisbra/csv.vim/issues#issue/3
			 ('gdefault' breaks |ArrangeColumn_CSV|, fix this;
			 thanks lespea!)
			-https://github.com/chrisbra/csv.vim/issues#issue/1
			 (make syntax highlighting more robust, thanks lespea!)
			-fix some small annoying bugs
			-WhatColumn! displays column name
0.12 Feb 24, 2011       -bugfix release:
			-don't use |:noa| when switching between windows
			-make sure, colwidth() doesn't throw an error
0.11 Feb 24, 2011       -new command |Copy_CSV|
			-|Search_CSV| did not find anything in the last
			 if no delimiter was given (reported by chroyer)
			- |VHeader_CSV| display the first column as
			 Header similar to how |Header_CSV| works
			- |HeaderToggle_CSV| and |VHeaderToggle_CSV| commands
			 that toggle displaying the header lines/columns
0.10 Feb 23, 2011	-Only conceal real delimiters
			-document g:csv_no_conceal variable
			-document g:csv_nl variable
			-document conceal feature and syntax highlighting
			-Normal mode command <Up>/<Down> work like K/J
			-More robust regular expression engine, that can
			 also handle newlines inside quoted strings.
			-Slightly adjusted syntax highlighting
0.9 Feb 19, 2011        -use conceal char depending on encoding
			-Map normal mode keys also for visual/select and
			 operator pending mode
0.8 Feb 17, 2011	-Better Error handling 
			-HiColumn! removes highlighting
			-Enable NrColumns, that was deactivated in v.0.7
			-a ColorScheme autocommand makes sure, that the syntax
			 highlighting is reapplied, after changing the
			 colorscheme.
			-SearchInColumn now searches in the current column,
			 if no column has been specified
			-A lot more documentation
			-Syntax Highlighting conceales delimiter
			-small performance improvements for
			 |ArrangeColumn_CSV|
0.7 Feb 16, 2011	-Make the motion commands 'W' and 'E' work more
			 reliable
			-Document how to setup filetype plugins
			-Make |WhatColumn_CSV| work more reliable
			 (report from http://vim.wikia.com/Script:3280)
			-DeleteColumn deletes current column, if no argument
			 given
			-|ArrangeColumn_CSV| handles errors better
			-Code cleanup
			-Syntax highlighting
			-'H' and 'L' move forward/backwards between csv fields
			-'K' and 'J' move upwards/downwards within the same
			 column
			-|Sort_CSV| to sort on a certain column
			-|csv-tips| on how to colorize the statusline
0.6 Feb 15, 2011	-Make |ArrangeColumn_CSV| work more reliable
			 (had problems with multibyte chars before)
			-Add |Header_CSV| function
			-'W' and 'E' move forward/backwards between csv fields
			-provide a file ftdetect/csv.vim to detect csv files
0.5  Apr 20 2010	-documentation update
			-switched to a public repository:
			 http://github.com/chrisbra/csv.vim
			-enabled GLVS (see |GLVS|)
0.4a Mar 11 2010	-fixed documentation
0.4  Mar 11 2010	-introduce |InitCSV|
			-better Error handling
			-HiColumn now by default highlights the current
			 column, if no argument is specified.
0.3  Oct, 28 2010	-initial Version

vim:tw=78:ts=8:ft=help:norl:
syntax/csv.vim	[[[1
108
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
    redraw!
    echohl WarningMsg
    echomsg "CSV Syntax:" . a:msg
    echohl Normal
endfu

fu! <sid>CheckSaneSearchPattern() "{{{3
    " Sane defaults ?
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
	call <sid>Warning("Invalid column pattern, using default pattern " .
		    \ s:col_def)
    finally
	let @/ = _s
	call setpos('.', _p)
    endtry
endfu

" Syntax rules {{{2
syn spell toplevel

" Not really needed
syn case ignore

if &t_Co < 88
    hi default CSVColumnHeaderOdd ctermfg=DarkRed ctermbg=15 guibg=grey80 guifg=black term=underline cterm=standout,bold gui=bold,underline 
    hi default CSVColumnOdd	  ctermfg=DarkRed ctermbg=15 guibg=grey80 guifg=black term=underline cterm=bold gui=underline
else
    hi default CSVColumnHeaderOdd ctermfg=darkblue ctermbg=white guibg=grey80 guifg=black cterm=standout,underline gui=bold,underline
    hi default CSVColumnOdd       ctermfg=darkblue ctermbg=white guibg=grey80 guifg=black cterm=reverse,underline gui=underline
endif
    
" ctermbg=8 should be safe, even in 8 color terms
hi default CSVColumnHeaderEven    ctermfg=white ctermbg=darkgrey guibg=grey50 guifg=black term=bold cterm=standout,underline gui=bold,underline 
hi default CSVColumnEven	  ctermfg=white ctermbg=darkgrey guibg=grey50 guifg=black term=bold cterm=underline gui=bold,underline 

" Make sure, we are using a sane, valid pattern for syntax
" highlighting
call <sid>CheckSaneSearchPattern()

" Check for filetype plugin. This syntax script relies on the filetype plugin,
" else, it won't work properly.
redir => s:a |sil filetype | redir end
let s:a=split(s:a, "\n")[0]
if match(s:a, '\cplugin:off') > 0
    call <sid>Warning("No filetype support, only simple highlighting using\n" .
		\ s:del_def . " as delimiter! See :h csv-installation")
endif

if has("conceal") && !exists("g:csv_no_conceal")
    exe "syn match CSVDelimiter /" . s:col . 
	\ '\%(.\)\@=/ms=e,me=e contained conceal cchar=' .
	\ (&enc == "utf-8" ? "│" : '|')
    exe "syn match CSVDelimiterEOL /" . s:del . 
	\ '$/ contained conceal cchar=' .
	\ (&enc == "utf-8" ? "│" : '|')
    hi def link CSVDelimiter Conceal
    hi def link CSVDelimiterEOL Conceal
else
    " The \%(.\)\@= makes sure, the last char won't be concealed,
    " if it isn't a delimiter
    exe "syn match CSVDelimiter /" . s:col . '\%(.\)\@=/ms=e,me=e contained'
    exe "syn match CSVDelimiterEOL /" . s:del . '$/ contained'
    hi def link CSVDelimiter Ignore
    hi def link CSVDelimiterEOL Ignore
endif

" Last match is prefered.

exe 'syn match CSVColumnEven nextgroup=CSVColumnOdd /'
	    \ . s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'
exe 'syn match CSVColumnOdd nextgroup=CSVColumnEven /'
	    \ . s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'

exe 'syn match CSVColumnHeaderEven nextgroup=CSVColumnHeaderOdd /\%1l'
	    \. s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'
exe 'syn match CSVColumnHeaderOdd nextgroup=CSVColumnHeaderEven /\%1l'
	    \. s:col . '/ contains=CSVDelimiter,CSVDelimiterEOL'

let b:current_syntax="csv"
ftdetect/csv.vim	[[[1
2
" Install Filetype detection for CSV files
au BufRead,BufNewFile *.csv,*.dat	set filetype=csv
