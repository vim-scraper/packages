" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/ft_csv.txt	[[[1
177
*ft_csv.txt*  A plugin for CSV Files                 - Vers 0.3  Oct 28, 2009

Author:  Christian Brabandt <cb@256bit.org>

This is a filetype plugin for CSV files. It was heavily influenced by the Vim
Wiki Tip667 (http://vim.wikia.com/wiki/VimTip667), though it works
differently.

                                                           *csv-toc*
1. Installation.................................|csv-installation|
2. CSV.Commands.................................|csv-commands|
    2.1 WhatColumn..............................|WhatColumn|
    2.2 NrColumns...............................|NrColumns|
    2.3 SearchInColumn..........................|SearchInColumn|
    2.4 HiColumn................................|HiColumn|
    2.5 ArrangeColumn...........................|ArrangeColumn|
    2.6 DeleteColumn............................|DeleteColumn|
3. CSV Filetype configuration...................|csv-configuration|
    3.1 Delimiter...............................|csv-delimiter|
    3.2 Column..................................|csv-column|
    3.3 hiGroup.................................|csv-higroup|

==============================================================================
1. Installation					*csv-installation*

In order to have vim automatically detect csv files, you need to have
|ftplugins| enabled (e.g. by having this line in your |.vimrc| file:

   :filetype plugin on

To have Vim automatically detect csv files, you need to do the following.

   1) Create your user runtime directory if you do not have one yet. This 
      directory needs to be in your 'runtime' path. In Unix this would
      typically the ~/.vim directory, while in Windows this is usually your
      ~/vimfiles directory. Use :echo expand("~") to find out, what Vim thinks
      your user directory is. 
      To create this directory, you can do:

	  :!mkdir ~/.vim 
	  
      for Unix and

	  :!mkdir ~/vimfiles 

      for Windows.

   2) In that directory you create a file that will detect csv files.

	if exists("did_load_csvfiletype")
	  finish
	endif
	let did_load_csvfiletype=1

	augroup filetypedetect
	  au! BufRead,BufNewFile *.csv,*.dat		setfiletype csv
	augroup END

      You save this file as "filetype.vim" in your user runtime diretory:

        :w ~/.vim/filetype.vim

   3) To be able to use your new filetype.vim detection, you need to restart
      Vim. Vim will then  load the csv filetype plugin for all files whose
      names end with .csv.

==============================================================================
2. Installation					                *csv-commands*

The CSV ftplugin provides several Commands:

2.1 WhatColumn                                                  *WhatColumn*
--------------

If you would like to know, on which column the cursor is, use :WhatColumn.

2.2 NrColumns                                                   *NrColumns*
--------------

:NrColumns outputs the maximum number of columns available. It does this by
testing the first 10 lines for the number of columns. This usually should be
enough.

2.3 SearchInColumn                                          *SearchInColumn*
------------------

Use :SearchInColumn to search for a pattern within a specific column. The
usage is:
:SearchInColumn <nr> /{pat}/

So if you would like to search in Column 1 for the word foobar, you enter

:SearchInColumn 1 /foobar/

Instead of / as delimiter, you can use any other delimiter you like.

2.4 HiColumn                                                     *HiColumn*
------------

:HiColumn <nr> can be used to highlight Column <nr>. Currently the plugin uses
the WildMenu Highlight Group. If you would like to change this, you need to
define the variable |g:csv_hiGroup|.

2.5 ArrangeColumn                                           *ArrangeColumn*
-----------------

If you would like all columns to be arranged visually, you can use the
:ArrangeColumn command. Beware, that this will change your file and depending
on the size of your file may slow down Vim significantly. This is highly
experimental.
:ArrangeCommand will try to vertically align all columns by their maximum
column size.

2.6 DeleteColumn                                           *DeleteColumn*
----------------

The command :DeleteColumn can be used to delete a specific column.

:DeleteColumn 2

will delete column 2.

==============================================================================
3. CSV Configuration					  *csv-configuration*

The CSV plugin tries to automatically detect the field delimiter for your
file, cause although often the file is called CSV (comma separated values), a
semicolon is actually used. The column separator is stored in the buffer-local
variable (|b:var| delimiter. This delimiter is heavily used, because you need
it to define a column. Almost all commands use this variable therefore.

3.1 Delimiter                                                  *csv-delimiter*
-------------   

So to override the automatic detection of the plugin and define the separator
manually, use:

:let b:delimiter=','

to let the comma be the delimiter.

3.2 Column                                                  *csv-column*
----------

The definition, of what a column is, is defined as buffer-local variable
b:col. By default this variable is initialized to:

let b:col='\%(\%([^' . b:delimiter . ']*"[^"]*"[^' . b:delimiter . ']*' 
    \. b:delimiter . '\)\|\%([^' . b:delimiter . ']*\%(' . b:delimiter 
    \. '\|$\)\)\)'

This should take care of quoted delimiters within a column. Those should
obviously not count as a delimiter. 

If you changed the b:delimiter variable, you need to redefine the b:col
variable, cause otherwise it will not reflect the change. To change the
variable from the comma to a semicolon, you could call in your CSV-Buffer
this command:

:let b:col=substitute(b:col, ',', ';', 'g')

Check with :echo b:col, if the definition is correct afterwards.

3.3 Hilighting Group                                         *csv-higroup*
--------------------

By default the csv ftplugin uses the WildMenu highlighting Group to define how
the |HiColumn| command highlights columns. If you would like to define a
different highlighting group, you need to set this via the g:csv_hiGroup
variable. You can e.g. define it in your .vimrc:

:let g:csv_hiGroup = "IncSearch"

You need to restart Vim, if you have changed this variable.

vim:tw=78:ts=8:ft=help:norl:

ftplugin/csv.vim	[[[1
219
" Filetype plugin for editing CSV files."{{{
" Copyright: C.Brabandt  <cb@256bit.org>
" Version:   0.3
" Some ideas are take from the wiki http://vim.wikia.com/wiki/VimTip667
" though, implementation differs.
if v:version < 700 || exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1"}}}

fu! <SID>GetDelimiter()"{{{
    let _cur = getpos('.')
    let Delim={0: ';', 1:  ','}
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
    return result[0]
endfu"}}}

fu! <SID>HiCol(colnr)"{{{
    if a:colnr > <SID>MaxColumns()
	call <SID>echoWarn("There exists no column " . a:colnr)
	return 1
    endif
    "let colpat='\%(\%("[^"]\+"' . b:delimiter . '\)\|\([^' . b:delimiter . ']\+' . b:delimiter . '\)\)'
    "let b:col= '\%(\%([^' . b:delimiter . ']*"[^"]*"[^' . b:delimiter . ']*' . b:delimiter . '\)\|\%([^' . b:delimiter . ']*\%(' . b:delimiter . '\|$\)\)\)'
    "let colpat='\%(\%([^' . b:delimiter . ']*\%("[^"]*"\)\?\)[^' . b:delimiter . ']*' . b:delimiter . '\?\)'
    "let pat='^' . <SID>GetColPat(0,a:colnr) . '\zs[^' . b:delimiter . ']*' . b:delimiter . '\?'
    "let pat='^' . <SID>GetColPat(0,a:colnr) . '\zs' . b:col
    if a:colnr==1
	let pat='^'. <SID>GetColPat(a:colnr,0)
    else
	let pat='^'. <SID>GetColPat(a:colnr-1,1) . b:col
    endif

    if exists("*matchadd")
	let matchlist=getmatches()
	call filter(matchlist, 'v:val["group"] !~ s:hiGroup')
	call setmatches(matchlist)
	let s:matchid=matchadd(s:hiGroup, pat, 0)
    else
        exe ":2match " . s:hiGroup . ' /' . pat . '/'
    endif
endfu"}}}

fu! <SID>WColumn()"{{{
    let _cur = getpos('.')
    let line=getline('.')
    " If the cursor is on the field delimiter,
    " match will return the next column
    " so we move one char left
    if line[col('.')-1] == b:delimiter
       normal h
    endif
    call search(b:col, 'e', line('.'))
    let end=col('.')-1
    call search(b:col, 'b', line('.'))
    let start=col('.')-1
    let i=escape(line[start : end], '\')
    let fields=(split(line,b:col.'\zs'))
    call setpos('.',_cur)
    return match(fields, '\V'.i)+1
endfu"}}}

fu! <SID>MaxColumns()"{{{
    "return maximum number of columns in first 10 lines
    let l=getline(1,10)
    let fields=[]
    let result=0
    for item in l
	let temp=len(split(item, b:col.'\zs'))
	let result=(temp>result ? temp : result)
    endfor
    return result
endfu"}}}

fu! <SID>echoWarn(mess)"{{{
    echohl WarningMsg
    echomsg a:mess
    echohl Normal
endfu"}}}

fu! <SID>SearchColumn(arg)"{{{
    let arglist=split(a:arg)
    let colnr=arglist[0]
    let pat=substitute(arglist[1], '^\(.\)\(.*\)\1$', '\2', '')
    let maxcolnr = <SID>MaxColumns()
    if colnr > maxcolnr
	call <SID>echoWarn("There exists no column " . colnr)
	return 1
    endif
    "let @/=<SID>GetColPat(colnr) . '*\zs' . pat . '\ze\([^' . b:delimiter . ']*' . b:delimiter .'\)\?' . <SID>GetColPat(maxcolnr-colnr-1)
    " GetColPat(nr) returns a pattern containing '\zs' if nr > 1,
    " therefore, we need to clear that flag again ;(
    " TODO:
    " Is there a better way, than running a substitute command on '\zs', may be using a flag
    " with GetColPat(zsflag, colnr)?
    if colnr > 1
	"let @/=<SID>GetColPat(colnr-1,0) . '*\zs' . pat . '\ze\([^' . b:delimiter . ']*' . b:delimiter .'\)\?' . <SID>GetColPat(maxcolnr-colnr-1,0)
	"let @/= '^' . <SID>GetColPat(colnr-1,0) . '[^' . b:delimiter . ']*\zs' . pat . '\ze[^' . b:delimiter . ']*'.b:delimiter . <SID>GetColPat(maxcolnr-colnr,0) . '$'
	"let @/= '^' . <SID>GetColPat(colnr-1,0) . b:col1 . '\?\zs' . pat . '\ze' . b:col1 .'\?' . <SID>GetColPat(maxcolnr-colnr,0) " . '$'
	let @/= '^' . <SID>GetColPat(colnr-1,0) . '\%([^' . b:delimiter .']*\)\?\zs' . pat . '\ze' . '\%([^' . b:delimiter .']*\)\?' . b:delimiter . <SID>GetColPat(maxcolnr-colnr,0)  . '$'
    else
	"let @/= '^\zs' . pat . '\ze' . substitute((<SID>GetColPat(maxcolnr - colnr)), '\\zs', '', 'g')
	"let @/= '^\zs' . b:col1 . '\?' . pat . '\ze' . b:col1 . '\?' .  <SID>GetColPat(maxcolnr,0) . '$'
	let @/= '^' . '\%([^' . b:delimiter . ']*\)\?\zs' . pat . '\ze\%([^' . b:delimiter . ']*\)\?' . b:delimiter .  <SID>GetColPat(maxcolnr-1,0) . '$'
    endif
    normal n
endfu"}}}

fu! <SID>DelColumn(colnr)"{{{
    let maxcolnr = <SID>MaxColumns()
    if a:colnr > maxcolnr
	call <SID>echoWarn("There exists no column " . a:colnr)
	return 
    endif
    if a:colnr != '1'
	let pat= '^' . <SID>GetColPat(a:colnr-1,1) . b:col
    else
	let pat= '^' . <SID>GetColPat(a:colnr,0) 
    endif
    "let @/ = pat
    "echo pat
    exe ':%s/' . escape(pat, '/') . '//'
endfu"}}}

fu! <SID>ColWidth(colnr)"{{{
    let list=getline(1,'$')
    let width=20 "Fallback (wild guess)
    try
	" we have a list of the first 10 rows
	" Now transform it to a list of field a:colnr
	" and then return the maximum strlen
	" We could do it with 1 line, but that would look ugly
	call map(list, 'split(v:val, b:col."\\zs")[a:colnr-1]')
	call map(list, 'substitute(v:val, ".", "x", "g")')
	call map(list, 'strlen(v:val)')
	"call map(list, 'strlen(substitute((split(v:val, b:col."\\zs")[a:colnr-1]), '.', 'x', 'g')')
	return max(list)
    catch
        return  width
    endtry
endfu"}}}

fu! <SID>ArrangeCol() range"{{{
   let _cur=getpos('.')
"   let max_width=<SID>ColWidth(<SID>WColumn())+1
   let col_width=[]
   let max_cols=<SID>MaxColumns()
   for i in range(1,max_cols)
       call add(col_width, <SID>ColWidth(i))
   endfor

   "exe ':%s/' . (b:col) . '/\=printf("%.' . (<SID>ColWidth(<SID>WColumn())+1) . 's", submatch(0).repeat(" ", (<SID>ColWidth(<SID>WColumn())-strlen(submatch(0)))))/g'
"   echo ':%s/' . (b:col) . '/\=printf("%*.*s",'  (col_width[<SID>WColumn()-1]+1) ", ". (col_width[<SID>WColumn()-1]+1) . ", submatch(0))"/g'
   "exe ':%s/' . (b:col) . '/\=printf("%*.*s",' . (col_width[<SID>WColumn()-1]+1) . ", " . (col_width[<SID>WColumn()-1]+1) . ", submatch(0))/g"
   "exe ':%s/' . (b:col) . '/\=printf("%*.*s",  (<SID>ColWidth(<SID>WColumn())+1) ,  (<SID>ColWidth(<SID>WColumn())+1) , submatch(0))/g'
   exe ':%s/' . (b:col) . '/\=printf("%-*.*s", (col_width[<SID>WColumn()-1]+1) ,  (col_width[<SID>WColumn()-1]+1) , submatch(0))/g'
   call setpos('.', _cur)
endfu"}}}

fu! <SID>GetColPat(colnr, zs_flag)"{{{
    if a:colnr > 1
	"let pat='\%(\%("\%([^"]\|""\)*"\)\|\%([^' . b:delimiter . '"]*\)\)\{' . (a:colnr-1) . '\}'
	"let pat='\%(\%([^' . b:delimiter . ']*\%("[^"]*"\)\?\)[^' . b:delimiter . ']*'.b:delimiter . '\)\{' . (a:colnr-1) . '\}'
	"    let pat=b:col . '\{' . (a:colnr) . '\}' . (a:zs_flag ? '\zs' : '') " . b:col
	let pat=b:col . '\{' . (a:colnr) . '\}' 
        "let pat='\%([^'.b:delimiter . ']*' . b:delimiter . '\)\{' . (a:colnr-1) . '\}'
	"if a:startofline
	"    let pat.= '[^' . b:delimiter . ']'
	"endif
    else
	"colnr = 1
        "let pat='[^' . b:delimiter . ']'
        let pat=b:col 
    endif
    "return (a:startofline ? '^' : '') . pat
    return pat . (a:zs_flag ? '\zs' : '')
endfu"}}}

fu! <SID>Init()"{{{
    " Hilight Group for Columns
    if exists("g:csv_hiGroup")
	let s:hiGroup = g:csv_hiGroup
    else
	let s:hiGroup="WildMenu"
    endif
    " Determine default Delimiter
    let b:delimiter=<SID>GetDelimiter()
    " Pattern for matching a single column
    let b:col='\%(\%([^' . b:delimiter . ']*"[^"]*"[^' . b:delimiter . ']*' . b:delimiter . '\)\|\%([^' . b:delimiter . ']*\%(' . b:delimiter . '\|$\)\)\)'
    setl nostartofline tw=0 nowrap
    command! -buffer WhatColumn :echo <SID>WColumn()
    command! -buffer NrColumns :echo <SID>MaxColumns()
    command! -buffer -nargs=1 HiColumn :call <SID>HiCol(<args>)
    command! -buffer -nargs=* SearchInColumn :call <SID>SearchColumn(<q-args>)
    command! -buffer -nargs=1 DeleteColumn :call <SID>DelColumn(<args>)
    command! -buffer ArrangeColumn :call <SID>ArrangeCol()
    " undo when setting a new filetype
    let b:undo_ftplugin = "setlocal sol< tw< wrap<"
	\ . "| unlet b:delimiter b:col"
endfu"}}}

:call <SID>Init()

" vim: set foldmethod=marker: 
