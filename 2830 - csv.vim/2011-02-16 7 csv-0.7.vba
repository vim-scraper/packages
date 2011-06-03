" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/csv.vim	[[[1
407
" Filetype plugin for editing CSV files. "{{{1
" Author:  Christian Brabandt <cb@256bit.org>
" Version: 0.7
" Script:  http://www.vim.org/scripts/script.php?script_id=2830
" License: VIM License
" Last Change: Wed, 16 Feb 2011 23:24:37 +0100
" Documentation: see :help ft_csv.txt
" GetLatestVimScripts: 2830 6 :AutoInstall: csv.vim
"
" Some ideas are take from the wiki http://vim.wikia.com/wiki/VimTip667
" though, implementation differs.

" Plugin folclore "{{{2
if v:version < 700 || exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

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
    " Determine default Delimiter
    if !exists("g:csv_delim")
	let b:delimiter=<SID>GetDelimiter()
    else
	let b:delimiter=g:csv_delim
    endif
    if empty(b:delimiter)
	call <SID>Warn("No delimiter found. See :h csv-delimiter to set it manually!")
    endif
    
    " Pattern for matching a single column
    let b:col='\%(\%([^' . b:delimiter . ']*"[^"]*"[^' . 
		\ b:delimiter . ']*' . b:delimiter . '\)\|\%([^' . 
		\ b:delimiter . ']*\%(' . b:delimiter . '\|$\)\)\)'

    " define buffer-local commands
    call <SID>CommandDefinitions()
    " CSV specific mappings
    call <SID>CSVMappings()

    " force reloading CSV Syntax Highlighting
    if exists("b:current_syntax")
	unlet b:current_syntax
	" Force reloading syntax file
	exe "silent do Syntax" expand("%")
    endif

    " undo when setting a new filetype
    let b:undo_ftplugin = "setlocal sol< tw< wrap<"
	\ . "| unlet b:delimiter b:col"
    " CSV local settings
    setl nostartofline tw=0 nowrap
endfu 

fu! <SID>SearchColumn(arg) "{{{3
    let arglist=split(a:arg)
    let colnr=arglist[0]
    let pat=substitute(arglist[1], '^\(.\)\(.*\)\1$', '\2', '')
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
    norm! n
endfu

fu! <SID>DelColumn(colnr) "{{{3
    let maxcolnr = <SID>MaxColumns()
    if a:colnr > maxcolnr
	call <SID>Warn("There exists no column " . a:colnr)
	return 
    endif

    if empty(a:colnr)
       let colnr=<SID>WColumn()
    else
       let colnr=a:colnr
    endif

    if a:colnr != '1'
	let pat= '^' . <SID>GetColPat(a:colnr-1,1) . b:col
    else
	let pat= '^' . <SID>GetColPat(a:colnr,0) 
    endif
    exe ':%s/' . escape(pat, '/') . '//'
endfu

fu! <SID>HiCol(colnr) "{{{3
    if a:colnr > <SID>MaxColumns()
	call <SID>Warn("There exists no column " . a:colnr)
	return
    endif
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

    if exists("*matchadd")
	if exists("s:matchid")
	   " ignore errors, that come from already deleted matches
	   sil! call matchdelete(s:matchid)
	endif
	" Additionally, filter all matches, that could have been used earlier
	let matchlist=getmatches()
	call filter(matchlist, 'v:val["group"] !~ s:hiGroup')
	call setmatches(matchlist)
	let s:matchid=matchadd(s:hiGroup, pat, 0)
    else
        exe ":2match " . s:hiGroup . ' /' . pat . '/'
    endif
endfu

fu! <SID>GetDelimiter() "{{{3
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
    if !empty(result)
	return result[0]
    else
	return ''
    endif
endfu

fu! <SID>WColumn() "{{{3
    " Return on which column the cursor is
    let _cur = getpos('.')
    let line=getline('.')
    " move cursor to end of field
    call search(b:col, 'ec', line('.'))
    let end=col('.')-1
    let fields=(split(line[0:end],b:col.'\zs'))
    call setpos('.',_cur)
    return len(fields)
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
    let list=getline(1,'$')
    let width=20 "Fallback (wild guess)
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
        return  width
    endtry
endfu

fu! <SID>ArrangeCol() range "{{{3
    let _cur=getpos('.')
    " Force recalculation of Column width
    if exists("b:col_width")
      unlet b:col_width
    endif

   exe ':%s/' . (b:col) . '/\=<SID>Columnize(submatch(0))/g'
   call setpos('.', _cur)
endfu

fu! <SID>Columnize(field) "{{{3
   if !exists("b:col_width")
	let b:col_width=[]
	let max_cols=<SID>MaxColumns()
	for i in range(1,max_cols)
	    call add(b:col_width, <SID>ColWidth(i))
	endfor
   endif
   " convert zero based indexed list to 1 based indexed list,
   " Default: 20 width, in case that column width isn't defined
   let width=get(b:col_width,<SID>WColumn()-1,20)
   let a = split(a:field, '\zs')
   let add = eval(join(map(a, 'len(v:val)'), '+'))
   let add -= len(a)
   
   " Add one for the frame
   " plus additional width for multibyte chars,
   " since printf(%*s..) uses byte width!
   let width = width + add  + 1

   return printf("%*s", width ,  a:field)
endfun

fu! <SID>GetColPat(colnr, zs_flag) "{{{3
    if a:colnr > 1
	let pat=b:col . '\{' . (a:colnr) . '\}' 
    else
        let pat=b:col 
    endif
    return pat . (a:zs_flag ? '\zs' : '')
endfu

fu! <SID>SplitHeaderLine(lines, bang) "{{{3
    if !a:bang && !exists("b:CSV_SplitWindow")
	" Split Window
	let _stl = &l:stl
	let _sbo = &sbo
	setl scrollopt=hor scrollbind
	let lines = empty(a:lines) ? 1 : a:lines
	noa sp
	1
	exe "resize" . lines
	setl scrollopt=hor scrollbind winfixheight
	"let &l:stl=repeat(' ', winwidth(0))
	let &l:stl="%#Normal#".repeat(' ',winwidth(0))
	" Highlight first row
	call matchadd("Type", b:col)
	let b:CSV_SplitWindow = winnr()
	exe "noa wincmd p"
    else
	" Close split window
	if !exists("b:CSV_SplitWindow")
	    return
	endif
	exe "noa" b:CSV_SplitWindow "wincmd w"
	unlet b:CSV_SplitWindow
	if exists("_stl")
	    let &l_stl = _stl
	endif
	if exists("_sbo")
	    let &sbo = _sbo
	endif
	setl noscrollbind
	wincmd c
    endif
endfu

fu! <SID>MoveCol(forward, line) "{{{3
    let colnr=<SID>WColumn()
    let maxcol=<SID>MaxColumns()

    " Check for valid column
    " a:forward == 1 : search next col
    " a:forward == -1: search prev col
    " a:forward == 0 : stay in col
    if colnr - v:count1 >= 1 && a:forward == -1
	let colnr -= v:count1
    elseif colnr - v:count1 < 1 && a:forward == -1
	let colnr = 0
    elseif colnr + v:count1 <= <SID>MaxColumns() && a:forward == 1
	let colnr += v:count1
    elseif colnr + v:count1 > <SID>MaxColumns() && a:forward == 1
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
	let pat = pat . '\%' . line . 'l'
    elseif colnr == 0
	let pat = '^' . '\%' . line . 'l'
    elseif colnr == maxcol + 1
	let pat='\%' . line . 'l$'
    else
	let pat='^'. <SID>GetColPat(colnr-1,1) . b:col
	let pat = pat . '\%' . line . 'l'
    endif

    " Search
    if a:forward > 0
	call search(pat, 'cW')
    elseif a:forward < 0
	call search(pat, 'bWe')
    else
	call search(pat, 'c')
    endif
endfun

fu! <SID>SortComplete(A,L,P) "{{{3
    return join(range(1,<sid>MaxColumns()),"\n")
endfun 

fu! <SID>Sort(bang, colnr) range "{{{3
    if a:colnr != '1'
	let pat= '^' . <SID>GetColPat(a:colnr-1,1) . b:col
    else
	let pat= '^' . <SID>GetColPat(a:colnr,0) 
    endif
    exe ":sort" . (a:bang ? '!' : '') . ' r /' . pat . '/'
endfun

fu! CSV_WCol() "{{{3
    return printf(" %d/%d", <SID>WColumn(), <SID>MaxColumns())
endfun

fu! <SID>CommandDefinitions() "{{{3
    if !exists(":WhatColumn")
	command! -buffer WhatColumn :echo <SID>WColumn()
    endif
    if exists(":NrColumns")
	command! -buffer NrColumns :echo <SID>MaxColumns()
    endif
    if !exists(":HiColumn")
	command! -buffer -nargs=? HiColumn :call <SID>HiCol(<q-args>)
    endif
    if !exists(":SearchInColumn")
	command! -buffer -nargs=* SearchInColumn :call <SID>SearchColumn(<q-args>)
    endif
    if !exists(":DeleteColumn")
	command! -buffer -nargs=? DeleteColumn :call <SID>DelColumn(<args>)
    endif
    if !exists(":ArrangeColumn")
	command! -buffer ArrangeColumn :call <SID>ArrangeCol()
    endif
    if !exists(":InitCSV")
	command! -buffer InitCSV :call <SID>Init()
    endif
    if !exists(":Header")
	command! -buffer -bang -nargs=? Header :call <SID>SplitHeaderLine(<q-args>,<bang>0)
    endif
    if !exists(":Sort")
	command! -buffer -nargs=1 -bang -range=% -complete=custom,<SID>SortComplete Sort :<line1>,<line2>call <SID>Sort(<bang>0,<args>)
    endif
endfu

fu! <SID>CSVMappings() "{{{3
    nnoremap <silent> <buffer> W :<C-U>call <SID>MoveCol(1, line('.'))<CR>
    nnoremap <silent> <buffer> E :<C-U>call <SID>MoveCol(-1, line('.'))<CR>
    nnoremap <silent> <buffer> K :<C-U>call <SID>MoveCol(0, line('.')-v:count1)<CR>
    nnoremap <silent> <buffer> J :<C-U>call <SID>MoveCol(0, line('.')+v:count1)<CR>
    " Map C-Right and C-Left as alternative to W and E
    nmap <silent> <buffer> <C-Right> W
    nmap <silent> <buffer> <C-Left>  E
    nmap <silent> <buffer> H E
    nmap <silent> <buffer> L W
endfu


" end function definition "}}}2
" Initialize Plugin "{{{2
:call <SID>Init()

" Vim Modeline " {{{2
" vim: set foldmethod=marker: 
doc/ft_csv.txt	[[[1
318
*ft_csv.txt*  A plugin for CSV Files

Author:  Christian Brabandt <cb@256bit.org>
Version: 0.7

The VIM LICENSE applies to csv.vim
(see |copyright|) except use csv.vim instead of "Vim".
NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.

This is a filetype plugin for CSV files. It was heavily influenced by
the Vim Wiki Tip667 (http://vim.wikia.com/wiki/VimTip667), though it
works differently.

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
    2.10 Normal mode commands...................|csv-motion|
3. CSV Filetype configuration...................|csv-configuration|
    3.1 Delimiter...............................|csv-delimiter|
    3.2 Column..................................|csv-column|
    3.3 hiGroup.................................|csv-higroup|
4. CSV Tips and Tricks..........................|csv-tips|
5. CSV Changelog................................|csv-changelog|

==============================================================================
1. Installation			*csv-installation*

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
      au! BufRead,BufNewFile *.csv,*.dat	setfiletype csv
    augroup END

      You save this file as "filetype.vim" in your user runtime diretory:

        :w ~/.vim/filetype.vim

   3) To be able to use your new filetype.vim detection, you need to restart
      Vim. Vim will then  load the csv filetype plugin for all files whose
      names end with .csv.

==============================================================================
2. Installation					*csv-commands*

The CSV ftplugin provides several Commands:

2.1 WhatColumn                                                  *WhatColumn_CSV*
--------------

If you would like to know, on which column the cursor is, use :WhatColumn.

2.2 NrColumns                                                   *NrColumns_CSV*
--------------

:NrColumns outputs the maximum number of columns available. It does this by
testing the first 10 lines for the number of columns. This usually should be
enough.

2.3 SearchInColumn                                          *SearchInColumn_CSV*
------------------

Use :SearchInColumn to search for a pattern within a specific column. The
usage is:
:SearchInColumn <nr> /{pat}/

So if you would like to search in Column 1 for the word foobar, you enter >

    :SearchInColumn 1 /foobar/

Instead of / as delimiter, you can use any other delimiter you like.

2.4 HiColumn                                                     *HiColumn_CSV*
------------

:HiColumn <nr> can be used to highlight Column <nr>. Currently the plugin uses
the WildMenu Highlight Group. If you would like to change this, you need to
define the variable |g:csv_hiGroup|.
If you do not specify a <nr>, HiColumn will hilight the column on which the
cursor is.

2.5 ArrangeColumn                                           *ArrangeColumn_CSV*
-----------------

If you would like all columns to be arranged visually, you can use the
:ArrangeColumn command. Beware, that this will change your file and depending
on the size of your file may slow down Vim significantly. This is highly
experimental.
:ArrangeCommand will try to vertically align all columns by their maximum
column size. Note, this can be very slow on large files!

2.6 DeleteColumn                                           *DeleteColumn_CSV*
----------------

The command :DeleteColumn can be used to delete a specific column.

:DeleteColumn 2

will delete column 2.

2.7 InitCSV                                                *InitCSV*
-----------

Reinitialize the Plugin. Use this, if you have changed the configuration
of the plugin (see |csv-configuration| ).

2.8 Header                                                *Header_CSV*
----------

This command splits the csv-buffer and adds a window, that holds a small
fraction of the csv file. This is useful, if the first line contains
some kind of a heading and you want always to display it. This works
similar to fixing a certain line at the top. As optional argument, you
can give the number of columns from the top, that shall be displayed. By
default, 1 is used. Use the '!' to close this window. So this >

    :Header 3

opens at the top a split window, that holds the first 3 lines, is fixed
and horizontally 'scrollbind'ed to the csv window, while >

    :Header!

closes the fixed window.

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

2.10 Normal mode commands					 *csv-motion*
-------------------------

The csv filetype plugin redefines the following keys as:

<C-Right> or L or W	Move [count] field forwards

<C-Left> or E or H	Move [count] field backwards

K			Move [count] lines upwards within the same column

J			Move [count] lines downwards within the same column


==============================================================================
3. CSV Configuration					 *csv-configuration*

The CSV plugin tries to automatically detect the field delimiter for your
file, cause although often the file is called CSV (comma separated values), a
semicolon is actually used. The column separator is stored in the buffer-local
variable b:delimiter. This delimiter is heavily used, because you need
it to define a column. Almost all commands use this variable therefore.

3.1 Delimiter                                                  *csv-delimiter*
-------------   

So to override the automatic detection of the plugin and define the separator
manually, use:

:let g:csv_delim=','

to let the comma be the delimiter. This sets the buffer local delimiter
variable b:delimiter.

If you changed the delimiter, you should reinitiliaze the plugin using
|InitCSV|

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

You need to restart Vim, if you have changed this variable or use |InitCSV|

==============================================================================
4. CSV Tips and Tricks						*csv-tips*

Suppose you want to include the column, on which the cursor is, into your
statusline. You can do this, by defining in your .vimrc the 'statusline' like
this: >

    function MySTL()
    if has("statusline")
	hi User1 term=standout ctermfg=0 ctermbg=11 guifg=Black guibg=Yellow
	let stl = ...
	let csv = '%=%1*%{&ft=~"csv" ? CSV_WCol() : ""}%*'
	return stl.csv
    endfunc
    set stl=%!MySTL()

<

This will draw in your statusline right aligned the current column and max
column (like 1/10), if you are inside a CSV file. This status uses the User1
highlighting (|hl-User1|), that has been defined in the second line of the
function. In the third line of your function, put your desired 'statusline'
settings as |expression|.

==============================================================================
5. CSV Changelog					       *csv-changelog*

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
22
" A simple syntax highlighting, simply alternate colors between two
" adjacent columns
if version < 600
    syn clear
elseif exists("b:current_syntax")
    finish
endif

syn spell toplevel

" Not really needed
syn case ignore

hi CSVColumnOdd	ctermfg=0 ctermbg=6 guibg=grey80 guifg=black
hi CSVColumnEven ctermfg=0 ctermbg=8 guibg=grey50 guifg=black
exe 'synt match CSVColumnOdd nextgroup=CSVColumnEven excludenl /'
	    \ . b:col . '/'
exe 'synt match CSVColumnEven nextgroup=CSVColumnOdd excludenl /'
	    \ . b:col . '/'


let b:current_syntax="csv"
ftdetect/csv.vim	[[[1
2
" Install Filetype detection for CSV files
au BufRead,BufNewFile *.csv,*.dat	set filetype=csv
