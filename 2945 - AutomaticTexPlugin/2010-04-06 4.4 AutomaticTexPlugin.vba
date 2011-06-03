" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/tex_atp.vim	[[[1
3117
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Apr 06
" URL:		
" GetLatestVimScripts: 2945 11 :AutoInstall: tex_atp.vim
" Copyright:    Copyright (C) 2010 Marcin Szamotulski Permission is hereby
"		granted to use and distribute this code, with or without
" 		modifications, provided that this copyright notice is copied
" 		with it. Like anything else that's free, Automatic TeX Plugin
" 		is provided *as is* and comes with no warranty of any kind,
" 		either expressed or implied. By using this plugin, you agree
" 		that in no event will the copyright holder be liable for any
" 		damages resulting from the use of this software. 
" 		This licence is valid for all files distributed with ATP
" 		plugin.
"
" TODO: write on ISearch in doc file
"
" Done: modify EditInputFiles so that it finds file in the g:mainfile
"
" Done: make a function which list all definitions
"
" TODO: bibtex is not processing right (after tex+bibtex+tex+tex, +\l gives
" the citation numbers)
"
" TODO: g:mainfile is not working with b:outdir, (b:outdir should not be
" changed for intput files)
"
" TODO: to make s:maketoc and s:generatelabels read all input files between
" \begin{document} and \end{document}, and make it recursive.
" now s:maketoc finds only labels of chapters/sections/...
" TODO: make toc work with parts!
"
" Comment: The time consuming part of TOC command is: openning new window
" ('vnew') as shown by profiling.
"
" TODO: pid file
" Comment: b:changedtick "HOW MANY CHANGES WERE DONE! this could be useful.
"
" TODO: Check against lilypond 
"
" Done: make a split version of EditInputFile
"
" TODO: for input files which filetype=plaintex (for example hyphenation
" files), the variable b:autex is not set.  
"
" NOTES
" s:tmpfile =	temporary file value of tempname()
" b:texfile =	readfile(bunfname("%")

" We need to know bufnumber and bufname in a tabpage.
let t:bufname=bufname("")
let t:bufnr=bufnr("")
let t:winnr=winnr()

" This limits how many consecutive runs there can be maximally.
let s:runlimit=5 

" These autocommands are used to remember the last opened buffer number and its
" window number:
au BufLeave *.tex let t:bufname=resolve(fnamemodify(bufname(""),":p"))
au BufLeave *.tex let t:bufnr=bufnr("")
" t:winnr the last window used by tex, ToC or Labels buffers:
au WinEnter *.tex let t:winnr=winnr("#")
au WinEnter __ToC__ 	let t:winnr=winnr("#")
au WinEnter __Labels__ 	let t:winnr=winnr("#")

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

" Options
setl keywordprg=texdoc\ -m
" setl matchpairs='(:),[:],{:}' " multibyte characters are not supported yet
" so \(:\), \[:\] want work :(. New function
" Borrowed from tex.vim written by Benji Fisher:
    " Set 'comments' to format dashed lists in comments
    setlocal com=sO:%\ -,mO:%\ \ ,eO:%%,:%

    " Set 'commentstring' to recognize the % comment character:
    " (Thanks to Ajit Thakkar.)
    setlocal cms=%%s

    " Allow "[d" to be used to find a macro definition:
    " Recognize plain TeX \def as well as LaTeX \newcommand and \renewcommand .
    " I may as well add the AMS-LaTeX DeclareMathOperator as well.
    let &l:define='\\\([egx]\|char\|mathchar\|count\|dimen\|muskip\|skip\|toks\)\='
	    \ .	'def\|\\font\|\\\(future\)\=let'
	    \ . '\|\\new\(count\|dimen\|skip\|muskip\|box\|toks\|read\|write'
	    \ .	'\|fam\|insert\)'
	    \ . '\|\\\(re\)\=new\(boolean\|command\|counter\|environment\|font'
	    \ . '\|if\|length\|savebox\|theorem\(style\)\=\)\s*\*\=\s*{\='
	    \ . '\|DeclareMathOperator\s*{\=\s*'
    setlocal include=\\\\input\\\\|\\\\include{
    setlocal suffixesadd=.tex

setl includeexpr=substitute(v:fname,'\\%(.tex\\)\\?$','.tex','')
" TODO set define and work on the abve settings, these settings work with [i
" command but not with [d, [D and [+CTRL D (jump to first macro definition)

"------------ append / at the end of a directory name ------------
fun! s:append(where,what)
    return substitute(a:where,a:what . "\s*$",'','') . a:what
endfun
" ----------------- FindInputFiles ---------------
" it should return in the values of the dictionary the name of the file that
" FindInputFile([bufname],[echo])
if !exists("*FindInputFiles") 
function! FindInputFiles(...)    

    let l:echo=1
    if a:0==0
	let l:bufname=bufname("%")
    else
	let l:bufname=a:1
	if a:0 == 2
	    let l:echo=0
	endif
    endif

    let l:dir=fnamemodify(l:bufname,":p:h")
    let l:texfile=readfile(fnamemodify(l:bufname,":p"))
    let s:i=0
    let l:inputlines=[]
    for l:line in l:texfile
	if l:line =~ "\\\\\\(input\\|include\\|includeonly\\)" && l:line !~ "^\s*%"
	    "add the line but cut it before first '%', thus we should get the
	    "file name.
	    let l:col=stridx(l:line,"%")
	    if l:col != -1
		let l:line=strpart(l:line,0,l:col)
	    endif
	    let l:inputlines=add(l:inputlines,l:line) 
	endif
    endfor

   " this is the dictionary that will be returned, its format is:
   " { input file name (as appear in tex file : [ input/include/bib, name of the main tex file ] }
    let l:inputfiles={}

    for l:line in l:inputlines
	if l:line !~ '{'
	    let l:inputfile=substitute(l:line,'\\\%(input\|include\|includeonly\)\s\+\(.*\)','\1','')
	    call extend(l:inputfiles, { l:inputfile : [ 'input' , fnamemodify(expand("%"),":p") ] } )
	else
	    let l:bidx=stridx(l:line,'{')
" 	    let l:eidx=stridx(l:line,'}')
	    let l:eidx=len(l:line)-stridx(join(reverse(split(l:line,'\zs')),''),'}')-1
	    let l:inputfile=strpart(l:line,l:bidx+1,l:eidx-l:bidx-1)
	    call extend(l:inputfiles, { l:inputfile : [ 'include' , fnamemodify(expand("%"),":p") ] } )
	endif
    endfor
    call extend(l:inputfiles,FindBibFiles(l:bufname))
    if l:echo 
	if len(keys(l:inputfiles)) > 0 
	    echohl WarningMsg | echomsg "Found input files:" 
	else
	    echohl WarningMsg | echomsg "No input files found." | echohl None
	    return []
	endif
	echohl texInput
	let l:nr=1
	for l:inputfile in keys(l:inputfiles)
	    if l:inputfiles[l:inputfile][0] == 'input'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiles)
	    if l:inputfiles[l:inputfile][0] == 'include'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiles)
	    if l:inputfiles[l:inputfile][0] == 'bib'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	echohl None
    endif
    return l:inputfiles
endfunction
endif
" ----------------- FIND BIB FILES ----------------------------------	
if !exists("*FindBibFiles")
function! FindBibFiles(...)

    if a:0==0
	let l:bufname=bufname("%")
    else
	let l:bufname=a:1
    endif

    let b:texfile=readfile(l:bufname)
    let s:i=0
    let s:bibline=[]
    " find all lines which define bibliography files
    for line in b:texfile
	if line =~ "\\\\bibliography{"
	    let s:bibline=add(s:bibline,line) 
	    let s:i+=1
	endif
    endfor
    let l:nr=s:i
    let s:i=1
    let files=""
    " make a comma separated list of bibfiles
    for l:line in s:bibline
	if s:i==1
	    let files=substitute(l:line,"\\\\bibliography{\\(.*\\)}","\\1","") . ","
	else
	    let files=files . substitute(l:line,"\\\\bibliography{\\(.*\\)}","\\1","") . "," 
	endif
	let s:i+=1
    endfor

    " rewrite files into a vim list
    let l:allbibfiles=split(files,',')
    
    " add the list b:bibfiles 
    if exists('b:bibfiles')
	call extend(l:allbibfiles,b:bibfiles)
    endif
    
    " clear the list s:allbibfile from double entries 
    let l:callbibfiles=[]
    for l:f in l:allbibfiles
	if count(l:callbibfiles,l:f) == 0
	    call add(l:callbibfiles,l:f)
	endif
    endfor
    let l:allbibfiles=deepcopy(l:callbibfiles)

    " this variable will store unreadable bibfiles:    
    let s:notreadablebibfiles=[]

    " this variable will store the final result:   
    let l:bibfiles={}

    for l:f in l:allbibfiles
	if filereadable(b:outdir . s:append(l:f,'.bib')) || filereadable(s:append($BIBINPUTS,"/") . s:append(l:f,'.bib'))
	    call extend(l:bibfiles,{l:f : [ 'bib' , fnamemodify(expand("%"),":p") ] })
	else
	    " echo warning if a bibfile is not readable
	    echohl WarningMsg | echomsg "Bibfile " . l:f . ".bib is not readable." | echohl None
	    if count(s:notreadablebibfiles,l:f) == 0 
		call add(s:notreadablebibfiles,l:f)
	    endif
	endif
    endfor

    " return the list  of readable bibfiles
    return l:bibfiles
endfunction
endif
"--------------------SHOW ALL DEFINITIONS----------------------------

function! s:make_defi_dict()
    "dictionary: { input_file : [[ begining_line,end_line],...] }
    let l:defi_dict={}


    let l:inputfiles=FindInputFiles(bufname("%"),"0")
    let b:if=l:inputfiles
    let l:input_files=[]

    for l:inputfile in keys(l:inputfiles)
	if l:inputfiles[l:inputfile][0] != "bib"
	    let l:input_file=s:append(l:inputfile,'.tex')
	    if filereadable(b:outdir . '/' . l:input_file)
		let l:input_file=b:outdir . '/' . l:input_file
	    else
		let l:input_file=findfile(l:inputfile,g:texmf . '**')
	    endif
	    call add(l:input_files, l:input_file)
	endif
    endfor

    for l:inputfile in l:input_files
	let l:defi_dict[l:inputfile]=[]
	" do not search for definitions in bib files 
	"TODO: it skips lines somehow. 
	let l:ifile=readfile(l:inputfile)
	
	" search for definitions
	let l:lnr=1
	while l:lnr <= len(l:ifile)

	    let l:match=0

	    let l:line=l:ifile[l:lnr-1]
	    if substitute(l:line,'%.*','','') =~ '\\def'

		let l:b_line=l:lnr
		let l:open=s:count(l:line,'{')    
		let l:close=s:count(l:line,'}')

		let l:lnr+=1	
		while l:open != l:close
		    "go to next line and count if the definition ends at
		    "this line
		    let l:line=l:ifile[l:lnr-1]
		    let l:open+=s:count(l:line,'{')    
		    let l:close+=s:count(l:line,'}')
		    let l:lnr+=1	
		endwhile
		let l:e_line=l:lnr-1
		call add(l:defi_dict[l:inputfile], [ l:b_line, l:e_line ])
	    else
		let l:lnr+=1
	    endif
	endwhile
    endfor
    let b:dd=l:defi_dict " DEBUG
    return l:defi_dict
endfunction

if !exists("*DefiSearch")
function! DefiSearch(...)

    if a:0 == 0
	let l:pattern=""
    else
	let l:pattern=a:1
    endif

    let l:ddict=s:make_defi_dict()
    let b:dd=l:ddict

    " open new buffer
    let l:openbuffer=" +setl\\ buftype=nofile\\ nospell " . fnameescape("Definitions:" . l:pattern )
    if g:vertical ==1
	let l:openbuffer="vsplit " . l:openbuffer 
    else
	let l:openbuffer="split " . l:openbuffer 
    endif

    " remove the old buffer and open new one instead
    if bufexists("Definitions:" . l:pattern)
	exe "bd " . bufnr("Definitions:" . l:pattern) 
    endif
    silent exe l:openbuffer
    map <buffer> q	:bd<CR>

    for l:inputfile in keys(l:ddict)
	let l:ifile=readfile(l:inputfile)
	for l:range in l:ddict[l:inputfile]

	    if l:ifile[l:range[0]-1] =~ l:pattern
		" print the lines into the buffer
		let l:i=0
		let l:c=0
		" add an empty line if the definition is longer than one line
		if l:range[0] != l:range[1]
		    call setline(line('$')+1,'')
		    let l:i+=1
		endif
		while l:c <= l:range[1]-l:range[0] 
		    let l:line=l:range[0]+l:c
		    call setline(line('$')+1,l:ifile[l:line-1])
		    let l:i+=1
		    let l:c+=1
		endwhile
	    endif

	endfor
    endfor
    try
	setl filetype=tex
    catch /Cannot redefine function DefiSearch/
    finally
	setl filetype=tex
    endtry
endfunction
endif
"--------------------SET THE PROJECT NAME----------------------------
" store a list of all input files associated to some file
fun! s:setprojectname()
    if !exists("g:inputfiles")
	let g:inputfiles=FindInputFiles(expand("%"),0)
    else
	call extend(g:inputfiles,FindInputFiles(bufname("%"),0))
    endif

    if !exists("g:atp_project")
	" the main file is not an input file
	if index(keys(g:inputfiles),fnamemodify(bufname("%"),":t:r")) == '-1' &&
	 \ index(keys(g:inputfiles),fnamemodify(bufname("%"),":t"))   == '-1' &&
	 \ index(keys(g:inputfiles),fnamemodify(bufname("%"),":p:r")) == '-1' &&
	 \ index(keys(g:inputfiles),fnamemodify(bufname("%"),":p"))   == '-1' 
	    let g:mainfile=fnamemodify(expand("%"),":p")
	elseif index(keys(g:inputfiles),fnamemodify(bufname("%"),":t")) != '-1'
	    let g:mainfile=g:inputfiles[fnamemodify(bufname("%"),":t")][1]
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(g:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1'
	    let g:mainfile=g:inputfiles[fnamemodify(bufname("%"),":t:r")][1]
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(g:inputfiles),fnamemodify(bufname("%"),":p:r")) != '-1' 
	    let g:mainfile=g:inputfiles[fnamemodify(bufname("%"),":p:r")][1]
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(g:inputfiles),fnamemodify(bufname("%"),":p"))   != '-1' 
	    let g:mainfile=g:inputfiles[fnamemodify(bufname("%"),":p")][1]
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	endif
    elseif exists("g:atp_project")
	let g:mainfile=g:atp_project
    endif
"     echomsg "DEBUG PNAME " . g:mainfile
endfun

au BufEnter *.tex :call s:setprojectname()

" let &l:errorfile=b:outdir . fnameescape(fnamemodify(expand("%"),":t:r")) . ".log"
if !exists("*SetErrorFile")
function! SetErrorFile()

    " set b:outdir if it is not set
    if !exists("b:outdir")
	call s:setoutdir(0)
    endif

    " set the g:mainfile varibale if it is not set (the project name)
    if !exists("g:mainfile")
	call s:setprojectname()
    endif

"     let l:ef=b:outdir . fnamemodify(expand("%"),":t:r") . ".log"
    let l:ef=b:outdir . fnamemodify(g:mainfile,":t:r") . ".log"
    let &l:errorfile=l:ef
endfunction
endif

au BufEnter *.tex call SetErrorFile()

" This options are set also when editing .cls files.
function! s:setoutdir(arg)
    " first we have to check if this is not a project file
    if exists("g:atp_project") || exists("g:inputfiles") && 
		\ ( index(keys(g:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1' || 
		\ index(keys(g:inputfiles),fnamemodify(bufname("%"),":t")) != '-1' )
	    " if we are in a project input/include file take the correct value of b:outdir from the s:outdir_dict dictionary.
	    
	    if index(keys(g:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1'
		let b:outdir=g:outdir_dict[g:inputfiles[fnamemodify(bufname("%"),":t:r")][1]]
	    elseif index(keys(g:inputfiles),fnamemodify(bufname("%"),":t")) != '-1'
		let b:outdir=g:outdir_dict[g:inputfiles[fnamemodify(bufname("%"),":t")][1]]
	    endif
    else
	
	    " if we are not in a project input/include file set the b:outdir
	    " variable	

	    " if the user want to be asked for b:outdir
	    if g:askfortheoutdir == 1 
		let b:outdir=input("Where to put output? do not escape white spaces ")
	    endif

	    if ( get(getbufvar(bufname("%"),""),"outdir","optionnotset") == "optionnotset" 
			\ && g:askfortheoutdir != 1 
			\ || b:outdir == "" && g:askfortheoutdir == 1 )
			\ && !exists("$TEXMFOUTPUT")
		 let b:outdir=fnamemodify(resolve(expand("%:p")),":h") . "/"
		 echoh WarningMsg | echomsg "Output Directory "b:outdir | echoh None

	    elseif exists("$TEXMFOUTPUT")
		 let b:outdir=$TEXMFOUTPUT 
	    endif	

	    " if arg != 0 then set errorfile option accordingly to b:outdir
	    if bufname("") =~ ".tex$" && a:arg != 0
		 call SetErrorFile()
	    endif

	    if exists("g:outdir_dict")
		let g:outdir_dict=extend(g:outdir_dict, {fnamemodify(bufname("%"),":p") : b:outdir })
	    else
		let g:outdir_dict={ fnamemodify(bufname("%"),":p") : b:outdir }
	    endif
    endif
endfunction


" these are all buffer related variables:
let s:optionsDict= { 	"texoptions" 	: "", 		"reloadonerror" : "0", 
		\	"openviewer" 	: "1", 		"autex" 	: "1", 
		\	"Viewer" 	: "xpdf", 	"ViewerOptions" : "", 
		\	"XpdfServer" 	: fnamemodify(expand("%"),":t"), 
		\	"outdir" 	: fnameescape(fnamemodify(resolve(expand("%:p")),":h")) . "/",
		\	"texcompiler" 	: "pdflatex",	"auruns"	: "1",
		\ 	"truncate_status_section"	: "40" }
let s:ask={ "ask" : "0" }
if !exists("g:rmcommand") && executable("perltrash")
    let g:rmcommand="perltrash"
endif
if !exists("g:askfortheoutdir")
    let g:askfortheoutdir=0
endif
if !exists("g:texextensions")
    let g:texextensions=["aux", "log", "bbl", "blg", "spl", "snm", "nav", "thm", "brf", "out", "toc", "mpx", "idx", "maf", "blg", "glo", "mtc[0-9]", "mtc1[0-9]"]
endif
if !exists("g:keep")
    let g:keep=["log","aux","toc","bbl"]
endif
if !exists("g:printingoptions")
    let g:printingoptions=''
endif
if !exists("g:atp_ssh")
    let g:atp_ssh=substitute(system("whoami"),'\n','','') . "@localhost"
endif
" opens bibsearch results in vertically split window.
if !exists("g:vertical")
    let g:vertical=1
endif
if !exists("g:matchpair")
    let g:matchpair="(:),\\(:\\),[:],\\[:\\],{:}"
endif
if !exists("g:texmf")
    let g:texmf=$HOME . "/texmf"
endif
if !exists("$BIBINPUTS")
    let $BIBINPUTS=substitute(g:texmf,'\/\s*^','','') . "/bibtex"
endif
if !exists("g:atp_compare_embedded_comments") || g:atp_compare_embedded_comments != 1
    let g:atp_compare_embedded_comments = 0
endif
if !exists("g:atp_compare_double_empty_lines") || g:atp_compare_double_empty_lines != 0
    let g:atp_compare_double_empty_lines = 1
endif
"TODO: put toc_window_with and labels_window_width into DOC file
if !exists("t:toc_window_width")
    if exists("g:toc_window_width")
	let t:toc_window_width=g:toc_window_width
    else
	let t:toc_window_width=30
    endif
endif
if !exists("t:labels_window_width")
    if exists("g:labels_window_width")
	let t:labels_window_width=g:labels_window_width
    else
	let t:labels_window_width=30
    endif
endif

" This function sets options (values of buffer related variables) which were
" not set by the user to their default values.
function! s:setoptions()
    let s:optionsKeys=keys(s:optionsDict)
    let s:optionsinuseDict=getbufvar(bufname("%"),"")

    "for each key in s:optionsKeys set the corresponding variable to its default
    "value unless it was already set in .vimrc file.
    for l:key in s:optionsKeys

	if get(s:optionsinuseDict,l:key,"optionnotset") == "optionnotset" && l:key != "outdir" 
	    call setbufvar(bufname("%"),l:key,s:optionsDict[l:key])
	elseif l:key == "outdir"
	    
	    " set b:outdir and the value of errorfile option
	    call s:setoutdir(1)
	    let s:ask["ask"] = 1
	endif
    endfor
endfunction
call s:setoptions()

if !exists("*ShowOptions")
function! ShowOptions(...)
    let s:bibfiles=keys(FindBibFiles(bufname("%")))
    if a:0 == 0
	echomsg "variable=local value"  
	echohl BibResultsMatch
	echomsg "b:texcompiler=   " . b:texcompiler 
	echomsg "b:texoptions=    " . b:texoptions 
	echomsg "b:autex=         " . b:autex 
	echomsg "b:outdir=        " . b:outdir 
	echomsg "b:Viewer=        " . b:Viewer 
	echomsg "b:ViewerOptions=   " . b:ViewerOptions 
	echohl BibResultsGeneral
	if b:Viewer == "xpdf"
	    echomsg "    b:XpdfServer=    " . b:XpdfServer 
	    echomsg "    b:reloadonerror= " . b:reloadonerror 
	endif
	echomsg "b:openviewer=    " . b:openviewer 
	echomsg "g:askfortheoutdir=" . g:askfortheoutdir 
	if (exists("g:atp_statusline") && g:atp_statusline == '1') || !exists("g:atp_statusline")
	    echomsg "status line set by atp"
	endif
	echohl BibResultsMatch
	echomsg "g:keep=          " . string(g:keep)  
	echomsg "g:texextensions= " . string(g:texextensions)
	echomsg "g:rmcommand=     " . g:rmcommand
	echohl BibResultsFileNames
	echomsg "g:defaultbibflags=     " . g:defaultbibflags
	echomsg "g:defaultallbibflags=  " . g:defaultallbibflags
	echomsg "Available Flags        " . string(keys(g:bibflagsdict))
	echomsg "Available KeyWordFlags " . string(keys(g:kwflagsdict))
	echohl BibResultsMatch
	if exists('b:lastbibflags')
	    echomsg "b:lastbibflags=    " . b:lastbibflags
	endif
	echohl None
	echomsg "g:bibentries=    " . string(g:bibentries)
	echohl BibResultsFileNames
	if exists('b:bibfiles')
	    echomsg "b:bibfiles=      " .  string(b:bibfiles)
	endif
	if exists('s:bibfiles')
	    echomsg "s:bibfiles=      " .  string(s:bibfiles)	. " bibfiles used by atp."
	endif
	if exists('s:notreadablebibfiles')
	    echomsg "s:notreadablebibfiles=" .  string(s:notreadablebibfiles)
	endif
	echohl None
    elseif a:0>=1 
	echohl BibResultsMatch
	echomsg "b:texcompiler=   " . b:texcompiler . "  [" . s:optionsDict["texcompiler"] . "]" 
	echomsg "b:texoptions=    " . b:texoptions . "  [" . s:optionsDict["texoptions"] . "]" 
	echomsg "b:autex=         " . b:autex . "  [" . s:optionsDict["autex"] . "]" 
	echomsg "b:outdir=        " . b:outdir . "  [" . s:optionsDict["outdir"] . "]" 
	echomsg "b:Viewer=        " . b:Viewer . "  [" . s:optionsDict["Viewer"] . "]" 
	echomsg "b:ViewerOptions=   " . b:ViewerOptions . "  [" . s:optionsDict["ViewerOptions"] . "]" 
	echohl None
	if b:Viewer == "xpdf"
	    echomsg "    b:XpdfServer=    " . b:XpdfServer . "  [" . s:optionsDict["XpdfServer"] . "]" 
	    echomsg "    b:reloadonerror= " . b:reloadonerror . "  [" . s:optionsDict["reloadonerror"] . "]" 
	endif
	echomsg "g:askfortheoutdir=" . g:askfortheoutdir . "  [" . s:optionsDict["askfortheoutdir"] . "]" 
	echomsg "b:openviewer=    " . b:openviewer . "  [" . s:optionsDict["openviewer"] . "]" 
	echo
	echohl BibResultsMatch
	echomsg "g:keep=          " . string(g:keep)  
	echomsg "g:texextensions= " . string(g:texextensions)
	echomsg "g:rmcommand=     " . g:rmcommand
	echohl None
	echohl BibResultsFileNames
	echomsg "g:defaultbibflags=     " . g:defaultbibflags
	echomsg "g:defaultallbibflags=  " . g:defaultallbibflags
	echomsg " "
	echomsg "Available Flags        "
	echomsg "   g:bibflagsdict=     " . string(items(g:bibflagsdict))
	echomsg " "
	echomsg "Available KeyWordFlags "
	echomsg "   g:kwflagsdict=      " . string(items(g:kwflagsdict))
	echomsg " "
	echohl BibResultsMatch
	if exists('b:lastbibflags')
	    echomsg "b:lastbibflags=" . b:lastbibflags
	endif
	echohl BibResultsLabel
	echomsg "g:bibentries=" . string(g:bibentries) . "  ['article', 'book', 'booklet', 'conference', 'inbook', 'incollection', 'inproceedings', 'manual', 'mastertheosis', 'misc', 'phdthesis', 'proceedings', 'techreport', 'unpublished']"
	echohl BibResultsFileNames
	if exists('b:bibfiles')
	    echomsg "b:bibfiles=  " .  string(b:bibfiles)
	endif
	if exists('s:bibfiles')
	    echomsg "s:bibfiles=      " .  string(s:bibfiles)	. " bibfiles used by atp."
	endif
	if exists('s:notreadablebibfiles')
	    echomsg "s:notreadablebibfiles=" .  string(s:notreadablebibfiles)
	endif
	echohl None
	echomsg ""
    endif
endfunction
endif

function! ATPStatusOutDir()
let s:status=""
if exists("b:outdir")
    if b:outdir != "" 
	let s:status= s:status . "Output dir: " . pathshorten(substitute(b:outdir,"\/\s*$","","")) 
    else
	let s:status= s:status . "Please set the Output directory, b:outdir"
    endif
endif	
    return s:status
endfunction

syntax match atp_statustitle 	/.*/ 
syntax match atp_statussection 	/.*/ 
syntax match atp_statusoutdir 	/.*/ 
hi link atp_statustitle Number
hi link atp_statussection Title
hi link atp_statusoutdir String
if !exists("*ATPStatus")
function! ATPStatus()
"     echomsg "Status line set by ATP." 
    if &filetype == 'tex'
	let &statusline='%<%f %(%h%m%r %)  %{CTOC("return")}%=%{ATPStatusOutDir()}  %-14.16(%l,%c%V%)%P'
    else 
	let  &statusline='%<%f %(%h%m%r %)  %=%{ATPStatusOutDir()}  %-14.16(%l,%c%V%)%P'
    endif
endfunction
endif
if (exists("g:atp_statusline") && g:atp_statusline == '1') || !exists("g:atp_statusline")
     au BufWinEnter *.tex call ATPStatus()
endif
let b:texruns=0
let b:log=0	
let b:ftype=getftype(expand("%:p"))	
let s:texinteraction="nonstopmode"
compiler tex
let s:lockef=1
au BufRead $l:errorfile setlocal autoread 

"--------- FUNCTIONs -----------------------------------------------------
function! s:outdir()
    if b:outdir !~ "\/$"
	let b:outdir=b:outdir . "/"
    endif
endfunction

if !exists("*ViewOutput")
function! ViewOutput()
    call s:outdir()
    if b:texcompiler == "pdftex" || b:texcompiler == "pdflatex"
	let l:ext = ".pdf"
    else
	let l:ext = ".dvi"	
    endif
    let l:outfile=b:outdir . (fnamemodify(expand("%"),":t:r")) . l:ext
    if b:Viewer == "xpdf"	
	let l:viewer=b:Viewer . " -remote " . shellescape(b:XpdfServer) . " " . b:ViewerOptions 
    else
	let l:viewer=b:Viewer  . " " . b:ViewerOptions
    endif
    let l:view=l:viewer . " " . shellescape(l:outfile)  . " &"
		let b:outfile=l:outfile
    if filereadable(l:outfile)
	if b:Viewer == "xpdf"	
	    let b:view=l:view
	    call system(l:view)
	else
	    call system(l:view)
	    redraw!
	endif
    else
	    echomsg "Output file do not exists. Calling " . b:texcompiler
	    call s:compiler(0,1,1,0,"AU",g:mainfile)
    endif	
endfunction
endif
"-------------------------------------------------------------------------
function! s:getpid()
	let s:command="ps -ef | grep -v " . $SHELL  . " | grep " . b:texcompiler . " | grep -v grep | grep " . fnameescape(expand("%")) . " | awk '{print $2}'"
	let s:var=substitute(system(s:command),'\D',' ','')
	return s:var
endfunction

if !exists("*Getpid")
function! Getpid()
	let s:var=s:getpid()
	if s:var != ""
		echomsg b:texcompiler . " pid " . s:var 
	else
		echomsg b:texcompiler . " is not running"
	endif
endfunction
endif

if !exists("*s:xpdfpid")
function! s:xpdfpid() 
    let s:checkxpdf="ps -ef | grep -v grep | grep '-remote '" . shellescape(b:XpdfServer) . " | awk '{print $2}'"
    return substitute(system(s:checkxpdf),'\D','','')
endfunction
endif
"-------------------------------------------------------------------------
function! s:compare(file)
    let l:buffer=getbufline(bufname("%"),"1","$")

    " rewrite l:buffer to remove all commands 
    let l:buffer=filter(l:buffer, 'v:val !~ "^\s*%"')

    let l:i = 0
    if g:atp_compare_double_empty_lines == 0 || g:atp_compare_embedded_comments == 0
    while l:i < len(l:buffer)-1
	let l:rem=0
	" remove comment lines at the end of a line
	if g:atp_compare_embedded_comments == 0
	    let l:buffer[l:i] = substitute(l:buffer[l:i],'%.*$','','')
	endif

	" remove double empty lines (i.e. from two conecutive empty lines
	" the first one is deleted, the second remains), if the line was
	" removed we do not need to add 1 to l:i (this is the role of
	" l:rem).
	if g:atp_compare_double_empty_lines == 0 && l:i< len(l:buffer)-2
	    if l:buffer[l:i] =~ '^\s*$' && l:buffer[l:i+1] =~ '^\s*$'
		call remove(l:buffer,l:i)
		let l:rem=1
	    endif
	endif
	if l:rem == 0
	    let l:i+=1
	endif
    endwhile
    endif
 
    " do the same with a:file
    let l:file=filter(a:file, 'v:val !~ "^\s*%"')

    let l:i = 0
    if g:atp_compare_double_empty_lines == 0 || g:atp_compare_embedded_comments == 0
    while l:i < len(l:file)-1
	let l:rem=0
	" remove comment lines at the end of a line
	if g:atp_compare_embedded_comments == 0
	    let l:file[l:i] = substitute(a:file[l:i],'%.*$','','')
	endif
	
	" remove double empty lines (i.e. from two conecutive empty lines
	" the first one is deleted, the second remains), if the line was
	" removed we do not need to add 1 to l:i (this is the role of
	" l:rem).
	if g:atp_compare_double_empty_lines == 0 && l:i < len(l:file)-2
	    if l:file[l:i] =~ '^\s*$' && l:file[l:i+1] =~ '^\s*$'
		call remove(l:file,l:i)
		let l:rem=1
	    endif
	endif
	if l:rem == 0
	    let l:i+=1
	endif
    endwhile
    endif

    return l:file !=# l:buffer
endfunction
"-------------------------------------------------------------------------
function! s:copy(input,output)
	call writefile(readfile(a:input),a:output)
endfunction

" This is the MAIN FUNCTION which sets the command and calls it.
function! s:compiler(bibtex,start,runs,verbose,command,filename)
    call s:outdir()
    	" IF b:texcompiler is not compatible with the viewer
	if b:texcompiler =~ "^\s*pdf" && b:Viewer == "xdvi" ? 1 :  b:texcompiler !~ "^\s*pdf" && (b:Viewer == "xpdf" || b:Viewer == "epdfview" || b:Viewer == "acroread" || b:Viewer == "kpdf")
	     
	    echohl WaningMsg | echomsg "Your"b:texcompiler"and"b:Viewer"are not compatible:" 
	    echomsg "b:texcompiler=" . b:texcompiler	
	    echomsg "b:Viewer=" . b:Viewer	
	endif

	" there is no need to run more than s:runlimit (=5) consecutive runs
	" this prevents from running tex as many times as the current line
	" what can be done by a mistake using the range for the command.
	if a:runs > s:runlimit
	    let l:runs = s:runlimit
	else
	    let l:runs = a:runs
	endif

	let s:tmpfile=tempname()
	let s:dir=fnamemodify(s:tmpfile,":h")
	let s:job=fnamemodify(s:tmpfile,":t")
	if b:texcompiler == "pdftex" || b:texcompiler == "pdflatex"
	    let l:ext = ".pdf"
	else
	    let l:ext = ".dvi"	
	endif

	let l:outfile = b:outdir . fnamemodify(a:filename,":t:r") . l:ext
	let l:outaux  = b:outdir . fnamemodify(a:filename,":t:r") . ".aux"
	let l:outlog  = b:outdir . fnamemodify(a:filename,":t:r") . ".log"
"	COPY IMPORTANT FILES TO TEMP DIRECTORY WITH CORRECT NAME 
	let l:list=filter(copy(g:keep),'v:val != "log"')
	for l:i in l:list
"   		echomsg "DEBUG extensions" l:i
	    let l:ftc=b:outdir . fnamemodify(expand("%"),":t:r") . "." . l:i
"  		echomsg "DEBUG file to copy"l:ftc
	    if filereadable(l:ftc)
		call s:copy(l:ftc,s:tmpfile . "." . l:i)
	    endif
	endfor
" 	HANDLE XPDF RELOAD 
	if b:Viewer == "xpdf"
	    if a:start == 1
		"if xpdf is not running and we want to run it.
		let s:xpdfreload = b:Viewer . " -remote " . shellescape(b:XpdfServer) . " " . shellescape(l:outfile)
	    else
		if s:xpdfpid() != ""
		    "if xpdf is running (then we want to reload it).
		    "This is where I use ps command to check if xpdf is
		    "running.
		    let s:xpdfreload = b:Viewer . " -remote " . shellescape(b:XpdfServer) . " -reload"	
		else
		    "if xpdf is not running (but we do not want
		    "to run it).
		    let s:xpdfreload = ""
		endif
	    endif
	else
	    if a:start == 1
		" if b:Viewer is not running and we want to open it.
		let s:xpdfreload = b:Viewer . " " . shellescape(l:outfile) 
	    else
		" if b:Viewer is not running then we do not want to
		" open it.
		let s:xpdfreload = ""
	    endif	
	endif
"  	echomsg "DEBUG xpdfreload="s:xpdfreload
" 	IF OPENINIG NON EXISTING OUTPUT FILE
"	only xpdf needs to be run before (we are going to reload it)
"	TODO THIS DO NOT WORKS!!!
	if a:start == 1 && b:Viewer == "xpdf"
	    let s:start = b:Viewer . " -remote " . shellescape(b:XpdfServer) . " " . b:ViewerOptions . " & "
	else
	    let s:start = ""	
	endif
"	SET THE COMMAND 
	let s:comp=b:texcompiler . " " . b:texoptions . " -interaction " . s:texinteraction . " -output-directory " . s:dir . " -jobname " . s:job . " " . a:filename
	let s:vcomp=b:texcompiler . " " . b:texoptions  . " -interaction errorstopmode -output-directory " . s:dir . " -jobname " . s:job . " " . a:filename
	if a:verbose == 0 || l:runs > 1
	    let s:texcomp=s:comp
	else
	    let s:texcomp=s:vcomp
	endif
	if l:runs >= 2 && a:bibtex != 1
	    " how many times we wan to call b:texcompiler
	    let l:i=1
	    while l:i < l:runs - 1
		let l:i+=1
		let s:texcomp=s:texcomp . " ; " . s:comp
	    endwhile
	    if a:verbose == 0
		let s:texcomp=s:texcomp . " ; " . s:comp
	    else
		let s:texcomp=s:texcomp . " ; " . s:vcomp
	    endif
"   		echomsg "DEBUG runs s:texcomp="s:texcomp
	endif
"  	    	echomsg "DEBUG X command s:texcomp=" s:texcomp
	if a:bibtex == 1
	    if filereadable(l:outaux)
		call s:copy(l:outaux,s:tmpfile . ".aux")
		let s:texcomp="bibtex " . s:tmpfile . ".aux ; " . s:comp . "  1>/dev/null 2>&1 "
	    else
		let s:texcomp=s:comp . " ; clear ; bibtex " . s:tmpfile . ".aux ; " . s:comp . " 1>/dev/null 2>&1 "
	    endif
	    if a:verbose != 0
		let s:texcomp=s:texcomp . " ; " . s:vcomp
	    else
		let s:texcomp=s:texcomp . " ; " . s:comp
	    endif
	endif
	let s:cpoption="--remove-destination "
	let s:cpoutfile="cp " . s:cpoption . shellescape(s:tmpfile . l:ext) . " " . shellescape(l:outfile) 
	let s:command="(" . s:texcomp . " && (" . s:cpoutfile . " ; " . s:xpdfreload . ") || (" . s:cpoutfile . ")" 
	let s:copy=""
	let l:j=1
	for l:i in g:keep 
" ToDo: Windows compatible?
" 	    Before copy, check if the file exists (for example toc files are
" 	    not always created) 
	    let s:copycmd=" [[ -e " . shellescape(s:tmpfile . "." . l:i) . " ]] && cp " . s:cpoption . " " . shellescape(s:tmpfile . "." . l:i) . " " . shellescape(b:outdir . (fnamemodify(expand("%"),":t:r")) . "." . l:i) 
"   		echomsg "DEBUG 2 copycmd"s:copycmd
	    if l:j == 1
		let s:copy=s:copycmd
	    else
		let s:copy=s:copy . " ; " . s:copycmd	  
	    endif
	    let l:j+=1
	endfor
	    let s:command=s:command . " ; " . s:copy
 	let s:rmtmp="rm " . s:tmpfile . "*" 
	let s:command=s:command . " ; " . s:rmtmp . ")&"
	if a:start == 1 
	    let s:command=s:start . s:command
	endif
	let b:texcommand=s:command
	let s:backup=&backup
	let s:writebackup=&writebackup
	if a:command == "AU"  
	    if &backup || &writebackup | setlocal nobackup | setlocal nowritebackup | endif
	endif
" 		echomsg "DEBUG writting backup=" . &backup . " writebackup=" . &writebackup
	silent! w
	if a:command == "AU"  
	    let &l:backup=s:backup 
	    let &l:writebackup=s:writebackup 
	endif
	if a:verbose == 0
" 		echomsg "DEBUG compile s:command="s:command
	    call system(s:command)
	else
	    let s:command="!clear;" . s:texcomp . " ; " . s:cpoutfile . " ; " . s:copy
	    let b:texcommand=s:command
" 		echomsg "DEBUG verbose compile s:command=" . s:command
	    exe s:command
	endif
endfunction
"-------------------------------------------------------------------------
function! s:auTeX()
   if b:autex	
    " if the file (or input file is modified) compile the document 
    if s:compare(readfile(expand("%")))
	call s:compiler(0,0,b:auruns,0,"AU",g:mainfile)
	redraw
    endif
   endif
endfunction
if !exists('#CursorHold#' . $HOME . '/*.tex')
    au CursorHold $HOME/*.tex call s:auTeX()
endif
"-------------------------------------------------------------------------
if !exists("*TEX")
function! TEX(...)
let s:name=tempname()
if a:0 >= 1
    if a:1 > 2 && a:1 <= 5
	echomsg b:texcompiler . " will run " . a:1 . " times."
    elseif a:1 == 2
	echomsg b:texcompiler . " will run twice."
    elseif a:1 == 1
	echomsg b:texcompiler . " will run once."
    elseif a:1 > 5
	echomsg b:texcompiler . " will run " . s:runlimit . " times."
    endif
    call s:compiler(0,0,a:1,0,"COM",g:mainfile)
elseif a:0 == 0
    call s:compiler(0,0,1,0,"COM",g:mainfile)
endif
endfunction
endif

" command! -buffer -count=1 TEX	:call TEX(<count>)		 
if !exists("*ToggleAuTeX")
function! ToggleAuTeX()
  if b:autex != 1
    let b:autex=1	
    echo "automatic tex processing is ON"
  else
    let b:autex=0
    echo "automatic tex processing is OFF"
endif
endfunction
endif
if !exists("*VTEX")
function! VTEX(...)
    let s:name=tempname()
if a:0 >= 1
    if a:1 > 2
	echomsg b:texcompiler . " will run " . a:1 . " times."
    elseif a:1 == 2
	echomsg b:texcompiler . " will run twice."
    elseif a:1 == 1
	echomsg b:texcompiler . " will run once."
    endif
    sleep 1
    call s:compiler(0,0,a:1,1,"COM",g:mainfile)
else
    call s:compiler(0,0,1,1,"COM",g:mainfile)
endif
endfunction
endif
"-------------------------------------------------------------------------
if !exists("*SimpleBibtex")
function! SimpleBibtex()
    call s:outdir() 
    let l:bibcommand="bibtex "
    let l:auxfile=b:outdir . (fnamemodify(expand("%"),":t:r")) . ".aux"
    if filereadable(l:auxfile)
	let l:command=l:bibcommand . shellescape(l:auxfile)
	echo system(l:command)
    else
	echomsg "No aux file in " . b:outdir
    endif
"  	!clear;if [[ -h "%" ]];then realname=`readlink "%"`;realdir=`dirname "$realname"`;b_name=`basename "$realname" .tex`;else realdir="%:p:h";b_name="%:r";fi;bibtex "$realdir/$b_name".aux
endfunction
endif

if !exists("*Bibtex")
function! Bibtex(...)
    let s:bibname=tempname()
    let s:auxf=s:bibname . ".aux"
    if a:0 == 0
"  	    echomsg "DEBUG Bibtex"
	call s:compiler(1,0,0,0,"COM",g:mainfile)
    else
"  	    echomsg "DEBUG Bibtex verbose"
	call s:compiler(1,0,0,1,"COM",g:mainfile)
    endif
endfunction
endif

"-------------------------------------------------------------------------

" TeX LOG FILE
if &buftype == 'quickfix'
	setlocal modifiable
	setlocal autoread
endif	

"-------------------------------------------------------------------------
if !exists("*Delete")
function! Delete()
    call s:outdir()
    let s:error=0
    for l:ext in g:texextensions
	if executable(g:rmcommand)
	    if g:rmcommand =~ "^\s*rm\p*" || g:rmcommand =~ "^\s*perltrash\p*"
		let l:rm=g:rmcommand . " " . shellescape(b:outdir) . "*." . l:ext . " 2>/dev/null && echo Removed ./*" . l:ext . " files"
	    endif
" 	    echomsg "DEBUG " l:rm
	echo system(l:rm)
	else
	    let s:error=1
		let l:file=b:outdir . fnamemodify(expand("%"),":t:r") . "." . l:ext
		if delete(l:file) == 0
		    echo "Removed " . l:file 
	    endif
	endif
    endfor
" 	if s:error
" 		echo "Pleas set g:rmcommand to clear the working directory"
" 	endif
endfunction
endif

"-------------------------------------------------------------------------
if !exists("*OpenLog")
function! OpenLog()
    if filereadable(&l:errorfile)
	exe "tabe +set\\ nospell\\ ruler " . &l:errorfile
    else
	echo "No log file"
    endif
endfunction
endif

if !exists("*TexLog")
function! TexLog(options)
    if executable("texloganalyser")
       let s:command="texloganalyser " . a:options . " " . &l:errorfile
       echo system(s:command)
    else	
       echo "Please install 'texloganalyser' to have this functionality. The perl program written by Thomas van Oudenhove."  
    endif
endfunction
endif

if !exists("*Pdffonts")
function! Pdffonts()
    if b:outdir !~ "\/$"
	b:outdir=b:outdir . "/"
    endif
    if executable("pdffonts")
	let s:command="pdffonts " . b:outdir . fnameescape(fnamemodify(expand("%"),":t:r")) . ".pdf"
	echo system(s:command)
    else
	echo "Please install 'pdffonts' to have this functionality. In 'gentoo' it is in the package 'app-text/poppler-utils'."  
    endif
endfunction	
endif

" function! s:setprintexpr()
"     if b:texcompiler == "pdftex" || b:texcompiler == "pdflatex"
" 	let s:ext = ".pdf"
"     else
" 	let s:ext = ".dvi"	
"     endif
"     let &printexpr="system('lpr' . (&printdevice == '' ? '' : ' -P' . &printdevice) . ' " . fnameescape(fnamemodify(expand("%"),":p:r")) . s:ext . "') . + v:shell_error"
" endfunction
" call s:setprintexpr()

if !exists("*Print")
function! Print(...)

    call s:outdir()

    " set the extension of the file to print
    if b:texcompiler == "pdftex" || b:texcompiler == "pdflatex" 
	let l:ext = ".pdf"
    elseif b:texcompiler =~ "lua"
	if b:texoptions == "" || b:texoptions =~ "output-format=\s*pdf"
	    let l:ext = ".pdf"
	else
	    let l:ext = ".dvi"
	endif
    else
	let l:ext = ".dvi"	
    endif

    " set the file to print
    let l:pfile=b:outdir . fnameescape(fnamemodify(expand("%"),":t:r")) . l:ext

    " set the printing command
    let l:lprcommand="lpr "
    if a:0 >= 2
	let l:lprcommand.= " " . a:2
    endif

    " print locally or remotely
    " the default is to print locally (g:atp_ssh=`whoami`@localhost)
    if exists("g:apt_ssh") 
	let l:server=strpart(g:atp_ssh,stridx(g:atp_ssh,"@")+1)
    else
	let l:server='locahost'
    endif
    if l:server =~ 'localhost'
	if a:0 == 0 || (a:0 != 0 && a:1 == 'default')
	    let l:com=l:lprcommand . " " . l:pfile
	else
	    let l:com=l:lprcommand . " -P " . a:1 . " " . l:pfile 
	endif
" 	call system(l:com)
	echo l:com
    " print over ssh on the server g:atp_ssh with the printer a:1 (or the
    " default system printer if a:0 == 0
    else 
	if a:0 == 0 || (a:0 != 0 && a:1 =~ 'default')
	    let l:com="cat " . l:pfile . " | ssh " . g:atp_ssh . " " . l:lprcommand
	else
	    let l:com="cat " . l:pfile . " | ssh " . g:atp_ssh . " " . l:lprcommand . " -P " . a:1 
	endif
	if g:printingoptions != "" || (a:0 >= 2 && a:2 != "")
	    if a:0 < 2
		echo "Printing Options: " . g:printingoptions
	    else
		echo a:2
	    endif
	    let l:ok = input("Is this OK? y/n")
	    if l:ok == 'y'
		if a:0 < 2
		    let l:printingoptions=g:printingoptions
		else
		    let l:printingoptions=a:2
		endif
	    else
		let l:printingoptions=input("Give printing options ")
	    endif
	else
	    let l:printingoptions=""
	endif
	let l:com = l:com . " " . l:printingoptions
	echo "\n " . l:com
	echo "Printing ..." 
	call system(l:com)
    endif

endfunction
endif

" it is used for completetion of the command SshPrint
if !exists("*ListPrinters")
function! ListPrinters(A,L,P)
    if exists("g:atp_ssh") && g:atp_ssh !~ '@localhost' && g:atp_ssh != ""
	let l:com="ssh -q " . g:atp_ssh . " lpstat -a | awk '{print $1}'"
    else
	let l:com="lpstat -a | awk '{print $1}'"
    endif
    return system(l:com)
endfunction
endif

"---------------------- SEARCH IN BIBFILES ----------------------
" This function counts accurence of a:keyword in string a:line, 
function! s:count(line,keyword)
    let l:line=a:line
    let l:i=0  
    while stridx(l:line,a:keyword) != '-1'
	if stridx(l:line,a:keyword) !='-1' 
            let l:line=strpart(l:line,stridx(l:line,a:keyword)+1)
	endif
	let l:i+=1
    endwhile
    return l:i
endfunction
let g:bibentries=['article', 'book', 'booklet', 'conference', 'inbook', 'incollection', 'inproceedings', 'manual', 'mastertheosis', 'misc', 'phdthesis', 'proceedings', 'techreport', 'unpublished']


"--------------------- SEARCH ENGINE ------------------------------ 
" ToDo should not search in comment lines.

" let s:bibfiles=FindBibFiles(bufname('%'))
function! s:searchbib(pattern) 
" 	echomsg "DEBUG pattern" a:pattern
    call s:outdir()
    let s:bibfiles=keys(FindBibFiles(bufname('%')))
    
    " Make a pattern which will match for the elements of the list g:bibentries
    let l:pattern = '^\s*@\(\%(\<article\>\)'
    for l:bibentry in g:bibentries
	if l:bibentry != 'article'
	let l:pattern=l:pattern . '\|\%(\<' . l:bibentry . '\>\)'
	endif
    endfor
    unlet l:bibentry
    let l:pattern=l:pattern . '\)'
    let b:bibentryline={} 
    
    " READ EACH BIBFILE IN TO DICTIONARY s:bibdict, WITH KEY NAME BEING THE bibfilename
    let s:bibdict={}
    let l:bibdict={}
    let b:bibdict={}				" DEBUG
    for l:f in s:bibfiles
	let s:bibdict[l:f]=[]

	" read the bibfile if it is in b:outdir or in $BIBINPUTS directory
	if filereadable(fnameescape(s:append(b:outdir,'/') . s:append(l:f,'.bib'))) 
	    let s:bibdict[l:f]=readfile(fnameescape(s:append(b:outdir,'/') . s:append(l:f,'.bib'))	
	else
	    let s:bibdict[l:f]=readfile(fnameescape(s:append($BIBINPUTS,'/') . s:append(l:f,'.bib')))
	endif
	let l:bibdict[l:f]=copy(s:bibdict[l:f])
	" clear the s:bibdict values from lines which begin with %    
	let l:x=0
	for l:line in s:bibdict[l:f]
	    if l:line =~ '^\s*\%(%\|@\cstring\)' 
		call remove(l:bibdict[l:f],l:x)
	    else
		let l:x+=1
	    endif
	endfor
	unlet l:line
    endfor
    for l:f in s:bibfiles
	let l:list=[]
	let l:nr=1
	    let b:bibdict[l:f]=l:bibdict[l:f]		" DEBUG
	for l:line in l:bibdict[l:f]
	    if substitute(l:line,'{\|}','','g') =~ a:pattern
		let l:true=1
		let l:t=0
		while l:true == 1
		    let l:tnr=l:nr-l:t
		   if l:bibdict[l:f][l:tnr-1] =~ l:pattern && l:tnr >= 0
		       let l:true=0
		       let l:list=add(l:list,l:tnr)
		   elseif l:tnr <= 0
		       let l:true=0
		   endif
		   let l:t+=1
		endwhile
	    endif
	    let l:nr+=1
	endfor
" CLEAR THE l:list FROM ENTRIES WHICH APPEAR TWICE OR MORE --> l:clist
    let l:pentry="A"		" We want to ensure that l:entry (a number) and p:entry are different
    for l:entry in l:list
	if l:entry != l:pentry
	    if count(l:list,l:entry) > 1
		while count(l:list,l:entry) > 1
		    let l:eind=index(l:list,l:entry)
		    call remove(l:list,l:eind)
		endwhile
	    endif 
	    let l:pentry=l:entry
	endif
    endfor
    let b:bibentryline=extend(b:bibentryline,{ l:f : l:list })
    endfor
"   CHECK EACH BIBFILE
    let l:bibresults={}
    for l:bibfile in keys(b:bibentryline)
	let l:f=l:bibfile . ".bib"
"s:bibdict[l:f])	CHECK EVERY STARTING LINE (we are going to read bibfile from starting
"	line till the last matching } 
 	let s:bibd={}
 	for l:linenr in b:bibentryline[l:bibfile]
"
" 	new algorithm is on the way, using searchpair function
" 	    l:time=0
" 	    l:true=1
" 	    let b:pair1=searchpair('(','',')','b')
" 	    let b:pair2=searchpair('{','','}','b')
" 	    let l:true=b:pair1+b:pair2
" 	    while l:true == 0
" 		let b:pair1p=b:pair1	
" 		let b:pair1=searchpair('(','',')','b')
" 		let b:pair2p=b:pair2	
" 		let b:pair2=searchpair('{','','}','b')
" 		let l:time+=1
" 	    endwhile
" 	    let l:bfieldline=l:time
	    
	    let l:nr=l:linenr-1
	    let l:i=s:count(get(l:bibdict[l:bibfile],l:linenr-1),"{")-s:count(get(l:bibdict[l:bibfile],l:linenr-1),"}")
	    let l:j=s:count(get(l:bibdict[l:bibfile],l:linenr-1),"(")-s:count(get(l:bibdict[l:bibfile],l:linenr-1),")") 
	    let s:lbibd={}
	    let s:lbibd["KEY"]=get(l:bibdict[l:bibfile],l:linenr-1)
	    let l:x=1
" we go from the first line of bibentry, i.e. @article{ or @article(, until the { and (
" will close. In each line we count brackets.	    
            while l:i>0	|| l:j>0
		let l:tlnr=l:x+l:linenr
		let l:pos=s:count(get(l:bibdict[l:bibfile],l:tlnr-1),"{")
		let l:neg=s:count(get(l:bibdict[l:bibfile],l:tlnr-1),"}")
		let l:i+=l:pos-l:neg
		let l:pos=s:count(get(l:bibdict[l:bibfile],l:tlnr-1),"(")
		let l:neg=s:count(get(l:bibdict[l:bibfile],l:tlnr-1),")")
		let l:j+=l:pos-l:neg
		let l:lkey=tolower(matchstr(strpart(get(l:bibdict[l:bibfile],l:tlnr-1),0,stridx(get(l:bibdict[l:bibfile],l:tlnr-1),"=")),'\<\w*\>'))
		if l:lkey != ""
		    let s:lbibd[l:lkey]=get(l:bibdict[l:bibfile],l:tlnr-1)
			let l:y=0
" IF THE LINE IS SPLIT ATTACH NEXT LINE									
			let l:lline=substitute(get(l:bibdict[l:bibfile],l:tlnr+l:y-1),'\\"\|\\{\|\\}\|\\(\|\\)','','g')
			let l:pos=s:count(l:lline,"{")
			let l:neg=s:count(l:lline,"}")
			let l:m=l:pos-l:neg
			let l:pos=s:count(l:lline,"(")
			let l:neg=s:count(l:lline,")")
			let l:n=l:pos-l:neg
			let l:o=s:count(l:lline,"\"")
" this checks if bracets {}, and () and "" appear in pairs in the current line:  
			if l:m>0 || l:n>0 || l:o>l:o/2*2 
			    while l:m>0 || l:n>0 || l:o>l:o/2*2 
				let l:pos=s:count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"{")
				let l:neg=s:count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"}")
				let l:m+=l:pos-l:neg
				let l:pos=s:count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"(")
				let l:neg=s:count(get(l:bibdict[l:bibfile],l:tlnr+l:y),")")
				let l:n+=l:pos-l:neg
				let l:o+=s:count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"\"")
" Let us append the next line: 
				let s:lbibd[l:lkey]=substitute(s:lbibd[l:lkey],'\s*$','','') . " ". substitute(get(l:bibdict[l:bibfile],l:tlnr+l:y),'^\s*','','')
				let l:y+=1
				if l:y > 30
				    echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry at line " . l:linenr . " (check line " . l:tlnr . ") in " . l:f
				    break
				endif
			    endwhile
			endif
		endif
" we have to go line by line and we could skip l:y+1 lines, but we have to
" keep l:m, l:o values. It do not saves much.		
		let l:x+=1
		if l:x > 30
			echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry at line " . l:linenr . " in " . l:f
			break
	        endif
		let b:x=l:x
		unlet l:tlnr
	    endwhile
	    let s:bibd[l:linenr]=s:lbibd
	    unlet s:lbibd
	endfor
	let l:bibresults[l:bibfile]=s:bibd
    endfor
    return l:bibresults
endfunction
"
"------------------------SHOW FOUND BIBFIELDS----------------------------
let g:bibmatchgroup='String'
let g:defaultbibflags='tabejsyu'
let g:defaultallbibflags='tabejfsvnyPNSohiuHcp'
let b:lastbibflags=g:defaultbibflags	" Set the lastflags variable to the default value on the startup.
" g:bibflagsdict={ 'flag' : ['name','what to print on the screen /13 letters/'] }
let g:bibflagsdict={ 't' : ['title', 'title        '] , 'a' : ['author', 'author       '], 
		\ 'b' : ['booktitle', 'booktitle    '], 'c' : ['mrclass', 'mrclass      '], 
		\ 'e' : ['editor', 'editor       '], 	'j' : ['journal', 'journal      '], 
		\ 'f' : ['fjournal', 'fjournal     '], 	'y' : ['year', 'year         '], 
		\ 'n' : ['number', 'number       '], 	'v' : ['volume', 'volume       '], 
		\ 's' : ['series', 'series       '], 	'p' : ['pages', 'pages        '], 
		\ 'P' : ['publisher', 'publisher    '], 'N' : ['note', 'note         '], 
		\ 'S' : ['school', 'school       '], 	'h' : ['howpublished', 'howpublished '], 
		\ 'o' : ['organization', 'organization '], 'I' : ['institution' , 'institution '],
		\ 'u' : ['url','url          '],
		\ 'H' : ['homepage', 'homepage     '], 	'i' : ['issn', 'issn         '] }
let s:bibflagslist=keys(g:bibflagsdict)
let s:bibflagsstring=join(s:bibflagslist,'')
let g:kwflagsdict={ 	  '@a' : '@article', 	'@b' : '@book\%(let\)\@<!', 
			\ '@B' : '@booklet', 	'@c' : '@in\%(collection\|book\)', 
			\ '@m' : '@misc', 	'@M' : '@manual', 
			\ '@p' : '@\%(conference\)\|\%(\%(in\)\?proceedings\)', 
			\ '@t' : '@\%(\%(master)\|\%(phd\)\)thesis', 
			\ '@T' : '@techreport', '@u' : '@unpublished'}    

" Set the g:{b:Viewer}Options as b:ViewerOptions for the current buffer
fun! s:set_viewer_options()
    if exists("b:Viewer") && exists("g:" . b:Viewer . "Options")
	let b:ViewerOptions=g:{b:Viewer}Options
    endif
endfun
au BufEnter *.tex :call s:set_viewer_options()

" Hilighlting
hi link BibResultsFileNames 	Title	
hi link BibResultEntry		ModeMsg
hi link BibResultsMatch		WarningMsg
hi link BibResultsGeneral	Normal


hi link Chapter 			Normal	
hi link Section			Normal
hi link Subsection		Normal
hi link Subsubsection		Normal
hi link CurrentSection		WarningMsg

function! s:comparelist(i1, i2)
   return str2nr(a:i1) == str2nr(a:i2) ? 0 : str2nr(a:i1) > str2nr(a:i2) ? 1 : -1
endfunction
"-------------------------s:showresults--------------------------------------
function! s:showresults(bibresults,flags,pattern)
 
    "if nothing was found inform the user and return:
    if len(a:bibresults) == count(a:bibresults,{})
	echo "BibSearch: no bib fields matched."
	return 0
    endif

" FLAGS:
" for currently supported flags see ':h atp_bibflags'
" All - all flags	
" L - last flag
" a - author
" e - editor
" t - title
" b - booktitle
" j - journal
" s - series
" y - year
" n - number
" v - volume
" p - pages
" P - publisher
" N - note
" S - school
" h - howpublished
" o - organization
" i - institution

    function! s:showvalue(value)
	return substitute(strpart(a:value,stridx(a:value,"=")+1),'^\s*','','')
    endfunction
    let s:z=1
    let l:ln=1
    let l:listofkeys={}
"--------------SET UP FLAGS--------------------------    
	    let l:allflagon=0
	    let l:flagslist=[]
	    let l:kwflagslist=[]
    " flags o and i are synonims: (but refer to different entry keys): 
	if a:flags =~ '\Ci' && a:flags !~ '\Co'
	    let a:flags=substitute(a:flags,'i','io','') 
	elseif a:flags !~ '\Ci' && a:flags =~ '\Co'
	    let a:flags=substitute(a:flags,'o','oi','')
	endif
	if a:flags !~ 'All'
	    if a:flags =~ 'L'
 		if strpart(a:flags,0,1) != '+'
 		    let l:flags=b:lastbibflags . substitute(strpart(a:flags,0),'\CL','','g')
 		else
 		    let l:flags=b:lastbibflags . substitute(a:flags,'\CL','','g')
 		endif
	    else
		if a:flags == "" 
		    let l:flags=g:defaultbibflags
		elseif strpart(a:flags,0,1) != '+' && a:flags !~ 'All' 
		    let l:flags=a:flags
		elseif strpart(a:flags,0,1) == '+' && a:flags !~ 'All'
		    let l:flags=g:defaultbibflags . strpart(a:flags,1)
		endif
	    endif
	    let b:lastbibflags=substitute(l:flags,'+\|L','','g')
		if l:flags != ""
		    let l:expr='\C[' . s:bibflagsstring . ']' 
		    while len(l:flags) >=1
			let l:oneflag=strpart(l:flags,0,1)
    " if we get a flag from the variable s:bibflagsstring we copy it to the list l:flagslist 
			if l:oneflag =~ l:expr
			    let l:flagslist=add(l:flagslist,l:oneflag)
			    let l:flags=strpart(l:flags,1)
    " if we get '@' we eat ;) two letters to the list l:kwflagslist			
			elseif l:oneflag == '@'
			    let l:oneflag=strpart(l:flags,0,2)
			    if index(keys(g:kwflagsdict),l:oneflag) != -1
				let l:kwflagslist=add(l:kwflagslist,l:oneflag)
			    endif
			    let l:flags=strpart(l:flags,2)
    " remove flags which are not defined
			elseif l:oneflag !~ l:expr && l:oneflag != '@'
			    let l:flags=strpart(l:flags,1)
			endif
		    endwhile
		endif
	else
    " if the flag 'All' was specified. 	    
	    let l:flagslist=split(g:defaultallbibflags, '\zs')
	    let l:af=substitute(a:flags,'All','','g')
	    for l:kwflag in keys(g:kwflagsdict)
		if a:flags =~ '\C' . l:kwflag	
		    call extend(l:kwflagslist,[l:kwflag])
		endif
	    endfor
	endif
" 	let b:flagslist=l:flagslist			" DEBUG
" 	let b:kwflagslist=l:kwflagslist			" DEBUG
"   Open a new window.
    let l:bufnr=bufnr("___" . a:pattern . "___"  )
    if l:bufnr != -1
	let l:bdelete=l:bufnr . "bdelete"
	exe l:bdelete
    endif
    unlet l:bufnr
    let l:openbuffer=" +setl\\ buftype=nofile\\ filetype=bibsearch_atp " . fnameescape("___" . a:pattern . "___")
    if g:vertical ==1
	let l:openbuffer="vsplit " . l:openbuffer 
	let l:skip=""
    else
	let l:openbuffer="split " . l:openbuffer 
	let l:skip="       "
    endif
    silent exe l:openbuffer

"     set the window options
    silent call s:setwindow()

    for l:bibfile in keys(a:bibresults)
	if a:bibresults[l:bibfile] != {}
	    call setline(l:ln, "Found in " . l:bibfile )	
	    let l:ln+=1
	endif
	for l:linenr in copy(sort(keys(a:bibresults[l:bibfile]),"s:comparelist"))
" make a dictionary of clear values, which we will fill with found entries. 	    
" the default value is no<keyname>, which after all is matched and not showed
	    let l:values={'key' : 'nokey'}	
	    for l:flag in s:bibflagslist 
		let l:values=extend(l:values,{ g:bibflagsdict[l:flag][0] : 'no' . g:bibflagsdict[l:flag][0] })
	    endfor
	    unlet l:flag
	    let b:values=l:values
" fill l:values with a:bibrsults	    
	    let l:values["key"]=a:bibresults[l:bibfile][l:linenr]["KEY"]
	    for l:key in keys(l:values)
		if l:key != 'key' && get(a:bibresults[l:bibfile][l:linenr],l:key,"no" . l:key) != "no" . l:key
		    let l:values[l:key]=a:bibresults[l:bibfile][l:linenr][l:key]
		endif
	    endfor
" ----------------------------- SHOW ENTRIES -------------------------
" first we check the keyword flags, @a,@b,... it passes if at least one flag
" is matched
	    let l:check=0
	    for l:lkwflag in l:kwflagslist
	        let l:kwflagpattern= '\C' . g:kwflagsdict[l:lkwflag]
		if l:values['key'] =~ l:kwflagpattern
		   let l:check=1
		endif
	    endfor
	    if l:check == 1 || len(l:kwflagslist) == 0
		let l:linenumber=index(s:bibdict[l:bibfile],l:values["key"])+1
 		call setline(l:ln,s:z . ". line " . l:linenumber . "  " . l:values["key"])
		let l:ln+=1
 		let l:c0=s:count(l:values["key"],'{')-s:count(l:values["key"],'(')

	
" this goes over the entry flags:
		for l:lflag in l:flagslist
" we check if the entry was present in bibfile:
		    if l:values[g:bibflagsdict[l:lflag][0]] != "no" . g:bibflagsdict[l:lflag][0]
" 			if l:values[g:bibflagsdict[l:lflag][0]] =~ a:pattern
			    call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " = " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
			    let l:ln+=1
" 			else
" 			    call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " = " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
" 			    let l:ln+=1
" 			endif
		    endif
		endfor
		let l:lastline=getline(line('$'))
		let l:c1=s:count(l:lastline,'{')-s:count(l:lastline,'}')
		let l:c2=s:count(l:lastline,'(')-s:count(l:lastline,')')
		let l:c3=s:count(l:lastline,'\"')
" 		echomsg "last line " . line('$') . "     l:ln=" l:ln . "    l:c0=" . l:c0		"DEBUG
		if l:c0 == 1 && l:c1 == -1
		    call setline(line('$'),substitute(l:lastline,'}\s*$','',''))
		    call setline(l:ln,'}')
		    let l:ln+=1
		elseif l:c0 == 1 && l:c1 == 0	
		    call setline(l:ln,'}')
		    let l:ln+=1
		elseif l:c0 == -1 && l:c2 == -1
		    call setline(line('$'),substitute(l:lastline,')\s*$','','')
		    call setline(l:ln,')')
		    let l:ln+=1
		elseif l:c0 == -1 && l:c1 == 0	
		    call setline(l:ln,')')
		    let l:ln+=1
		endif
		let l:listofkeys[s:z]=l:values["key"]
		let s:z+=1
	    endif
	endfor
    endfor
    call matchadd("Search",a:pattern)
    " return l:listofkeys which will be available in the bib search buffer
    " as b:listofkeys (see the BibSearch function below)
    return l:listofkeys
endfunction

if !exists("*BibSearch")
"  There are three arguments: {pattern}, [flags, [choose]]
function! BibSearch(...)
    if a:0 == 0
	let l:bibresults=s:searchbib('')
	let b:listofkeys=s:showresults(l:bibresults,'','')
    elseif a:0 == 1
	let l:bibresults=s:searchbib(a:1)
	let b:listofkeys=s:showresults(l:bibresults,'',a:1)
    else
	let l:bibresults=s:searchbib(a:1)
	let b:listofkeys=s:showresults(l:bibresults,a:2,a:1)
    endif
endfunction
endif

"---------- TOC -----------------------------------------------------------
" this function sets the options of BibSearch, ToC and Labels windows.
function! s:setwindow()
" These options are set in the command line
" +setl\\ buftype=nofile\\ filetype=bibsearch_atp   
" +setl\\ buftype=nofile\\ filetype=toc_atp\\ nowrap
" +setl\\ buftype=nofile\\ filetype=toc_atp\\ syntax=labels_atp
	setlocal nonumber
 	setlocal winfixwidth
	setlocal noswapfile	
	setlocal window
	setlocal nobuflisted
	if &filetype == "bibsearch_atp"
" 	    setlocal winwidth=30
	    setlocal nospell
	elseif &filetype == "toc_atp"
" 	    setlocal winwidth=20
	    setlocal nospell
	endif
endfunction

let g:sections={
    \	'chapter' 	: [           '^\s*\(\\chapter.*\)',	'\\chapter\*'],	
    \	'section' 	: [           '^\s*\(\\section.*\)',	'\\section\*'],
    \ 	'subsection' 	: [	   '^\s*\(\\subsection.*\)',	'\\subsection\*'],
    \	'subsubsection' : [ 	'^\s*\(\\subsubsection.*\)',	'\\subsubsection\*'],
    \	'bibliography' 	: ['^\s*\(\\begin.*{bibliography}.*\|\\bibliography\s*{.*\)' , 'nopattern'],
    \	'abstract' 	: ['^\s*\(\\begin\s*{abstract}.*\|\\abstract\s*{.*\)',	'nopattern']}

"----------- Make TOC -----------------------------
" This makes sense only for latex documents.
"
" It makes t:toc - a dictionary (with keys: full path of the buffer name)
" which values are dictionaries which keys are: line numbers and values lists:
" [ 'section-name', 'number', 'title'] where section name is element of
" keys(g:sections), number is the total number, 'title=\1' where \1 is
" returned by the g:section['key'][0] pattern.
function! s:maketoc(filename)
    let b:fname=a:filename
    "
    " this will store information { 'linenumber' : ['chapter/section/..', 'sectionnumber', 'section title', '0/1=not starred/starred'] }
    let l:toc={}

    " if the dictinary with labels is not defined, define it
    if !exists("t:labels")
	let t:labels={}
    endif
    " TODO we could check if there are changes in the file and copy the buffer
    " to this variable only if there where changes.
    let l:texfile=[]
    " getbufline reads only loaded buffers, unloaded can be read from file.
    let l:bufname=fnamemodify(a:filename,":t")
    if bufloaded("^" . l:bufname . "$")
	let l:texfile=getbufline("^" . l:bufname . "$","1","$")
    else
	w
	let l:texfile=readfile(a:filename)
    endif
    let l:true=1
    let l:i=0
    " remove the part before \begin{document}
    while l:true == 1 && len(l:texfile)>0
	if l:texfile[0] =~ '\\begin\s*{document}'
		let l:true=0
	endif
	call remove(l:texfile,0)
	let l:i+=1
    endwhile
    let l:bline=l:i
    let l:i=1
    " set variables for chapter/section numbers
    for l:section in keys(g:sections)
	let l:ind{l:section}=0
    endfor
    " make a filter
    let l:j=0
    for l:section in keys(g:sections)
	if l:j == 0 
	    let l:filter=g:sections[l:section][0] . ''
	else
	    let l:filter=l:filter . '\|' . g:sections[l:section][0] 
	endif
	let l:j+=1
    endfor
    " filter l:texfile    
    let s:filtered=filter(deepcopy(l:texfile),'v:val =~ l:filter')
    let b:filtered=s:filtered
    let b:texfile=l:texfile
    for l:line in s:filtered
	for l:section in keys(g:sections)
	    if l:line =~ g:sections[l:section][0] 
		if l:line !~ '^\s*%'
		    " THIS DO NOT WORKS WITH \abstract{ --> empty set, but with
		    " \chapter{title} --> title, solution: the name of
		    " 'Abstract' will be plased, as we know what we have
		    " matched
		    let l:title=l:line
		    " test if it is a starred version.
		    let l:star=0
		    if g:sections[l:section][1] != 'nopattern' && l:line =~ g:sections[l:section][1] 
			let l:star=1 
		    else
			let l:star=0
		    endif
		    let l:i=index(l:texfile,l:line)
		    let l:tline=l:i+l:bline+1
		    " if it is not starred version add one to the section number
		    if l:star==0
			let l:ind{l:section}+=1
		    endif

		    " Find the title:
		    let l:start=stridx(l:title,'{')+1
		    let l:title=strpart(l:title,l:start)
		    " we are looking for the maching '}' 
		    let l:count=1
		    let l:i=-1
		    while l:i<=len(l:title)
			let l:i+=1
			if strpart(l:title,l:i,1) == '{'	
			    let l:count+=1
			elseif strpart(l:title,l:i,1) == '}'
			    let l:count-=1
			endif
			if l:count==0
			    break
			endif
		    endwhile	
		    let l:title=strpart(l:title,0,l:i)

		    " Find the short title:
		    let l:shorttitle=l:line
		    let l:start=stridx(l:shorttitle,'[')+1
		    if l:start == 0
			let l:shorttitle=''
		    else
			let l:shorttitle=strpart(l:shorttitle,l:start)
			" we are looking for the maching ']' 
			let l:count=1
			let l:i=-1
			while l:i<=len(l:shorttitle)
			    let l:i+=1
			    if strpart(l:shorttitle,l:i,1) == '['	
				let l:count+=1
			    elseif strpart(l:shorttitle,l:i,1) == ']'
				let l:count-=1
			    endif
			    if l:count==0
				break
			    endif
			endwhile	
			let l:shorttitle=strpart(l:shorttitle,0,l:i)
		    endif
		    call extend(l:toc, { l:tline : [ l:section, l:ind{l:section}, l:title, l:star, l:shorttitle] }) 

		    " Extend t:labels
		    let l:lname=matchstr(l:line,'\\label\s*{.*','')
		    let l:start=stridx(l:lname,'{')+1
		    let l:lname=strpart(l:lname,l:start)
		    let l:end=stridx(l:lname,'}')
		    let l:lname=strpart(l:lname,0,l:end)
		    let b:lname=l:lname
		    if	l:lname != ''
			" if there was no t:labels for a:filename make an entry in
			" t:labels
			if !has_key(t:labels,a:filename)
			    let t:labels[a:filename] = {}
			endif
			call extend(t:labels[a:filename],{ l:tline : l:lname },"force")
		    endif
		endif
	    endif
	endfor
    endfor
    if exists("t:toc")
	call extend(t:toc, { a:filename : l:toc },"force")
    else
	let t:toc={ a:filename : l:toc }
    endif
    return t:toc
endfunction
let t:texcompiler=b:texcompiler
"--------------------- Make a List of Buffers ----
if !exists("t:buflist")
    let t:buflist=[]
endif
function! s:buflist()
    " this names are used in TOC and passed to s:maketoc, which
    " makes a dictionary whose keys are the values of l:name defined here
    " below:
    let l:name=resolve(fnamemodify(bufname("%"),":p"))
    " add an entry to the list t:buflist if it is not there.
    if bufname("") =~ ".tex" && index(t:buflist,l:name) == -1
	call add(t:buflist,l:name)
    endif
endfunction
call s:buflist()
 
if !exists("*RemoveFromBufList")
    function RemoveFromBufList()
	let l:i=1
	for l:f in t:buflist
	    echo "(" . l:i . ") " . l:f
	    let l:i+=1
	endfor
	let l:which=input("Which file to remove (press <Enter> for none)")
	if l:which != "" && l:which =~ '\d\+'
	    call remove(t:buflist,l:f-1)
	endif
    endfunction
endif
"---------------------- Show TOC -----------------
function! s:showtoc(toc)
    " this is a dictionary of line numbers where a new file begins.
    let l:cline=line(".")
"     " Open new window or jump to the existing one.
"     " Remember the place from which we are coming:
"     let t:bufname=bufname("")
"     let t:winnr=winnr()	 these are already set by TOC()
    let l:bname="__ToC__"
    let l:tocwinnr=bufwinnr("^" . l:bname . "$") 
"     echomsg "DEBUG a " . l:tocwinnr
    if l:tocwinnr != -1
	" Jump to the existing window.
	    exe l:tocwinnr . " wincmd w"
	    silent exe "%delete"
    else
	" Open new window if its width is defined (if it is not the code below
	" will put toc in the current buffer so it is better to return.
	if !exists("t:toc_window_width")
	    echoerr "t:toc_window_width not set"
	    return
	endif
	let l:openbuffer=t:toc_window_width . "vsplit +setl\\ wiw=15\\ buftype=nofile\\ filetype=toc_atp\\ nowrap __ToC__"
	silent exe l:openbuffer
	" We are setting the address from which we have come.
	silent call s:setwindow()
    endif
    setlocal tabstop=4
    let l:number=1
    " this is the line number in ToC.
    " l:number is a line number relative to the file listed in ToC.
    " the current line number is l:linenumber+l:number
    " there are two loops: one over l:linenumber and the second over l:number.
    let l:numberdict={}
    " this variable will be used to set the cursor position in ToC.
    for l:openfile in keys(a:toc)
	call extend(l:numberdict,{ l:openfile : l:number })
	let l:chapon=0
	let l:chnr=0
	let l:secnr=0
	let l:ssecnr=0
	let l:sssecnr=0
	let l:path=fnamemodify(bufname(""),":p:h")
	for l:line in keys(a:toc[l:openfile])
	    if a:toc[l:openfile][l:line][0] == 'chapter'
		let l:chapon=1
		break
	    endif
	endfor
	let l:sorted=sort(keys(a:toc[l:openfile]),"s:comparelist")
	let l:len=len(l:sorted)
	" write the file name in ToC (with a full path in paranthesis)
	call setline(l:number,fnamemodify(l:openfile,":t") . " (" . fnamemodify(l:openfile,":p:h") . ")")
	let l:number+=1
	for l:line in l:sorted
	    let l:lineidx=index(l:sorted,l:line)
	    let l:nlineidx=l:lineidx+1
	    if l:nlineidx< len(l:sorted)
		let l:nline=l:sorted[l:nlineidx]
	    else
		let l:nline=line("$")
	    endif
	    let l:lenght=len(l:line) 	
	    if l:lenght == 0
		let l:showline="     "
	    elseif l:lenght == 1
		let l:showline="    " . l:line
	    elseif l:lenght == 2
		let l:showline="   " . l:line
	    elseif l:lenght == 3
		let l:showline="  " . l:line
	    elseif l:lenght == 4
		let l:showline=" " . l:line
	    elseif l:lenght>=5
		let l:showline=l:line
	    endif
	    " Print ToC lines.
	    if a:toc[l:openfile][l:line][0] == 'abstract'
		call setline(l:number, l:showline . "\t" . "  " . "Abstract" )
	    elseif a:toc[l:openfile][l:line][0] =~ 'bibliography\|references'
		call setline (l:number, l:showline . "\t" . "  " . a:toc[l:openfile][l:line][2])
	    elseif a:toc[l:openfile][l:line][0] == 'chapter'
		let l:chnr=a:toc[l:openfile][l:line][1]
		let l:nr=l:chnr
		if a:toc[l:openfile][l:line][3]
		    "if it is stared version" 
		    let l:nr=substitute(l:nr,'.',' ','')
		endif
		if a:toc[l:openfile][l:line][4] != ''
		    call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		else
		    call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		endif
	    elseif a:toc[l:openfile][l:line][0] == 'section'
		let l:secnr=a:toc[l:openfile][l:line][1]
		if l:chapon
		    let l:nr=l:chnr . "." . l:secnr  
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		else
		    let l:nr=l:secnr 
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		endif
	    elseif a:toc[l:openfile][l:line][0] == 'subsection'
		let l:ssecnr=a:toc[l:openfile][l:line][1]
		if l:chapon
		    let l:nr=l:chnr . "." . l:secnr  . "." . l:ssecnr
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		else
		    let l:nr=l:secnr  . "." . l:ssecnr
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		endif
	    elseif a:toc[l:openfile][l:line][0] == 'subsubsection'
		let l:sssecnr=a:toc[l:openfile][l:line][1]
		if l:chapon
		    let l:nr=l:chnr . "." . l:secnr . "." . l:sssecnr  
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline(l:number, a:toc[l:openfile][l:line][0] . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline(l:number, a:toc[l:openfile][l:line][0] . "\t\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		else
		    let l:nr=l:secnr  . "." . l:ssecnr . "." . l:sssecnr
		    if a:toc[l:openfile][l:line][3]
			"if it is stared version" 
			let l:nr=substitute(l:nr,'.',' ','g')
		    endif
		    if a:toc[l:openfile][l:line][4] != ''
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][4])
		    else
			call setline (l:number, l:showline . "\t\t" . l:nr . " " . a:toc[l:openfile][l:line][2])
		    endif
		endif
	    else
		let l:nr=""
	    endif
	    let l:number+=1
	endfor
    endfor
	" set the cursor position on the correct line number.
	" first get the line number of the begging of the ToC of t:bufname
	" (current buffer)
	let t:numberdict=l:numberdict	"DEBUG
" 	t:bufname is the full path to the current buffer.
	let l:number=l:numberdict[t:bufname]
	let l:sorted=sort(keys(a:toc[t:bufname]),"s:comparelist")
	let t:sorted=l:sorted
	for l:line in l:sorted
	    if l:cline>=l:line
		let l:number+=1
	    endif
	call setpos('.',[bufnr(""),l:number,1,0])
	endfor
endfunction
"------------------- TOC ---------------------------------------------
if !exists("*TOC")
function! TOC()
    if &filetype != 'tex'    
	echoerr "Wrong 'filetype'. This function works only for latex documents."
	return
    endif
    " for each buffer in t:buflist (set by s:buflist)
    for l:buffer in t:buflist 
	    let t:toc=s:maketoc(l:buffer)
    endfor
    call s:showtoc(t:toc)
endfunction
endif
"------------------- Current TOC -------------------------------------
" This function finds the section name of the current section unit with
" respect to the dictionary a:section={ 'line number' : 'section name', ... }
" it returns the [ section_name, section line, next section line ]
function! s:nearestsection(section)
    let l:cline=line('.')

    let l:sorted=sort(keys(a:section),"s:comparelist")
    let l:x=0
    while l:x<len(l:sorted) && l:sorted[l:x]<=l:cline
       let l:x+=1 
    endwhile
    if l:x>=1 && l:x < len(l:sorted)
	let l:section_name=a:section[l:sorted[l:x-1]]
	return [l:section_name, l:sorted[l:x-1], l:sorted[l:x]]
    elseif l:x>=1 && l:x >= len(l:sorted)
	let l:section_name=a:section[l:sorted[l:x-1]]
	return [l:section_name,l:sorted[l:x-1], line('$')]
    elseif l:x<1 && l:x < len(l:sorted)
	" if we are before the first section return the empty string
	return ['','0', l:sorted[l:x]]
    elseif l:x<1 && l:x >= len(l:sorted)
	return ['','0', line('$')]
    endif
endfunction

function! s:CTOC()
    if &filetype != 'tex'    
	echomsg "CTOC: Wrong 'filetype'. This function works only for latex documents."
	" Set the status line once more, to remove the CTOC() function.
	call ATPStatus()
	return ""
    endif
    " resolve the full path:
    let t:bufname=resolve(fnamemodify(bufname("%"),":p"))
    
    " if t:toc(t:bufname) exists use it otherwise make it 
    if !exists("t:toc") || !has_key(t:toc,t:bufname) 
	silent let t:toc=s:maketoc(t:bufname)
    endif

    " count where the preambule ends
    let l:buffer=getbufline(bufname("%"),"1","$")
    let l:i=0
    let l:line=l:buffer[0]
    while l:line !~ '\\begin\s*{document}' && l:i < len(l:buffer)
	let l:line=l:buffer[l:i]
	if l:line !~ '\\begin\s*{document}' 
	    let l:i+=1
	endif
    endwhile
	
    " if we are before the '\\begin{document}' line: 
    if line(".") <= l:i
	return ['Preambule']
    endif

    let l:chapter={}
    let l:section={}
    let l:subsection={}

    for l:key in keys(t:toc[t:bufname])
	if t:toc[t:bufname][l:key][0] == 'chapter'
	    " return the short title if it is provided
	    if t:toc[t:bufname][l:key][4] != ''
		call extend(l:chapter, {l:key : t:toc[t:bufname][l:key][4]},'force')
	    else
		call extend(l:chapter, {l:key : t:toc[t:bufname][l:key][2]},'force')
	    endif
	elseif t:toc[t:bufname][l:key][0] == 'section'
	    " return the short title if it is provided
	    if t:toc[t:bufname][l:key][4] != ''
		call extend(l:section, {l:key : t:toc[t:bufname][l:key][4]},'force')
	    else
		call extend(l:section, {l:key : t:toc[t:bufname][l:key][2]},'force')
	    endif
	elseif t:toc[t:bufname][l:key][0] == 'subsection'
	    " return the short title if it is provided
	    if t:toc[t:bufname][l:key][4] != ''
		call extend(l:subsection, {l:key : t:toc[t:bufname][l:key][4]},'force')
	    else
		call extend(l:subsection, {l:key : t:toc[t:bufname][l:key][2]},'force')
	    endif
	endif
    endfor

    " Remove $ from chapter/section/subsection names to save the space.
    let l:chapter_name=substitute(s:nearestsection(l:chapter)[0],'\$','','g')
    let l:chapter_line=s:nearestsection(l:chapter)[1]
    let l:chapter_nline=s:nearestsection(l:chapter)[2]

    let l:section_name=substitute(s:nearestsection(l:section)[0],'\$','','g')
    let l:section_line=s:nearestsection(l:section)[1]
    let l:section_nline=s:nearestsection(l:section)[2]
"     let b:section=s:nearestsection(l:section)		" DEBUG

    let l:subsection_name=substitute(s:nearestsection(l:subsection)[0],'\$','','g')
    let l:subsection_line=s:nearestsection(l:subsection)[1]
    let l:subsection_nline=s:nearestsection(l:subsection)[2]
"     let b:ssection=s:nearestsection(l:subsection)		" DEBUG

    let l:names	= [ l:chapter_name ]
    if (l:section_line+0 >= l:chapter_line+0 && l:section_line+0 <= l:chapter_nline+0) || l:chapter_name == '' 
	call add(l:names, l:section_name) 
    elseif l:subsection_line+0 >= l:section_line+0 && l:subsection_line+0 <= l:section_nline+0
	call add(l:names, l:subsection_name)
    endif
    return l:names
endfunction

if !exists("*CTOC")
    function CTOC(...)
	" if there is any argument given, then the function returns the value
	" (used by ATPStatus()), otherwise it echoes the section/subsection
	" title. It returns only the first b:truncate_status_section
	" characters of the the whole titles.
	let l:names=s:CTOC()
	let b:names=l:names
	let l:chapter_name=get(l:names,0,'')
	let l:section_name=get(l:names,1,'')
	let l:subsection_name=get(l:names,2,'')

	if l:chapter_name == "" && l:section_name == "" && l:subsection_name == ""

	if a:0 == '0'
	    echo "" 
	else
	    return ""
	endif
	    
	elseif l:chapter_name != ""
	    if l:section_name != ""
		if a:0 == '0'
		    echo "XXX" . l:chapter_name . "/" . l:section_name 
		else
		    return substitute(strpart(l:chapter_name,0,b:truncate_status_section/2), '\_s*$', '','') . "/" . substitute(strpart(l:section_name,0,b:truncate_status_section/2), '\_s*$', '','')
		endif
	    else
		if a:0 == '0'
		    echo "XXX" . l:chapter_name
		else
		    return substitute(strpart(l:chapter_name,0,b:truncate_status_section), '\_s*$', '','')
		endif
	    endif

	elseif l:chapter_name == "" && l:section_name != ""
	    if l:subsection_name != ""
		if a:0 == '0'
		    echo "XXX" . l:section_name . "/" . l:subsection_name 
		else
		    return substitute(strpart(l:section_name,0,b:truncate_status_section/2), '\_s*$', '','') . "/" . substitute(strpart(l:subsection_name,0,b:truncate_status_section/2), '\_s*$', '','')
		endif
	    else
		if a:0 == '0'
		    echo "XXX" . l:section_name
		else
		    return substitute(strpart(l:section_name,0,b:truncate_status_section), '\_s*$', '','')
		endif
	    endif

	elseif l:chapter_name == "" && l:section_name == "" && l:subsection_name != ""
	    if a:0 == '0'
		echo "XXX" . l:subsection_name
	    else
		return substitute(strpart(l:subsection_name,0,b:truncate_status_section), '\_s*$', '','')
	    endif
	endif
    endfunction
endif

"--------- LABELS --------------------------------------------------------
" the argument should be: resolved full path to the file:
" resove(fnamemodify(bufname("%"),":p"))
function! s:generatelabels(filename)
    let s:labels={}
    let l:bufname=fnamemodify(a:filename,":t")
    " getbufline reads onlu loaded buffers, unloaded can be read from file.
    if bufloaded("^" . l:bufname . "$")
	let l:texfile=getbufline("^" . l:bufname . "$","1","$")
    else
	w
	let l:texfile=readfile(a:filename)
    endif
"     echomsg "DEBUG X        " . fnamemodify(a:filename,":t")
    let l:true=1
    let l:i=0
    " remove the bart before \begin{document}
    while l:true == 1
	if l:texfile[0] =~ '\\begin\s*{document}'
		let l:true=0
	endif
	call remove(l:texfile,0)
	let l:i+=1
    endwhile
    let l:bline=l:i
    let l:i=0
    while l:i < len(l:texfile)
	if l:texfile[l:i] =~ '\\label\s*{'
	    let l:lname=matchstr(l:texfile[l:i],'\\label\s*{.*','')
	    let l:start=stridx(l:lname,'{')+1
	    let l:lname=strpart(l:lname,l:start)
	    let l:end=stridx(l:lname,'}')
	    let l:lname=strpart(l:lname,0,l:end)
	    let b:lname=l:lname
    "This can be extended to have also the whole environmet which
    "could be shown.
	    call extend(s:labels,{ l:i+l:bline+1 : l:lname })
	endif
	let l:i+=1 
    endwhile
    if exists("t:labels")
	call extend(t:labels,{ a:filename : s:labels },"force")
    else
	let t:labels={ a:filename : s:labels }
    endif
    return t:labels
endfunction

" The argument is the dictionary generated by s:generatelabels.
function! s:showlabels(labels)
    " the argument a:labels=t:labels[bufname("")] !
    let l:cline=line(".")
    let l:lines=sort(keys(a:labels),"s:comparelist")
    " Open new window or jump to the existing one.
    let l:bufname=bufname("")
"     let l:bufpath=fnamemodify(bufname(""),":p:h")
    let l:bufpath=fnamemodify(resolve(fnamemodify(bufname("%"),":p")),":h")
    let l:bname="__Labels__"
    let l:labelswinnr=bufwinnr("^" . l:bname . "$")
	let b:labelswinnr=l:labelswinnr		"DEBUG
    let t:labelswinnr=winnr()
    let t:labelsbufnr=bufnr("^" . l:bname . "$") 
    let l:labelswinnr=bufwinnr(t:labelsbufnr)
    if l:labelswinnr != -1
	" Jump to the existing window.
	exe l:labelswinnr . " wincmd w"
	if l:labelswinnr != t:labelswinnr
	    silent exe "%delete"
	else
	    echoerr "ATP error in function s:showtoc, TOC/LABEL buffer 
		    \ and the tex file buffer agree."
	    return
	endif
    else
    " Open new window if its width is defined (if it is not the code below
    " will put lab:cels in the current buffer so it is better to return.
	if !exists("t:labels_window_width")
	    echoerr "t:labels_window_width not set"
	    return
	endif
	let l:openbuffer=t:labels_window_width . "vsplit +setl\\ buftype=nofile\\ filetype=toc_atp\\ syntax=labels_atp __Labels__"
	silent exe l:openbuffer
	silent call s:setwindow()
	let t:labelsbufnr=bufnr("")
    endif
    call setline(1,l:bufname . " (" . l:bufpath . ")")
    let l:ln=2
    for l:line in l:lines
	call setline(l:ln, l:line . "\t" . a:labels[l:line]) 
	let l:ln+=1
    endfor
    " set the cursor position on the correct line number.
    let l:number=1
    for l:line in l:lines
    if l:cline>=l:line
	call setpos('.',[bufnr(bufname('%')),l:number+1,1,0])
    elseif l:number == 1 && l:cline<l:line
	call setpos('.',[bufnr(bufname('%')),l:number+1,1,0])
    endif
    let l:number+=1
    endfor
endfunction
" -------------------- Labels ---------------------------------------
if !exists("*Labels")
function! Labels()
    let t:bufname=bufname("%")
    let l:bufname=resolve(fnamemodify(t:bufname,":p"))
    " Generate the dictionary with labels
    let t:labels=s:generatelabels(l:bufname)
    " Show the labels in seprate window
    call s:showlabels(t:labels[l:bufname])
endfunction
endif
" ------------- Edit Input Files  -----------------------------------
if !exists("*EditInputFile")
function! EditInputFile(...)

    if a:0==0
	let l:inputfile=""
	let l:bufname=g:mainfile
	let l:opencom="edit"
    elseif a:0==1
	let l:inputfile=a:1
	let l:bufname=g:mainfile
	let l:opencom="edit"
    else
	let l:inputfile=a:1
	let l:opencom=a:2

	" the last argument is the bufername in which search for the input files 
	" TODO: to DOC
	if a:0>2
	    let l:bufname=a:3
	else
	    let l:bufname=g:mainfile
	endif
    endif

    let l:dir=fnamemodify(l:bufname,":p:h")

    if a:0 == 0
	let l:inputfiles=FindInputFiles(l:bufname)
    else
	let l:inputfiles=FindInputFiles(l:bufname,0)
    endif

    if !len(l:inputfiles) > 0
	return 
    endif

    if index(keys(l:inputfiles),l:inputfile) == '-1'
	let l:which=input("Which file to edit? <enter> for none ","","customlist,EI_compl")
	if l:which == ""
	    return
	endif
    else
	let l:which=l:inputfile
    endif

    if l:which =~ '^\s*\d\+\s*$'
	let l:ifile=keys(l:inputfiles)[l:which-1]
    else
	let l:ifile=l:which
    endif

    "g:texmf should end with a '/', if not add it.
    if g:texmf !~ "\/$"
	let g:texmf=g:texmf . "/"
    endif

    " remove all '"' from the line (latex do not supports file names with '"')
    " this make the function work with lines like: '\\input "file name with spaces.tex"'
    let l:ifile=substitute(l:ifile,'^\s*\"\|\"\s*$','','g')
    " add .tex extension if it was not present
    if l:inputfiles[l:ifile][0] == 'input' || l:inputfiles[l:ifile][0] == 'include'
	let l:ifilename=s:append(l:ifile,'.tex')
    else
	let l:ifilename=s:append(l:ifile,'.bib')
    endif
    if l:ifile !~ '\s*\/'
	if filereadable(l:dir . "/" . l:ifilename) 
	    let s:ft=&filetype
	    exe "edit " . fnameescape(b:outdir . l:ifilename)
	    let &l:filetype=s:ft
	else
	    if l:inputfiles[l:ifile][0] == 'input' || l:inputfiles[l:ifile][0] == 'include'
		let l:ifilename=findfile(l:ifile,g:texmf . '**')
		let s:ft=&filetype
		exe l:opencom . " " . fnameescape(l:ifilename)
	    let &l:filetype=s:ft
	    else
		let s:ft=&filetype
		exe l:opencom . " " . fnameescape(s:append($BIBINPUTS,'/') . l:ifilename)
		let &l:filetype=s:ft
	    endif
	endif
    else
	exe l:opencom . " " . fnameescape(l:ifilename)
    endif
endfunction
endif

if !exists("*EI_compl")
fun! EI_compl(A,P,L)
"     let l:inputfiles=FindInputFiles(bufname("%"),1)
    let l:inputfiles=FindInputFiles(g:mainfile,1)
    " rewrite the keys of FindInputFiles the order: input files, bibfiles
    let l:oif=[]
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'input'
	    call add(l:oif,l:key)
	endif
    endfor
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'include'
	    call add(l:oif,l:key)
	endif
    endfor
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'bib'
	    call add(l:oif,l:key)
	endif
    endfor

    " check what is already written, if it matches something return only the
    " matching strings
    let l:return_oif=[]
    for l:i in l:oif
	if l:i =~ '^' . a:A 
	    call add(l:return_oif,l:i)
	endif
    endfor
    return l:return_oif
endfun
endif

" TODO if the file was not found ask to make one.
"--------- ToDo -----------------------------------------------------------
"
function! ToDo(keyword,stop,...)

    if a:0==0
	let l:bufname=bufname("%")
    else
	let l:bufname=a:1
    endif

    " read the buffer
    let l:texfile=getbufline(l:bufname, 1, "$")

    " find ToDos
    let b:todo={}
    let l:nr=1
    for l:line in l:texfile
	if l:line =~ '%.*' . a:keyword 
	    call extend(b:todo, { l:nr : l:line }) 
	endif
	let l:nr+=1
    endfor

    " Show ToDos
    echohl atp_Todo
    if len(keys(b:todo)) == 0
	echomsg " List for '%.*" . a:keyword . "' in '" . l:bufname . "' is empty."
	return
    endif
    echomsg " List for '%.*" . a:keyword . "' in '" . l:bufname . "':"
    let l:sortedkeys=sort(keys(b:todo),"s:comparelist")
    for l:key in l:sortedkeys
	" echo the todo line.
	echomsg l:key . " " . substitute(substitute(b:todo[l:key],'%','',''),'\t',' ','g')
	let l:true=1
	let l:a=1
	let l:linenr=l:key
	" show all comment lines right below the found todo line.
	while l:true && l:texfile[l:linenr] !~ '%.*\c\<todo\>' 
	    let l:linenr=l:key+l:a-1
	    if l:texfile[l:linenr] =~ "\s*%" && l:texfile[l:linenr] !~ a:stop
		" make space of length equal to len(l:linenr)
		let l:space=""
		let l:j=0
		while l:j < len(l:linenr)
		    let l:space=l:space . " " 
		    let l:j+=1
		endwhile
		echomsg l:space . " " . substitute(substitute(l:texfile[l:linenr],'%','',''),'\t',' ','g')
	    else
		let l:true=0
	    endif
	    let l:a+=1
	endwhile
    endfor
    echohl None
endfunction
" 
"--------- FOLDING --------------------------------------------------------
"
let s:a=0
function! FoldExpr(line)
    let s:a+=1
    echomsg "DEBUG " . s:a " at line " . a:line

    call s:maketoc(fnamemodify(bufname("%"),":p"))
    let l:line=a:line

    " make a fold of the preambule /now this folds too much/
    let l:sorted=sort(keys(t:toc[fnamemodify(bufname("%"),":p")]),"s:comparelist")
    if a:line < l:sorted[0]
	return 1
    endif

    let l:secname=""
    while l:secname == ""  
	let l:secname=get(t:toc[fnamemodify(bufname("%"),":p")],l:line,'')[0]
	let l:line-=1
    endwhile
    let l:line+=1
    if l:secname == 'part'
	return 1
    elseif l:secname == 'chapter'
	return 2
    elseif l:secname == 'section'
	return 3
    elseif l:secname == 'subsection'
	return 4
    elseif l:secname == 'subsubsection'
	return 5
    elseif l:secname == 'paragraph'
	return 6
    elseif l:secname == 'subparagraph'
	return 7
    else 
	return 0
    endif
endfunction
" setlocal foldmethod=expr
" foldmethod=marker do not work in preamble as there might appear }}}
" this folds entire document with one fold when there are only sections
" it is thus not a good method of folding.
" setlocal foldexpr=FoldExpr(v:lnum)
"
"-------- SHOW ERRORS -----------------------------------------------------
"
" this functions sets errorformat according to the flag given in the argument,
" possible flags:
" e	- errors (or empty flag)
" w	- all warnings
" c	- citasion warnings
" r	- reference warnings
" f	- font warnings
function! s:SetErrorFormat(...)
    let &l:errorformat=""
    if a:0 == 0 || a:0 > 0 && a:1 =~ 'e'
	if &l:errorformat == ""
	    let &l:errorformat= "%E!\ LaTeX\ %trror:\ %m,\%E!\ %m"
	else
	    let &l:errorformat= &l:errorformat . ",%E!\ LaTeX\ %trror:\ %m,\%E!\ %m"
	endif
    endif
    if a:0>0 && a:1 =~ 'w'
	if &l:errorformat == ""
	    let &l:errorformat="%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,
			\%+W%.%#\ at\ lines\ %l--%*\\d,
			\%WLaTeX\ %.%#Warning:\ %m"
	else
	    let &l:errorformat= &l:errorformat . ",%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,
			\%+W%.%#\ at\ lines\ %l--%*\\d,
			\%WLaTeX\ %.%#Warning:\ %m"
	endif
    endif
    if a:0>0 && a:1 =~ 'c'
	if &l:errorformat == ""
	    let &l:errorformat = "%+WLaTeX\ %.%#Warning:\ Citation\ %.%#line\ %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%+WLaTeX\ %.%#Warning:\ Citation\ %.%#line\ %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ 'c'
	if &l:errorformat == ""
	    let &l:errorformat = "%+WLaTeX\ %.%#Warning:\ Citation\ %.%#line\ %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%+WLaTeX\ %.%#Warning:\ Citation\ %.%#line\ %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ 'r'
	if &l:errorformat == ""
	    let &l:errorformat = "%+WLaTeX\ %.%#Warning:\ Reference\ %.%#line\ %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%+WLaTeX\ %.%#Warning:\ Reference\ %.%#line\ %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ 'f'
	if &l:errorformat == ""
	    let &l:errorformat = "%+WLaTeX\ Font\ Warning:\ %.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%+WLaTeX\ Font\ Warning:\ %.%#"
	endif
    endif
    if &l:errorformat != ""
	let &l:errorformat = &l:errorformat . ",%Cl.%l\ %m,
			    \%+C\ \ %m.,
			    \%+C%.%#-%.%#,
			    \%+C%.%#[]%.%#,
			    \%+C[]%.%#,
			    \%+C%.%#%[{}\\]%.%#,
			    \%+C<%.%#>%.%#,
			    \%C\ \ %m,
			    \%-GSee\ the\ LaTeX%m,
			    \%-GType\ \ H\ <return>%m,
			    \%-G\ ...%.%#,
			    \%-G%.%#\ (C)\ %.%#,
			    \%-G(see\ the\ transcript%.%#),
			    \%-G\\s%#,
			    \%+O(%*[^()])%r,
			    \%+O%*[^()](%*[^()])%r,
			    \%+P(%f%r,
			    \%+P\ %\\=(%f%r,
			    \%+P%*[^()](%f%r,
			    \%+P[%\\d%[^()]%#(%f%r,
			    \%+Q)%r,
			    \%+Q%*[^()])%r,
			    \%+Q[%\\d%*[^()])%r"
    endif
endfunction

function! s:ShowErrors(...)

    " set errorformat 
    if a:0 > 0

	" if one uses completeion to set different flags, they will be in
	" different variables, so we concatenate them first.
	let l:arg=''
	let l:i=1 
	while l:i<=a:0
	    let l:arg.=a:{l:i}
	    let l:i+=1
	endwhile
	call s:SetErrorFormat(l:arg)
    else
	call s:SetErrorFormat()
    endif

    " read the log file
    cg
    " list errors
    cl
endfunction

if !exists("*ListErrorsFlags")
function! ListErrorsFlags(A,L,P)
	return "e\nw\nc\nr\ncr\nf"
endfunction
endif
"--------- Special Space -----------------------------------------------------
if !exists("*SpecialSpaceToggle")
function! SpecialSpaceToggle()
    if maparg('<space>','c') == ""
	echomsg "special space on"
	cmap <Space> \_s\+
    else
	echomsg "special space off"
 	cunmap <Space>
    endif
endfunction
endif
"--------- Set Viewers  ------------------------------------------------------
"
fun! SetXdvi()
    let b:texcompiler="latex"
    let b:texoptions="-src-specials"
    let b:Viewer="xdvi -editor 'gvim --remote-wait +%l %f'"
    if exists("g:xdviOptions")
	let b:ViewerOptions=g:xdviOptions
    endif
    if !exists("*ISearch")
    function ISearch()
	let l:xdvi_inverse_search="xdvi -sourceposition " . line(".") . ":" . col(".") . expand("%") . " " . fnamemodify(expand("%"),":r") . ".dvi"
	call system(l:xdvi_inverse_search)
    endfunction
    endif
    command! -buffer ISearch	:call ISearch()
    command! -buffer IS 	:call ISearch()
    map <buffer> <LocalLeader>is		:call ISearch()<CR>
endfun
fun! SetXpdf()
    let b:texcompiler="pdflatex"
    let b:texoptions=""
    let b:Viewer="xpdf"
    if exists("g:XdviOptions")
	let b:ViewerOptions=g:xpdfOptions
    else
	let b:ViewerOptions=''
    endif
    if hasmapto("ISearch()",'n')
	unmap <buffer> <LocalLeader>is
    endif
    if exists("IS")
	delcommand IS
    endif
    if exists("ISearch")
	delcommand ISearch
    endif
endfun

"--------- Search for Matching Pair  -----------------------------------------
"This is a tiny modification of the function defined in matchparent.vim to
"handle multibyte characters
"
" The function that is invoked (very often) to define a ":match" highlighting
" for any matching paren.
" function! s:Highlight_Matching_Pair()
"   " Remove any previous match.
"   if exists('w:paren_hl_on') && w:paren_hl_on
"     3match none
"     let w:paren_hl_on = 0
"   endif
" 
"   " Avoid that we remove the popup menu.
"   " Return when there are no colors (looks like the cursor jumps).
"   if pumvisible() || (&t_Co < 8 && !has("gui_running"))
"     return
"   endif
" 
"   " Get the character under the cursor and check if it's in 'matchpairs'.
"   let c_lnum = line('.')
"   let c_col = col('.')
"   let before = 0
" 
" 
"   let plist = split(g:matchpairs, '.\zs[:,]')
"   let i = index(plist, c)
"   if i < 0
"     " not found, in Insert mode try character before the cursor
"     if c_col > 1 && (mode() == 'i' || mode() == 'R')
"       let before = 1
"       let c = getline(c_lnum)[c_col - 2]
"       let i = index(plist, c)
"     endif
"     if i < 0
"       " not found, nothing to do
"       return
"     endif
"   endif
" 
"   " Figure out the arguments for searchpairpos().
"   if i % 2 == 0
"     let s_flags = 'nW'
"     let c2 = plist[i + 1]
"   else
"     let s_flags = 'nbW'
"     let c2 = c
"     let c = plist[i - 1]
"   endif
"   if c == '['
"     let c = '\['
"     let c2 = '\]'
"   endif
" 
"   " Find the match.  When it was just before the cursor move it there for a
"   " moment.
"   if before > 0
"     let save_cursor = winsaveview()
"     call cursor(c_lnum, c_col - before)
"   endif
" 
"   " When not in a string or comment ignore matches inside them.
"   let s_skip ='synIDattr(synID(line("."), col("."), 0), "name") ' .
" 	\ '=~?  "string\\|character\\|singlequote\\|comment"'
"   execute 'if' s_skip '| let s_skip = 0 | endif'
" 
"   " Limit the search to lines visible in the window.
"   let stoplinebottom = line('w$')
"   let stoplinetop = line('w0')
"   if i % 2 == 0
"     let stopline = stoplinebottom
"   else
"     let stopline = stoplinetop
"   endif
" 
"   try
"     " Limit the search time to 300 msec to avoid a hang on very long lines.
"     " This fails when a timeout is not supported.
"     let [m_lnum, m_col] = searchpairpos(c, '', c2, s_flags, s_skip, stopline, 300)
"   catch /E118/
"     " Can't use the timeout, restrict the stopline a bit more to avoid taking
"     " a long time on closed folds and long lines.
"     " The "viewable" variables give a range in which we can scroll while
"     " keeping the cursor at the same position.
"     " adjustedScrolloff accounts for very large numbers of scrolloff.
"     let adjustedScrolloff = min([&scrolloff, (line('w$') - line('w0')) / 2])
"     let bottom_viewable = min([line('$'), c_lnum + &lines - adjustedScrolloff - 2])
"     let top_viewable = max([1, c_lnum-&lines+adjustedScrolloff + 2])
"     " one of these stoplines will be adjusted below, but the current values are
"     " minimal boundaries within the current window
"     if i % 2 == 0
"       if has("byte_offset") && has("syntax_items") && &smc > 0
" 	let stopbyte = min([line2byte("$"), line2byte(".") + col(".") + &smc * 2])
" 	let stopline = min([bottom_viewable, byte2line(stopbyte)])
"       else
" 	let stopline = min([bottom_viewable, c_lnum + 100])
"       endif
"       let stoplinebottom = stopline
"     else
"       if has("byte_offset") && has("syntax_items") && &smc > 0
" 	let stopbyte = max([1, line2byte(".") + col(".") - &smc * 2])
" 	let stopline = max([top_viewable, byte2line(stopbyte)])
"       else
" 	let stopline = max([top_viewable, c_lnum - 100])
"       endif
"       let stoplinetop = stopline
"     endif
"     let [m_lnum, m_col] = searchpairpos(c, '', c2, s_flags, s_skip, stopline)
"   endtry
" 
"   if before > 0
"     call winrestview(save_cursor)
"   endif
" 
"   " If a match is found setup match highlighting.
"   if m_lnum > 0 && m_lnum >= stoplinetop && m_lnum <= stoplinebottom 
"     exe '3match MatchParen /\(\%' . c_lnum . 'l\%' . (c_col - before) .
" 	  \ 'c\)\|\(\%' . m_lnum . 'l\%' . m_col . 'c\)/'
"     let w:paren_hl_on = 1
"   endif
" endfunction

"--------- MOVING FUNCTIONS ----------------------------------------------- 

function! NextEnv(envname)
    call search('\\begin{' . a:envname . '}','W')
endfunction

function! PrevEnv(envname)
    call search('\\begin{' . a:envname . '}','bW')
endfunction

function! Env_compl(A,P,L)
    let l:envlist=sort(['definition', 'equation', 'proposition', 'theorem', 'lemma', 'array', 'tikzpicture', 'tabular', 'table', 'align\*\?', 'alignat\*\?', 'proof', 'corollary', 'enumerate', 'examples\?', 'itemize', 'remark', 'notation', 'center', 'quotation', 'quote', 'tabbing', 'picture', 'minipage', 'list', 'flushright', 'flushleft', 'figure', 'eqnarray', 'description', 'thebibliography', 'titlepage', 'verbatim', 'verse' ])
    let l:returnlist=[]
    for l:env in l:envlist
	if l:env =~ '^' . a:A 
	    call add(l:returnlist,l:env)
	endif
    endfor
    return l:returnlist
endfunction
"--------- MAPPINGS -------------------------------------------------------
" Add mappings, unless the user didn't want this.
if !exists("no_plugin_maps") && !exists("no_atp_maps")

    map  <buffer> <LocalLeader>v		:call ViewOutput() <CR><CR>
    map  <buffer> <F2> 				:SpecialSpaceToggle<CR>
    map  <buffer> <F3>        			:call ViewOutput() <CR><CR>
    imap <buffer> <F3> <Esc> 			:call ViewOutput() <CR><CR>
    map  <buffer> <LocalLeader>g 		:call Getpid()<CR>
    map  <buffer> <LocalLeader>t		:TOC<CR>
    map  <buffer> <LocalLeader>L		:Labels<CR>
    map  <buffer> <LocalLeader>l 		:TEX<CR>	
    map  <buffer> 2<LocalLeader>l 		:2TEX<CR>	 
    " imap <buffer> <LocalLeader>l	<Left><ESC>:TEX<CR>a
    " imap <buffer> 2<LocalLeader>l	<Left><ESC>:2TEX<CR>a
    " todo: this is nice idea but it do not works as it should: 
    " map  <buffer> <f4> [d:let nr = input("which one: ")<bar>exe "normal " . nr . "[\t"<cr> 
    map  <buffer> <f5> 				:call VTEX() <cr>	
    map  <buffer> <s-f5> 			:call ToggleAuTeX()<cr>
    imap <buffer> <f5> <left><esc> 		:call VTEX() <cr>a
    map  <buffer> <localleader>sb		:call SimpleBibtex()<cr>
    map  <buffer> <localleader>b		:call Bibtex()<cr>
    map  <buffer> <f6>d 			:call Delete() <cr>
    imap <buffer> <silent> <f6>l 		:call OpenLog() <cr>
    map  <buffer> <silent> <f6>l 		:call OpenLog() <cr>
    map  <buffer> <localleader>e 		:cf<cr> 
    map  <buffer> <f6>w 			:call texlog("-w")<cr>
    imap <buffer> <f6>w 			:call texlog("-w")<cr>
    map  <buffer> <f6>r 			:call texlog("-r")<cr>
    imap <buffer> <f6>r 			:call texlog("-r")<cr>
    map  <buffer> <f6>f 			:call texlog("-f")<cr>
    imap <buffer> <f6>f 			:call texlog("-f")<cr>
    map  <buffer> <f6>g 			:call pdffonts()<cr>
    map  <buffer> <f1> 	   			:!clear;texdoc -m 
    imap <buffer> <f1> <esc> 			:!clear;texdoc -m  
    map  <buffer> <localleader>p 		:call print('','')<cr>

    " FONT MAPPINGS
    imap <buffer> ##rm \textrm{}<Left>
    imap <buffer> ##it \textit{}<Left>
    imap <buffer> ##sl \textsl{}<Left>
    imap <buffer> ##sf \textsf{}<Left>
    imap <buffer> ##bf \textbf{}<Left>
	    
    imap <buffer> ##mit \mathit{}<Left>
    imap <buffer> ##mrm \mathrm{}<Left>
    imap <buffer> ##msf \mathsf{}<Left>
    imap <buffer> ##mbf \mathbf{}<Left>

    " GREEK LETTERS
    imap <buffer> #a \alpha
    imap <buffer> #b \beta
    imap <buffer> #c \chi
    imap <buffer> #d \delta
    imap <buffer> #e \epsilon
    imap <buffer> #f \phi
    imap <buffer> #y \psi
    imap <buffer> #g \gamma
    imap <buffer> #h \eta
    imap <buffer> #k \kappa
    imap <buffer> #l \lambda
    imap <buffer> #i \iota
    imap <buffer> #m \mu
    imap <buffer> #n \nu
    imap <buffer> #p \pi
    imap <buffer> #o \theta
    imap <buffer> #r \rho
    imap <buffer> #s \sigma
    imap <buffer> #t \tau
    imap <buffer> #u \upsilon
    imap <buffer> #vs \varsigma
    imap <buffer> #vo \vartheta
    imap <buffer> #w \omega
    imap <buffer> #x \xi
    imap <buffer> #z \zeta

    imap <buffer> #D \Delta
    imap <buffer> #Y \Psi
    imap <buffer> #F \Phi
    imap <buffer> #G \Gamma
    imap <buffer> #L \Lambda
    imap <buffer> #M \Mu
    imap <buffer> #N \Nu
    imap <buffer> #P \Pi
    imap <buffer> #O \Theta
    imap <buffer> #S \Sigma
    imap <buffer> #T \Tau
    imap <buffer> #U \Upsilon
    imap <buffer> #V \Varsigma
    imap <buffer> #W \Omega

    imap <buffer> [b \begin{}<Left>
    imap <buffer> [e \end{}<Left>
    imap [s \begin{}<CR>\end{}<Up><Right>

    imap <buffer> ]c \begin{center}<Cr>\end{center}<Esc>O
    imap <buffer> [c \begin{corollary}<Cr>\end{corollary}<Esc>O
    imap <buffer> [d \begin{definition}<Cr>\end{definition}<Esc>O
    imap <buffer> ]e \begin{enumerate}<Cr>\end{enumerate}<Esc>O
    imap <buffer> [q \begin{equation}<Cr>\end{equation}<Esc>O
    imap <buffer> [a \begin{align}<Cr>\end{align}<Esc>O
    imap <buffer> [x \begin{example}<Cr>\end{example}<Esc>O
    imap <buffer> ]q \begin{equation}<Cr>\end{equation}<Esc>O
    imap <buffer> ]l \begin{flushleft}<Cr>\end{flushleft}<Esc>O
    imap <buffer> ]r \begin{flushright}<Cr>\end{flushright}<Esc>O
    imap <buffer> [i \item
    imap <buffer> ]i \begin{itemize}<Cr>\end{itemize}<Esc>O
    imap <buffer> [l \begin{lemma}<Cr>\end{lemma}<Esc>O
    imap <buffer> [n \begin{note}<Cr>\end{note}<Esc>O
    imap <buffer> [o \begin{observation}<Cr>\end{observation}<Esc>O
    imap <buffer> ]p \begin{proof}<Cr>\end{proof}<Esc>O
    imap <buffer> [p \begin{proposition}<Cr>\end{proposition}<Esc>O
    imap <buffer> [r \begin{remark}<Cr>\end{remark}<Esc>O
    imap <buffer> [t \begin{theorem}<Cr>\end{theorem}<Esc>O
    imap <buffer> ]t \begin{center}<CR>\begin{tikzpicture}<CR><CR>\end{tikzpicture}<CR>\end{center}<Up><Up>

    " imap {c \begin{corollary*}<Cr>\end{corollary*}<Esc>O
    " imap {d \begin{definition*}<Cr>\end{definition*}<Esc>O
    " imap {x \begin{example*}\normalfont<Cr>\end{example*}<Esc>O
    " imap {l \begin{lemma*}<Cr>\end{lemma*}<Esc>O
    " imap {n \begin{note*}<Cr>\end{note*}<Esc>O
    " imap {o \begin{observation*}<Cr>\end{observation*}<Esc>O
    " imap {p \begin{proposition*}<Cr>\end{proposition*}<Esc>O
    " imap {r \begin{remark*}<Cr>\end{remark*}<Esc>O
    " imap {t \begin{theorem*}<Cr>\end{theorem*}<Esc>O

    imap <buffer> __ _{}<Left>
    imap <buffer> ^^ ^{}<Left>
    imap <buffer> [m \[\]<Left><Left>
endif

" This is an additional syntax group for enironment provided by the TIKZ
" package, a very powerful tool to make beautiful diagrams, and all sort of
" pictures in latex.
syn match texTikzCoord '\(|\)\?([A-Za-z0-9]\{1,3})\(|\)\?\|\(|\)\?(\d\d)|\(|\)\?'

" COMMANDS
command! -buffer SetErrorFile 		:call SetErrorFile()
command! -buffer -nargs=? ShowOptions 	:call ShowOptions(<f-args>)
command! -buffer GPID 			:call Getpid()
command! -buffer CXPDF 			:echo s:xpdfpid()
command! -buffer -nargs=? -count=1 TEX  :call TEX(<count>,<f-args>)
command! -buffer -nargs=? -count=1 VTEX	:call VTEX(<count>,<f-args>)
command! -buffer SBibtex 		:call SimpleBibtex()
command! -buffer -nargs=? Bibtex 	:call Bibtex(<f-args>)
command! -buffer -nargs=? -complete=buffer FindBibFiles 	:echo keys(FindBibFiles(<f-args>))
command! -buffer -nargs=* BibSearch	:call BibSearch(<f-args>)
command! -buffer -nargs=? DefiSearch	:call DefiSearch(<f-args>)
command! -buffer TOC 			:call TOC()
command! -buffer CTOC 			:call CTOC()
command! -buffer Labels			:call Labels() 
command! -buffer SetOutDir 		:call s:setoutdir(1)
command! -buffer ATPStatus 		:call ATPStatus() 
command! -buffer -nargs=? SetErrorFormat 	:call s:SetErrorFormat(<f-args>)
command! -buffer -nargs=? -complete=custom,ListErrorsFlags ShowErrors 	:call s:ShowErrors(<f-args>)
command! -buffer -nargs=? -complete=buffer	 FindInputFiles		:call FindInputFiles(<f-args>)
command! -buffer -nargs=* -complete=customlist,EI_compl	 EditInputFile 		:call EditInputFile(<f-args>)
command! -buffer -nargs=? -complete=buffer	 ToDo 			:call ToDo('\c\<todo\>','\s*%\c.*\<note\>',<f-args>)
command! -buffer -nargs=? -complete=buffer	 Note			:call ToDo('\c\<note\>','\s*%\c.*\<todo\>',<f-args>)
command! -buffer SpecialSpaceToggle	:call SpecialSpaceToggle()
command! -buffer SetXdvi		:call SetXdvi()
command! -buffer SetXpdf		:call SetXpdf()	
command! -complete=custom,ListPrinters  -buffer -nargs=* SshPrint	:call Print(<f-args>)

command! -buffer -nargs=1 -complete=customlist,Env_compl NEnv			:call NextEnv(<f-args>)
command! -buffer -nargs=1 -complete=customlist,Env_compl PEnv			:call PrevEnv(<f-args>)
ftplugin/bibsearch_atp.vim	[[[1
101
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Feb 4
" URL:		

"
" Status Line:
function! Status()
    return "Bibsearch: " . substitute(expand("%"),"___","","g")
endfunction
setlocal statusline=%{Status()}
" MAPPINGS
if !exists("no_plugin_maps") && !exists("no_atp_bibsearch_maps")
    map <buffer> c :call BibChoose()<CR>
    map <buffer> y :call BibChoose()<CR>
    map <buffer> p :call BibChoose()<CR>
    map <buffer> q :hide<CR>
    command! -buffer -nargs=* BibChoose 	:call BibChoose(<f-args>)
endif

if !exists("*BibChoose")
function! BibChoose(...)
    let l:which=input("Which entry? ( <Number><reg name><Enter>, <Number><Enter> or <Enter> for none) ")
    if l:which =~ '\<\d*\>'
	let l:start=stridx(b:listofkeys[l:which],'{')+1
	let l:choice=substitute(strpart(b:listofkeys[l:which],l:start),',','','')
	q
	let l:line=getline(".")
	let l:col=col(".")
	let l:line=strpart(l:line,0,l:col) . l:choice . strpart(l:line,l:col)
	call setline(line("."), l:line)
    elseif l:which =~ '\<\d*\a\>'
	    let l:letter=substitute(l:which,'\d','','g')
	    let l:which=substitute(l:which,'\a','','g')
	    let l:start=stridx(b:listofkeys[l:which],'{')+1
	    let l:choice=substitute(strpart(b:listofkeys[l:which],l:start),',','','')
	    silent if l:letter == 'a'
		let @a=l:choice
	    elseif l:letter == 'b'
		let @b=l:choice
	    elseif l:letter == 'c'
		let @c=l:choice
	    elseif l:letter == 'd'
		let @d=l:choice
	    elseif l:letter == 'e'
		let @e=l:choice
	    elseif l:letter == 'f'
		let @f=l:choice
	    elseif l:letter == 'g'
		let @g=l:choice
	    elseif l:letter == 'h'
		let @h=l:choice
	    elseif l:letter == 'i'
		let @i=l:choice
	    elseif l:letter == 'j'
		let @j=l:choice
	    elseif l:letter == 'k'
		let @k=l:choice
	    elseif l:letter == 'l'
		let @l=l:choice
	    elseif l:letter == 'm'
		let @m=l:choice
	    elseif l:letter == 'n'
		let @n=l:choice
	    elseif l:letter == 'o'
		let @o=l:choice
	    elseif l:letter == 'p'
		let @p=l:choice
	    elseif l:letter == 'q'
		let @q=l:choice
	    elseif l:letter == 'r'
		let @r=l:choice
	    elseif l:letter == 's'
		let @s=l:choice
	    elseif l:letter == 't'
		let @t=l:choice
	    elseif l:letter == 'u'
		let @u=l:choice
	    elseif l:letter == 'v'
		let @v=l:choice
	    elseif l:letter == 'w'
		let @w=l:choice
	    elseif l:letter == 'x'
		let @x=l:choice
	    elseif l:letter == 'y'
		let @y=l:choice
	    elseif l:letter == 'z'
		let @z=l:choice
	    elseif l:letter == '*'
		let @-=l:choice
	    elseif l:letter == '+'
		let @+=l:choice
	    elseif l:letter == '-'
		let @@=l:choice
	    endif
	    echohl WarningMsg | echomsg "Choice yanekd to the register '" . l:letter . "'" | echohl None
    endif
endfunction
endif

ftplugin/toc_atp.vim	[[[1
308
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Feb 14
" URL:		

function! ATP_TOC_StatusLine()
    if expand("%") == "__ToC__"
	return "Table of Contents"
    elseif expand("%") == "__Labels__"
	return "List of Labels"
    endif
endfunction
setlocal statusline=%{ATP_TOC_StatusLine()}

function! s:getlinenr(...)
    if a:0 == 0
	let l:line=getline('.')
    else
	let l:line=getline(a:1)
    endif
    let l:nr=substitute(matchstr(l:line,'^\s*\d\+'),'^\s*','','')
    return l:nr
endfunction

" Get the file name and its path from the LABELS/ToC list.
function! s:file()
    let l:true=1
    let l:linenr=line('.')
    while l:true == 1
	let l:line=s:getlinenr(l:linenr)
	if l:line != ""
	    let l:linenr-=1
	else
	    let l:true=0
	    " NOTE THAT FILE NAME SHOULD NOT INCLUDE '(' and ')' and SHOULD
	    " NOT BEGIN WITH A NUMBER.
	    let l:line=getline(l:linenr)
	    let l:bufname=strpart(l:line,0,stridx(l:line,'(')-1)
	    let l:path=substitute(strpart(l:line,stridx(l:line,'(')+1),')\s*$','','')
" 	    echomsg "BUFNAME " . l:bufname
" 	    echomsg "PATH " . l:path
	endif
    endwhile
    return [ l:path, l:bufname ]
endfunction
command! File	:echo s:file()
 
"---------------------------------------------------------------------
" Notes:
" 		(1) choose window with matching buffer name
" 		(2) choose among those choose the one which we eddited last
" Solution:
"        			       --N-> choose this window
"			 	       |
"			     --N-> ----|
"			     | 	       --Y-> choose that window		
" --go from where you come-->|         Does there exists another open window 
"  			     |	       with the right buffer name?
"			     |	
"  			     --Y-> use this window
"			   Does the window has
"			   a correct name?
"
" This function returns the window number to which we will eventually go.
function! s:gotowinnr()
    " This is the line number to which we will go.
    let l:nr=s:getlinenr()
    " t:bufname
    " t:winnr		were set by TOC(), they should also be set by
    " 			autocommands
    let l:buf=s:file()
    let l:bufname=l:buf[0] . "/" . l:buf[1]

    if t:bufname == l:bufname
	" if t:bufname agree with that found in ToC
	" if the t:winnr is still open
	if bufwinnr(t:bufname) != -1
	    let l:gotowinnr=t:winnr
" 	    echomsg "DEBUG A"
	else
	    let l:gotowinnr=-1
" 	    echomsg "DEBUG B"
	endif
" 	echomsg "DEBUG C " . l:gotowinnr
    else
 	if bufwinnr("^" . l:bufname . "$") != 0
	    " if not but there is a window with buffer l:bufname
	    let l:gotowinnr=bufwinnr("^" . l:bufname . "$")
 	else
	    " if not and there is no window with buffer l:bufname
 	    let l:gotowinnr=t:winnr
 	endif
    endif
    return l:gotowinnr
endif
endfunction

function! GotoLine(closebuffer)
    
    let l:buf=s:file()

    " remember the ToC window number
    let l:tocbufnr=bufnr("")

    " line to go to
    let l:nr=s:getlinenr()

    " window to go to
    let l:gotowinnr=s:gotowinnr()

    if l:gotowinnr != -1
 	exe l:gotowinnr . " wincmd w"
    else
 	exe l:gotowinnr . " wincmd w"
	exe "e " . fnameescape(l:buf[0] . "/" . l:buf[1])
    endif
	
    "if we were asked to close the window
    if a:closebuffer == 1
	exe "bdelete " . l:tocbufnr
    endif

    "finally, set the position
    call setpos('.',[0,l:nr,1,0])
    exe "normal zt"
    
endfunction
" endif

function! s:yank(arg)

    let l:cbufnr=bufnr("")
    let l:buf=s:file()
    let l:bufname=l:buf[1]
    let l:filename=l:buf[0] . "/" . l:buf[1]
    let b:fn=l:filename

    if exists("t:labels")
	let l:choice=get(t:labels[l:filename],s:getlinenr())
    else
	let l:choice="nokey"
    endif

    if l:choice=="nokey"
	" in TOC, if there is a key we will give it back if not:
	echomsg "There is no key."
	return ""
    else
	if a:arg == '@'
	    let l:letter=input("To which register? <reg name><Enter> or empty for none ")
	    silent if l:letter == 'a'
		let @a=l:choice
	    elseif l:letter == 'b'
		let @b=l:choice
	    elseif l:letter == 'c'
		let @c=l:choice
	    elseif l:letter == 'd'
		let @d=l:choice
	    elseif l:letter == 'e'
		let @e=l:choice
	    elseif l:letter == 'f'
		let @f=l:choice
	    elseif l:letter == 'g'
		let @g=l:choice
	    elseif l:letter == 'h'
		let @h=l:choice
	    elseif l:letter == 'i'
		let @i=l:choice
	    elseif l:letter == 'j'
		let @j=l:choice
	    elseif l:letter == 'k'
		let @k=l:choice
	    elseif l:letter == 'l'
		let @l=l:choice
	    elseif l:letter == 'm'
		let @m=l:choice
	    elseif l:letter == 'n'
		let @n=l:choice
	    elseif l:letter == 'o'
		let @o=l:choice
	    elseif l:letter == 'p'
		let @p=l:choice
	    elseif l:letter == 'q'
		let @q=l:choice
	    elseif l:letter == 'r'
		let @r=l:choice
	    elseif l:letter == 's'
		let @s=l:choice
	    elseif l:letter == 't'
		let @t=l:choice
	    elseif l:letter == 'u'
		let @u=l:choice
	    elseif l:letter == 'v'
		let @v=l:choice
	    elseif l:letter == 'w'
		let @w=l:choice
	    elseif l:letter == 'x'
		let @x=l:choice
	    elseif l:letter == 'y'
		let @y=l:choice
	    elseif l:letter == 'z'
		let @z=l:choice
	    elseif l:letter == '*'
		let @-=l:choice
	    elseif l:letter == '+'
		let @+=l:choice
	    elseif l:letter == '-'
		let @@=l:choice
	    endif
	    echohl WarningMsg | echomsg "Choice yanked to the register '" . l:letter . "'" | echohl None
	elseif a:arg =='p'

	    let l:gotowinnr=s:gotowinnr()
	    exe l:gotowinnr . " wincmd w"

	    " delete the buffer
	    exe "bdelete " . l:cbufnr

	    " set the line
	    let l:line=getline('.')
	    let l:colpos=getpos('.')[2]
	    let l:bline=strpart(l:line,0,l:colpos)
	    let l:eline=strpart(l:line,l:colpos)
	    call setline('.',l:bline . l:choice . l:eline)
	    call setpos('.',[getpos('.')[0],getpos('.')[1],getpos('.')[2]+len(l:choice),getpos('.')[3]])
	endif
    endif
endfunction

command! -buffer P :call Yank("p")

if !exists("*YankToReg")
function! YankToReg()
    call s:yank("@")
endfunction
endif

if !exists("*Paste")
function! Paste()
    call s:yank("p")
endfunction
endif
command! -buffer -nargs=1 Y :call YankToReg(<f-arg>)

if !exists("*ShowLabelContext")
function! ShowLabelContext()
    let l:cbufname=bufname('%')
    let l:bufname=s:file()[1]
    let l:bufnr=bufnr("^" . l:bufname . "$")
    let l:winnr=bufwinnr(l:bufname)
" 	echomsg "DEBUG bufname " . l:bufname
    let l:line=s:getlinenr()
    if !exists("t:labels")
	let t:labels=UpdateLabels(l:bufname)
    endif
    exe l:winnr . " wincmd w"
	if l:winnr == -1
	    exe "e #" . l:bufnr
	endif
    exe "12split "
    call setpos('.',[0,l:line,1,0])
endfunction
endif

if !exists("*EchoLabel")
function! EchoLabel()
    let l:bufname=s:file()[1]
    let l:bufnr=bufnr("^" . l:bufname . "$")
    if !exists("t:labels")
	let t:labels[l:bufname]=UpdateLabels(l:bufname)[l:bufname]
    endif
    let l:line=s:getlinenr()
    echo getbufline(l:bufname,l:line)
endfunction
endif

" function! s:bdelete()
"     call s:deletevariables()
"     bdelete
" endfunction
" command -buffer Bdelete 	:call s:bdelete()

" TODO:
" function! Update()
"     l:cbufname=bufname("")
"     let l:bufname=substitute(l:cbufname,'\C\%(-TOC\|-LABELS\)$','','')
"     let l:bufnr=bufnr("^" . l:bufname . "$")
"     let t:labels[l:bufname]=UpdateLabels(l:bufname)[l:bufname]
"     if l:cbufname =~ "-TOC$"
" 	" TODO
"     elseif l:cbufname =~ "-LABELS$"
" 	" TODO
"     endif
" endfunction


" MAPPINGS
if !exists("no_plugin_maps") && !exists("no_atp_toc_maps")
    map <silent> <buffer> q 		:bdelete<CR>
    map <silent> <buffer> <CR> 		:call GotoLine(1)<CR>
    map <silent> <buffer> <space> 	:call GotoLine(0)<CR>
    map <buffer> c 			:call YankToReg()<CR>
    map <buffer> y 			:call YankToReg()<CR>
    noremap <silent> <buffer> p 	:call Paste()<CR>
    noremap <silent> <buffer> s 	:call ShowLabelContext()<CR> 
    noremap <silent> <buffer> e 	:call EchoLabel()<CR>
endif
doc/automatic-tex-plugin.txt	[[[1
1343
			      automatic-tex-plugin* *atp* 
			Introduction to Automatic (la)TeX Plugin
				by Marcin Szamotulski
			----------------------------------------

If you found this plugin useful, you are cordially invited to write to me:
mszamot@gmail.com. This is my first vim plugin, and its a great pleasure 
to get even the smallest message.


					Abstract
					========	

This is a new plugin for vim to comfortably write TeX (LaTeX, PdfLaTeX)
documents, which provides functionality not met in other such plugins. It
makes you FREE from compiling procedure, making this process automatic using
autocommands. It also provides useful mappings and other functions: analyse
your .log file to see the table contents, to search for a label, to search in
bib files.  To have full functionality you need: texloganalyser (a perl
program written by Thomas van Oudenhove) and pdffonts available in the package
app-text/poppler-utils (at least in Gentoo). Another good tool is texdoc,
which is a part of texlive - these days standard TeX distribution for Linux,
and MikTeX on Windows.

Features include:
* a powerful function to search in bibliographic files (bib files):
	See |atp-bibsearch|,
* table of contents which allows to switch between different '.tex' files:
	See |atp-TOC()|,
* list of labels which allows to see the context of a label:
	See |atp-Labels()|,
* a command to list ToDo lines:
	See |atp-ToDo()|.

                                                		*atp-help-toc*
|atp-installation| 	Installation								
|atp-functions| 	Functions
|atp-bibsearch|		Searching in bib files
|atp-configure| 	How to configure to your needs 
|atp-mappings|  	Mappings and Commands
|atp-errors|  		Error handling
|atp-requirements|  	Requirements
|atp-viewers| 		Note about viewers (including inverse and reverse searching for xdvi)
|atp-color-highlight|	Colors and syntax files
|atp-remarks|  		Final remarks
|atp-copy-rights|	Copy Rights

	
Note on usage: type :help atp<CTRL>d to see all the helptags. To see help tags
for all the defined functions :help atp*()<CTRL>d, mappings: :help atp-map<CTRL>d

================================================================================
Installation                               			*atp-installation*
>
	 :filetype plugin on is required to run this plugin, see
	 |:filetype-plugin-on| and |:filetype-indent-on| if you want to have
	 automatic indentation for TeX files.
<
To install you just need to copy tex.vim file to ~your ~/.vim/ftplugin/
directory copy this help file to ~/.vim/doc and then run :helptags ~/.vim/doc
and that's all, now you can just type your story ... :)


================================================================================
Functions                               			*atp-functions*

The main function is not seen by the user (it is called s:compiler, for those
who want to read the plugin). It executes tex compiler specified by the
variable b:texcompiler. It is executed
as an autocommand by the line:
	au! CursorHold $HOME*.tex silent call 's:auTeX()'
where s:auTeX() is a simple function which calls s:compiler if the file written
on the disk and the buffer differ.
As you can see it will run if a key is not pressed during time defined by
option 'updatetime' (see |CursorHold|) in the normal mode. If you type in
insertmode the file wan't be compiled (and that's alright as you can be in the
middle of your very long formula). The value of 'updatetime' which works fine
is around 1000ms ('updatetime' is set in miliseconds). Tex compiler is run with
two options:
	-output-directory 
	-jobname
which points to a unique temporary file in vim temporary directory (using the
function 'tempname()' (see |tempname()|. If you are concerned with security
reasons read also: |shelltemp|.

You can switch off/on the function s:auTeX by pressing <S-F5> or by letting
the local to buffer variable b:autex=1 (on) b:autex=0 (off). It is useful in
some situations turn automatic compiling off. The key <S-F5> calls the function
ToggleAuTex() which sets the variable b:autex and issue a message. You can also
set this variable to 0 for some files that are not supposed to be processed,
for example:
>
    au BufRead texmf/*.tex let b:autex=0
<
The second important variable b:texcompiler (see |atp-texcompiler|) configures
if you use TeX, PdfTeX, LaTeX, PdfLaTeX and it should point to the program
name so please do not use capital letters.

Next variable to set is b:outdir (see |atp-outdir|). It configures where TeX
will put the output and where viewer and log analyzing tools can find
appropriate files. 

The last top most important variable is |atp-keep| which is a list of extensions,
by default it is
	let g:keep = ["log","aux","toc","bbl"]
Files with this extension will be copied from b:outdir to the temporary
directory with appropriate name to be used when (La)TeX is compiling. (log file
will be only copied after it is created, other files will be copied back and
forth between you b:outdir and the temporary directory)

There is also a variable which stores the last command which executed
your tex compiler, see |atp-texcommand|.   

The last remark is that it is convenient to see if tex is running, if you are
using a simple window manager like DWM or WMII or some others you can do that
in your .xinitrc file. If you are using more complex window manager (Gnome,
KDE) you can use the command GPID (which is mapped to \g). It returns pid
of |atp-texcompiler| if it runs. The vim status line could be used for that but
it seems to be to slow and the screen gets messy (at least for me).

If you have defined a function with the same name it wan't be overwritten.
You can check where the function was defined by the command
	:verbose function TEX
the last component is the name of the function (without () at the end).	

Below I explain functions which are defined:
/all of them do not have any arguments/

TEX([runs])						*atp-TEX()*
map \l,imap \l, :TEX 
	If anyway you want to run TeX yourself but you do not want to see the
	output this is the right tool. This runs TeX in 'nonstopmode'. You can
	specify an argument 'runs' which tells how many consecutive runs of
	TeX you need (this is important if you want to compile Table of
	Contents, or index, or the bibliography (see |atp-Bibtex()|)

	If b:openviewer=1 and there current viewer (b:Viewer) is not running on the
	output file then this function will open a viewer. By default b:openviewer=0
	and this feature is disabled. 

	The command :2TEX will run :call TEX(2), :TEX 3 do :call TEX(3), and
	:2TEX3 will resolve to :call TEX(3).

	It is useful when you want to make the outline (using hyperref
	package) of your article in pdf files, the tex file has to be
	'sourced' twice. To make the bibliography you can use |atp-Bibtex()|.

	If runs > 5 it will be reduced to 5, to avoid running tex for hundreds
	(or event thousands) of times (what could happen otherwise by
	a mistake giving the range of the command to be the current line
	number).

VTEX([runs])						*atp-VTEX()*
map <F5>,imap <F5>, :VTEX 
	Verbose TeX, if you want to see the log file after tex compiler is
	called. This runs TeX in the errorstop mode. 

	If b:openviewer=1 and there current viewer (b:Viewer) is not running
	on the ouput file then this function will open a viewer. By default
	b:openviewer=0 and this feature is disabled. 

	If the variable 'runs' is specified is given then VTEX will call the
	tex compiler as many times as given. Only the last time is with
	errorstop mode.
	
	The command :2VTEX will run :call VTEX(2), :VTEX 3 do :call VTEX(3), and
	:2VTEX3 will resolve to :call VTEX(3).

ShowErrors						*atp-ShowErrors*
:ShowErrors <flag>
	This command shows error/warning messages. It sets the |'errorformat'|
	variable accordingly to the flag, which is a word made of letters:
>
		e		- include errors
		w		- include all warning messages
		r		- include all reference warnings
		c		- include all citations warnings
		f		- include all font warnings
<
	If none flag is given 'e' is used.
	Example:
>
		:ShowErrors rc
<
	will show all reference and citation warnings.

	
SetErrorFormat						*atp-SetErrorFormat* 
:SetErrorFormat <flag> 	
	This command has the same syntax as :ShowErrors. It only sets the
	|'erroformat'| variable.
	
Bibtex([debug])						*atp-Bibtex()* 
map \b, :Bibtex, :Bibtex v
	This function will call bibtex to produce the bibliography file
	(.bbl). If in |atp-outdir| there is no 'aux' file it first calls tex
	compiler. After the 'bbl' file is produced two consecutive runs of tex
	compiler are called to make the bibliography.

	If you specify any value to the debug variable then then this function
	will be called in verbose mode (only the last time tex compiler will
	run in errorstop mode). This gives you the chance to see the output of
	bibtex command for a second. The command :Bibtex v is associated to
	this behaviour. If you want to just run bibtex see the next function.

	The command :Bibtex  will :call Bibtex(), while :Bibtex v
	(and :Bibtex <anything>) will :call Bibtex(1)

							*atp-BIBINPUTS*
	Bibtex is looking for the date base files in the current directory
	otherwise in the directory defined by $BIBINPUTS environment variable. 
	You can set in in your environment. If it is not set it will be
	set to g:texmf/bibtex (so the default value is $HOME/texmf/bibtex).

SimpleBibtex()						*atp-SimpleBibtex()*
map \sb, :SBibtex
	This calls bibtex on the aux file in your |atp-outdir| directory and
	shows you the output. It is useful if you are debugging your
	bibliography database. 
	
ViewOutput()						*atp-ViewOutput()*
map \v,map <F3>, imap \v, imap <F3>  
	You would like to see what you are editing use this function. It will
	use the program defined in the variable b:Viewer. See |atp-Viewer|,
	|atp-XpdfServer|, |atp-XpdfOptions|. When there is no output file it will run
	TeX and open the file. Read more about particular viewers
	(inverse/reverse searching) in |atp-viewers|. 

:SetXdvi						*atp-SetXdvi*
	This command sets the options for xdvi viewer, which enables inverse
	and reverse searching. It sets the command
		:IS[earch]
	and the map '<LocalLeader>is' for inverse searching. For reverse
	searching hold CTRL and click with left mouse button on the text in
	xdvi viewer.

:SetXpdf						*atp-SetXpdf*
	This command sets the options for xpdf viewer (as for now the
	inverse/reverse searching in pdf files is not implemented)

BibSearch({pattern},{flag})				see |atp-BibSearch|
:BibSearch
	This function finds bib entries in bib files defined in your tex file and in the
	variable b:bibfiles (see |atp-bibfiles|), which match the
	pattern. The output is configurable by the flag argument, see
	|atp-bibflags|.

BibChoose()						see |atp-BibChoose|
:BibChoose, map c, map y, map p
	This function is defined in the window with results of BibSearch
	command. It is mapped to 'y' and 'c' and let you copy the bib entry key
	to a register (see |atp-BibChoose|) or directly to last opened
	buffer (after the last cursor position). When you paste it will close
	the Bibsearch window.

FindBibFiles({bufname})					*atp-FindBibFiles()*
:FindBibFiles
	This updates the variables s:bibfiles, s:allbibfiles,
	s:notreadablebibfiles (showed by ShowOptions command). Finds all bib
	files defined in all '\bibliography' commands. For more about the
	above variables read |atp-variables-bib|. This function is called
	internally be the script functions BibSearch/BibChoose/ShowOptions.
	The command :FindBibFiles finds bib files in the current buffer. 

	If a readable bib file was not found in b:outdir and $BIBINPUTS
	(see |atp-BIBINPUTS|) directories it is classified as not readable.  

FindInputFiles([bufname])				*atp-FindInputFiles()*
:FindInputFiles [bufname]
	This function finds all the input files, i.e. files put after the
	tex commands: '\input', '\include' or '\includeonly'. And prints the
	reuslt. 
	The function and the command have one optional argument - the buffer
	name. By default it searches for input files in the current buffer.
	The bufername complition is set, hence you can use <Tab> to choose the
	bufername.

EditInputFile([input_file_name],[bufname])				*atp-EditInputFile()*
:EditInputFile [input_file_name] [bufname]
	This function finds input files (using the function FindInputFiles()
	in the current buffer and let you choose one to edit.  window. 

	The first as well as the second argument are optional. There is
	completion for the input file name, so that you can just press <Tab>
	to switch between input files. If you want to list the first and then
	choose do not pass any arguments.

	The input file name may contain white spaces, but then has to be quoted
	with '"' (this is a latex requirement).

	Input files are searched in the |atp-outdir| directory, if not found
	there then in the directory |atp-texmf|. The default value is 
	g:texmf=$HOME/texmf, which is the default value of local texmf tree in
	texlive.

	The bibliographis declared are also listed. The command searches for
	them in the b:outdir and if not found there in $BIBINPUTS directory.
	(see: man bibtex)

OpenLog()						*atp-OpenLog()*
map <F6>l, imap <F6>l
	Opens log file in a new tab with two options (which are set
	locally): ruler, nospell.	

	You can also use the standard command (distributed with vim)
	":Texplore" to see log,aux,... files.

Delete()						*atp-Delete()*
map <F6>d
	Delets the files produced by TeX wich extensions belongs to
	g:texextensions (see |atp-texextensions|). It removes not taking care
	about the name, i.e. it runs like: rm *.out.

Print(<printer>,<printer_options>			*atp-Print()*
map \p,:SshPrint
	It will run 'lpr' command and append to it the options defined in the
	variable 'g:printeroptions' + options given in the second argument. It
	prints the pdf or dvi depending on the value of 'b:texcompiler' (see
	|atp-texcompiler|).  If you specify the variable
	'g:atp_ssh=<user>@<host>' it will print via ssh on the <host> using
	the <printer>. The command ':SshPrint' has a completion set for the
	printers available on your local system or in the host. All the
	arguments of the command SshPrint are |<f-agrs>|. 
	
	The map '\p' will print on the default printer.

ShowOptions()						*atp-show-options* *atp-ShowOptions()*
:ShowOptions, :ShowOptions v 
	This will show values of variables that are currently set. If you specify any
	argument the deafult values will be shown is square brackets.
>
		:ShowOptions v
<		
TOC()							*atp-TOC()*
map \t, :TOC
	Shows Table of Contents of your document. It do not yet support the
	started version of chapter, section,... environemnts. It opens new
	window in which you can use the mappings: 

		'e' 	to echo the line from your tex file
		'y' 	to yank the label of the chapter under the curosor
				to a register, if it exists,
	 	'p' 	to paste it directly to your tex file (just after the
				current cursor position), 
		's'	it splits the window with your tex source file and
			sets the current line to the beginning of the
			chapter/section under the cursor,
		'q' 	to quit, and finaly, 
		<Enter> to go to the chapter under the cursor and close ToC.
		<space> to go to the chapter under the cursor but leave ToC
			open.

	There are also commands: ':C' and ':P', which do the same as 'c' and
	'p' mappings. They all call the function 'Yank(<where>)', the argument
	<where> can be one of: '@<register name>' or 'p'.  

	TOC() supports many eddited files. For example if you have in your
	buffer list two files a.tex and b.tex this command will produce table
	of contents of both of them. If you have just one opened window
	(excluding the ToC window) then pressing <space>, <enter>, p and q
	will take you to the right buffer (which will be read if is unloaded
	or hidden). If you split a window then <space>, <enter>,
	p, q will take you to the window from which you are comming. However,
	if you have two windows with two different buffers loaded they will
	act on the window with the matching buffer name.

	The variable t:toc_window_width sets the width of table of contents
	window. By default t:toc_window_width=30. You can set a global
	variable g:toc_window_width to overide the default value.

CTOC()							*atp-CTOC()*
:CTOC	
	This function returns the name of the currently eddited chapter/
	section/subsection/subsubsection. Use ':echo CTOC()' or just ':CTOC' to
	see the returned value. If you added a section unit the function will
	not update the database, run ':TOC' to do that (map \t).

Labels()						*atp-Labels()*
map \L, :Labels
	Shows labels defined in your file. You can also use the mappings
	'e','c','p','s','q' and <Enter> as above.

	If you forget what are these mappings, write ':map' in the TOC or
	LABELS window.

	The key 's' shows the context of the label under the cursor (your
	current window splits).

	The variable t:labels_window_width sets the width of labels window. By
	default t:labels_window_width=30. You can set a global
	variable g:labels_window_width to overide the default value.

ToDo(<keyword>,<stop>,[bufname])				*atp-ToDo()*
:ToDo [bufname]
:Note [bufname]
	This function list all the lines of the buffer [bufname] which match
	for the pattern '%.*<keyword>'. The <stop> argument is the pattern to
	before which to stop. The optional argument is the buffer
	name (the buffer name complition is set on). If not given the
	current buffer is assumed.
	You can set highlighting for this command by:
		highlight atp-Todo ctermfg=... 	guifg=...
	The command :ToDo sets keyowrd='\c\<todo\>' and
	stop='\s*%.*\c\<note\>', and the command :Note
	sets keyowrd='\c\<note\>' and stop='\s*%.*\c\<todo\>'. This prevent
	from listing ToDo lines with Notes and vice versa. 
	 
SpecialSpaceToggle()	 					*atp-SpecialSpaceToggle*
:SpecialSpaceToggle, map <F2>
	This function (command) sets, if it is undefined or removes if it is
	defined, the mapping:
>
		:cmap <Space> \_s\+
<
	which is useful when searching by the command '/', especially if
	|'textwidth'| or |'wrapmargin'| is non zero (and |'formatoptions'|
	contains one of the flags 't', 'v' or 'b'). Then each <Space> will
	match for a space which also might end of the line.

:SetOutDir						*atp-SetOutDir*
	This is a command wich sets the 'b:outdir' variable and the |'errorfile'| option.
	See |atp-outdir| for the default value.

:SetErrorFile						*atp-SetErrorFile*
	If you change |atp-outdir| variable and you want to update the
	|'errorfile'| option use this command. It will show you the value to
	which |'errorfile'| was set. 

ATPStatus						*atp-ATPStatus*
:ATPStatus
	This function (command) sets the status line, which include: the name
	of currently eddited chapter (or section) the value of 'b:outdir' and
	it will warn you if 'b:outdir' variable is not set. This function is
	called at startup unless the variable 'g:atp_statusline=0' is set (for
	example in you $VIMRC file). The status is set by the autocommand:
>
		au BufWinEnter *.tex :call ATPStatus()
<
	In this way every opened window with a '*.tex' file will get the correct
	status line.


================================================================================
Searching in bib files 		                        *atp-bibsearch*

		___________________________
		Tabel of Contents:
		|atp-BibSearch|
		|atp-bibpatterns|
		|atp-bibflags|
			|atp-bibflags:default|
			|atp-bibsearch-show-only-keys|
			|atp-bibflags:+|
			|atp-bibflags:output|
			|atp-bibflags:all|
			|atp-bibflags:last|
			|atp-bibflags:add-flag|	
		|atp-BibChoose|	
		|atp-bibsearch-highlight|
		|atp-BibSearch-command|
		|atp-bibflags:examples|
		|atp-bibsearch-variables|
			|atp-bibfiles|

		____________________________
		Naming Convensions:	

		@article{<label>,					\	
			author = { .... },		<-- bib entry    | 
			title  = { .... },				 > bib field
			journal= " .... ",				|
		}							/	

			article 		<-- bib field keyword 
			author,title,...	<-- bib entry label 	
			<label>			<-- bib field label 	


One more function is provided which searches the bib files for bib fields, 
and for the bib field labels for the latex command \cite{}.

BibSearch([pattern],[flags])				*atp-BibSearch* 
	The function BibSearch allows you to search for the pattern in bib
	files and opens a new window with results. For the command, please read
	|atp-bibsearch-command|.

	The function BibSearch takes two arguments (the last one is optional).
	The first one is the pattern to match against each line of the
	bibliographic files supplied in the commands \bibliography (if there
	are several names,please do not add a white space ' ' after ',' unless
	the file name begins with a space, i.e.
>
 	\bibliography(Mathematics, Physics,/home/user/Bibliography)
< 
	then the plugin will understand that the names of the bib files are
	'Mathematics.bib', ' Physics.bib' and '/home/user/Bibliography.bib'.

								*atp-bibpatterns*
	Each line of every bib file found in your tex docummnet will be
	matched against the pattern, for example if the pattern is:
>
 		'author.*Grothendieck'
<
	the BibSearch function will find all the bibliographic fields
	which in one line have the words 'author' and 'Grothendieck' (in most
	cases it means that you will see only works of Grothendieck). Another
	example:
>
 	'^\(\s*author.*Joayl\)\|Galois Theory'
<
	will result in all bib fields which author is Joyal or 
	which includes the words 'Galois Theory' (which by the way apear in
	many article/book titles), yet another example:	
>
 	'author.*Joayl\|title\p*Galois Theory'
<
	This will match against all bib entries written by Joyal or which title
	includes the word 'Galois Theory'.
>
 		:call BibSearch('author.*Joyal\&.*Tirney')	
<	
	will find all the bib entries which were written by Joyal and Tirney
	(and maybe somebody else). 

	For now, there is no possibility to filter bibliographic entries which
	both match a pattern in separate lines, i.g. to show all bib entries
	written by Joyal on 'Descent Theory'.

	Before a match, all '{', and '}' are deleted from the line of the bib file.
	But you will see them in the output (what can be useful for debugging
	errors in bib files)

	Note that in vim patterns should be quoted using '...' not "...".   

	Further examples are supplied after the next section
	|atp-bibflags:examples|, which describes other functionalities
	of the BibSearch/BibChoos functions.

								*atp-bibflags*
	The first optional argument [flags] chooses what and in which order
	you want to see the  bib entries found (entries are listed in
	the order they appear in bib file).  Flag is a word made of letters.
	There are three kinds of flags: entry flags which mathes againts
	labels of bib entries, like author, title, etc..., and keyword flags: which
	matches agains keywords of bib fields: @article, @book, @techreport,
	etc...  and two special flags 'All' and 'L'. A flag is a word on
	letters:
>
		a  - author
 		e  - editor
 		t  - title
 		b  - booktitle
 		j  - journal
 		s  - series
 		y  - year
 		n  - number
 		v  - volume
 		p  - pages
 		P  - Publisher
 		N  - Note
 		S  - School
 		h  - howpublished
 		o  - organization
		u  - url	
		H  - Homepage	
  any other letter - do not show anything but the first line of bib entry 
		@a - article 						/@article/
		@b - book or booklet 					/@book,@booklet/
		@B - Booklet 						/@booklet/	
		@c - incollection 					/@incollection,@inbook/
		@p - proceedings, inproceedings, conference   		/@proceedings,@inproceedings,@conference/
		@m - misc 						/@misc/
		@M - Manual 						/@manual/
		@t - master or PhD thesis  				/@masterthesis,@phdthesis/
		@T - Techreport 					/@techreport/
		@u - unpublished  					/@unpublished/		
		All - all flags						(see |atp-bibflags:all|)		
		L   - last flags					(see |atp-bibflags:last|)		
<

	Examples:
>
		tayu@a		--> show the entries: tile, author, year, url of matching articles.
		baeP@b		--> show the entries: booktitle, author, editor, 
 							publisher of matching books (@book,@booklet).
<
	If flags '@.' are filtered out, if one do not belongs to the one above
	then it is deleated. You can see which flags are defined using
	ShowOptions function/command (they are listed as Available
	KeyWordFlags).
								*atp-bibflags:default*
	The default flag is stored in the global variable g:defaultbibflags and is
	equal to 'tabejsyu'. This means that the ouput for each bib field found 
	will include the 
		title
		author
		booktitle
		editor
		journal 
		series
		year
	if title,author,... are specified in the bibliography for the given
	position. If there are many position which match you can set flags to
	be as simple as possible to include more lines on the screen. For
	example 'tabe' is quite reasonable (note that all bib entries are
	matched separately, i.e. if a bib field has both 'title' and 'booktitle'
	bib entries it will give you both of them.

								*atp-bibsearch-show-only-keys*
	If you just want to list just the lines with bib fields keywords:
	@article{, @book{, etc. supply a flag which do not belongs to
	'g:defaultallbibflags', for example 'X', or 'X@a'
	
								*atp-bibflags:+*
	You can also specify flags with '+', for example: 
>
	flags='+p'
	flags='+@b'
<
	This feature ADDS FLAGS TO THE DEFAULT VALUE defined in the variable
	g:defaultbibflags (see |atp-defaulbibflags|). The first will result in
	showing the default entries and the page number, the second will
	result in showing only books with the default bib entries. You can
	specify as many additional flags as you wish.  *atp-bibflags:output*
	Note that the function shows the line with database file name if there
	are entries in this bibliography which match the pattern thus,for
	example, if you specify the flag '@a' and you see the line with
	database file name, but you do not see any bib entry, then in this
	database there are bib fields which match but these are not articles.
	
								*atp-bibflags:all*
	The flags='All' is a synonim of flag=g:defaultallbibflags which by default is
	equal to'tabejfsvnyPNSohiuHcp' i.e. all flags in this order. If you
	add your own flag you should change this global variable. You can add to
	this flag any flag which contains '@' (see |atp-bibflags|) by
	the plus operator, i.e. All+@a@b or +@aAll will give the same result.

								*atp-bibflags:last*
								*atp-lastbibflags*	
	The variable 'b:lastbibflags' stores the recently used flags. The flag
	'L' sets the flags for this search to the value of 'b:lastbibflags'.
	You can write '+L@a', '+L@a', 'L@a' or '@aL' but not '+@La', if you
	want to add some flags to previous searches. Next time the flag 'L'
	will change the meaning (i.e. it is really the last time not earlier
	:) However, there is no '-' :( '-@aL' could be helpful.
	 
	The variable 'b:lastbibflags' is not changed when you use the 'All'
	flag.

								*atp-bibflags:add-flag*
	You can add your own flags but not keyword flags (i.e. @a,@b,...).
	Just add an entry to the dictionary g:bibflagsdict. (:ShowOptions v to
	see its current value), For example
>
	let g:bibflagsdict=extend(g:bibflagsdict, { '<flags_name>' : [
	'<bib_entry_name>': '<how_to_show>'] })
< 
	where, <flags_name> is the flag to use (it should be one letter), it
	must be different from the defined flags, <bib_entry_name> is a
	lowercase bib entry name, like 'title', 'url', etc., <how_to_show> if
	you want to have a nice output put the bib entry name and that much of
	white spaces to get 13 strings.
		
BibChoose							*atp-BibChoose*
map c, map y, map p
	This function/command is only available in the window with BibSearch results
	and allows to copy a bib entry key to a register or directly to the
	last opened buffer (after the cursor position). It is mapped to 'c'
	and 'y'. You will be asked to give the number of bib entry to yank:
>
	    <bib entry number><register name><Enter>	- to copy it to a register
	    <bib entry number><Enter>			- to paste it to 'tex' file
	    <Enter>					- to skip the choice
<	
	When you paste the bib entry key the bib search window will close.

								*atp-bibsearch-highlight*
	The colours of the output are set by the syntax file
	'syntax/bibsearch_atp.vim'. All groups except one are the same as in
	the syntax file for bib files ('syntax/bib.vim' in your $VIMRUNTIME
	directory). Their names are 'bibsearchEntryKw' instead 'bibEntryKw'.
	The one that is differently defined 'bibsearchComment'.  Which is
	changed in that way to highlight the bib file names.  One additional
	highlight group is: 'bibsearchInfo'. It highlights the number of
	entry and its line number in the bib file. By default all bibsearch
	groups are linked to the corresponding bib group, the bibsearchInfo
	group is not set.
	
	In a colour file (~/.vim/color/*.vim) you can use these groups to set
	colours.  See |highlight| or just read a colour file. For example,
	this is a nice set of colours for dark background 
		
							 	
:BibSearch [pattern] [flag] 					*atp-BibSearch-command*
	which do what you expect. The arguments should not be quoted and
	separated by a white spaces (if you want to include a white space use
	'\ '), for more see |f-args|. If you do not provide any argument then
	all entries of all bib files will be shown. Examples:

	Some examples:						*atp-bibflags:examples*
>
 	:BibSearch 
<				this is a tip how to show all bib fields with
				the default flags
>
 	:BibSearch @ yt	
<				this is a tip how to show all bib fields with
				different flags than the default ones, which
				is equivalent to:
>
	:call BibSearch('','yt')

 	:BibSearch title[\s.]*Galois Theory  aetb
<
	The next one shows all bib fields which were written by Joyal and
	Tirney (and mayby somebody else).
>
	:BibSearch 'author.*Joyal\&.*Tirney'
<

:DefiSearch [pattern]						*atp-DefiSearch-command*
	Finds all definiitons which matches the pattern. It looks in all the
	input files (except bib files).

								*atp-bibsearch-variables*
								*atp-variables-bib*	
SOME VARIABLES:
	All the values of important variables can be shown by ShowOption
	command.

								
b:bibfiles							*atp-bibfiles*
	This variable is a list and you can put here additional bib files.
	They will be parsed by the BibSearch/BibChoose functions.

	The following variables you can see using the ShowOptions command (see
	|atp-ShowOptions()|).
s:bibfiles
	This variable is a list which stores bib files found in your tex
	files and which are readable. It is set up when you first run of the commands:
	BibSearch/BibChoose/ShowOptions. Its value is shown by the
	functions FindBibFiles({bufname}).
s:allbibfiles 
	this is a sum of found bib files the locally defined b:bibfiles, which
	not necessarily are readable.
s:notreadablebibfiles
	guess what :)

-----------------------------------------------------------------------------------
								*atp-bibsearch-comments*
	Please do not hesitate to report any bug to me:
	mszamot@gmail.com 							
	
	The algorithm will work only if all kind of bib entries of your bib
	file are included in the list g:bibentries. However, changing just
	this variable is not enough. In that case the search engine (function
	s:search) will produce correct output, but the function which displays
	found entries, will not know how to work with the new entries. One
	would have to also add an entry to the dictionary 'g:bibflagsdict'. If
	it is the case, please let me know: mszamot@gmail.com  

	As you can see entries of the type '@string' which can be used in bib
	files are not supported (i.e. there is no function which substitutes
	the variables defined in @string to their values), but it is doable.
			@string{ Name = Value }
			
------------------------------------------------------------------------------------	
	Comments: for a bib file with over 16000 lines and 1675 entries, it
	took on my computer (AMD Core 2Duo 1.9Ghz) 25.5s to find all entries,
	with the command ':BibSearch . All' and 1s to show results (the
	longest loop took over 100000 times). However to find in this database
	one entry with default flags (:BibSearch <Name>) it took 2.84s
	(exactly 79 matching bib fields), and to display them another 0.6s. 

	If for you the algorithm is too slow, there is a way to make it more
	efficient: the problem with when every line is matched: when we are in
	line x we go back to find the nearest line of the form @article{,
	@book{, etc., if we find it we go to line x+1 and do it once again. We
	could remember previous line numbers and test if we already have done
	the calculations. This can avoid  long loops.

================================================================================
How to configure to your needs                          *atp-configure*
							*atp-variables*

There are several options you can set, and they might be set in your vimrc
file. The default values are given below.

All buffer variables (see |b:var|), i.e. these which name begins with "b:",
should be set in your vimrc file. The best way to do that is by using
autocommand:
>
	au BufReadPre *.tex let b:texcompiler="latex"
<
If you put just let b:texcompiler, this will also work but not always: for
example when you open a new buffer in existing vim session.

let b:texcompiler="pdflatex" 					*atp-texcompiler*
	Used by functions: TEX() (map \l, imap \l), VTEX() (map <F5>, imap <F5>)

	You can set it to latex, tex, luatex, and so on and possibly to
	lilypond as well. 

let b:texoptions=""
	If you want to set some additional options to your texcompiler you can
	use this variable, note that, -jobname, -output-directory, -mode, are
	already used. You can use this to make reverse searching with xdvi see
	|atp-xdvi|.

								*atp-outdir*
let b:outdir=fnameescape(fnamemodify(resolve(expand("%:p")),":h")) . "/"
Used by ViewOutput(), TEX(), VTEX(), BibTeX(), TexLog(), Pdffonts(), Delete() 
			i.e. in all functions.

	This is the directory in which tex will put the output files. If the
	open file is not a symbolic link it is equal to the directory in which
	the tex file is located. If the open file is a symbolic link it points
	to the directory in which the real file is located. 
	
	If you set this variable to './' (or '.') and change the current
	working directory for example to /tmp (:cd /tmp) then the latex output
	will be placed in /tmp, i.e. it will move with with cd. However, the
	default value of b:outdir is not affected by :cd command.

	White spaces and other characters should not be escaped. It will be
	quoted in '...' using the |shellescape()| function.

	You can see the current output directory in the status (it is in the
	short notation) to see it whole type:
		:echo b:outdir
	or use the function ShowOptions() (see |apt-ShowOptions()|).		

	If in your environment the variable $TEXMFOUTDIR is set the value of
	b:outdir will be set to its value.

let b:auruns=1							*atp-auruns*
	This variable control how many times the automatic function calls tex
	compiler (consecutively). It is useful if you are working with PDF
	files and you want to have bookmarks (you can get them using hyperref
	package with the option: bookmarks. Then set b:auruns to '2'.

let g:texmf=$HOME/texmf						*atp-texmf*
	This variable configures where input files are placed. See
	|atp-EditInputFile()|.

let g:askforoutdir=0						*atp-askforoutdir*
	Its values are 1 and 0.  When it is set to 1 you will be asked for the
	name of a directory where tex will put output files, note that this
	name should end with a "/".

let b:Viewer="xpdf"						*atp-Viewer*
	it was tested with xpdf, evince, epdfviewer, kpdf, okular, xdvi and
	they all works fine. I'm using xpdf and the xpdf server options are
	supported so that the file is automatically reloaded (other viewers,
	except epdfview, have this functionality as well. This do not works
	for acroread. Read more about viewers in |atp-viewers|. 

let b:XpdfOptions=""						*atp-XpdfOptions*
	Used by function: ViewOutput() (map \v, map <F3>, imap <F3>)

	For example, if you want to have different look of one document you can
	set it to "-bg gray20". Other example:
>
 	let b:XpdfOptions="-bg Grey30 -mattecolor SlateBlue2 -papercolor White"
<
	If you want this option to work in every new open buffer, use an
	autocommand like in the following line:
>
 	au BufRead *.tex let b:XpdfOptions="-bg NavajoWhite4 -fg black -mattecolor burlywood"
<
let b:XpdfServer=fnamemodify(expand("%"),":t")			*atp-XpdfServer*	
	Used by function: ViewOutput() (map \v, map <F3>, imap <F3>)

	It is equal to the name of the source file. You do not need escape
	spaces in the name (shellescape() function is used before it is send
	to the shell).

let b:openviewer=1						*atp-openviewer*
	If the function which calles TeX compiler do not see that you are
	viewing the output file it will open it for you if b:openviewer=1.
	Otherwise, this feature is disabled.

let g:rmcommand="perltrash"					*atp-rmcommand*
	Used by function: Delete() (map <F6>d imap <F6>d)	

	If you have another 'trash can' program you can use it here, if you do
	not have it you can use "rm" (at your own risk). It is used to delete
	the files produced by (La)TeX (see |apt-Delete()|). The function
	Delete() will remove all files in the output directory (see
	|atp-outdir|), which ends with an extension defined in the list
	|atp-texextensions|. If you set:
>
	let g:rmcommand=''
<
	then the function Delete() (see |apt-Delete()|) will use the vim
	|delete()| command, and will delete only the files produced by the
	current '.tex' file. The temporary directory is cleared by rm command.

	The program 'perltrash' is in the package app-misc/perltrash (at least
	for Gentoo).

								*atp-texextensions*	
let g:texextensions=["aux", "log", "bbl", "blg", "spl", "snm", "nav", "thm", "brf", "out", "toc", "mpx", "idx", "maf", "blg", "glo", "mtc[0-9]", "mtc1[0-9]"]	
	 This list is used by the function Delete() (see |apt-Delete()|) which
	 delets all the files with the specified extension in the directory
	 b:outdir, unless g:rmcommand="" (see |atp-rmcommand|) in which case
	 Delete() deletes only the output files for the current buffer.
									
let g:keep=["log","aux","toc","bbl"]				*atp-keep*
	Files with an extension belonging to this list will be copied from
	'b:outdir' to the temporary directory with appropriate name. Then it
	will be used by (La)TeX. (log file will be copied after it is created,
	other files will be copied back and forth between 'b:outdir' and the
	temporary directory). These four elements: log,aux,toc,bbl are
	essentially minimum to work with: table of contents, pdf-bookmarks and
	bibtex. There are possibly other classes, like beamer, or packages
	like theorem (produce .thm files) which will need to configure this
	variable.

	You can change this variable by the command:
		:let g:keep+=["thm","spl"]
								
let g:printeroptions=""						*atp-printeroptions*
	You can set the printer options. These are options for the 'lpr'
	command, which will print the output file (pdf or dvi) this depends on
	the b:texcompiler that you use.

b:texcommand							*atp-texcommand*
	This variable is for debugging. It stores the last executed command to
	compile your document. This changes also when your compiler was run
	automatically.
>
		:TEX
		:echo b:texcommand
		:TEX 2
		:echo b:texcommand
<		
		
g:defaultbibflags		see |atp-bibflags:default|
g:defaultallbibflags		see |atp-bibflags:all|
b:lastbibflags			see |atp-bibflags:last|

b:bibfiles			see |atp-variables-bib|
s:bibfiles
s:allbibfiles
s:notreadablebibfiles
	For more on bib flags see |atp-bibflags|.
	
let t:toc_window_width=30
	g:toc_window_width (by default not set, if set ovverides
	t:toc_window_width)
	Configures the initial width of the window with table of contents.

let t:labels_window_width=30
	g:labels_window_width (by default not set, if set ovverides
	t:labels_window_width)
	Configures the initial width of the window with lables.

g:atp_statusline
	by default it is not set, put the line
>
	let g:atp_statusline=0
<
	in your $VIMRC file if you do not want the status line provided by this
	plugin. (See |atp-ATPStatus|).

let b:truncate_status_section=40
	This variable sets how many characters of the section/subsection title
	(or chapter/section titles if you write a book) should be shown in the
	status line.  Section title and subsection title gets equal amount of
	characters.

================================================================================
Mappings and Commands                     		*atp-mappings*

Lots of mappings which are given here uses #. This is a convenient map on
British keyboards, but not in the US layout, you can change them for '`' or
some other key that it is not used in vim (there are not many of them though).
The most commonly used latex-suite plugin uses similar set of mappings (but
there might be some differences).

These mappings are loaded unless you set one of variables: 'no_plugin_maps' or
'no_atp_maps' (disables maps defined in tex_atp.vim) or 'no_atp_toc_maps'
(disables maps defined in 'toc_atp.vim'.

Note: in all mappings "\" is set to your <LocalLeader> (and thus, in fact, the
mappings can differ).

ShowOptions						*atp-map-ShowOptions()*
:ShowOptions, :ShowOptions v

TEX()							*atp-map-TEX()*
map \l,imap \l, :TEX 

VTEX()							*atp-map-VTEX()*
map <F5>,imap <F5>, :VTEX 

ViewOutput()						*atp-map-ViewOutput()*
map \v,map <F3>, imap \v, imap <F3>  

Bibtex()						*atp-map-Bibtex()*
map \b, :Bibtex

SimpleBibtex()						*atp-map-SimpleBibtex()*
map \sb, :SBibtex

OpenLog()						*atp-map-OpenLog()*
map <F6>l, imap <F6>l

Delete()						*atp-map-Delete()*
map <F6>d

Print()							*atp-map-Print()*
map \p
	This calls the function Printer(g:printeroptions) (see |Print()|).

							*atp-map-TOC*
:TOC
map \t
	This is a mapping to the command ':TOC'

							*atp-map-Labels*
:Label
map \L
	This is a mapping to the command ':Labels'

							*atp-map-UpdateLabels()* 
:call UpdateLabels(bufname)	
map \UL			<-- is mapped to UpdateLabels(bufname("%"))

								
texdoc:							*atp-map-texdoc*
<F1> 	   
	This is both map and imap. Then you have to type what you are looking
	for and press enter. The option 'keywordprg' is set to 'texdoc -m',
	i.e when your curosr is over a package name and you press 'K' key
	then you should see the package document file (if it is named
	after the package).

	Note: if you are using: 
		au! BufWinEnter *.tex silent loadview
		au! BufWinLeave *.tex mkview
	in your $VIMRC file, you will need to change this option in all
	~/.vim/view/*.tex= files. To check if this option was set from a view
	file type 
		:verbose set keywordprg?
	Type in your terminal 
		vim ~/.vim/view/*.tex=
	then issue the command
		:bufdo %s/setlocal keywordprg=\p*/setlocal keywordprg=texdoc\ -m/g | w
		

texloganalyzer mappings:				*atp-texlog* *atp-texloganalyzer*			
<F6>+w			to see all warnings
<F6>+r			to see warnings coming from references 
<F6>+f			to see font warnings

this is not a texloganalyzer mapping but it is a good place to mention it:
<F6>+l			to open log file in another tab
			this is a mapping to a function called |OpenLog()|.

								
pdffonts is mapped to <F6>+g				*atp-pdffonts*

" FONT COMMANDS
imap ##rm \textrm{}<Left>
imap ##it \textit{}<Left>
imap ##sl \textsl{}<Left>
imap ##sf \textsf{}<Left>
imap ##bf \textbf{}<Left>
	
imap ##mit \mathit{}<Left>
imap ##mrm \mathrm{}<Left>
imap ##msf \mathsf{}<Left>
imap ##mbf \mathbf{}<Left>

							*atp-mappings-greek-letters*
" GREEK LETTERS
imap #a \alpha
imap #b \beta
imap #c \chi
imap #d \delta
imap #e \epsilon
imap #f \phi
imap #y \psi
imap #g \gamma
imap #h \eta
imap #k \kappa
imap #l \lambda
imap #i \iota
imap #m \mu
imap #n \nu
imap #p \pi
imap #o \theta
imap #r \rho
imap #s \sigma
imap #t \tau
imap #u \upsilon
imap #vs \varsigma
imap #vo \vartheta
imap #w \omega
imap #x \xi
imap #z \zeta

Not all upper Greek letters are in LaTeX:
imap #D \Delta
imap #Y \Psi
imap #F \Phi
imap #G \Gamma
imap #L \Lambda
imap #M \Mu
imap #N \Nu
imap #P \Pi
imap #O \Theta
imap #S \Sigma
imap #T \Tau
imap #U \Upsilon
imap #V \Varsigma
imap #W \Omega

								*atp-mappings-environments*
imap [b \begin{}<Left>
imap [e \end{}<Left>
inoremap [s \begin{}<CR>\end{}<Up><Right>

inoremap ]c \begin{center}<Cr>\end{center}<Esc>O
inoremap [c \begin{corollary}<Cr>\end{corollary}<Esc>O
inoremap [d \begin{definition}<Cr>\end{definition}<Esc>O
inoremap ]e \begin{enumerate}<Cr>\end{enumerate}<Esc>O
inoremap [q \begin{equation}<Cr>\end{equation}<Esc>O
inoremap [a \begin{align}<Cr>\end{align}<Esc>O
inoremap [x \begin{example}<Cr>\end{example}<Esc>O
inoremap ]q \begin{equation}<Cr>\end{equation}<Esc>O
inoremap ]l \begin{flushleft}<Cr>\end{flushleft}<Esc>O
inoremap ]r \begin{flushright}<Cr>\end{flushright}<Esc>O
inoremap [i \item  
inoremap ]i \begin{itemize}<Cr>\end{itemize}<Esc>O
inoremap [l \begin{lemma}<Cr>\end{lemma}<Esc>O
inoremap [n \begin{note}<Cr>\end{note}<Esc>O
inoremap [o \begin{observation}<Cr>\end{observation}<Esc>O
inoremap ]p \begin{proof}<Cr>\end{proof}<Esc>O
inoremap [p \begin{proposition}<Cr>\end{proposition}<Esc>O
inoremap [r \begin{remark}<Cr>\end{remark}<Esc>O
inoremap [t \begin{theorem}<Cr>\end{theorem}<Esc>O
imap 	 ]t \begin{center}<CR>\begin{tikzpicture}<CR><CR>\end{tikzpicture}<CR>\end{center}<Up><Up>

								*atp-mappings-other*
These are very useful mappings for typing mathematics:
imap __ _{}<Left>
imap ^^ ^{}<Left>
imap [m \[\]<Left><Left>

================================================================================
Error Handling							*atp-errors*
	
This plugins sets the option 
>
	set errorfile=  log file in your b:outdir directory
<
This allows you to use |quickfix| commands, for example to read the error file
use :cg (see |cg|), to jump to the first error :cf (see |cg|), to list all
errors :cl (see |cl|), read |errorformat| if you want to change the output of
this commands.

								*atp-errors-bibsearch*
A possible error which may accour using the :BibSearch commands has a simple
cause: we count number of brackets '()', '{}' and '"' (but nor '\"') to see
where the bib entry ends and where to join lines. The message error is echoed
when more than 30 lines where processed and the matching bracket was not found
(or the number of '"' is odd). Look at your bib file at the specified
position.  Syntax highlighting for bib files can help you finding where such
an error is located. (After reading the bib file comment lines and lines which
begin with @string are removed, so that the brackets in comment lines do not
count.)

================================================================================
Requirements							*atp-requirements*

This plugin requires vim version higher than 7, however a some simple changes
can make it run on older versions as well (the 'source code' uses lists
/g:keep and let g:texextensions are the only ones). 

It is nice to have 'texdoc' program. This plugin maps <F1> to :!texdoc -m ...
and allows to speed up your searches. Also the option 'keywordprg' has the
value "texdoc -m", thus pressing 'K' (see |K|) over a texpackage should open
you the package documentation. It is also a good advice to install help files
of latex-suite, you can find there a big part of 'Non so short introduction to
Latex' by Tobias Oetiker (copy them to your ~/.vim/doc document then use
|:helptags| to install the helptags :helptags ~/.vim/doc and that's it). The
same applies to this help file.

Another good programs are texloganalyzer and pdffonts wchich are not required
by there are some mappings to defined (see |texlog| and |pdffonts|).

================================================================================
Notes on Viewers                               			*atp-viewers*

xpdf								*atp-viewers-xpdf*
	It is fully supported. It is configured in the way that when your tex
	file have errors, xpdf viewer will not reload your file, which I found
	useful. 

	You can set your own options of xpdf using b:XpdfOptions, for example
>
	    let b:XpdfOptions="-bg NavajoWhite4 -fg black -mattecolor burylwood"
<
	will make xpdf view different. This is helpfull when you edit to
	files, and do not want to xpdf mix them. Another example:
>
	    let b:XpdfOptions="-bg Grey30 -mattecolor SlateBlue2 -papercolor White"
<
evince
	Works fine.
okular
kpdf
	Works fine (moves page a little bit when updates a file).
epdfview
	This viewer does not support automatic reloads when the file changes
	(but it seems that the work is in progress). You have to issue CTRL-R
	yourself when the file is changed.
acroread
	As with epdfview (with the difference that it supports automatic
	updates, but it do not works somehow)
								
xdvi							*atp-viewers-xdvi*
							*atp-viewers-xdvi-reverse/inverse-searching*
	Works fine. The file will be updated after a click (or use the xdvi
	options '-watchfile 1' see man xdvi for explanetions). You can set
	inverse/reverse searching by the command |SetXdvi|. Here I discribe
	how it is done. 
	
    (1) Reverse searching
	(i.e. position vim's cursor after xdvi event:
	usually CTRL+Left Mouse) with this options:
>
	let b:texcompiler="latex"
	let b:texoptions="-src-specials"
	let b:Viewer="xdvi -editor 'gvim --remote-wait +%l %f'"
<
	See vim tip at: http://vim.wikia.com/wiki/Vim_can_interact_with_xdvi 	

    (2) Inverse searching
	For inverse searching (position xdvi according to the vims cursor
	position) you can set:
>
	let b:inverse_search="xdvi -sourceposition " . line(".") . ":" . col(".") . expand("%") . " " . fnamemodify(expand("%"),":r") . ".dvi"
<
	To make an inverse search:
>
		:call system(b:inverse_search)
<
	And xdvi will place itself at the current cursor position in the 'tex'
	source file. You can make a map for this. 

	To use this with vim one have to add servername. Run
	vim as:
>
 	vim --servername vimtex
 	let b:Viewer="xdvi -editor 'vim --servername " . v:servername . " --remote-wait +%l %f'"
 	let b:inverse_search="xdvi -editdor 'vim --servername " . v:servername "' -sourceposition " . line(".") . ":" . col(".") . expand("%") . " " . fnamemodify(expand("%"),":r") . ".dvi"
<

================================================================================
Color highliting and syntax groups				*atp-color-highlight*

There is a color scheme included: coots-beauty-256 you need 256 colors to use
it. These are the highlights groups defined for various files:

1) ToC file
    highlight atp_linenumber
    highlight atp_number
    highlight atp_chapter
    highlight atp_section
    highlight atp_subsection
    highlight atp_abstract
	*this group highlights abstrac, all the unnubered chapters and
	bibliography.

    The chapter group highlights or chapters, or sections, or parts, depending
    what is your top level section in your latex document. This applies,
    accordingly, to other groups.

2) Labels file
    highlight atp_label_filename
    highlight atp_label_linenr
    highlight atp_label_name 

3) BibSearch file
    this is very much the same as the standard syntax for bib files. Groups
    are named bibsearch<NAME> instead of bib<NAME>. There is one more group
    added:
>
	    bibsearchInfo
<
    which highlights the line number of the bib entry in the bib file.  All
    bibsearch groups are by default linked to the bib groups.

    Yet, there is no default highlighting, try coots-beauty-256 color scheme.
    If you like it, I'm glad, if you have a nice (non standard) color scheme,
    I'm happy to get it, if you like to share it.

================================================================================
Final Remarks                               			*atp-remarks*
	
	To see some messages that are issued you can use :messages command
	(see |:mes|).

	If you find this plugin useful and have some comments you are
	cordially invited to write to the author: <mszamot@gmail.com>.

	There are other ways to make such an automatic plugin. The crucial
	step is to make the plugin know that tex is already in use (if you run
	to tex compilers on the same file at the same time the effect won't be
	good).

	For several month I was using a different code which was not using
	temporary copies but was checking if the tex is running or not. It was
	working very good but I didn't understand why (silly isn't it), and
	when I started making changes it stopped working: the issue is that
	it is difficult to make a function sleep until tex stops working not
	putting whole vim into sleep - this is the time that we want to save.
	However, the advantage of using temporary files is smoothness (changes
	appear faster in the output file). 

	Best regards, and hopefully you will find this useful :) 
	Marcin Szamotulski
	
	
================================================================================
Copy Rights							*atp-copy-rights*


" Copyright:    Copyright (C) 2010 Marcin Szamotulski Permission is hereby
"		granted to use and distribute this code, with or without
"		modifications, provided that this copyright notice is copied
"		with it. Like anything else that's free, Automatic TeX Plugin
"		is provided *as is* and comes with no warranty of any kind,
"		either expressed or implied. By using this plugin, you agree
"		that in no event will the copyright holder be liable for any
"		damages resulting from the use of this software.


vim:tw=78:ts=8:ft=help:norl:
doc/latexhelp.txt	[[[1
2430
*latexhelp.txt*    For Vim version 6.0.  Last change: 2001 Dec 20


				LATEX HELP 1.6  
		   translated (with minor changes) for vim
			     by Mikolaj Machowski

This file documents LaTeX2e, a document preparation system. LaTeX2e is a
macro package for TeX.

  This is edition 1.6 of the LaTeX2e documentation, and is for the Texinfo
that is distributed as part of Version 19 of GNU Emacs. It uses version
2.134 or later of the texinfo.tex input file.

  This is translated from LATEX.HLP v1.0a in the VMS Help Library.  The
pre-translation version was written by George D. Greenwade of Sam Houston
State University.

  The LaTeX 2.09 version was written by Stephen Gilmore <stg@dcs.ed.ac.uk>.

  The LaTeX2e version was adapted from this by Torsten Martinsen
<bullestock@dk-online.dk>.

  Version for vim of this manual was written by Mikolaj Machowski
<mikmach@wp.pl>

  Copyright 1988,1994 Free Software Foundation, Inc.  Copyright 1994-1996
Torsten Martinsen. Copyright for `translation' for vim Mikolaj Machowski 2001.

  Permission is granted to make and distribute verbatim copies of this manual
provided the copyright notice and this permission notice are preserved on
all copies.

  Permission is granted to copy and distribute modified versions of this
manual under the conditions for verbatim copying, provided that the entire
resulting derived work is distributed under the terms of a permission
notice identical to this one.

  Permission is granted to copy and distribute translations of this manual
into another language, under the above conditions for modified versions,
except that the sections entitled "Distribution" and "General Public
License" may be included in a translation approved by the author instead of
in the original English.

==============================================================================
*LaTeX* *latex*

The LaTeX command typesets a file of text using the TeX program and the LaTeX
Macro package for TeX. To be more specific, it processes an input file
containing the text of a document with interspersed commands that describe how
the text should be formatted.

1.  Commands					|latex-commands|
2.  Counters					|latex-counters|
3.  Cross References				|latex-references|
4.  Definitions					|latex-definitions|
5.  Document Classes				|latex-classes|
6.  Layout					|latex-layout|
7.  Environments				|latex-environments|
8.  Footnotes					|latex-footnotes|
9.  Lengths					|latex-lengths|
10. Letters					|latex-letters|
11. Line & Page Breaking			|latex-breaking|
12. Making Paragraphs				|latex-paragraphs|
13. Margin Notes				|latex-margin-notes|
14. Math Formulae				|latex-math|
15. Modes					|latex-modes|
16. Page Styles					|latex-page-styles|
17. Sectioning					|latex-sectioning|
18. Spaces & Boxes				|latex-spaces-boxes|
19. Special Characters				|latex-special-char|
20. Splitting the Input				|latex-inputting|
21. Starting & Ending				|latex-start-end|
22. Table of Contents				|latex-toc|
23. Terminal Input/Output			|latex-terminal|
24. Typefaces					|latex-typefaces|
25. Parameters					|latex-parameters|

==============================================================================
1. Commands					*latex-commands*

A LaTeX command begins with the command name, which consists of a \ followed
by either
	(a) a string of letters or
	(b) a single non-letter.

Arguments contained in square brackets, [], are optional while arguments
contained in braces, {}, are required.

NOTE: LaTeX is case sensitive. Enter all commands in lower case unless
explicitly directed to do otherwise.

==============================================================================
2. Counters					*latex-counters*

|\addtocounter|		Add a quantity to a counter
|\alph|			Print value of a counter using letters
|\arabic|		Print value of a counter using numerals
|\fnsymbol|		Print value of a counter using symbols
|\newcounter|		Define a new counter
|\refstepcounter|	Add to counter, resetting subsidiary counters
|\roman|		Print value of a counter using roman numerals
|\setcounter|		Set the value of a counter
|\stepcounter|		Add to counter, resetting subsidiary counters
|\usecounter|		Use a specified counter in a list environment
|\value|		Use the value of a counter in an expression

Everything LaTeX numbers for you has a counter associated with it. The name of
the counter is the same as the name of the environment or command that
produces the number, except with no |\\|. (|lc-enumi| - |lc-enumiv| are used
for the nested |\enumerate| environment.) Below is a list of the counters
used in LaTeX's standard document classes to control numbering.

 |part|          |paragraph|     |figure|      |enumi|    |itemi|
 |chapter|       |subparagraph|  |table|       |enumii|   |itemii|
 |section|       |page|          |footnote|    |enumiii|  |itemiii|
 |subsection|    |equation|      |mpfootnote|  |enumiv|   |itemiv|
 |subsubsection|


\addtocounter{counter}{value}			*\addtocounter*
		Increments the {counter} by the amount specified by the
		{value} argument. The {value} argument can be negative.

\alph{counter}					*\alph* *\Alph*
\Alph{counter}
		This command causes the value of the counter to be printed in
		alphabetic characters. |\alph| command uses lower case
		alphabetic alphabetic characters, i.e., a, b, c... while the
		|\Alph| command uses upper case alphabetic characters, i.e.,
		A, B, C....

\arabic{counter} 				*\arabic*
		Causes the value of the {counter} to be printed in Arabic
		numbers, i.e., 3.

\fnsymbol{counter} 				*\fnsymbol*
		Causes the value of the {counter} to be printed in a specific
		sequence of nine symbols that can be used for numbering
		footnotes.
		Note: counter must have a value between 1 and 9 inclusive.

\newcounter{foo}[counter] 			*\newcounter*
		Defines a new counter named {foo}. The counter is initialized
		to zero.  The optional argument [counter] causes the counter
		{foo} to be reset whenever the counter named in the optional
		argument is incremented.

\refstepcounter{counter}			*\refstepcounter*
		Command works like |\stepcounter|, except it also defines the
		current |\ref| value to be the result of \thecounter.

\roman{counter} 				*\roman* *\Roman*
\Roman{counter}
		Causes the value of the {counter} to be printed in Roman
		numerals.  The |\roman| command uses lower case Roman numerals,
		i.e., i, ii, iii..., while the |\Roman| command uses upper case
		Roman numerals, i.e., I, II, III....

\stepcounter{counter}				*\stepcounter*
		Adds one to the {counter} and resets all subsidiary counters.

\setcounter{counter}{value}			*\setcounter*
		Sets the value of the {counter} to that specified by the
		{value} argument.

\usecounter{counter} 				*\usecounter*
		Command is used in the second argument of the |list|
		environment to allow the {counter} specified to be used to
		number the list items.

\value{counter} 				*\value*
		Produces the value of the {counter} named in the mandatory
		argument. It can be used where LaTeX expects an integer or
		number, such as the second argument of a |\setcounter| or
		|\addtocounter| command, or in: >
			\hspace{\value{foo}\parindent}
<		It is useful for doing arithmetic with counters.

==============================================================================
3. Cross References				*latex-references*

One reason for numbering things like figures and equations is to refer the
reader to them, as in "See Figure 3 for more details."

|\label|		Assign a symbolic name to a piece of text
|\pageref|		Refer to a page number
|\ref|			Refer to a section, figure or similar


\label{key} 					*\label*
		Command appearing in ordinary text assigns to the {key} the
		number of the current sectional unit; one appearing inside a
		numbered environment assigns that number to the {key}.

		A {key} can consist of any sequence of letters, digits, or
		punctuation characters. Upper and lowercase letters are
		different.

		To avoid accidentally creating two labels with the same name,
		it is common to use labels consisting of a prefix and a suffix
		separated by a colon. The prefixes conventionally used are
			* 'cha' for chapters
			* 'sec' for lower-level sectioning commands
			* 'fig' for figures
			* 'tab' for tables
			* 'eq'  for equations
		Thus, a label for a figure would look like: >
			\label{fig:bandersnatch}

\pageref{key} 					*\pageref*
		Command produces the page number of the place in the text
		where the corresponding |\label| command appears.  ie. where
		\label{key} appears.

\ref{key}					*\ref*
		Command produces the number of the sectional unit, equation
		number, ... of the corresponding |\label| command.

==============================================================================
4. Definitions					*latex-definitions*

|\newcommand| 		Define a new command
|\newenvironment| 	Define a new environment
|\newtheorem| 		Define a new theorem-like environment
|\newfont| 		Define a new font name


\newcommand{cmd}[args]{definition}		*\newcommand* *\renewcommand*
\newcommand{cmd}[args][default]{definition}
\renewcommand{cmd}[args]{definition}
\renewcommand{cmd}[args][default]{definition}

These commands define (or redefine) a command.

{cmd}		A command name beginning with a |\\|. For |\newcommand| it must
		not be already defined and must not begin with |\end|; for
		|\renewcommand| it must already be defined.

{args}		An integer from 1 to 9 denoting the number of arguments of the
		command being defined. The default is for the command to have
		no arguments.

{default}	If this optional parameter is present, it means that the
		command's first argument is optional. The default value of the
		optional argument is default.

{definition}	The text to be substituted for every occurrence of {cmd}; a
		parameter of the form #n in {cmd} is replaced by the text of
		the nth argument when this substitution takes place.

       					*\newenvironment* *\renewenvironment*
\newenvironment{nam}[args]{begdef}{enddef}
\newenvironment{nam}[args][default]{begdef}{enddef}
\renewenvironment{nam}[args]{begdef}{enddef}

These commands define or redefine an environment.

{nam} 		The name of the environment. For |\newenvironment| there must
		be no currently defined environment by that name, and the
		command \nam must be undefined.  For |\renewenvironment| the
		environment must already be defined.

{args}		An integer from 1 to 9 denoting the number of arguments of
		the newly-defined environment. The default is no arguments.

{default} 	If this is specified, the first argument is optional, and
		default gives the default value for that argument.

{begdef} 	The text substituted for every occurrence of \begin{nam}; a
		parameter of the form #n in {cmd} is replaced by the text of
		the nth argument when this substitution takes place.

{enddef} 	The text substituted for every occurrence of \end{nam}. It
		may not contain any argument parameters.


\newtheorem{envname}{caption}[within]			*\newtheorem*
\newtheorem{envname}[numberedlike]{caption}

This command defines a theorem-like environment.

{envname}	The name of the environment to be defined. A string of
		letters. It must not be the name of an existing environment or
		counter.

{caption}	The text printed at the beginning of the environment, right
		before the number. This may simply say "Theorem", for example.

{within}	The name of an already defined counter, usually of a sectional
		unit. Provides a means of resetting the new theorem counter
		within the sectional unit.

{numberedlike}	The name of an already defined theorem-like environment.

The |\newtheorem| command may have at most one optional argument.


\newfont{cmd}{fontname} 				*\newfont*
		Defines the command name {cmd}, which must not be currently
		defined, to be a declaration that selects the font named
		{fontname} to be the current font.

==============================================================================
5. Document Classes				*latex-classes*


\documentclass[options]{class}			*\documentclass*

Valid LaTeX document classes include:
	*article		*article-class*
	*report			*report-class*
	*letter			*letter-class*
	*book			*book-class*
	*slides			*slides-class*

All the standard classes (except slides) accept the following options for
selecting the typeface size (10 pt is default):

10pt, 11pt, 12pt

All classes accept these options for selecting the paper size (default is
letter):

a4paper, a5paper, b5paper, letterpaper, legalpaper, executivepaper

Miscellaneous options:

landscape 					*landscape*
	Selects landscape format. Default is portrait.

titlepage, notitlepage				*notitlepage*
		Selects if there should be a separate title page.

leqno						*leqno* *rqno*
		Equation number on left side of equations.  Default is
		right side.

fleqn						*fleqn*
		Displayed formulas flush left.  Default is centred.

openbib						*openbib*
		Use "open" bibliography format.

draft, final					*draft* *final*
		Mark/do not mark overfull boxes with a rule. Default is
		final.

These options are not available with the slides class:

oneside, twoside				*oneside* *twoside*
		Selects one- or twosided layout. Default is oneside,
		except for the book class.

openright, openany				*openright* *openany*
		Determines if a chapter should start on a right-hand page.
		Default is openright for book.

onecolumn, twocolumn				*onecolumn* *twocolumn*
		One or two columns.  Defaults to one column.

The slides class offers the option clock for printing the time at the bottom
of each |\note|.

If you specify more than one option, they must be separated by a comma.

\usepackage[options]{pkg} 			*\usepackage*
		Additional packages are loaded by this. If you
		specify more than one package, they must be separated by a
		comma.

Any options given in the |\documentclass| command that are unknown by the
selected document class are passed on to the packages loaded with |\usepackage|.

==============================================================================
6. Layout					*latex-layout*

Miscellaneous commands for controlling the general layout of the page.

|\flushbottom|		Make all text pages the same height.
|\onecolumn| 		Use one-column layout.
|\raggedbottom| 	Allow text pages of differing height.
|\twocolumn| 		Use two-column layout.

\flushbottom					*\flushbottom*
		Makes all text pages the same height, adding extra vertical
		space when necessary to fill out the page.  This is the
		standard if twocolumn mode is selected.

\onecolumn					*\onecolumn*
		Starts a new page and produces single-column output.

\raggedbottom					*\raggedbottom*
		Makes all pages the height of the text on that page.  No extra
		vertical space is added.

\twocolumn[text]				*\twocolumn*
		Starts a new page and produces two-column output.  If the
		optional [text] argument is present, it is typeset in
		one-column mode.

==============================================================================
7. Environments					*latex-environments*

						*\begin* *\end*
LaTeX provides a number of different paragraph-making environments. Each
environment begins and ends in the same manner: >

	\begin{environment-name}
	.
	.
	.
	\end{environment-name}
<
a. |array| 		Math arrays
b. |center| 		Centred lines
c. |description| 	Labelled lists
d. |enumerate|		Numbered lists
e. |eqnarray| 		Sequences of aligned equations
f. |equation| 		Displayed equation
g. |figure| 		Floating figures
h. |flushleft| 		Flushed left lines
i. |flushright| 	Flushed right lines
j. |itemize| 		Bulleted lists
k. |letter| 		Letters
l. |list| 		Generic list environment
m. |minipage| 		Miniature page
n. |picture| 		Picture with text, arrows, lines and circles
o. |quotation| 		Indented environment with paragraph indentation
p. |quote-l| 		Indented environment with no paragraph indentation
q. |tabbing| 		Align text arbitrarily
r. |table| 		Floating tables
s. |tabular| 		Align text in columns
t. |thebibliography| 	Bibliography or reference list
u. |theorem| 		Theorems, lemmas, etc
v. |titlepage| 		For hand crafted title pages
x. |verbatim| 		Simulating typed input
y. |verse| 		For poetry and other things

==============================================================================
 a. array					*array*
>
	\begin{array}{col1col2...coln}
		column 1 entry & column 2 entry ... & column n entry \\
		.
		.
		.
	\end{array}

Math arrays are produced with the |array| environment. It has a single mandatory
argument describing the number of columns and the alignment within them. Each
column, coln, is specified by a single letter that tells how items in that row
should be formatted.
	* c -- for centred
	* l -- for flush left
	* r -- for flush right
Column entries must be separated by an |&|. Column entries may include other
LaTeX commands. Each row of the array must be terminated with the string |\\|.

Note that the |array| environment can only be used in |math-mode|, so normally
it is used inside an |equation| environment.

==============================================================================
b. center					*center*
>
	\begin{center}
		Text on line 1 \\
		Text on line 2 \\
		.
		.
		.
	\end{center}

The |\center| environment allows you to create a paragraph consisting of lines
that are centred within the left and right margins on the current page. Each
line must be terminated with the string |\\|.

\centering					*\centering*
		This declaration corresponds to the |center| environment. This
		declaration can be used inside an environment such as
		|quote-l| or in a |\parbox|. The text of a |figure| or |table|
		can be centred on the page by putting a |\centering| command
		at the beginning of the |figure| or |table| environment.
		Unlike the |center| environment, the |\centering| command does
		not start a new paragraph; it simply changes how LaTeX formats
		paragraph units. To affect a paragraph unit's format, the
		scope of the declaration must contain the blank line or |\end|
		command (of an environment like |quote-l|) that ends the
		paragraph unit.

==============================================================================
c. description					*description*
>
	\begin{description}
		\item [label] First item
		\item [label] Second item
		.
		.
		.
	\end{description}

The |description| environment is used to make labelled lists. The label is
bold face and flushed right.

==============================================================================
d. enumerate					*enumerate*
>
	\begin{enumerate}
		\item First item
		\item Second item
		.
		.
		.
	\end{enumerate}

The |enumerate| environment produces a numbered list.  Enumerations can be
nested within one another, up to four levels deep.  They can also be nested
within other paragraph-making environments.

\item		Each item of an enumerated list begins with an |\item|
		command. There must be at least one |\item| command
		within the environment.

The |enumerate| environment uses the |\enumi| through |\enumiv| counters (see
section |latex-counters|). The type of numbering can be changed by redefining
\theenumi etc.

==============================================================================
e. eqnarray					*eqnarray*
>
	\begin{eqnarray}
		math formula 1 \\
		math formula 2 \\
		.
		.
		.
	\end{eqnarray}

The |eqnarray| environment is used to display a sequence of equations or
inequalities. It is very much like a three-column |array| environment, with
consecutive rows separated by |\\| and consecutive items within a row separated
by an |&|.

\nonumber					*\nonumber*
		An equation number is placed on every line unless that
		line has a |\nonumber| command.

\lefteqn					*\lefteqn*
		The command |\lefteqn| is used for splitting long
		formulas across lines.  It typesets its argument in
		display style flush left in a box of zero width.

==============================================================================
f. equation	 				*equation*
>
	\begin{equation}
		math formula
	\end{equation}

The |equation| environment centres your equation on the page and places the
equation number in the right margin.

==============================================================================
g. figure					*figure*
>
	\begin{figure}[placement]
		body of the figure
		\caption{figure title}
	\end{figure}

Figures are objects that are not part of the normal text, and are usually
"floated" to a convenient place, like the top of a page. Figures will not be
split between two pages.

The optional argument [placement] determines where LaTeX will try to place
your figure. There are four places where LaTeX can possibly put a float:

h (Here)		at the position in the text where the figure
			environment appears.
t (Top)			at the top of a text page.
b (Bottom)		at the bottom of a text page.
p (Page of floats)	on a separate float page, which is a page containing
			no text, only floats.

The standard |report-class| and |article-class| use the default placement
[tbp].

The body of the |figure| is made up of whatever text, LaTeX commands, etc.  you
wish.

The \caption command allows you to title your figure.

==============================================================================
h. flushleft					*flushleft*
>
	\begin{flushleft}
		Text on line 1 \\
		Text on line 2 \\
		.
		.
		.
	\end{flushleft}

The |flushleft| environment allows you to create a paragraph consisting of
lines that are flushed left, to the left-hand margin. Each line must be
terminated with the string |\\|.

\raggedright					*\raggedright*
		This declaration corresponds to the |flushleft| environment.
		This declaration can be used inside an environment such as
		|quote-l| or in a |\parbox|.  Unlike the |flushleft|
		environment, the |\raggedright| command does not start a new
		paragraph; it simply changes how LaTeX formats paragraph
		units. To affect a paragraph unit's format, the scope of the
		declaration must contain the blank line or |\end| command (of
		an environment like |quote-l|) that ends the paragraph unit.

==============================================================================
i. flushright					*flushright*
>
	\begin{flushright}
		Text on line 1 \\
		Text on line 2 \\
		.
		.
		.
 	\end{flushright}

The |flushright| environment allows you to create a paragraph consisting of
lines that are flushed right, to the right-hand margin. Each line must be
terminated with the string |\\|.

\raggedleft					*\raggedleft*
		This declaration corresponds to the |flushright| environment.
		This declaration can be used inside an environment such as
		|quote-l| or in a |\parbox|.  Unlike the |flushright|
		environment, the |\raggedleft| command does not start a new
		paragraph; it simply changes how LaTeX formats paragraph
		units. To affect a paragraph unit's format, the scope of the
		declaration must contain the blank line or |\end| command (of
		an environment like |quote-l|) that ends the paragraph unit.

==============================================================================
j. itemize					*itemize*
>
	\begin{itemize}
		\item First item
		\item Second item
		.
		.
		.
	\end{itemize}

The |itemize| environment produces a "bulleted" list.  Itemizations can be
nested within one another, up to four levels deep.  They can also be nested
within other paragraph-making environments.

\item						*\item*
		Each item of an itemized list begins with an |\item| command.
		There must be at least one |\item| command within the
		environment.

The itemize environment uses the |\itemi| through |\itemiv| counters (see
section |latex-counters|). The type of numbering can be changed by redefining
\theitemi etc.

==============================================================================
k. letter					*\letter*

This environment is used for creating letters. See section |latex-letters|.

==============================================================================
l. list						*list*

The |list| environment is a generic environment which is used for defining many
of the more specific environments. It is seldom used in documents, but often
in macros.
>
	\begin{list}{label}{spacing}
		\item First item
		\item Second item
		.
		.
		.
	\end{list}

'label'		The {label} argument specifies how items should be labelled.
		This argument is a piece of text that is inserted in a box to
		form the {label}.  This argument can and usually does contain
		other LaTeX commands.

'spacing'	The {spacing} argument contains commands to change the spacing
		parameters for the |list|. This argument will most often be
		null, i.e., {}. This will select all default spacing which
		should suffice for most cases.

==============================================================================
m. minipage					*minipage*
>
	\begin{minipage}[position]{width}
		text
	\end{minipage}

The |minipage| environment is similar to a |\parbox| command. It takes the
same optional [position] argument and mandatory {width} argument. You may use
other paragraph-making environments inside a |minipage|.  Footnotes in a
minipage environment are handled in a way that is particularly useful for
putting footnotes in figures or tables. A |\footnote| or |\footnotetext|
command puts the footnote at the bottom of the minipage instead of at the
bottom of the page, and it uses the |\mpfootnote| counter instead of the
ordinary footnote counter. See sections |latex-counters| and
|latex-footnotes|.

NOTE: Don't put one |minipage| inside another if you are using footnotes; they
may wind up at the bottom of the wrong minipage.

==============================================================================
n. picture					*picture*
>
		 	   size		  position
	\begin{picture}(width,height)(x offset,y offset)
		.
		.
		picture commands
		.
		.
	\end{picture}

The |picture| environment allows you to create just about any kind of picture
you want containing text, lines, arrows and circles. You tell LaTeX where to
put things in the picture by specifying their coordinates. A coordinate is a
number that may have a decimal point and a minus sign -- a number like 5, 2.3
or -3.1416. A coordinate specifies a length in multiples of the unit length
|\unitlength|, so if |\unitlength| has been set to 1cm, then the coordinate
2.54 specifies a length of 2.54 centimetres. You can change the value of
|\unitlength| anywhere you want, using the |\setlength| command, but strange
things will happen if you try changing it inside the |picture| environment.

A position is a pair of coordinates, such as (2.4,-5), specifying the point
with x-coordinate 2.4 and y-coordinate -5. Coordinates are specified in the
usual way with respect to an origin, which is normally at the lower-left
corner of the |picture|.
Note that when a position appears as an argument, it is not enclosed in
braces; the parentheses serve to delimit the argument.

The |picture| environment has one mandatory argument, which is a position.  It
specifies the size of the picture. The environment produces a rectangular box
with width and height determined by this argument's x- and y-coordinates.

The |picture| environment also has an optional position argument, following
the size argument, that can change the origin. (Unlike ordinary optional
arguments, this argument is not contained in square brackets.) The optional
argument gives the coordinates of the point at the lower-left corner of the
picture (thereby determining the origin).  For example, if |\unitlength| has
been set to 1mm, the command: >
	\begin{picture}(100,200)(10,20)
>
produces a picture of width 100 millimetres and height 200 millimetres, whose
lower-left corner is the point (10,20) and whose upper-right corner is
therefore the point (110,220). When you first draw a picture, you will omit
the optional argument, leaving the origin at the lower-left corner. If you
then want to modify your picture by shifting everything, you just add the
appropriate optional argument.

The environment's mandatory argument determines the nominal size of the
picture. This need bear no relation to how large the picture really is; LaTeX
will happily allow you to put things outside the picture, or even off the
page. The picture's nominal size is used by LaTeX in determining how much room
to leave for it.

Everything that appears in a picture is drawn by the |\put| command. The
command: >
	\put (11.3,-.3){...}

puts the object specified by ... in the picture, with its
reference point at coordinates (11.3,-.3). The reference points for various
objects will be described below.

The |\put| creates an LR box (|lrbox|). You can put anything in the text
argument of the |\put| that you'd put into the argument of an |\mbox| and
related commands. When you do this, the reference point will be the lower left
corner of the box.

Picture commands:
|\circle|		Draw a circle
|\dashbox|		Draw a dashed box
|\frame|		Draw a frame around an object
|\framebox(picture)|	Draw a box with a frame around it
|\line|			Draw a straight line
|\linethickness|	Set the line thickness
|\makebox(picture)|	Draw a box of the specified size
|\multiput|		Draw multiple instances of an object
|\oval|			Draw an ellipse
|\put|			Place an object at a specified place
|\shortstack|		Make a pile of objects
|\vector|		Draw a line with an arrow

\circle[*]{diameter}				*\circle*
		Command produces a circle with a {diameter} as close to the
		specified one as possible. If the *-form of the command is
		used, LaTeX draws a solid circle.
		Note: only circles up to 40 pt can be drawn.


\dashbox{dashlength}(width,height){...} 	*\dashbox*
		Draws a box with a dashed line.  The |\dashbox| has an extra
		argument which specifies the width of each dash.  A dashed box
		looks best when the width and height are multiples of the
		{dashlength}.

\frame{...} 					*\frame*
		Puts a rectangular frame around the object specified in the
		argument. The reference point is the bottom left corner of the
		frame. No extra space is put between the frame and the object.

\framebox(width,height)[position]{...}		*\picture-framebox*
		The |\framebox| command is exactly the same as the
		|picture-makebox| command, except that it puts a frame around
		the outside of the box that it creates.  The |\framebox|
		command produces a rule of thickness |\fboxrule|, and leaves a
		space |\fboxsep| between the rule and the contents of the box.

\line(x slope,y slope){length} 			*\line*
		Draws a line of the specified length and slope.
		Note: LaTeX can only draw lines with slope = x/y, where x and
		y have integer values from -6 through 6.

\linethickness{dimension}			*\linethickness*
		Declares the thickness of horizontal and vertical lines in a
		|picture| environment to be dimension, which must be a
		positive length. It does not affect the thickness of slanted
		lines (|\line|) and circles (|circle|), or the quarter circles
		drawn by |\oval| to form the corners of an oval.

\makebox(width,height)[position]{...} 		*picture-makebox*
		The makebox command for the |picture| environment is similar
		to the normal |\makebox| command except that you must specify
		a width and height in multiples of |\unitlength|.
		The optional argument, [position], specifies the quadrant that
		your text appears in. You may select up to two of the
		following:
			t - Moves the item to the top of the rectangle
			b - Moves the item to the bottom
			l - Moves the item to the left
			r - Moves the item to the right

						*\multiput*
\multiput(x coord,y coord)(delta x,delta y){no of copies}{object}
		This command can be used when you are putting the same
		object in a regular pattern across a picture.

\oval(width,height)[portion] 			*\oval*
		Produces a rectangle with rounded corners. The optional
		argument, [portion], allows you to select part of the oval.
			t - top portion
			b - bottom portion
			r - right portion
			l - left portion
		Note: the "corners" of the oval are made with quarter circles
		with a maximum radius of 20 pt, so large "ovals" will look
		more like boxes with rounded corners.

\put(x coord,y coord){ ... } 			*\put*
		Places the item specified by the mandatory argument at the
		given coordinates.

\shortstack[position]{... \\ ... \\ ...} 	*\shortstack*
		The |\shortstack| command produces a stack of objects.
		The valid positions are:
			r - right of the stack
			l - left of the stack
			c - centre of the stack (default)

\vector(x slope,y slope){length} 		*\vector*
		Draws a line with an arrow of the specified length and slope.
		The x and y values must lie between -4 and +4, inclusive.

==============================================================================
o. quotation					*quotation*
 >
	\begin{quotation}
		text
	\end{quotation}

The margins of the |quotation| environment are indented on the left and the
right. The text is justified at both margins and there is paragraph
indentation. Leaving a blank line between text produces a new paragraph.

==============================================================================
p. quote					*quote-l*
>
	\begin{quote}
		text
	\end{quote}

The margins of the |quote-l| environment are indented on the left and the right.
The text is justified at both margins.  Leaving a blank line between text
produces a new paragraph.

==============================================================================
q. tabbing					*tabbing*
>
	\begin{tabbing}
	text \= more text \= still more text \= last text \\
	second row \>  \> more \\
	.
	.
	.
	\end{tabbing}

The |tabbing| environment provides a way to align text in columns. It works by
setting tab stops and tabbing to them much the way you do with an ordinary
typewriter.

It is best suited for cases where the width of each column is constant and
known in advance.

This environment can be broken across pages, unlike the |tabular| environment.
The following commands can be used inside a tabbing environment:

				*tab=*
\= 		Sets a tab stop at the current position.

						*tab>*
\> 		Advances to the next tab stop.

						*tab<*
\< 		This command allows you to put something to the left of the
		local margin without changing the margin. Can only be used at
		the start of the line.

						*tab+*
\+ 		Moves the left margin of the next and all the following
		commands one tab stop to the right.

						*tab-*
\- 		Moves the left margin of the next and all the following
		commands one tab stop to the left.

						*tab'*
\' 		Moves everything that you have typed so far in the current
		column, i.e.  everything from the most recent \> (|tab>|), \<
		(|tab<|), \' (|tab'|), |\\|, or |\kill| command, to the right
		of the previous column, flush against the current column's tab
		stop.

						*tab`*
\`		Allows you to put text flush right against any tab stop,
		including tab stop 0. However, it can't move text to the right
		of the last column because there's no tab stop there. The \`
		(|tab`|) command moves all the text that follows it, up to the
		|\\| or \end{tabbing} command that ends the line, to the right
		margin of the tabbing environment. There must be no \>
		(|tab>|) or \' (|tab'|) command between the \` (|tab`|) and
		the command that ends the line.

						*\kill*
\kill 		Sets tab stops without producing text. Works just like |\\|
		except that it throws away the current line instead of
		producing output for it. The effect of any \= (|tab=|), \+
		(|tab+|) or \- (|tab-|) commands in that line remain in
		effect.

						*\pushtabs*
\pushtabs	Saves all current tab stop positions. Useful for temporarily
		changing tab stop positions in the middle of a tabbing
		environment. Also restores the tab stop positions saved by the
		last |\pushtabs|.

						*taba*
\a		In a tabbing environment, the commands \= (|tab=|), \'
		(|tab'|) and \` (|tab`|) do not produce accents as normal.
		Instead, the commands \a=, \a' and \a` are used.

This example typesets a Pascal function in a traditional format:
>
        \begin{tabbing}
        function \= fact(n : integer) : integer;\\
                 \> begin \= \+ \\
                       \> if \= n $>$ 1 then \+ \\
                                fact := n * fact(n-1) \- \\
                          else \+ \\
                                fact := 1; \-\- \\
                    end;\\
        \end{tabbing}

==============================================================================
r. table					*\table*
>
	\begin{table}[placement]
		body of the table
		\caption{table title}
	\end{table}

Tables are objects that are not part of the normal text, and are usually
"floated" to a convenient place, like the top of a page. Tables will not be
split between two pages.

The optional argument [placement] determines where LaTeX will try to place
your table. There are four places where LaTeX can possibly put a float:

	h (Here)		at the position in the text where the table
				environment appears.
	t (Top)			at the top of a text page.
	b (Bottom)		at the bottom of a text page.
	p (Page of floats)	on a separate float page, which is a page
				containing no text, only floats.

The standard |report-class| and |article-class| use the default placement [tbp].

The body of the table is made up of whatever text, LaTeX commands, etc., you
wish.

The \caption command allows you to title your table.

==============================================================================
s. tabular					*tabular*
>
	\begin{tabular}[pos]{cols}
		column 1 entry & column 2 entry ... & column n entry \\
		.
		.
		.
	\end{tabular}

or
>
	\begin{tabular*}{width}[pos]{cols}
		column 1 entry & column 2 entry ... & column n entry \\
		.
		.
		.
	\end{tabular*}

These environments produce a box consisting of a sequence of rows of items,
aligned vertically in columns. The mandatory and optional arguments consist
of:

{width}	Specifies the width of the tabular* environment. There must be
	rubber space between columns that can stretch to fill out the
	specified width.

[pos]	Specifies the vertical position; default is alignment on the
	centre of the environment.
		t - align on top row
		b - align on bottom row

{cols}	Specifies the column formatting. It consists of a sequence of
	the following specifiers, corresponding to the sequence of
	columns and intercolumn material.
		l - A column of left-aligned items.

		r - A column of right-aligned items.

		c - A column of centred items.

		| - A vertical line the full height and depth of the
		environment.

		@{text} - This inserts text in every row. An @-expression
		suppresses the intercolumn space normally inserted
		between columns; any desired space between the
		inserted text and the adjacent items must be included
		in text. An \extracolsep{wd} command in an
		@-expression causes an extra space of width {wd} to
		appear to the left of all subsequent columns, until
		countermanded by another |\extracolsep| command. Unlike
		ordinary intercolumn space, this extra space is not
		suppressed by an @-expression. An |\extracolsep|
		command can be used only in an @-expression in the
		cols argument.

		p{wd} - Produces a column with each item typeset in a |\parbox|
		of width {wd}, as if it were the argument of a
		\parbox[t]{wd} command. However, a |\\| may not appear
		in the item, except in the following situations:
		1. inside an environment like |minipage|, |array|, or
		|tabular|.
		2. inside an explicit |\parbox|.
		3. in the scope of a |\centering|, |\raggedright|, or
		|\raggedleft| declaration. The latter declarations must
		appear inside braces or an environment when used in a
		p-column element.

		{num}{cols} - Equivalent to num copies of cols, where num is any positive
		integer and cols is any list of column-specifiers,
		which may contain another -expression.

These commands can be used inside a tabular environment:

|\cline|		Draw a horizontal line spanning some columns.
|\hline|		Draw a * horizontal line spanning all columns.
|\multicolumn|		Make an item spanning * several columns.
|\vline|		Draw a vertical line.


\cline{i-j}					*\cline*
		The |\cline| command draws horizontal lines across the columns
		specified, beginning in column i and ending in column j,
		which are identified in the mandatory argument.

\hline						*\hline*
		The |\hline| command will draw a horizontal line the width of
		the table.  It's most commonly used to draw a line at the top,
		bottom, and between the rows of the table.

\multicolumn{cols}{pos}{text} 			*\multicolumn*
		The |\multicolumn| is used to make an entry that spans several
		columns.  The first mandatory argument, {cols}, specifies the
		number of columns to span. The second mandatory argument,
		{pos}, specifies the formatting of the entry:
			c - centered
			l - flushleft
			r - flushright.
		The third mandatory argument, {text}, specifies what text is
		to make up the entry.

\vline						*\vline*
		The |\vline| command will draw a vertical line extending the
		full height and depth of its row. An |\hfill| command can be
		used to move the line to the edge of the column. It can also
		be used in an @-expression.

==============================================================================
t. thebibliography				*\thebibliography*
>
	\begin{thebibliography}{widestlabel}
		\bibitem[label]{cite_key}
		.
		.
		.
	\end{thebibliography}

The |\thebibliography| environment produces a bibliography or reference list.

In the |article-class|, this reference list is labelled "References"; in the
|report-class|, it is labelled "Bibliography".

{widestlabel}	Text that, when printed, is approximately as wide as the
 		widest item label produces by the |\bibitem| commands.

|\bibitem|		Specify a bibliography item.
|\cite|			Refer to a bibliography item.
|\nocite|		Include an item in the bibliography.
|BibTeX|		Automatic generation of bibliographies.

\bibitem					*\bibitem*
\bibitem[label]{citekey}
		The |\bibitem| command generates an entry labelled by [label].
		If the [label] argument is missing, a number is generated as
		the label, using the |\enumi| counter.  The {citekey} is any
		sequence of letters, numbers, and punctuation symbols not
		containing a comma. This command writes an entry on the `.aux'
		file containing {citekey} and the item's label. When this
		`.aux' file is read by the \begin{document} command, the
		item's label is associated with {citekey}, causing the
		reference to {citekey} by a |\cite| command to produce the
		associated label.

\cite						*\cite*
\cite[text]{keylist}
		The {keylist} argument is a list of citation keys.  This
		command generates an in-text citation to the references
		associated with the keys in {keylist} by entries on the `.aux'
		file read by the \begin{document} command.
		The optional text argument will appear after the
		citation, i.e.: >
			\cite[p.2]{knuth}
<		might produce `[Knuth, p. 2]'.

\nocite						*\nocite*
\nocite{keylist}
		The |\nocite| command produces no text, but writes
		{keylist}, which is a list of one or more citation
		keys, on the `.aux' file.

BibTeX						*BibTeX* *bibtex*
						*\bibliographystyle*
If you use the BibTeX program by Oren Patashnik (highly recommended if you
need a bibliography of more than a couple of titles) to maintain your
bibliography, you don't use the |thebibliography| environment.  Instead, you
include the lines:
>
	\bibliographystyle{style}
	\bibliography{bibfile}

where {style} refers to a file style.bst, which defines how your citations
will look. The standard styles distributed with BibTeX are:

{alpha}	Sorted alphabetically. Labels are formed from name of author and year
	of publication.
{plain} Sorted alphabetically. Labels are numeric.
{unsrt} Like plain, but entries are in order of citation.
{abbrv} Like plain, but more compact labels.

In addition, numerous other BibTeX style files exist tailored to the demands
of various publications.

						*\bibliography*
The argument to |\bibliography| refers to the file bibfile.bib, which should
contain your database in BibTeX format. Only the entries referred to via
|\cite| and |\nocite| will be listed in the bibliography.

==============================================================================
u. theorem					*theorem*
>
	\begin{theorem}
		theorem text
	\end{theorem}

The |theorem| environment produces "Theorem x" in boldface followed by your
theorem text.

==============================================================================
v. titlepage					*titlepage*
>
	\begin{titlepage}
		text
	\end{titlepage}

The |titlepage| environment creates a title page, i.e. a page with no printed
page number or heading. It also causes the following page to be numbered page
one. Formatting the title page is left to you. The |\today| command comes in
handy for title pages.

Note that you can use the |\maketitle| to produce a standard title page.

==============================================================================
x. verbatim					*verbatim*
>
	\begin{verbatim}
		text
	\end{verbatim}

The |verbatim| environment is a paragraph-making environment that gets LaTeX
to print exactly what you type in. It turns LaTeX into a typewriter with
carriage returns and blanks having the same effect that they would on a
typewriter.

\verb						*\verb*
\verb char literal_text char
\verb*char literal_text char
		Typesets literal_text exactly as typed, including
		special characters and spaces, using a typewriter |\tt|
		type style. There may be no space between |\verb| or
		|\verb|* and char (space is shown here only for
		clarity).  The *-form differs only in that spaces are
		printed as `\verb*| |\'.

==============================================================================
y. verse					*verse*
>
	\begin{verse}
		text
	\end{verse}

The |verse| environment is designed for poetry, though you may find other uses
for it.

The margins are indented on the left and the right. Separate the lines of each
stanza with |\\|, and use one or more blank lines to separate the stanzas.

==============================================================================
8. Footnotes					*latex-footnotes*

Footnotes can be produced in one of two ways. They can be produced with one
command, the |\footnote| command. They can also be produced with two commands,
the |\footnotemark| and the |\footnotetext| commands. See the specific command for
information on why you would use one over the other.

|\footnote| 	Insert a footnote
|\footnotemark|	Insert footnote mark only
|\footnotetext|	Insert footnote text only

\footnote[number]{text}				*\footnote*
		Command places the numbered footnote text at the bottom of the
		current page. The optional argument, number, is used to change
		the default footnote number.  This command can only be used in
		outer paragraph mode; i.e., you cannot use it in sectioning
		commands like |\chapter|, in |\figure|, |\table| or in a
		|\tabular| environment.

\footnotemark					*\footnotemark*
		Command puts the footnote number in the text. This command can
		be used in inner paragraph mode. The text of the footnote is
		supplied by the |\footnotetext| command.
		This command can be used to produce several consecutive
		footnote markers referring to the same footnote by using
>
			\footnotemark[\value{footnote}]
<
		after the first |\footnote| command.

\footnotetext[number]{text}			*\footnotetext*
		Command produces the text to be placed at the bottom of the
		page. This command can come anywhere after the |\footnotemark|
		command. The |\footnotetext| command must appear in outer
		paragraph mode.  The optional argument, number, is used to
		change the default footnote number.

==============================================================================
9. Lengths					*latex-lengths*

A length is a measure of distance. Many LaTeX commands take a length as an
argument.

|\newlength|	Define a new length.
|\setlength|	Set the value of a length.
|\addtolength|	Add a quantity to a length.
|\settodepth|	Set a length to  the depth of something.
|\settoheight|	Set a length to the height of  something.
|\settowidth|	Set a length to the width of something.
|pre-lengths|	Lengths that are, like, predefined.

\newlength{\gnat}				*\newlength*
		The |\newlength| command defines the mandatory argument, \gnat,
		as a length command with a value of 0in. An error occurs if a
		\gnat command already exists.

\setlength{\gnat}{length}			*\setlength*
		The |\setlength| command is used to set the value of a \gnat
		command. The {length} argument can be expressed in any terms
		of length LaTeX understands, i.e., inches (in), millimetres
		(mm), points (pt), etc.

\addtolength{\gnat}{length} 			*\addtolength*
		The |\addtolength| command increments a \gnat by the amount
		specified in the {length} argument. It can be a negative
		amount.

\settodepth{\gnat}{text} 			*\settodepth*
		The |\settodepth| command sets the value of a \gnat command
		equal to the depth of the {text} argument.

\settoheight{\gnat}{text} 			*\settoheight*
		The |\settoheight| command sets the value of a \gnat command
		equal to the height of the {text} argument.

\settowidth{\gnat}{text}			*\settowidth*
		The |\settowidth| command sets the value of a \gnat command
		equal to the width of the {text} argument.

Predefined lengths				*pre-lengths*

\width 						*\width*
\height						*\height*
\depth						*\depth*
\totalheight					*\totalheight*
		These length parameters can be used in the arguments of the
		box-making commands See section Spaces & Boxes. They specify
		the natural width etc.  of the text in the box.
		\totalheight equals \height + \depth.
		To make a box with the text stretched to double the natural
		size, e.g., say: >
			\makebox[2\width]{Get a stretcher}

==============================================================================
10. Letters					*latex-letters*

You can use LaTeX to typeset letters, both personal and business. The letter
document class is designed to make a number of letters at once, although you
can make just one if you so desire.

Your `.tex' source file has the same minimum commands as the other document
classes, i.e., you must have the following commands as a minimum: >
	\documentclass{letter}
	\begin{document}
		...
		letters
		...
	\end{document}

Each letter is a letter environment, whose argument is the name and address of
the recipient. For example, you might have: >
	\begin{letter}
		{Mr. Joe Smith\\
		2345 Princess St.  \\
		Edinburgh, EH1 1AA}
		...
	\end{letter}

The letter itself begins with the |\opening| command.  The text of the letter
follows. It is typed as ordinary LaTeX input.  Commands that make no sense in
a letter, like |\chapter|, do not work. The letter closes with a |\closing|
command.

After the closing, you can have additional material. The |\cc| command produces
the usual "cc: ...". There's also a similar |\encl| command for a list of
enclosures. With both these commands, use|\\| to separate the items.

These commands are used with the letter class:
|\address|	Your return address.
|\cc|		Cc list.  closing Saying goodbye.
|\encl|		List of enclosed material.
|\location|	Your organisation's address.
|\makelabels|	Making address labels.
|\name|		Your name, for the return address.
|\opening|	Saying hello.
|\ps|		Adding a postscript.
|\signature|	Your signature.
|\startbreaks|	Allow page breaks.
|\stopbreaks|	Disallow page breaks.
|\telephone|	Your phone number.

\address{Return address}			*\address*
		The return address, as it should appear on the letter and the
		envelope.  Separate lines of the address should be separated
		by |\\| commands. If you do not make an |\address| declaration,
		then the letter will be formatted for copying onto your
		organisation's standard letterhead. (See section Overview of
		LaTeX and Local Guide, for details on your local
		implementation). If you give an |\address| declaration, then
		the letter will be formatted as a personal letter.

\cc{Kate Schechter\\Rob McKenna}		*\cc*
		Generate a list of other persons the letter was sent to. Each
		name is printed on a separate line.

\closing{text}					*\closing*
		The letter closes with a |\closing| command, i.e., >
			\closing{Best Regards,} \encl{CV\\Certificates}
<		Generate a list of enclosed material.

\location{address}				*\location*
		This modifies your organisation's standard address. This only
		appears if the firstpage pagestyle is selected.

\makelabels{number}				*\makelabels*
		If you issue this command in the preamble, LaTeX will create a
		sheet of address labels. This sheet will be output before the
		letters.

\name{June Davenport}				*\name*
		Your name, used for printing on the envelope together with the
		return address.

\opening{text}					*\opening*
		The letter begins with the |\opening| command. The mandatory
		argument, text, is whatever text you wish to start your
		letter, i.e., >
			\opening{Dear Joe,}

\ps						*\ps*
		Use this command before a postscript.

\signature{Harvey Swick}			*\signature*
		Your name, as it should appear at the end of the letter
		underneath the space for your signature. Items that should go
		on separate lines should be separated by |\\| commands.

\startbreaks					*\startbreaks*
		Used after a |\stopbreaks| command to allow page breaks again.

\stopbreaks					*\stopbreaks*
		Inhibit page breaks until a |\startbreaks| command occurs.

\telephone{number}				*\telephone*
		This is your telephone number. This only appears if the
		firstpage pagestyle is selected.

==============================================================================
11. Line & Page Breaking			*latex-breaking*

The first thing LaTeX does when processing ordinary text is to translate your
input file into a string of glyphs and spaces. To produce a printed document,
this string must be broken into lines, and these lines must be broken into
pages. In some environments, you do the line breaking yourself with the |\\|
command, but LaTeX usually does it for you.

|\\| 			Start a new line
|hyph-| 		Insert explicit hyphenation
|\cleardoublepage| 	Start a new right-hand page
|\clearpage| 		Start a new page
|\enlargethispage| 	Enlarge the current page a bit
|\fussy| 		Be fussy about line breaking
|\hyphenation| 		Tell LaTeX how to hyphenate a word
|\linebreak| 		Break the line
|\newline| 		Break the line prematurely
|\newpage| 		Start a new page
|\nolinebreak| 		Don't break the current line
|\nopagebreak| 		Don't make a page break here
|\pagebreak| 		Please make a page break here
|\sloppy| 		Be sloppy about line breaking

\\[*][extraspace]				*\\* *\\\\*
		The |\\| command tells LaTeX to start a new line. It has an
		optional argument, [extraspace], that specifies how much extra
		vertical space is to be inserted before the next line. This
		can be a negative amount.
		The \\* command is the same as the ordinary |\\| command
		except that it tells LaTeX not to start a new page after the
		line.

\-						*hyph-*
		The \- command tells LaTeX that it may hyphenate the word at
		that point.  LaTeX is very good at hyphenating, and it will
		usually find all correct hyphenation points. The \- command is
		used for the exceptional cases.
		Note: when you insert \- commands in a word, the word will
		only be hyphenated at those points and not at any of the
		hyphenation points that LaTeX might otherwise have chosen.

\cleardoublepage				*\cleardoublepage*
		The |\cleardoublepage| command ends the current page and causes
		all figures and tables that have so far appeared in the input
		to be printed.  In a two-sided printing style (|twoside|), it
		also makes the next page a right-hand (odd-numbered) page,
		producing a blank page if necessary.

\clearpage					*\clearpage*
		The |\clearpage| command ends the current page and causes all
		figures and tables that have so far appeared in the input to
		be printed.

\enlargethispage{size} 				*\enlargethispage*
\enlargethispage*{size}
		Enlarge the textheight for the current page by the
		specified amount; e.g.: >

			\enlargethispage{\baselineskip}
<
		will allow one additional line.  The starred form
		tries to squeeze the material together on the page as
		much as possible. This is normally used together with
		an explicit |\pagebreak|.

\fussy						*\fussy*
		This declaration (which is the default) makes TeX more fussy
		about line breaking. This can avoids too much space between
		words, but may produce overfull boxes.  This command cancels
		the effect of a previous |\sloppy| command.

\hyphenation{words}				*\hyphenation*
		The |\hyphenation| command declares allowed hyphenation points,
		where words is a list of words, separated by spaces, in which
		each hyphenation point is indicated by a - character.

\linebreak[number]				*\linebreak*
		The |\linebreak| command tells LaTeX to break the current line
		at the point of the command. With the optional argument,
		number, you can convert the |\linebreak| command from a demand
		to a request. The [number] must be a number from 0 to 4. The
		higher the number, the more insistent the request is.  The
		|\linebreak| command causes LaTeX to stretch the line so it
		extends to the right margin.

\newline					*\newline*
		The |\newline| command breaks the line right where it is. It
		can only be used in paragraph mode.

\newpage					*\newpage*
		The |\newpage| command ends the current page.

\nolinebreak[number]				*\nolinebreak*
		The |\nolinebreak| command prevents LaTeX from breaking the
		current line at the point of the command. With the optional
		argument, [number], you can convert the |\nolinebreak| command
		from a demand to a request. The [number] must be a number from 0
		to 4. The higher the number, the more insistent the request
		is.

\nopagebreak[number]				*\nopagebreak*
		The |\nopagebreak| command prevents LaTeX from breaking the
		current page at the point of the command. With the optional
		argument, [number], you can convert the |\nopagebreak| command
		from a demand to a request. The [number] must be a number from
		0 to 4. The higher the number, the more insistent the request
		is.

\pagebreak[number]				*\pagebreak*
		The |\pagebreak| command tells LaTeX to break the current page
		at the point of the command. With the optional argument,
		[number], you can convert the |\pagebreak| command from a
		demand to a request. The [number] must be a number from 0 to
		4. The higher the number, the more insistent the request is.

\sloppy						*\sloppy*
		This declaration makes TeX less fussy about line breaking.
		This can prevent overfull boxes, but may leave too much space
		between words.
		Lasts until a |\fussy| command is issued.

==============================================================================
12. Making Paragraphs				*latex-paragraphs*

A paragraph is ended by one or more completely blank lines -- lines not
containing even a |\%|. A blank line should not appear where a new paragraph
cannot be started, such as in math mode or in the argument of a sectioning
command.

|\indent| 	Indent this paragraph.
|\noindent| 	Do not indent this paragraph.
|\par| 		Another way of writing a blank line.

\indent 					*\indent*
		This produces a horizontal space whose width equals the width
		of the paragraph indentation. It is used to add paragraph
		indentation where it would otherwise be suppressed.

\noindent 					*\noindent*
		When used at the beginning of the paragraph, it suppresses the
		paragraph indentation. It has no effect when used in the
		middle of a paragraph.

\par						*\par*
		Equivalent to a blank line; often used to make command or
		environment definitions easier to read.

==============================================================================
13. Margin Notes				*latex-margin-notes*

\marginpar[left]{right}				*\marginpar*
		This command creates a note in the margin. The first line will
		be at the same height as the line in the text where the
		|\marginpar| occurs.

		When you only specify the mandatory argument {right}, the text
		will be placed:
		* in the right margin for one-sided layout
		* in the outside margin for two-sided layout (|twoside|)
		* in the nearest margin for two-column layout (|twocolumn|)

\reversemarginpar				*\reversemarginpar*
		By issuing the command |\reversemarginpar|, you can force the
		marginal notes to go into the opposite (inside) margin.

When you specify both arguments, left is used for the left margin, and right
is used for the right margin.

The first word will normally not be hyphenated; you can enable hyphenation by
prefixing the first word with a \hspace{0pt} command (|hspace|).

==============================================================================
14. Math Formulae				*latex-math*
						*displaymath*
There are three environments (|latex-environments|) that put LaTeX in math
mode:
|math|  	For Formulae that appear right in the text.
|displaymath|  	For Formulae that appear on their own line.
|equation|  	The same as the displaymath environment except that it adds an
		equation number in the right margin.

The |math| environment can be used in both paragraph and LR mode, but the
|displaymath| and |equation| environments can be used only in paragraph mode. The
|math| and |displaymath| environments are used so often that they have the
following short forms:
	\(...\)    instead of    \begin{math}...\end{math}
	\[...\]    instead of    \begin{displaymath}...\end{displaymath}

In fact, the math environment is so common that it has an even shorter form:
	$ ... $    instead of     \(...\)

|sub-sup|	Also known as exponent or index.
|math-symbols|	Various mathematical squiggles.
|math-spacing|	Thick, medium, thin and negative spaces.
|math-misc|	Stuff that doesn't fit anywhere else.

==========
Subscripts & Superscripts			*sub-sup*
						*subscripts* *superscripts*

To get an expression exp to appear as a subscript, you just type _{exp}.  To
get exp to appear as a superscript, you type ^{exp}. LaTeX handles
superscripted superscripts and all of that stuff in the natural way. It even
does the right thing when something has both a subscript and a superscript.

==========
Math Symbols					*math-symbols*

LaTeX provides almost any mathematical symbol you're likely to need. The
commands for generating them can be used only in math mode. For example, if
you include >
	$\pi$
in your source, you will get the symbol in your output.

==========
Spacing in Math Mode				*math-spacing*

In a math environment, LaTeX ignores the spaces you type and puts in the
spacing that it thinks is best. LaTeX formats mathematics the way it's done in
mathematics texts. If you want different spacing, LaTeX provides the following
four commands for use in math mode:
	\; - a thick space			*math;*
	\: - a medium space			*math:*
	\, - a thin space			*math,*
	\! - a negative thin space		*math!*

==========
Math Miscellany					*math-misc*

\cdots						*\cdots*
		Produces a horizontal ellipsis where the dots are raised to
		the centre of the line.
\ddots						*\ddots*
		Produces a diagonal ellipsis.
\frac{num}{den}					*\frac*
		Produces the fraction num divided by den.
\ldots						*\ldots*
		Produces an ellipsis. This command works in any mode, not just
		math mode.
\overbrace{text}				*\overbrace*
		Generates a brace over text.
\overline{text}					*\overline*
		Causes the argument text to be overlined.
\sqrt[root]{arg}				*\sqrt*
		Produces the square root of its argument.  The optional
		argument, [root], determines what root to produce, i.e., the
		cube root of x+y would be typed as: >
			$\sqrt[3]{x+y}$.
\underbrace{text}				*\underbrace*
		Generates text with a brace underneath.
\underline{text}				*\underline*
		Causes the argument text to be underlined. This command can
		also be used in paragraph and LR mode.
\vdots						*\vdots*
		Produces a vertical ellipsis.

==============================================================================
15. Modes					*latex-modes*

When LaTeX is processing your input text, it is always in one of three modes:
	Paragraph mode					*paragraph-mode*
	Math mode					*math-mode*
	Left-to-right mode, called LR mode for short.	*lr-mode*

LaTeX changes mode only when it goes up or down a staircase to a different
level, though not all level changes produce mode changes. Mode changes occur
only when entering or leaving an environment, or when LaTeX is processing the
argument of certain text-producing commands.

|paragraph-mode| is the most common; it's the one LaTeX is in when processing
ordinary text. In that mode, LaTeX breaks your text into lines and breaks the
lines into pages. LaTeX is in |math-mode| when it's generating a mathematical
formula. In |lr-mode|, as in |paragraph-mode|, LaTeX considers the output that
it produces to be a string of words with spaces between them. However, unlike
|paragraph-mode|, LaTeX keeps going from left to right; it never starts a new
line in |lr-mode|. Even if you put a hundred words into an |\mbox|, LaTeX would
keep typesetting them from left to right inside a single box, and then
complain because the resulting box was too wide to fit on the line.

LaTeX is in |lr-mode| when it starts making a box with an |\mbox| command.  You
can get it to enter a different mode inside the box - for example, you can
make it enter |math-mode| to put a formula in the box. There are also several
text-producing commands and environments for making a box that put LaTeX in
|paragraph-mode|. The box make by one of these commands or environments will be
called a |\parbox|. When LaTeX is in |paragraph-mode| while making a box, it is
said to be in "inner paragraph mode". Its normal |paragraph-mode|, which it
starts out in, is called "outer paragraph mode".

==============================================================================
16. Page Styles					*latex-page-styles*

The |\documentclass| command determines the size and position of the page's head
and foot. The page style determines what goes in them.

|\maketitle| 	Generate a title page.
|\pagenumbering| Set the style used for page numbers.
|\pagestyle| 	Change the headings/footings style.
|\thispagestyle| Change the headings/footings style for this page.

\maketitle					*\maketitle*
		The |\maketitle| command generates a title on a separate title
		page - except in the |\article| class, where the title normally
		goes at the top of the first page.  Information used to
		produce the title is obtained from the following declarations:

		|\author|	Who wrote this stuff?
		|\date|		The date the document was created.
		|\thanks|	A special form of footnote.
		|\title|		How to set the document title.

		\author{names}				*\author* *\and*
			The |\author| command declares the author(s), where
			names is a list of authors separated by \and commands.
			Use |\\| to separate lines within a single author's
			entry -- for example, to give the author's institution
			or address.

		\date{text}				*\date*
			The |\date| command declares text to be the document's
			date.  With no |\date| command, the current date is
			used.

		\thanks{text}				*\thanks*
			The |\thanks| command produces a |\footnote| to the
			title.

		\title{text}				*\title*
			The |\title| command declares text to be the title. Use
			|\\| to tell LaTeX where to start a new line in a long
			title.

\pagenumbering{numstyle}			*\pagenumbering*
		Specifies the style of page numbers. Possible values of
		'numstyle' are:
			arabic - Arabic numerals		*arabic*
			roman  - Lowercase Roman numerals 	*roman*
			Roman  - Uppercase Roman numerals 	*Roman*
			alph   - Lowercase letters 		*alph*
			Alph   - Uppercase letters 		*Alph*

\pagestyle{option}				*\pagestyle*
						*plain* *empty* *headings*
		The |\pagestyle| command changes the style from the current
		page on throughout the remainder of your document.
		The valid options are:
		plain      - Just a plain page number.
		empty      - Produces empty heads and feet no page numbers.
		headings   - Puts running headings on each page. The document
			     style specifies what goes in the headings.
		myheadings - You specify what is to go in the heading with the
			     |\markboth| or the |\markright| commands.

		|\markboth| 	Set left and right headings.
		|\markright| 	Set right heading only.

		\markboth{left head}{right head}	*\markboth*
			The |\markboth| command is used in conjunction with the
			page style myheadings for setting both the left and
			the right heading.
			Note that a "left-hand heading" is generated by the
			last |\markboth| command before the end of the page,
			while a "right-hand heading" is generated by the first
			|\markboth| or |\markright| that comes on the page if
			there is one, otherwise by the last one before the
			page.


		\markright{right head}			*\markright*
			The |\markright| command is used in conjunction with
			the page style |\myheadings| for setting the right
			heading, leaving the left heading unchanged.
			Note that a "left-hand heading" is generated by the
			last |\markboth| command before the end of the page,
			while a "right-hand heading" is generated by the first
			|\markboth| or |\markright| that comes on the page if
			there is one, otherwise by the last one before the
			page.

\thispagestyle{option}				*\thispagestyle*
		The |\thispagestyle| command works in the same manner as the
		|\pagestyle| command except that it changes the style for the
		current page only.

==============================================================================
17. Sectioning					*latex-sectioning*

Sectioning commands provide the means to structure your text into units.
|\part|
|\chapter| (report and book class only)
|\section|
|\subsection|
|\subsubsection|
|\paragraph|
|\subparagraph|

All sectioning commands take the same general form, i.e.,

					*\part*
					*\chapter* (report and book class only)
					*\section* *\subsection* *\subsubsection*
					*\paragraph* *\subparagraph*
\chapter[optional]{title}
		In addition to providing the heading in the text, the
		mandatory argument of the sectioning command can appear in two
		other places:
		1. The table of contents
		2. The running head at the top of the page. You may not want
		   the same thing to appear in these other two places as
		   appears in the text heading. To handle this situation, the
		   sectioning commands have an optional argument that provides
		   the text for these other two purposes.

All sectioning commands have *\-forms that print a title, but do not include a
number and do not make an entry in the table of contents.

\appendix 					*\appendix*
		The |\appendix| command changes the way sectional units are
		numbered. The |\appendix| command generates no text and does
		not affect the numbering of parts. The normal use of this
		command is something like: >
			\chapter{The First Chapter}
			...
			\appendix \chapter{The First Appendix}


==============================================================================
18. Spaces & Boxes				*latex-spaces-boxes*

All the predefined length parameters See section Predefined lengths can be
used in the arguments of the box-making commands.

 Horizontal space:

|\dotfill|	Stretchable horizontal dots.
|\hfill|	Stretchable horizontal space.
|\hrulefill|	Stretchable horizontal rule.
|\hspace|	Fixed horizontal space.

 Vertical space:

|\addvspace|	Fixed vertical space.
|\bigskip|	Fixed vertical space.
|\medskip|	Fixed vertical space.
|\smallskip|	Fixed vertical space.
|\vfill|	Stretchable vertical space.
|\vspace|	Fixed vertical space.

 Boxes:

|\fbox|		Framebox.
|\framebox|	Framebox, adjustable position.
|\lrbox|	An environment like |\sbox|.
|\makebox|	Box, adjustable position.
|\mbox|		Box.
|\newsavebox|	Declare a name for saving a box.
|\parbox|	Box with text in paragraph mode.
|\raisebox|	Raise or lower text.
|\rule|		Lines and squares.
|\savebox|	Like |\makebox|, but save the text for later use.
|\sbox|		Like |\mbox|, but save the text for later use.
|\usebox|	Print saved text.

Horizontal space:				*latex-hor-space*

LaTeX removes horizontal space that comes at the end of a line. If you don't
want LaTeX to remove this space, include the optional * argument.  Then the
space is never removed.

\dotfill					*\dotfill*
		The |\dotfill| command produces a "rubber length" that produces
		dots instead of just spaces.

\hfill						*\hfill*
		The |\hfill| fill command produces a "rubber length" which can
		stretch or shrink horizontally. It will be filled with spaces.

\hrulefill					*\hrulefill*
		The |\hrulefill| fill command produces a "rubber length" which
		can stretch or shrink horizontally. It will be filled with a
		horizontal rule.

\hspace[*]{length}				*\hspace*
		The |\hspace| command adds horizontal space. The length of the
		space can be expressed in any terms that LaTeX understands,
		i.e., points, inches, etc. You can add negative as well as
		positive space with an |\hspace| command. Adding negative space
		is like backspacing.


Vertical space:					*latex-ver-space*

LaTeX removes vertical space that comes at the end of a page. If you don't
want LaTeX to remove this space, include the optional * argument.  Then the
space is never removed.

\addvspace{length}				*\addvspace*
		The |\addvspace| command normally adds a vertical space of
		height length.  However, if vertical space has already been
		added to the same point in the output by a previous
		|\addvspace| command, then this command will not add more space
		than needed to make the natural length of the total vertical
		space equal to length.

\bigskip					*\bigskip*
		The |\bigskip| command is equivalent to \vspace{bigskipamount}
		where bigskipamount is determined by the document class.

\medskip					*\medskip*
		The |\medskip| command is equivalent to \vspace{medskipamount}
		where medskipamount is determined by the document class.

\smallskip					*\smallskip*
		The |\smallskip| command is equivalent to
		\vspace{smallskipamount} where smallskipamount is determined
		by the document class.

\vfill						*\vfill*
		The |\vfill| fill command produces a rubber length which can
		stretch or shrink vertically.

\vspace[*]{length}				*\vspace*
		The |\vspace| command adds vertical space. The length of the
		space can be expressed in any terms that LaTeX understands,
		i.e., points, inches, etc. You can add negative as well as
		positive space with an |\vspace| command.


Boxes:						*latex-boxes*

\fbox{text}					*\fbox*
		The |\fbox| command is exactly the same as the |\mbox| command,
		except that it puts a frame around the outside of the box that
		it creates.

\framebox[width][position]{text}		*\framebox*
		The |\framebox| command is exactly the same as the |\makebox|
		command, except that it puts a frame around the outside of the
		box that it creates.
		The |\framebox| command produces a rule of thickness
		|\fboxrule|, and leaves a space |\fboxsep| between the rule and
		the contents of the box.

lrbox						*\lrbox*
\begin{lrbox}{cmd} text \end{lrbox}
		This is the environment form of |\sbox|.
		The text inside the environment is saved in the box cmd, which
		must have been declared with |\newsavebox|.

\makebox[width][position]{text} 		*\makebox*
		The |\makebox| command creates a box just wide enough to
		contain the text specified. The width of the box is specified
		by the optional [width] argument.  The position of the text
		within the box is determined by the optional [position]
		argument.
			c -- centred (default)
			l -- flushleft
			r -- flushright
			s -- stretch from left to right margin. The text must
			     contain stretchable space for this to work.
		See section |\picture-makebox|.

\mbox{text}					*\mbox*
		The |\mbox| command creates a box just wide enough to hold the
		text created by its argument.
		Use this command to prevent text from being split across
		lines.

\newsavebox{cmd}				*\newsavebox*
		Declares {cmd}, which must be a command name that is not
		already defined, to be a bin for saving boxes.


\parbox[position][height][innerpos]{width}{text} 	*\parbox*
		A parbox is a box whose contents are created in
		|\paragraph-mode|. The |\parbox| has two

	Mandatory arguments:
'width'		specifies the width of the parbox
'text'		the text that goes inside the parbox.

	Optional arguments:
'position'	LaTeX will position a parbox so its centre lines up with the
		centre of the text line. The optional position argument allows
		you to line up either the top or bottom line in the parbox
		(default is top).

'height'        If the height argument is not given, the box will have the
		natural height of the text.

'innerpos'	The inner-pos argument controls the placement of the text
		inside the box. If it is not specified, position is used.
			t -- text is placed at the top of the box
			c -- text is centred in the box
			b -- text is placed at the bottom of the box
			s -- stretch vertically. The text must contain
			     vertically stretchable space for this to work.

		A |\parbox| command is used for a parbox containing a small
		piece of text, with nothing fancy inside. In particular, you
		shouldn't use any of the paragraph-making environments inside
		a |\parbox| argument. For larger pieces of text, including ones
		containing a paragraph-making environment, you should use a
		|\minipage| environment.

\raisebox{distance}[extendabove][extendbelow]{text}   *\raisebox*
		The |\raisebox| command is used to raise or lower text. The
		first mandatory argument specifies how high the text is to be
		raised (or lowered if it is a negative amount). The text
		itself is processed in LR mode.
		Sometimes it's useful to make LaTeX think something has a
		different size than it really does - or a different size than
		LaTeX would normally think it has.  The |\raisebox| command
		lets you tell LaTeX how tall it is.
		The first optional argument, extend-above, makes LaTeX think
		that the text extends above the line by the amount specified.
		The second optional argument, extend-below, makes LaTeX think
		that the text extends below the line by the amount specified.

\rule[raiseheight]{width}{thickness} 		*\rule*
		The |\rule| command is used to produce horizontal lines. The
		arguments are defined as follows:
'raiseheight'	specifies how high to raise the rule (optional)
'width'		specifies the length of the rule (mandatory)
'thickness'	specifies the thickness of the rule (mandatory)

\savebox{cmd}[width][pos]{text} 		*\savebox*
		This command typeset text in a box just as for |\makebox|.
		However, instead of printing the resulting box, it saves it in
		bin cmd, which must have been declared with |\newsavebox|.

\sbox{text}					*\sbox*
		This commands typeset text in a box just as for |\mbox|.
		However, instead of printing the resulting box, it saves it in
		bin cmd, which must have been declared with |\newsavebox|.

\usebox{cmd}					*\usebox*
		Prints the box most recently saved in bin cmd by a |\savebox|
		command.

==============================================================================
19. Special Characters				*latex-special*

The following characters play a special role in LaTeX and are called "special
printing characters", or simply "special characters". >
			 #  $  %  &  ~  _  ^  \  {  }
Whenever you put one of these special characters into your file, you are doing
something special. If you simply want the character to be printed just as any
other letter, include a \ in front of the character. For example, \$ will
produce $ in your output.

One exception to this rule is the \ itself because |\\| has its own special
meaning. A \ is produced by typing $\backslash$ in your file.

Also, \~ means `place a tilde accent over the following letter', so you will
probably want to use |\verb| instead.
						*\symbol*
In addition, you can access any character of a font once you know its number
by using the |\symbol| command. For example, the character used for displaying
spaces in the |\verb|* command has the code decimal 32, so it can be typed as
\symbol{32}.

You can also specify octal numbers with ' or hexadecimal numbers with ", so
the previous example could also be written as \symbol{'40} or \symbol{"20}.

==============================================================================
20. Splitting the Input				*latex-inputting*

A large document requires a lot of input. Rather than putting the whole input
in a single large file, it's more efficient to split it into several smaller
ones. Regardless of how many separate files you use, there is one that is the
root file; it is the one whose name you type when you run LaTeX.

|\include| 		Conditionally include a file
|\includeonly| 		Determine which files are included
|\input| 		Unconditionally include a file

\include{file}					*\include*
		The \include command is used in conjunction with the
		|\includeonly| command for selective inclusion of
		files. The file argument is the first name of a file,
		denoting `file.tex' . If file is one the file names in
		the file list of the |\includeonly| command or if there
		is no |\includeonly| command, the \include command is
		equivalent to: >
			\clearpage \input{file} \clearpage
<
		except that if the file `file.tex' does not exist,
		then a warning message rather than an error is
		produced. If the file is not in the file list, the
		\include command is equivalent to |\clearpage|.

		The |\include| command may not appear in the preamble or in a
		file read by another |\include| command.

\includeonly{filelist}	 			*\includeonly*
		The |\includeonly| command controls which files will be read in
		by an |\include| command. {filelist} should be a
		comma-separated list of filenames. Each filename must match
		exactly a filename specified in a |\include| command. This
		command can only appear in the preamble.

\input{file} 					*\input*
		The |\input| command causes the indicated file to be read and
		processed, exactly as if its contents had been inserted in the
		current file at that point. The file name may be a complete
		file name with extension or just a first name, in which case
		the file `file.tex' is used.
==============================================================================
21. Starting & Ending				*latex-start-end*

Your input file must contain the following commands as a minimum:
\documentclass{class} 		|\documentclass|
\begin{document} 		|\begin|
... your text goes here ...
\end{document} 			|\end|

where the class selected is one of the valid classes for LaTeX.
See |\classes|for details of the various document classes.

You may include other LaTeX commands between the |\documentclass| and the
\begin{document} commands (i.e., in the `preamble').
==============================================================================
22. Table of Contents				*latex-toc*

						*\tableofcontents*
A table of contents is produced with the |\tableofcontents| command. You put
the command right where you want the table of contents to go; LaTeX does the
rest for you. It produces a heading, but it does not automatically start a new
page. If you want a new page after the table of contents, include a |\newpage|
command after the |\tableofcontents| command.

						*\listoffigures* *\listoftables*
There are similar commands |\listoffigures| and |\listoftables| for producing a
list of figures and a list of tables, respectively.  Everything works exactly
the same as for the table of contents.

						*\nofiles*
NOTE: If you want any of these items to be generated, you cannot have the
\nofiles command in your document.

|\addcontentsline|	Add an entry to table of contents etc.
|\addtocontents|		Add text directly to table of contents file etc.

\addcontentsline{file}{secunit}{entry}		*\addcontentsline*
		The |\addcontentsline| command adds an entry to the specified
		list or table where:
{file}		is the extension of the file on which information is to be
        	written:
        		toc (table of contents),
        		lof (list of figures),
        		lot (list of tables).
{secunit}	controls the formatting of the entry. It should be one of the
		following, depending upon the value of the file argument:
			toc -- the name of the sectional unit, such as part or
				subsection.
			lof -- figure
			lot -- table
{entry}		is the text of the entry.

\addtocontents{file}{text}				*\addtocontents*
		The |\addtocontents| command adds text (or formatting commands)
		directly to the file that generates the table of contents or
		list of figures or tables.
{file}		is the extension of the file on which information is to be written:
			toc (table of contents),
			lof (list of figures),
			lot (list of tables).
{text}		is the information to be written.

==============================================================================
23. Terminal Input/Output				*latex-terminal*

|\typein|		Read text from the terminal.
|\typeout|		Write text to the terminal.

\typein[cmd]{msg}					*\typein*
		Prints {msg} on the terminal and causes LaTeX to stop and wait
		for you to type a line of input, ending with return. If the
		[cmd] argument is missing, the typed input is processed as if
		it had been included in the input file in place of the
		|\typein| command. If the [cmd] argument is present, it must be
		a command name. This command name is then defined or redefined
		to be the typed input.

\typeout{msg}						*\typeout*
		Prints {msg} on the terminal and in the `.log' file. Commands
		in {msg} that are defined with |\newcommand| or |\renewcommand|
		are replaced by their definitions before being printed.

							*\space*
LaTeX's usual rules for treating multiple spaces as a single space and
ignoring spaces after a command name apply to {msg}. A |\space| command in {msg}
causes a single space to be printed. A ^^J in {msg} prints a newline.

==============================================================================
24. Typefaces					*latex-typefaces*

The typeface is specified by giving the "size" and "style". A typeface is also
called a "font".
|font-styles|		Select roman, italics etc.
|font-size|		Select point size.
|font-lowlevelcommands|	Commands for wizards.

Styles						*font-styles*

The following type style commands are supported by LaTeX.

These commands are used like: >
	\textit{italics text}.
The corresponding command in parenthesis is the "declaration form", which
takes no arguments. The scope of the declaration form lasts until the next
type style command or the end of the current group.

The declaration forms are cumulative; i.e., you can say: >
	\sffamily\bfseries
to get sans serif boldface.

You can also use the environment form of the declaration forms; e.g.: >
	\begin{ttfamily}...\end{ttfamily}.
<
\textrm (\rmfamily)		*\textrm* *\rmfamily*
		Roman

\textit (\itshape)		*\textit* *\itshape* *\emph*
		Emphasis (toggles between |\textit| and |\textrm|).

\textmd (\mdseries)		*\textmd* *\mdseries*
		Medium weight (default). The opposite of boldface.

\textbf (\bfseries)		*\textbf* *\bfseries*
		Boldface.

\textup (\upshape)		*\textup* *\upshape*
		Upright (default).  The opposite of slanted.

\textsl (\slshape)		*\textsl* *\slshape*
		Slanted.

\textsf (\sffamily)		*\textsf* *\sffamily*
		Sans serif.

\textsc (\scshape)		*\textsc* *\scshape*
		Small caps.

\texttt (\ttfamily)		*\texttt* *\ttfamily*
		Typewriter.

\textnormal (\normalfont)	*\textnormal* *\normalfont*
		Main document font.

\mathrm				*\mathrm*
		Roman, for use in math mode.

\mathbf  			*\mathbf*
		Boldface, for use in math mode.

\mathsf				*\mathsf*
		Sans serif, for use in math mode.

\mathtt				*\mathtt*
		Typewriter, for use in math mode.

\mathit				*\mathit*
		Italics, for use in math mode, e.g. variable names with
		several letters.

\mathnormal			*\mathnormal*
		For use in math mode, e.g. inside another type style
		declaration.

\mathcal			*\mathcal*
 `Calligraphic' letters, for use in math mode.

				*\mathversion*
In addition, the command \mathversion{bold} can be used for switching to bold
letters and symbols in formulas. \mathversion{normal} restores the default.

==========
Sizes						*font-size*

The following standard type size commands are supported by LaTeX.

The commands as listed here are "declaration forms". The scope of the
declaration form lasts until the next type style command or the end of the
current group.

You can also use the environment form of these commands; e.g. >
	\begin{tiny}...\end{tiny}

\tiny			        *\tiny*
\scriptsize		        *\scriptsize*
\footnotesize		        *\footnotesize*
\small			        *\small*
\normalsize(default)	        *\normalsize*
\large			        *\large*
\Large			        *\Large*
\LARGE			        *\LARGE*
\huge			        *\huge*
\Huge			        *\Huge*

==========
Low-level font commands				*font-lowlevelcommands*

These commands are primarily intended for writers of macros and packages. The
commands listed here are only a subset of the available ones. For full
details, you should consult Chapter 7 of The LaTeX Companion.

\fontencoding{enc}				*\fontencoding*
		Select font encoding. Valid encodings include OT1 and T1.

\fontfamily{family}  				*\fontfamily*
		Select font family. Valid families include:
			cmr  for Computer Modern Roman
			cmss for Computer Modern Sans Serif
			cmtt for Computer Modern Typewriter
		and numerous others.

\fontseries{series}				*\fontseries*
		Select font series. Valid series include:
			m Medium (normal)
			b Bold
			c Condensed
			bc Bold condensed
			bx Bold extended
		and various other combinations.

\fontshape{shape}				*\fontshape*
		Select font shape. Valid shapes are:
			n Upright (normal)
			it Italic
			sl Slanted (oblique)
			sc Small caps
			ui Upright italics
			ol Outline
		The two last shapes are not available for most font families.

\fontsize{size}{skip}				*\fontsize*
		Set font size. The first parameter is the font size to switch
		to; the second is the \baselineskip to use. The unit of both
		parameters defaults to pt. A rule of thumb is that the
		baselineskip should be 1.2 times the font size.

\selectfont					*\selectfont*
		The changes made by calling the four font commands described
		above do not come into effect until |\selectfont| is called.

\usefont{enc}{family}{series}{shape}		*\usefont*
		Equivalent to calling |\fontencoding|, |\fontfamily|,
		|\fontseries| and |\fontshape| with the given parameters,
		followed by |\selectfont|.

==============================================================================
25. Parameters					*latex-parameters*

The input file specification indicates the file to be formatted; TeX uses
`.tex' as a default file extension. If you omit the input file entirely, TeX
accepts input from the terminal. You specify command options by supplying a
string as a parameter to the command; e.g. >

	latex "\scrollmode\input foo.tex"

will process `foo.tex' without pausing after every error.

Output files are always created in the current directory. When you fail to
specify an input file name, TeX bases the output names on the file
specification associated with the logical name TEX_OUTPUT, typically
texput.log.

 vim:tw=78:ts=8:ft=help:norl:
syntax/bibsearch_atp.vim	[[[1
99
" Vim syntax file
" Language:	bibsearchTeX (bibsearchliographic database format for (La)TeX)
" Author:	Bernd Feige <Bernd.Feige@gmx.net>
" Modified:	Marcin Szamotulski
" Last Change:	Feb 19, 2010

" This is a modification of syntax/bibsearch.vim

" Initialization
" ==============
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
" First we source syntax/bib.vim file
source $VIMRUNTIME/syntax/bib.vim
" Ignore case
syn case ignore

" Keywords
" ========
syn keyword bibsearchType contained	article book booklet conference inbook
syn keyword bibsearchType contained	incollection inproceedings manual
syn keyword bibsearchType contained	mastersthesis misc phdthesis
syn keyword bibsearchType contained	proceedings techreport unpublished
syn keyword bibsearchType contained	string
syn keyword bibsearchEntryKw contained	address author booktitle annote chapter
syn keyword bibsearchEntryKw contained	crossref edition editor issn howpublished
syn keyword bibsearchEntryKw contained	institution fjournal journal key month mrclass 
syn keyword bibsearchEntryKw contained	note number organization pages publisher
syn keyword bibsearchEntryKw contained	school series type title volume year
" Non-standard:
syn keyword bibsearchNSEntryKw contained	abstract isbn issn keywords url

" Clusters
" ========
syn cluster bibsearchVarContents	contains=bibsearchUnescapedSpecial,bibsearchBrace,bibsearchParen
" This cluster is empty but things can be added externally:
"syn cluster bibsearchCommentContents

" Matches
" =======
syn match bibsearchUnescapedSpecial contained /[^\\][%&]/hs=s+1
syn match bibsearchKey contained /\s*[^ \t}="]\+,/hs=s,he=e-1 nextgroup=bibsearchField
syn match bibsearchVariable contained /[^{}," \t=]/
syn region bibsearchQuote contained start=/"/ end=/"/  contains=@bibsearchVarContents
syn region bibsearchBrace contained start=/{/ end=/}/  contains=@bibsearchVarContents
syn region bibsearchParen contained start=/(/ end=/)/  contains=@bibsearchVarContents
syn region bibsearchField contained start="\S\+\s*=\s*" end=/[}),]/me=e-1 contains=bibsearchEntryKw,bibsearchNSEntryKw,bibsearchBrace,bibsearchParen,bibsearchQuote,bibsearchVariable
syn region bibsearchEntryData contained start=/[{(]/ms=e+1 end=/[})]/me=e-1 contains=bibsearchKey,bibsearchField
" Actually, 5.8 <= Vim < 6.0 would ignore the `fold' keyword anyway, but Vim<5.8 would produce
" an error, so we explicitly distinguish versions with and without folding functionality:
if version < 600
  syn region bibsearchEntry start=/@\S\+[{(]/ end=/^\s*[})]/ transparent contains=bibsearchType,bibsearchEntryData nextgroup=bibsearchComment
else
  syn region bibsearchEntry start=/@\S\+[{(]/ end=/^\s*[})]/ transparent fold contains=bibsearchType,bibsearchEntryData nextgroup=bibsearchComment
endif
syn region bibComment2 start=/@Comment[{(]/ end=/^\s*@/me=e-1 contains=@bibsearchCommentContents nextgroup=bibsearchEntry

" Synchronization
" ===============
syn sync match All grouphere bibsearchEntry /^\s*@/
syn sync maxlines=200
syn sync minlines=50

" Bibsearch
" =========
syn region bibsearchComment start=/./ end=/\s*@/me=e-1 contains=@bibsearchCommentContents,@bibsearchSearchInfo nextgroup=bibsearchEntry
syn region bibsearchInfo start=/\s*\d/ end=/\s@/me=e-1 contained containedin=bibsearchComment

" Highlighting defaults
" =====================
" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_bib_syn_inits")
  if version < 508
    let did_bib_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink bibsearchType		bibType
  HiLink bibsearchEntryKw	bibEntryKw
  HiLink bibsearchNSEntryKw	bibNSEntryKw
  HiLink bibsearchKey		bibKey
  HiLink bibsearchVariable	bibVariable
  HiLink bibsearchUnescapedSpecial	bibUnescapedSpecial
  HiLink bibsearchComment	bibComment
  HiLink bibsearchComment2	bibsearchComment
  HiLink bibsearchQuote		bibQuote        
  HiLink bibsearchBrace		bibBrace        
  HiLink bibsearchParen		bibParen        
  delcommand HiLink
endif
let b:current_syntax = "bibsearch"
syntax/toc_atp.vim	[[[1
31
" Vim syntax file
" Language:	toc_atp
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Feb 7
" URL:		

syntax match  atp_filename /^\s*\D.*$/
syntax match  atp_linenumber /^\s*\d\+/ skipwhite nextgroup=atp_number,atp_abstract
syntax match  atp_number /\t\%(\d\+\.\?\)\+/ms=b+1,me=e contained nextgroup=atp_sectiontitle,atp_subsectiontitle 

syntax match atp_abstract /\t\+\s\s\(\S\&\D\).*$/ 

syntax match  atp_chapter /^\s*\d\+\t\+\d\+\s.*/ contains=atp_linenumber,atp_number,atp_chaptertitle
" syntax region atp_chaptertitle matchgroup=atp_chaptertitle start=/\d\s\(\S\&\D\)/ms=e-1 end=/$/me=e contained oneline

syntax match  atp_section /^\s*\d\+\t\+\(\d\+\.\d\+\|\s\{3,}\)\s.\+/ contains=atp_linenumber,atp_number,atp_sectiontitle 
" syntax region atp_sectiontitle matchgroup=atp_sectiontitle start=/\d\s\t\@<!/ms=e+1,ms=e+1 end=/$/me=e contained oneline

syntax match  atp_subsection /^\s*\d\+\t\+\(\d\+\.\d\+\.\d\+\|\s\{5,}\)\s.\+/ contains=atp_linenumber,atp_number,atp_subsectiontitle 
" syntax region atp_subsectiontitle matchgroup=atp_subsectiontitle start=/\d\s\t\@<!/ms=e+1,ms=e+1 end=/$/me=e contained oneline

hi link atp_filename Title
hi link atp_linenumber LineNr
hi link atp_number Number
hi link atp_abstract Label
hi link atp_chapter Label
hi link atp_section Label 
hi link atp_subsection Label
" hi link atp_chaptertitle Title
" hi link atp_sectiontitle Title 
" hi link atp_subsectiontitle Title
syntax/labels_atp.vim	[[[1
15
" Vim syntax file
" Language:	toc_atp
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Feb 7
" URL:		

syntax region atp_label_line start=/^/ end=/$/ transparent contains=atp_label_linenr,atp_label_tab,atp_label_name oneline 
syntax match  atp_label_linenr /^\d\+/ contained nextgroup=atp_label_tab
syntax match  atp_label_tab /\t\+/ contained nextgroup=atp_label_name
syntax region atp_label_name start=/\D\S/ end=/$/ oneline 
syntax match atp_label_filename /^\D.*$/	

hi link atp_label_filename Title
hi link atp_label_linenr LineNr
hi link atp_label_name 	Label
colors/coots-beauty-256.vim	[[[1
204
" Vim color file
" Maintainer:	Marcin Szamotulski  <mszamot at gmail dot com>
" Last Change:	2009 Feb 19
" Version:	1.0.0
" URL:		http://www.axisym3.net/jdany/vim-the-editor/#ocean237256
"
" These are the colors of the "Ocean237" theme by Chris Vertonghen modified
" to work on 256-color xterms.
"
set background=dark

highlight clear
if exists("syntax_on")
    syntax reset
endif

"let g:colors_name = "coot-256"

highlight Normal         cterm=none           ctermfg=250 ctermbg=233	guifg=white	guibg=#1c1c1c
highlight NonText        cterm=none           ctermfg=105 ctermbg=233	guifg=#1c1c1c	guibg=#1c1c1c 

highlight Visual         		      ctermbg=238				guibg=gray35
highlight VisualNOS      cterm=bold,underline ctermfg=57  ctermbg=233

highlight Cursor         cterm=none           ctermfg=15  ctermbg=93	guifg=#000000	guibg=#8A4C98
highlight CursorIM       cterm=bold           ctermfg=15  ctermbg=93	guifg=#000000	guibg=#8A4C98
"highlight CursorColumn
"highlight CursorLine

highlight Directory      			ctermfg=5   ctermbg=233	guifg=DarkViolet	guibg=#1c1c1c

highlight DiffAdd        cterm=none           	ctermfg=15  ctermbg=22
highlight DiffChange     cterm=none           	ctermfg=207 ctermbg=39
highlight DiffDelete     cterm=none           	ctermfg=19  ctermbg=17
highlight DiffText       cterm=bold           	ctermfg=226 ctermbg=39

highlight Question       cterm=bold           	ctermfg=33  ctermbg=233 guifg=#0087ff	guibg=#1c1c1c
highlight ErrorMsg       cterm=bold            	ctermfg=160 ctermbg=233 guifg=#d70000	guibg=#1c1c1c
highlight ModeMsg              			ctermfg=33  ctermbg=233 guifg=#0087ff	guibg=#1c1c1c
highlight MoreMsg        	           	ctermfg=39  ctermbg=233 guifg=#00afff	guibg=#1c1c1c
highlight WarningMsg    cterm=bold           	ctermfg=161 ctermbg=233 guifg=#d7005f	guibg=#1c1c1c

highlight LineNr                              	ctermfg=57 ctermbg=233	guifg=#837598	guibg=#1c1c1c
highlight Folded  				ctermfg=57 ctermbg=233	guifg=#837598	guibg=#1c1c1c
highlight FoldColumn     cterm=none           	ctermfg=green ctermbg=233 guifg=#5CB80C guibg=#1c1c1c
"highlight SignColumn

highlight Search         cterm=bold           	ctermfg=black  	ctermbg=226	guifg=black guibg=yellow
highlight IncSearch      cterm=bold        	ctermfg=black  	ctermbg=red	guifg=gold guibg=#1c1c1c
highlight MatchParen     			ctermfg=233	ctermbg=226	guifg=#1c1c1c guibg=gold

"highlight PMenu
"highlight PMenuSBar
"highlight PMenuSel
"highlight PMenuThumb

highlight SpecialKey     ctermfg=129           	ctermbg=233			guifg=DarkViolet

highlight StatusLine     cterm=none     ctermfg=226 ctermbg=232		 guifg=#111111 guibg=wheat1
highlight StatusLineNC   cterm=none     ctermfg=245 ctermbg=232		 guifg=#111111 guibg=snow4	 
highlight User1		 cterm=bold	ctermfg=226 ctermbg=232	gui=bold guifg=DarkOrange  guibg=#111111	 
highlight User2		 cterm=none	ctermfg=red ctermbg=232		 guifg=Khaki1  guibg=#111111
highlight User3		 cterm=none	ctermfg=226 ctermbg=232		 guifg=Goldenrod2   guibg=#111111
highlight VertSplit      cterm=none     ctermfg=green   ctermbg=233	 guifg=#1c1c1c	  guibg=DeepSkyBlue4
highlight WildMenu       cterm=bold     ctermfg=0   ctermbg=118

highlight Title          cterm=bold           	ctermfg=226 	ctermbg=232

"highlight Menu
"highlight Scrollbar
"highlight Tooltip

"          Syntax         Groups
highlight Comment        cterm=none           	ctermfg=90 ctermbg=233		guifg=Magenta4

highlight Constant       ctermfg=125          	ctermbg=233			guifg=DeepPink3
highlight String         cterm=none           	ctermfg=27   ctermbg=233	guifg=RoyalBlue1
"highlight Character
highlight Number         cterm=none           	ctermfg=161  ctermbg=233	guifg=DeepPink2
highlight Boolean        cterm=none           	ctermfg=161  ctermbg=233	guifg=DeepPink1
"highlight Float

highlight Identifier     		      	ctermfg=39			guifg=DodgerBlue
highlight Function       cterm=none           	ctermfg=51   ctermbg=233	guifg=Turquoise1

highlight Statement      cterm=none           	ctermfg=135			guifg=MediumOrchid
"248
highlight Conditional    cterm=none           	ctermfg=27   ctermbg=233	guifg=SlateBlue2
highlight Repeat         cterm=none           	ctermfg=82   ctermbg=233
"highlight Label
highlight Operator       cterm=none	      	ctermfg=40   ctermbg=233	guifg=Chartreuse1
highlight Keyword        cterm=none           	ctermfg=197  ctermbg=233	guifg=DeepPink1
highlight Exception      cterm=none           	ctermfg=82   ctermbg=233	guifg=Chartreuse1

highlight PreProc        ctermfg=82						guifg=DeepPink1
highlight Include        cterm=none           	ctermfg=130  ctermbg=233
highlight Define         cterm=none           	ctermfg=39   ctermbg=233
highlight Macro          cterm=none           	ctermfg=39   ctermbg=233
highlight PreCondit      cterm=bold           	ctermfg=125  ctermbg=233

"jak mutt odpala vima i \bf,\textrm itd:
highlight Type           cterm=none           	ctermfg=82               	guifg=LawnGreen
highlight StorageClass   cterm=none           	ctermfg=21   ctermbg=233
highlight Structure      cterm=none           	ctermfg=21   ctermbg=233
highlight Typedef        cterm=none           	ctermfg=21   ctermbg=233

" $, $$:
highlight Special        cterm=none	      	ctermfg=93			guifg=BlueViolet
"249
"tex math mode
"highlight SpecialChar
"highlight Tag:
"highlight Delimiter
"highlight SpecialComment
"highlight Debug

highlight Underlined     cterm=underline      	ctermfg=102 ctermbg=233		gui=underline
highlight Ignore         ctermfg=67

"highlight SpellBad       ctermfg=21           	ctermbg=233
"highlight SpellCap       ctermfg=19           	ctermbg=233
"highlight SpellRare      ctermfg=18           	ctermbg=233
"highlight SpellLocal     ctermfg=17           	ctermbg=233

highlight Todo           ctermfg=21           ctermbg=233	guifg=red guibg=#1c1c1c	gui=underline,bold

highlight TabLine	cterm=none	ctermfg=white 	ctermbg=240
highlight TabLineFill 	cterm=none	ctermfg=white 	ctermbg=240
highlight TabLineSel	cterm=bold	ctermfg=white	ctermbg=57
"highlight TabLineSel	cterm=bold	ctermfg=white	ctermbg=197
" \command
highlight texDelimiter			ctermfg=161	ctermbg=233	guifg=MediumVioletRed
" \begin, \end:
highlight texSectionMarker		ctermfg=238	ctermbg=233	guifg=FireBrick		gui=bold
highlight texSection	cterm=bold	ctermfg=242	ctermbg=233	guifg=FireBrick2	gui=bold
" highlight texSectionName						guifg=FireBrick
highlight texDocType			ctermfg=90	ctermbg=233	guifg=DeepPink4
highlight texInputFile			ctermfg=90	ctermbg=233	guifg=DeepPink4
highlight texDocTypeArgs		ctermfg=204	ctermbg=233	guifg=DeepPink2
highlight texInputFileopt		ctermfg=204	ctermbg=233	guifg=DeepPink2
highlight texType			ctermfg=40	ctermbg=233	guifg=green3
highlight texMath			ctermfg=245	ctermbg=233	guifg=DarkKhaki
highlight texStatement 			ctermfg=245	ctermbg=233	guifg=DeepPink3
highlight texString			ctermfg=39	ctermbg=233	guifg=DodgerBlue
highlight texSpecialChar		ctermfg=39	ctermbg=233	guifg=DodgerBlue
highlight texRefZone							guifg=DeepPink2		gui=bold
highlight texCite							guifg=DeepPink4
highlight texRefOption							guifg=HotPink4
" \chapter, \section, ... {theorem} {definition}

highlight Error          ctermfg=196         	ctermbg=233
highlight SpellErrors  	 cterm=underline      	ctermfg=darkred ctermbg=233
highlight SpellBad       ctermfg=196         	ctermbg=233
highlight SpellCap       ctermfg=202         	ctermbg=233
highlight SpellRare      ctermfg=203         	ctermbg=233
highlight SpellLocal     ctermfg=202         	ctermbg=233

hi bibsearchInfo 	ctermfg=33			guifg=DeepPink3
hi bibsearchComment	cterm=bold 	ctermfg=27	guifg=LawnGreen		gui=bold 
hi bibComment2		cterm=bold 	ctermfg=30	guifg=SeaGreen4		gui=bold
hi bibsearchCommentContents cterm=none	ctermfg=30	guifg=SeaGreen4		gui=none
hi bibsearchType			ctermfg=24	guifg=MediumVioletRed
" hi bibsearchEntryData						ctermfg=magenta
hi bibsearchKey		cterm=bold 		ctermfg=white	guifg=white	gui=bold
hi bibsearchEntry 				ctermfg=33	guifg=DeepSkyBlue
    hi bibsearchField 				ctermfg=green	guifg=green1
	hi bibsearchEntryKw			ctermfg=white	guifg=BlueViolet
	hi bibsearchBrace			cterm=bold	guifg=white gui=none
	hi bibsearchVariable 			ctermfg=white	guifg=white

" ATP toc file
highlight atp_filename						guifg=FireBrick
highlight atp_linenumber	cterm=bold	ctermfg=27	guifg=PeachPuff4
highlight atp_number 				ctermfg=33	guifg=sienna
highlight atp_chapter 		cterm=bold 	ctermfg=white	guifg=seashell2		gui=bold
highlight atp_section				ctermfg=30	guifg=seashell4
highlight atp_subsection			ctermfg=24	guifg=seashell4
highlight atp_abstract	cterm=bold	ctermfg=gray		guifg=seashell2		gui=bold

" ATP label file
highlight atp_label_filename					guifg=DeepPink4		gui=bold
highlight atp_label_linenr cterm=bold	ctermfg=white		guifg=maroon
" highlight atp_label_name 		ctermfg=green		guifg=chartreuse
highlight atp_label_name 					guifg=DeepPink2		gui=bold

highlight atp_statusline 	cterm=bold	ctermfg=green 	ctermbg=233

highlight atp_statustitle 	cterm=bold	ctermfg=grey 	ctermbg=233  
highlight atp_statussection 	cterm=bold	ctermfg=yellow 	ctermbg=233  
highlight atp_statusoutdir 			ctermfg=grey 	ctermbg=233 

highlight link atp_Todo Normal

highlight ywtxt_todo	guifg=yellow gui=bold
highlight ywtxt_note	guifg=yellow gui=bold

highlight ywtxt_heading1 guifg=slateblue1 gui=bold 
highlight ywtxt_heading2 guifg=slateblue gui=bold 
highlight ywtxt_heading3 guifg=slateblue4 gui=bold 
highlight ywtxt_heading4 guifg=darkslateblue gui=bold 
highlight ywtxt_bold 		cterm=bold 	gui=bold
highlight ywtxt_italic 		cterm=italic  	gui=italic
highlight ywtxt_underline 	cterm=underline gui=underline
highlight ywtxt_comment guifg=honeydew4
