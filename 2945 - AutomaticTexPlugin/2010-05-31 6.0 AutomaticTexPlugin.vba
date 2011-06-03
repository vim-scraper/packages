" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/tex_atp.vim	[[[1
5575
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 May 31
" URL:		
" Email:	mszamot [AT] gmail [DOT] com
" GetLatestVimScripts: 2945 18 :AutoInstall: tex_atp.vim
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
" Idea: write a diff function wich compares two files ignoring new lines, but
" using the structure of \begin:\end \(:\), etc... .
"
" ToDo: tikz commands showup in ordinary math environment \(:\).
"
" Todo: make small Help functions with list of most important mappings and
" commands for each window (this can help at the begining) make it possible to
" turn it off.
"
" DONE: write commands to help choose nice fonts! (Integrate my bash script).
"
" Todo: update mappings <F6>+w r f (see :h atp-texlog). 
"
" Todo: check completion for previous/next environment with MCNw.tex there are
" some ambiguities.
"
" Done: using a symbolic link, run \v it will use the name of symbolic name
" not the target. Also the name of the xpdfserver is taken from the actually
" opend file (for example input file) and not the target name of the link. 
" Look intfor b:texcommand.
"
" Done: modify EditInputFiles so that it finds file in the b:atp_mainfile
"
" Done: EditInputFile if running from an input file a main file should be
" added. Or there should be a function to come back.
"
" Done: make a function which list all definitions
"
" TODO: bibtex is not processing right (after tex+bibtex+tex+tex, +\l gives
" the citation numbers)
"
" Done: b:atp_mainfile is not working with b:outdir, (b:outdir should not be
" changed for input files)
"
" TODO: to make s:maketoc and s:generatelabels read all input files between
" \begin{document} and \end{document}, and make it recursive.
" now s:maketoc finds only labels of chapters/sections/...
" TODO: make toc work with parts!
"
" Comment: The time consuming part of TOC command is: opening new window
" ('vnew') as shown by profiling.
"
" TODO: Check against lilypond 
"
" Done: make a split version of EditInputFile
"
" Done: for input files which filetype=plaintex (for example hyphenation
" files), the variable b:autex is not set. Just added plaintex_atp.vim file
" which sources tex_atp.vim file.  
"
" NOTES
" s:tmpfile =	temporary file value of tempname()
" b:texfile =	readfile(bunfname("%")

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

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
" TODO set define and work on the above settings, these settings work with [i
" command but not with [d, [D and [+CTRL D (jump to first macro definition)

"------------ append / at the end of a directory name ------------
fun! s:append(where,what)
    return substitute(a:where,a:what . "\s*$",'','') . a:what
endfun
" ----------------- FindInputFiles ---------------
" it should return in the values of the dictionary the name of the file that
" FindInputFile([bufname],[echo])

" ToDo: this function should have a mode to find input files recursively.
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
    if buflisted(fnamemodify(l:bufname,":t"))
	let l:texfile=getbufline(fnamemodify(l:bufname,":t"),1,'$')
    else
	let l:texfile=readfile(fnamemodify(l:bufname,":p"))
    endif
    let b:texfile=l:texfile
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
	    let l:eidx=len(l:line)-stridx(join(reverse(split(l:line,'\zs')),''),'}')-1
	    let l:inputfile=strpart(l:line,l:bidx+1,l:eidx-l:bidx-1)
	    call extend(l:inputfiles, { l:inputfile : [ 'include' , fnamemodify(expand("%"),":p") ] } )
	endif
    endfor
    call extend(l:inputfiles,FindBibFiles(l:bufname))
    " this function is used to set b:atp_mainfile, but at this stage there is no
    " need to add b:atp_mainfile to the list of input files (this is also
    " a requirement for the function s:setprojectname.
    if exists("b:atp_mainfile")
	call extend(l:inputfiles, { fnamemodify(b:atp_mainfile,":t") : ['main file', b:atp_mainfile]}, "error") 
    endif
    let l:inputfiless=deepcopy(l:inputfiles)
    call filter(l:inputfiless, 'v:key !~ fnamemodify(bufname("%"),":t:r")')
    if l:echo 
	if len(keys(l:inputfiless)) > 0 
	    echohl WarningMsg | echomsg "Found input files:" 
	else
	    echohl WarningMsg | echomsg "No input files found." | echohl None
	    return []
	endif
	echohl texInput
	let l:nr=1
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'main file'
		echomsg fnamemodify(l:inputfile,":t") 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'input'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'include'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	for l:inputfile in keys(l:inputfiless)
	    if l:inputfiless[l:inputfile][0] == 'bib'
		echomsg substitute(l:inputfile,'^\s*\"\|\"\s*$','','g') 
		let l:nr+=1
	    endif
	endfor
	echohl None
    endif
    let s:inputfiles=l:inputfiles
    return l:inputfiles
endfunction
endif
" ----------------- FIND BIB FILES ----------------------------------	
"
" this function is for complition of \bibliography and \input commands it returns a list
" of all files under a:dir and in g:outdir with a given extension.
function! s:Find_files(dir,in_current_dir,ext)
	let l:raw_files=split(globpath(s:append(a:dir,'/'),'**'))
	if a:in_current_dir
	    call extend(l:raw_files,split(globpath(b:outdir,'*')))
	endif
" 	let b:raw=l:raw_files " DEBUG
	let l:file_list=[]
	for l:key in l:raw_files
	    if l:key =~ a:ext . '$'
		call add(l:file_list,l:key)
	    endif
	endfor
	return l:file_list
endfunction

if !exists("*FindBibFiles")
function! FindBibFiles(...)

    if a:0==0
	let l:bufname=bufname("%")
    else
	let l:bufname=a:1
    endif

"     let b:texfile=readfile(l:bufname)
    if buflisted(fnamemodify(l:bufname,":p"))
	let b:texfile=getbufline(l:bufname,1,'$')
    else
	let b:texfile=readfile(fnameescape(fnamemodify(l:bufname,":p")))
    endif
    let s:i=0
    let s:bibline=[]
    " find all lines which define bibliography files
    for line in b:texfile
	" ToDo: %\bibliography should not be matched!
	if line =~ "^[^%]*\\\\bibliography{"
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
    let b:abf=deepcopy(l:allbibfiles)

    " this variable will store unreadable bibfiles:    
    let s:notreadablebibfiles=[]

    " this variable will store the final result:   
"     let l:bibfiles={}
    let l:bibfiles_dict={}
    let b:bibfiles_dict=l:bibfiles_dict

    " Make a list of all bib files which tex can find.
    let l:bibfiles_list=[]
    let b:bibfiles_list=l:bibfiles_list " DEBUG
    for l:dir in g:atp_bibinputs
	let l:bibfiles_list=extend(l:bibfiles_list,s:Find_files(l:dir,0,".bib"))
    endfor

    for l:f in l:allbibfiles
	" ToDo: change this to find in any directory under g:atp_bibinputs. 
	" also change in the line 1406 ( s:searchbib )
	for l:bibfile in l:bibfiles_list
	    if count(l:allbibfiles,fnamemodify(l:bibfile,":t:r"))
		if filereadable(l:bibfile) 
		call extend(l:bibfiles_dict, 
		    \ {fnamemodify(l:bibfile,":t:r") : [ 'bib' , fnamemodify(expand("%"),":p"), l:bibfile ] })
		else
		" echo warning if a bibfile is not readable
		    echohl WarningMsg | echomsg "Bibfile " . l:f . ".bib not found." | echohl None
		    if count(s:notreadablebibfiles,fnamemodify(l:f,":t:r")) == 0 
			call add(s:notreadablebibfiles,fnamemodify(l:f,":t:r"))
		    endif
		endif
	    endif
	endfor
    endfor

    " return the list  of readable bibfiles
    return l:bibfiles_dict
endfunction
endif
"--------------------SHOW ALL DEFINITIONS----------------------------

" make a dictionary: { input_file : [[beginning_line,end_line],...] }
" if a:1 is given it is the name of the buffer in which to search for input
" files.
function! s:make_defi_dict(...)

    if a:0 > 0
	let l:bufname=a:1
    else
	let l:bufname=bufname("%")
    endif

    " pattern to match the definitions this function is also used to fine
    " \newtheorem, and \newenvironment commands  
    if a:0 > 1	
	let l:pattern = a:2
    else
	let l:pattern = '\\def\|\\newcommand'
    endif

    let l:defi_dict={}

    let l:inputfiles=FindInputFiles(l:bufname,"0")
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

    let l:input_files=filter(l:input_files, 'v:val != ""')
    call extend(l:input_files,[ b:atp_mainfile ])

    if len(l:input_files) > 0
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
	    if substitute(l:line,'%.*','','') =~ l:pattern

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
    endif

    return l:defi_dict
endfunction

if !exists("*DefiSearch")
function! DefiSearch(...)

    if a:0 == 0
	let l:pattern=''
    else
	let l:pattern='\C' . a:1
    endif

    let l:ddict=s:make_defi_dict()
"     let b:dd=l:ddict

    " open new buffer
    let l:openbuffer=" +setl\\ buftype=nofile\\ nospell " . fnameescape("DefiSearch")
    if g:vertical ==1
	let l:openbuffer="vsplit " . l:openbuffer 
    else
	let l:openbuffer="split " . l:openbuffer 
    endif

    if len(l:ddict) > 0
	" wipe out the old buffer and open new one instead
	if bufexists("DefiSearch")
	    exe "silent bw! " . bufnr("DefiSearch") 
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

	if getbufline("DefiSearch",'1','$') == ['']
	    :bw
	    echomsg "Definition not found."
	endif
    else
	echomsg "Definition not found."
    endif
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
    " if the project name was already set do not set it for the second time
    " (which sets then b:atp_mainfile to wrong value!)  
    if &filetype == "fd_atp"
	let b:atp_mainfile=fnamemodify(expand("%"),":p")
	let b:atp_projectname_is_set=1
    endif
    if exists("b:atp_projectname_is_set")
	let b:pn_return.=" exists"
	return b:pn_return
    else
	let b:atp_projectname_is_set=1
    endif

    if !exists("s:inputfiles")
	let s:inputfiles=FindInputFiles(expand("%"),0)
    else
	call extend(s:inputfiles,FindInputFiles(bufname("%"),0))
    endif

    if !exists("g:atp_project")
	" the main file is not an input file (at this stage!)
	if index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) == '-1' &&
	 \ index(keys(s:inputfiles),fnamemodify(bufname("%"),":t"))   == '-1' &&
	 \ index(keys(s:inputfiles),fnamemodify(bufname("%"),":p:r")) == '-1' &&
	 \ index(keys(s:inputfiles),fnamemodify(bufname("%"),":p"))   == '-1' 
	    let b:atp_mainfile=fnamemodify(expand("%"),":p")
	    let b:pn_return="not an input file"
" 	    let b:atp_mainfile=s:append(b:outdir,'/') . expand("%")
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":t")) != '-1'
	    let b:atp_mainfile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":t")][1],":p")
	    let b:pn_return="input file 1"
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1'
	    let b:atp_mainfile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":t:r")][1],":p")
	    let b:pn_return="input file 2"
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":p:r")) != '-1' 
	    let b:atp_mainfile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":p:r")][1],":p")
" 	    let b:pn_return="input file 3"
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":p"))   != '-1' 
	    let b:atp_mainfile=fnamemodify(s:inputfiles[fnamemodify(bufname("%"),":p")][1],":p")
" 	    let b:pn_return="input file 3"
	    if !exists('#CursorHold#' . fnamemodify(bufname("%"),":p"))
		exe "au CursorHold " . fnamemodify(bufname("%"),":p") . " call s:auTeX()"
	    endif
	endif
    elseif exists("g:atp_project")
	let b:atp_mainfile=g:atp_project
	let b:pn_return="set from g:atp_project"
    endif

    " we need to escape white spaces in b:atp_mainfile but not in all places so
    " this is not done here
    return b:pn_return
endfun
" DEBUG
command! SetProjectName	:call s:setprojectname()
" DEBUG
" command! InputFiles 		:echo s:inputfiles

au BufEnter *.tex :call s:setprojectname()
au BufEnter *.fd  :call s:setprojectname()

" let &l:errorfile=b:outdir . fnameescape(fnamemodify(expand("%"),":t:r")) . ".log"
if !exists("*SetErrorFile")
function! SetErrorFile()

    " set b:outdir if it is not set
    if !exists("b:outdir")
	call s:setoutdir(0)
    endif

    " set the b:atp_mainfile varibale if it is not set (the project name)
    if !exists("b:atp_mainfile")
	call s:setprojectname()
    endif

"     let l:ef=b:outdir . fnamemodify(expand("%"),":t:r") . ".log"
    let l:ef=b:outdir . fnamemodify(b:atp_mainfile,":t:r") . ".log"
    let &l:errorfile=l:ef
endfunction
endif

au BufEnter *.tex call SetErrorFile()

" This options are set also when editing .cls files.
function! s:setoutdir(arg)
    " first we have to check if this is not a project file
    if exists("g:atp_project") || exists("s:inputfiles") && 
		\ ( index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1' || 
		\ index(keys(s:inputfiles),fnamemodify(bufname("%"),":t")) != '-1' )
	    " if we are in a project input/include file take the correct value of b:outdir from the s:outdir_dict dictionary.
	    
	    if index(keys(s:inputfiles),fnamemodify(bufname("%"),":t:r")) != '-1'
		let b:outdir=g:outdir_dict[s:inputfiles[fnamemodify(bufname("%"),":t:r")][1]]
	    elseif index(keys(s:inputfiles),fnamemodify(bufname("%"),":t")) != '-1'
		let b:outdir=g:outdir_dict[s:inputfiles[fnamemodify(bufname("%"),":t")][1]]
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

" ________________GLOBAL_AND_LOCAL_VARIABLES________________________

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
elseif !exists("g:rmcommand")
    let g:rmcommand="rm"
endif
if !exists("g:atp_amsmath")
    let g:atp_amsmath=0
endif
if !exists("g:atp_no_math_command_completion")
    let g:atp_no_math_command_completion=0
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
    let g:matchpair="(:),[:],{:}"
endif
if !exists("g:texmf")
    let g:texmf=$HOME . "/texmf"
endif
" a list where tex looks for bib files
if !exists("g:atp_bibinputs")
    let g:atp_bibinputs=split(substitute(substitute(
		\ system("kpsewhich -show-path bib")
		\ ,'\/\/\+','\/','g'),'!\|\n','','g'),':')
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
if !exists("g:atp_completion_limits")
    let g:atp_completion_limits=[40,150,150]
endif
if !exists("g:atp_no_complete")
     let g:atp_no_complete=['document']
endif
" if !exists("g:atp_close_after_last_closed")
"     let g:atp_close_after_last_closed=1
" endif
if !exists("g:atp_no_env_maps")
    let g:atp_no_env_maps=0
endif
if !exists("g:atp_extra_env_maps")
    let g:atp_extra_env_maps=0
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
    redraw!
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
" ToDo: to doc.
" add server call back (then automatically reads errorfiles)
if !exists("g:atp_status_notification")
    let g:atp_status_notification=0
endif
if !exists("b:atp_callback")
    if exists("g:atp_status_notification") && g:atp_status_notification == 1
	let b:atp_callback=1
    else
	let b:atp_callback=0
    endif
endif
let g:atp_running=0
" ToDo: to doc.
" this shows errors as ShowErrors better use of call back mechnism is :copen!
if !exists("g:atp_debug_mode")
    let g:atp_debug_mode=0
endif
if !exists("*ATPRunnig")
function! ATPRunning()
    if g:atp_running && b:atp_callback
	redrawstatus
	return b:texcompiler
    endif
    return ''
endfunction
endif

if !exists("*ATPStatus")
function! ATPStatus()
"     echomsg "Status line set by ATP." 
    if &filetype == 'tex'
	if g:atp_status_notification
	    let &statusline='%<%f %(%h%m%r %)  %{CTOC("return")}%= %{ATPRunning()} %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	else
	    let &statusline='%<%f %(%h%m%r %)  %{CTOC("return")}%= %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	endif 
    else 
	if g:atp_status_notification
	    let  &statusline='%<%f %(%h%m%r %)  %= %{ATPRunning()} %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	else
	    let  &statusline='%<%f %(%h%m%r %)  %= %{ATPStatusOutDir()} %-14.16(%l,%c%V%)%P'
	endif
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

    let l:link=system("readlink " . shellescape(b:atp_mainfile))
    if l:link != ""
	let l:outfile=fnamemodify(l:link,":r") . l:ext
    else
	let l:outfile=fnamemodify(b:atp_mainfile,":r"). l:ext 
    endif
    let b:outfile=l:outfile
    if b:Viewer == "xpdf"	
	let l:viewer=b:Viewer . " -remote " . shellescape(b:XpdfServer) . " " . b:ViewerOptions 
    else
	let l:viewer=b:Viewer  . " " . b:ViewerOptions
    endif
    let l:view=l:viewer . " " . shellescape(l:outfile)  . " &"
		let b:outfile=l:outfile
		let b:viewcommand=l:view " DEBUG
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
	call s:compiler(0,1,1,0,"AU",b:atp_mainfile)
    endif	
endfunction
endif
"-------------------------------------------------------------------------
function! s:getpid()
	let s:command="ps -ef | grep -v " . $SHELL  . " | grep " . b:texcompiler . " | grep -v grep | grep " . fnameescape(expand("%")) . " | awk 'BEGIN {ORS=\" \"} {print $2}'" 
	let s:var=system(s:command)
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
    let s:checkxpdf="ps -ef | grep -v grep | grep xpdf | grep '-remote '" . shellescape(b:XpdfServer) . " | awk '{print $2}'"
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

" this variable =1 if s:complier was called and tex has not finished.
" let g:atp_running=0
" This is the MAIN FUNCTION which sets the command and calls it.
" NOTE: the filename argument is not escaped!
function! s:compiler(bibtex,start,runs,verbose,command,filename)
    if has('clientserver')
	let g:atp_running=1
    endif
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

	let s:tmpdir=tempname()
	let s:tmpfile=s:append(s:tmpdir,"/") . fnamemodify(a:filename,":t:r")
" 	let b:tmpdir=s:tmpdir " DEBUG
	if exists("*mkdir")
	    call mkdir(s:tmpdir, "p", 0700)
	else
	    echoerr 'Your vim doesn't have mkdir function, there is a workaround this though. 
			\ Send an email to the author: mszamot@gmail.com '
	endif

	" SET THE NAME OF OUTPUT FILES
	" first set the extension pdf/dvi
	if b:texcompiler == "pdftex" || b:texcompiler == "pdflatex"
	    let l:ext = ".pdf"
	else
	    let l:ext = ".dvi"	
	endif

	" check if the file is a symbolic link, if it is then use the target
	" name.
	let l:link=system("readlink " . a:filename)
	if l:link != ""
	    let l:basename=fnamemodify(l:link,":r")
	else
	    let l:basename=a:filename
	endif

	" finaly, set the the output file names. 
	let l:outfile = b:outdir . fnamemodify(l:basename,":t:r") . l:ext
	let l:outaux  = b:outdir . fnamemodify(l:basename,":t:r") . ".aux"
	let l:outlog  = b:outdir . fnamemodify(l:basename,":t:r") . ".log"

"	COPY IMPORTANT FILES TO TEMP DIRECTORY WITH CORRECT NAME 
	let l:list=filter(copy(g:keep),'v:val != "log"')
	for l:i in l:list
	    let l:ftc=b:outdir . fnamemodify(l:basename,":t:r") . "." . l:i
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
"	Check: THIS DO NOT WORKS!!!
	if a:start == 1 && b:Viewer == "xpdf"
	    let s:start = b:Viewer . " -remote " . shellescape(b:XpdfServer) . " " . b:ViewerOptions . " & "
	else
	    let s:start = ""	
	endif

"	SET THE COMMAND 
	let s:comp=b:texcompiler . " " . b:texoptions . " -interaction " . s:texinteraction . " -output-directory " . s:tmpdir . " " . fnameescape(a:filename)
	let s:vcomp=b:texcompiler . " " . b:texoptions  . " -interaction errorstopmode -output-directory " . s:tmpdir .  " " . fnameescape(a:filename)
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
	endif
	
	" add g:atp_running
	if has('clientserver') && v:servername != "" && b:atp_callback == 1
	    let s:texcomp = s:texcomp . ' ; vim --servername ' . v:servername . 
			\ ' --remote-send "<ESC>:let g:atp_running=0<CR>"'
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
	let s:cpoutfile="cp " . s:cpoption . shellescape(s:append(s:tmpdir,"/")) . "*" . l:ext . " " . shellescape(s:append(b:outdir,"/")) 
	let s:command="(" . s:texcomp . " && (" . s:cpoutfile . " ; " . s:xpdfreload . ") || (" . s:cpoutfile . ")" 
	let s:copy=""
	let l:j=1
	for l:i in g:keep 
" 	    ToDo: this can be don using internal vim functions.
	    let s:copycmd=" cp " . s:cpoption . " " . shellescape(s:append(s:tmpdir,"/")) . 
			\ "*." . l:i . " " . shellescape(s:append(b:outdir,"/"))  
" 	    let b:copycmd=s:copycmd " DEBUG
	    if l:j == 1
		let s:copy=s:copycmd
	    else
		let s:copy=s:copy . " ; " . s:copycmd	  
	    endif
	    let l:j+=1
	endfor
	    let s:command=s:command . " ; " . s:copy
	if has('clientserver') && v:servername != "" && b:atp_callback == 1
" 	    let s:command = s:command . ' ; vim --servername ' . v:servername . 
" 			\ ' --remote-send "<ESC>echomsg &errorfile<CR>"'
	    let s:command = s:command . ' ; vim --servername ' . v:servername . 
			\ ' --remote-send "<ESC>:cg<CR>"'
" 	    if  g:atp_debug_mode == 1
" 		let s:command = s:command . ' ; vim --servername ' . v:servername . 
" 			    \ ' --remote-send "<ESC>:ShowErrors<CR>"'
" 	    endif
	endif
 	let s:rmtmp="rm -r " . s:tmpdir 
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
	silent! w
	if a:command == "AU"  
	    let &l:backup=s:backup 
	    let &l:writebackup=s:writebackup 
	endif
	if a:verbose == 0
	    call system(s:command)
	else
" 	    let s:command="!clear;" . s:texcomp . " ; " . s:cpoutfile . " ; " . s:copy . " ; " . s:rmtmp
	    let s:command="!clear;" . s:texcomp . " ; " . s:cpoutfile . " ; " . s:copy 
	    exe s:command
	endif
	let b:texomp=s:texcomp
endfunction
"-------------------------------------------------------------------------
function! s:auTeX()
   if b:autex
	" if the file (or input file is modified) compile the document 
	if filereadable(expand("%"))
	    if s:compare(readfile(expand("%")))
		call s:compiler(0,0,b:auruns,0,"AU",b:atp_mainfile)
		redraw
	    endif
	else
	    call s:compiler(0,0,b:auruns,0,"AU",b:atp_mainfile)
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
    call s:compiler(0,0,a:1,0,"COM",b:atp_mainfile)
elseif a:0 == 0
    call s:compiler(0,0,1,0,"COM",b:atp_mainfile)
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
    call s:compiler(0,0,a:1,1,"COM",b:atp_mainfile)
else
    call s:compiler(0,0,1,1,"COM",b:atp_mainfile)
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
	call s:compiler(1,0,0,0,"COM",b:atp_mainfile)
    else
"  	    echomsg "DEBUG Bibtex verbose"
	call s:compiler(1,0,0,1,"COM",b:atp_mainfile)
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

if !exists("*PdfFonts")
function! PdfFonts()
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

fun! YesNoCompletion(A,P,L)
    return ['yes','no']
endfun
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
    let b:pfile=l:pfile

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
	let l:server='localhost'
    endif
    let b:server=l:server
    if l:server =~ 'localhost'
	if g:printingoptions != "" || (a:0 >= 2 && a:2 != "")
	    if a:0 < 2
		let l:message=g:printingoptions
	    else
		let l:message=a:2
	    endif
	    " TODO: write completion :).
	    let l:ok = confirm("Are the printing options set right?\n".l:message,"&Yes\n&No\n&Cancel")
	    if l:ok == "1" 
		if a:0 <= 1
		    let l:printingoptions=g:printingoptions
		else
		    let l:printingoptions=a:2
		endif
	    elseif l:ok == "2"
		let l:printingoptions=input("Give printing options ")
	    elseif l:ok == "3"
		return 0
	    endif
	else
	    let l:printingoptions=""
	endif
	if a:0 == 0 || (a:0 != 0 && a:1 == 'default')
	    let l:com=l:lprcommand . " " . l:printingoptions . " " .  fnameescape(l:pfile)
	else
	    let l:com=l:lprcommand . " " . l:printingoptions . " -P " . a:1 . " " . fnameescape(l:pfile) 
	endif
	redraw!
	echomsg "Printing ...  " . l:com
" 	let b:com=l:com " DEBUG
	call system(l:com)
    " print over ssh on the server g:atp_ssh with the printer a:1 (or the
    " default system printer if a:0 == 0
    else 
	if a:0 == 0 || (a:0 != 0 && a:1 =~ 'default')
	    let l:com="cat " . fnameescape(l:pfile) . " | ssh " . g:atp_ssh . " " . l:lprcommand
	else
	    let l:com="cat " . fnameescape(l:pfile) . " | ssh " . g:atp_ssh . " " . l:lprcommand . " -P " . a:1 
	endif
	if g:printingoptions != "" || (a:0 >= 2 && a:2 != "")
	    if a:0 < 2
		let l:message=g:printingoptions
	    else
		let l:message=a:2
	    endif
	    " TODO: write completion :).
	    let l:ok = confirm("Are the printing options set right?\n".l:message,"&Yes\n&No\n&Cancel")
	    if l:ok == "1" 
		if a:0 <= 1
		    let l:printingoptions=g:printingoptions
		else
		    let l:printingoptions=a:2
		endif
	    elseif l:ok == "2"
		let l:printingoptions=input("Give printing options ")
	    elseif l:ok == "3"
		return 0
	    endif
	else
	    let l:printingoptions=""
	endif
	let l:com = l:com . " " . l:printingoptions
	redraw!
	echomsg "Printing ...  " . l:com
	call system(l:com)
    endif
endfunction
endif

fun! Lpstat()
    if exists("g:apt_ssh") 
	let l:server=strpart(g:atp_ssh,stridx(g:atp_ssh,"@")+1)
    else
	let l:server='locahost'
    endif
    if l:server == 'localhost'
	echo system("lpstat -l")
    else
	echo system("ssh " . g:atp_ssh . " lpstat -l ")
    endif
endfunction

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
" there are two methods keyword is a string to find (a:1=0)or a pattern to
" match, the pattern used to is a:keyword\zs.* to find the place where to cut.
function! s:count(line,keyword,...)
   
    if a:0 == 0 || a:1 == 0
	let l:method=0
    elseif a:1 == 1
	let l:method=1
    endif

    let l:line=a:line
    let l:i=0  
    if l:method==0
	while stridx(l:line,a:keyword) != '-1'
" 		if stridx(l:line,a:keyword) !='-1' 
	    let l:line=strpart(l:line,stridx(l:line,a:keyword)+1)
" 		endif
	    let l:i+=1
	endwhile
    elseif l:method==1
	let l:line=escape(l:line,'\\')
" 	let b:line=l:line " DEBUG
	while match(l:line,a:keyword . '\zs.*') != '-1'
	    let l:line=strpart(l:line,match(l:line,a:keyword . '\zs.*'))
	    let l:i+=1
	endwhile
    endif
    return l:i
endfunction
" DEBUG:
command -buffer -nargs=* Count :echo s:count(<args>)

let g:bibentries=['article', 'book', 'booklet', 'conference', 'inbook', 'incollection', 'inproceedings', 'manual', 'mastertheosis', 'misc', 'phdthesis', 'proceedings', 'techreport', 'unpublished']


"--------------------- SEARCH ENGINE ------------------------------ 
" ToDo should not search in comment lines.

" To make it work afet kpsewhich is searching for bib path.
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
    let b:bibdict={}	" DEBUG
    for l:f in s:bibfiles
	let s:bibdict[l:f]=[]

	" read the bibfile if it is in b:outdir or in g:atp_bibinputs directory
	" ToDo: change this to look in directories under g:atp_bibinputs. 
	" (see also ToDo in FindBibFiles 284)
	for l:path in g:atp_bibinputs 
	    " it might be problem when there are multiple libraries with the
	    " same name under different locations (only the last one will
	    " survive)
	    let s:bibdict[l:f]=readfile(fnameescape(findfile(s:append(l:f,'.bib'),s:append(l:path,"/") . "**")))
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
    let g:bibresults=l:bibresults
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
" 	let t:numberdict=l:numberdict	"DEBUG
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
" TO DO:
" 	if  exists(g:tex_flavor)
" 	    if g:tex_flavor != "latex"
" 		echomsg "CTOC: Wrong 'filetype'. This function works only for latex documents."
" 	    endif
" 	endif
	" Set the status line once more, to remove the CTOC() function.
	call ATPStatus()
	return []
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
	let l:return=['Preambule']
	return l:return
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
" 	echo " DEBUG CTOC " . join(l:names)
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
" 		if a:0 == '0'
" 		    echo "XXX" . l:chapter_name . "/" . l:section_name 
" 		else
		if a:0 != 0
		    return substitute(strpart(l:chapter_name,0,b:truncate_status_section/2), '\_s*$', '','') . "/" . substitute(strpart(l:section_name,0,b:truncate_status_section/2), '\_s*$', '','')
		endif
	    else
" 		if a:0 == '0'
" 		    echo "XXX" . l:chapter_name
" 		else
		if a:0 != 0
		    return substitute(strpart(l:chapter_name,0,b:truncate_status_section), '\_s*$', '','')
		endif
	    endif

	elseif l:chapter_name == "" && l:section_name != ""
	    if l:subsection_name != ""
" 		if a:0 == '0'
" 		    echo "XXX" . l:section_name . "/" . l:subsection_name 
" 		else
		if a:0 != 0
		    return substitute(strpart(l:section_name,0,b:truncate_status_section/2), '\_s*$', '','') . "/" . substitute(strpart(l:subsection_name,0,b:truncate_status_section/2), '\_s*$', '','')
		endif
	    else
" 		if a:0 == '0'
" 		    echo "XXX" . l:section_name
" 		else
		if a:0 != 0
		    return substitute(strpart(l:section_name,0,b:truncate_status_section), '\_s*$', '','')
		endif
	    endif

	elseif l:chapter_name == "" && l:section_name == "" && l:subsection_name != ""
" 	    if a:0 == '0'
" 		echo "XXX" . l:subsection_name
" 	    else
	    if a:0 != 0
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

    let l:mainfile=b:atp_mainfile

    if a:0 == 0
	let l:inputfile=""
	let l:bufname=b:atp_mainfile
	let l:opencom="edit"
    elseif a:0 == 1
	let l:inputfile=a:1
	let l:bufname=b:atp_mainfile
	let l:opencom="edit"
    else
	let l:inputfile=a:1
	let l:opencom=a:2

	" the last argument is the bufername in which search for the input files 
	if a:0 > 2
	    let l:bufname=a:3
	else
	    let l:bufname=b:atp_mainfile
	endif
    endif

    let l:dir=fnamemodify(b:atp_mainfile,":p:h")

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

    "if the choosen file is the main file put the whole path.
"     if l:ifile == fnamemodify(b:atp_mainfile,":t")
" 	let l:ifile=b:atp_mainfile
"     endif

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
    elseif l:inputfiles[l:ifile][0] == 'bib'
	let l:ifilename=s:append(l:ifile,'.bib')
    elseif  l:inputfiles[l:ifile][0] == 'main file'
	let l:ifilename=b:atp_mainfile
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
		let b:atp_mainfile=l:mainfile
	    elseif l:inputfiles[l:ifile][0] == 'bib' 
		let s:ft=&filetype
		exe l:opencom . " " . l:inputfiles[l:ifile][2]
		let &l:filetype=s:ft
		let b:atp_mainfile=l:mainfile
	    elseif  l:inputfiles[l:ifile][0] == 'main file' 
		exe l:opencom . " " . b:atp_mainfile
		let b:atp_mainfile=l:mainfile
	    endif
	endif
    else
	exe l:opencom . " " . fnameescape(l:ifilename)
	let b:atp_mainfile=l:mainfile
    endif
endfunction
endif

if !exists("*EI_compl")
fun! EI_compl(A,P,L)
"     let l:inputfiles=FindInputFiles(bufname("%"),1)

    let l:inputfiles=filter(FindInputFiles(b:atp_mainfile,1), 'v:key !~ fnamemodify(bufname("%"),":t:r")')
    " rewrite the keys of FindInputFiles the order: input files, bibfiles
    let l:oif=[]
    for l:key in keys(l:inputfiles)
	if l:inputfiles[l:key][0] == 'main file'
	    call add(l:oif,fnamemodify(l:key,":t"))
	endif
    endfor
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
"     echomsg "DEBUG " . s:a " at line " . a:line

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
" w	- all warning messages
" c	- citasion warning messages
" r	- reference warning messages
" f	- font warning messages
" fi	- font warning and info messages
" F	- files
" p	- package info messages
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
	    let &l:errorformat='%WLaTeX\ %tarning:\ %m\ on\ input\ line\ %l%.,
			\%WLaTeX\ %.%#Warning:\ %m,
	    		\%Z(Font) %m\ on\ input\ line\ %l%.,
			\%+W%.%#\ at\ lines\ %l--%*\\d'
	else
	    let &l:errorformat= &l:errorformat . ',%WLaTeX\ %tarning:\ %m\ on\ input\ line\ %l%.,
			\%WLaTeX\ %.%#Warning:\ %m,
	    		\%Z(Font) %m\ on\ input\ line\ %l%.,
			\%+W%.%#\ at\ lines\ %l--%*\\d'
" 	    let &l:errorformat= &l:errorformat . ',%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,
" 			\%WLaTeX\ %.%#Warning:\ %m,
" 			\%+W%.%#\ at\ lines\ %l--%*\\d'
	endif
    endif
    if a:0>0 && a:1 =~ '\Cc'
" NOTE:
" I would like to include 'Reference/Citation' as an error message (into %m)
" but not include the 'LaTeX Warning:'. I don't see how to do that actually. 
" The only solution, that I'm aware of, is to include the whole line using
" '%+W' but then the error messages are long and thus not readable.
	if &l:errorformat == ""
	    let &l:errorformat = "%WLaTeX\ Warning:\ Citation\ %m\ on\ input\ line\ %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%WLaTeX\ Warning:\ Citation\ %m\ on\ input\ line\ %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ '\Cr'
	if &l:errorformat == ""
	    let &l:errorformat = "%WLaTeX\ Warning:\ Reference %m on\ input\ line\ %l%.%#,%WLaTeX\ %.%#Warning:\ Reference %m,%C %m on input line %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%WLaTeX\ Warning:\ Reference %m on\ input\ line\ %l%.%#,%WLaTeX\ %.%#Warning:\ Reference %m,%C %m on input line %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ '\Cf'
	if &l:errorformat == ""
	    let &l:errorformat = "%WLaTeX\ Font\ Warning:\ %m,%Z(Font) %m on input line %l%.%#"
	else
	    let &l:errorformat = &l:errorformat . ",%WLaTeX\ Font\ Warning:\ %m,%Z(Font) %m on input line %l%.%#"
	endif
    endif
    if a:0>0 && a:1 =~ '\Cfi'
	if &l:errorformat == ""
	    let &l:errorformat = '%ILatex\ Font\ Info:\ %m on input line %l%.%#,
			\%ILatex\ Font\ Info:\ %m,
			\%Z(Font) %m\ on input line %l%.%#,
			\%C\ %m on input line %l%.%#'
	else
	    let &l:errorformat = &l:errorformat . ',%ILatex\ Font\ Info:\ %m on input line %l%.%#,
			\%ILatex\ Font\ Info:\ %m,
			\%Z(Font) %m\ on input line %l%.%#,
			\%C\ %m on input line %l%.%#'
	endif
    endif
    if a:0>0 && a:1 =~ '\CF'
	if &l:errorformat == ""
	    let &l:errorformat = 'File: %m'
	else
	    let &l:errorformat = &l:errorformat . ',File: %m'
	endif
    endif
    if a:0>0 && a:1 =~ '\Cp'
	if &l:errorformat == ""
	    let &l:errorformat = 'Package: %m'
	else
	    let &l:errorformat = &l:errorformat . ',Package: %m'
	endif
    endif
    if &l:errorformat != "" && &l:errorformat !~ "fi"
	let &l:errorformat = &l:errorformat . ",%Cl.%l\ %m,
			    \%+C\ \ %m%.%#,
			    \%+C%.%#-%.%#,
			    \%+C%.%#[]%.%#,
			    \%+C[]%.%#,
			    \%+C%.%#%[{}\\]%.%#,
			    \%+C<%.%#>%.%#,
			    \%+C%m,
			    \%-GSee\ the\ LaTeX%m,
			    \%-GType\ \ H\ <return>%m,
			    \%-G\ ...%.%#,
			    \%-G%.%#\ (C)\ %.%#,
			    \%-G(see\ the\ transcript%.%#),
			    \%-G\\s%#,
			    \%+O(%*[^()])%r,
			    \%+O%*[^()](%*[^()])%r"
" this defines wrong file name and I think this is not that important in TeX. 			    
" 			    \%+P(%f%r,
" 			    \%+P\ %\\=(%f%r,
" 			    \%+P%*[^()](%f%r,
" 			    \%+P[%\\d%[^()]%#(%f%r,
" 			    \%+Q)%r,
" 			    \%+Q%*[^()])%r,
" 			    \%+Q[%\\d%*[^()])%r"
    endif
endfunction

function! s:ShowErrors(...)

    " read the log file and merge warning lines 
    if !filereadable(&errorfile)
	echohl WarningMsg
	echomsg "No error file: " . &errorfile  
	echohl Normal
	return
    endif
    let l:log=readfile(&errorfile)
    let l:nr=1
    for l:line in l:log
	if l:line =~ "LaTeX Warning:" && l:log[l:nr] !~ "^$" 
	    let l:newline=l:line . l:log[l:nr]
	    let l:log[l:nr-1]=l:newline
	    call remove(l:log,l:nr)
	endif
	let l:nr+=1
    endfor
    call writefile(l:log,&errorfile)
    
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
	return "e\nw\nc\nr\ncr\nf\nfi\nF"
endfunction
endif
"--------- Set Viewers  ------------------------------------------------------
"
fun! SetXdvi()
    let b:texcompiler="latex"
    let b:texoptions="-src-specials"
    if exists("g:xdviOptions")
	let b:ViewerOptions=g:xdviOptions
    endif
    let b:Viewer="xdvi " . b:ViewerOptions . " -editor 'gvim --servername " . v:servername . " --remote-wait +%l %f'"
    if !exists("*RevSearch")
    function RevSearch()
	let b:xdvi_reverse_search="xdvi " . b:ViewerOptions . 
		\ " -editor 'gvim --servername " . v:servername . 
		\ " --remote-wait +%l %f' -sourceposition " . 
		\ line(".") . ":" . col(".") . fnamemodify(expand("%"),":p") . 
		\ " " . fnamemodify(expand("%"),":p:r") . ".dvi"
	call system(b:xdvi_reverse_search)
    endfunction
    endif
    command! -buffer RevSearch 					:call RevSearch()
    map <buffer> <LocalLeader>rs				:call RevSearch()<CR>
    nmenu 550.65 &LaTeX.Reverse\ Search<Tab>:map\ <LocalLeader>rs	:RevSearch<CR>
endfun

fun! SetXpdf()
    let b:texcompiler="pdflatex"
    let b:texoptions=""
    let b:Viewer="xpdf"
    if exists("g:xpdfOptions")
	let b:ViewerOptions=g:xpdfOptions
    else
	let b:ViewerOptions=''
    endif
    if hasmapto("RevSearch()",'n')
	unmap <buffer> <LocalLeader>rs
    endif
    if exists("RevSearch")
	delcommand RevSearch
    endif
    if exists("RevSearch")
	delcommand RevSearch
    endif
    aunmenu LaTeX.Reverse\ Search
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

" Move to next environment which name is given as the argument. Do not wrap
" around the end of the file.
function! NextEnv(envname)
    call search('\\begin{' . a:envname . '}','W')
endfunction

function! PrevEnv(envname)
    call search('\\begin{' . a:envname . '}','bW')
endfunction

" Move to next section, the extra argument is a pattern to match for the
" section title. The first, obsolete argument stands for:
" part,chapter,section,subsection,etc.
" This commands wrap around the end of the file.
function! NextSection(secname,...)
    if a:0==0
	call search('\\' . a:secname . '\>','w')
    else
	call search('\\' . a:secname . '\>' . '\s*{.*' . a:1,'w') 
    endif
endfunction
function! PrevSection(secname,...)
    if a:0==0
	call search('\\' . a:secname . '\>','bw')
    else
	call search('\\' . a:secname . '\>' . '\s*{.*' . a:1,'bw') 
    endif
endfunction

function! Env_compl(A,P,L)
    let l:envlist=sort(['abstract', 'definition', 'equation', 'proposition', 
		\ 'theorem', 'lemma', 'array', 'tikzpicture', 
		\ 'tabular', 'table', 'align\*\?', 'alignat\*\?', 'proof', 
		\ 'corollary', 'enumerate', 'examples\?', 'itemize', 'remark', 
		\ 'notation', 'center', 'quotation', 'quote', 'tabbing', 
		\ 'picture', 'minipage', 'list', 'flushright', 'flushleft', 
		\ 'figure', 'eqnarray', 'thebibliography', 'titlepage', 
		\ 'verbatim', 'verse' ])
    let l:returnlist=[]
    for l:env in l:envlist
	if l:env =~ '^' . a:A 
	    call add(l:returnlist,l:env)
	endif
    endfor
    return l:returnlist
endfunction

"--------- Special Space for Searching  ----------------------------------
let s:special_space="[off]"
" if !exists("*ToggleSpace")
function! ToggleSpace()
    if maparg('<space>','c') == ""
	echomsg "special space is on"
	cmap <Space> \_s\+
	let s:special_space="[on]"
	aunmenu LaTeX.Toggle\ Space\ [off]
	nmenu 550.78 &LaTeX.&Toggle\ Space\ [on]	:ToggleSpace<CR>
	tmenu &LaTeX.&Toggle\ Space\ [on] cmap <space> \_s\+ is curently on
    else
	echomsg "special space is off"
 	cunmap <Space>
	let s:special_space="[off]"
	aunmenu LaTeX.Toggle\ Space\ [on]
	nmenu 550.78 &LaTeX.&Toggle\ Space\ [off]	:ToggleSpace<CR>
	tmenu &LaTeX.&Toggle\ Space\ [off] cmap <space> \_s\+ is curently off
    endif
endfunction
" endif

function! ToggleCheckMathOpened()
    if g:atp_math_opened
	echomsg "check if in math environment is off"
	aunmenu LaTeX.Toggle\ Check\ if\ in\ Math\ [on]
	nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [off]<Tab>g:atp_math_opened			
		    \ :ToggleCheckMathOpened<CR>
    else
	echomsg "check if in math environment is on"
	aunmenu LaTeX.Toggle\ Check\ if\ in\ Math\ [off]
	nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [on]<Tab>g:atp_math_opened
		    \ :ToggleCheckMathOpened<CR>
    endif
    let g:atp_math_opened=!g:atp_math_opened
endfunction

function! ToggleCallBack()
    if b:atp_callback
	echomsg "call back is off"
	aunmenu LaTeX.Toggle\ Call\ Back\ [on]
	nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [off]<Tab>b:atp_callback	
		    \ :call ToggleCallBack()<CR>
    else
	echomsg "call back is on"
	aunmenu LaTeX.Toggle\ Call\ Back\ [off]
	nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [on]<Tab>b:atp_callback
		    \ :call ToggleCallBack()<CR>
    endif
    let b:atp_callback=!b:atp_callback
endfunction
" ToDo: to doc.
" describe DEBUG MODE in doc properly.
function! ToggleDebugMode()
    call ToggleCallBack()
    if g:atp_debug_mode
	echomsg "debug mode is off"
	aunmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [on]
	nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [off]<Tab>g:atp_debug_mode			
		    \ :call ToggleDebugMode()<CR>
	let g:atp_debug_mode=0
	let g:atp_status_notification=0
	if g:atp_statusline
	    call ATPStatus()
	endif
	silent cclose
    else
	echomsg "debug mode is on"
	aunmenu 550.20.5 LaTeX.Log.Toggle\ Debug\ Mode\ [off]
	nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [on]<Tab>g:atp_debug_mode
		    \ :call ToggleDebugMode()<CR>
	let b:atp_callback=1
	let g:atp_debug_mode=1
	let g:atp_status_notification=1
	if g:atp_statusline
	    call ATPStatus()
	endif
	silent copen
    endif
endfunction
"
"--------- TAB COMPLETION ----------------------------------------------------
"
" This function searches if the package in question is declared or not.
" Returns 1 if it is and 0 if it is not.
" It was inspired by autex function written by Carl Mueller, math at carlm e4ward c o m
function! s:Search_Package(name)
    let l:n=1
    let l:bufnr=bufnr(b:atp_mainfile)
    let l:line=join(getbufline(l:bufnr,l:n))
"     echo "DEBUG SEARCH PACKAGE " . l:line  . " bufnr " . l:bufnr
    let l:len=len(getbufline(l:bufnr,1,'$'))
    while l:line !~ '\\begin\s*{document}' &&  l:n <= l:len
	if l:line =~ '^[^%]*\\usepackage\s*{.*' . a:name
	    return 1
	endif
	let l:n+=1
	let l:line=join(getbufline(l:bufnr,l:n))
    endwhile
    return 0
endfunction
" DEBUG
command! -nargs=1 SearchPackage 	:echo s:Search_Package(<f-args>)

function! s:Document_Class()

    let l:bufnr=bufnr(b:atp_mainfile)

    let l:n=1
    let l:line=join(getbufline(l:bufnr,l:n))

    if l:line =~ '\\documentclass'
" 	let b:line=l:line " DEBUG
	return substitute(l:line,'.*\\documentclass\s*\%(\[.*\]\)\?{\(.*\)}.*','\1','')
    endif
    while l:line !~ '\\documentclass'
	if l:line =~ '\\documentclass'
	    return substitute(l:line,'.*\\documentclass\s*\%(\[.*\]\)\?{\(.*\)}.*','\1','')
	endif
	let l:n+=1
	let l:line=join(getbufline(l:bufnr,l:n))
    endwhile
endfunction

" ToDo: make list of complition commands from the input files.
" ToDo: make complition fot \cite, and for \ref and \eqref commands.

" ToDo: there is second such a list! line 3150
	let g:atp_environments=['array', 'abstract', 'center', 'corollary', 
		\ 'definition', 'document', 
		\ 'enumerate', 'example', 'eqnarray', 
		\ 'flushright', 'flushleft', 'figure', 'frontmatter', 
		\ 'keywords', 
		\ 'itemize', 'lemma', 'list', 'notation', 'minipage', 
		\ 'proof', 'proposition', 'picture', 'theorem', 'tikzpicture',  
		\ 'tabular', 'table', 'tabbing', 'thebibliography', 'titlepage',
		\ 'quotation', 'quote',
		\ 'remark', 'verbatim', 'verse' ]

	let g:atp_amsmath_environments=['align', 'alignat', 'equation', 'gather',
		\ 'multiline', 'split', 'substack', 'flalign', 'smallmatrix', 'subeqations',
		\ 'pmatrix', 'bmatrix', 'Bmatrix', 'vmatrix' ]

	" if short name is no_short_name or '' then both means to do not put
	" anything, also if there is no key it will not get a short name.
	let g:atp_shortname_dict = { 'theorem' : 'thm', 
		    \ 'proposition' : 'prop', 	'definition' : 'defi',
		    \ 'lemma' : 'lem',		'array' : 'ar',
		    \ 'abstract' : 'no_short_name',
		    \ 'tikzpicture' : 'tikz',	'tabular' : 'table',
		    \ 'table' : 'table', 	'proof' : 'pr',
		    \ 'corollary' : 'cor',	'enumerate' : 'enum',
		    \ 'example' : 'ex',		'itemize' : 'it',
		    \ 'item'	: 'itm',
		    \ 'remark' : 'rem',		'notation' : 'not',
		    \ 'center' : '', 		'flushright' : '',
		    \ 'flushleft' : '', 	'quotation' : 'quot',
		    \ 'quot' : 'quot',		'tabbing' : '',
		    \ 'picture' : 'pic',	'minipage' : '',	
		    \ 'list' : 'list',		'figure' : 'fig',
		    \ 'verbatim' : 'verb', 	'verse' : 'verse',
		    \ 'thebibliography' : '',	'document' : 'no_short_name',
		    \ 'titlepave' : '', 	'align' : 'eq',
		    \ 'alignat' : 'eq',		'equation' : 'eq',
		    \ 'gather'  : 'eq', 	'multiline' : '',
		    \ 'split'	: 'eq', 	'substack' : '',
		    \ 'flalign' : 'eq',
		    \ 'part'	: 'prt',	'chapter' : 'chap',
		    \ 'section' : 'sec',	'subsection' : 'ssec',
		    \ 'subsubsection' : 'sssec', 'paragraph' : 'par',
		    \ 'subparagraph' : 'spar' }

	" ToDo: Doc.
	" Usage: \label{l:shorn_env_name . g:atp_separator
	if !exists("g:atp_separator")
	    let g:atp_separator=':'
	endif
	if !exists("g:atp_no_separator")
	    let g:atp_no_separator = 0
	endif
	if !exists("g:atp_no_short_names")
	    let g:atp_env_short_names = 1
	endif
	" the separator will not be put after the environments in this list:  
	" the empty string is on purpose: to not put separator when there is
	" no name.
	let g:atp_no_separator_list=['', 'titlepage']

	let g:atp_package_list=sort(['amsmath', 'amssymb', 'amsthm', 'amstex', 
	\ 'babel', 'booktabs', 'bookman', 'color', 'colorx', 'chancery', 'charter', 'courier',
	\ 'enumerate', 'euro', 'fancyhdr', 'fancyheadings', 'fontinst', 
	\ 'geometry', 'graphicx', 'graphics',
	\ 'hyperref', 'helvet', 'layout', 'longtable',
	\ 'newcent', 'nicefrac', 'ntheorem', 'palatino', 'stmaryrd', 'showkeys', 'tikz',
	\ 'qpalatin', 'qbookman', 'qcourier', 'qswiss', 'qtimes', 'verbatim', 'wasysym'])

	" the command \label is added at the end.
	let g:atp_commands=['begin{', 'end{', 
	\ 'cite{', 'nocite{', 'ref{', 'pageref{', 'eqref{', 'bibitem', 'item',
	\ 'emph{', 'documentclass{', 'usepackage{',
	\ 'section{', 'subsection{', 'subsubsection{', 'part{', 
	\ 'chapter{', 'appendix ', 'subparagraph ', 'paragraph ',
	\ 'textbf{', 'textsf{', 'textrm{', 'textit{', 'texttt{', 
	\ 'textsc{', 'textsl{', 'textup{', 'textnormal ', 
	\ 'bfseries', 'mdseries',
	\ 'tiny ', 'scriptsize ', 'footnotesize ', 'small ',
	\ 'normal ', 'large ', 'Large ', 'LARGE ', 'huge ', 'HUGE ',
	\ 'usefont{', 'fontsize{', 'selectfont ',
	\ 'addcontentsline{', 'addtocontents ',
	\ 'input', 'include', 'includeonly', 
	\ 'savebox', 'sbox', 'usebox ', 'rule ', 'raisebox{', 
	\ 'parbox{', 'mbox{', 'makebox{', 'framebox{', 'fbox{',
	\ 'bigskip ', 'medskip ', 'smallskip ', 'vfill ', 'vspace{', 
	\ 'hspace ', 'hrulefill ', 'hfill ', 'dotfill ',
	\ 'thispagestyle ', 'markright ', 'pagestyle ', 'pagenumbering ',
	\ 'author{', 'date{', 'thanks{', 'title{',
	\ 'maketitle ', 'overbrace{', 'underbrace{',
	\ 'marginpar ', 'indent ', 'noindent ', 'par ', 'sloppy ', 'pagebreak[', 'nopagebreak[',
	\ 'newpage ', 'newline ', 'linebreak[', 'hyphenation{', 'fussy ',
	\ 'enlagrethispage{', 'clearpage ', 'cleardoublepage ',
	\ 'opening{', 'name{', 'makelabels{', 'location{', 'closing{', 'address{', 
	\ 'signature{', 'stopbreaks ', 'startbreaks ',
	\ 'newcounter{', 'refstepcounter{', 
	\ 'roman{', 'Roman{', 'stepcounter{', 'setcounter{', 
	\ 'usecounter{', 'value{', 'newtheorem{', 'newfont{', 
	\ 'newlength{', 'setlength{', 'addtolength{', 'settodepth{', 
	\ 'settoheight{', 'settowidth{', 
	\ 'width', 'height', 'depth', 'totalheight',
	\ 'footnote{', 'footnotemark ', 'footnotetetext', 
	\ 'bibliography{', 'bibliographystyle{', 'linethickness', 'line', 'circle',
	\ 'frame', 'multiput', 'oval', 'put', 'shortstack', 'vector', 'dashbox',
	\ 'flushbottom', 'onecolumn', 'raggedbottom', 'twocolumn',  
	\ 'alph{', 'Alph{', 'arabic{', 'fnsymbol{', 'reversemarginpar',
	\ 'hat{', 'grave{', 'bar{', 'acute{', 'mathring{', 'check{', 'dot{', 'vec{', 'breve{',
	\ 'tilde{', 'widetilde{', 'widehat{', 'ddot{', 'exhyphenpenalty',
	\ 'topmargin', 'oddsidemargin', 'evensidemargin', 'headheight', 'headsep', 
	\ 'textwidth', 'textheight', 'marginparwidth', 'marginparsep', 'marginparpush', 'footskip', 'hoffset',
	\ 'voffset', 'paperwidth', 'paperheight', 'theequation', 'thepage' ]
	
	" ToDo: end writting layout commands. 
	" ToDo: MAKE COMMANDS FOR PREAMBULE.

	let g:atp_math_commands=['forall', 'exists', 'emptyset', 'aleph', 'partial',
	\ 'nabla', 'Box', 'Diamond', 'bot', 'top', 'flat', 'sharp',
	\ 'mathbf{', 'mathsf{', 'mathrm{', 'mathit{', 'mathbb{', 'mathtt{', 'mathcal{', 
	\ 'mathop{', 'limits', 'text{', 'leqslant', 'leq', 'geqslant', 'geq',
	\ 'gtrsim', 'lesssim', 'gtrless', 
	\ 'rightarrow', 'Rightarrow', 'leftarrow', 'Leftarrow', 'iff', 
	\ 'leftrightarrow', 'Leftrightarrow', 'downarrow', 'Downarrow', 'Uparrow',
	\ 'Longrightarrow', 'longrightarrow', 'Longleftarrow', 'longleftarrow',
	\ 'overrightarrow{', 'overleftarrow{', 'underrightarrow{', 'underleftarrow{',
	\ 'uparrow', 'nearrow', 'searrow', 'swarrow', 'nwarrow', 
	\ 'hookrightarrow', 'hookleftarrow', 'gets', 
	\ 'sum', 'bigsum', 'cup', 'bigcup', 'cap', 'bigcap', 
	\ 'prod', 'coprod', 'bigvee', 'bigwedge', 'wedge',  
	\ 'oplus', 'otimes', 'odot', 'oint',
	\ 'int', 'bigoplus', 'bigotimes', 'bigodot', 'times',  
	\ 'smile', 'frown', 'subset', 'subseteq', 'supset', 'supseteq',
	\ 'dashv', 'vdash', 'vDash', 'Vdash', 'models', 'sim', 'simeq', 
	\ 'prec', 'preceq', 'preccurlyeq', 'precapprox',
	\ 'succ', 'succeq', 'succcurlyeq', 'succapprox', 'approx', 
	\ 'thickapprox', 'conq', 'bullet', 
	\ 'lhd', 'unlhd', 'rhd', 'unrhd', 'dagger', 'ddager', 'dag', 'ddag', 
	\ 'ldots', 'cdots', 'vdots', 'ddots', 
	\ 'vartriangleright', 'vartriangleleft', 'trianglerighteq', 'trianglelefteq',
	\ 'copyright', 'textregistered', 'puonds',
	\ 'big', 'Big', 'Bigg', 'huge', 
	\ 'left', 'right', 'bigr', 'Bigr', 'biggr', 'Biggr',
	\ 'bigl', 'Bigl', 'biggl', 'Biggl',
	\ 'sqrt', 'frac{', 'binom{', 'cline', 'vline', 'hline', 'multicolumn{', 
	\ 'nouppercase', 'sqsubset', 'sqsupset', 'square', 'blacksqaure', 'triangledown', 'triangle', 
	\ 'diagdown', 'diagup', 'nexists', 'varnothing', 'Bbbk', 'circledS', 'complement', 'hslash', 'hbar', 
	\ 'eth', 'rightrightarrows', 'leftleftarrows', 'rightleftarrows', 'leftrighrarrows', 
	\ 'downdownarrows', 'upuparrows', 'rightarrowtail', 'leftarrowtail', 
	\ 'twoheadrightarrow', 'twoheadleftarrow', 'rceil', 'lceil', 'rfloor', 'lfloor', 
	\ 'bullet', 'bigtriangledown', 'bigtriangleup', 'ominus', 'bigcirc', 'amalg', 
	\ 'setminus', 'sqcup', 'sqcap', 
	\ 'notin', 'neq', 'smile', 'frown', 'equiv', 'perp',
	\ 'quad', 'qquad', 'stackrel', 'displaystyle', 'textstyle', 'scriptstyle', 'scriptscriptstyle',
	\ 'langle', 'rangle' ]

	" commands defined by the user in input files.
	" ToDo: to doc.
	" ToDo: this doesn't work with input files well enough. 
	
	" Returns a list of two lists:  [ commanad_names, enironment_names ]

    	if !exists("*LocalCommands")
	function! LocalCommands()
	    call s:setprojectname()
	    let l:command_names=[]
	    let l:environment_names=[]

	    let l:ddict=s:make_defi_dict(b:atp_mainfile,'\\def\>\|\\newcommand\>\|\\newenvironment\|\\newtheorem')
" 	    echomsg " LocalCommands DEBUG " . b:atp_mainfile
	    let b:ddict=l:ddict
		for l:inputfile in keys(l:ddict)
		    let l:ifile=readfile(l:inputfile)
		    for l:range in l:ddict[l:inputfile]
			if l:ifile[l:range[0]-1] =~ '\\def\|\\newcommand'
			    " check only definitions which starts at 0 column
			    let l:name=matchstr(l:ifile[l:range[0]-1],
					\ '^\%(\\def\\\zs[^{#]*\ze[{#]\|\\newcommand{\?\\\zs[^\[{]*\ze[\[{}]}\?\)')
			    if l:name != ""
				call add(l:command_names,l:name)
" 				echomsg l:name
			    endif
			endif
			if l:ifile[l:range[0]-1] =~ '\\newenvironment\|\\newtheorem'
			    " check only definitions which starts at 0 column
			    let l:name=matchstr(l:ifile[l:range[0]-1],
					\ '^\\\%(newtheorem\*\?\|newenvironment\){\zs[^}]*\ze}')
			    if l:name != ""
				call add(l:environment_names,l:name)
			    endif
			endif
		    endfor
		endfor
	    let b:atp_local_commands		= []
	    let b:atp_local_environments	= []

	    " remove double entries
	    for l:type in ['command', 'environment']
" 		echomsg l:type
		for l:item in l:{l:type}_names
" 		    if l:type == 'environment'
" 			echomsg l:item . "  " . index(g:atp_{l:type}s,l:item)
" 		    endif
		    if index(g:atp_{l:type}s,l:item) == '-1'
			call add(b:atp_local_{l:type}s,l:item)
		    endif
		endfor
	    endfor
	    return [ b:atp_local_environments, b:atp_local_commands ]
	endfunction
	endif
	" The BufEnter augroup doesn't work with EditInputFile, but at least it works
	" when entering. Debuging shows that when entering new buffer it uses
	" wrong b:atp_mainfile, it is still equal to the bufername and not the
	" real main file. Maybe it is better to use s:mainfile variable.
	if !exists("g:atp_local_completion_lists")
	    let g:atp_local_completion_lists = 1
	endif
	if g:atp_local_completion_lists == 1
	    call LocalCommands()
	elseif g:atp_local_completion_lists == 2 
	    au BufEnter *.tex call LocalCommands()
	endif


	let g:atp_math_commands_non_expert_mode=[ 'leqq', 'geqq', 'succeqq', 'preceqq', 
		    \ 'subseteqq', 'supseteqq', 'gtrapprox', 'lessapprox' ]
	 
	" requiers amssymb package:
	let g:atp_ams_negations=[ 'nless', 'ngtr', 'lneq', 'gneq', 'nleq', 'ngeq', 'nleqslant', 'ngeqslant', 
		    \ 'nsim', 'nconq', 'nvdash', 'nvDash', 
		    \ 'nsubseteq', 'nsupseteq', 
		    \ 'varsubsetneq', 'subsetneq', 'varsupsetneq', 'supsetneq', 
		    \ 'ntriangleright', 'ntriangleleft', 'ntrianglerighteq', 'ntrianglelefteq', 
		    \ 'nrightarrow', 'nleftarrow', 'nRightarrow', 'nLeftarrow', 
		    \ 'nleftrightarrow', 'nLeftrightarrow', 'nsucc', 'nprec', 'npreceq', 'nsucceq', 
		    \ 'precneq', 'succneq', 'precnapprox' ]

	let g:atp_ams_negations_non_expert_mode=[ 'lneqq', 'ngeqq', 'nleqq', 'ngeqq', 'nsubseteqq', 
		    \ 'nsupseteqq', 'subsetneqq', 'supsetneqq', 'nsucceqq', 'precneqq', 'succneqq' ] 

	" ToDo: add more amsmath commands.
	let g:atp_amsmath_commands=[ 'boxed', 'inserttext', 'multiligngap', 'shoveleft', 'shoveright', 'notag', 'tag', 
		    \ 'raistag{', 'displaybreak', 'allowdisplaybreaks', 'numberwithin{',
		    \ 'hdotsfor{' , 'mspace{',
		    \ 'negthinspace', 'negmedspace', 'negthickspace', 'thinspace', 'medspace', 'thickspace',
		    \ 'leftroot{', 'uproot{', 'overset{', 'underset{', 'sideset{', 
		    \ 'dfrac{', 'tfrac{', 'cfrac{', 'dbinom{', 'tbinom{', 'smash',
		    \ 'lvert', 'rvert', 'lVert', 'rVert', '\DeclareMatchOperator{',
		    \ 'arccos', 'arcsin', 'arg', 'cos', 'cosh', 'cot', 'coth', 'csc', 'deg', 'det',
		    \ 'dim', 'exp', 'gcd', 'hom', 'inf', 'injlim', 'ker', 'lg', 'lim', 'liminf', 'limsup',
		    \ 'ln', 'log', 'min', 'max', 'Pr', 'projlim', 'sec', 'sin', 'sinh', 'sup', 'tan', 'tanh',
		    \ 'varlimsup', 'varliminf', 'varinjlim', 'varprojlim', 'mod', 'bmod', 'pmod', 'pod', 'sideset',
		    \ 'iint', 'iiint', 'iiiint', 'idotsint',
		    \ 'varGamma', 'varDelta', 'varTheta', 'varLambda', 'varXi', 'varPi', 'varSigma', 
		    \ 'varUpsilon', 'varPhi', 'varPsi', 'varOmega' ]
	
	" ToDo: integrate in Tab_Completion (amsfonts, euscript packages).
	let g:atp_amsfonts=[ 'mathfrak', 'mathscr' ]

	" not yet supported: in Tab_Completion:
	let g:atp_amsxtra_commands=[ 'sphat', 'sptilde' ]
	let g:atp_fancyhdr_commands=['lfoot{', 'rfoot{', 'rhead{', 'lhead{', 
		    \ 'cfoot{', 'chead{', 'fancyhead{', 'fancyfoot{',
		    \ 'fancypagestyle{', 'fancyhf{}', 'headrulewidth ', 'footrulewidth ',
		    \ 'rightmark', 'leftmark', 'markboth', 
		    \ 'chaptermark', 'sectionmark', 'subsectionmark',
		    \ 'fancyheadoffset', 'fancyfootoffset', 'fancyhfoffset']


	" ToDo: remove tikzpicture from above and integrate the
	" tikz_envirnoments variable
	" \begin{pgfonlayer}{background} (complete the second argument as
	" well}
	"
	" Tikz command cuold be accitve only in tikzpicture and after \tikz
	" command! There is a way to do that.
	" 
	let g:atp_tikz_environments=['tikzpicture', 'scope', 'pgfonlayer', 'background' ]
	" ToDo: this should be completed as packages.
	let g:atp_tikz_libraries=sort(['arrows', 'automata', 'backgrounds', 'calc', 'calendar', 'chains', 'decorations', 
		    \ 'decorations.footprints', 'decorations.fractals', 
		    \ 'decorations.markings', 'decorations.pathmorphing', 
		    \ 'decorations.replacing', 'decorations.shapes', 
		    \ 'decorations.text', 'er', 'fadings', 'fit',
		    \ 'folding', 'matrix', 'mindmap', 'scopes', 
		    \ 'patterns', 'pteri', 'plothandlers', 'plotmarks', 
		    \ 'plcaments', 'pgflibrarypatterns', 'pgflibraryshapes',
		    \ 'pgflibraryplotmarks', 'positioning', 'replacements', 
		    \ 'shadows', 'shapes.arrows', 'shapes.callout', 'shapes.geometric', 
		    \ 'shapes.gates.logic.IEC', 'shapes.gates.logic.US', 'shapes.misc', 
		    \ 'shapes.multipart', 'shapes.symbols', 'topaths', 'through', 'trees' ])
	" ToDo: completion for arguments in brackets [] for tikz commands.
	let g:atp_tikz_commands=[ 'matrix', 'node', 'shadedraw', 'draw', 'tikz', 'usetikzlibrary{', 'tikzset',
		    \ 'path', 'filldraw', 'fill', 'clip', 'drawclip', 'foreach', 'angle', 'coordinate',
		    \ 'useasboundingbox', 'tikztostart', 'tikztotarget', 'tikztonodes', 'tikzlastnode',
		    \ 'pgfextra', 'endpgfextra',
		    \ 'pattern', 'shade', 'shadedraw', ]
	" ToDo: think of keyword completions
" 	let g:tikz_keywords=[]

" ToDo: to doc.
" adding commands to completion list whether to check or not if we are in the
" correct environment (for example \tikz or \begin{tikzpicture})
if !exists("g:atp_check_if_opened")
    let g:atp_check_if_opened=1
endif
" This is as the above, but works only if one uses \(:\), \[:\]
if !exists("g:atp_math_opened")
    if search('\%([^\\]\|^\)\$\$\?','wnc') != 0
	let g:atp_math_opened=0
    else
	let g:atp_math_opened=1
    endif
endif
" ToDo: Think about even better math modes patterns.
" \[ - math mode \\[ - not mathmode (this can be at the end of a line as: \\[3pt])
" \\[ - this is math mode, but tex will complain (now I'm not matching it,
" that's maybe good.) 
" How to deal with $:$ (they are usually in one line, we could count them)  and $$:$$ 
" matchpair
let g:atp_math_modes=[ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)(','\%([^\\]\|^\)\%(\\\|\\\{3}\))'],
	    \ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)\[','\%([^\\]\|^\)\%(\\\|\\\{3}\)\]'], 	
	    \ ['\\begin{align', '\end{align'], 		['\\begin{gather', '\\end{gather'], 
	    \ ['\\begin{flign', '\\end{flagin'], 	['\\begin[multiline', '\\end{multiline'],
	    \ ['\\begin{tikz', '\\end{tikz'],		['\begin{equation', '\end{equation'] ]
" ToDo: user command list, env list g:atp_commands, g:atp_environments, 
"
" this function looks for an input file: in the list of buffers, under a path if
" it is given, then in the b:outdir.
" directory. The last argument if equal to 1, then look also
" under g:texmf.
function! s:Read_Input_File(ifile,check_texmf)

    let l:input_file = []

    " read the buffer or read file if the buffer is not listed.
    if buflisted(fnamemodify(a:ifile,":t"))
	let l:input_file=getbufline(fnamemodify(a:ifile,":t"),1,'$')
    " if the ifile is given with a path it should be tried to reaad from there
    elseif filereadable(a:ifile)
	let l:input_file=readfile(a:ifile)
    " if not then try to read it from b:outdir
    elseif filereadable(b:outdir . fnamemodify(a:ifile,":t"))
	let l:input_file=readfile(filereadable(b:outdir . fnamemodify(a:ifile,":t")))
    " the last chance is to look for it in the g:texmf directory
    elseif a:check_texmf && filereadable(findfile(a:ifile,g:texmf . '**'))
	let l:input_file=readfile(findfile(a:ifile,g:texmf . '**'))
    endif

    return l:input_file
endfunction
 
function! s:Add_to_List(list,what)
    let l:new=[] 
    for l:el in a:list
	call add(l:new,l:el . a:what)
    endfor
    return l:new
endfunction

" the argument should be b:atp_mainfile but in any case it is made in this way.
" it specifies in which file to search for include files.
function! s:Search_Bib_Items(name)

    " we are going to make a dictionary { citekey : label } (see :h \bibitem) 
    let l:citekey_label_dict={}

    " make a list of include files.
    let l:inputfile_dict=FindInputFiles(a:name,0)
    let l:includefile_list=[]
    for l:key in keys(l:inputfile_dict)
	if l:inputfile_dict[l:key][0] =~ '^\%(include\|input\|includeonly\)$'
	    call add(l:includefile_list,s:append(l:key,'.tex'))
	endif
    endfor
    call add(l:includefile_list,b:atp_mainfile) 
    let b:ifl=l:includefile_list

    " search for bibitems in all include files.
    for l:ifile in l:includefile_list

	let l:input_file = s:Read_Input_File(l:ifile,0)

	    " search for bibitems and make a dictionary of labels and citekeys
	    for l:line in l:input_file
		if l:line =~ '\\bibitem'
		    let l:label=substitute(l:line,'.*\\bibitem\s*\[\(.*\)\].*$','\1','')
		    if l:label =~ 'bibitem'
			let l:label=''
		    endif
		    call extend(l:citekey_label_dict,
			\ { substitute(l:line,'.*\\bibitem\s*\%(\[.*\]\)\?\s*{\(.*\)}.*$','\1','') : l:label },
			\ 'error') 
		endif
	    endfor
    endfor
	
    return l:citekey_label_dict
endfunction


command! SearchBibItems 	:echo s:Search_Bib_Items(b:atp_mainfile)

" ToDo: \ref{<Tab> do not closes the '}', its by purpose, as sometimes one
" wants to add more than one reference. But this is not allowed by this
" command! :) I can add it.
" works for:
" 	labels   (\ref,\eqref)
" 	bibitems (\cite)
" 	bibfiles (\bibliography)
" 	packages (\usepackage)
" 	commands
" 	environments (\begin)
" 	end	     (close \begin{env} with \end{env})
"

"ToDo: the completion should be only done if the completed text is different
"from what it is. But it might be as it is, there are reasons to keep this.
"

function! s:Copy_Indentation(line)
    let l:indent=split(a:line,'\s\zs')
    let l:eindent=""
    for l:s in l:indent
	if l:s =~ '^\%(\s\|\t\)'
	    let l:eindent.=l:s
	else
	    break
	endif
    endfor
    return l:eindent
endfunction
" the argument specifies if to use i or a (append before or after)
" default is to use i (before), so the cursor will be after.
" the second argument specifies which environment to close (without it tries
" checks which to close.
" ToDo: this would be nice if it worked with nested environments which starts in
" the same line (if starts in seprate lines the only thing to change is to
" move the cursor to the end of inserted closing).
" ToDo: add closing of other pairs {:},[:],\{:\} , \left:\right 
" ToDo: the mechanism closes:
" \begin{theorem}
"       .....
"       <Tab>
" just under \begin{theorem}
" ToDo: Ad a highlight to messages!!! AND MAKE IT NOT DISAPEAR SOME HOW?
" (redrawing doesn't help). 
function! CloseLastEnv(...)

    if a:0 == 0 
	let l:com = 'i'
    elseif a:0 >= 1  && a:1 == 'a' 
	let l:com ='a'
    elseif  a:0 >= 1 && a:1 == 'i'
	let l:com = 'i'
    endif

    if a:0 >= 2
	let l:close=a:2
    endif
    if a:0 >= 3
	let l:env_name=a:3
    else
	let l:env_name="0"
    endif

    " ADD: if l:com == 'i' move before what we put.

"     let b:debug=0
"     let b:com=l:com "DEBUG

    if l:env_name == "0"
	let l:begin_line_env 	= searchpair('\\begin\s*{', '', '\\end\s*{', 'bnW')
	let l:begin_line_dmath 	= searchpair(g:atp_math_modes[1][0],'',g:atp_math_modes[1][1], 'bnW')
	let l:begin_line_imath 	= searchpair(g:atp_math_modes[0][0],'',g:atp_math_modes[0][1], 'bnW')
    else
	let l:begin_line 	= searchpair('\\begin\s*{' . l:env_name , '', '\\end\s*{' . l:env_name, 'bnW')
    endif

    if a:0 <= 1
	let l:begin_line=max([ l:begin_line_env, l:begin_line_imath, l:begin_line_dmath])
" 	echo "OK"
    elseif a:0 <= 2 && l:close == "environment"
" 	echo "env"
	let l:begin_line = l:begin_line_env
    elseif a:0 <= 2 && l:close == "displayed_math"
" 	echo "disp"
	let l:begin_line = l:begin_line_dmath
    elseif a:0 <= 2 && l:close == "inline_math"
" 	echo "inl"
	let l:begin_line = l:begin_line_imath
    endif

    if a:0 < 2
	if l:begin_line_env >= l:begin_line_dmath && l:begin_line_env >= l:begin_line_imath
	    let l:close='environment'
	elseif l:begin_line_dmath > l:begin_line_env && l:begin_line_dmath > l:begin_line_imath
	    let l:close='displayed_math'
	else
	    let l:close='inline_math'
	endif
    endif

    " regardles of a:2 if a:3 is given:
    if a:0 == 3
	let l:close='environment'
    endif
    let b:close=l:close " DEBUG
    let b:begin_line=l:begin_line "DEBUG

    if l:begin_line
	let l:line=getline(l:begin_line)
	let l:cline=getline(".")
" 	let b:line=l:line	" DEBUG
	if l:close == 'environment'
	    if l:env_name == 0
		let l:env = matchstr(l:line, '\%(\\begin\s*{[^}]*}\s*\%(\\label\s*{[^}]*}\)\?\)*\s*\\begin{\zs[^}]*\ze}\%(.*\\begin\s{\)\@!')
	    else
		let l:env=l:env_name
	    endif
	endif
" 	let b:env=l:env " DEBUG
	let l:pos=getpos(".")
	" Copy the intendation of what we are closing.
	let l:eindent=s:Copy_Indentation(l:line)

	" Rules:
	" env & \[ \]: close in the same line 
	" unless it starts in a seprate line,
	" \( \): close in the same line. 
	if (l:close == 'environment' 
		    \ && l:line !~ '^\s*\%(\$\|\$\$\|[^\\]\\(\|[^\\]\\\[\)\?\s*\\begin\s*{[^}]*}\s*\%(\[.*]*\]\|\\label{[^}]*}\)*\s*$' 
		    \ && l:line !~ '^\s*\%(\$\|\$\$\|[^\\]\\(\|[^\\]\\\[\)\?\s*\\begin\s*{\%(array\|tabular\)}\%(\s*{[^}]*}\)\?\s*$' ) ||
		    \ (l:close == 'displayed_math' && l:line !~ '^\s*[^\\]\\\[\s*$' ) ||
		    \ (l:close == 'inline_math' && (l:line !~ '^\s*[^\\]\\(\s*$' || l:begin_line == line(".") )) 
	    " the above condition matches for the situations when we have to
	    " complete in the same line in three cases:
	    " l:close == environment, displayd_math or inline_math. 
	    if l:close == 'environment' && index(g:atp_no_complete,l:env) == '-1' &&
		\ !s:Check_if_Closed('\\begin\s*{' . l:env,'\\end\s*{' . l:env,line("."),g:atp_completion_limits[2])
" 		let l:env_name=matchstr(l:line,'\%(\\begin.*\)*\\begin{\zs[^}]*\ze}') 
		let b:d=1 
		if l:com == 'a'
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\end{'.l:env.'}' . strpart(l:cline,getpos(".")[2]))
		    let l:pos=getpos(".")
		    let l:pos[2]=len(strpart(l:cline,0,getpos(".")[2]) . '\end{'.l:env.'}')+1
		    keepjumps call setpos(".",l:pos)
		else
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\end{'.l:env.'}' . strpart(l:cline,getpos(".")[2]-1))
		    let l:pos=getpos(".")
		    let l:pos[2]=len(strpart(l:cline,0,getpos(".")[2]-1) . '\end{'.l:env.'}')+1
		    keepjumps call setpos(".",l:pos)
		endif
	    elseif l:close == 'displayed_math' && !s:Check_if_Closed(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line("."),g:atp_completion_limits[1])
		let b:d=2
		if l:com == 'a'
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\]'. strpart(l:cline,getpos(".")[2]))
		else
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\]'. strpart(l:cline,getpos(".")[2]-1))
" TODO: This could be optional: (but the option rather
" should be an argument of this function rather than
" here!
		    let l:pos=getpos(".")
		    let l:pos[2]+=2
		    keepjumps call setpos(("."),l:pos)
		endif
	    elseif l:close == 'inline_math' && !s:Check_if_Closed(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line("."),g:atp_completion_limits[1])
		let b:d=2
" 		exec "normal " . l:com  . "\\)"
		if l:com == 'a'
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\)'. strpart(l:cline,getpos(".")[2]))
		else
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\)'. strpart(l:cline,getpos(".")[2]-1))
		    let l:pos=getpos(".")
		    let l:pos[2]+=2
		    keepjumps call setpos(("."),l:pos)
		endif
	    endif
" 	    let b:debug=1 " DEBUG
	else
	" We are closing in a new line, preerving the indentation.
	    
	    let l:line_nr=line(".")

	    "Debug:
	    if l:close == 'environment'
	    " NESTING
		let l:error=0
		let l:prev_line_nr="-1"
		let l:cenv_lines=[]
		let b:env_open_name=[] " DEBUG
		let l:nr=line(".")
		" l:line_nr number of line which we complete
		" l:cenv_lines list of closed environments (we complete after
		" line number maximum of these numbers.
" 		echomsg "DEBUG ----------"

" 		if g:atp_close_after_last_closed == 1	
		    let l:pos=getpos(".")
" 		endif
		let l:pos_saved=deepcopy(l:pos)

		while l:line_nr >= 0
" 		    if g:atp_close_after_last_closed == 1	
			let l:line_nr=search('\\begin\s*{','bW')
" 		    else
" 			let l:line_nr=s:Check_if_Opened('\\begin\s*{', '\\end\s*{',
" 				\ l:line_nr,g:atp_completion_limits[2],1)
" 		    endif
		    " match last environment openned in this line.
		    " ToDo: afterwards we can make it works for multiple openned
		    " envs.
		    let l:env_name=matchstr(getline(l:line_nr),'\\begin\s*{\zs[^}]*\ze}\%(.*\\begin\s*{[^}]*}\)\@!')
		    let l:close_line_nr=s:Check_if_Closed('\\begin\s*{' . l:env_name, 
				\ '\\end\s*{' . l:env_name,
				\ l:line_nr,g:atp_completion_limits[2],1)
" 		    echomsg "CLE line_nr " . l:line_nr . " close_line_nr " . l:close_line_nr . " env_name " . l:env_name

" 			let l:bis_close_line_nr=s:Check_if_Closed('\\begin\s*{', '\\end\s*{',
" 				    \ l:line_nr,g:atp_completion_limits[2],1)
" 			if l:bis_close_line_nr != 0 && l:bis_close_line_nr < l:nr
" 			    call add(l:cenv_lines,l:bis_close_line_nr)
" 			endif

		    if l:close_line_nr != 0
			call add(l:cenv_lines,l:close_line_nr)
		    else
			break
		    endif
		    let l:line_nr-=1
" 		    echo "CLE DEBUG l:line_nr " . l:line_nr
		endwhile

" 		if g:atp_close_after_last_closed == 1	
		    keepjumps call setpos(".",l:pos)
" 		endif
		    
		let b:cenv_lines=deepcopy(l:cenv_lines)

		let b:line_nr=l:line_nr " DEBUG
			
		" get all names of environments which begin in this line
		let l:env_names=[]
		let l:line=getline(l:line_nr)
		while l:line =~ '\\begin\s*{' 
		    let l:cenv_begins = match(l:line,'\\begin{\zs[^}]*\ze}\%(.*\\begin\s{\)\@!')
		    let l:cenv_name = matchstr(l:line,'\\begin{\zs[^}]*\ze}\%(.*\\begin\s{\)\@!')
		    let l:cenv_len=len(l:cenv_name)
		    let l:line=strpart(l:line,l:cenv_begins+l:cenv_len)
		    call add(l:env_names,l:cenv_name)
			" DEBUG:
			let b:env_names=l:env_names
			let b:line=l:line
			let b:cenv_begins=l:cenv_begins
			let b:cenv_name=l:cenv_name
		endwhile
		" thus we have a list of env names.
		
		" make a dictionary of lines where they closes. 
		" this is a list of pairs (I need the order!)
		let l:env_dict=[]
		let b:env_dict=l:env_dict " DEBUG
		" list of closed environments
		let l:cenv_names=[]
" 		let b:cenv_names=l:cenv_names
		for l:uenv in l:env_names
		    let l:uline_nr=s:Check_if_Closed('\\begin\s*{' . l:uenv . '}', 
				\ '\end\s*{' . l:uenv . '}', l:line_nr, g:atp_completion_limits[2])
		    call extend(l:env_dict,[ l:uenv, l:uline_nr])
		    if l:uline_nr != '0'
			call add(l:cenv_names,l:uenv)
		    endif
		endfor
		
		" close unclosed environment

		" check if at least one of them is closed
		if len(l:cenv_names) == 0
" 		    echomsg "cle DEBUG A1"
		    let l:str=""
		    for l:uenv in l:env_names
			if index(g:atp_no_complete,l:uenv) == '-1'
			    let l:str.='\end{' . l:uenv .'}'
			endif
		    endfor
		    " Do not append empty lines (l:str is empty if all l:uenv
		    " belongs to the g:atp_no_complete list.
		    if len(l:str) == 0
			return 0
		    endif
" 		    let b:str=l:str
		    let l:eindent=s:Copy_Indentation(getline(l:line_nr))
		    let l:pos=getpos(".")
		    if len(l:cenv_lines) > 0 
" 			call append(max(l:cenv_lines), l:eindent . l:str)

			let l:max=max(l:cenv_lines)
			let l:pos[1]=l:max+1
			" find the first closed item below the last closed
			" pair (below l:pos[1]). (I assume every env is in
			" a seprate line!
" 			let l:end=s:Check_if_Closed('\\begin\s*{','\\end\s*{',max(l:cenv_lines)+1,g:atp_completion_limits[2],1)
			let l:end=s:Check_if_Closed('\\begin\s*{','\\end\s*{',l:line_nr,g:atp_completion_limits[2],1)
			let b:info= " l:max " .  l:max . " l:end " . l:end . " line('.') " . line(".")
			" if the line was found append just befor it.
			echohl WarningMsg 
			if l:end != 0 
				if line(".") <= l:max
				    if line(".") <= l:end
					call append(l:max, l:eindent . l:str)
					echomsg l:str . " appneded after line " . l:end
					let b:cle_return="append cenv_lines 1.1.1 before line " . l:max 
				    else
					call append(l:end-1, l:eindent . l:str)
					echomsg l:str . " appneded after line " . l:end
					let b:cle_return="append cenv_lines 1.1.2 before line " . l:max 
				    endif
				elseif line(".") < l:end
				    call append(line("."), l:eindent . l:str)
				    echomsg l:str . " appneded after line " . line(".")
				    let b:cle_return="append cenv_lines 1.2 before line " . line(".")
				elseif line(".") >= l:end
				    call append(l:end-1, l:eindent . l:str)
				    echomsg l:str . " appneded after line " . (l:end-1)
				    let b:cle_return="append cenv_lines 1.3 before line " . (l:end-1)
				endif
			else
			    if line(".") >= l:max
				call append(l:pos_saved[1], l:eindent . l:str)
				keepjumps call setpos(".",l:pos_saved)
				echomsg l:str . " appneded after line " . line(".")
				let b:cle_return="append cenv_lines 2.1 after line " . line(".")
			    elseif line(".") < l:max
				call append(l:max, l:eindent . l:str)
				keepjumps call setpos(".",l:pos_saved)
				echomsg l:str . " appneded after line " . l:max
				let b:cle_return="append cenv_lines 2.2 after line " . l:max
" 			    elseif line(".") >= l:end
"				If we are to far				
" 				call append(l:end-1, l:eindent . l:str)
" 				echomsg l:str . " appneded after line " . (l:end-1)
" 				let b:cle_return="append cenv_lines 2.3 before line " . (l:end-1)
			    endif
			endif
			echohl None 
		    else
			" REVIEW THIS CODE: it seems that I only need: 'append else 2'
			"			echomsg "CLE saved position "  . join(l:pos_saved)
			let l:pos[1]=l:line_nr
			let l:pos[2]=1
			keepjumps call setpos(".",l:pos)
			keepjumps let l:eline_nr=search('\\end\s*{','nW',l:pos_saved[1])
			if l:eline_nr <= l:pos_saved[1] && l:eline_nr > 0
			    call append(l:eline_nr-1, l:eindent . l:str)
			    echomsg l:str . " appneded after line " . (l:eline_nr-1)
			    let b:cle_return="append if 1 str " . l:str . " before line " . l:eline_nr 
			    keepjumps call setpos(".",l:pos_saved)
			    return 1
			elseif l:eline_nr >= 0
			    call append(l:pos_saved[1], l:eindent . l:str)
			    echomsg l:str . " appneded after line " . l:pos_saved[1]
			    let b:cle_return="append if 2 str " . l:str . " after line " . l:pos_saved[1] 
			    keepjumps call setpos(".",l:pos_saved)
			    return 1
			endif
		    endif
		else
		    return "this is too hard?"
		endif
		unlet! l:env_names
		unlet! l:env_dict
		unlet! l:cenv_names
		unlet! l:pos 
		unlet! l:pos_saved
" 		if getline('.') =~ '^\s*$'
" 		    exec "normal dd"
" 		endif
	    elseif  l:close == 'displayed_math'
		call append(l:iline, l:eindent . '\]')
		echomsg "\[ closed in line " . l:iline
		let b:cle_return=2 . " dispalyed math " . l:iline  " DEBUG
	    elseif l:close == 'inline_math'
		call append(l:iline, l:eindent . '\)')
		echomsg "\( closed in line " . l:iline
		let b:cle_return=2 . " inline math " . l:iline  " DEBUG
	    endif
	    return ''
	endif
" 	" preserve the intendation
" 	if getline(line(".")) =~ '^\s\+\\end{'
" 	    call setline(line("."),substitute(getline(line(".")),'^\s*',l:eindent,''))
" 	    echomsg "DEBUG: WHAT's THAT?"
" 	endif
    endif
endfunction
" imap <F7> <Esc>:call CloseLastEnv()<CR>

" check if last bpat is closed.
" starting from the current line, limits the number of
" lines to search. It returns 0 if the environment is not closed or the line
" number where it is closed (an env is cannot be closed in 0 line)

" ToDo: the two function should only check not commented lines!
" ToDo: this do not works well with nested envs.
" Method 0 makes mistakes if the pattern is \begin:\end, if
" \begin{env_name}:\end{env_names} rather no (unless there are nested
" environments of the same name.
" Mechod 1 doesn't make mistakes and thus is preferable.
" after testing I shall remove method 0
function! s:Check_if_Closed(bpat,epat,line,limit,...)

    if a:0 == 0 || a:1 == 0
	let l:method = 0
    else
	let l:method = a:1
    endif
"     echomsg "DEBUG METHOD " . l:method

    let l:len=len(getbufline(bufname("%"),1,'$'))
    let l:nr=a:line

    if a:limit == "$" || a:limit == "-1"
	let l:limit=l:len-a:line
    else
	let l:limit=a:limit
    endif

    if l:method==0
	while l:nr <= a:line+l:limit
	    let l:line=getline(l:nr)
" 	    echomsg "CC line " . l:nr . " " . l:line
	" Check if Closed
	    if l:nr == a:line
		if strpart(l:line,getpos(".")[2]-1) =~ '\%(' . a:bpat . '.*\)\@<!' . a:epat
" " 		    echo "CC 1 l:nr " . l:nr
		    return l:nr
		endif
	    else
		if l:line =~ '\%(' . a:epat . '.*\)\@<!' . a:bpat
		    return 0
		elseif l:line =~ '\%(' . a:bpat . '.*\)\@<!' . a:epat 
"     	    if l:line =~ a:epat 
		    return l:nr
		endif
	    endif
	    let l:nr+=1
	endwhile

    elseif l:method==1

	let l:bpat_count=0
	let l:epat_count=0
	let l:begin_line=getline(a:line)
	let l:begin_line_nr=line(a:line)
" 	echomsg "CC DEBUG ------------"
	while l:nr <= a:line+l:limit
	    let l:line=getline(l:nr)
	" I assume that the env is opened in the line before!
	    let l:bpat_count+=s:count(l:line,a:bpat,1)
	    let l:epat_count+=s:count(l:line,a:epat,1)
" 	    echomsg "cc line nr " . l:nr . " bpat " . l:bpat_count . " epat " . l:epat_count
	    if (l:bpat_count+1) == l:epat_count && l:begin_line !~ a:bpat
" 		echomsg "A"
		return l:nr
	    elseif l:bpat_count == l:epat_count && l:begin_line =~ a:bpat
" 		echomsg "B"
		return l:nr
	    endif 
	    let l:nr+=1
	endwhile
	return 0
    endif
endfunction

" Usage: By default (a:0 == 0 || a:1 == 0 ) it returns line number where the
" environment is opened if the environment is opened and is not closed (for
" completion), else it returns 0. However, if a:1 == 1 it returns line number
" where the environment is opened, if we are inside an environemt (it is
" openned and closed below the starting line or not closed at all), it if a:1
" = 2, it just check if env is opened without looking if it is closed (
" cursor position is important).
" a:1 == 0 first non closed
" a:1 == 2 first non closed by counting.
function! s:Check_if_Opened(bpat,epat,line,limit,...)

    if a:0 == 0 || a:1 == 0
	let l:check_mode = 0
    elseif a:1 == 1
	let l:check_mode = 1
    elseif a:1 == 2
	let l:check_mode = 2
    endif

    let b:check_mode=l:check_mode

    let l:len=len(getbufline(bufname("%"),1,'$'))
    let l:nr=a:line

    if a:limit == "^" || a:limit == "-1"
	let l:limit=a:line-1
    else
	let l:limit=a:limit
    endif

    if l:check_mode == 0 || l:check_mode == 1
	while l:nr >= a:line-l:limit && l:nr >= 1
	    let l:line=getline(l:nr)
" 	echo "DEBUG A " . l:nr . " " . l:line
		if l:nr == a:line
" 		    let l:x= a:bpat . '.\{-}' . a:epat
" 		    echomsg " DEBUG CifO " . l:x
			if substitute(strpart(l:line,0,getpos(".")[2]), a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ a:bpat
			    let b:cifo_return=1
			    return l:nr
			endif
		else
		    if l:check_mode == 0
			if substitute(l:line, a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ a:bpat
			    " check if it is closed up to the place where we start. (There
			    " is no need to check after, it will be checked anyway
			    " b a seprate call in Tab_Completion.
			    if !s:Check_if_Closed(a:bpat,a:epat,l:nr,a:limit,0)
					    " LAST CHANGE 1->0 above
				let b:cifo_return=2 . " " . l:nr 
				return l:nr
			    endif
			endif
		    elseif l:check_mode == 1
			if substitute(l:line, a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ a:bpat
			    let l:check=s:Check_if_Closed(a:bpat,a:epat,l:nr,a:limit)
" 		    echo "DEBUG line nr: " l:nr . " line: " . l:line . " check: " . l:check
			    " if env is not closed or is closed after a:line
			    if  l:check == 0 || l:check >= a:line
				let b:cifo_return=2 . " " . l:nr 
				return l:nr
			    endif
			endif
		    endif
		endif
	    let l:nr-=1
	endwhile
    elseif l:check_mode == 2
	let l:bpat_count=0
	let l:epat_count=0
	let l:begin_line=getline(".")
	let l:c=0
	while l:nr >= a:line-l:limit  && l:nr >= 1
	    let l:line=getline(l:nr)
	" I assume that the env is opened in line before!
" 		let l:line=strpart(l:line,getpos(".")[2])
	    let l:bpat_count+=s:count(l:line,a:bpat,1)
	    let l:epat_count+=s:count(l:line,a:epat,1)
" 		echomsg "co " . l:c . " lnr " . l:nr . " bpat " . l:bpat_count . " epat " . l:epat_count
	    if l:bpat_count == (l:epat_count+1+l:c) && l:begin_line != line(".") 
		let l:env_name=matchstr(getline(l:nr),'\\begin{\zs[^}]*\ze}')
		let b:check=s:Check_if_Closed('\\begin{' . l:env_name . '}', '\\end{' . l:env_name . '}',1,a:limit,1)
" 			echomsg "co DEBUG " b:check . " env " . l:env_name
		if !b:check
		    return l:nr
		else
		    let l:c+=1
		endif
	    elseif l:bpat_count == l:epat_count && l:begin_line == line(".")
		return l:nr
	    endif 
	    let l:nr-=1
	endwhile
    endif
    return 0 
endfunction

command -buffer -nargs=* CheckIfOpened	:echo s:Check_if_Opened(<args>)
command -buffer -nargs=* CheckIfClosed	:echo s:Check_if_Closed(<args>)
" usage:
command -buffer CheckA	:echomsg "CheckA " . s:Check_if_Closed(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line('.'),g:atp_completion_limits[0])
command -buffer CheckB	:echomsg "CheckB " .  s:Check_if_Closed(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1])
command -buffer CheckC	:echomsg "CheckC " .  s:Check_if_Closed('\\begin{','\\end{',line('.'),g:atp_completion_limits[2])
command -buffer OCheckA	:echomsg "OCheckA " .  s:Check_if_Opened(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line('.'),g:atp_completion_limits[0])
command -buffer OCheckB	:echomsg "OCheckB " .  s:Check_if_Opened(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1])
command -buffer OCheckC	:echomsg "OCheckC " .  s:Check_if_Opened('\\begin{','\\end{',line('.'),g:atp_completion_limits[2])

" ToDo: to doc.
" I switched this off.
" if !exists("g:atp_complete_math_env_first")
"     let g:atp_complete_math_env_first=0
" endif
if !exists("g:atp_math_commands_first")
    let g:atp_math_commands_first=1
endif

" This is the main TAB COMPLITION function.
"
" expert_mode = 1 (on)  gives less completions in some cases (commands,...)
" 			the matching pattern has to match at the begining and
" 			is case sensitive. Furthermode  in expert mode, if
" 			completing a command and found less than 1 match then
" 			the function tries to close \(:\) or \[:\] (but not an
" 			environment, before doing ToDo in line 3832 there is
" 			no sense to make it).
" 			<Tab> or <F7> (if g:atp_no_tab_map=1)
" expert_mode = 0 (off) gives more matches but in some cases better ones, the
" 			string has to match somewhare and is case in
" 			sensitive, for example:
" 			\arrow<Tab> will show all the arrows definded in tex,
" 			in expert mode there would be no match (as there is no
" 			command in tex which begins with \arrow).
" 			<S-Tab> or <S-F7> (if g:atp_no_tab_map=1)
"
" ToDo: line 3832.
" ToDo: add math completion only if in math mode \(:\) or \[:\], but many
" people cab be used to $:$ and $$:$$.
" the pattern:
" \$\zs\([^\$]\|\\\)*\ze\$
" matches math modes (but not only, also the connecting parts, and it doesn't
" behave well with line breaks)
"
" Would it be hard to implement rules for completion
" environments are usually followed by \label, [...] or \end{}.
" ToDo: add closing for [:].


let g:atp_completion_modes=[ 
	    \ 'commands', 		'inline_math', 
	    \ 'displayed_math', 	'package_names', 
	    \ 'tikz_libraries', 	'environment_names', 
	    \ 'close_environments' , 	'labels', 
	    \ 'bibitems', 		'input_files',
	    \ 'bibfiles' ] 

if !exists("g:atp_completion_active_modes")
    let g:atp_completion_active_modes=g:atp_completion_modes
endif


" arguments are the same as for extend(), but it adds only the entries which
" are not present.
function! s:extend(list_a,list_b,...)
    let l:list_a=deepcopy(a:list_a)
    let l:diff=[]

    for l:b in a:list_b
	if index(a:list_a,l:b) == '-1'
	    call add(l:diff, l:b)
	endif
    endfor
    if a:0 == 0
	return extend(deepcopy(l:list_a),l:diff)
    else
	return extend(deepcopy(l:list_a),l:diff,a:1)
    endif
endfunction

" CHECK: l:completion_method=end ?
function! Tab_Completion(expert_mode)

    " this specifies the default argument for CloseLastEnv()
    " in some cases it is better to append after than before.
    let b:append='i'

    let l:pos=getpos(".")
    let l:line=join(getbufline("%",l:pos[1]))
    let l:nchar=strpart(l:line,l:pos[2]-1,1)
    let l:l=strpart(l:line,0,l:pos[2]-1)
    let b:l=l:l	"DEBUG
    let l:n=strridx(l:l,'{')
    let l:m=strridx(l:l,',')
    let l:o=strridx(l:l,'\')
    let l:s=strridx(l:l,' ')
     
    let b:n=l:n
    let b:o=l:o
    let b:s=l:s

    let l:nr=max([l:n,l:m,l:o,l:s])
    let l:begin=strpart(l:l,l:nr+1)
    let b:begin=l:begin "DEBUG
    " what we are trying to complete: usepackage, environment.
    let l:pline=strpart(l:l,0,l:nr)
    let b:pline=l:pline	"DEBUG
    if l:pline =~ '\\usepackage\%([.*]\)\?\s*'
	if index(g:atp_completion_active_modes, 'package_names') != '-1'
	    let l:completion_method='package'
	    let b:comp_method='package' "DEBUG
	else
	    return ''
	endif
    elseif l:pline =~ '\\usetikzlibrary\%([.*]\)\?\s*'
	if index(g:atp_completion_active_modes, 'tikz_libraries') != '-1'
	    let l:completion_method='tikz_libraries'
	    let b:comp_method='tikz_libraries' "DEBUG
	else
	    return ''
	endif
    elseif l:pline =~ '\%(\\begin\|\\end\)\s*$' && l:begin !~ '}.*$'
	if index(g:atp_completion_active_modes, 'environment_names') != '-1'
	    let l:completion_method='environment_names'
	    let b:comp_method='begin' "DEBUG
	else
	    return ''
	endif
    elseif (l:pline =~ '\\begin\s*$' && l:begin =~ '}\s*$') || ( l:pline =~ '\\begin\s*{[^}]*}\s*\\label' )
	if index(g:atp_completion_active_modes, 'close_environments') != '-1'
	    let l:completion_method='end'
	    let b:comp_method='end' "DEBUG
	else
	    return ''
	endif
    elseif l:o > l:n && l:o > l:s && 
	\ l:pline !~ '\%(input\|include\%(only\)\?\|[^\\]\\\\[^\\]$\)' &&
	\ l:begin !~ '{\|}\|,\|-\|\^\|\$\|(\|)\|&\|-\|+\|=\|#\|:\|;\|\.\|,\||\|?$' &&
		\ l:begin !~ '^\[\|\]\|-\|{\|}\|(\|)'
	" in this case we are completeing a command
	" the last match are the things which for sure do not ends any
	" command. The pattern '[^\\]\\\\[^\\]$' do not matches "\" and "\\\",
	" in which case the line contains "\\" and "\\\\" ( = line ends!)
	" (here "\" is one character \ not like in magic patterns '\\')
	" but matches "" and "\\" (i.e. when completing "\" or "\\\" [end line
	" + command].
	if index(g:atp_completion_active_modes, 'commands') != '-1'
	    let l:completion_method='command'
	    let b:comp_method='command' "DEBUG
	else
	    return ''
	endif
    elseif l:pline =~ '\\\%(eq\)\?ref\s*$'
	if index(g:atp_completion_active_modes, 'labels') != '-1'
	    let l:completion_method='labels'
	    let b:comp_method='label'  "DEBUG	
	else
	    return ''
	endif
    elseif l:pline =~ '\\\%(no\)\?cite'
	if index(g:atp_completion_active_modes, 'bibitems') != '-1'
	    let l:completion_method='bibitems'
	    let b:comp_method='bibitems'  "DEBUG	
	    if l:begin =~ '}\s*$'
		return ''
	    endif 
	else
	    return ''
	endif
    elseif (l:pline =~ '\\input' || l:begin =~ 'input') ||
		\ (l:pline =~ '\\include' || l:begin =~ 'include') ||
		\ (l:pline =~ '\\includeonly' || l:begin =~ 'includeonly') 
	if l:begin =~ 'input'
	    let l:begin=substitute(l:begin,'.*\%(input\|include\%(only\)\?\)\s\?','','')
	endif
	if index(g:atp_completion_active_modes, 'input_files') != '-1'
	    let l:completion_method='inputfiles'
	    " DEBUG:
	    let b:comp_method='inputfiles'
	else
	    return ''
	endif
    elseif l:pline =~ '\\bibliography'
	if index(g:atp_completion_active_modes, 'bibitems') != '-1'
	    let l:completion_method='bibfiles'
	    " DEBUG:
	    let b:comp_method='bibfiles'
	else
	    return ''
	endif
    else
	if index(g:atp_completion_active_modes, 'close_environments') != '-1'
	    let l:completion_method='close_env'
	    "DEBUG:
	    let b:comp_method='close_env' 
	else
	    return ''
	endif
    endif

    " if the \[ is not closed we prefer to first close it and then to complete
    " the commands, it is better as then automatic tex will have better file
    " to operate on.
    
"     echomsg join(getpos("."))
"     let l:pos=getpos(".")
"     let l:pos_changed=0
"     if l:pos[2]>1
" 	let l:pos[2]-=1
" 	let l:pos_changed=1
"     endif
"     call setpos(".",l:pos)
    " ToDo: envrionments should be called with name! 
    " and this is known later :(
"     let l:env_lnr=search('\\begin\s*{','bnW')
"     let l:env_name=matchstr(getline(l:env_lnr),'\\begin\s*{\zs[^}]*\ze}\%(.*\\begin\s*{\)\@!')
    let l:env_opened 	= s:Check_if_Opened('\\begin{','\\end{',
				\ line('.'),g:atp_completion_limits[2],2)
    let b:env_opened = l:env_opened
    if l:env_opened != 0
	let l:env_lnr=l:env_opened
	let l:env_name=matchstr(getline(l:env_lnr),'\\begin\s*{\zs[^}]*\ze}\%(.*\\begin\s*{\)\@!')
	let b:env_name=l:env_name " DEBUG
	let l:env_closed 	= s:Check_if_Closed('\\begin{' . l:env_name,'\\end{' . l:env_name,
				\ line('.'),g:atp_completion_limits[2],1)
    else
	let l:env_closed=1
	let l:env_name=0 	" this is compatible with CloseLastEnv() function (argument for a:3).
    endif
    let l:imath_closed	= s:Check_if_Closed(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line('.'),g:atp_completion_limits[0])
    let l:imath_opened	= s:Check_if_Opened(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line('.'),g:atp_completion_limits[0])
    let l:dmath_closed	= s:Check_if_Closed(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1])
    let l:dmath_opened	= s:Check_if_Opened(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1])
    " DEBUG:
"     echomsg "ic " l:imath_closed 		. " io " . l:imath_opened . 
" 		\ " dc " . l:dmath_closed 	. " do " . l:dmath_opened . 
" 		\ " ec " . l:env_closed 	. " eo " . l:env_opened
    let b:imath_closed=l:imath_closed
    let b:imath_opened=l:imath_opened
    let b:dmath_closed=l:dmath_closed
    let b:dmath_opened=l:dmath_opened
    let b:env_closed = l:env_closed " DEBUG
    let b:env_opened = l:env_opened " DEBUG

"     if l:pos_changed==1
" 	 let l:pos[2]+=1
" 	 let l:pos_changed=0
" 	 call setpos(".",l:pos)
"     endif
"     if l:completion_method=='command' && g:atp_complete_math_env_first
" 	 if !s:Check_if_Closed('\\\[','\\\]',line('.'),g:atp_completion_limits[1]) && !s:Check_if_Closed('\\(','\\)',line('.'),g:atp_completion_limits[1])
" " 	 if !l:env_closed && !
" 	     let l:completion_method='close_env'
" 	     let b:comp_method='close_env'
" 	     let b:append='a'
" 	 endif
"     endif

" echomsg "TAB_COMPLETION DEBUG " .  l:completion_method
    if l:completion_method=='close_env'
" 	    echomsg " DEBUG OK " 
" 	    echomsg "imath closed:open " . l:imath_closed . ":" . l:imath_opened 
" 	    echomsg "dmath closed:open " . l:dmath_closed . ":" . l:dmath_opened
" 	    echomsg "env   closed:open " . l:env_closed	  . ":" . l:env_opened
" 	let b:debugg = !l:env_closed || !l:imath_closed || !l:dmath_closed
	if !l:env_closed || !l:imath_closed || !l:dmath_closed
	    if !l:imath_closed && l:imath_opened 
		let b:tc_return="close_env inl"
		call CloseLastEnv(b:append,'inline_math')
		return ''
	    elseif !l:dmath_closed && l:dmath_opened
		let b:tc_return="close_env disp"
		call CloseLastEnv(b:append,'displayed_math')
		return ''
	    else
"           elseif !l:env_closed && l:env_opened	
		" the env name above might be not the one because it is looked
		" using '\\begin' and '\\end' this might be not enough,
		" however the function CloseLastEnv works prefectly and this
		" should be save:
		call CloseLastEnv(b:append,'environment')
		let b:tc_return="close_env env_name "  . l:env_name . " closed:" . l:env_closed . " opened:" . l:env_opened 
		return ''
	    endif
	endif
" 	if !exists("b:tc_return")
" 	    call CloseLastEnv(b:append,'environment')
" 	    let b:tc_return="close_env XY"
" 	endif

	" unlet variables if there were defined.
	if exists("l:completion_list")
	    unlet l:completion_list
	endif
	if exists("l:completions")
	    unlet l:completions
	endif
	return ''
    endif

    " generate the completion names
    " ------------ BEGIN --------------
    if l:completion_method == 'environment_names'
	let l:end=strpart(l:line,l:pos[2]-1)
	if l:end !~ '\s*}'
	    let l:completion_list=deepcopy(g:atp_environments)
	    if g:atp_local_completion_lists
		let l:completion_list=s:extend(l:completion_list,b:atp_local_environments)
	    endif
	    let l:completion_list=s:Add_to_List(l:completion_list,'}')
	else
	    let l:completion_list=deepcopy(g:atp_environments)
	    if g:atp_local_completion_lists
		call s:extend(l:completion_list, b:atp_local_environments)
	    endif
	endif
		    " TIKZ
		    if s:Search_Package('tikz') && 
				\ ( !g:atp_check_if_opened || 
				\ s:Check_if_Opened('\\begin{tikzpicture}','\\end{tikzpicture}',line('.'),80) || 
				\ s:Check_if_Opened('\\tikz{','}',line("."),g:atp_completion_limits[2]) )
			if l:end !~ '\s*}'
			    call deepcopy(extend(l:completion_list,s:Add_to_List(g:atp_tikz_environments,'}')))
			else
			    call deepcopy(extend(l:completion_list,g:atp_tikz_environments))
			endif
		    endif
		    " AMSMATH
		    let b:ddebug=0
		    if s:Search_Package('amsmath') || g:atp_amsmath == 1 || s:Document_Class() =~ '^ams'
			let b:ddebug=2
			if l:end !~ '\s*}'
			    call deepcopy(extend(l:completion_list,s:Add_to_List(g:atp_amsmath_environments,'}'),0))
			else
			    call deepcopy(extend(l:completion_list,g:atp_amsmath_environments,0))
			endif
		    endif
    " ------------ PACKAGE ---------------
    elseif l:completion_method == 'package'
	let l:completion_list=deepcopy(g:atp_package_list)    
    " ------------ TIKZ LIBRARIES --------
    elseif l:completion_method == 'tikz_libraries'
	let l:completion_list=deepcopy(g:atp_tikz_libraries)
    " ------------ COMMAND ---------------
    elseif l:completion_method == 'command'
	let l:obegin=strpart(l:l,l:o+1)
	let l:completion_list=[]

		" Are we in the math mode?
		let l:math_is_opened=0
		if g:atp_math_opened
		    for l:key in g:atp_math_modes
			if s:Check_if_Opened(l:key[0],l:key[1],line("."),g:atp_completion_limits[2])
			    let l:math_is_opened=1
			    let b:math_is_opened=l:key
			    break
			endif
		    endfor
		endif

		" if math is not opened or we do not check for math mode
		if ( !g:atp_math_opened || !l:math_is_opened )
		    let l:completion_list=deepcopy(g:atp_commands)
		endif


		" if we are in math mode or if we do not check for it ...
" 		let b:adebug=0
" 		echomsg "DEBUG " . g:atp_no_math_command_completion != 1 &&  ( !g:atp_math_opened  || l:math_is_opened )
		if g:atp_no_math_command_completion != 1 &&  ( !g:atp_math_opened || l:math_is_opened )
" 		    let b:adebug=1
		    " add commands if thier package is declared.
		    " AMSMATH
		    let b:debug="no amsmath commands"
		    if g:atp_amsmath == 1 || s:Search_Package('amsmath') || 
				\ s:Search_Package('amssymb') || s:Document_Class() =~ '^ams'
			let b:debug="amsmath commands added"
			if a:expert_mode == 0
			    call deepcopy(extend(l:completion_list,g:atp_math_commands_non_expert_mode))
			endif
			if g:atp_math_commands_first == 1
			    call deepcopy(extend(l:completion_list,g:atp_amsmath_commands,0))
			    call deepcopy(extend(l:completion_list,g:atp_math_commands,0))
			else
			    call deepcopy(extend(l:completion_list,g:atp_math_commands,len(l:completion_list)))
			    call deepcopy(extend(l:completion_list,g:atp_amsmath_commands,len(l:completion_list)))
			endif
		    endif
		    if s:Search_Package('amssymb')
			call deepcopy(extend(l:completion_list,g:atp_ams_negations))
			if a:expert_mode == 0 
			    call deepcopy(extend(l:completion_list,g:atp_ams_negations_non_expert_mode))
			endif
		    endif
		    " TIKZ 
		    if s:Search_Package('tikz')
			call deepcopy(extend(l:completion_list,g:atp_tikz_commands))
		    endif
		    " NICEFRAC
		    if s:Search_Package('nicefrac')
			call add(l:completion_list,'nicefrac')
		    endif
		    " FANCYHDR
		    if s:Search_Package('fancyhdr')
			call deepcopy(extend(l:completion_list,g:atp_fancyhdr_commands))
		    endif
		    " LOCAL COMMNADS
		    if g:atp_local_completion_lists
			call deepcopy(extend(l:completion_list,b:atp_local_commands))
		    endif
		    " ToDo: LAYOUT and many more packages.

		endif
		
" change the \label{ to \label{short_env_name, also adds it if we are labeling an item (but only if \label is just after \itme\s*\([ ]\)\s* (in the item text one want to have a diffrent prefix).
	let l:env_name=substitute(l:pline,'.*\%(\\\%(begin\|end.*\){\(.\{-}\)}.*\|\\\%(\(item\)\s*\)\%(\[.*\]\)\?\s*$\)','\1\2','') 
	if l:env_name =~ '\\\%(\%(sub\)\?paragraph\|\%(sub\)*section\|chapter\|part\)'
	    let l:env_name=substitute(l:env_name,'.*\\\(\%(sub\)\?paragraph\|\%(sub\)*section\|chapter\|part\).*','\1','')
	endif
	let l:env_name=substitute(l:env_name,'\*$','','')
	" if the pattern did not work do not put the env name.
	" for example \item cos\lab<Tab> the pattern will not work and we do
	" not want env name. 
	if l:env_name == l:pline
	    let l:env_name=''
	endif
	let b:env_name=l:env_name " DEBUG

	if has_key(g:atp_shortname_dict,l:env_name)
	    if g:atp_shortname_dict[l:env_name] != 'no_short_name' && g:atp_shortname_dict[l:env_name] != '' 
		let l:short_env_name=g:atp_shortname_dict[l:env_name]
		let l:no_separator=0
	    else
		let l:short_env_name=''
		let l:no_separator=1
	    endif
	else
	    let l:short_env_name=''
	    let l:no_separator=1
	endif

" 	if index(g:atp_no_separator_list,l:env_name) != -1
" 	    let l:no_separator = 1
" 	endif

	if g:atp_env_short_names == 1
	    if l:no_separator == 0 && g:atp_no_separator == 0
		let l:short_env_name=l:short_env_name . g:atp_separator
	    endif
	else
	    let l:short_env_name=''
	endif

" 	let b:no_sep=l:no_separator " DEBUG
	call deepcopy(extend(l:completion_list, [ 'label{' . l:short_env_name ],0))

    " ----------- LABELS ------------------
    elseif l:completion_method == 'labels'
	let l:completion_list=[]
	let l:precompletion_list=deepcopy(values(s:generatelabels(fnamemodify(bufname("%"),":p"))[fnamemodify(bufname("%"),":p")]))
	for l:label in l:precompletion_list
	    call add(l:completion_list,l:label . '}')
	endfor

    " ----------- TEX_INPUTFILES ----------------- 
    elseif l:completion_method ==  'inputfiles'
	let l:inputfiles=s:Find_files(g:texmf,1,".tex")
	let l:completion_list=[]
	for l:key in l:inputfiles
	    call add(l:completion_list,fnamemodify(l:key,":t:r"))
	endfor
	call sort(l:completion_list)
    " ----------- BIBFILES ----------------- 
    elseif l:completion_method ==  'bibfiles'
	let l:bibfiles=[]
	for l:dir in g:atp_bibinputs
	    let l:bibfiles=extend(l:bibfiles,s:Find_files(l:dir,0,".bib"))
	endfor
	let l:completion_list=[]
	for l:key in l:bibfiles
	    call add(l:completion_list,fnamemodify(l:key,":t:r"))
	endfor
	call sort(l:completion_list)
    " ----------- BIBITEMS ----------------- 
    elseif l:completion_method == 'bibitems'
	let l:bibitems_list=values(s:searchbib(''))
	let b:bibitems_list=l:bibitems_list
	let l:pre_completion_list=[]
	let l:completion_list=[]
	for l:dict in l:bibitems_list
	    for l:key in keys(l:dict)
		call add(l:pre_completion_list,l:dict[l:key]['KEY']) 
	    endfor
	endfor
	for l:key in l:pre_completion_list
	    call add(l:completion_list,substitute(strpart(l:key,max([stridx(l:key,'{'),stridx(l:key,'(')])+1),',\s*','',''))
	endfor

	" add the \bibitems found in include files
	call deepcopy(extend(l:completion_list,keys(s:Search_Bib_Items(b:atp_mainfile))))
    endif
    if exists("l:completion_list")
	let b:completion_list=l:completion_list	" DEBUG
    endif

    " make the list of matching items
    if l:completion_method != 'end' && l:completion_method != 'env_close'
	let l:completions=[]
	for l:item in l:completion_list
	    " Packages, environments, labels, bib and input files must match
	    " at the beginning (in expert_mode).
	    if (l:completion_method == 'package' ||
			\ l:completion_method == 'environment_names' ||
			\ l:completion_method == 'labels' ||
			\ l:completion_method == 'bibfiles' )
		if a:expert_mode == 1 && l:item =~ '\C^' . l:begin
		    call add(l:completions,l:item)
		elseif a:expert_mode!=1 && l:item =~ l:begin
		    call add(l:completions,l:item)
		endif
	    " Bibitems match not only in the beginning!!! 
	    elseif (l:completion_method == 'bibitems' ||
			\ l:completion_method == 'tikz_libraries' ||
			\ l:completion_method == 'inputfiles') &&
			\ l:item =~ l:begin
		call add(l:completions,l:item)
	    " Commands must match at the beginning (but in a different way)
	    " (only in epert_mode).
	    elseif l:completion_method == 'command' 
		if l:nchar == '{'
		    if a:expert_mode == 1 && l:item =~ '\C^' . l:obegin && l:item =~ '.*{[}\s]*$'
			call add(l:completions, '\' . substitute(l:item,'{[}\s]*$','',''))
		    elseif a:expert_mode != 1 && l:item =~  l:obegin && l:item =~ '.*{[}\s]*$'
			call add(l:completions, '\' . substitute(l:item,'{[}\s]*$','',''))
		    endif
		else
		    if a:expert_mode == 1 && l:item =~ '\C^' . l:obegin
			call add(l:completions, '\' . l:item)
		    elseif a:expert_mode != 1 && l:item =~  l:obegin
			call add(l:completions, '\' . l:item)
		    endif
		endif
	    endif
	endfor
    else
	" preserve the indentation
	let l:indent=substitute(l:l,'^\(\s*\)\\begin.*','\1','')
	let b:indent=l:indent " DEBUG
	" 	LAST CHANGE
" 	call append(line("."),l:indent . '\end{' . substitute(l:l,'.*\\begin{\(.\{-}}\).*','\1',''))
	let b:tc_return="1"
	call CloseLastEnv('a','environment')
	return ''
    endif

    let b:completions=l:completions " DEBUG

    " if the list is long it is better if it is sorted, if it short it is
    " better if the more used things are at the begining.
    if len(l:completions) > 5 && l:completion_method != 'labels'
	let l:completions=sort(l:completions)
    endif

    if l:completion_method == 'environment_names' || l:completion_method == 'package' || 
		\ l:completion_method == 'tikz_libraries' 	|| l:completion_method == 'labels' ||
		\ l:completion_method == 'bibitems' 		|| l:completion_method == 'bibfiles' || 
		\ l:completion_method == 'bibfiles'		|| l:completion_method == 'inputfiles'
	call complete(l:nr+2,l:completions)
	let b:tc_return="2"
    elseif l:completion_method == 'command'
	call complete(l:o+1,l:completions)
	let b:tc_return="3 X"
    endif

    " If the completion method was a command (probably in a math mode) and
    " there was no completion, check if the \[ and \( are closed.
    if l:completion_method == 'command' && (len(l:completions) == 0 && a:expert_mode ||
	\ len(l:completions) == 1 && l:completions[0] == '\'. l:begin ) &&
	\ (!s:Check_if_Closed(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line("."),g:atp_completion_limits[1]) ||
	\ !s:Check_if_Closed(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line("."),g:atp_completion_limits[0]) )

	" DEBUG:
	let b:dmc=s:Check_if_Closed(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1])
	let b:dmo=s:Check_if_Opened(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1])

	if !s:Check_if_Closed(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1]) && s:Check_if_Opened(g:atp_math_modes[1][0],g:atp_math_modes[1][1],line('.'),g:atp_completion_limits[1])
	    let l:a='disp' " DEBUG
	    call CloseLastEnv('i','displayed_math')
	elseif !s:Check_if_Closed(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line('.'),g:atp_completion_limits[0]) && s:Check_if_Opened(g:atp_math_modes[0][0],g:atp_math_modes[0][1],line('.'),g:atp_completion_limits[1])
	    call CloseLastEnv('i','inline_math')
	    let l:a='inl' " DEBUG
	elseif !s:Check_if_Closed('\\begin\s*{','\\end\*{',line("."),g:atp_completion_limits[2]) &&
		    \ s:Check_if_Opened('\\begin\s*{','\\end\*{',line("."),g:atp_completion_limits[2])
	    call CloseLastEnv('a','environment')
	    let l:a='env' " DEBUG
	endif
	let b:comp_method='close_env' "DEBUG
	if exists("l:a")
	    let b:tc_return="close_env end " . l:a
	else
	    let b:tc_return="close_env end"
	endif
    endif

"  ToDo: (a chalanging one)  
"  Move one step after completion is done (see the condition).
"  for this one have to end till complete() function will end, and this I do
"  not know how to do. 
"     let b:check=0
"     if l:completion_method == 'environment_names' && l:end =~ '\s*}'
" 	let b:check=1
" 	let l:pos=getpos(".")
" 	let l:pos[2]+=1
" 	call setpos(".",l:pos) 
"     endif
"
    " unlet variables if there were defined.
    if exists("l:completion_list")
	unlet l:completion_list
    endif
    if exists("l:completions")
	unlet l:completions
    endif
    return ''
endfunction

" ------- Wrap Seclection ----------------------------
if !exists("*WrapSelection")
function! WrapSelection(wrapper)
    normal `>a}
    exec 'normal `<i\'.a:wrapper.'{'
endfunction
endif

" --------------- FD FILES -----------------

" FontSearch([<pattern>,<method>]) 
" method = 0 match for name of fd file
" method = 1 match againts whole path
if !exists("*FontSearch")
function! FontSearch(...)
	
    if a:0 == 0
	let l:pattern=""
	let l:method=0
    else
	let l:pattern=a:1
	if a:0 == 1
	    let l:method=0
	else
	    let l:method=1
	endif
    endif
    let b:a=a:0
    let b:method=l:method

    " Find fd file
    let l:path=substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
    let l:path=substitute(l:path,':\|\n',',','g')
    let b:path=l:path
    let l:fd=split(globpath(l:path,"**/*.fd"),'\n') 
    let b:fd=l:fd

    " Match for l:pattern
    let s:fd_matches=[]
    for l:fd_file in l:fd
	if (l:method==0 && fnamemodify(l:fd_file,":t") =~ l:pattern) ||
		    \ (l:method==1 && l:fd_file =~ l:pattern)
	    call add(s:fd_matches,l:fd_file)
	endif
    endfor
    let b:fd_matches=s:fd_matches

    " Open Buffer and list fd files
    " set filetype to fd_atp
    let l:tmp_dir=tempname()
    call mkdir(l:tmp_dir)
    let l:fd_bufname="fd_list " . l:pattern
    let l:openbuffer="32vsplit! +setl\\ nospell\\ ft=fd_atp ". fnameescape(l:tmp_dir . "/" . l:fd_bufname )

    let g:fd_matches=[]
    if len(s:fd_matches) > 0
	echohl WarningMsg
	echomsg "Found " . len(s:fd_matches) . " files."
	echohl None
	" wipe out the old buffer and open new one instead
	if buflisted(fnameescape(l:tmp_dir . "/" . l:fd_bufname))
" 	    echomsg "DEBUG DELETE BUFFER"
	    silent exe "bd! " . bufnr(fnameescape(l:tmp_dir . "/" . l:fd_bufname))
	endif
	silent exe l:openbuffer
	" make l:tmp_dir available for this buffer.
	let b:tmp_dir=l:tmp_dir
	cd /tmp
	map <buffer> q	:bd<CR>

	" print the lines into the buffer
	let l:i=0
	call setline(1,"Press Enter to open a font definition file:")
	for l:fd_file in s:fd_matches
	    " we put in line the last directory/fd_filename:
	    " this is what we cut:
	    let l:path=fnamemodify(l:fd_file,":h:h")
	    let l:fd_name=substitute(l:fd_file,"^" . l:path . '/\?','','')
" 	    call setline(line('$')+1,fnamemodify(l:fd_file,":t"))
	    call setline(line('$')+1,l:fd_name)
	    call add(g:fd_matches,l:fd_file)
	    let l:i+=1
	endfor
	silent w
	setlocal nomodifiable
	setlocal ro
    else
	echohl WarningMsg
	echomsg "No fd file found."
	echohl None
    endif

endfunction
endif

if !exists("*Fd_completion")
function! Fd_completion(A,C,P)
    	
    " Find all files
    let l:matches=[]
    for l:fd_file in s:fd_matches
	if l:fd_file =~ a:A
	    call add(l:matches,l:fd_file)
	endif
    endfor

endfunction
endif

" function! DeleteTempBuffers()
"     " name of the temp ir without '/' at the end
"     let l:tmp_dir=substitute(tempname(),'\/\d\+','','g')
"     execute "bufdo " 
" endfunction

" -------------- RELOAD ---------------------

if !exists("g:atp_debug")
    let g:atp_debug=0
endif
if g:atp_debug==1 && !exists("*Reload")
" Reload() - reload all the tex_apt functions
" Reload(func1,func2,...) reload list of functions func1 and func2
fun! Reload(...)
    let l:bufname=fnamemodify(expand("%"),":p")

    if a:0 == 0
	let l:runtime_path=split(&runtimepath,',')
	echo "Searching for atp plugin files"
	let l:file_list=['ftplugin/tex_atp.vim','ftplugin/fd_atp.vim', 'ftplugin/bibsearch_atp.vim', 'ftplugin/toc_atp.vim']
	let l:file_path=[]
	for l:file in l:file_list
		call add(l:file_path,globpath(&rtp,l:file))
	endfor
	for l:file in l:file_path
	    echomsg "DELETING FUNCTIONS FROM " . l:file
	    let l:atp=readfile(l:file)
	    for l:line in l:atp
		let l:function_name=matchstr(l:line,'^\s*fun\%(ction\)\?!\?\s\+\zs\<[^(]*\>\ze(')
		if l:function_name != "" && l:function_name != "Reload"
		    if exists("*" . l:function_name)
			if exists("b:atp_debug")
			    if b:atp_debug=="v" || b:atp_debug=="verbose"
				echomsg "deleting function " . l:function_name
			    endif
			endif
			execute "delfunction " . l:function_name
		    endif
		endif
	    endfor
	endfor
    else
	let l:f_list=split(a:1,',')
	let g:f_list=l:f_list
	for l:function in l:f_list
	    execute "delfunction " . l:function
	endfor
    endif
    w
"   THIS IS THE SLOW WAY:
    bd!
    execute "edit " . l:bufname 
"   This could be faster: but aparently doesn't work.
"     execute "source " . l:file_path[0]
endfunction
endif
command -buffer -nargs=* Reload	:call Reload(<f-args>)


"--------- MAPPINGS -------------------------------------------------------
" Add mappings, unless the user didn't want this.
" ToDo: to doc.
if !exists("no_plugin_maps") && !exists("no_atp_maps")
    " ToDo to doc.
    if exists("g:atp_no_tab_map") && g:atp_no_tab_map == 1
	inoremap <buffer> <F7> <C-R>=Tab_Completion(1)<CR>
	inoremap <buffer> <S-F7> <C-R>=Tab_Completion(0)<CR>
    else
	inoremap <buffer> <Tab> <C-R>=Tab_Completion(1)<CR>
	inoremap <buffer> <S-Tab> <C-R>=Tab_Completion(0)<CR>
	vmap <buffer> <silent> <F7> <Esc>:call WrapSelection('')<CR>i
    endif

    map  <buffer> <LocalLeader>v		:call ViewOutput() <CR><CR>
    map  <buffer> <F2> 				:ToggleSpace<CR>
    map  <buffer> <F3>        			:call ViewOutput() <CR><CR>
    imap <buffer> <F3> <Esc> 			:call ViewOutput() <CR><CR>
    map  <buffer> <LocalLeader>g 		:call Getpid()<CR>
    map  <buffer> <LocalLeader>t		:TOC<CR>
    map  <buffer> <LocalLeader>L		:Labels<CR>
    map  <buffer> <LocalLeader>l 		:TEX<CR>	
    map  <buffer> 2<LocalLeader>l 		:2TEX<CR>	 
    map  <buffer> 3<LocalLeader>l		:3TEX<CR>
    map  <buffer> 4<LocalLeader>l		:4TEX<CR>
    " imap <buffer> <LocalLeader>l	<Left><ESC>:TEX<CR>a
    " imap <buffer> 2<LocalLeader>l	<Left><ESC>:2TEX<CR>a
    " todo: this is nice idea but it do not works as it should: 
    " map  <buffer> <f4> [d:let nr = input("which one: ")<bar>exe "normal " . nr . "[\t"<cr> 
    map  <buffer> <F5> 				:call VTEX() <cr>	
    map  <buffer> <s-F5> 			:call ToggleAuTeX()<cr>
    imap <buffer> <F5> <left><esc> 		:call VTEX() <cr>a
    map  <buffer> <localleader>sb		:call SimpleBibtex()<cr>
    map  <buffer> <localleader>b		:call Bibtex()<cr>
    map  <buffer> <F6>d 			:call Delete() <cr>
    imap <buffer> <silent> <F6>l 		:call OpenLog() <cr>
    map  <buffer> <silent> <F6>l 		:call OpenLog() <cr>
    map  <buffer> <localleader>e 		:cf<cr> 
    map  <buffer> <F6>e 			:ShowErrors e<cr>
    imap <buffer> <F6>e 			:ShowErrors e<cr>
    map  <buffer> <F6>w 			:ShowErrors w<cr>
    imap <buffer> <F6>w 			:ShowErrors w<cr>
    map  <buffer> <F6>r 			:ShowErrors rc<cr>
    imap <buffer> <F6>r 			:ShowErrors rc<cr>
    map  <buffer> <F6>f 			:ShowErrors f<cr>
    imap <buffer> <F6>f 			:ShowErrors f<cr>
    map  <buffer> <F6>g 			:call PdfFonts()<cr>
    map  <buffer> <F1> 	   			:!clear;texdoc -m 
    imap <buffer> <F1> <esc> 			:!clear;texdoc -m  
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

    if g:atp_no_env_maps != 1
	imap <buffer> [b \begin{}<Left>
	imap <buffer> [e \end{}<Left>

	imap <buffer> ]c \begin{center}<Cr>\end{center}<Esc>O
	imap <buffer> [c \begin{corollary}<Cr>\end{corollary}<Esc>O
	imap <buffer> [d \begin{definition}<Cr>\end{definition}<Esc>O
	imap <buffer> ]e \begin{enumerate}<Cr>\end{enumerate}<Esc>O
	imap <buffer> [a \begin{align}<Cr>\end{align}<Esc>O
	imap <buffer> [i \item
	imap <buffer> ]i \begin{itemize}<Cr>\end{itemize}<Esc>O
	imap <buffer> [l \begin{lemma}<Cr>\end{lemma}<Esc>O
	imap <buffer> ]p \begin{proof}<Cr>\end{proof}<Esc>O
	imap <buffer> [p \begin{proposition}<Cr>\end{proposition}<Esc>O
	imap <buffer> [t \begin{theorem}<Cr>\end{theorem}<Esc>O
	imap <buffer> ]t \begin{center}<CR>\begin{tikzpicture}<CR><CR>\end{tikzpicture}<CR>\end{center}<Up><Up>

	if g:atp_extra_env_maps == 1
	    imap <buffer> [r \begin{remark}<Cr>\end{remark}<Esc>O
	    imap <buffer> ]l \begin{flushleft}<Cr>\end{flushleft}<Esc>O
	    imap <buffer> ]r \begin{flushright}<Cr>\end{flushright}<Esc>O
	    imap <buffer> [f \begin{frame}<Cr>\end{frame}<Esc>O
	    imap <buffer> ]q \begin{equation}<Cr>\end{equation}<Esc>O
	    imap <buffer> [n \begin{note}<Cr>\end{note}<Esc>O
	    imap <buffer> [o \begin{observation}<Cr>\end{observation}<Esc>O
	    imap <buffer> [x \begin{example}<Cr>\end{example}<Esc>O
	endif

	" imap {c \begin{corollary*}<Cr>\end{corollary*}<Esc>O
	" imap {d \begin{definition*}<Cr>\end{definition*}<Esc>O
	" imap {x \begin{example*}\normalfont<Cr>\end{example*}<Esc>O
	" imap {l \begin{lemma*}<Cr>\end{lemma*}<Esc>O
	" imap {n \begin{note*}<Cr>\end{note*}<Esc>O
	" imap {o \begin{observation*}<Cr>\end{observation*}<Esc>O
	" imap {p \begin{proposition*}<Cr>\end{proposition*}<Esc>O
	" imap {r \begin{remark*}<Cr>\end{remark*}<Esc>O
	" imap {t \begin{theorem*}<Cr>\end{theorem*}<Esc>O
    endif

    imap <buffer> __ _{}<Left>
    imap <buffer> ^^ ^{}<Left>
    imap <buffer> [m \[\]<Left><Left>
endif

" This is an additional syntax group for enironment provided by the TIKZ
" package, a very powerful tool to make beautiful diagrams, and all sort of
" pictures in latex.
syn match texTikzCoord '\(|\)\?([A-Za-z0-9]\{1,3})\(|\)\?\|\(|\)\?(\d\d)|\(|\)\?'

" COMMANDS
command! -buffer ViewOutput		:call ViewOutput()
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
command! -buffer -nargs=* FontSearch	:call FontSearch(<f-args>)
command! -buffer TOC 			:call TOC()
command! -buffer CTOC 			:call CTOC()
command! -buffer Labels			:call Labels() 
command! -buffer SetOutDir 		:call s:setoutdir(1)
command! -buffer ATPStatus 		:call ATPStatus() 
command! -buffer PdfFonts		:call PdfFonts()
command! -buffer -nargs=? 					SetErrorFormat 	:call s:SetErrorFormat(<f-args>)
command! -buffer -nargs=? -complete=custom,ListErrorsFlags 	ShowErrors 	:call s:ShowErrors(<f-args>)
command! -buffer -nargs=? -complete=buffer	 		FindInputFiles	:call FindInputFiles(<f-args>)
command! -buffer -nargs=* -complete=customlist,EI_compl	 	EditInputFile 	:call EditInputFile(<f-args>)
command! -buffer -nargs=? -complete=buffer	 ToDo 			:call ToDo('\c\<todo\>','\s*%\c.*\<note\>',<f-args>)
command! -buffer -nargs=? -complete=buffer	 Note			:call ToDo('\c\<note\>','\s*%\c.*\<todo\>',<f-args>)
command! -buffer SetXdvi		:call SetXdvi()
command! -buffer SetXpdf		:call SetXpdf()	
command! -complete=custom,ListPrinters  -buffer -nargs=* SshPrint	:call Print(<f-args>)
command! -buffer Lpstat	:call Lpstat()

command! -buffer -nargs=1 -complete=customlist,Env_compl NEnv			:call NextEnv(<f-args>)
command! -buffer -nargs=1 -complete=customlist,Env_compl PEnv			:call PrevEnv(<f-args>)
command! -buffer -nargs=? -complete=customlist,Env_compl NSec			:call NextSection('section',<f-args>)
command! -buffer -nargs=? -complete=customlist,Env_compl PSec			:call PrevSection('section',<f-args>)
command! -buffer -nargs=? -complete=customlist,Env_compl NChap			:call NextSection('chapter',<f-args>)
command! -buffer -nargs=? -complete=customlist,Env_compl PChap			:call PrevSection('chapter',<f-args>)
command! -buffer -nargs=? -complete=customlist,Env_compl NPart			:call NextSection('part',<f-args>)
command! -buffer -nargs=? -complete=customlist,Env_compl PPart			:call PrevSection('part',<f-args>)
command! -buffer ToggleSpace   		:call ToggleSpace()
command! -buffer ToggleCheckMathOpened 	:call ToggleCheckMathOpened()
command! -buffer ToggleDebugMode 	:call ToggleDebugMode()
command! -buffer ToggleCallBack 	:call ToggleCallBack()

" MENU
if !exists("no_plugin_menu") && !exists("no_atp_menu")
nmenu 550.10 &LaTeX.&Make<Tab>:TEX		:TEX<CR>
nmenu 550.10 &LaTeX.Make\ &twice<Tab>:2TEX	:2TEX<CR>
nmenu 550.10 &LaTeX.Make\ verbose<Tab>:VTEX	:VTEX<CR>
nmenu 550.10 &LaTeX.&Bibtex<Tab>:Bibtex	:Bibtex<CR>
" nmenu 550.10 &LaTeX.&Bibtex\ (bibtex)<Tab>:SBibtex		:SBibtex<CR>
nmenu 550.10 &LaTeX.&View<Tab>:ViewOutput 	:ViewOutput<CR>
"
nmenu 550.20.1 &LaTeX.&Errors<Tab>:ShowErrors				:ShowErrors<CR>
nmenu 550.20.1 &LaTeX.&Log.&Open\ Log\ File<Tab>:map\ <F6>l		:call OpenLog()<CR>
if b:atp_callback
    nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [on]<Tab>b:atp_callback		:call ToggleCallBack()<CR>
else
    nmenu 550.80 &LaTeX.Toggle\ &Call\ Back\ [off]<Tab>b:atp_callback		:call ToggleCallBack()<CR>
endif  
if g:atp_debug_mode
    nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [on]			:call ToggleDebugMode()<CR>
else
    nmenu 550.20.5 &LaTeX.&Log.Toggle\ &Debug\ Mode\ [off]		:call ToggleDebugMode()<CR>
endif  
nmenu 550.20.20 &LaTeX.&Log.-ShowErrors-	:
nmenu 550.20.20 &LaTeX.&Log.&Warnings<Tab>:ShowErrors\ w 			:ShowErrors w<CR>
nmenu 550.20.20 &LaTeX.&Log.&Citation\ Warnings<Tab>:ShowErrors\ c		:ShowErrors c<CR>
nmenu 550.20.20 &LaTeX.&Log.&Reference\ Warnings<Tab>:ShowErrors\ r		:ShowErrors r<CR>
nmenu 550.20.20 &LaTeX.&Log.&Font\ Warnings<Tab>ShowErrors\ f			:ShowErrors f<CR>
nmenu 550.20.20 &LaTeX.&Log.Font\ Warnings\ &&\ Info<Tab>:ShowErrors\ fi	:ShowErrors fi<CR>
nmenu 550.20.20 &LaTeX.&Log.&Show\ Files<Tab>:ShowErrors\ F			:ShowErrors F<CR>
"
nmenu 550.20.20 &LaTeX.&Log.-PdfFotns- :
nmenu 550.20.20 &LaTeX.&Log.&Pdf\ Fonts<Tab>:PdfFonts		:PdfFonts<CR>

nmenu 550.20.20 &LaTeX.&Log.-Delete-	:
nmenu 550.20.20 &LaTeX.&Log.&Delete\ Tex\ Output\ Files<Tab>:map\ <F6>d		:call Delete()<CR>
nmenu 550.20.20 &LaTeX.&Log.Set\ Error\ File<Tab>:SetErrorFile	:SetErrorFile<CR> 
"
nmenu 550.30 &LaTeX.-TOC- :
nmenu 550.30 &LaTeX.&Table\ of\ Contents<Tab>:TOC		:TOC<CR>
nmenu 550.30 &LaTeX.L&abels<Tab>:Labels			:Labels<CR>
"
nmenu 550.40 &LaTeX.&Go\ to.&EditInputFile<Tab>:EditInputFile		:EditInputFile<CR>
"
nmenu 550.40 &LaTeX.&Go\ to.-Environment- :
nmenu 550.40 &LaTeX.&Go\ to.Next\ Definition<Tab>:NEnv\ definition	:NEnv definition<CR>
nmenu 550.40 &LaTeX.&Go\ to.Previuos\ Definition<Tab>:PEnv\ definition	:PEnv definition<CR>
nmenu 550.40 &LaTeX.&Go\ to.Next\ Environment<Tab>:NEnv\ <arg>		:NEnv 
nmenu 550.40 &LaTeX.&Go\ to.Previuos\ Environment<Tab>:PEnv\ <arg>	:PEnv 
"
nmenu 550.40 &LaTeX.&Go\ to.-Section- :
nmenu 550.40 &LaTeX.&Go\ to.&Next\ Section<Tab>:NSec			:NSec<CR>
nmenu 550.40 &LaTeX.&Go\ to.&Previuos\ Section<Tab>:PSec		:PSec<CR>
nmenu 550.40 &LaTeX.&Go\ to.Next\ Chapter<Tab>:NChap			:NChap<CR>
nmenu 550.40 &LaTeX.&Go\ to.Previous\ Chapter<Tab>:PChap		:PChap<CR>
nmenu 550.40 &LaTeX.&Go\ to.Next\ Part<Tab>:NPart			:NPart<CR>
nmenu 550.40 &LaTeX.&Go\ to.Previuos\ Part<Tab>:PPart			:PPart<CR>
"
nmenu 550.50 &LaTeX.-Bib-			:
nmenu 550.50 &LaTeX.Bib\ Search<Tab>:Bibsearch\ <arg>			:BibSearch 
nmenu 550.50 &LaTeX.Find\ Bib\ Files<Tab>:FindBibFiles			:FindBibFiles<CR> 
nmenu 550.50 &LaTeX.Find\ Input\ Files<Tab>:FindInputFiles			:FindInputFiles<CR>
"
nmenu 550.60 &LaTeX.-Viewer-			:
nmenu 550.60 &LaTeX.Set\ &XPdf<Tab>:SetXpdf					:SetXpdf<CR>
nmenu 550.60 &LaTeX.Set\ X&Dvi\ (inverse\/reverse\ search)<Tab>:SetXdvi	:SetXdvi<CR>
"
nmenu 550.70 &LaTeX.-Editting-			:
"
" ToDo: show options doesn't work from the menu (it disappears immediately, but at
" some point I might change it completely)
nmenu 550.70 &LaTeX.&Options.&Show\ Options<Tab>:ShowOptions		:ShowOptions<CR> 
nmenu 550.70 &LaTeX.&Options.-set\ options- :
nmenu 550.70 &LaTeX.&Options.Automatic\ TeX\ Processing<Tab>b:autex	:let b:autex=
nmenu 550.70 &LaTeX.&Options.Set\ Runs<Tab>b:auruns			:let b:auruns=
nmenu 550.70 &LaTeX.&Options.Set\ TeX\ Compiler<Tab>b:texcompiler	:let b:texcompiler="
nmenu 550.70 &LaTeX.&Options.Set\ Viewer<Tab>b:Viewer			:let b:Viewer="
nmenu 550.70 &LaTeX.&Options.Set\ Viewer\ Options<Tab>b:ViewerOptions	:let b:ViewerOptions="
nmenu 550.70 &LaTeX.&Options.Set\ Output\ Directory<Tab>b:outdir	:let b:ViewerOptions="
nmenu 550.70 &LaTeX.&Options.Set\ Output\ Directory\ to\ the\ default\ value<Tab>:SetOutDir	:SetOutDir<CR> 
nmenu 550.70 &LaTeX.&Options.Ask\ for\ the\ Output\ Directory<Tab>g:askfortheoutdir		:let g:askfortheoutdir="
nmenu 550.70 &LaTeX.&Options.Open\ Viewer<Tab>b:openviewer		:let b:openviewer="
nmenu 550.70 &LaTeX.&Options.Open\ Viewer<Tab>b:openviewer		:let b:openviewer="
nmenu 550.70 &LaTeX.&Options.Set\ Error\ File<Tab>:SetErrorFile		:SetErrorFile<CR> 
nmenu 550.70 &LaTeX.&Options.Which\ TeX\ files\ to\ copy<Tab>g:keep	:let g:keep="
nmenu 550.70 &LaTeX.&Options.Tex\ extensions<Tab>g:texextensions	:let g:texextensions="
nmenu 550.70 &LaTeX.&Options.Remove\ Command<Tab>g:rmcommand		:let g:rmcommand="
nmenu 550.70 &LaTeX.&Options.Default\ Bib\ Flags<Tab>g:defaultbibflags	:let g:defaultbibflags="
"
nmenu 550.78 &LaTeX.&Toggle\ Space\ [off]				:ToggleSpace<CR>
if g:atp_math_opened
    nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [on]<Tab>g:atp_math_opened			:ToggleCheckMathOpened<CR>
else
    nmenu 550.79 &LaTeX.Toggle\ &Check\ if\ in\ Math\ [off]<Tab>g:atp_math_opened			:ToggleCheckMathOpened<CR>
endif
tmenu &LaTeX.&Toggle\ Space\ [off] cmap <space> \_s\+ is curently off
" ToDo: add menu for printing.
endif
ftplugin/plaintex_atp.vim	[[[1
1
source $HOME/.vim/ftplugin/tex_atp.vim
ftplugin/bibsearch_atp.vim	[[[1
108
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 May 31
" URL:		

"
" Status Line:

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

function! ATPBibStatus()
    return "Bibsearch: " . substitute(expand("%"),"___","","g")
endfunction
setlocal statusline=%{ATPBibStatus()}
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
    		echomsg "DEBUG " . l:which
    if l:which =~ '^\d*$'
		echomsg "DEBUG 1"
	let l:start=stridx(b:listofkeys[l:which],'{')+1
	let l:choice=substitute(strpart(b:listofkeys[l:which],l:start),',','','')
	q
	let l:line=getline(".")
	let l:col=col(".")
	let l:line=strpart(l:line,0,l:col) . l:choice . strpart(l:line,l:col)
	call setline(line("."), l:line)
    elseif l:which =~ '^\d*\(\a\|+\| . "*" .\)$'
		echomsg "DEBUG 2"
	    let l:letter=substitute(l:which,'\d','','g')
	    let l:which=substitute(l:which,'\a\|+\|' . "*",'','g')
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
367
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 May 31
" URL:		

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

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
    let l:sec_line=join(getbufline(l:bufname,l:line))
    let l:sec_type=""
    if l:sec_line =~ '\\subparagraph[^\*]'
	let l:sec_type="subparagraph"
    elseif l:sec_line =~ '\\subparagraph\*'
	let l:sec_type="subparagraph*"
    elseif l:sec_line =~ '\\paragraph[^\*]'
	let l:sec_type="paragraph"
    elseif l:sec_line =~ '\\paragraph\*'
	let l:sec_type="paragraph*"
    elseif l:sec_line =~ '\\subsubsection[^\*]'
	let l:sec_type="subsubsection"
    elseif l:sec_line =~ '\\subsubsection\*'
	let l:sec_type="subsubsection*"
    elseif l:sec_line =~ '\\subsection[^\*]'
	let l:sec_type="subsection"
    elseif l:sec_line =~ '\\subsection\*'
	let l:sec_type="subsection*"
    elseif l:sec_line =~ '\\section[^\*]'
	let l:sec_type="section"
    elseif l:sec_line =~ '\\section\*'
	let l:sec_type="section*"
    elseif l:sec_line =~ '\\chapter[^\*]'
	let l:sec_type="chapter"
    elseif l:sec_line =~ '\\chapter\*'
	let l:sec_type="chapter*"
    elseif l:sec_line =~ '\\part[^\*]'
	let l:sec_type="part"
    elseif l:sec_line =~ '\\part\*'
	let l:sec_type="part*"
    elseif l:sec_line =~ '\\bibliography'
	let l:sec_type="bibliography"
    elseif l:sec_line =~ '\\abstract'
	let l:sec_type="abstract"
    endif

    echo l:sec_type . " : " . strpart(l:sec_line,stridx(l:sec_line,'{')+1,stridx(l:sec_line,'}')-stridx(l:sec_line,'{')-1)


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

" To DoC
function! Help()
    echo "Available Mappings:"
    echo "q 		close ToC window"
    echo "<CR>  		go to and close"
    echo "<space>		go to"
    echo "c or y		yank the label to a register"
    echo "p		yank and paste the label (in the source file)"
    echo "e		echo the title to command line"
    echo "h		this help message"
endfunction

" MAPPINGS
if !exists("no_plugin_maps") && !exists("no_atp_toc_maps")
    map <silent> <buffer> q 		:bdelete<CR>
    map <silent> <buffer> <CR> 		:call GotoLine(1)<CR>
    map <silent> <buffer> <space> 	:call GotoLine(0)<CR>
" This does not work: 
"   noremap <silent> <buffer> <LeftMouse> :call GotoLine(0)<CR>
"   when the cursor is in another buffer (and the option mousefocuse is not
"   set) it calles the command instead of the function, I could add a check if
"   mouse is over the right buffer. With mousefocuse it also do not works very
"   well.
    map <buffer> c 			:call YankToReg()<CR>
    map <buffer> y 			:call YankToReg()<CR>
    noremap <silent> <buffer> p 	:call Paste()<CR>
    noremap <silent> <buffer> s 	:call ShowLabelContext()<CR> 
    noremap <silent> <buffer> e 	:call EchoLabel()<CR>
    noremap <silent> <buffer> <F1>	:call Help()<CR>
endif
ftplugin/fd_atp.vim	[[[1
234
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 May 31
" URL:		

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

if !exists("*OpenFile")
function! OpenFile()
    let l:line=max([line("."),'2'])-2
    let l:file=g:fd_matches[l:line]

    " The list of fd files starts at second line.
    let l:openbuffer="topleft split! +setl\\ nospell\\ ft=fd_atp\\ noro " . fnameescape(l:file)
    silent exe l:openbuffer
    let b:autex=0
endfunction
endif

" keep_tex=1 open the tex file of the sample file, otherwise it is deleted (at
" least from the bufer list).
function! s:Preview(fd_file,keep_tex)
    if a:fd_file != "buffer" 
	let l:fd_file=readfile(a:fd_file)
    else
	let l:fd_file=getline(1,"$")
    endif
    let l:declare_command='\C\%(DeclareFontShape\%(WithSizes\)\?\|sauter@\%(tt\)\?family\|EC@\%(tt\)\?family\|krntstexmplfamily\|HFO@\%(tt\)\?family\)'
    let b:declare_command=l:declare_command
    
    let l:font_decl=[]
    let b:font_decl=l:font_decl
    for l:line in l:fd_file
	if l:line =~ '\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'
	    call add(l:font_decl,l:line)
	endif
    endfor

"     let l:tmp_dir=tempname()
    if exists("b:tmp_dir")
	let b:debug="tmp_dir from b:tmp_dir"
	let l:tmp_dir=b:tmp_dir
    else
	let b:debug="tmp_dir from tempname()"
	let l:tmp_dir=tempname()
    endif
    if !isdirectory(l:tmp_dir)
	call mkdir(l:tmp_dir)
    endif
    if a:fd_file == "buffer"
	let l:testfont_file=l:tmp_dir . "/" . fnamemodify(bufname("%"),":t:r") . ".tex"
    else
	let l:testfont_file=l:tmp_dir . "/" . fnamemodify(a:fd_file,":t:r") . ".tex"
    endif
    call system("touch " . l:testfont_file)
    
    let l:fd_bufnr=bufnr("%")

    let s:text="On November 14, 1885, Senator \\& Mrs.~Leland Stanford called
		\ together at their San Francisco mansion the 24~prominent men who had
		\ been chosen as the first trustees of The Leland Stanford Junior University.
		\ They handed to the board the Founding Grant of the University, which they
		\ had executed three days before.\\\\
		\ (!`THE DAZED BROWN FOX QUICLY GAVE 12345--67890 JUMPS!)"

"     let l:text="On November 14, 1885, Senator \\& Mrs.~Leland Stanford called
" 	\ together at their San Francisco mansion the 24~prominent men who had
" 	\ been chosen as the first trustees of The Leland Stanford Junior University.
" 	\ They handed to the board the Founding Grant of the University, which they
" 	\ had executed three days before. This document---with various amendments,
" 	\ legislative acts, and court decrees---remains as the University's charter.
" 	\ In bold, sweeping language it stipulates that the objectives of the University
" 	\ are ``to qualify students for personal success and direct usefulness in life;
" 	\ and to promote the publick welfare by exercising an influence in behalf of
" 	\ humanity and civilization, teaching the blessings of liberty regulated by
" 	\ law, and inculcating love and reverence for the great principles of
" 	\ government as derived from the inalienable rights of man to life, liberty,
" 	\ and the pursuit of happiness.''\\
" 	\ (!`THE DAZED BROWN FOX QUICKLY GAVE 12345--67890 JUMPS!)\\par}}
" 	\ \\def\\\moretext{?`But aren't Kafka's Schlo{\\ss} and {\\AE}sop's {\\OE}uvres
" 	\ often na{\\"\\i}ve  vis-\\`a-vis the d{\\ae}monic ph{\\oe}nix's official r\\^ole
" 	\ in fluffy souffl\\'es? }
" 	\ \\moretext"

    if a:fd_file == "buffer"
	let l:openbuffer="edit "
    else
	let l:openbuffer="topleft split!"
    endif
    execute l:openbuffer . " +setlocal\\ ft=tex\\ modifiable\\ noro " . l:testfont_file 
    map <buffer> q :bd!<CR>

    call setline(1,'\documentclass{article}')
    call setline(2,'\oddsidemargin=0pt')
    call setline(3,'\textwidth=450pt')
    call setline(4,'\textheight=700pt')
    call setline(5,'\topmargin=-10pt')
    call setline(6,'\headsep=0pt')
    call setline(7,'\begin{document}')

    let l:i=8
    let l:j=1
    let l:len_font_decl=len(l:font_decl)
    let b:match=[]
    for l:font in l:font_decl
	" SHOW THE FONT ENCODING, FAMILY, SERIES and SHAPE
	if matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') == "b" ||
		    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') == "bx"
	    let b:show_font='\noindent{\large \textit{Font Encoding}: \textsf{' . 
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			\ ' \textit{Font Family}: \textsf{' .  
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^}#]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			\ ' \textit{Font Series}: \textsf{' .  
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') . '}' . 
			\ ' \textit{Font Shape}: \textsf{' .  
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}') . '}}\\[2pt]'
	else
	    let b:show_font='\noindent{\large \textbf{Font Encoding}: \textsf{' . 
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			\ ' \textbf{Font Family}: \textsf{' .  
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^}#]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			\ ' \textbf{Font Series}: \textsf{' .  
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') . '}' . 
			\ ' \textbf{Font Shape}: \textsf{' .  
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}') . '}}\\[2pt]'
	endif
	call setline(l:i,b:show_font)
	let l:i+=1
	" CHANGE THE FONT
	call setline(l:i,'{' . substitute(
		    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'),
		    \ l:declare_command,'usefont','') . 
		    \ '\selectfont')
	" WRITE SAMPLE TEXT
	call add(b:match,matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'))
	let l:i+=1
	" END
	if l:j<l:len_font_decl
	    call setline(l:i,s:text . '}\\\\')
	else
	    call setline(l:i,s:text . '}')
	endif
	let l:i+=1
	let l:j+=1
    endfor
    call setline(l:i,'\end{document}')
    silent w
    if b:texcompiler =~ '^pdf'	
	let l:ext=".pdf"
    else
	let l:ext=".dvi"
    endif
    call system(b:texcompiler . " " . l:testfont_file . 
	    \ " && " . b:Viewer . " " . fnamemodify(l:testfont_file,":p:r") . l:ext ." &")
"	1st TRY 
"     if v:servername == ""	
" 	call system(b:texcompiler . " " . l:testfont_file . 
" 		\ " && " . b:Viewer . " " . fnamemodify(l:testfont_file,":p:r") . l:ext)
"     else
" 	let g:atp_fd_callback=0
" 	call system(b:texcompiler . " " . l:testfont_file . ";vim --servername " . v:servername . " --remote-send <Esc>:let g:atp_fd_callback=1<CR>")
" 	while g:atp_fd_callback==0
" 	    sleep 250m
" 	    redraw!
" 	    echomsg "waiting for fd_callback"
" 	endwhile
" 	call system("xpdf " . fnamemodify(l:testfont_file,":p:r") . l:ext)
"
"	2nd TRY
"     echomsg system("(" . b:texcompiler . " " . l:testfont_file . "2>&1 1>/dev/null" .
" 	    \ " ; " . b:Viewer . " " . fnamemodify(l:testfont_file,":p:r") . l:ext . 
" 	    \ " ;  cat ". fnameescape(fnamemodify(l:testfont_file,":p:r"). ".log") . ")&" )
"
"	3rd TRY
"     let g:log=system("(" . b:texcompiler . " " . l:testfont_file . "2>&1 1>/dev/null" .
" 	    \ " && " . b:Viewer . " " . fnamemodify(l:testfont_file,":p:r") . l:ext . "" .
" 	    \ " || cat ". fnameescape(fnamemodify(l:testfont_file,":p:r"). ".log") . ")" )
"
"	4th & 5th TRIES
"     let g:log=system("(pdflatex ".fnameescape(l:testfont_file)." 2>&1 1>/dev/null "." && xpdf ".fnameescape(fnamemodify(l:testfont_file,":p:r").l:ext)." || cat ".fnameescape(fnamemodify(l:testfont_file,":p:r").".log)&"))
"     let g:log=system("(pdflatex " . expand("%") . "2>&1 1>/dev/null" .  " && " . b:Viewer . " " . fnamemodify(expand("%"),":p:r") . l:ext .  " || cat ". fnameescape(fnamemodify(expand("%"),":p:r"). ".log") . ")&" )
"     let g:shell_error=v:shell_error
"
"     ?:/ 	One can not get shell command output if it is with '&'.
    if !a:keep_tex
	silent exe "bd"
    endif
endfunction

function! ShowFonts(fd_file)
    let l:declare_command='\C\%(DeclareFontShape\%(WithSizes\)\?\|sauter@\%(tt\)\?family\|EC@\%(tt\)\?family\|krntstexmplfamily\|HFO@\%(tt\)\?family\)'
    let b:declare_command=l:declare_command
    
    let l:font_decl=[]
    let b:font_decl=l:font_decl
    for l:line in readfile(a:fd_file)
	if l:line =~ '\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'
	    call add(l:font_decl,l:line)
	endif
    endfor
    let l:font_commands=[]
    for l:font in l:font_decl
	call add(l:font_commands,substitute(
		    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'),
		    \ l:declare_command,'usefont',''))
    endfor
    let l:message=""
    for l:fcom in l:font_commands
	let l:message.="\n".l:fcom
    endfor
    let l:message="Fonts Declared:".l:message
    call confirm(l:message)
endfunction

" AUTOCOMMANDS
au CursorHold fd_list* :echo g:fd_matches[(max([line("."),'2'])-2)]

" COMMANDS
if bufname("%") =~ 'fd_list'
    command! -buffer -nargs=1 Preview	:call s:Preview(g:fd_matches[(max([line("."),'2'])-2)],<f-args>)
    command! -buffer ShowFonts		:call ShowFonts(g:fd_matches[(max([line("."),'2'])-2)])
    map <buffer> <Enter> 	:call OpenFile()<CR>
    map <buffer> <Tab>		:call ShowFonts(g:fd_matches[(max([line("."),'2'])-2)])<CR>
else
    command! -buffer -nargs=1 Preview	:call s:Preview("buffer",<f-args>)
endif

" MAPS
noremap <buffer> P :Preview 1<CR>
noremap <buffer> p :Preview 0<CR>
map <buffer> Q :bd!<CR>
map <buffer> q :q!<CR>R
doc/automatic-tex-plugin.txt	[[[1
1771
*atp* 		Automatic Tex Plugin

			An Introduction to Automatic (La)TeX Plugin
				by Marcin Szamotulski
			----------------------------------------

If you found this plugin useful, you are cordially invited to write to me:
mszamot@gmail.com. Voting at vim site is also welcome ;) .

New Features:
	|atp-FontSearch|	(make your docs in a new shape!)	
	|atp-completion|	(final changes)

					Abstract
					========	

This is a new plugin for vim to comfortably write TeX (LaTeX, PdfLaTeX)
documents, which provides functionality not met in other such plugins. It
makes you FREE from compiling procedure, making this process automatic using
autocommands. It also provides useful mappings and other functions: to analyse
your .log file, to see the table contents, to search for a label, to search in
bib files or to find a macro definition matching a pattern. To have full
functionality you need: pdffonts available in the package
app-text/poppler-utils (at least in Gentoo). Another good tool is texdoc,
which is a part of texlive - these days standard TeX distribution for Linux,
and MikTeX on Windows.

Features include:
* completion for commands, closing environments (even nested), package names,
  citations and lables.
	See |atp-completion|, 
* a powerful function to search in bibliographic files (bib files):
	See |atp-bibsearch|,
* table of contents which allows to switch between different '.tex' files:
	See |atp-TOC()|,
* list of labels which allows to see the context of a label:
	See |atp-Labels()|,
* a command to list ToDo lines:
	See |atp-ToDo()|.
* a command to search for a macro definition:
 	See |atp-DefiSearch()|
* a command to search and PREVIEW fonts in your latex distribution:
 	See |atp-FontSearch()| 

                                                		*atp-help-toc*
|atp-installation| 	Installation								
|atp-functions| 	Functions and commands
|atp-bibsearch|		Searching in bib files
|atp-completion|        How to use and configure completion	(*NEW*)
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
News:
The new feature:  

Wrap selection (thanks to D.Munger):
vmap <buffer> <silent> <F7> <Esc>:call <SID>WrapSelection('')<CR>i
(i.e. select a piece of text and press F7, the selected text will be put
inside \{ ... }, and the cursor will be placed just after "\". (Then you can
use completion to chose which command, or environment to put). Contributed by
D. Munger.


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
								*atp-commands*

The main function is not seen by the user (it is called s:compiler, for those
who want to read the plugin). It executes tex compiler specified by the
variable b:texcompiler. It is executed
as an autocommand by the line:
	au! CursorHold $HOME*.tex silent call 's:auTeX()'
where s:auTeX() is a simple function which calls s:compiler if the file written
on the disk and the buffer differ.
As you can see it will run if a key is not pressed during time defined by
option 'updatetime' (see |CursorHold|) in the normal mode. If you type in
insert mode the file won't be compiled (and that's alright as you can be in the
middle of your very long formula). The value of 'updatetime' which works fine
is around 1000ms ('updatetime' is set in milliseconds). Tex compiler is run with
one options:
	-output-directory 
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
		fi		- include font info massages
		F		- show files listed in the log
				    (messages which start with 'File: ')
				    shows the files loaded by tex
					for example fd files that LaTeX is using
		p		- show packages loaded by tex 
				    (messages which start with 'Package: ')
<
	If none flag is given 'e' is used.
	Example:
>
		:ShowErrors rc
<
	will show all reference and citation warnings.

ShowErrors maps:					*atp-ShowErrors-maps* 

<F6>+e			to see all errors 	(:ShowErrors e)
<F6>+w			to see all warnings	(:ShowErrors w)
<F6>+r			to see warnings coming	(:ShowErrors rc) 
			from references or citations  
<F6>+f			to see font warnings 	(:ShowErrors f)

this is not a texloganalyzer mapping but it is a good place to mention it:
<F6>+l			to open log file in another tab
			this is a mapping to a function called |OpenLog()|.

	
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

							*atp_bibinputs*
	Tex is looking for the date base files in the path: `kpsewhich
	-show-path bib`. The variable g:atp_bibinputs contains vim list of
	these direcrories. Thus if atp cannot find your bib file, tex also
	won't be able. 

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
	xdvi viewer. It will read the xdvi options from the variable
	g:xdviOptions (to the variable b:ViewerOptions).

:SetXpdf						*atp-SetXpdf*
	This command sets the options for xpdf viewer (as for now the
	inverse/reverse searching in pdf files is not implemented)
	It will read the xpdf viewer options from the variable g:xpdfOptions
	(to the varaible b:ViewerOptions).

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

	If a readable bib file was not found in any of g:atp_bibinputs
	directories (see |atp_bibinputs|) directories it is classified as
	not readable.  

FindInputFiles([bufname])				*atp-FindInputFiles()*
:FindInputFiles [bufname]
	This function finds all the input files, i.e. files put after the
	tex commands: '\input', '\include' or '\includeonly'. And prints the
	reuslt. 
	The function and the command have one optional argument - the buffer
	name. By default it searches for input files in the current buffer.
	The bufername complition is set, hence you can use <Tab> to choose the
	bufername.

EditInputFile([input_file_name],[bufname])		*atp-EditInputFile()*
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
	them in the any of g:atp_bibinputs directory (see |atp_bibinputs|.

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

Print([<printer>,<printer_options>])			*atp-Print()*
map \p,:SshPrint
	It will run 'lpr' command and append to it the options defined in the
	variable 'g:printeroptions' + options given in the second argument. It
	prints the pdf or dvi depending on the value of 'b:texcompiler' (see
	|atp-texcompiler|).  If you specify the variable
	'g:atp_ssh=<user>@<host>' it will print via ssh on the <host> using
	the <printer>. The command ':SshPrint' has a completion set for the
	printers available on your local system or in the host. All the
	arguments of the command SshPrint are |<f-agrs>|. 
	
	Both arguments are optional (the default system printer is used, and
	only the options 'g:printeroptions' applay).

	The map '\p' will print on the default printer.

	The command has completion for the names of printers (also remote
	printers), press <Tab> to cycle through printers, or type first
	letters of the printers name and press <Tab> to complete it.

							*atp-Lpstat()
Lpstat()
:Lpstat
	Sends "lpstat -l" remotly (using the 'g:atp_ssh' value) or locally and
	echoes the output.

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

NextEnv(<environment>)
:NEnv <environment>
	Move to next environment, for example ':NEnv definition'. Complition
	is set, which finds environemts defined in current tex source file.

PrevEnv(<environment>)
:PEnv <environment>
	Move to previous environment, for example ':NEnv definition'. Complition
	is set, which finds environemts defined in current tex source file.

NextSection(<section>)
:NPart, :NChap, NSec
	Goes to next <section>, the commands need not to be explained.

PrevSection(<section>)
:PPart, :PChap, PSec
	Goes to previous <section>.

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
	 
SpaceToggle()	 					*atp-SpaceToggle*
:SpaceToggle, map <F2>
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


FontSearch([<pattern>,<match option>])				*atp-FontSearch*
:FontSearch [<pattern> <match option>]
	
	For example:
	:FontSearch ^t1
		will list all the fd files in your tex distribution which
		names starts with t1 (i.e. which describes fonts in encoding
		'T1')
	:FontSearch bookman 1
		will list all fd files which full path mathes 'bookman'.

	The <match option> argument has two values 0/1, if it is not given
	0 is assumed.

	In the opened window there are several mappings defined:
	    <Enter>   	open the fd file under the cursor
	    <Tab>	list fonts defined in the fd file (shows the command
			that you can use in your tex file to use this font)
	    p		preview the fonts defined in the fd file under the
			cursor, do not shows the latex source. 	
	    P		preview fonts and show the latex source 
			(you can then see the errors why the preview was not
			produced, in many cases there are no encoding files or
			fonts themselves for which you have to look in CTAN
			archive yourself)
	    q 		close the window (actually, delete the buffer using
		       :bd, it will be still in the list if you type ":ls!",
		       so even then you can reload previous searches.)  

	The same mappings are defined in the window with fd file opened
	(except <Enter>, <Tab>).  

	Additionally, read |font-lowlevelcommands| to learn how to use
	|\usefont|, |\fontsize|, |\selectfont| and other such commands.

	Hopefully, this will help your documents to become beautiful :)

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

:DefiSearch [pattern]						*atp-DefiSearch()*
	Finds all definitons which matches the pattern. It looks in the main
	file and all the input files (except bib files). The pattern is case
	sensitive (i.e. the functions appends '\C' at the beginning of the
	pattern by default, if you want to make case insensitive matching put '\c'
	at the beginning of your pattern).

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
Completion			                        *atp-completion*

The completion is by default mapped to <Tab>. For example if you type
\math<Tab> you will get a list of choices which completes this command. (See
':h popupmenu-completion' and ':h completion' for more).

There are two completion algorithm: expert mode  and non expert mode: the
keyword for completion in expert mode must match at the beginning, in non
expert mode any where. Also in expert mode the list of possible completions is
smaller (for example there is no '\lneqq', there is only '\lneq').

If you prefer to not map <Tab> key (you can use '>' '<' to make the tabulation
in visual mode - see ':h >') then you can define g:atp_no_tab_map=1 in your
vimrc file. 

You can switch a completion mode adjusting the variable
'g:atp_completion_active_modes', all names of completion modes are stored in the
variable 'g:atp_tab_completion_modes'.

If 'g:atp_local_completion_lists' is set to non zero value, then input files
will be scanned for \def, \newcommand, \newnevironment and \newtheorem
commands and they will be used for completion. 

Completion works for:
	commands
		if g:atp_check_if_math_mode = 1 then the pull of commands
		contains math commands only if there you are inside a math
		environment. This works perfectly if you switch from $:$ and
		$$:$$ to their equivalent (and more up-to-date) \(:\) and \[:\].
		The list of math environment in which ATP will think you are
		editing in a math mode is stored in the variable:
		'g:atp_math_modes'. Its entries are two element list of
		patterns which matches the beginning and end of math mode.
		The '0' entry have to provide the beginning and end pattern of
		in line math '\(:\)', the second for displayed math '\[:\]'.
		Its default value is given below.
	
		If you add a package (like tikz or amsmath, or amssymb) then
		the pull of completions will contain extra
		commands/environment names defined in these packages.  
		Because some classes calls amsmath package silently you can
		setting the variable 'g:atp_amsmath=1' will ensure that the
		you will get completions for these commands. The algorithm
		checks if you have this package declared or if you use some of
		the standard ams class (actually checks if the document class
		name matches '^ams'). 

		If you do not want math commands completions at all define
		':let g:atp_no_math_command_completion=1' (you can put it in your
		~/.vimrc, or define while writing, both will work, so you can
		switch off the math completions temporarily)

		If you want math commands to be shown at the end rather than at the
		beginning then define ':let g:atp_math_commands_first=0' (the
		default is 1, first go math commands).


		The label commands completes in a special way: for example in
		a line like:
			\begin{theorem}\lab<Tab>
		will complete to 
			\begin{theorem}\label{thm:
		The dictionary of short names is 'g:atp_shortname_dict'. If
		you do not likes this idea (however it can help you to
		correctly write \ref{ - to avoid referring to lemmas as
		propositions, and also it makes completion for \ref{ nicer
		- you can list only labels for theorems), so if you do not
		want it anyway: 'let g:atp_no_short_names=1' will make the
		work.

	ref/label/cite
		for label completion puts short names, for ref and eqref
		commands the completions are the labels found in all files
		associated to the main file (the plugin searches the input
		and include files for them). The same for cite: which searches
		also in bib files defined in the main file.

								*atp-closing-environments*
								*atp-completion-env*
	environments
		Completes after '\begin{' and '\end{'. For example
		'\begin{cen<Tab>' will give '\begin{center}' 
		But '\begin{thoerem}<Tab>' or
		'\begin{theorem}\label{thm:1}<Tab> will close the environment.
		The algorithm tries to close environment in many natural
		situations: for example when it did found less than one command
		completion. It closes the right environment when they are
		nested (however not in right place!) and it preserves the
		indention. When after \begin{center}\label{...} XXX is
		something (in place of XXX) it will close the environment in
		after the cursor position otherwise in next line.

				
			(the <Tab> indicates in which position the
			<Tab> can be pressed to get the described
			behaviour).
>
 				\begin{theorem}
 					\begin{enumerate}
 					\item .....
 					\item .....
 					    \begin{minipage} 	
 						......
 					    \end{minipage}
					        ......
						......<Tab>
 						XXXXXX
						......
 				\end{theorem}
<			Usually the closing comes in the next line,
			unless we are inside an environemnt which is opened
			after the non closed environemnt: 
>
 				\begin{theorem}
 					\begin{enumerate}
 					\item .....
 					\item .....
 					    \begin{minipage}<Tab> 	
 						......<Tab>
 					    \end{minipage}<Tab>
 						XXXXXX
					        ......
						......
						......
 				\end{theorem}
<			Then the closing will be put just after the last
			opend environment closes, or
>
 				\begin{theorem}
 					\begin{enumerate}
 					\item .....
 					\item .....
 					    \begin{minipage}
 						......
 					    \end{minipage}
					        ......
						......
						......
 						XXXXXX
 				\end{theorem}<Tab>
				....<Tab>
<			If we are outside the theorem environment,
			'\end{enumerate}' will be placed just above
			'\end{theorem}', and 	
>
 				\begin{theorem}[Joayal\&Tirney]\label{thm:jt}
 					\begin{enumerate}
 					\item .....
 					\item .....
 					    \begin{minipage} 	
 						......
						......
 						XXXXXX
 					\end{enumerate}<Tab>
 				\end{theorem}<Tab>
<			will put \end{minipage} just above
			\begin{enumerate}. Furthermore, if:
>
 				\begin{theorem}
 					\begin{enumerate}\label{enu:1}
 					\item .....
 					\item .....
 					    \begin{minipage} 	
 						......
 						\begin{itemize}
 							......
 						\end{itemize}
 						......
						......
 						XXXXXX
 					\end{enumerate}<Tab>
 				\end{theorem}<Tab>
<			'\end{minipage}' will be put just above
			'\end{enumerate}'.  Furthermore,
>
	\begin{theorem}[...]\label{...} Let \(C\) be a ....
	......
	......<Tab> XXXXX
<	That is, if you like to write \begin{} \end{} in the beginning and end of a line
	this will be preserved. However, there is no support for nested environments
	then.

	THIS IS NO LONGER IN  USE, but may come later:
				If this option is set to '0' then closing is put in
				the line below the current line. The default is '1'.

				Furthermore, this option is in use only if the
				environments are in separate lines, and
				\begin{<env_name>} is at most followed by '[...]' or
				'\label{}'. Otherwise the closing will put
				\end{env_name} just before the cursor position.

These are all variables which can help to customise the completion:
(if the value is given it is the default, if it is not means it is too long to
put it here).
>
 g:atp_completion_limits		= [ '40', '160', '160' ]
<
				The above variable specifies how long should
				atp plugin search for unclosed environments:
				the first value	 - search for \(:\)  [ in line math ]
				the second	 - search for \[:\]  [ displayed math ]
				the third	 - search for \begin{<env>:\end{<env>	

				You can also put "-1" as the value, then the
				search forward/backward will last till
				first/last line. However, this makes it run slower.
				
>
  g:atp_completion_modes		= [ 
				\ 'commands', 		'inline_math', 
				\ 'displayed_math', 	'package_names', 
				\ 'tikz_libraries', 	'environment_names', 
				\ 'close_environments' ,'labels', 
				\ 'bibitems', 		'input_files',
				\ 'bibfiles' ] 	
<				
				This is the list of completion modes.

g:atp_completion_active_modes	= g:atp_completion_modes
				This is the list of completion modes which are
				active, by default all modes are active. Remove
				a value from this list to make it inactive (You can
				use remove() command, see ':h remove()').  
>
    g:atp_environments
    g:atp_amsmath_environments
    g:atp_shortname_dict
    g:atp_separator			= ':'
    g:atp_no_separator 		= 0
    g:atp_env_short_names 		= 1
    g:atp_no_separator_list		= ['', 'titlepage']
    g:atp_package_list
    g:atp_commands
    g:atp_math_commands
    g:atp_math_commands_non_expert_mode
    g:atp_ams_negations
    g:atp_ams_negations_non_expert_mode
    g:atp_amsmath_commands
    g:atp_fancyhdr_commands
    g:atp_tikz_environments
    g:atp_tikz_libraries
    g:atp_tikz_commands
<
								*atp-g:atp_check_if_opened*
>
     g:atp_check_if_opened	= 1     
<
				     (this checks if your are in some environments,
				     for example inside \begin{tikzpicture} or
					 \tikz{}, if 1 then the tikz commands will be
					 available only inside this environments)
								*atp-g:atp_math_opened*
>
     g:atp_math_opened		
<
				(the default is 1 if in your tex file there was no
				$ or $$ excluding \$, thus it will be one in any newly
				edited file!)
								*atp-g:atp_math_modes*
>
     let g:atp_math_modes	=[ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)(','\%([^\\]\|^\)\%(\\\|\\\{3}\))'],
				\ ['\%([^\\]\|^\)\%(\\\|\\\{3}\)\[','\%([^\\]\|^\)\%(\\\|\\\{3}\)\]'], 	
				\ ['\\begin{align', '\end{align'], 		['\\begin{gather', '\\end{gather'], 
				\ ['\\begin{flign', '\\end{flagin'], 	['\\begin[multiline', '\\end{multiline'],
				\ ['\\begin{tikz', '\\end{tikz'],		['\begin{equation', '\end{equation'] ]
<				The first pattern of the 0th item matches '\(' and '\\\(' but
				not '\\('. Similarly for '\)', '\[', '\]'.  Remember
				that if you change this list the first term should
				correspond to '\(:\)' and the second to '\[:\]'. If
				you want to switch checking if cursors stands in math
				mode use g:atp_math_opened variable.

g:atp_no_tab_map
g:atp_no_complete		=['document']
				List of environments which is not closed by
				Tab_Completion. (The document environment in longer
				documents can be not seen by the algorithm as closed,
				because it searches only in a part of the text, see
				g:atp_completion_limits variable above).


================================================================================
How to configure to your needs                          *atp-configure*
							*atp-variables*

There are several options you can set, and they might be set in your vimrc
file. The default values are given below (except the completion setup and
bibtex documented above).

Tip: If you want to see (almost) all the variables, type ':let g:atp-<CTRL+d>',
and ':let b:<CTRL+d>'.

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
	If you want to set some additional options to your tex compiler you can
	use this variable, note that -output-directory, and -mode, are
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
	If the function which calls TeX compiler do not see that you are
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
	 deletes all the files with the specified extension in the directory
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
	g:toc_window_width (by default not set, if set overrides
	t:toc_window_width)
	Configures the initial width of the window with table of contents.

let t:labels_window_width=30
	g:labels_window_width (by default not set, if set overrides
	t:labels_window_width)
	Configures the initial width of the window with labels.

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

These mappings are loaded unless you set one of variables: 'g:no_plugin_maps' or
'g:no_atp_maps' (disables maps defined in tex_atp.vim), 'g:no_atp_toc_maps'
(disables maps defined in 'toc_atp.vim'),  'g:atp_no_env_maps' (disables the
environment mapps '[*', ']*') or 'g:atp_no_tab_map' (disables the tab mapping
for completion, then completion is mapped to F7 but there is not mapping for
'WrapSelection()', you have to provide one by your self).

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
	i.e when your cursor is over a package name and you press 'K' key
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
		

								
pdffonts is mapped to <F6>+g				*atp-pdffonts*
There is also a command ':PdfFonts' which does the same. 

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
A possible error which may occur using the :BibSearch commands has a simple
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
value "texdoc -m", thus pressing 'K' (see |K|) over a tex package should open
you the package documentation. It is also a good advice to install help files
of latex-suite, you can find there a big part of 'Non so short introduction to
Latex' by Tobias Oetiker (copy them to your ~/.vim/doc document then use
|:helptags| to install the help tags :helptags ~/.vim/doc and that's it). The
same applies to this help file.

Another good programs are texloganalyzer and pdffonts which are not required
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
	will make xpdf view different. This is helpful when you edit to
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
	options '-watchfile 1' see man xdvi for explanations). You can set
	inverse/reverse searching by the command |SetXdvi|. It is recommended
	to use gvim rather than vim, if you want to use vim you have to run it
	with the command:
>
 "	vim --servername xdvi <file>
<
	You can pick any server name.
	
	The command SetXdvi defines a new function:

RevSearch()							*atp-ISearch()*
map \is, :RevSearch
	Which makes an reverse search (sets the xdvi position according to the
	cursor position in gvim).

	Here I describe how inverse/reverse searching is done. 
	
    (1) Inverse searching
	(i.e. position vim's cursor after xdvi event:
	usually CTRL+Left Mouse) with this options:
>
	let b:texcompiler="latex"
	let b:texoptions="-src-specials"
	let b:Viewer="xdvi -editor 'gvim --remote-wait +%l %f'"
<
	See vim tip at: http://vim.wikia.com/wiki/Vim_can_interact_with_xdvi 	

    (2) Reverse searching
	For reverse searching (position xdvi according to the vim's cursor
	position) you can set:
>
	let b:reverse_search="xdvi -sourceposition " . line(".") . ":" . col(".") . fnamemodify(expand("%"),":p") . " " . fnamemodify(expand("%"),":p:r") . ".dvi"
<
	To make an reverse search:
>
		:call system(b:reverse_search)
<
	And xdvi will place itself at the current cursor position in the 'tex'
	source file. You can make a map for this. 

	To use this with vim one have to add servername. Run
	vim as:
>
 	vim --servername vimtex
 	let b:Viewer="xdvi -editor 'vim --servername " . v:servername . " --remote-wait +%l %f'"
 	let b:reverse_search="xdvi -editdor 'vim --servername " . v:servername "' -sourceposition " . line(".") . ":" . col(".") . fnamemodify(expand("%"),":p") . " " . fnamemodify(expand("%"),":p:r") . ".dvi"
<

	In case of troubles:

	Reverse Searching:
	    If reverse searching do not works for you, and you get an error, that
	    there is no reference to your tex file in the dvi file, then open
	    your dvi file in an editor (vim :) and check what is the name after
	    'src:<line number>' (this are the source specials put by 'latex
	    -src-specials' command). It should refer to your tex file. Xdvi
	    will not recognize if you specify the full name of the tex file (i.e.
	    with path) in the b:reverse_search (as we do above using the
	    modifier :p and the function fnamemodify in
	    'fnamemodify(expand("%"),":p")' the other one with ":p:r" is OK!)
	    and in the dvi file there is written just the name without the
	    path (and vice versa, if you give just the name in
	    b:reverse_search and in the file there is full path).

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


vim:tw=75:ts=8:ft=help:norl:
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

highlight DiffAdd        cterm=none           	ctermfg=15  ctermbg=56  guifg=white	guibg=SlateBlue4 gui=bold
highlight DiffDelete     cterm=none           	ctermfg=19  ctermbg=56	guifg=VioletRed	guibg=SlateBlue4
highlight DiffChange     cterm=none           	ctermfg=173 ctermbg=125	guifg=salmon	guibg=DeepPink4
highlight DiffText       cterm=bold           	ctermfg=white ctermbg=125  guifg=white	guibg=DeepPink4

highlight Question       cterm=bold           	ctermfg=33  ctermbg=233 guifg=#0087ff	guibg=#1c1c1c
highlight ErrorMsg       cterm=bold            	ctermfg=160 ctermbg=233 guifg=#d70000	guibg=#1c1c1c
highlight ModeMsg              			ctermfg=33  ctermbg=233 guifg=#0087ff	guibg=#1c1c1c
highlight MoreMsg        	           	ctermfg=39  ctermbg=233 guifg=#00afff	guibg=#1c1c1c
highlight WarningMsg    cterm=bold           	ctermfg=161 ctermbg=233 guifg=#d7005f	guibg=#1c1c1c

highlight LineNr                              	ctermfg=57 ctermbg=233	guifg=#837598	guibg=#1c1c1c
highlight Folded  				ctermfg=57 ctermbg=233	guifg=#837598	guibg=#1a1a1a
highlight FoldColumn     cterm=none           	ctermfg=green ctermbg=233 guifg=#5CB80C guibg=#1c1c1c
"highlight SignColumn

highlight Search         cterm=bold           	ctermfg=black  	ctermbg=226	guifg=black guibg=yellow
highlight IncSearch      cterm=bold        	ctermfg=black  	ctermbg=red	guifg=gold guibg=#1c1c1c
highlight MatchParen     			ctermfg=233	ctermbg=226	guifg=#1c1c1c guibg=gold

highlight PMenu          ctermbg=18 ctermfg=39  
highlight PMenuSel       ctermbg=39 ctermfg=18
highlight PMenuSBar      ctermbg=white ctermfg=33
highlight PMenuThumb     ctermbg=white ctermfg=33

highlight SpecialKey     ctermfg=129    ctermbg=233			 guifg=DarkViolet
highlight StatusLine     cterm=none     ctermfg=226 ctermbg=232		 guifg=#111111 guibg=wheat1
highlight StatusLineNC   cterm=none     ctermfg=245 ctermbg=232		 guifg=#111111 guibg=snow4	 
highlight default User1		 cterm=bold	ctermfg=226 ctermbg=232	gui=bold guifg=DeepPink4  	guibg=#111111	 
highlight default User2		 cterm=none	ctermfg=red ctermbg=232		 guifg=Khaki1  		guibg=#111111
highlight default User3		 cterm=none	ctermfg=226 ctermbg=232		 guifg=BlueViolet   	guibg=#111111

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

highlight Todo           ctermfg=21           ctermbg=233	guifg=DeepPink guibg=#1c1c1c	gui=underline,bold

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
