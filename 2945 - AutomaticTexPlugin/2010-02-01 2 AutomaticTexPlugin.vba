" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/tex_atp.vim	[[[1
1460
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Jan 25
" URL:		
"
" GetLatestVimScripts: 2945 2 :AutoInstall: tex_atp.vim
"
" TODO Check against lilypond 
" NOTES
" s:tmpfile=temporary file value of tempname()
" s:texfile=readfile(bunfname("%")

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

setl keywordprg=texdoc\ -m

function! s:setoutdir()
if g:askfortheoutdir == 1 
    let b:outdir=input("Where to put output? ")
elseif get(getbufvar(bufname("%"),""),"outdir","optionnotset") == "optionnotset" && g:askfortheoutdir != 1 
     let b:outdir=fnameescape(fnamemodify(resolve(expand("%:p")),":h")) . "/"
"      	echomsg "DEBUG setting b:outdir to " . b:outdir
     echoh WarningMsg | echomsg "Output Directory "b:outdir | echoh None
endif	
endfunction

let s:optionsDict= { "texoptions" : "", "reloadonerror" : "0", "openviewer" : "1", "autex" : "1", "Viewer" : "xpdf", "XpdfOptions" : "", "XpdfServer" : fnamemodify(expand("%"),":t"), "outdir" : fnameescape(fnamemodify(resolve(expand("%:p")),":h")) . "/", "askfortheoutdir" : "0", "texcompiler" : "pdflatex"}
let s:ask={ "ask" : "0" }
let g:rmcommand="perltrash"
let g:texextensions=["aux", "log", "bbl", "blg", "spl", "snm", "nav", "thm", "brf", "out", "toc", "mpx", "idx", "maf", "blg", "glo", "mtc[0-9]", "mtc1[0-9]"]
let g:keep=["log","aux","toc","bbl"]
" let b:texinteraction='errorstopmode'
let g:printingoptions=''
let s:COM=''
" let b:outdir=substitute(fnameescape(resolve(expand("%:p"))),resolve(expand("%:r")) . "." . resolve(expand("%:e")) . "$","","")
" let b:outdir=substitute(fnameescape(resolve(expand("%:p"))),resolve(expand("%:r")) . "." . resolve(expand("%:e")) . "$","","")
function! s:setoptions()
let s:optionsKeys=keys(s:optionsDict)
let s:optionsinuseDict=getbufvar(bufname("%"),"")
for l:key in s:optionsKeys
    if get(s:optionsinuseDict,l:key,"optionnotset") == "optionnotset" && l:key != "outdir" && l:key != "askfortheoutdir"
"  	    echomsg "Setting " . l:key . "=" . s:optionsDict[l:key]
	call setbufvar(bufname("%"),l:key,s:optionsDict[l:key])
    elseif get(s:optionsinuseDict,l:key,"optionnotset") == "optionnotset" && l:key == "outdir"
	call s:setoutdir()
	let s:ask["ask"] = 1
    endif
endfor
endfunction
call s:setoptions()

if !exists("*ShowOptions")
function! ShowOptions(...)
    let s:bibfiles=FindBibFiles(bufname("%"))
if a:0 == 0
    echomsg "variable=local value"  
    echohl BibResultsMatch
    echomsg "b:texcompiler=   " . b:texcompiler 
    echomsg "b:texoptions=    " . b:texoptions 
    echomsg "b:autex=         " . b:autex 
    echomsg "b:outdir=        " . b:outdir 
    echomsg "b:Viewer=        " . b:Viewer 
    echohl BibResultsGeneral
    if b:Viewer == "xpdf"
	echomsg "    b:XpdfOptions=   " . b:XpdfOptions 
	echomsg "    b:XpdfServer=    " . b:XpdfServer 
	echomsg "    b:reloadonerror= " . b:reloadonerror 
    endif
    echomsg "b:openviewer=    " . b:openviewer 
    echomsg "g:askfortheoutdir=" . g:askfortheoutdir 
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
    echomsg ""
    highlight Chapter
    highlight Section
    highlight Subsection
    highlight Subsubsection
    highlight CurrentSection
elseif a:0>=1 
    echohl BibResultsMatch
    echomsg "b:texcompiler=   " . b:texcompiler . "  [" . s:optionsDict["texcompiler"] . "]" 
    echomsg "b:texoptions=    " . b:texoptions . "  [" . s:optionsDict["texoptions"] . "]" 
    echomsg "b:autex=         " . b:autex . "  [" . s:optionsDict["autex"] . "]" 
    echomsg "b:outdir=        " . b:outdir . "  [" . s:optionsDict["outdir"] . "]" 
    echomsg "b:Viewer=        " . b:Viewer . "  [" . s:optionsDict["Viewer"] . "]" 
    echohl None
    if b:Viewer == "xpdf"
	echomsg "    b:XpdfOptions=   " . b:XpdfOptions . "  [" . s:optionsDict["XpdfOptions"] . "]" 
	echomsg "    b:XpdfServer=    " . b:XpdfServer . "  [" . s:optionsDict["XpdfServer"] . "]" 
	echomsg "    b:reloadonerror= " . b:reloadonerror . "  [" . s:optionsDict["reloadonerror"] . "]" 
    endif
    echomsg "g:askfortheoutdir=" . g:askfortheoutdir . "  [" . s:optionsDict["askfortheoutdir"] . "]" 
    echomsg "b:openviewer=    " . b:openviewer . "  [" . s:optionsDict["openviewer"] . "]" 
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
    highlight Chapter
    highlight Section
    highlight Subsection
    highlight CurrentSection
    highlight Subsubsection
endif
endfunction
endif
command -buffer -nargs=? ShowOptions :call ShowOptions(<f-args>)

if !exists("*StatusR")
function! StatusR()
let s:status=""
if exists("b:outdir")
    if b:outdir != "" 
	if b:outdir =~ "\.\s*$" || b:outdir =~ "\.\/\s*$"
	    let s:status= s:status . "Output dir: " . pathshorten(getcwd())
	else
	    let s:status= s:status . "Output dir: " . pathshorten(substitute(b:outdir,"\/\s*$","","")) 
	endif
    else
	let s:status= s:status . "Please set the Output directory, b:outdir"
    endif
endif	
    return s:status
endfunction
endif
let &statusline='%<%f %(%h%m%r %)%#ErrorMsg#%{StatusL()}%#None#%=%#Title#%{Tlist_Get_Tagname_By_Line_EM()}%* %{StatusR()}  %-9.15(%l,%c%V%)%P'
let b:texruns=0
let b:log=0	
let b:ftype=getftype(expand("%:p"))	
let s:texinteraction="nonstopmode"
let &l:errorfile=b:outdir . fnameescape(fnamemodify(expand("%"),":t:r")) . ".log"
setlocal errorformat=%E!\ LaTeX\ %trror:\ %m,
	\%E!\ %m,
	\%Cl.%l\ %m,
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
	\%+Q[%\\d%*[^()])%r,
let s:lockef=1
au BufRead $l:errorfile setlocal autoread 
"--------- FUNCTIONs -----------------------------------------------------
"	
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
	let l:viewer=b:Viewer . " -remote " . shellescape(b:XpdfServer) . " " . b:XpdfOptions 
"    		echomsg "DEBUG 1 l:view="l:viewer 
    else
	let l:viewer=b:Viewer 
"    		echomsg "DEBUG 2 l:view="l:viewer
    endif
    let l:view=l:viewer . " " . shellescape(l:outfile)  . " &"
		let b:outfile=l:outfile
" 		echomsg "DEBUG l:outfile="l:outfile
    if filereadable(l:outfile)
	if b:Viewer == "xpdf"	
	    let b:view=l:view
" 	    echomsg "DEBUG 3 l:view="l:view
	    call system(l:view)
	else
	    call system(l:view)
	    redraw!
	endif
    else
	    echomsg "Output file do not exists. Calling "b:texcompiler
	    call s:compiler(0,1,1,0,"AU")
    endif	
endfunction
endif
"-------------------------------------------------------------------------
if !exists("*s:getpid")
function! s:getpid()
	let s:command="ps -ef | grep -v " . $SHELL  . " | grep " . b:texcompiler . " | grep -v grep | grep " . fnameescape(expand("%")) . " | awk '{print $2}'"
	let s:var=substitute(system(s:command),'\D',' ','')
	return s:var
endfunction
endif
if !exists("*Getpid")
function! Getpid()
	let s:var=s:getpid()
	if s:var != ""
		echomsg b:texcompiler"pid"s:var 
	else
		echomsg b:texcompiler"is not running"
	endif
endfunction
endif
command! -buffer GPID call Getpid()

if !exists("*s:xpdfpid")
function! s:xpdfpid() 
    let s:checkxpdf="ps -ef | grep -v grep | grep '-remote '" . shellescape(b:XpdfServer) . " | awk '{print $2}'"
    return substitute(system(s:checkxpdf),'\D','','')
endfunction
endif
command! -buffer CXPDF echo s:xpdfpid()
"-------------------------------------------------------------------------
if !exists("*s:compare")
function! s:compare(file,buffer)
    let l:buffer=getbufline(bufname("%"),"1","$")
    return a:file !=# l:buffer
endfunction
endif
"-------------------------------------------------------------------------
function! s:copy(input,output)
	call writefile(readfile(a:input),a:output)
endfunction
function! s:compiler(bibtex,start,runs,verbose,command)
    call s:outdir()
    	" IF b:texcompiler is not compatible with the viewer
    if b:texcompiler =~ "^\s*pdf" && b:Viewer == "xdvi" ? 1 :  b:texcompiler !~ "^\s*pdf" && (b:Viewer == "xpdf" || b:Viewer == "epdfview" || b:Viewer == "acroread" || b:Viewer == "kpdf")
	 
    	echohl WaningMsg | echomsg "Your"b:texcompiler"and"b:Viewer"are not compatible:" 
	echomsg "b:texcompiler=" . b:texcompiler	
	echomsg "b:Viewer=" . b:Viewer	
	echomsg ""
	echohl None
" 	sleep 1
    endif
	" First argument name is a name usually it is always set 
	let s:tmpfile=tempname()
"  		echomsg "DEBUG tempfile="s:tmpfile 
	let s:dir=fnamemodify(s:tmpfile,":h")
	let s:job=fnamemodify(s:tmpfile,":t")
	if b:texcompiler == "pdftex" || b:texcompiler == "pdflatex"
	    let l:ext = ".pdf"
	else
	    let l:ext = ".dvi"	
	endif
	let l:outfile=b:outdir . (fnamemodify(expand("%"),":t:r")) . l:ext
	let l:outaux=b:outdir . (fnamemodify(expand("%"),":t:r")) . ".aux"
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
" 	let l:texoutputfiles=b:outdir . (fnamemodify(expand("%"),":t:r")) . ".*"
" 	HANDLE XPDF RELOAD 
	if b:Viewer == "xpdf"
	    if a:start == 1
		"if xpdf is not running and we want to run it.
		let s:xpdfreload = b:Viewer . " -remote " . shellescape(b:XpdfServer) . " " . shellescape(l:outfile)
	    else
		if s:xpdfpid() != ""
		    "if xpdf is running (then we want to reload).
		    "This is where I use ps command.
		    let s:xpdfreload = b:Viewer . " -remote " . shellescape(b:XpdfServer) . " -reload"	
		else
		    "if xpdf is not running (then we do not want
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
" 				echomsg "DEBUG xpdfreload="s:xpdfreload
" 	IF OPENINIG NON EXISTENT OUTPUT FILE
"	only xpdf needs to be run before (above we are going to reload it!)
	if a:start == 1 && b:Viewer == "xpdf"
	    let s:start = b:Viewer . " -remote " . shellescape(b:XpdfServer) . " & "
	else
	    let s:start = ""	
	endif
"	SET THE COMMAND 
	let s:comp=b:texcompiler . " -interaction " . s:texinteraction . " -output-directory " . s:dir . " -jobname " . s:job . " " . shellescape(expand("%"))
	let s:vcomp=b:texcompiler . " -interaction errorstopmode -output-directory " . s:dir . " -jobname " . s:job . " " . shellescape(expand("%"))
	if a:verbose == 0 
	    let s:texcomp=s:comp
	elseif a:runs >= 1 && a:verbose != 0
	    let s:texcomp=s:comp
	elseif a:runs == 1 && a:verbose != 0
	    let s:texcomp=s:vcomp
	endif
	if a:runs >= 2 && a:bibtex != 1
	    " how many times we wan to call b:texcompiler
	    let l:i=1
	    while l:i < a:runs - 1
		let l:i+=1
		let s:texcomp=s:texcomp . " ; " . s:comp
	    endwhile
	    if a:verbose == 0
		let s:texcomp=s:texcomp . " ; " . s:comp
	    else
		let s:texcomp=s:texcomp . " ; " . s:vcomp
	    endif
"  		echomsg "DEBUG runs s:texcomp="s:texcomp
	endif
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
	    let s:copycmd="cp " . s:cpoption . " " . shellescape(s:tmpfile . "." . l:i) . " " . shellescape(b:outdir . (fnamemodify(expand("%"),":t:r")) . "." . l:i) 
" 		echomsg "DEBUG 2 copycmd"s:copycmd
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
"  	    echomsg "DEBUG compile s:command="s:command
	    return system(s:command)
	else
	    let s:command="!clear;" . s:texcomp . " ; " . s:cpoutfile . " ; " . s:copy
	    let b:texcommand=s:command
" 	    echomsg "DEBUG verbose compile s:command=" . s:command
	    exe s:command
	endif
endfunction
"-------------------------------------------------------------------------
function! s:auTeX()
   if b:autex	
    if s:compare(readfile(expand("%")),bufname("%"))
	call s:compiler(0,0,1,0,"AU")
" 	    echomsg "DEBUG compare: DIFFER"
	redraw
"   else
"  	    echomsg "DEBUG compare: THE SAME"
    endif
   endif
endfunction
au! CursorHold $HOME*.tex call s:auTeX()
"-------------------------------------------------------------------------
if !exists("*TEX")
function! TEX(...)
let s:name=tempname()
if a:0 >= 1
    echomsg b:texcompiler . " will run " . a:1 . " times."
    call s:compiler(0,0,a:1,0,"COM")
elseif a:0 == 0
    call s:compiler(0,0,1,0,"COM")
endif
endfunction
endif
command! -buffer -nargs=? -count=1 TEX   :call TEX(<count>,<f-args>)
" command! -buffer -count=1 TEX	:call TEX(<count>)		 
if !exists("*ToggleAuTeX")
function! ToggleAuTeX()
  if b:autex != 1
    let b:autex=1	
    echo "automatic tex processing in ON"
  else
    let b:autex=0
    echo "automatic tex processing in OFF"
endif
endfunction
endif
if !exists("*VTEX")
function! VTEX(...)
    let s:name=tempname()
if a:0 >= 1
    echomsg b:texcompiler . " will run " . a:1 . " times."
    sleep 1
    call s:compiler(0,0,a:1,1,"COM")
else
    call s:compiler(0,0,1,1,"COM")
endif
endfunction
endif
command! -buffer -nargs=? -count=1 VTEX	:call  VTEX(<count>,<f-args>)
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
command! -buffer SBibtex :call SimpleBibtex()

if !exists("*Bibtex")
function! Bibtex(...)
    let s:bibname=tempname()
    let s:auxf=s:bibname . ".aux"
    if a:0 == 0
"  	    echomsg "DEBUG Bibtex"
	call s:compiler(1,0,0,0,"COM")
    else
"  	    echomsg "DEBUG Bibtex verbose"
	call s:compiler(1,0,0,1,"COM")
    endif
endfunction
endif
command! -buffer -nargs=? Bibtex :call Bibtex(<f-args>)

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
		let l:rm=g:rmcommand . " " . b:outdir . "*." . l:ext . " 2>/dev/null && echo Removed ./*" . l:ext . " files"
	    endif
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
       echo "Please install texloganalyser to have this functionality. Perl program written by Thomas van Oudenhove."  
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
	echo "Please install pdffonts to have this functionality. In gentoo it is in the package app-text/poppler-utils."  
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
function! Print(printeroptions)
    call s:outdir()
    if b:texcompiler == "pdftex" || b:texcompiler == "pdflatex" 
	let l:ext = ".pdf"
    else
	let l:ext = ".dvi"	
    elseif b:texcompiler =~ "lua"
	if b:texoptions == "" || b:texoptions =~ "output-format=\s*pdf"
	    let l:ext = ".pdf"
	else
	    let l:ext = ".dvi"
	endif
    endif
    if a:printeroptions==''
	s:command="lpr " . b:outdir . fnameescape(fnamemodify(expand("%"),":t:r")) . l:ext
    else
	s:command="lpr " . a:printeroptions . " " . b:outdir . fnameescape(fnamemodify(expand("%"),":p:t:r")) . ".pdf"
    endif
    call system(s:command)
endfunction
endif

"---------------------- SEARCH IN BIB FILES ----------------------
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
if !exists("*FindBibFiles")
function! FindBibFiles(bufname)
    let s:texfile=readfile(a:bufname)
    let s:i=0
    let s:bibline=[]
    for line in s:texfile
	if line =~ "\\\\bibliography{"
	    let s:bibline=add(s:bibline,line) 
	    let s:i+=1
	endif
    endfor
    let l:nr=s:i
    let s:i=1
    let files=""
    for l:line in s:bibline
	if s:i==1
	    let files=substitute(l:line,"\\\\bibliography{\\(.*\\)}","\\1","") . ","
	else
	    let files=files . substitute(l:line,"\\\\bibliography{\\(.*\\)}","\\1","") . "," 
	endif
	let s:i+=1
    endfor
    unlet l:line
    let l:bibfs=[]
    while len(files) > 0
	let l:x=stridx(files,",")
	let l:f=strpart(files,0,l:x)
	let files=strpart(files,l:x+1)
	let l:bibfs=add(l:bibfs,l:f)
    endwhile
    unlet l:f
" this variable will store all found and user defined bibfiles:    
    let l:allbibfiles=[]
    for l:f in l:bibfs
	if l:f =~ "^\s*\/" 
	    call add(l:allbibfiles,l:f)
	else	
	    call add(l:allbibfiles,b:outdir . l:f)
	endif
    endfor
    unlet l:f
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
    unlet l:f
" this variable will store unreadable bibfiles:    
    let s:notreadablebibfiles=[]
" this variable will store the final result:   
    let s:bibfiles=[]
    for l:f in l:allbibfiles
	if filereadable(l:f . ".bib")
	    call add(s:bibfiles,l:f)
	else
	    echohl WarningMsg | echomsg "Bibfile " . l:f . ".bib is not readable." | echohl None
	    if count(s:notreadablebibfiles,l:f) == 0 
		call add(s:notreadablebibfiles,l:f)
	    endif
	endif
    endfor
    unlet l:f
    if s:notreadablebibfiles == l:allbibfiles
	echoerr "All bib files are not readable."
    endif
    return s:bibfiles
endfunction
endif
command -buffer FindBibFiles echo FindBibFiles(bufname('%'))
" let s:bibfiles=FindBibFiles(bufname('%'))
function! s:searchbib(pattern) 
" 	echomsg "DEBUG pattern" a:pattern
    call s:outdir()
    let s:bibfiles=FindBibFiles(bufname('%'))
"   Make a pattern which will match for the elements of the list g:bibentries
    let l:pattern = '^\s*@\(\%(\<article\>\)'
    for l:bibentry in g:bibentries
	if l:bibentry != 'article'
	let l:pattern=l:pattern . '\|\%(\<' . l:bibentry . '\>\)'
	endif
    endfor
    unlet l:bibentry
    let l:pattern=l:pattern . '\)'
    let b:bibentryline={} 
"   READ EACH BIBFILE IN TO DICTIONARY s:bibdict, WITH KEY NAME BEING THE bibfilename
    let s:bibdict={}
    let l:bibdict={}
    let b:bibdict={}				" DEBUG
    for l:f in s:bibfiles
	let s:bibdict[l:f]=[]
 	let s:bibdict[l:f]=readfile(l:f . ".bib")	
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
    for l:key in keys(b:bibentryline)
	let l:f=l:key . ".bib"
"s:bibdict[l:f])	CHECK EVERY STARTING LINE (we are going to read bibfile from starting
"	line till the last matching } 
 	let s:bibd={}
 	for l:linenr in b:bibentryline[l:key]
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
	    let l:i=s:count(get(l:bibdict[l:key],l:linenr-1),"{")-s:count(get(l:bibdict[l:key],l:linenr-1),"}")
	    let l:j=s:count(get(l:bibdict[l:key],l:linenr-1),"(")-s:count(get(l:bibdict[l:key],l:linenr-1),")") 
	    let s:lbibd={}
	    let s:lbibd["KEY"]=get(l:bibdict[l:key],l:linenr-1)
	    let l:x=1
" we go from the first line of bibentry, i.e. @article{ or @article(, until the { and (
" will close. In each line we count brackets.	    
            while l:i>0	|| l:j>0
		let l:tlnr=l:x+l:linenr
		let l:pos=s:count(get(l:bibdict[l:key],l:tlnr-1),"{")
		let l:neg=s:count(get(l:bibdict[l:key],l:tlnr-1),"}")
		let l:i+=l:pos-l:neg
		let l:pos=s:count(get(l:bibdict[l:key],l:tlnr-1),"(")
		let l:neg=s:count(get(l:bibdict[l:key],l:tlnr-1),")")
		let l:j+=l:pos-l:neg
		let l:lkey=tolower(matchstr(strpart(get(l:bibdict[l:key],l:tlnr-1),0,stridx(get(l:bibdict[l:key],l:tlnr-1),"=")),'\<\w*\>'))
		if l:lkey != ""
		    let s:lbibd[l:lkey]=get(l:bibdict[l:key],l:tlnr-1)
			let l:y=0
" IF THE LINE IS SPLIT ATTACH NEXT LINE									
			let l:lline=substitute(get(l:bibdict[l:key],l:tlnr+l:y-1),'\\"\|\\{\|\\}\|\\(\|\\)','','g')
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
				let l:pos=s:count(get(l:bibdict[l:key],l:tlnr+l:y),"{")
				let l:neg=s:count(get(l:bibdict[l:key],l:tlnr+l:y),"}")
				let l:m+=l:pos-l:neg
				let l:pos=s:count(get(l:bibdict[l:key],l:tlnr+l:y),"(")
				let l:neg=s:count(get(l:bibdict[l:key],l:tlnr+l:y),")")
				let l:n+=l:pos-l:neg
				let l:o+=s:count(get(l:bibdict[l:key],l:tlnr+l:y),"\"")
" Let us append the next line: 
				let s:lbibd[l:lkey]=substitute(s:lbibd[l:lkey],'\s*$','','') . " ". substitute(get(l:bibdict[l:key],l:tlnr+l:y),'^\s*','','')
				let l:y+=1
				if l:y > 30
				    echoerr "ATP-Error /see :h atp-errors-bibsearch/, infinite in bibentry at line "  l:linenr " (check line " . l:tlnr . ") in " . l:f
				    break
				endif
			    endwhile
			endif
		endif
" we have to go line by line and we could skip l:y+1 lines, but we have to
" keep l:m, l:o values. It do not saves much.		
		let l:x+=1
		if l:x > 30
			echoerr "ATP-Error /see :h atp-errors-bibsearch/, infinite loop in bibentry at line "  l:linenr " in " . l:f
			break
	        endif
		let b:x=l:x
		unlet l:tlnr
	    endwhile
	    let s:bibd[l:linenr]=s:lbibd
	    unlet s:lbibd
	endfor
	let l:bibresults[l:key]=s:bibd
    endfor
    let b:bibresults=l:bibresults
    return l:bibresults
    unlet l:bibresults
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
		\ 'o' : ['organization', 'organization '], 'u' : ['url','url          '],
		\ 'H' : ['homepage', 'homepage     '], 	'i' : ['issn', 'issn         '] }
let s:bibflagslist=keys(g:bibflagsdict)
let s:bibflagsstring=join(s:bibflagslist,'')
let g:kwflagsdict={ 	  '@a' : '@article', 	'@b' : '@book\%(let\)\@<!', 
			\ '@B' : '@booklet', 	'@c' : '@in\%(collection\|book\)', 
			\ '@m' : '@misc', 	'@M' : '@manual', 
			\ '@p' : '@\%(conference\)\|\%(\%(in\)\?proceedings\)', 
			\ '@t' : '@\%(\%(master)\|\%(phd\)\)thesis', 
			\ '@T' : '@techreport', '@u' : '@unpublished'}    

highlight link BibResultsFileNames 		Title	
highlight link BibResultEntry		ModeMsg
highlight link BibResultsMatch		WarningMsg
highlight link BibResultsGeneral		Normal


highlight link Chapter 			Normal	
highlight link Section			Normal
highlight link Subsection		Normal
highlight link Subsubsection		Normal
highlight link CurrentSection		WarningMsg

function! s:comparelist(i1, i2)
   return str2nr(a:i1) == str2nr(a:i2) ? 0 : str2nr(a:i1) > str2nr(a:i2) ? 1 : -1
endfunction
"-------------------------s:showresults--------------------------------------
let g:vertical=1
function! s:showresults(bibresults,flags,pattern)
 
" FLAGS:
" All - all flags	
" L - last flag
" a - author
" e - editor
" t - title
" b - booktitle
" j - journal
" s -series
" y - year
" n - number
" v - volume
" p - pages
" P - publisher
" N - note
" S - school
" h - howpublished
" o - organization

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
	    let b:flagslist=l:flagslist				" debug
	    let b:kwflagslist=l:kwflagslist			" debug
" echohl BibResultEntry | echomsg "BibSearch 2.0" | echohl None	    
" echohl BibResultsMatch | echomsg "flags:" . join(l:flagslist,'') . join(l:kwflagslist,'') | echohl None
				let l:bufnr=bufnr(a:pattern)
				if bufexists(bufname(a:pattern))
				    let l:bdelete=l:bufnr . "bdelete"
				    exe l:bdelete
				endif
				unlet l:bufnr
 				let l:openbuffer=" +set\\ buftype=nofile\\ filetype=bibsearch " . fnameescape(a:pattern)
				if g:vertical ==1
				    let l:openbuffer="vnew" . l:openbuffer
				    let l:skip=""
				else
				    let l:openbuffer="vnew" . l:openbuffer
				    let l:skip="       "
				endif
				exe l:openbuffer
" 				call setline(l:ln,"@Comment{BibSearch 2.0 flags " . join(l:flagslist,'') . join(l:kwflagslist,'') . "}")
" 				let l:ln+=1
" 				call setline(l:ln,"@Comment{flags " . join(l:flagslist,'') . join(l:kwflagslist,'') . "}")
" 				let l:ln+=1

    for l:bibfile in keys(a:bibresults)
	if a:bibresults[l:bibfile] != {}
" 	    echohl BibResultsFileNames | echomsg "Found in " . l:bibfile | echohl None
	    call setline(l:ln, "Found in " . l:bibfile )	
	    let l:ln+=1
	endif
	for l:linenr in copy(sort(keys(a:bibresults[l:bibfile]),"s:comparelist"))
" make a dictionary of clear values, which we will fill with found entries. 	    
" the default value if no<keyname>, which after all is matched and not showed
	    let l:values={}	
	    for l:key in s:bibflagslist 
		if l:key == 'key'
		    let l:values=extend(l:values,{'key' : 'nokey'}
		else
		    let l:values=extend(l:values,{ g:bibflagsdict[l:key][0] : 'no' . g:bibflagsdict[l:key][0] })
		endif
	    endfor
	    let b:values=l:values	" DEBUG
" fill l:values with a:bibrsults	    
	    unlet l:key
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
" 		echohl BibResultEntry | echomsg " " . s:z . " line " . l:linenumber . "  " . l:values["key"] 
 					call setline(l:ln,s:z . ". line " . l:linenumber . "  " . l:values["key"])
"  					call setline(l:ln,l:values["key"])
					let l:ln+=1
 					let l:c0=s:count(l:values["key"],'{')-s:count(l:values["key"],'(')
"  					call setline( l:ln,"@Comment{" . s:z . " line " . l:linenumber . "}")
" 					let l:ln+=1

	
		let b:values=l:values
" this goes over the entry flags:
		for l:lflag in l:flagslist
" we check if the entry was present in bib file:
		    if l:values[g:bibflagsdict[l:lflag][0]] != "no" . g:bibflagsdict[l:lflag][0]
			if l:values[g:bibflagsdict[l:lflag][0]] =~ a:pattern
" 			    echohl BibResultsMatch | echomsg "          " . g:bibflagsdict[l:lflag][1] . "   " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]])
 			    		call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " =  " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
					let l:ln+=1
			else
" 			    echohl BibResultsGeneral | echomsg "          " . g:bibflagsdict[l:lflag][1] . "   " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]])
					call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " = " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
					let l:ln+=1
			endif
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
command -buffer -nargs=* BibSearch	:call BibSearch(<f-args>)

"---------- TOC -----------------------------------------------------------
" g:sections, b:toc
let g:sections={
    \	'chapter' 	: [           '^\s*\(\\chapter.*\)',		'\\chapter*'	,'\\chapter.*[\(.*\)].*'],	
    \	'section' 	: [           '^\s*\(\\section.*\)',	'\\section*'	,'\\section.*[\(.*\)].*,'],
    \ 	'subsection' 	: [	   '^\s*\(\\subsection.*\)',	'\\subsection*'	,'\\subsection.*[\(.*\)].*'],
    \	'subsubsection' : [ 	'^\s*\(\\subsubsection.*\)',	'\\subsubsection*'	,'\\subsubsection.*[\(.*\)].*'],
    \	'bibliography' 	: ['^\s*\(\\begin.*{bibliography}.*\|\\bibliography\s*{.*\)'	     ,	'nopattern'		, 'nopattern'],
    \	'abstract' 	: ['^\s*\(\\begin\s*{abstract}.*\|\\abstract\s*{.*\)',	'nopattern'	, 'nopattern']}
" this works only for latex documents.
"----------- Make TOC -----------------------------
" make l:toc a dictionary with keys: line numbers, values 
" [ 'section-name', 'number', 'title']
" where section name is element of keys(g:sections), number is the total
" number, 'title=\1' where \1 is returned by the g:section['key'][0] pattern,
" for now it is just whole line. The number can be skipped, is not used, the
" pattern have to be written so that it returns the title, shorttitle and
" label value or better, writte a function which reads a line character by
" character and finds these values. For now \chapter*,  \section* are counted
" it can be fixed.
function! s:maketoc(bufname)
   let l:toc={}
    let s:texfile=readfile(a:bufname) 
    let l:true=1
    let l:i=0
    " remove the bart before \begin{document}
    while l:true == 1
	if s:texfile[0] =~ '\\begin\s*{document}'
		let l:true=0
	endif
	call remove(s:texfile,0)
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
    " filter s:texfile    
    let s:filtered=filter(deepcopy(s:texfile),'v:val =~ l:filter')
    let b:filtered=s:filtered
    let b:texfile=s:texfile
    for l:line in s:filtered
	for l:section in keys(g:sections)
	    if l:line =~ g:sections[l:section][0] 
		if l:line =~ '^\s*%'
		else
		    let l:title=substitute(l:line,g:sections[l:section][0],'\1','')
		    let l:i=index(s:texfile,l:line)
		    let l:tline=l:i+l:bline+1
		    let l:ind{l:section}+=1
		    call extend(l:toc, { l:tline : [ l:section, l:ind{l:section}, l:title] }) 
		endif
	    endif
	endfor
    endfor
    return l:toc
endfunction
"---------------------- Show TOC -----------------
function! s:showtoc(toc)
    " make a test if the section number must start from chapter number or not
    " (if there are no chapters)
    let l:chapon=0
    for l:line in keys(a:toc)
	if a:toc[l:line][0] == 'chapter'
	    let l:chapon=1
	    break
	endif
    endfor
    let l:cline=line(".")
    let l:chnr=0
    let l:secnr=0
    let l:ssecnr=0
    for l:sections in keys(g:sections)
	let l:nr{l:sections}=""
    endfor
    let l:sorted=sort(keys(a:toc),"s:comparelist")
    let l:len=len(l:sorted)
"     echomsg "LENGHT l:len=" l:len
    for l:line in l:sorted
	let l:lineidx=index(l:sorted,l:line)
" 	echomsg "line idx  " l:lineidx
	let l:nlineidx=l:lineidx+1
	if l:nlineidx< len(l:sorted)
	    let l:nline=l:sorted[l:nlineidx]
	else
	    let l:nline=line("$")
	endif
" 	    echomsg "l:line          " l:line
" 	    echomsg "Current line    " l:cline 
" 	    echomsg "NEXT LINE       " l:nline
	if a:toc[l:line][0] == 'abstract'
	    if l:cline >= l:line && l:cline < l:nline
		echohl CurrentSection
	    else
		if l:chapon
		    echohl Chapter 
		else
		    echohl Section
		endif
	    endif
	echomsg " " . a:toc[l:line][2] | echohl None
	elseif a:toc[l:line][0] =~ 'bibliography\|references'
	    if l:cline >= l:line && l:cline < l:nline
		echohl CurrentSection
	    else
		if l:chapon
		    echohl Chapter 
		else
		    echohl Section
		endif
	    endif
	echomsg " " . a:toc[l:line][2] | echohl None
	elseif a:toc[l:line][0] == 'chapter'
	    let l:chnr+=1
	    let l:secnr=0
	    let l:ssecnr=0
	    let l:sssecnr=0
	    let l:nr=l:chnr
	    if l:cline >= l:line && l:cline < l:nline
		echohl CurrentSection
	    else
		echohl Chapter 
	    endif
	echomsg l:nr . " " . a:toc[l:line][2] | echohl None
	elseif a:toc[l:line][0] == 'section'
	    let l:secnr+=1
	    let l:ssecnr=0
	    let l:sssecnr=0
	    if l:chapon
		let l:nr=l:chnr . "." . l:secnr  
	    else
		let l:nr=l:secnr 
	    endif
	    if l:cline >= l:line && l:cline < l:nline
		echohl CurrentSection
	    else
		echohl Section 
	    endif
	    echomsg "    " .l:nr . " " . a:toc[l:line][2] | echohl None
	elseif a:toc[l:line][0] == 'subsection'
	    let l:ssecnr+=1
	    let l:sssecnr=0
	    if l:chapon
		let l:nr=l:chnr . "." . l:secnr  . "." . l:ssecnr
	    else
		let l:nr=l:secnr  . "." . l:ssecnr
	    endif
	    if l:cline >= l:line && l:cline < l:nline
		echohl CurrentSection
	    else
		echohl Subsection 
	    endif
	    echomsg "        " . l:nr . " " . a:toc[l:line][2] | echohl None
	elseif a:toc[l:line][0] == 'subsection'
	    let l:sssecnr+=1
	    if l:chapon
		let l:nr=l:chnr . "." . l:secnr . "." . l:sssecnr  
	    else
		let l:nr=l:secnr  . "." . l:ssecnr
	    endif
	    if l:cline >= l:line && l:cline < l:nline
		echohl CurrentSection
	    else
		echohl Subsection 
	    endif
	    echomsg "        " .l:nr . " " . a:toc[l:line][2] | echohl None
	else
	    let l:nr=""
	endif
" 	echo l:line . " " . a:toc[l:line][0] . " " . a:toc[l:line][1] . " " . a:toc[l:line][2]
    endfor
endfunction
if !exists("*TOC")
function! TOC()
if b:texcompiler =~ 'latex'    
    let b:toc=s:maketoc(bufname("%"))
    call s:showtoc(b:toc)
else    
    echomsg "This function works only for latex documents."
endif
endfunction
endif
command -buffer TOC :call TOC()
"--------- MAPPINGS -------------------------------------------------------
" Add mappings, unless the user didn't want this.
if !exists("no_plugin_maps") && !exists("no_atp_maps")

noremap  <buffer> <LocalLeader>v		:call ViewOutput() <CR><CR>
noremap  <buffer> <F3>        			:call ViewOutput() <CR><CR>
inoremap <buffer> <F3> <Esc> 			:call ViewOutput() <CR><CR>
noremap  <buffer> <LocalLeader>g 		:call Getpid()<CR>
noremap  <buffer> <LocalLeader>l 		:call TEX() <CR>	
inoremap <buffer> <LocalLeader>l<Left><ESC> :call TEX() <CR>a
noremap  <buffer> <F5> 			:call VTEX() <CR>	
noremap  <buffer> <S-F5> 			:call ToggleAuTeX()<CR>
inoremap <buffer> <F5> <Left><ESC> 		:call VTEX() <CR>a
noremap  <buffer> <LocalLeader>sb		:call SimpleBibtex()<CR>
noremap  <buffer> <LocalLeader>b		:call Bibtex()<CR>
noremap  <buffer> <F6>d 			:call Delete() <CR>
inoremap <buffer> <silent> <F6>l 		:call OpenLog() <CR>
noremap  <buffer> <silent> <F6>l 		:call OpenLog() <CR>
noremap  <buffer> <LocalLeader>e 		:cf<CR> 
noremap  <buffer> <F6>w 			:call TexLog("-w")<CR>
inoremap <buffer> <F6>w 			:call TexLog("-w")<CR>
noremap  <buffer> <F6>r 			:call TexLog("-r")<CR>
inoremap <buffer> <F6>r 			:call TexLog("-r")<CR>
noremap  <buffer> <F6>f 			:call TexLog("-f")<CR>
inoremap <buffer> <F6>f 			:call TexLog("-f")<CR>
noremap  <buffer> <F6>g 			:call Pdffonts()<CR>
noremap  <buffer> <F1> 	   			:!clear;texdoc -m 
inoremap <buffer> <F1> <Esc> 			:!clear;texdoc -m  
noremap  <buffer> <LocalLeader>p 		:call Print(g:printeroptions)<CR>

" FONT COMMANDS
inoremap <buffer> ##rm \textrm{}<Left>
inoremap <buffer> ##it \textit{}<Left>
inoremap <buffer> ##sl \textsl{}<Left>
inoremap <buffer> ##sf \textsf{}<Left>
inoremap <buffer> ##bf \textbf{}<Left>
	
inoremap <buffer> ##mit \mathit{}<Left>
inoremap <buffer> ##mrm \mathrm{}<Left>
inoremap <buffer> ##msf \mathsf{}<Left>
inoremap <buffer> ##mbf \mathbf{}<Left>

" GREEK LETTERS
inoremap <buffer> #a \alpha
inoremap <buffer> #b \beta
inoremap <buffer> #c \chi
inoremap <buffer> #d \delta
inoremap <buffer> #e \epsilon
inoremap <buffer> #f \phi
inoremap <buffer> #y \psi
inoremap <buffer> #g \gamma
inoremap <buffer> #h \eta
inoremap <buffer> #k \kappa
inoremap <buffer> #l \lambda
inoremap <buffer> #i \iota
inoremap <buffer> #m \mu
inoremap <buffer> #n \nu
inoremap <buffer> #p \pi
inoremap <buffer> #o \theta
inoremap <buffer> #r \rho
inoremap <buffer> #s \sigma
inoremap <buffer> #t \tau
inoremap <buffer> #u \upsilon
inoremap <buffer> #vs \varsigma
inoremap <buffer> #vo \vartheta
inoremap <buffer> #w \omega
inoremap <buffer> #x \xi
inoremap <buffer> #z \zeta

inoremap <buffer> #D \Delta
inoremap <buffer> #Y \Psi
inoremap <buffer> #F \Phi
inoremap <buffer> #G \Gamma
inoremap <buffer> #L \Lambda
inoremap <buffer> #M \Mu
inoremap <buffer> #N \Nu
inoremap <buffer> #P \Pi
inoremap <buffer> #O \Theta
inoremap <buffer> #S \Sigma
inoremap <buffer> #T \Tau
inoremap <buffer> #U \Upsilon
inoremap <buffer> #V \Varsigma
inoremap <buffer> #W \Omega

inoremap <buffer> [b \begin{}<Left>
inoremap <buffer> [e \end{}<Left>
inoremap [s \begin{}<CR>\end{}<Up><Right>

inoremap <buffer> ]c \begin{center}<Cr>\end{center}<Esc>O
inoremap <buffer> [c \begin{corollary}<Cr>\end{corollary}<Esc>O
inoremap <buffer> [d \begin{definition}<Cr>\end{definition}<Esc>O
inoremap <buffer> ]e \begin{enumerate}<Cr>\end{enumerate}<Esc>O
inoremap <buffer> [q \begin{equation}<Cr>\end{equation}<Esc>O
inoremap <buffer> [a \begin{align}<Cr>\end{align}<Esc>O
inoremap <buffer> [x \begin{example}<Cr>\end{example}<Esc>O
inoremap <buffer> ]q \begin{equation}<Cr>\end{equation}<Esc>O
inoremap <buffer> ]l \begin{flushleft}<Cr>\end{flushleft}<Esc>O
inoremap <buffer> ]r \begin{flushright}<Cr>\end{flushright}<Esc>O
inoremap <buffer> [i \item  
inoremap <buffer> ]i \begin{itemize}<Cr>\end{itemize}<Esc>O
inoremap <buffer> [l \begin{lemma}<Cr>\end{lemma}<Esc>O
inoremap <buffer> [n \begin{note}<Cr>\end{note}<Esc>O
inoremap <buffer> [o \begin{observation}<Cr>\end{observation}<Esc>O
inoremap <buffer> ]p \begin{proof}<Cr>\end{proof}<Esc>O
inoremap <buffer> [p \begin{proposition}<Cr>\end{proposition}<Esc>O
inoremap <buffer> [r \begin{remark}<Cr>\end{remark}<Esc>O
inoremap <buffer> [t \begin{theorem}<Cr>\end{theorem}<Esc>O
inoremap <buffer> 	 ]t \begin{center}<CR>\begin{tikzpicture}<CR><CR>\end{tikzpicture}<CR>\end{center}<Up><Up>

" inoremap {c \begin{corollary*}<Cr>\end{corollary*}<Esc>O
" inoremap {d \begin{definition*}<Cr>\end{definition*}<Esc>O
" inoremap {x \begin{example*}\normalfont<Cr>\end{example*}<Esc>O
" inoremap {l \begin{lemma*}<Cr>\end{lemma*}<Esc>O
" inoremap {n \begin{note*}<Cr>\end{note*}<Esc>O
" inoremap {o \begin{observation*}<Cr>\end{observation*}<Esc>O
" inoremap {p \begin{proposition*}<Cr>\end{proposition*}<Esc>O
" inoremap {r \begin{remark*}<Cr>\end{remark*}<Esc>O
" inoremap {t \begin{theorem*}<Cr>\end{theorem*}<Esc>O

inoremap <buffer> __ _{}<Left>
inoremap <buffer> ^^ ^{}<Left>
inoremap <buffer> [m \[\]<Left><Left>
endif


command! -buffer Texmf 	:tabe /home/texmf/tex
command! -buffer Arrows :tabe /home/texmf/tex/arrows.tex
" command! -buffer Bib 	:tabe ~/bibtex/
" command! -buffer BibM 	:tabe ~/bibtex/Mat.bib
" command! -buffer BibGT 	:tabe ~/bibtex/GameTheory.bib
command! -buffer Settings :tabe /home/texmf/tex/settings.tex

syn match texTikzCoord '\(|\)\?([A-Za-z0-9]\{1,3})\(|\)\?\|\(|\)\?(\d\d)|\(|\)\?'
ftplugin/bibsearch_atp.vim	[[[1
101
" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Jan 25
" URL:		

map <buffer> c :call BibChoose()  <CR>
map <buffer> q :hide<CR>

if !exists("*BibChoose")
function! BibChoose(...)
"     if a:0 == 0
" 	let l:bibresults=s:searchbib('')
" 	let b:listofkeys=s:showresults(l:bibresults,'','')
"     elseif a:0 == 1
" 	let l:bibresults=s:searchbib(a:1)
" 	let b:listofkeys=s:showresults(l:bibresults,'',a:1)
"     else
" 	let l:bibresults=s:searchbib(a:1)
" 	let b:listofkeys=s:showresults(l:bibresults,a:2,a:1)
"     endif
    let l:which=input("Which entry? (enter for none) ")
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
	    q
	    echohl WarningMsg | echomsg "Choice written to the register '" . l:letter . "'" | echohl None
    endif
endfunction
endif
command! -buffer -nargs=* BibChoose 	:call BibChoose(<f-args>)

doc/automatic-tex-plugin.txt	[[[1
969
			      *automatic-tex-plugin* *atp* 
			Introduction to Automatic (la)TeX Plugin
				by Marcin Szamotulski
			----------------------------------------

If you found this plugin useful, you are cordially invited to write to me
mszamot@gmail.com, this is my first vim plugin, and its a great pleasure 
to get even the smallest message.


					Abstract
					========	

This is a new plugin for vim to comfortably write TeX (LaTeX, PdfLaTeX)
documents, which provides functionality not met in other plugins.  It makes
you FREE from compiling procedure, making this process automatic using
autocommands. It also provides useful mappings and ways to analyse your .log
file.  To have full functionality you need: texloganalyser (a perl program
written by Thomas van Oudenhove) and pdffonts available in the package
app-text/poppler-utils (at least in Gentoo). Another good tool is texdoc,
which is a part of texlive these day standard TeX distribution for Linux.

We provide a powerful function to search in bibliographic files (bib files).

                                                		*atp-help-toc*
|atp-installation| 	Installation								
|atp-functions| 	Functions
|atp-bibsearch|		Searching in bib files
|atp-configure| 	How to configure to your needs 
|atp-mappings|  	Mappings and Commands
|atp-errors|  		Error handling
|atp-requirements|  	Requirements
|atp-viewers| 		Note about viewers
|atp-remarks|  		Final remarks

	
Note on usage: type :help atp<CTR>d to see all the helptags. To see help tags
for all the defined functions :help atp*()<CTR>d, mappings: :help atp-map<CTR>d

================================================================================
Installation                               			*atp-installation*
>
	" :filetype plugin on is required to run this plugin, see
	" |:filetype-plugin-on| and |:filetype-indent-on| if you want to have
	" automatic intendation for TeX files.
<
To install you just need to copy tex.vim file to ~your ~/.vim/ftplugin/
directory copy this help file to ~/.vim/doc and then run :helptags ~/.vim/doc
and that's all, now you can just type your story ... :)


================================================================================
Functions                               			*atp-functions*

The main function is not seen by the user (it is called s:compiler, for those
who want to read the plugin). It executes tex compiler specified by the
variable b:texcompiler after checking if the file has changed. It is executed
as an autocommand by the line:
	au! CursorHold $HOME*.tex silent call 's:auTeX()'
where s:auTeX() is a simple function which calls s:compiler.
As you can see it will run if a key is not pressed during time defined by
option 'updatetime' (see |CursorHold|) in the normal mode. If you type in
insertmode the file wan't be compiled (and that's alright as you can be in the
middle of your very long formula). The value of 'updatetime' which works fine
is around 1000ms ('updatetime' is set in miliseconds). The function s:auTeX() 
if finds a difference between what is on you hard drive and what is in your
buffer runs tex compiler (configured in b:texcompiler) with options:
	-output-directory 
	-jobname
which points to a unique temporary file in vim temporary directory (using the
function 'tempname()' (see |tempname()|. If you are concerned with security
reasons read also: |shelltemp|, ... (THERE IS A HELP ON: HOW TO SET
PERMISSIONS TO TEMPORARY FILES)

You can switch off/on the function s:auTeX by pressing <S-F5>  or by letting
the local to buffer variable b:autex=1 (on) b:autex=0 (off). It is useful in
some situations. The key <S-F5> calls the function ToggleAuTex() which sets
the variable b:autex and issue a message. 

The second important variable b:texcompiler (see |b:texcompiler|) configures
if you use TeX, PdfTeX, LaTeX, PdfLaTeX and it should point to the program
name so please do not use capital letters.

Next variable to set is b:outdir (see |b:outdir|). It configures where TeX
will put the output and where viewer and log analyzing tools can find
appropriate files.

The last top most important variable is |g:keep| which is a list of extensions,
by default it is
	let g:keep = ["log","aux","toc","bbl"]
Files with this extension will be copied from b:outdir to the temporary
directory with appropriate name to be used when (La)TeX is compiling. (log file
will be only copied after it is created, other files will be copied back and
forth between you b:outdir and the temporary directory)

There is also a variable which stores the last command which executed
your tex compiler, see |b:texcommand|.   

The last remark is that it is convenient to see if tex is running, if you are
using a simple window manager like DWM or WMII or some others you can do that
in your .xinitrc file. If you are using more complex window manager (Gnome,
KDE) you can use the command GPID (or which is mapped to \g) which returns pid
of |b:texcompiler| if it runs. The vim status line could be used for that but
it seems to be to slow and the screen gets messy (at least for me).

If you have defined a function with the same name it wan't be overwritten.
You can check where the function was defined by the command
	:verbose function TEX
the last component is the name of the function (without () at the end).	

Below I explain functions which are defined:
/all of them do not have any arguments/

TEX([runs])						*atp-TEX()*
map \l,imap \ll, :TEX 

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

Bibtex([debug])						*atp-Bibtex()* 
map \b, :Bibtex, :Bibtex v
	This function will call bibtex to produce the bibliography file
	(.bbl). If in you |b:outdir| there is no aux file it first calls your
	tex compiler. After the bbl file is produced two consecutive runs of
	tex compiler are called to make your bibliography.

	If you specify any value to the debug variable then then this function
	will be called in verbose mode (only the last time tex compiler will
	run in errorstop mode). This gives you the chance to see the output of
	bibtex command for a second. The command :Bibtex v is associated to
	this behaviour.

	The command :Bibtex  will :call Bibtex(), while :Bibtex v
	(and :Bibtex <anything>) will :call Bibtex(1)

SimpleBibtex()						*atp-SimpleBibtex()*
map \sb, :SBibtex
	This calls bibtex on the aux file in your |b:outdir| directory and
	shows you the output. It is useful if you are debugging your
	bibliography database. 
	
ViewOutput()						*atp-ViewOutput()*
map \v,map <F3>, imap \v, imap <F3>  

	You would like to see what you are editing use this function. It will
	use the program defined in the variable b:Viewer. See |b:Viewer|,
	|b:XpdfServer|, |b:XpdfOptions|. When there is no output file it will run
	TeX and open the file.

BibSearch({pattern},{flag})				see |atp-BibSearch|
:BibSearch
	Finds bibentries in bibfiles given in yout tex file, which match the
	pattern the output is configurable by the flag argument see
	|atp-bibflags|.

BibChoose						see |atp-BibChoose|
map c,:BibChoose			/only in the buffer with search results/
	It lets you copy the bib key directly to your tex file (after the
	last cursor position) or to a choosen register see
	|atp-bibchoose-register|.

FindBibFiles({bufname})					*atp-FindBibFiles()*
:FindBibFiles
	This updates the variables s:bibfiles, s:allbibfiles,
	s:notreadablebibfiles. Finds all bib files defined in all
	\bibliography commands. For more about the above variables read
	|atp-variables-bib|. This function is called internally be the
	script functions BibSearch/BibChoose/ShowOptions.
	The command is called with the argument=the name of current buffer. 

OpenLog()						*atp-OpenLog()*
map <F6>l, imap <F6>l
	Opens log file in a new tab with two options (which are set
	locally): ruler, nospell.	

Delete()						*atp-Delete()*
map <F6>d
	Delets the files produced by TeX wich extensions belongs to
	g:texextensions (see |g:texextensions|). It removes not taking care
	about the name, i.e. it runs like: rm *.out.

Print()							*atp-Print()*
map \p
	It will run lpr command and append to it the options defined in the
	variable g:printeroptions. It prints the pdf file if you are using
	Pdf(La)TeX  or the dvi file if you are using (La)TeX. Depneding, what
	you have in |b:texcompiler|.

ShowOptions()						*atp-show-options* *atp-ShowOptions()*
:ShowOptions, :ShowOptions v 
	This will show values of values that you use. If you specify any
	argument additionaly the deafult values will be shown is suqare brackets.
>
		:ShowOptions v
<		
	
TOC()
:TOC
	List a table of contents of the eddited file /there are other better
	ways to do that, for example using ctags program (see |ctags|). Also
	tagslist plugin seems to be good.  Hopefuly it will develope to
	something more useful. 

================================================================================
Searching in bib files 		                        *atp-bibsearch*

		
		____________________________
		Naming Convensions:	
		@article{<label>,					\	
			author = { .... },		<-- bibentry    | 
			title  = { .... },				 > bibfield
			journal= " .... ",				|
		}							/	

			article 		<-- bibfield keyword 
			author,title,...	<-- bibentry label 	
			<label>			<-- bibfield label 	

One more function is provided which searches the bib files for bibfields, 
and for the bibfield labels for the latex command \cite{}.

BibSearch([pattern],[flags])				*atp-BibSearch* 

	BibSearch finds bibfields in bibliographis found in your tex source
	file and defined in the variable b:bibfiles (which is |List|). It
	shows the result in a new buffer (which is named after the pattern),
	where there are two mappings c - for choose and q - for quit. The
	first one calls the function BibChoose, wich  allows you to copy the
	keyword of the whole bibentry directly to your source file or to the
	register, for the useg see |atp-bibchoose:input|. The second key: 'q',
	is mapped to :hide, so that you can open the window afterwards.
	The buftype option is set to nowrite. 

	Former searches are accesible unless you delete their buffers, use :ls
	to see which searches are done. 

	Both takes two arguments (the last one is optional), first one is the
	pattern to match against each line of the bibliographic files supplied
	in the commands \bibliography (if there are several names,please do
	not add a white space ' ' after ',' unless the file name begins with a
	space. i.e.
>
 "	\bibliography(Mathematics, Physics,/home/user/Bibliography)
< 
	then the plugin will understand that the names of the bib files are
	'Mathematics.bib', ' Physics.bib' and '/home/user/Bibliography.bib'.

								*atp-bibsearch-patterns*
								*atp-bibsearch-examples*
	Each line of every bib file found in your tex docummnet will be
	matched against the pattern, for example 
>
 "	if pattern='author.*Grothendieck'
<
 	will result in all the bibliographic positions which in one line have
	the words 'author' and 'Grothendieck' (in most cases it means that you
	will see only works of Grothendieck). Another example:
>
 "	if pattern='author.*Joayl\|Galois Theory'
<
	will result in all bibfields which author is Joyal or 
	which includes the words 'Galois Theory' (which by the way apear in
	many article/book tiles):	
>
 "	if pattern='author.*Joayl\|title\p*Galois Theory'
<
	This will match against all bibentries written by Joyal or which title
	includes the word 'Galois Theory'.
> 	
 "		:call BibSearch('author.*Joyal\&.*Tirney')	
<	
	will find all the bibentries which were written by Joyal and Tirney
	(and maybe somebody else). 

	For now, there is no possibility to filter bibliographic entries which
	both match a pattern in separate lines, i.g. to show all bibentries
	written by Joyal on 'Descent Theory'.

	Before a match, all '{', and '}' are deleted from the line of the bibfile.
	But you will see them in the output (what can be useful for debugging
	errors in bibfiles)

	Note that in vim patterns should be quoted using '...' not "...".   

	Further examples are supplied after the next section
	|atp-bibflags-examples|, which describes other functionalities
	of the BibSearch/BibChoos functions.

								*atp-bibsearch-flags*
								*atp-bibflags*
	The first optional argument [flags] chooses what and in which order
	you want to see the  bibentries found (entries are listed in
	the order they appear in bib file).  Flag is a word made of letters.
	There are three kinds of flags: entry flags which mathes againts
	labels of bibentries, like author, title, etc..., and keyword flags: which
	matches agains keywords of bibfields: @article, @book, @techreport,
	etc...  and two special flags 'All' and 'L'. A flag is a word on
	letters:
>
 "		a  - author
 " 		e  - editor
 " 		t  - title
 " 		b  - booktitle
 " 		j  - journal
 " 		s  - series
 " 		y  - year
 " 		n  - number
 " 		v  - volume
 " 		p  - pages
 " 		P  - Publisher
 " 		N  - Note
 " 		S  - School
 " 		h  - howpublished
 " 		o  - organization
 "		u  - url	
 "		H  - Homepage	
 "any other letter - do not show anything but the first line of bibentry 
 "		@a - article 						/@article/
 "		@b - book or booklet 					/@book,@booklet/
 "		@B - Booklet 						/@booklet/	
 "		@c - incollection 					/@incollection,@inbook/
 "		@p - proceedings, inproceedings, conference   		/@proceedings,@inproceedings,@conference/
 "		@m - misc 						/@misc/
 "		@M - Manual 						/@manual/
 "		@t - master or PhD thesis  				/@masterthesis,@phdthesis/
 "		@T - Techreport 					/@techreport/
 "		@u - unpublished  					/@unpublished/		
 "		All - all flags						(see |atp-bibflags:all|)		
 "		L   - last flags					(see |atp-bibflags:last|)		
<

	Examples:
>
 "		tayu@a		--> show the entries: tile, author, year, url of matching articles.
 "		baeP@b		--> show the entries: booktitle, author, editor, 
 " 							publisher of matching books (@book,@booklet).
< 
	If flags '@.' are filtered out, if one do not belongs to the one above
	then it is deleated. You can see which flags are defined using
	ShowOptions function/command (they are listed as Available
	KeyWordFlags).
								*atp-g:defaultbibflags*
								*atp-bibflags:default*
	The default flag is stored in the global variable g:defaultbibflags and is
	equal to 'tabejsyu'. This means that the ouput for each bibfield found 
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
	example 'tabe' is quite reasonable (note that all bibentries are
	matched separately, i.e. if a bibfield has both 'title' and 'booktitle'
	bibentries it will give you both of them.

								*atp-bibsearch-show-only-keys*
	If you just want to list just the lines with bibfields keywords:
	@article{, @book{, etc. supply a flag which do not belongs to
	'g:defaultallbibflags', for example 'X', or 'X@a'
	
								*atp-bibflags:+*
	You can also specify flags with '+', for example: 
>	
 "	flags='+p'
 "	flags='+@b'
<
	This feature ADDS FLAGS TO THE DEFAULT VALUE defined in
	|g:defaulbibflags|. The first will result in showing the default
	entries and the page number, the second will result in showing only
	books with the default bibentries. You can specify as many additional
	flags as you wish. 
								*atp-bibsearch-output*
	Note that the function shows the line with database file name if
		there are entries in this bibliography 
		which match the pattern
	thus,for example, if you specify the flag '@a' and you see the line
	with database file name, but you do not see any bibfield under, then
	in this database there are bibfields which match but these are not
	articles. 	
	
								*atp-bibflags:all*
								*atp-g:defaultallbibflags*
	The flags='All' is a synonim of flag=g:defaultallbibflags which by default is
	equal to'tabejfsvnyPNSohiuHcp' i.e. all flags in this order. If you
	add your own flag you should change this global variable. You can add to
	this flag any flag which contains '@' (see |atp-bibsearch-flags|) by
	the plus operator, i.e. All+@a@b or +@aAll will give the same result.

								*atp-bibflags:last*
								*atp-b:lastbibflags*	
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
 "    let g:bibflagsdict=extend(g:bibflagsdict, { '<oneletter>' : [ '<bibentryname>': '<how to show>'] })
< 
    	where, <oneletter> is the flag to use, it must be different from the
	dfined flags, <bibentry_name> is a lowercase bibentry name, like
	'title', 'url', etc., <how to show> if you want to have a nice output
	put the bibentry name and that much of white spaces to get 13 strings.

		
								*atp-bibchoose-register*
	The function/command BibChoose ask you for the number of the biblabel to
	copy to your file (after the current cursor position) or to a
	specified register.

	    If you give just a number, e.g. '12', then the bibfield label of this
	    bibfield will be inserted after the cursor position, if you
	    specify number followed by a register name, i.g. '12e', '12+',
	    '12-', then the label will be copied to this register. Registers
	    which you can use are [a-z*+-].

	    Press <Enter> if you do not want to copy anything.

								*atp-bibsearch-highlight*
		
	Syntax for the buffer keeping the results of the BibSearch
	command are defined in the file syntax/bibsearch.vim. It is based on
	the syntax file for bibfiles, and by default links the groups to the
	groups for bibfiles. There is one group more:
		bibsearchInfo
	which is used to highlight the number and line numbers of found
	entries. (Actually, just one group changed, comparing
	to syntax file for bibfiels, bibComment --> bibsearchComment).

	There are two commands to make the life easier:
								*atp-BibChoose-command*
							 	*atp-BibSearch-command*
:BibSearch [pattern] [flag] 
:BibChoose 			/available only in the buffer with results of :BibSearch./
map c

	which do what you expect. The arguments should not be quoted and
	should be separated with a white space (if you want to include a white
	space in an argument use '\ '), for more see |f-args|. If you do not
	provide any argument then all entries of all bibfiles will be shown.
	Examples:

								*atp-bibflags:examples*
	Some examples:
>
 "	:BibSearch 
<				this is a tip how to show all bibfields with
				the default flags
>
 "	:BibSearch @ yt	
<				this is a tip how to show all bibfields with
				different flags than the default ones, which
				is equivalent to:
>
 "      :call BibSearch('','yt')
< 
>
 "	:BibSearch title[\s.]*Galois Theory  aetb
<
	The next one shows all bibfileds which were written by Joyal and
	Tirney (and mayby somebody else).
> 	
 "	:BibSearch 'author.*Joyal\&.*Tirney'
<	

								*atp-bibsearch-variables*
								*atp-variables-bib*	
SOME VARIABLES:
	All the values of important variables can be shown by ShowOption
	command.

								*atp-b:bibfiles*
b:bibfiles
	This variable is a list and you can put here additional bib files.
	They will be parsed by the BibSearch function.
s:bibfiles
	This variable is a list which stores bib files found in your tex files
	and which are readable. It is set up when you first run of the
	commands: BibSearch/ShowOptions. Its value is produced by the
	functions FindBibFiles({bufname}).
s:allbibfiles 
	this is a sum of found bibfiles the locally defined b:bibfiles, which
	not necessarily are readable.
s:notreadablebibfiles
	guess what :)



-----------------------------------------------------------------------------------
								*atp-bibsearch-bugs*
	Please do not hesitate to report any bug to me:
	mszamot@gmail.com 							
	
	The algorithm will work only if all kind of bibentries of your bibfile
	are included in the list g:bibentries. However, changing just this
	variable is not enough. In that case the search engine (function
	s:search) will produce correct output, but the function which
	displays found entries, will not know how to work with the new
	entries. One would have to also add an entry to the dictionary
	'g:bibflagsdict'. If it is the case, please let me know:
	mszamot@gmail.com  

	As you can see entries of the type '@string' which can be used in bib
	files are not supported (i.e. there is no function which substitutes
	the variables defined in @string to their values), but it is doable.
			@string{ Name = Value }
			
------------------------------------------------------------------------------------	
	Comments: for a bibfile with over 16000 lines and 1675 entries, it
	took on my computer (AMD Core 2Duo 1.9Ghz) 25.5s to find all entries,
	with the command ':BibSearch . All' and 1s to show results (the
	longest loop took over 100000 times). However to find in this database
	one entry with default flags (:BibSearch <Name>) it took 2.84s
	(exactly 79 matching bibfields), and to display them another 0.6s. 

	If for you the algorithm is too slow, there is a way to make it more
	efficient: the problem with when every line is matched: when
	we are in line x we go back to find the nearest line of the form
	@article{, @book{, etc., if we find it we go to line x+1 and do it
	once again. We could remember previous line numbers and test if we
	already have done the calculations. This can avoid  long loops.

================================================================================
How to configure to your needs                          *atp-configure*
							*atp-variables*
There are several options you can set, and they might be set in your vimrc
file, unless you want to use the default, which I provide below.

All buffer variables (see |b:var|), i.e. these which name begins with "b:" can
be should be set in you vimrc file. The best way to do that is by using
autocommand:
	au BufReadPre *.tex let b:texcompiler="latex"
If you put just let b:texcompiler, this will also work but only open vim from
terminal (not when you want to start edditing next buffer). 

let b:texcompiler="pdflatex" 					*b:texcompiler*
	Used by functions: TEX() (map \l, imap \l), VTEX() (map <F5>, imap <F5>)

	You can set it to latex, tex, luatex, and so on and possibly to
	lilypond. 

let b:texoptions=""
	If you want to set some additional options to your texcompiler, note
	that, -jobname, -output-directory, -mode, are already used. 

								*b:outdir*
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

	You can see the current output directory in the status (it is in short
	notation) to see it whole type
		:echo b:outdir
	or use the function ShowOptions() (see |apt-ShowOptions()|).		

let g:askforoutdir=0						*g:askforoutdir*
	Its values are 1 and 0.  When it is set to 1 you will be asked for the
	name of a directory where tex will put output files, note that this
	name should end with a "/".

let b:Viewer="xpdf"						*b:Viewer*
	it was tested with xpdf, evince, epdfviewer, kpdf, okular, xdvi and
	they all works fine.  I'm using xpdf and the xpdf server options are
	supported so that the file is automatically reloaded (other viewers,
	except epdfview, have this functionality as well, except acroread in
	which somehow it do not works. Read more about viewers in
	|atp-viewers|. 

let b:XpdfOptions=""						*b:XpdfOptions*
	Used by function: ViewOutput() (map \v, map <F3>, imap <F3>)

	For example if you want to have different look of one document you can
	set it to "-bg gray20". Other example:

	let b:XpdfOptions="-bg Grey30 -mattecolor SlateBlue3 -papercolor SlateBlue4"

let b:XpdfServer=fnamemodify(expand("%"),":t")			*b:XpdfServer*	
	Used by function: ViewOutput() (map \v, map <F3>, imap <F3>)

	that is it is equal to the name of your open file. You do not need
	escape spaces in the name (shellescape() function is used before it is
	send to your shell).

let b:openviewer=1						*b:openviewer*
	If the function which calles TeX compiler do not see that you are
	viewing the output file it will open it for you if b:openviewer=1.
	Otherwise, this feature is disabled.

let g:rmcommand="perltrash"					*g:rmcommand*
	Used by function: Delete() (map <F6>d imap <F6>d)	

	If you have another trash can program you can use it here, if you do
	not have it you can use "rm" (at your own risk). Now, it is used to
	delete the files produced by (La)TeX by the function Delete() (see
	|apt-Delete()|). It will remove all files in the output directory (see
	|b:outdir|), which ends with
	an extension defined in the list g:texextensions  
	
	If you set 
		let g:rmcommand=""
	then the function Delete() (see |apt-Delete()|) will use the vim
	|delete()| command, and will delete only the files produced by the
	current .tex file. The temporary directory is cleared by rm command.

	The program 'perltrash' is in the package app-misc/perltrash (at least
	for Gentoo)

								*g:texextensions*	
let g:texextensions=["aux", "log", "bbl", "blg", "spl", "snm", "nav", "thm", "brf", "out", "toc", "mpx", "idx", "maf", "blg", "glo", "mtc[0-9]", "mtc1[0-9]"]	
	 These list is used by the function Delete() (see |apt-Delete()|)
	 which delets all the file with this extension in the directory
	 b:outdir, unless g:rmcommand="" (see |g:rmcommand|) whene Delete()
	 deletes only the output files for the current buffer.
									
let g:keep=["log","aux","toc","bbl"]				*g:keep*
	
	Files with this extension will be copied from 'b:outdir' to the
	temporary directory with appropriate name to be used when (La)TeX is
	compiling. (log file will be only copied after it is created, other
	files will be copied back and forth between you 'b:outdir' and the
	temporary directory). These four are essentially minimum to work with 
	table of contents and bibtex, possibly other classes like beamer or
	packages like theorem (produced .thm files) have more special needs.
	You can change this variable by the command:
		:let g:keep+=["thm","spl"]
								
let g:printeroptions=""						*g:printeroptions*
	You can set the printer options. These are options for the lpr
	command, which will print the output file (pdf or dvi) this depends on
	the b:texcompiler that you use.

b:texcommand							*b:texcommand*
	This variable is for debugging, it stores the last executed command to
	compile your document. This changes also when your compiler was run
	automatically.>
		:TEX
		:echo b:texcommand
		:TEX 2
		:echo b:texcommand

let g:vartical=1		
	If set to 1 BibSearch will split the window vertically, if set to
	other value it will split the window horisontally.
<		
		
g:defaultbibflags		see |atp-bibflags:default|
g:defaultallbibflags		see |atp-bibflags:all|
b:lastbibflags			see |atp-bibflags:last|

b:bibfiles			see |atp-variables-bib|
s:bibfiles
s:allbibfiles
s:notreadablebibfiles
	For more on bib flags see |atp-bibsearch-flags|.
	

================================================================================
Mappings and Commands                     		*atp-mappings*

Lots mappings which are given here uses # which is in a convenient place on
British keyboards, but not in the US layout, you can change them for '`' or
some other key that it is not used in vim (there are not many of them
though). The most commonly used latex-suite plugin uses similar set of
mappings (but there might be some differences).

These mappings are loaded unless you set one of variables: 'no_plugin_maps' or
'no_atp_maps'.

Note: in all mappings "\" is really set to your <LocalLeader> (and thus, in
fact, the mapping can differ).

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

								

texdoc:							*atp-map-texdoc*
<F1> 	   
	This is both map and imap. Then You have to type what you are looking
	for and press enter. The option 'keywordprg' set to 'texdoc -m', i.e
	when your curosr is over a package name and you press 'K' key then you
	should see the package document file.

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
	set errorfile=  log file in your b:outdir directory
This allows you to use |quickfix| commands, for example to read the error file
use :cg (see |cg|), to jump to the first error :cf (see |cg|), to list all
errors :cl (see |cl|), read |errorformat| if you want to change the output of
this commands

								*atp-errors-bibsearch*
A possible error which may accour using the :BibSearch and :BibChoose commands
has a simple cause: we count number of brackets '()', '{}' and '"' (but nor
'\"') to see where the bibentry ends and where to join lines. The message
error is echoed when more than 30 lines where processed and the matching
bracket was not found (or the number of '"' is odd). Look at your bib file at
specified position. Syntax highlighting for bib files can help you finding
where is such an error. (After reading the bibfiles comment lines and lines
which begin with @string are removed, so that the brackets in comment lines do
not count)

================================================================================
Requirements							*atp-requirements*

This plugin requires vim version higher than 7, however a some simple changes
can make it run on older versions as well (the 'source code' uses lists /g:keep
and let g:texextensions are the only ones). 

It is nice to have 'texdoc' program. This plugin maps <F1> to :!texdoc -m ...
and allows to speed up your searches. Also the option 'keywordprg' has the
value "texdoc -m", thus pressing 'K' (see |K|) over a texpackage should open
you the package documentation. It is also a good advice to install help
files of latex-suite, you can find there a big part of 'Non so short
introduction to Latex' by Tobias Oetiker (copy them to your ~/.vim/doc
document then use |:helptags| to install the helptags :helptags ~/.vim/doc and
that's it). The same applies to this help file.

Another good programs are texloganalyzer and pdffonts wchich are not required
by there are some mappings to defined (see |texlog| and |pdffonts|).

================================================================================
Notes on Viewers                               			*atp-viewers*

xpdf	
	It is fully supported. 
	It is configured in the way that when your tex file have errors, 
	xpdf viewer will not reload your file, which I found useful. 

	You can set your own options of xpdf using b:XpdfOptions, for example
		let b:XpdfOptions="-bg grey20"
	will make xpdf view different. This is helpfull when you edit to
	files, and do not want to xpdf mix them. Other useful options are
	-papercolor and -mattecolot for example
		let b:XpdfOptions="-bg Grey30 -mattecolor SlateBlue3 -papercolor SlateBlue4"
	for something which is more eye friendly.	
evince
	Works fine.
okular
kpdf
	Works fine (moves page a little bit when updates a file).
epdfview
	This viewer does not support automatic reloads when the file
	changes (but it seems that the work is in progress). You have to
	issue CTRL-R yourself when the file is changed.
acroread
	As with epdfview (with the difference that it supports automatic
	updates, but it do not works somehow)
    xdvi
	Works fine.


================================================================================
Final Remarks                               			*atp-remarks*
	
	To see some messages that are issued you can use :messages command.

	If you find this plugin useful and have some comments you are
	cordially invited to write to the author: <mszamot@gmail.com>.

	There are other ways to make such an automatic plugin. The crucial
	step is to make the plugin know that tex is already in use (if you run
	to tex compilers on the same file at the same time the effect won't be
	good).

	For several month I was using a different code which was not using
	temporary copies but was checking if the tex is running. It was
	working very good but I didn't understand why (silly isn't it), and
	when I started making changes it stopped working: the issue is that
	it is difficult to make a function sleep until tex stops working not
	putting whole vim into sleep - this the time that we want to save.
	However, the advantage of using temporary files is smoothness (changes
	appear faster in the output file). 

	Best regards, and hopefully you will find this useful :) 
	Marcin Szamotulski
	
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
	\! - a negative thin space		*matn!*

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
syntax/bibsearch.vim	[[[1
100
" Vim syntax file
" Language:	bibsearchTeX (bibsearchliographic database format for (La)TeX)
" Author:	Bernd Feige <Bernd.Feige@gmx.net>
" Modified:	Marcin Szamotulski
" Last Change:	Feb 01, 2010

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

" Ignore case
syn case ignore

" Keywords
" ========
syn keyword bibsearchType contained	article book booklet conference inbook
syn keyword bibsearchType contained	incollection inproceedings manual
syn keyword bibsearchType contained	mastersthesis misc phdthesis
syn keyword bibsearchType contained	proceedings techreport unpublished
syn keyword bibsearchType contained	string

syn keyword bibsearchEntryKw contained	address annote author booktitle chapter
syn keyword bibsearchEntryKw contained	crossref edition editor howpublished
syn keyword bibsearchEntryKw contained	institution journal key month note
syn keyword bibsearchEntryKw contained	number organization pages publisher
syn keyword bibsearchEntryKw contained	school series title type volume year
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
syn region bibsearchQuote contained start=/"/ end=/"/ skip=/\(\\"\)/ contains=@bibsearchVarContents
syn region bibsearchBrace contained start=/{/ end=/}/ skip=/\(\\[{}]\)/ contains=@bibsearchVarContents
syn region bibsearchParen contained start=/(/ end=/)/ skip=/\(\\[()]\)/ contains=@bibsearchVarContents
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
syn region bibsearchInfo start=/\s*\d/ end=/\s@/me=e-1 contained contains=@bibsearchCommentContents 
syn region bibsearchComment start=/./ end=/\s*@/me=e-1 contains=@bibsearchCommentContents,@bibsearchSearchInfo nextgroup=bibsearchEntry

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
  HiLink bibsearchBrace  	bibBrace
  HiLink bibsearchParenendif	bibParenendif
  HiLink bibsearchField		bibField
  delcommand HiLink
endif
let b:current_syntax = "bibsearch"
