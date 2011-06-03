" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" $Revision: 1.4 $
" $Date: 2001/08/13 08:29:52 $
"
" Tested with Vim 6.0
" Primary site : http://ezytools.sourceforge.net/
" Located in the "VimTools" section
"
" ToDo : set CVS options (compression, cvsroot)
" ToDo : CVS add / remove / login / logout
" ToDo : support for directories

"unmenu CVS

if has("unix")
  let $sep='/'
else
  let $sep='\'
endif

if ($temp == "")
  let $temp=expand("%:p:h")
endif

"-----------------------------------------------------------------------------
" Menu entries
"-----------------------------------------------------------------------------

nmenu &CVS.in&fo        	:call ShowCVSinfo()<cr>
nmenu &CVS.&Diff		:call CVSdiff()<cr>
nmenu &CVS.&Annotate		:call CVSannotate()<cr>
nmenu &CVS.&Log			:call CVSlog()<cr>
nmenu &CVS.&Status		:call CVSstatus()<cr>
nmenu &CVS.&query\ Update	:call CVSqueryupdate()<cr>
nmenu &CVS.&Update		:call CVSupdate()<cr>
nmenu &CVS.Check&out		:call CVScheckout()<cr>
nmenu &CVS.Comm&it		:call CVScommit()<cr>

"-----------------------------------------------------------------------------
" show cvs info
"-----------------------------------------------------------------------------

function! ShowCVSinfo()
  new
  " show CVS info from directory
  let root="none"
  let repository="none"
  if filereadable("CVS/Root")
    let root=system("cat CVS/Root")
  endif
  if filereadable("CVS/Repository")
    let repository=system("cat CVS/Repository")
  endif
  " show settings
  call append("$","\"Current directory : ".expand("%:p:h"))
  call append("$","\"Current Root : ".root)
  call append("$","\"Current Repository : ".repository)
  call append("$","")
  call append("$","\"\t\t\t set environment var to cvsroot")
  call append("$","let $CVSROOT=\"".$CVSROOT."\"")
  call append("$","")
  call append("$","\"\t\t\t set environment var to rsh/ssh")
  call append("$","let $CVS_RSH=\"".$CVS_RSH."\"")
  call append("$","")
  call append("$","\"\t\t\t show cvs version")
  call append("$","exec(\":!cvs -v\")")
  call append("$","")
  call append("$","\"\t\t\t edit cvs menu")
  call append("$","split $VIM/cvsmenu.vim")
  call append("$","")
  call append("$","----------------------------------------")
  call append("$","\" CVSmenu $Revision: 1.4 $")
  call append("$","\" Change above values to your needs.")
  call append("$","\" To execute a line, put the cursor on it")
  call append("$","\" and press <shift-cr> or <DoubleClick>")
  map <buffer> q :bd!<cr>
  map <buffer> <s-cr> :exec getline(".")<cr>
  map <buffer> <2-LeftMouse> :exec getline(".")<cr>
  set nomod
  set syntax=vim
endfunction
"-----------------------------------------------------------------------------
" CVS update / query update : tools
"-----------------------------------------------------------------------------

function! OpenRO()
" Used for CVSqueryupdate : Open output in new window, open file under cursor
" by <doubleclick> or <shift-cr>
  set nomod
  nmap <buffer> <2-LeftMouse> 0<2-Right>
  nmap <buffer> <S-CR> 0<2-Right>
endfunction

function! UpdateSyntax()
" Used for CVSqueryupdate : Set syntax hilighting
  syn match cvsupdateMerge	"^M .*$"
  syn match cvsupdatePatch	"^P .*$"
  syn match cvsupdateConflict	"^C .*$"
  syn match cvsupdateDelete	"^D .*$"
  syn match cvsupdateUnknown	"^? .*$"
  syn match cvscheckoutUpdate	"^U .*$"
  syn match cvsimportNew	"^N .*$"
  hi link cvsimportNew		Special
  hi link cvscheckoutUpdat	Special
  hi link cvsupdateMerge	Special
  hi link cvsupdatePatch	Constant
  hi link cvsupdateConflict	WarningMsg
  hi link cvsupdateDelete	Statement
  hi link cvsupdateUnknown	Comment
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update
"-----------------------------------------------------------------------------

function! CVSqueryupdate()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.$sep.expand("%")
  let rev=""	
"input("revision:","")
  if rev!=""
    let tmpnam=tmpnam.".".rev
    let rev="-r ".rev." "
  else
    let tmpnam=tmpnam.".current"
  endif
  exec "!cvs -z9 -n update -P ".rev.expand("%:p:t")." > ".tmpnam
  exec "sp ".tmpnam
  call OpenRO()
  call UpdateSyntax()
endfunction

"-----------------------------------------------------------------------------
" CVS diff
"-----------------------------------------------------------------------------

function! CVSdiff()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.$sep.expand("%")
  let rev=input("revision:","")
  if rev!=""
    let tmpnam=tmpnam.".".rev
    let rev="-r ".rev." "
  else
    let tmpnam=tmpnam.".current"
  endif
  wincmd _
  exec "!cvs -z9 update -p ".rev.expand("%:p:t")." > ".tmpnam
  if v:version<600
    exec "diffsplit ".tmpnam
  else
    exec "vertical diffsplit ".tmpnam
  endif
endfunction

"-----------------------------------------------------------------------------
" CVS annotate / log /status : tools
"-----------------------------------------------------------------------------

function! DoCVScommand(cmd,tmpnam)
  exec "!cvs -z9 ".a:cmd." > ".a:tmpnam
  new
  exec "find ".a:tmpnam
  set nomod
endfunction

"-----------------------------------------------------------------------------
" CVS annotate / log / status
"-----------------------------------------------------------------------------

function! CVSannotate()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.$sep.expand("%").".ann"
  call DoCVScommand("annotate ".expand("%:p:t"),tmpnam)
  wincmd _
endfunction

function! CVSlog()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.$sep.expand("%").".log"
  call DoCVScommand("log ".expand("%:p:t"),tmpnam)
endfunction

function! CVSstatus()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.$sep.expand("%").".stat"
  call DoCVScommand("status ".expand("%:p:t"),tmpnam)
endfunction

function! CVSupdate()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.$sep.expand("%").".upd"
  call DoCVScommand("update ".expand("%:p:t"),tmpnam)
  call OpenRO()
  call UpdateSyntax()
endfunction

"-----------------------------------------------------------------------------
" CVS commit
"-----------------------------------------------------------------------------

function! CVScommit()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.$sep.expand("%").".ci"
  let message=input("Message : ")
  if message==""
    return
  endif
  call DoCVScommand("commit -m \'".message."\' ".expand("%:p:t"),tmpnam)
endfunction

"-----------------------------------------------------------------------------
" CVS checkout
"-----------------------------------------------------------------------------

function! CVScheckout()
  let tmpnam=tempname()

  let destdir=expand("%:p:h")
  let destdir=input("Checkout to : ",destdir)
  if destdir==""
    return
  endif
  let module=input("Module name :")
  if module==""
    echo "Aborted"
    return
  endif
  let rev=input("revision:","")
  if rev!=""
    let rev="-r ".rev." "
  endif
  call DoCVScommand("checkout ".rev.module,tmpnam)
  call OpenRO()
  call UpdateSyntax()
endfunction

