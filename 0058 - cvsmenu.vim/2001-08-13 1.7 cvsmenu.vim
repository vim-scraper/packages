" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" $Revision: 1.3.2.3 $
" $Date: 2001/08/13 20:38:17 $
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
  let g:sep='/'
else
  let g:sep='\'
endif

if ($temp == "")
  let $temp=expand("%:p:h")
endif

if ($CVSOPT == "")
  let $CVSOPT="-z9"
endif
"-----------------------------------------------------------------------------
" Menu entries
"-----------------------------------------------------------------------------

nmenu &CVS.in&fo        	:call ShowCVSinfo()<cr>
nmenu &CVS.Ad&min.log&in	:call CVSlogin()<cr>
nmenu &CVS.Ad&min.log&out	:call CVSlogout()<cr>
nmenu &CVS.-SEP1-		:
nmenu &CVS.&Diff		:call CVSdiff()<cr>
nmenu &CVS.A&nnotate		:call CVSannotate()<cr>
nmenu &CVS.&Log			:call CVSlog()<cr>
nmenu &CVS.&Status		:call CVSstatus()<cr>
nmenu &CVS.-SEP2-		:
nmenu &CVS.Check&out		:call CVScheckout()<cr>
nmenu &CVS.&query\ Update	:call CVSqueryupdate()<cr>
nmenu &CVS.&Update		:call CVSupdate()<cr>
nmenu &CVS.-SEP3-		:
nmenu &CVS.Comm&it		:call CVScommit()<cr>
nmenu &CVS.&Add			:call CVSadd()<cr>

"-----------------------------------------------------------------------------
" show cvs info
"-----------------------------------------------------------------------------

function! ShowCVSinfo()
  " show CVS info from directory
  let cvsroot="CVS".g:sep."Root"
  let cvsrepository="CVS".g:sep."Repository"
  if has("unix")
    let catcmd="cat "
  else
    let catcmd="type "
  endif
  if filereadable(cvsroot)
    let root=system(catcmd.cvsroot)
  else
    let root="none"
  endif
  if filereadable("CVS/Repository")
    let repository=system(catcmd.cvsrepository)
  else
    let repository="none"
  endif
  unlet catcmd cvsroot cvsrepository
  " show settings
  new
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
  call append("$","\"\t\t\t set cvs options (see cvs --help-options)")
  call append("$","let $CVSOPT=\"".$CVSOPT."\"")
  call append("$","")
  call append("$","\"\t\t\t set cvs command options")
  call append("$","let $CVSCMDOPT=\"".$CVSCMDOPT."\"")
  call append("$","")
  call append("$","\"\t\t\t show cvs version")
  call append("$","exec(\":!cvs -v\")")
  call append("$","")
  call append("$","\"\t\t\t edit cvs menu")
  call append("$","split $VIM/cvsmenu.vim")
  call append("$","")
  call append("$","----------------------------------------")
  call append("$","\" CVSmenu $Revision: 1.3.2.3 $")
  call append("$","\" Change above values to your needs.")
  call append("$","\" To execute a line, put the cursor on it")
  call append("$","\" and press <shift-cr> or <DoubleClick>")
  unlet root repository
  map <buffer> q :bd!<cr>
  map <buffer> <s-cr> :exec getline(".")<cr>:set nomod<cr>:echo getline(".")<cr>
  map <buffer> <2-LeftMouse> <s-cr>
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
  let tmpnam=$temp.g:sep.expand("%")
  let rev=""	
"input("revision:","")
  if rev!=""
    let tmpnam=tmpnam.".".rev
    let rev="-r ".rev." "
  else
    let tmpnam=tmpnam.".current"
  endif
  exec "!cvs ".$CVSOPT." -n update -P ".rev.expand("%:p:t")." ".$CVSCMDOPT."> ".tmpnam
  exec "sp ".tmpnam
  call OpenRO()
  call UpdateSyntax()
  unlet tmpnam rev
endfunction

"-----------------------------------------------------------------------------
" CVS diff
"-----------------------------------------------------------------------------

function! CVSdiff()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.g:sep.expand("%")
  let rev=input("revision:","")
  if rev!=""
    let tmpnam=tmpnam.".".rev
    let rev="-r ".rev." "
  else
    let tmpnam=tmpnam.".current"
  endif
  wincmd _
  exec "!cvs ".$CVSOPT." update -p ".rev.expand("%:p:t")." ".$CVSOPT."> ".tmpnam
  if v:version<600
    exec "diffsplit ".tmpnam
  else
    exec "vertical diffsplit ".tmpnam
  endif
  unlet tmpnam rev
endfunction

"-----------------------------------------------------------------------------
" CVS annotate / log /status : tools
"-----------------------------------------------------------------------------

function! DoCVScommand(cmd,tmpnam)
  exec "!cvs ".$CVSOPT." ".a:cmd." ".$CVSCMDOPT."> ".a:tmpnam
  new
  exec "find ".a:tmpnam
  set nomod
endfunction

"-----------------------------------------------------------------------------
" CVS login / logout
"-----------------------------------------------------------------------------

function! CVSlogin()
  exec "!cvs ".$CVSOPT." login ".$CVSCMDOPT
endfunction

function! CVSlogout()
  exec "!cvs ".$CVSOPT." logout ".$CVSCMDOPT
endfunction

"-----------------------------------------------------------------------------
" CVS annotate / log / status
"-----------------------------------------------------------------------------

function! CVSannotate()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.g:sep.expand("%").".ann"
  call DoCVScommand("annotate ".expand("%:p:t"),tmpnam)
  wincmd _
  unlet tmpnam
endfunction

function! CVSlog()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.g:sep.expand("%").".log"
  call DoCVScommand("log ".expand("%:p:t"),tmpnam)
  unlet tmpnam
endfunction

function! CVSstatus()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.g:sep.expand("%").".stat"
  call DoCVScommand("status ".expand("%:p:t"),tmpnam)
  unlet tmpnam
endfunction

function! CVSupdate()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.g:sep.expand("%").".upd"
  call DoCVScommand("update ".expand("%:p:t"),tmpnam)
  call OpenRO()
  call UpdateSyntax()
  unlet tmpnam
endfunction

"-----------------------------------------------------------------------------
" CVS add
"-----------------------------------------------------------------------------

function! CVSadd()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.g:sep.expand("%").".add"
  let message=input("Message : ")
  if message==""
    return
  endif
  call DoCVScommand("add -m \'".message."\' ".expand("%:p:t"),tmpnam)
  " Is there any output to stdout ? I only receive stderr... we may close
  if !&modified
    bdelete
  endif
  unlet tmpnam message
endfunction

"-----------------------------------------------------------------------------
" CVS commit
"-----------------------------------------------------------------------------

function! CVScommit()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp.g:sep.expand("%").".ci"
  let message=input("Message : ")
  if message==""
    return
  endif
  call DoCVScommand("commit -m \'".message."\' ".expand("%:p:t"),tmpnam)
  unlet tmpnam message
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
  unlet destdir module rev
endfunction

