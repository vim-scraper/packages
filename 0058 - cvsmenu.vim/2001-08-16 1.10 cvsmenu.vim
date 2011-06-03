" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" $Revision: 1.10 $
" $Date: 2001/08/16 14:41:48 $
"
" Tested with Vim 6.0
" Primary site : http://ezytools.sourceforge.net/
" Located in the "VimTools" section
"
" TODO: CVS (r)tag / branch / import
" TODO: All tagging / branching related (->update,commit,import)
" TODO: Close, if no output ??? (may cause trouble, if called by a script)
" TODO: Better support for additional params
" TODO: Errorchecking : isFile / isDirectory
" TODO: Avoid Errormessages, if pattern not found (CVSMoveToTop)
" TODO: Sort more logs (e.g. conflicts)

"unmenu CVS

if has('unix')
  let g:sep='/'
else
  let g:sep='\'
endif

if ($TEMP == '')
  let $TEMP=expand('%:p:h')
endif

if ($CVSOPT == '')
  let $CVSOPT='-z9'
endif

if ($CVSCMD == '')
  let $CVSCMD='cvs'
endif

"-----------------------------------------------------------------------------
" Menu entries
"-----------------------------------------------------------------------------

nmenu &CVS.in&fo        		:call CVSShowInfo()<cr>
nmenu &CVS.Ad&min.log&in		:call CVSlogin()<cr>
nmenu &CVS.Ad&min.log&out		:call CVSlogout()<cr>
nmenu &CVS.Comple&x.&Short\ status	:call CVSshortstatus()<cr>
nmenu &CVS.D&elete.Re&move		:call CVSremove()<cr>
nmenu &CVS.D&elete.Re&lease		:call CVSrelease()<cr>
nmenu &CVS.-SEP1-			:
nmenu &CVS.&Diff			:call CVSdiff()<cr>
nmenu &CVS.A&nnotate			:call CVSannotate()<cr>
nmenu &CVS.&History			:call CVShistory()<cr>
nmenu &CVS.&Log				:call CVSlog()<cr>
nmenu &CVS.&Status			:call CVSstatus()<cr>
nmenu &CVS.-SEP2-			:
nmenu &CVS.Check&out			:call CVScheckout()<cr>
nmenu &CVS.&query\ Update		:call CVSqueryupdate()<cr>
nmenu &CVS.&Update			:call CVSupdate()<cr>
nmenu &CVS.-SEP3-			:
nmenu &CVS.Comm&it			:call CVScommit()<cr>
nmenu &CVS.&Add				:call CVSadd()<cr>

"-----------------------------------------------------------------------------
" show cvs info
"-----------------------------------------------------------------------------

function! CVSShowInfo()
  " show CVS info from directory
  let cvsroot='CVS'.g:sep.'Root'
  let cvsrepository='CVS'.g:sep.'Repository'
  if has('unix')
    let catcmd='cat '
  else
    let catcmd='type '
  endif
  if filereadable(cvsroot)
    let root=system(catcmd.cvsroot)
  else
    let root='none'
  endif
  if filereadable(cvsrepository)
    let repository=system(catcmd.cvsrepository)
  else
    let repository='none'
  endif
  unlet catcmd cvsroot cvsrepository
  " show settings
  new
  call append('$',"\"Current directory : ".expand('%:p:h'))
  call append('$',"\"Current Root : ".root)
  call append('$',"\"Current Repository : ".repository)
  call append('$',"")
  call append('$',"\"\t\t\t set environment var to cvsroot")
  call append('$',"let $CVSROOT=\'".$CVSROOT."\'")
  call append('$',"")
  call append('$',"\"\t\t\t set environment var to rsh/ssh")
  call append('$',"let $CVS_RSH=\'".$CVS_RSH."\'")
  call append('$',"")
  call append('$',"\"\t\t\t set cvs options (see cvs --help-options)")
  call append('$',"let $CVSOPT=\'".$CVSOPT."\'")
  call append('$',"")
  call append('$',"\"\t\t\t set cvs command options")
  call append('$',"let $CVSCMDOPT=\'".$CVSCMDOPT."\'")
  call append('$',"")
  call append('$',"\"\t\t\t set cvs command")
  call append('$',"let $CVSCMD=\'".$CVSCMD."\'")
  call append('$',"")
  call append('$',"\"\t\t\t show cvs version")
  call append('$',"exec(\':!".$CVSCMD." -v\')")
  call append('$',"")
  call append('$',"\"\t\t\t edit cvs menu")
  call append('$',"split $VIM".g:sep."cvsmenu.vim")
  call append('$',"")
  call append('$',"\"----------------------------------------")
  call append('$',"\" CVSmenu $Revision: 1.10 $")
  call append('$',"\" Change above values to your needs.")
  call append('$',"\" To execute a line, put the cursor on it")
  call append('$',"\" and press <shift-cr> or <DoubleClick>")
  unlet root repository
  map <buffer> q :bd!<cr>
  map <buffer> <s-cr> :exec getline('.')<cr>:set nomod<cr>:echo getline('.')<cr>
  map <buffer> <2-LeftMouse> <s-cr>
  set nomod
  set syntax=vim
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update : tools
"-----------------------------------------------------------------------------

function! OpenRO()
" output window: open file under cursor by <doubleclick> or <shift-cr>
  set nomod
  nmap <buffer> <2-LeftMouse> 0<C-Right>
  nmap <buffer> <S-CR> 0<C-Right>
  nmap <buffer> q :bd!<cr>
endfunction

function! UpdateSyntax()
" Used for CVSqueryupdate : Set syntax hilighting
  syn match cvsupdateMerge	'^M .*$'
  syn match cvsupdatePatch	'^P .*$'
  syn match cvsupdateConflict	'^C .*$'
  syn match cvsupdateDelete	'^D .*$'
  syn match cvsupdateUnknown	'^? .*$'
  syn match cvscheckoutUpdate	'^U .*$'
  syn match cvsimportNew	'^N .*$'
  hi link cvsimportNew		Special
  hi link cvscheckoutUpdate	Special
  hi link cvsupdateMerge	Special
  hi link cvsupdatePatch	Constant
  hi link cvsupdateConflict	WarningMsg
  hi link cvsupdateDelete	Statement
  hi link cvsupdateUnknown	Comment
endfunction

"-----------------------------------------------------------------------------
" CVS call
"-----------------------------------------------------------------------------

function! CVSDoCommand(cmd,tmpext)
  exec "cd ".expand("%:p:h")
  let tmpnam=$TEMP.g:sep.expand('%').a:tmpext
  exec '!'.$CVSCMD.' '.$CVSOPT.' '.a:cmd.' '.$CVSCMDOPT.'> '.tmpnam
  new
  exec 'find '.tmpnam
  call OpenRO()
  if delete (tmpnam)==1
    echo 'CVS: could not delete temp:'.tmpnam
  endif
  unlet tmpnam
endfunction

"#############################################################################
" following commands read from STDIN. Call CVS directly
"#############################################################################
"-----------------------------------------------------------------------------
" CVS login / logout (password prompt)
"-----------------------------------------------------------------------------

function! CVSlogin()
  exec '!'.$CVSCMD.' '.$CVSOPT.' login '.$CVSCMDOPT
endfunction

function! CVSlogout()
  exec '!'.$CVSCMD.' '.$CVSOPT.' logout '.$CVSCMDOPT
endfunction

"-----------------------------------------------------------------------------
" CVS release (confirmation prompt)
"-----------------------------------------------------------------------------

function! CVSrelease()
  let localtoo=input('Release:Also delete local file [y]:')
  if (localtoo=='y') || (localtoo=='')
    let localtoo='-d '
  else
    let localtoo=''
  endif
  let releasedir=expand("%:p:h")
  exec ":cd .."
" confirmation prompt -> dont use CVSDoCommand
  exec '!'.$CVSCMD.' '.$CVSOPT.' release '.localtoo.releasedir.' '.$CVSCMDOPT
  unlet localtoo releasedir
endfunction

"-----------------------------------------------------------------------------
" CVS diff (output needed)
"-----------------------------------------------------------------------------

function! CVSdiff()
  exec "cd ".expand("%:p:h")
  let tmpnam=$TEMP.g:sep.expand('%')
  let rev=input('Revision (optional):','')
  if rev!=''
    let tmpext='.'.rev
    let rev='-r '.rev.' '
  else
    let tmpext='.current'
  endif
  wincmd _
" We need CVS output. -> dont use CVSDoCommand
  exec '!'.$CVSCMD.' '.$CVSOPT.' update -p '.rev.expand('%:p:t').' '.$CVSCMDOPT.'> '.tmpnam.tmpext
  if v:version<600
    exec 'diffsplit '.tmpnam.tmpext
  else
    exec 'vertical diffsplit '.tmpnam.tmpext
  endif
  unlet tmpnam tmpext rev
endfunction

"#############################################################################
" from here : use CVSDoCommand wrapper
"#############################################################################
"-----------------------------------------------------------------------------
" CVS annotate / log / status / history
"-----------------------------------------------------------------------------

function! CVSannotate()
  call CVSDoCommand('annotate '.expand('%:p:t'),'.ann')
  wincmd _
endfunction

function! CVSlog()
  call CVSDoCommand('log '.expand('%:p:t'),'.log')
endfunction

function! CVSstatus()
  call CVSDoCommand('status '.expand('%:p:t'),'.stat')
endfunction

function! CVShistory()
  call CVSDoCommand('history '.expand('%:p:t'),'.stat')
endfunction

function! CVSupdate()
  call CVSDoCommand('update '.expand('%:p:t'),'.upd')
  call UpdateSyntax()
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update
"-----------------------------------------------------------------------------

function! CVSqueryupdate()
  let rev=''	
"input('revision:','')
  if rev!=''
"    let tmpext=tmpext.'.'.rev
    let rev='-r '.rev.' '
  else
"    let tmpext=tmpext.'.current'
  endif
  call CVSDoCommand('-n update -P '.rev.expand('%:p:t'),'.upd')
  call UpdateSyntax()
  unlet rev
endfunction

"-----------------------------------------------------------------------------
" CVS remove (request confirmation)
"-----------------------------------------------------------------------------

function! CVSremove()
  let localtoo=input('Remove:Also delete local file [y]:')
  if (localtoo=='y') || (localtoo=='')
    let localtoo='-f '
  else
    let localtoo=''
  endif
  let confrm=input('Remove:confirm with "y":')
  if confrm!='y'
    echo 'CVS remove: aborted'
    return
  endif
  call CVSDoCommand('remove '.localtoo.expand('%:p:t'),'.rem')
endfunction

"-----------------------------------------------------------------------------
" CVS add
"-----------------------------------------------------------------------------

function! CVSadd()
  let message=escape(input('Message:'),'"<>|&')
  if message==""
    echo 'CVS add: aborted'
    return
  endif
  call CVSDoCommand('add -m "'.message.'" '.expand('%:p:t'),'.add')
  " Is there any output to stdout ? I only receive stderr... we may close
  if !&modified
    bdelete
  endif
  unlet message
endfunction

"-----------------------------------------------------------------------------
" CVS commit
"-----------------------------------------------------------------------------

function! CVScommit()
  let message=escape(input('Message:'),'"<>|&')
  if message==""
    echo 'CVS commit: aborted'
    return
  endif
  let rev=input('Revision (optional):','')
  if rev!=''
    let rev='-r '.rev.' '
  endif
  call CVSDoCommand('commit -m "'.message.'" '.rev.expand("%:p:t"),'.ci')
  unlet message rev
endfunction

"-----------------------------------------------------------------------------
" CVS checkout
"-----------------------------------------------------------------------------

function! CVScheckout()
  let destdir=expand('%:p:h')
  let destdir=input('Checkout to:',destdir)
  if destdir==''
    return
  endif
  let module=input('Module name:')
  if module==''
    echo 'CVS checkout: aborted'
    return
  endif
  let rev=input('Revision (optional):','')
  if rev!=''
    let rev='-r '.rev.' '
  endif
  call CVSDoCommand('checkout '.rev.module,'.co')
  call UpdateSyntax()
  unlet destdir module rev
endfunction

"-----------------------------------------------------------------------------
" some tools
"-----------------------------------------------------------------------------

function! CVSCompressStatus()
  let curline=1
  while curline < line('$')
    let str=strpart(getline(curline),0,5)
    " Leave only lines starting with 'File:'
    if (str!='File:')
      " move to that line
      exec ':'.curline
      " delete it
      exec ':d'
    else
      " next line
      let curline = curline + 1
    endif
  endwhile
endfunction

function! CVSMoveToTop(searchstr)
  let @z=""
  normal gg
  let v:warningmsg=''
  while (v:warningmsg=='')
    exec "/".a:searchstr
    if v:warningmsg==''
      normal "Zddk
    endif
  endwhile
  normal gg"ZP
endfunction

"-----------------------------------------------------------------------------
" compound/complex commands
"-----------------------------------------------------------------------------

function! CVSshortstatus()
  call CVSstatus()
  call CVSCompressStatus()
  call CVSMoveToTop('Status: Unknown$')
  call CVSMoveToTop('Status: Needs Checkout$')
  call CVSMoveToTop('Status: Needs Merge$')
  call CVSMoveToTop('Status: Needs Patch$')
  call CVSMoveToTop('Status: Locally Removed$')
  call CVSMoveToTop('Status: Locally Added$')
  call CVSMoveToTop('Status: Locally Modified$')
  call CVSMoveToTop('Status: File had conflicts on merge$')
  call OpenRO()
endfunction

