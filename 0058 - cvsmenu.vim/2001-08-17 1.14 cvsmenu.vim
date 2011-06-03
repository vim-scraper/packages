" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" $Revision: 1.14 $
" $Date: 2001/08/18 02:31:57 $
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

let g:CVSforcedirectory = 0		" 0:off 1:once 2:forever

"-----------------------------------------------------------------------------
" Menu entries
"-----------------------------------------------------------------------------
" <esc> in Keyword menus to avoid expansion

amenu &CVS.in&fo  	      			:call CVSShowInfo()<cr>
amenu &CVS.Director&y.&Toggle\ buf/dir		:call CVSToggleForceDir()<cr>
amenu &CVS.Director&y.-SEP1-			:
amenu &CVS.Director&y.&Add			:call CVSSetForceDir(1)<cr>:call CVSadd()<cr>
amenu &CVS.Director&y.Comm&it			:call CVSSetForceDir(1)<cr>:call CVScommit()<cr>
amenu &CVS.Director&y.S&hort\ Status		:call CVSSetForceDir(1)<cr>:call CVSshortstatus()<cr>
amenu &CVS.Director&y.&Status			:call CVSSetForceDir(1)<cr>:call CVSstatus()<cr>
amenu &CVS.Director&y.&Log			:call CVSSetForceDir(1)<cr>:call CVSlog()<cr>
amenu &CVS.Director&y.&Query\ Update		:call CVSSetForceDir(1)<cr>:call CVSqueryupdate()<cr>
amenu &CVS.Director&y.&Update			:call CVSSetForceDir(1)<cr>:call CVSupdate()<cr>
amenu &CVS.Director&y.Re&move\ from\ rep.	:call CVSSetForceDir(1)<cr>:call CVSremove()<cr>
amenu &CVS.&Keyword.&Author  			a$Author<esc>a$<esc>
amenu &CVS.&Keyword.&Date    			a$Date<esc>a$<esc>
amenu &CVS.&Keyword.&Header  			a$Header<esc>a$<esc>
amenu &CVS.&Keyword.&Id      			a$Id<esc>a$<esc>
amenu &CVS.&Keyword.&Name    			a$Name<esc>a$<esc>
amenu &CVS.&Keyword.Loc&ker  			a$Locker<esc>a$<esc>
amenu &CVS.&Keyword.&Log     			a$Log<esc>a$<esc>
amenu &CVS.&Keyword.RCS&file 			a$RCSfile<esc>a$<esc>
amenu &CVS.&Keyword.&Re&vision			a$Revision<esc>a$<esc>
amenu &CVS.&Keyword.&Source  			a$Source<esc>a$<esc>
amenu &CVS.&Keyword.S&tate   			a$State<esc>a$<esc>
amenu &CVS.-SEP1-				:
amenu &CVS.Ad&min.log&in			:call CVSlogin()<cr>
amenu &CVS.Ad&min.log&out			:call CVSlogout()<cr>
amenu &CVS.Comple&x.S&hort\ Status		:call CVSshortstatus()<cr>
amenu &CVS.D&elete.Re&move\ from\ rep.		:call CVSremove()<cr>
amenu &CVS.D&elete.Re&lease\ workdir		:call CVSrelease()<cr>
amenu &CVS.-SEP2-				:
amenu &CVS.&Diff				:call CVSdiff()<cr>
amenu &CVS.A&nnotate				:call CVSannotate()<cr>
amenu &CVS.&History				:call CVShistory()<cr>
amenu &CVS.&Log					:call CVSlog()<cr>
amenu &CVS.&Status				:call CVSstatus()<cr>
amenu &CVS.-SEP3-				:
amenu &CVS.Check&out				:call CVScheckout()<cr>
amenu &CVS.&Query\ Update			:call CVSqueryupdate()<cr>
amenu &CVS.&Update				:call CVSupdate()<cr>
amenu &CVS.-SEP4-				:
amenu &CVS.Comm&it				:call CVScommit()<cr>
amenu &CVS.&Add					:call CVSadd()<cr>


"-----------------------------------------------------------------------------
" show cvs info
"-----------------------------------------------------------------------------

function! CVSShowInfo()
  exec "cd ".expand("%:p:h")
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
  call append('$',"\"\t\t\t run CVS on buffer or directory (0:off 1:once 2:forever")
  call append('$',"let g:CVSforcedirectory = ".g:CVSforcedirectory)
  call append('$',"")
  call append('$',"\"\t\t\t show cvs version")
  call append('$',"exec(\':!".$CVSCMD." -v\')")
  call append('$',"")
  call append('$',"\"----------------------------------------")
  call append('$',"\" Change above values to your needs.")
  call append('$',"\" To execute a line, put the cursor on it")
  call append('$',"\" and press <shift-cr> or <DoubleClick>")
  call append('$',"\" CVSmenu $Revision: 1.14 $")
  call append('$',"\" Site: http://ezytools.sf.net/VimTools")
  unlet root repository
  map <buffer> q :bd!<cr>
  map <buffer> <s-cr> :exec getline('.')<cr>:set nomod<cr>:echo getline('.')<cr>
  map <buffer> <2-LeftMouse> <s-cr>
  set nomodified
  set syntax=vim
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update : tools
"-----------------------------------------------------------------------------

function! CVSOpenRO()
" output window: open file under cursor by <doubleclick> or <shift-cr>
  set nomodified
  setlocal nomodifiable
  nmap <buffer> <2-LeftMouse> 0<C-Right>
  nmap <buffer> <S-CR> 0<C-Right>
  nmap <buffer> q :bd!<cr>
endfunction

function! CVSUpdateSyntax()
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

  syn match cvsstatusUpToDate	"^File:\s.*\sStatus: Up-to-date$"
  syn match cvsstatusLocal	"^File:\s.*\sStatus: Locally.*$"
  syn match cvsstatusNeed	"^File:\s.*\sStatus: Need.*$"
  syn match cvsstatusConflict	"^File:\s.*\sStatus: File had conflict.*$"
  syn match cvsstatusUnknown	"^File:\s.*\sStatus: Unknown$"
  hi link cvsstatusUpToDate	Type
  hi link cvsstatusLocal	Constant
  hi link cvsstatusNeed    	Identifier
  hi link cvsstatusConflict    	Warningmsg
  hi link cvsstatusUnknown    	Comment

  if !filereadable($VIM.g:sep."syntax".g:sep."rcslog")
    syn match cvslogRevision	"^revision.*$"
    syn match cvslogFile	"^RCS file:.*"
    syn match cvslogDate	"^date: .*$"
    hi link cvslogFile		Type
    hi link cvslogRevision	Constant
    hi link cvslogDate		Identifier
  endif
endfunction

"-----------------------------------------------------------------------------
" CVS call
"-----------------------------------------------------------------------------

function! CVSDoCommand(cmd,tmpext)
  " Warn, if working on DIR
  exec "cd ".expand("%:p:h")
  let tmpnam=$TEMP.g:sep.expand('%').a:tmpext
  exec '!'.$CVSCMD.' '.$CVSOPT.' '.a:cmd.' '.$CVSCMDOPT.'> '.tmpnam
  new
  exec 'read '.tmpnam
  call CVSOpenRO()
  if delete (tmpnam)==1
    echo 'CVS: could not delete temp:'.tmpnam
  endif
  call CVSUpdateSyntax()
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
  if g:CVSforcedirectory>0
    call CVSDoCommand('log ','.log')
    if g:CVSforcedirectory==1
      let g:CVSforcedirectory = 0
    endif
  else
    call CVSDoCommand('log '.expand('%:p:t'),'.log')
  endif
endfunction

function! CVSstatus()
  if g:CVSforcedirectory>0
    call CVSDoCommand('status ','.stat')
    if g:CVSforcedirectory==1
      let g:CVSforcedirectory = 0
    endif
  else
    call CVSDoCommand('status '.expand('%:p:t'),'.stat')
  endif
endfunction

function! CVShistory()
  call CVSDoCommand('history '.expand('%:p:t'),'.stat')
endfunction

function! CVSupdate()
  if g:CVSforcedirectory>0
    call CVSDoCommand('update ','.upd')
    if g:CVSforcedirectory==1
      let g:CVSforcedirectory = 0
    endif
  else
    call CVSDoCommand('update '.expand('%:p:t'),'.upd')
  endif
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update
"-----------------------------------------------------------------------------

function! CVSqueryupdate()
  if g:CVSforcedirectory>0
    call CVSDoCommand('-n update -P ','.upd')
    if g:CVSforcedirectory==1
      let g:CVSforcedirectory = 0
    endif
  else
    call CVSDoCommand('-n update -P '.expand('%:p:t'),'.upd')
  endif
endfunction

"-----------------------------------------------------------------------------
" CVS remove (request confirmation)
"-----------------------------------------------------------------------------

function! CVSremove()
  if g:CVSforcedirectory>0
    let localtoo=input('Remove:Also delete local DIRECTORY [y]:')
  else
    let localtoo=input('Remove:Also delete local file [y]:')
  endif
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
  if g:CVSforcedirectory>0
    call CVSDoCommand('remove '.localtoo,'.rem')
    if g:CVSforcedirectory==1
      let g:CVSforcedirectory = 0
    endif
  else
    call CVSDoCommand('remove '.localtoo.expand('%:p:t'),'.rem')
  endif
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
  if g:CVSforcedirectory>0
    call CVSDoCommand('add -m "'.message.'"','.add')
    if g:CVSforcedirectory>0
      let g:CVSforcedirectory = 0
    endif
  else
    call CVSDoCommand('add -m "'.message.'" '.expand('%:p:t'),'.add')
  endif
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
  if g:CVSforcedirectory>0
    call CVSDoCommand('commit -m "'.message.'" '.rev,'.ci')
    if g:CVSforcedirectory==1
      let g:CVSforcedirectory = 0
    endif
  else
    call CVSDoCommand('commit -m "'.message.'" '.rev.expand("%:p:t"),'.ci')
  endif
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
  let v:errmsg=''
  while (v:errmsg=='')
    silent! exec "/".a:searchstr
    if v:errmsg==''
      normal "Zddk
    endif
  endwhile
  normal gg"ZP
endfunction

"-----------------------------------------------------------------------------
" compound/complex commands
"-----------------------------------------------------------------------------

function! CVSshortstatus()
  " prevent flag from being reset
  if g:CVSforcedirectory==1
    let savedironce=1
    let g:CVSforcedirectory=2
  else
    let savedironce=0
  endif
  call CVSstatus()
  " allow throwing some line around
  set modifiable
  call CVSCompressStatus()
  call CVSMoveToTop('Status: Unknown$')
  call CVSMoveToTop('Status: Needs Checkout$')
  call CVSMoveToTop('Status: Needs Merge$')
  call CVSMoveToTop('Status: Needs Patch$')
  call CVSMoveToTop('Status: Locally Removed$')
  call CVSMoveToTop('Status: Locally Added$')
  call CVSMoveToTop('Status: Locally Modified$')
  call CVSMoveToTop('Status: File had conflicts on merge$')
  " insert last blank line (moving with '{}')
  let v:errmsg=""
  silent! exec "/Status: Up-to-date$"
  if v:errmsg==""
    normal O
  endif
  normal gg
  call CVSOpenRO()
  if savedironce==1
    let g:CVSforcedirectory=1
  endif
  unlet savedironce
endfunction

function! CVSSetForceDir(value)
  let g:CVSforcedirectory=a:value
  if g:CVSforcedirectory==1
    echo 'CVS:Using current DIRECTORY once'
  elseif g:CVSforcedirectory==2
    echo 'CVS:Using current DIRECTORY'
  else
    echo 'CVS:Using current buffer'
  endif
endfunction

function! CVSToggleForceDir()
  if g:CVSforcedirectory > 0
    call CVSSetForceDir(0)
  else
    call CVSSetForceDir(2)
  endif
endfunction
