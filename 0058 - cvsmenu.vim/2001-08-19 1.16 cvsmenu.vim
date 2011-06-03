" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" $Revision: 1.16 $
" $Date: 2001/08/19 19:19:34 $
"
" Tested with Vim 6.0
" Primary site : http://ezytools.sourceforge.net/
" Located in the "VimTools" section
"
" TODO: Close, if no output ??? (may cause trouble, if called by a script)
" TODO: Better support for additional params
" TODO: Errorchecking : isFile / isDirectory
" TODO: Sort more logs (e.g. conflicts)

"aunmenu CVS

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

if !exists("g:CVSforcedirectory")
  let g:CVSforcedirectory = 0		" 0:off 1:once 2:forever
endif
if !exists("g:CVSupdatequeryonly")
  let g:CVSupdatequeryonly = 0		" 0:really update 1:be a simulant
endif
if !exists("g:CVSqueryrevision")
  let g:CVSqueryrevision = 0		" 0:fast update 1:query for revisions
endif

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
amenu &CVS.D&elete.Re&move\ from\ rep.		:call CVSremove()<cr>
amenu &CVS.D&elete.Re&lease\ workdir		:call CVSrelease()<cr>
amenu &CVS.&Tag.Toggle\ revision\ &queries	:call CVSToggleQueryRevision()<cr>
amenu &CVS.&Tag.-SEP1-				:
amenu &CVS.&Tag.&Create\ Tag			:call CVStag()<cr>
amenu &CVS.&Tag.&Remove\ Tag			:call CVStagremove()<cr>
amenu &CVS.&Tag.Create\ &Branch			:call CVSbranch()<cr>
amenu &CVS.&Tag.-SEP2-				:
amenu &CVS.&Tag.Cre&ate\ Tag\ by\ module	:call CVSrtag()<cr>
amenu &CVS.&Tag.Rem&ove\ Tag\ by\ module	:call CVSrtagremove()<cr>
amenu &CVS.&Tag.Create\ Branc&h\ by\ module	:call CVSrbranch()<cr>
amenu &CVS.&Watch/Edit.&Watchers		:call CVSwatchwatchers()<cr>
amenu &CVS.&Watch/Edit.Watch\ &Add		:call CVSwatchadd()<cr>
amenu &CVS.&Watch/Edit.Watch\ &Remove		:call CVSwatchremove()<cr>
amenu &CVS.&Watch/Edit.Watch\ O&n		:call CVSwatchon()<cr>
amenu &CVS.&Watch/Edit.Watch\ O&ff		:call CVSwatchoff()<cr>
amenu &CVS.&Watch/Edit.-SEP1-			:
amenu &CVS.&Watch/Edit.&Editors			:call CVSwatcheditors()<cr>
amenu &CVS.&Watch/Edit.Edi&t			:call CVSwatchedit()<cr>
amenu &CVS.&Watch/Edit.&Unedit			:call CVSwatchunedit()<cr>
amenu &CVS.-SEP2-				:
amenu &CVS.&Diff				:call CVSdiff()<cr>
amenu &CVS.A&nnotate				:call CVSannotate()<cr>
amenu &CVS.Histo&ry				:call CVShistory()<cr>
amenu &CVS.&Log					:call CVSlog()<cr>
amenu &CVS.&Status				:call CVSstatus()<cr>
amenu &CVS.S&hort\ Status			:call CVSshortstatus()<cr>
amenu &CVS.-SEP3-				:
amenu &CVS.Check&out				:call CVScheckout()<cr>
amenu &CVS.&Query\ Update			:call CVSqueryupdate()<cr>
amenu &CVS.&Update				:call CVSupdate()<cr>
amenu &CVS.Re&vert\ Changes			:call CVSrevertchanges()<cr>
amenu &CVS.-SEP4-				:
amenu &CVS.&Add					:call CVSadd()<cr>
amenu &CVS.Comm&it				:call CVScommit()<cr>
amenu &CVS.Im&port				:call CVSimport()<cr>


"-----------------------------------------------------------------------------
" show cvs info
"-----------------------------------------------------------------------------

function! CVSShowInfo()
  exec 'cd '.expand('%:p:h')
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
  call append('$',"\"\t\t\t set environment var to rsh/ssh")
  call append('$',"let $CVS_RSH=\'".$CVS_RSH."\'")
  call append('$',"\"\t\t\t set cvs options (see cvs --help-options)")
  call append('$',"let $CVSOPT=\'".$CVSOPT."\'")
  call append('$',"\"\t\t\t set cvs command options")
  call append('$',"let $CVSCMDOPT=\'".$CVSCMDOPT."\'")
  call append('$',"\"\t\t\t set cvs command")
  call append('$',"let $CVSCMD=\'".$CVSCMD."\'")
  call append('$',"\"\t\t\t run CVS on buffer or directory (0:off 1:once 2:forever")
  call append('$',"let g:CVSforcedirectory = ".g:CVSforcedirectory)
  call append('$',"\"\t\t\t Query for revisions (0:no 1:yes)")
  call append('$',"let g:CVSqueryrevision = ".g:CVSqueryrevision)
  call append('$',"\"\t\t\t show cvs version")
  call append('$',"exec(\':!".$CVSCMD." -v\')")
  call append('$',"")
  call append('$',"\"----------------------------------------")
  call append('$',"\" Change above values to your needs.")
  call append('$',"\" To execute a line, put the cursor on it")
  call append('$',"\" and press <shift-cr> or <DoubleClick>")
  call append('$',"\" CVSmenu $Revision: 1.16 $")
  call append('$',"\" Site: http://ezytools.sf.net/VimTools")
  normal dd
  map <buffer> q :bd!<cr>
  map <buffer> <s-cr> :exec getline('.')<cr>:set nomod<cr>:echo getline('.')<cr>
  map <buffer> <2-LeftMouse> <s-cr>
  set nomodified
  set syntax=vim
  unlet root repository
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update : tools
"-----------------------------------------------------------------------------

function! CVSMakeRO()
  set nomodified
  set readonly
  if v:version >= 600
    setlocal nomodifiable
  endif
endfunction

function! CVSMakeRW()
  set noreadonly
  if v:version >= 600
    setlocal modifiable
  endif
endfunction

function! CVSUpdateMapping()
" output window: open file under cursor by <doubleclick> or <shift-cr>
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
  syn match cvstagNew		'^T .*$'
  hi link cvstagNew		Special
  hi link cvsimportNew		Special
  hi link cvscheckoutUpdate	Special
  hi link cvsupdateMerge	Special
  hi link cvsupdatePatch	Constant
  hi link cvsupdateConflict	WarningMsg
  hi link cvsupdateDelete	Statement
  hi link cvsupdateUnknown	Comment

  syn match cvsstatusUpToDate	'^File:\s.*\sStatus: Up-to-date$'
  syn match cvsstatusLocal	'^File:\s.*\sStatus: Locally.*$'
  syn match cvsstatusNeed	'^File:\s.*\sStatus: Need.*$'
  syn match cvsstatusConflict	'^File:\s.*\sStatus: File had conflict.*$'
  syn match cvsstatusUnknown	'^File:\s.*\sStatus: Unknown$'
  hi link cvsstatusUpToDate	Type
  hi link cvsstatusLocal	Constant
  hi link cvsstatusNeed    	Identifier
  hi link cvsstatusConflict    	Warningmsg
  hi link cvsstatusUnknown    	Comment

  if !filereadable($VIM.g:sep.'syntax'.g:sep.'rcslog')
    syn match cvslogRevision	'^revision.*$'
    syn match cvslogFile	'^RCS file:.*'
    syn match cvslogDate	'^date: .*$'
    hi link cvslogFile		Type
    hi link cvslogRevision	Constant
    hi link cvslogDate		Identifier
  endif
endfunction

"-----------------------------------------------------------------------------
" CVS call
"-----------------------------------------------------------------------------

function! CVSDoCommand(cmd,tmpext,target)
  " change to buffers directory
  exec 'cd '.expand('%:p:h')
  " get file/directory to work on (if not given)
  if a:target == ''
    if g:CVSforcedirectory>0
      let filename=''
    else
      let filename=expand('%:p:t')
    endif
  else
    let filename = a:target
  endif
"  let tmpnam=$TEMP.g:sep.expand('%').a:tmpext
  let tmpnam=$TEMP.g:sep.filename.a:tmpext
  exec '!'.$CVSCMD.' '.$CVSOPT.' '.a:cmd.' '.$CVSCMDOPT.filename'> '.tmpnam
  new
  exec 'read '.tmpnam
  if delete (tmpnam)==1
    echo 'CVS: could not delete temp:'.tmpnam
  endif
  " reset single shot flag
  if g:CVSforcedirectory==1
    let g:CVSforcedirectory = 0
  endif
  call CVSMakeRO()
  call CVSUpdateMapping()
  call CVSUpdateSyntax()
  unlet tmpnam filename
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
  let releasedir=expand('%:p:h')
  exec ':cd ..'
" confirmation prompt -> dont use CVSDoCommand
  exec '!'.$CVSCMD.' '.$CVSOPT.' release '.localtoo.releasedir.' '.$CVSCMDOPT
  unlet localtoo releasedir
endfunction

"-----------------------------------------------------------------------------
" CVS diff (output needed)
"-----------------------------------------------------------------------------

function! CVSdiff()
  exec 'cd '.expand('%:p:h')
  let tmpnam=$TEMP.g:sep.expand('%')
  " query revision (if wanted)
  if g:CVSqueryrevision > 0
    let rev=input('Revision (optional):','')
  else 
    let rev=''
  endif
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
  call CVSDoCommand('annotate ',expand('%:p:t'),'.ann','')
  wincmd _
endfunction

function! CVSlog()
  call CVSDoCommand('log ','.log','')
endfunction

function! CVSstatus()
  call CVSDoCommand('status ','.stat','')
endfunction

function! CVShistory()
  call CVSDoCommand('history ','.hist','')
endfunction


"-----------------------------------------------------------------------------
" CVS watch / edit : common
"-----------------------------------------------------------------------------

function! CVSQueryAction()
  let action=input('Action (e)dit, (u)nedit, (c)ommit, (a)ll, [n]one:')
  if action == 'e'
    let action = '-a edit '
  elseif action == 'u'
    let action = '-a unedit '
  elseif action == 'a'
    let action = '-a all '
  else
    let action = ''
  endif
  return action
endfunction

"-----------------------------------------------------------------------------
" CVS edit
"-----------------------------------------------------------------------------

function! CVSwatcheditors()
  call CVSDoCommand('editors ','.ed','')
endfunction

function! CVSwatchedit()
  let action=CVSQueryAction()
  call CVSDoCommand('edit '.action,'.ed','')
  unlet action
endfunction

function! CVSwatchunedit()
  call CVSDoCommand('unedit ','.ed','')
endfunction

"-----------------------------------------------------------------------------
" CVS watch
"-----------------------------------------------------------------------------

function! CVSwatchwatchers()
  call CVSDoCommand('watchers ','.watch','')
endfunction

function! CVSwatchadd()
  let action=CVSQueryAction()
  call CVSDoCommand('watch add '.action,'.watch','')
  unlet action
endfunction

function! CVSwatchremove()
  call CVSDoCommand('watch remove ','.watch','')
endfunction

function! CVSwatchon()
  call CVSDoCommand('watch on ','.watch','')
endfunction

function! CVSwatchoff()
  call CVSDoCommand('watch off ','.watch','')
endfunction
                       
"-----------------------------------------------------------------------------
" CVS tag
"-----------------------------------------------------------------------------

function! CVSDoTag(usertag,tagopt)
  " force tagname input
  let tagname=escape(input('tagname:'),'"<>|&')
  if tagname==''
    echo 'CVS tag: aborted'
    return
  endif
  " if rtag, force module instead local copy
  if a:usertag > 0
    let tagcmd = 'rtag '
    let module = input('Enter module name:')
    if module == ''
      echo 'rtag: aborted'
      return
    endif
    let target = module
    unlet module
  else
    let tagcmd = 'tag '
    let target = ''
  endif
  " g:CVSqueryrevision ?
  " tag by date, revision or local
  let tagby=input('Tag by (d)ate, (r)evision (default:none):')
  if (tagby == 'd')
    let tagby='-D '
    let tagwhat=input('Enter date:')
  elseif (tagby == 'r')
    let tagby='-r '
    let tagwhat=input('Enter revision:')
  else 
    let tagby = ''
  endif
  " input date / revision
  if tagby != ''
    if tagwhat == ''
      echo 'CVS tag: aborted'
      return
    else
      let tagwhat = tagby.tagwhat.' '
    endif
  else
    let tagwhat = ''
  endif
  " check if working file is unchanged (if not rtag)
  if a:usertag == 0
    let checksync=input('Override sync check [n]:')
    if (checksync == 'n') || (checksync == '')
      let checksync='-c '
    else
      let checksync=''
    endif
  else
    let checksync=''
  endif
"  call CVSDoCommand('tag '.checksync.tagwhat,'.tag')
  call CVSDoCommand(tagcmd.' '.a:tagopt.checksync.tagwhat.tagname,'.tag',target)
  unlet checksync tagname tagcmd tagby tagwhat target
endfunction

" tag local copy (usertag=0)
function! CVStag()
  call CVSDoTag(0,'')
endfunction

function! CVStagremove()
  call CVSDoTag(0,'-d ')
endfunction

function! CVSbranch()
  call CVSDoTag(0,'-b ')
endfunction

" tag module (usertag=1)
function! CVSrtag()
  call CVSDoTag(1,'')
endfunction

function! CVSrtagremove()
  call CVSDoTag(1,'-d ')
endfunction

function! CVSrbranch()
  call CVSDoTag(1,'-b ',)
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update
"-----------------------------------------------------------------------------

function! CVSupdate()
  " ask for revisions to merge/join (if wanted)
  if g:CVSqueryrevision > 0
    let rev=input('Revision (optional):','')
    if rev!=''
      let rev='-r '.rev.' '
    endif
    let mergerevstart=input('Merge from 1st Revision (optional):','')
    if mergerevstart!=''
      let mergerevstart='-j '.mergerevstart.' '
    endif
    let mergerevend=input('Merge from 2nd Revision (optional):','')
    if mergerevend!=''
      let mergerevend='-j '.mergerevend.' '
    endif
  else
    let rev = ''
    let mergerevstart = ''
    let mergerevend = ''
  endif
  " update or query
  if g:CVSupdatequeryonly > 0
    call CVSDoCommand('-n update -P '.rev.mergerevstart.mergerevend,'.upd','')
  else
    call CVSDoCommand('update '.rev.mergerevstart.mergerevend,'.upd','')
  endif
  unlet rev mergerevstart mergerevend
endfunction

function! CVSqueryupdate()
  let g:CVSupdatequeryonly = 1
  call CVSupdate()
  let g:CVSupdatequeryonly = 0
endfunction

"-----------------------------------------------------------------------------
" CVS remove (request confirmation)
"-----------------------------------------------------------------------------

function! CVSremove()
  " remove from rep. also local ?
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
  " force confirmation
  let confrm=input('Remove:confirm with "y":')
  if confrm!='y'
    echo 'CVS remove: aborted'
    return
  endif
  call CVSDoCommand('remove '.localtoo,'.rem','')
  unlet localtoo
endfunction

"-----------------------------------------------------------------------------
" CVS add
"-----------------------------------------------------------------------------

function! CVSadd()
  " force message input
  let message=escape(input('Message:'),'"<>|&')
  if message==""
    echo 'CVS add: aborted'
    return
  endif
  call CVSDoCommand('add -m "'.message.'" ','.add','')
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
  " force message input
  let message=escape(input('Message:'),'"<>|&')
  if message==''
    echo 'CVS commit: aborted'
    return
  endif
  " query revision (if wanted)
  if g:CVSqueryrevision > 0
    let rev=input('Revision (optional):','')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-r '.rev.' '
  endif
  call CVSDoCommand('commit -m "'.message.'" '.rev,'.ci','')
  unlet message rev
endfunction

"-----------------------------------------------------------------------------
" CVS import
"-----------------------------------------------------------------------------

function! CVSimport()
  " force message input
  let message=escape(input('Message:'),'"<>|&')
  if message==''
    echo 'CVS import: aborted'
    return
  endif
  " query branch (if wanted)
  if g:CVSqueryrevision > 0
    let rev=input('Branch (optional):','')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-b '.rev.' '
  endif
  call CVSDoCommand('import -m "'.message.'" '.rev,'.imp','')
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
  " query revision (if wanted)
  if g:CVSqueryrevision > 0
    let rev=input('Revision (optional):','')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-r '.rev.' '
  endif
  call CVSDoCommand('checkout '.rev.module,'.co','')
  unlet destdir module rev
endfunction

"-----------------------------------------------------------------------------
" compound/complex commands
"-----------------------------------------------------------------------------

function! CVSrevertchanges()
  let filename=expand("%:p:t")
  if filename == ''
    echo 'Revert changes:only on files'
    return
  endif
  if delete(filename) != 0
    echo 'Revert changes:could not delete file'
    return
  endif
  let cvscmdoptbak=$CVSCMDOPT
  let $CVSCMDOPT='-A '
  call CVSupdate()
  let $CVSCMDOPT=cvscmdoptbak
  unlet cvscmdoptbak
endfunction

" get status info, compress it (one line/file), sort by status
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
  call CVSMakeRW()
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
  let v:errmsg=''
  silent! exec '/Status: Up-to-date$'
  if v:errmsg==''
    normal O
  endif
  normal gg
  call CVSMakeRO()
  if savedironce==1
    let g:CVSforcedirectory=1
  endif
  unlet savedironce
endfunction

"-----------------------------------------------------------------------------
" some tools
"-----------------------------------------------------------------------------

" leave only leading line with status info
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

" move all lines matching "searchstr" to top
function! CVSMoveToTop(searchstr)
  let @z=''
  normal gg
  let v:errmsg=''
  while (v:errmsg=='')
    silent! exec '/'.a:searchstr
    if v:errmsg==''
      normal "Zddk
    endif
  endwhile
  normal gg"ZP
endfunction

" set scope : file or directory, inform user
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

" switch scope : file or directory
function! CVSToggleForceDir()
  if g:CVSforcedirectory > 0
    call CVSSetForceDir(0)
  else
    call CVSSetForceDir(2)
  endif
endfunction

" enable/disable revs/branchs queries
function! CVSToggleQueryRevision()
  if g:CVSqueryrevision > 0
    let g:CVSqueryrevision = 0
    echo 'CVS:Not asking for revisions'
  else
    let g:CVSqueryrevision = 1
    echo 'CVS:Enable revision queries'
  endif
endfunction
