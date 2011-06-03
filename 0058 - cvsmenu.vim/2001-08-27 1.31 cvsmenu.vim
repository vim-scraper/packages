" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" $Revision: 1.31 $
" $Date: 2001/08/27 23:26:31 $
" License : LGPL
"
" Tested with Vim 6.0
" Primary site : http://ezytools.sourceforge.net/
" Located in the "VimTools" section
"
" TODO: Better support for additional params
" TODO: Sort more logs (e.g. conflicts)

if exists("loaded_cvsmenu")
  aunmenu CVS
endif

if has('unix')
  let s:sep='/'
else
  let s:sep='\'
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
if !exists("g:CVSdumpandclose")
  let g:CVSdumpandclose = 0		" 1:put output to statusline and close output window
endif

let s:cvsmenuhttp="http://cvs.sf.net/cgi-bin/viewcvs.cgi/~checkout~/ezytools/VimTools/"
let s:cvsmenucvs=":pserver:anonymous@cvs.ezytools.sf.net:/cvsroot/ezytools"

"-----------------------------------------------------------------------------
" Menu entries
"-----------------------------------------------------------------------------
" <esc> in Keyword menus to avoid expansion

amenu &CVS.in&fo  	      				:call CVSShowInfo()<cr>
amenu &CVS.Settin&gs.Toggle\ buffer/&dir		:call CVSToggleForceDir()<cr>
amenu &CVS.Settin&gs.Toggle\ revision\ &queries		:call CVSToggleQueryRevision()<cr>
amenu &CVS.Settin&gs.Toggle\ &output			:call CVSToggleDumpAndClose()<cr>
amenu &CVS.Settin&gs.-SEP1-				:
amenu &CVS.Settin&gs.&Install\ updates			:call CVSInstallUpdates()<cr>
amenu &CVS.Settin&gs.Download\ &updates			:call CVSDownloadUpdates()<cr>
amenu &CVS.Settin&gs.Install\ buffer\ as\ &help		:call CVSInstallAsHelp()<cr>
amenu &CVS.Settin&gs.Install\ buffer\ as\ &plugin	:call CVSInstallAsPlugin()<cr>
amenu &CVS.Director&y.&Add				:call CVSSetForceDir(1)<cr>:call CVSadd()<cr>
amenu &CVS.Director&y.Comm&it				:call CVSSetForceDir(1)<cr>:call CVScommit()<cr>
amenu &CVS.Director&y.S&hort\ Status			:call CVSSetForceDir(1)<cr>:call CVSshortstatus()<cr>
amenu &CVS.Director&y.&Status				:call CVSSetForceDir(1)<cr>:call CVSstatus()<cr>
amenu &CVS.Director&y.&Log				:call CVSSetForceDir(1)<cr>:call CVSlog()<cr>
amenu &CVS.Director&y.&Query\ Update			:call CVSSetForceDir(1)<cr>:call CVSqueryupdate()<cr>
amenu &CVS.Director&y.&Update				:call CVSSetForceDir(1)<cr>:call CVSupdate()<cr>
amenu &CVS.Director&y.Re&move\ from\ rep.		:call CVSSetForceDir(1)<cr>:call CVSremove()<cr>
amenu &CVS.&Keyword.&Author  				a$Author<esc>a$<esc>
amenu &CVS.&Keyword.&Date    				a$Date<esc>a$<esc>
amenu &CVS.&Keyword.&Header  				a$Header<esc>a$<esc>
amenu &CVS.&Keyword.&Id      				a$Id<esc>a$<esc>
amenu &CVS.&Keyword.&Name    				a$Name<esc>a$<esc>
amenu &CVS.&Keyword.Loc&ker  				a$Locker<esc>a$<esc>
amenu &CVS.&Keyword.&Log     				a$Log<esc>a$<esc>
amenu &CVS.&Keyword.RCS&file 				a$RCSfile<esc>a$<esc>
amenu &CVS.&Keyword.&Re&vision				a$Revision<esc>a$<esc>
amenu &CVS.&Keyword.&Source  				a$Source<esc>a$<esc>
amenu &CVS.&Keyword.S&tate   				a$State<esc>a$<esc>
amenu &CVS.-SEP1-					:
amenu &CVS.Ad&min.log&in				:call CVSlogin()<cr>
amenu &CVS.Ad&min.log&out				:call CVSlogout()<cr>
amenu &CVS.D&elete.Re&move\ from\ rep.			:call CVSremove()<cr>
amenu &CVS.D&elete.Re&lease\ workdir			:call CVSrelease()<cr>
amenu &CVS.&Tag.&Create\ Tag				:call CVStag()<cr>
amenu &CVS.&Tag.&Remove\ Tag				:call CVStagremove()<cr>
amenu &CVS.&Tag.Create\ &Branch				:call CVSbranch()<cr>
amenu &CVS.&Tag.-SEP1-					:
amenu &CVS.&Tag.Cre&ate\ Tag\ by\ module		:call CVSrtag()<cr>
amenu &CVS.&Tag.Rem&ove\ Tag\ by\ module		:call CVSrtagremove()<cr>
amenu &CVS.&Tag.Create\ Branc&h\ by\ module		:call CVSrbranch()<cr>
amenu &CVS.&Watch/Edit.&Watchers			:call CVSwatchwatchers()<cr>
amenu &CVS.&Watch/Edit.Watch\ &Add			:call CVSwatchadd()<cr>
amenu &CVS.&Watch/Edit.Watch\ &Remove			:call CVSwatchremove()<cr>
amenu &CVS.&Watch/Edit.Watch\ O&n			:call CVSwatchon()<cr>
amenu &CVS.&Watch/Edit.Watch\ O&ff			:call CVSwatchoff()<cr>
amenu &CVS.&Watch/Edit.-SEP1-				:
amenu &CVS.&Watch/Edit.&Editors				:call CVSwatcheditors()<cr>
amenu &CVS.&Watch/Edit.Edi&t				:call CVSwatchedit()<cr>
amenu &CVS.&Watch/Edit.&Unedit				:call CVSwatchunedit()<cr>
amenu &CVS.-SEP2-					:
amenu &CVS.&Diff					:call CVSdiff()<cr>
amenu &CVS.A&nnotate					:call CVSannotate()<cr>
amenu &CVS.Histo&ry					:call CVShistory()<cr>
amenu &CVS.&Log						:call CVSlog()<cr>
amenu &CVS.&Status					:call CVSstatus()<cr>
amenu &CVS.S&hort\ Status				:call CVSshortstatus()<cr>
amenu &CVS.-SEP3-					:
amenu &CVS.E&xtra.&Update\ to\ revision			:call CVSupdatetorev()<cr>
amenu &CVS.E&xtra.&Merge\ revision			:call CVSupdatemergerev()<cr>
amenu &CVS.E&xtra.Merge\ revision\ &diffs		:call CVSupdatemergediff()<cr>
amenu &CVS.E&xtra.-SEP1-				:
amenu &CVS.E&xtra.&Get\ file				:call CVSGet()<cr>
amenu &CVS.E&xtra.Get\ file\ (&password)		:call CVSGet('','','io')<cr>
amenu &CVS.Check&out					:call CVScheckout()<cr>
amenu &CVS.&Query\ Update				:call CVSqueryupdate()<cr>
amenu &CVS.&Update					:call CVSupdate()<cr>
amenu &CVS.Re&vert\ Changes				:call CVSrevertchanges()<cr>
amenu &CVS.-SEP4-					:
amenu &CVS.&Add						:call CVSadd()<cr>
amenu &CVS.Comm&it					:call CVScommit()<cr>
amenu &CVS.Im&port					:call CVSimport()<cr>

" key mappings : <Leader> (mostly '\' ?), then same as menu hotkeys
" e.g. <ALT>ci = \ci = CVS.Commit
if v:version >= 600
  map <Leader>cf	:call CVSShowInfo()<cr>
  map <Leader>cgd	:call CVSToggleForceDir()<cr>
  map <Leader>cgq	:call CVSToggleQueryRevision()<cr>
  map <Leader>cgo	:call CVSToggleOutput()<cr>
  map <Leader>cya	:call CVSSetForceDir(1)<cr>:call CVSadd()<cr>
  map <Leader>cyi	:call CVSSetForceDir(1)<cr>:call CVScommit()<cr>
  map <Leader>cyh	:call CVSSetForceDir(1)<cr>:call CVSshortstatus()<cr>
  map <Leader>cys	:call CVSSetForceDir(1)<cr>:call CVSstatus()<cr>
  map <Leader>cyl	:call CVSSetForceDir(1)<cr>:call CVSlog()<cr>
  map <Leader>cyq	:call CVSSetForceDir(1)<cr>:call CVSqueryupdate()<cr>
  map <Leader>cyu	:call CVSSetForceDir(1)<cr>:call CVSupdate()<cr>
  map <Leader>cym	:call CVSSetForceDir(1)<cr>:call CVSremove()<cr>
  map <Leader>cka	a$Author<esc>a$<esc>
  map <Leader>ckd	a$Date<esc>a$<esc>
  map <Leader>ckh	a$Header<esc>a$<esc>
  map <Leader>cki	a$Id<esc>a$<esc>
  map <Leader>ckn	a$Name<esc>a$<esc>
  map <Leader>ckk	a$Locker<esc>a$<esc>
  map <Leader>ckl	a$Log<esc>a$<esc>
  map <Leader>ckf	a$RCSfile<esc>a$<esc>
  map <Leader>ckv	a$Revision<esc>a$<esc>
  map <Leader>cks	a$Source<esc>a$<esc>
  map <Leader>ckt	a$State<esc>a$<esc>
  map <Leader>cmi	:call CVSlogin()<cr>
  map <Leader>cmo	:call CVSlogout()<cr>
  map <Leader>cem	:call CVSremove()<cr>
  map <Leader>cel	:call CVSrelease()<cr>
  map <Leader>ctc	:call CVStag()<cr>
  map <Leader>ctr	:call CVStagremove()<cr>
  map <Leader>ctb	:call CVSbranch()<cr>
  map <Leader>cta	:call CVSrtag()<cr>
  map <Leader>cto	:call CVSrtagremove()<cr>
  map <Leader>cth	:call CVSrbranch()<cr>
  map <Leader>cww	:call CVSwatchwatchers()<cr>
  map <Leader>cwa	:call CVSwatchadd()<cr>
  map <Leader>cwr	:call CVSwatchremove()<cr>
  map <Leader>cwn	:call CVSwatchon()<cr>
  map <Leader>cwf	:call CVSwatchoff()<cr>
  map <Leader>cwe	:call CVSwatcheditors()<cr>
  map <Leader>cwt	:call CVSwatchedit()<cr>
  map <Leader>cwu	:call CVSwatchunedit()<cr>
  map <Leader>cd	:call CVSdiff()<cr>
  map <Leader>cn	:call CVSannotate()<cr>
  map <Leader>cr	:call CVShistory()<cr>
  map <Leader>cl	:call CVSlog()<cr>
  map <Leader>cs	:call CVSstatus()<cr>
  map <Leader>ch	:call CVSshortstatus()<cr>
  map <Leader>cxu	:call CVSupdatetorev()<cr>
  map <Leader>cxm	:call CVSupdatemergerev()<cr>
  map <Leader>cxd	:call CVSupdatemergediff()<cr>
  map <Leader>co	:call CVScheckout()<cr>
  map <Leader>cq	:call CVSqueryupdate()<cr>
  map <Leader>cu	:call CVSupdate()<cr>
  map <Leader>cv	:call CVSrevertchanges()<cr>
  map <Leader>ca	:call CVSadd()<cr>
  map <Leader>ci	:call CVScommit()<cr>
  map <Leader>cp	:call CVSimport()<cr>
endif

"-----------------------------------------------------------------------------
" show cvs info
"-----------------------------------------------------------------------------

function! CVSShowInfo()
  exec 'cd '.expand('%:p:h')
  " show CVS info from directory
  let cvsroot='CVS'.s:sep.'Root'
  let cvsrepository='CVS'.s:sep.'Repository'
  exec 'split '.cvsroot
  let root=getline(1)
  close
  exec 'split '.cvsrepository
  let repository=getline(1)
  close
  unlet cvsroot cvsrepository
  " show settings
  new
  let @i = ''
    \."\n\"Current directory : ".expand('%:p:h')
    \."\n\"Current Root : ".root
    \."\n\"Current Repository : ".repository
    \."\n"
    \."\n\"\t\t\t set environment var to cvsroot"
    \."\nlet $CVSROOT=\'".$CVSROOT."\'"
    \."\n\"\t\t\t set environment var to rsh/ssh"
    \."\nlet $CVS_RSH=\'".$CVS_RSH."\'"
    \."\n\"\t\t\t set cvs options (see cvs --help-options)"
    \."\nlet $CVSOPT=\'".$CVSOPT."\'"
    \."\n\"\t\t\t set cvs command options"
    \."\nlet $CVSCMDOPT=\'".$CVSCMDOPT."\'"
    \."\n\"\t\t\t set cvs command"
    \."\nlet $CVSCMD=\'".$CVSCMD."\'"
    \."\n\"\t\t\t run CVS on buffer or directory (0:off 1:once 2:forever)"
    \."\nlet g:CVSforcedirectory = ".g:CVSforcedirectory
    \."\n\"\t\t\t Query for revisions (0:no 1:yes)"
    \."\nlet g:CVSqueryrevision = ".g:CVSqueryrevision
    \."\n\"\t\t\t Toggle output (0:buffer 1:statusline)"
    \."\nlet g:CVSdumpandclose = ".g:CVSdumpandclose
    \."\n\"\t\t\t show cvs version"
    \."\nexec(\':!".$CVSCMD." -v\')"
    \."\n"
    \."\n\"----------------------------------------"
    \."\n\" Change above values to your needs."
    \."\n\" To execute a line, put the cursor on it"
    \."\n\" and press <shift-cr> or <DoubleClick>"
    \."\n\" CVSmenu $Revision: 1.31 $"
    \."\n\" Site: http://ezytools.sf.net/VimTools"
  normal "iP
  normal dd
  if g:CVSdumpandclose > 0
    exec '4,$g/^"/d'
    normal dddd
    call CVSDumpAndClose()
  else
    map <buffer> q :bd!<cr>
    map <buffer> <s-cr> :exec getline('.')<cr>:set nomod<cr>:echo getline('.')<cr>
    map <buffer> <2-LeftMouse> <s-cr>
    set syntax=vim
  endif
  unlet root repository
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update : tools
"-----------------------------------------------------------------------------

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

  if !filereadable($VIM.s:sep.'syntax'.s:sep.'rcslog')
    syn match cvslogRevision	'^revision.*$'
    syn match cvslogFile	'^RCS file:.*'
    syn match cvslogDate	'^date: .*$'
    hi link cvslogFile		Type
    hi link cvslogRevision	Constant
    hi link cvslogDate		Identifier
  endif
endfunction

function! CVSAddConflictSyntax()
  syn region CVSConflictOrg start="^<<<<<<<" end="^===="
  syn region CVSConflictNew start="===" end="^>>>>>>>.*"
  hi link CVSConflictOrg DiffChange
  hi link CVSConflictNew DiffAdd
endfunction

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

"-----------------------------------------------------------------------------
" status variables
"-----------------------------------------------------------------------------

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

" Dump output to statusline, close output buffer
function! CVSToggleDumpAndClose()
  let g:CVSdumpandclose = 1 - g:CVSdumpandclose
  if g:CVSdumpandclose > 0
    echo 'CVS:output to statusline'
  else
    echo 'CVS:output to buffer'
  endif
endfunction
" Dump output to statusline, close output buffer
function! CVSDumpAndClose()
  let endline=line("$")
  let curline=0
  while curline < endline
    let curline = curline + 1
    echo getline(curline)
  endwhile
  set nomodified
  close
endfunction

function! CVSSaveOpts()
  let s:CVSROOTbak            = $CVSROOT
  let s:CVS_RSHbak            = $CVS_RSH
  let s:CVSOPTbak             = $CVSOPT
  let s:CVSCMDOPTbak          = $CVSCMDOPT
  let s:CVSCMDbak             = $CVSCMD
  let s:CVSforcedirectorybak  = g:CVSforcedirectory
  let s:CVSqueryrevisionbak   = g:CVSqueryrevision
  let s:CVSdumpandclosebak    = g:CVSdumpandclose
endfunction

function! CVSRestoreOpts()
  let $CVSROOT                = s:CVSROOTbak          
  let $CVS_RSH                = s:CVS_RSHbak          
  let $CVSOPT                 = s:CVSOPTbak           
  let $CVSCMDOPT              = s:CVSCMDOPTbak        
  let $CVSCMD                 = s:CVSCMDbak           
  let g:CVSforcedirectory     = s:CVSforcedirectorybak
  let g:CVSqueryrevision      = s:CVSqueryrevisionbak 
  let g:CVSdumpandclose       = s:CVSdumpandclosebak  
  unlet s:CVSROOTbak s:CVS_RSHbak s:CVSOPTbak s:CVSCMDOPTbak s:CVSCMDbak
  unlet s:CVSforcedirectorybak s:CVSqueryrevisionbak s:CVSdumpandclosebak
endfunction

"-----------------------------------------------------------------------------
" CVS call
"-----------------------------------------------------------------------------

function! CVSDoCommand(cmd,...)
  " change to buffers directory
  exec 'cd '.expand('%:p:h')
  " get file/directory to work on (if not given)
  if a:0 < 1
    if g:CVSforcedirectory>0
      let filename=''
    else
      let filename=expand('%:p:t')
    endif
  else
    let filename = a:1
  endif
  let tmpnam=tempname()
  exec '!'.$CVSCMD.' '.$CVSOPT.' '.a:cmd.' '.$CVSCMDOPT.' '.filename.'> '.tmpnam
  new
  exec 'read '.tmpnam
  normal ggdd
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
  if g:CVSdumpandclose > 0
    call CVSDumpAndClose()
  endif
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
  let tmpnam=tempname()
  " query revision (if wanted)
  if g:CVSqueryrevision > 0
    let rev=input('Revision (optional):')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-r '.rev.' '
  endif
  wincmd _
  " We need CVS output. -> dont use CVSDoCommand
  exec '!'.$CVSCMD.' '.$CVSOPT.' update -p '.rev.expand('%:p:t').' '.$CVSCMDOPT.'> '.tmpnam
  if v:version<600
    exec 'diffsplit '.tmpnam
  else
    exec 'vertical diffsplit '.tmpnam
  endif
  unlet tmpnam rev
endfunction

"#############################################################################
" from here : use CVSDoCommand wrapper
"#############################################################################
"-----------------------------------------------------------------------------
" CVS annotate / log / status / history
"-----------------------------------------------------------------------------

function! CVSannotate()
  call CVSDoCommand('annotate',expand('%:p:t'))
  wincmd _
endfunction

function! CVSlog()
  call CVSDoCommand('log')
endfunction

function! CVSstatus()
  call CVSDoCommand('status')
endfunction

function! CVShistory()
  call CVSDoCommand('history')
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
  call CVSDoCommand('editors')
endfunction

function! CVSwatchedit()
  let action=CVSQueryAction()
  call CVSDoCommand('edit '.action)
  unlet action
endfunction

function! CVSwatchunedit()
  call CVSDoCommand('unedit')
endfunction

"-----------------------------------------------------------------------------
" CVS watch
"-----------------------------------------------------------------------------

function! CVSwatchwatchers()
  call CVSDoCommand('watchers')
endfunction

function! CVSwatchadd()
  let action=CVSQueryAction()
  call CVSDoCommand('watch add '.action)
  unlet action
endfunction

function! CVSwatchremove()
  call CVSDoCommand('watch remove')
endfunction

function! CVSwatchon()
  call CVSDoCommand('watch on')
endfunction

function! CVSwatchoff()
  call CVSDoCommand('watch off')
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
  call CVSDoCommand(tagcmd.' '.a:tagopt.checksync.tagwhat.tagname,target)
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
    let rev=input('Revision (optional):')
    if rev!=''
      let rev='-r '.rev.' '
    endif
    let mergerevstart=input('Merge from 1st Revision (optional):')
    if mergerevstart!=''
      let mergerevstart='-j '.mergerevstart.' '
    endif
    let mergerevend=input('Merge from 2nd Revision (optional):')
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
    call CVSDoCommand('-n update -P '.rev.mergerevstart.mergerevend)
  else
    call CVSDoCommand('update '.rev.mergerevstart.mergerevend)
  endif
  unlet rev mergerevstart mergerevend
endfunction

function! CVSqueryupdate()
  let g:CVSupdatequeryonly = 1
  call CVSupdate()
  let g:CVSupdatequeryonly = 0
endfunction

function! CVSupdatetorev()
  " Force revision input
  let rev=input('Revision:')
  if rev==''
    echo "UpdateToRev:aborted"
    return
  endif
  let rev='-r '.rev.' '
  " save old state
  call CVSSaveOpts()
  let $CVSCMDOPT=rev
  " call update
  call CVSupdate()
  " restore options
  call CVSRestoreOpts()
  unlet rev
endfunction

function! CVSupdatemergerev()
  " Force revision input
  let mergerevstart=input('Merge from 1st Revision (optional):')
  if mergerevstart==''
    echo "UpdateMergeRev:aborted"
    return
  endif
  let mergerevstart='-j '.mergerevstart.' '
  " save old state
  call CVSSaveOpts()
  let $CVSCMDOPT=mergerevstart
  " call update
  call CVSupdate()
  " restore options
  call CVSRestoreOpts()
  unlet mergerevstart
endfunction

function! CVSupdatemergediff()
  " Force revision input
  let mergerevstart=input('Merge from 1st Revision (optional):')
  if mergerevstart==''
    echo "UpdateMergeRev:aborted"
    return
  endif
  let mergerevend=input('Merge from 2nd Revision (optional):')
  if mergerevend==''
    echo "UpdateMergeRev:aborted"
    return
  endif
  let mergerevstart='-j '.mergerevstart.' '
  let mergerevend='-j '.mergerevend.' '
  " save old state
  call CVSSaveOpts()
  let $CVSCMDOPT=mergerevstart.mergerevend
  " call update
  call CVSupdate()
  " restore options
  call CVSRestoreOpts()
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
  call CVSDoCommand('remove '.localtoo)
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
  call CVSDoCommand('add -m "'.message.'"')
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
    let rev=input('Revision (optional):')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-r '.rev.' '
  endif
  call CVSDoCommand('commit -m "'.message.'" '.rev)
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
    let rev=input('Branch (optional):')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-b '.rev.' '
  endif
  call CVSDoCommand('import -m "'.message.'" '.rev)
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
    let rev=input('Revision (optional):')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-r '.rev.' '
  endif
  call CVSDoCommand('checkout '.rev.module)
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
  call CVSSaveOpts()
  let $CVSCMDOPT='-A '
  call CVSupdate()
  call CVSRestoreOpts()
endfunction

" get status info, compress it (one line/file), sort by status
function! CVSshortstatus()
  " prevent CVSupdate from closing buffer
  let savedump=g:CVSdumpandclose
  let g:CVSdumpandclose=0
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
  " restore output flag, close if needed
  let g:CVSdumpandclose=savedump
  if g:CVSdumpandclose>0
    call CVSDumpAndClose()
  endif
  unlet savedironce savedump
endfunction

"-----------------------------------------------------------------------------
" some tools
"-----------------------------------------------------------------------------

" leave only leading line with status info (for CVSShortStatus)
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

" move all lines matching "searchstr" to top (for CVSShortStatus)
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

"-----------------------------------------------------------------------------
" quick get file.
"-----------------------------------------------------------------------------

function! CVSGet(...)
  " Params :
  " 0:ask file,rep
  " 1:filename
  " 2:repository
  " 3:string:i=login,o=logout
  " save flags, do not destroy CVSSaveOpts
  let cvsoptbak=$CVSCMDOPT
  let outputbak=g:CVSdumpandclose
    let rep=''
    let log=''
  " eval params
  if a:0 == 3		" file,rep,logflag
    let fn=a:1
    let rep=a:2
    let log=a:3
  elseif a:0 == 2	" file,rep
    let fn=a:1
    let rep=a:2
  elseif a:0 == 1	" file: (use current rep) 
    let fn=a:1
  endif
  if fn == ''		" no name:query file and rep
    let rep=input("CVSROOT:")
    let fn=input("Filename:")
  endif
  " still no filename : abort
  if fn == ''
    echo "CVSGet:aborted"
  else
    " prepare param
    if rep != ''
      let $CVSOPT = '-d'.rep
    endif
    " no output windows
    let g:CVSdumpandclose=0
    " login
    if match(log,'i') > -1
      call CVSlogin()
    endif
    " get file
    call CVSDoCommand('checkout -p',fn)
    " logout
    if match(log,'o') > -1
      call CVSlogout()
    endif
  endif
  " restore flags, cleanup
  let g:CVSdumpandclose=outputbak
  let $CVSOPT=cvsoptbak
  unlet fn rep outputbak cvsoptbak
endfunction

"-----------------------------------------------------------------------------
" Download help and install it
"-----------------------------------------------------------------------------

function! CVSInstallUpdates()
  call CVSDownloadUpdates()
  call CVSInstallAsHelp('cvsmenu.txt')
  " force switch buffer. (closing is cached)
  normal 
  call CVSInstallAsPlugin('cvsmenu.vim')
  normal t
  close
  close
endfunction

function! CVSDownloadUpdates()
  call CVSGet('VimTools/cvsmenu.vim',s:cvsmenucvs,'i')
  call CVSGet('VimTools/cvsmenu.txt',s:cvsmenucvs,'o')
endfunction

function! CVSInstallAsHelp(destname)
  " ask for name to save as (if not given)
  let dest=input('Helpfilename (clear to abort):',a:destname)
  " abort if still no filename
  if dest==''
    echo 'Install help:aborted'
  else
    " create directories "~/.vim/doc" if needed
    let localvim=expand('~/.vim')
    let localvimdoc=localvim.'/doc'
    if !isdirectory(localvim)
      exec '!mkdir '.localvim
    endif
    if !isdirectory(localvimdoc)
      exec '!mkdir '.localvimdoc
    endif
    " copy to local doc dir
    exec 'w! '.localvimdoc.'/'.dest
    " create tags
    exec 'helptags '.localvimdoc
    unlet localvim localvimdoc
  endif
  unlet dest
endfunction

function! CVSInstallAsPlugin(destname)
  " ask for name to save as
  let dest=input('Pluginfilename (clear to abort):',a:destname)
  " abort if still no filename
  if dest==''
    echo 'Install plugin:aborted'
  else
    " copy to plugin dir
    exec 'w! '.$VIMRUNTIME.'/plugin/'.dest
  endif
  unlet dest
endfunction

"-----------------------------------------------------------------------------
" initialization
"-----------------------------------------------------------------------------

call CVSAddConflictSyntax()

if !exists("loaded_cvsmenu")
  let loaded_cvsmenu=1
endif

