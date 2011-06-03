" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" $Revision: 1.42 $
" $Date: 2001/09/01 22:08:12 $
" License : LGPL
"
" Tested with Vim 6.0
" Primary site : http://ezytools.sourceforge.net/
" Located in the "VimTools" section
"
" TODO: Better support for additional params

"#############################################################################
" Settings (1/2)
"#############################################################################

if exists("loaded_cvsmenu")
  aunmenu CVS
endif

if !exists('$TIMEOFFSET')
  "let $TIMEOFFSET = 0	" GMT
  let $TIMEOFFSET = 2	" GMT+1, Summertime (CEST)
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
if !exists("g:CVSqueryrevision")
  let g:CVSqueryrevision = 0		" 0:fast update 1:query for revisions
endif
if !exists("g:CVSdumpandclose")
  let g:CVSdumpandclose = 2		" 0:new buffer 1:statusline 2:autoswitch
endif
if !exists("g:CVSsortoutput")
  let g:CVSsortoutput = 1		" sort cvs output (group conflicts,updates,...)
endif
if !exists("g:CVScompressoutput")
  let g:CVScompressoutput = 1		" show extended output only if error
endif

if has('unix')
  let s:sep='/'
else
  let s:sep='\'
endif

let s:CVSupdatequeryonly = 0
let s:CVSentries='CVS'.s:sep.'Entries'
let s:cvsmenuhttp="http://cvs.sf.net/cgi-bin/viewcvs.cgi/~checkout~/ezytools/VimTools/"
let s:cvsmenucvs=":pserver:anonymous@cvs.ezytools.sf.net:/cvsroot/ezytools"

"-----------------------------------------------------------------------------
" Menu entries
"-----------------------------------------------------------------------------
" <esc> in Keyword menus to avoid expansion
" use only TAB between menu item and command (used for MakeLeaderMapping)

amenu &CVS.in&fo					:call CVSShowInfo()<cr>
amenu &CVS.Settin&gs.in&fo\ (buffer)			:call CVSShowInfo(1)<cr>
amenu &CVS.Settin&gs.&Target.File\ in\ &buffer		:call CVSSetForceDir(0)<cr>
amenu &CVS.Settin&gs.&Target.&Directory			:call CVSSetForceDir(2)<cr>
amenu &CVS.Settin&gs.Revision\ &queries.&Enable		:call CVSSetQueryRevision(1)<cr>
amenu &CVS.Settin&gs.Revision\ &queries.&Disable	:call CVSSetQueryRevision(0)<cr>
amenu &CVS.Settin&gs.&Output.-SEP1-				:
amenu &CVS.Settin&gs.&Output.To\ new\ &buffer		:call CVSSetDumpAndClose(0)<cr>
amenu &CVS.Settin&gs.&Output.To\ status&line		:call CVSSetDumpAndClose(1)<cr>
amenu &CVS.Settin&gs.&Output.&Autoswitch		:call CVSSetDumpAndClose(2)<cr>
amenu &CVS.Settin&gs.&Output.-SEP2-				:
amenu &CVS.Settin&gs.&Output.&Compressed		:call CVSSetCompressOutput(1)<cr>
amenu &CVS.Settin&gs.&Output.&Full			:call CVSSetCompressOutput(0)<cr>
amenu &CVS.Settin&gs.&Output.-SEP3-				:
amenu &CVS.Settin&gs.&Output.&Sorted			:call CVSSetSortOutput(1)<cr>
amenu &CVS.Settin&gs.&Output.&Unsorted			:call CVSSetSortOutput(0)<cr>
amenu &CVS.Settin&gs.-SEP1-				:
amenu &CVS.Settin&gs.&Install.&Install\ updates		:call CVSInstallUpdates()<cr>
amenu &CVS.Settin&gs.&Install.&Download\ updates		:call CVSDownloadUpdates()<cr>
amenu &CVS.Settin&gs.&Install.Install\ buffer\ as\ &help	:call CVSInstallAsHelp()<cr>
amenu &CVS.Settin&gs.&Install.Install\ buffer\ as\ &plugin	:call CVSInstallAsPlugin()<cr>
amenu &CVS.Director&y.&Add				:call CVSSetForceDir(1)<cr>:call CVSadd()<cr>
amenu &CVS.Director&y.Comm&it				:call CVSSetForceDir(1)<cr>:call CVScommit()<cr>
amenu &CVS.Director&y.Lo&cal\ Status			:call CVSSetForceDir(1)<cr>:call CVSLocalStatus()<cr>
amenu &CVS.Director&y.S&hort\ Status			:call CVSSetForceDir(1)<cr>:call CVSshortstatus()<cr>
amenu &CVS.Director&y.&Status				:call CVSSetForceDir(1)<cr>:call CVSstatus()<cr>
amenu &CVS.Director&y.&Log				:call CVSSetForceDir(1)<cr>:call CVSlog()<cr>
amenu &CVS.Director&y.&Query\ Update			:call CVSSetForceDir(1)<cr>:call CVSqueryupdate()<cr>
amenu &CVS.Director&y.&Update				:call CVSSetForceDir(1)<cr>:call CVSupdate()<cr>
amenu &CVS.Director&y.Re&move\ from\ rep.		:call CVSSetForceDir(1)<cr>:call CVSremove()<cr>
amenu &CVS.&Keyword.&Author				a$Author<esc>a$<esc>
amenu &CVS.&Keyword.&Date				a$Date<esc>a$<esc>
amenu &CVS.&Keyword.&Header				a$Header<esc>a$<esc>
amenu &CVS.&Keyword.&Id					a$Id<esc>a$<esc>
amenu &CVS.&Keyword.&Name				a$Name<esc>a$<esc>
amenu &CVS.&Keyword.Loc&ker				a$Locker<esc>a$<esc>
amenu &CVS.&Keyword.&Log				a$Log<esc>a$<esc>
amenu &CVS.&Keyword.RCS&file				a$RCSfile<esc>a$<esc>
amenu &CVS.&Keyword.&Re&vision				a$Revision<esc>a$<esc>
amenu &CVS.&Keyword.&Source				a$Source<esc>a$<esc>
amenu &CVS.&Keyword.S&tate				a$State<esc>a$<esc>
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
amenu &CVS.Lo&cal\ Status				:call CVSLocalStatus()<cr>
amenu &CVS.-SEP3-					:
amenu &CVS.E&xtra.&Update\ to\ revision			:call CVSupdatetorev()<cr>
amenu &CVS.E&xtra.&Merge\ revision			:call CVSupdatemergerev()<cr>
amenu &CVS.E&xtra.Merge\ revision\ &diffs		:call CVSupdatemergediff()<cr>
amenu &CVS.E&xtra.-SEP1-				:
amenu &CVS.E&xtra.CVS\ &Links				:call CVSOpenLinks()<cr>
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

"" create key mappings from this script
"" key mappings : <Leader> (mostly '\' ?), then same as menu hotkeys
"" e.g. <ALT>ci = \ci = CVS.Commit
function! CVSMakeLeaderMapping()
  let cvsmenu=expand("$VIM").s:sep.'plugin'.s:sep.'cvsmenu.vim'
  silent! call CVSMappingFromMenu(cvsmenu,',')
  unlet cvsmenu
endfunction

function! CVSMappingFromMenu(filename,...)
  if !filereadable(a:filename)
    return
  endif
  if a:0 == 0
    if v:version < 600
      let leader = '\'
    else
      let leader = '<Leader>'
    endif
  else
    let leader = a:1
  endif
  " create mappings from &-chars
  new
  exec 'read '.a:filename
  " leave only amenu defs
  exec 'g!/^\s*amenu/d'
  " delete separators and blank lines
  exec 'g/\.-SEP/d'
  exec 'g/^$/d'
  " count entries
  let entries=line("$")
  " extract menu entries, put in @m
  exec '%s/^\s*amenu\s\([^'."\t".']*\).\+/\1/eg' 
  exec '%y m'
  " extract mappings from '&'
  exec '%s/&\(\w\)[^&]*/\l\1/eg'
  " create cmd, delete to @k
  exec '%s/^\(.*\)$/nmap '.leader.'\1 :em /eg' 
  exec '%d k'
  " restore menu, delete '&'
  normal "mP
  exec '%s/&//eg'
  " visual block inserts failed, when called from script (vim60at)
  " append keymappings
  normal G"kP
  " merge keys/commands, execute
  let curlin=0
  while curlin < entries
    let curlin = curlin + 1
    call setline(curlin,getline(curlin + entries).getline(curlin).'<cr>')
    exec getline(curlin)
  endwhile
  set nomodified
  bdelete
endfunction

"-----------------------------------------------------------------------------
" show cvs info
"-----------------------------------------------------------------------------
" Param : ToBuffer (bool)
function! CVSShowInfo(...)
  if a:0 == 0
    let tobuf = 0
  else
    let tobuf = a:1
  endif
  exec 'cd '.expand('%:p:h')
  " show CVS info from directory
  let cvsroot='CVS'.s:sep.'Root'
  let cvsrepository='CVS'.s:sep.'Repository'
  silent! exec 'split '.cvsroot
  let root=getline(1)
  close
  silent! exec 'split '.cvsrepository
  let repository=getline(1)
  close
  unlet cvsroot cvsrepository
  " show settings
  new
  let regbak=@z
  let @z = ''
    \."\n\"CVSmenu $Revision: 1.42 $"
    \."\n\"Current directory : ".expand('%:p:h')
    \."\n\"Current Root : ".root
    \."\n\"Current Repository : ".repository
    \."\n\""
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
    \."\n\"\t\t\t Toggle output (0:buffer 1:statusline 2:autoswitch)"
    \."\nlet g:CVSdumpandclose = ".g:CVSdumpandclose
    \."\n\"\t\t\t Toggle sorting output (0:no sorting)"
    \."\nlet g:CVSsortoutput = ".g:CVSsortoutput
    \."\n\"\t\t\t Show extended output only if error"
    \."\nlet g:CVScompressoutput = ".g:CVScompressoutput
    \."\n\"\t\t\t show cvs version"
    \."\nexec(\':!".$CVSCMD." -v\')"
    \."\n"
    \."\n\"----------------------------------------"
    \."\n\" Change above values to your needs."
    \."\n\" To execute a line, put the cursor on it"
    \."\n\" and press <shift-cr> or <DoubleClick>"
    \."\n\" Site: http://ezytools.sf.net/VimTools"
  normal "zP
  let @z=regbak
  normal dd
  if tobuf == 0
    exec '5,$g/^"/d'
    normal dddd
    call CVSDumpAndClose()
  else
    map <buffer> q :bd!<cr>
    map <buffer> <s-cr> :exec getline('.')<cr>:set nomod<cr>:echo getline('.')<cr>
    map <buffer> <2-LeftMouse> <s-cr>
    set syntax=vim
    set nomodified
  endif
  unlet root repository tobuf 
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

  syn match cvslocalstatusUnknown	'^unknown:.*'
  syn match cvslocalstatusUnchanged	'^unchanged:.*'
  syn match cvslocalstatusMissing	'^missing:.*'
  syn match cvslocalstatusModified	'^modified:.*'
  hi link cvslocalstatusUnknown		Comment
  hi link cvslocalstatusUnchanged	Type
  hi link cvslocalstatusMissing		Identifier
  hi link cvslocalstatusModified	Constant

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
  syn region CVSConflictOrg start="^<<<<<<<" end="^====" contained
  syn region CVSConflictNew start="===$" end="^>>>>>>>" contained
  syn region CVSConflict start="^<<<<<<<" end=">>>>>>>.*" contains=CVSConflictOrg,CVSConflictNew keepend
"  hi link CVSConflict Special
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

" output window: open file under cursor by <doubleclick> or <shift-cr>
function! CVSUpdateMapping()
  nmap <buffer> <2-LeftMouse> 0<C-Right>
  nmap <buffer> <S-CR> 0<C-Right>
  nmap <buffer> q :bd!<cr>
endfunction

"-----------------------------------------------------------------------------
" sort output
"-----------------------------------------------------------------------------

" move all lines matching "searchstr" to top
function! CVSMoveToTop(searchstr)
  silent exec 'g/'.a:searchstr.'/m0'
endfunction

" only called by CVSShortStatus
function! CVSSortStatusOutput()
  " allow changes
  call CVSMakeRW()
  call CVSMoveToTop('Status: Unknown$')
  call CVSMoveToTop('Status: Needs Checkout$')
  call CVSMoveToTop('Status: Needs Merge$')
  call CVSMoveToTop('Status: Needs Patch$')
  call CVSMoveToTop('Status: Locally Removed$')
  call CVSMoveToTop('Status: Locally Added$')
  call CVSMoveToTop('Status: Locally Modified$')
  call CVSMoveToTop('Status: File had conflicts on merge$')
endfunction

" called by CVSDoCommand
function! CVSSortOutput()
  " allow changes
  call CVSMakeRW()
  " localstatus
  call CVSMoveToTop('^unknown:')
  call CVSMoveToTop('^unchanged:')
  call CVSMoveToTop('^missing:')
  call CVSMoveToTop('^modified:')
  " org cvs
  call CVSMoveToTop('^? ')	" unknown
  call CVSMoveToTop('^T ')	" tag
  call CVSMoveToTop('^D ')	" delete
  call CVSMoveToTop('^N ')	" new
  call CVSMoveToTop('^U ')	" update
  call CVSMoveToTop('^M ')	" merge
  call CVSMoveToTop('^P ')	" patch
  call CVSMoveToTop('^C ')	" conflict
endfunction

"-----------------------------------------------------------------------------
" status variables
"-----------------------------------------------------------------------------

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

" Set output to statusline, close output buffer
function! CVSSetDumpAndClose(value)
  if a:value > 1
    echo 'CVS:output to status(file) and buffer(dir)'
  elseif a:value > 0
    echo 'CVS:output to statusline'
  else
    echo 'CVS:output to buffer'
  endif
  let g:CVSdumpandclose = a:value
endfunction

" enable/disable revs/branchs queries
function! CVSSetQueryRevision(value)
  if a:value > 0
    echo 'CVS:Enabled revision queries'
  else
    echo 'CVS:Not asking for revisions'
  endif
  let g:CVSqueryrevision = a:value
endfunction

" Sort output (group conflicts,updates,...)
function! CVSSetSortOutput(value)
  if a:value > 0
    echo 'CVS:sorting output'
  else
    echo 'CVS:unsorted output'
  endif
  let g:CVSsortoutput = value
endfunction


" compress output to one line
function! CVSSetCompressOutput(value)
  if a:value > 0
    echo 'CVS:compressed output'
  else
    echo 'CVS:full output'
  endif
  let g:CVScompressoutput = value
endfunction

"#############################################################################
" CVS commands
"#############################################################################

"-----------------------------------------------------------------------------
" CVS call
"-----------------------------------------------------------------------------

function! CVSDoCommand(cmd,...)
  " needs to be called from orgbuffer
  let isfile = CVSUsesFile()
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
  let regbak=@z
  let @z=system($CVSCMD.' '.$CVSOPT.' '.a:cmd.' '.$CVSCMDOPT.' '.filename)
  new
  normal "zP
  call CVSProcessOutput(isfile, filename, a:cmd)
  let @z=regbak
  unlet filename
endfunction

" also jumped in by CVSLocalStatus
function! CVSProcessOutput(isfile,filename,cmd)
  " delete leading and trainling blank lines
  while (getline(1) == '') && (line("$")>1)
    exec '0d'
  endwhile
  while (getline("$") == '') && (line("$")>1)
    exec '$d'
  endwhile
  " group conflicts, updates, ....
  if g:CVSsortoutput > 0
    call CVSSortOutput()
  endif
  " compress output ?
  if g:CVScompressoutput > 0
    if (g:CVSdumpandclose > 0) && a:isfile
      call CVSCompressOutput(a:cmd)
    endif
  endif
  " move to top
  normal gg
  set nowrap
  " reset single shot flag
  if g:CVSforcedirectory==1
    let g:CVSforcedirectory = 0
  endif
  call CVSMakeRO()
  call CVSUpdateMapping()
  call CVSUpdateSyntax()
  if (g:CVSdumpandclose == 1) || ((g:CVSdumpandclose == 2) && a:isfile)
    call CVSDumpAndClose()
  endif
endfunction

" return: 1=file 0=dir
function! CVSUsesFile()
  let filename=expand("%:p:t")
  if    ((g:CVSforcedirectory == 0) && (filename != ''))
   \ || ((g:CVSforcedirectory > 0) && (filename == ''))
    return 1
  else
    return 0
  endif
  unlet filename
endfunction

" compress output
function! CVSCompressOutput(cmd)
  " commit
  if match(a:cmd,"commit") > -1
    let v:errmsg = ''
    silent! exec '/^cvs \[commit aborted]:'
    " only compress, if no error found
    if v:errmsg != ''
      silent! exec 'g!/^new revision:/d'
    endif
  endif
endfunction

"#############################################################################
" following commands read from STDIN. Call CVS directly
"#############################################################################

"-----------------------------------------------------------------------------
" CVS login / logout (password prompt)
"-----------------------------------------------------------------------------

function! CVSlogin(...)
  if a:0 == 0
    let pwpipe = ''
  else
    let pwpipe = 'echo '
    if !has("unix") 
      if a:1 == ''
        let pwpipe = pwpipe . '.'
      endif
    endif
    let pwpipe = pwpipe . a:1 . '|'
  endif
  if has("unix")
    " show password prompt 
    exec '!'.pwpipe.$CVSCMD.' '.$CVSOPT.' login '.$CVSCMDOPT
  else
    " shell is opened in win32 (dos?)
    silent! exec '!'.pwpipe.$CVSCMD.' '.$CVSOPT.' login '.$CVSCMDOPT
  endif
endfunction

function! CVSlogout()
  silent! exec '!'.$CVSCMD.' '.$CVSOPT.' logout '.$CVSCMDOPT
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
  if has("unix")
    " show confirmation prompt
    exec '!'.$CVSCMD.' '.$CVSOPT.' release '.localtoo.releasedir.' '.$CVSCMDOPT
  else
    silent! exec '!'.$CVSCMD.' '.$CVSOPT.' release '.localtoo.releasedir.' '.$CVSCMDOPT
  endif
  unlet localtoo releasedir
endfunction

"#############################################################################
" from here : use CVSDoCommand wrapper
"#############################################################################

"-----------------------------------------------------------------------------
" CVS diff (output needed)
"-----------------------------------------------------------------------------

function! CVSdiff()
  exec 'cd '.expand('%:p:h')
  let outputbak = g:CVSdumpandclose
  let g:CVSdumpandclose = 0
  " tempname() would be deleted before diff (linux)!
  let tmpnam=expand("%").'.dif'.localtime()
  " query revision (if wanted)
  if g:CVSqueryrevision > 0
    let rev=input('Revision (optional):')
  else 
    let rev=''
  endif
  if rev!=''
    let rev='-r '.rev.' '
  endif
  call CVSDoCommand('update -p '.rev)
  redraw
  " delete stderr ('checking out...')
  call CVSStripHeader()
  call CVSMakeRO()
  " x! did not write before diffing (linux)!
  exec "w! ".tmpnam
  bdelete
  wincmd _
  redraw
  if v:version<600
    exec 'diffsplit '.tmpnam
  else
    exec 'vertical diffsplit '.tmpnam
  endif
  let dummy=delete(tmpnam)
  " jump to next diff (other buffer)
  map <buffer> <tab> ]c
  map <buffer> <s-tab> [c
  let g:CVSdumpandclose = outputbak
  unlet outputbak
  unlet tmpnam rev
  "dummy
endfunction

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
  if s:CVSupdatequeryonly > 0
    call CVSDoCommand('-n update -P '.rev.mergerevstart.mergerevend)
  else
    call CVSDoCommand('update '.rev.mergerevstart.mergerevend)
  endif
  unlet rev mergerevstart mergerevend
endfunction

function! CVSqueryupdate()
  let s:CVSupdatequeryonly = 1
  call CVSupdate()
  let s:CVSupdatequeryonly = 0
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

"#############################################################################
" extended commands
"#############################################################################

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
  let isfile = CVSUsesFile()
  " save flags
  let filename = expand("%:p:t")
  let savedump = g:CVSdumpandclose
  let forcedirbak = g:CVSforcedirectory
  " output needed
  let g:CVSdumpandclose=0
  call CVSstatus()
  call CVSMakeRW()
  call CVSCompressStatus()
  if g:CVSsortoutput > 0
    call CVSSortStatusOutput()
  endif
  normal gg
  call CVSMakeRO()
  " restore flags
  let g:CVSdumpandclose = savedump
  if forcedirbak == 1
    let g:CVSforcedirectory = 0
  else
    let g:CVSforcedirectory = forcedirbak
  endif
  if   (g:CVSdumpandclose == 1) || ((g:CVSdumpandclose == 2) && isfile)
    call CVSDumpAndClose()
  endif
  unlet savedump forcedirbak filename isfile
endfunction

"-----------------------------------------------------------------------------
" some tools
"-----------------------------------------------------------------------------

" Dump output to statusline, close output buffer
function! CVSDumpAndClose()
  " collect in reg. z first, otherwise func
  " will terminate, if user stops output with "q"
  let curlin=1
  let regbak=@z
  let @z = getline(curlin)
  while curlin < line("$")
    let curlin = curlin + 1
    let @z = @z . "\n" . getline(curlin)
  endwhile
  " appends \n on winnt
  "exec ":1,$y z"
  set nomodified
  close
  redraw
  " statusline may be cleared otherwise
  echo @z
  let @z=regbak
endfunction

" leave only leading line with status info (for CVSShortStatus)
function! CVSCompressStatus()
  exec 'g!/^File:\|?/d'
endfunction

" delete stderr ('checking out...')
" CVS checkout ends with ***************(15)
function! CVSStripHeader()
  call CVSMakeRW()
  exec '1,/\*\{15}/d'
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
  " 4:string:login password
  " save flags, do not destroy CVSSaveOpts
  let cvsoptbak=$CVSCMDOPT
  let outputbak=g:CVSdumpandclose
  let rep=''
  let log=''
  let fn=''
  " eval params
  if     a:0 > 2	" file,rep,logflag[,logpw]
    let fn  = a:1
    let rep = a:2
    let log = a:3
  elseif a:0 > 1	" file,rep
    let fn  = a:1
    let rep = a:2
  elseif a:0 > 0	" file: (use current rep) 
    let fn  = a:1
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
      if (a:0 == 4)	" login with pw (if given)
        call CVSlogin(a:4)
      else
        call CVSlogin()
      endif
    endif
    " get file
    call CVSDoCommand('checkout -p',fn)
    " delete stderr ('checking out...')
    call CVSStripHeader()
    set nomodified
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
  if confirm("Install updates: Close all buffers, first !", 
           \ "&Cancel\n&Ok") < 2
    echo 'Install updates:aborted'
    return
  endif
  call CVSDownloadUpdates()
  call CVSInstallAsHelp('cvsmenu.txt')
  " force switch buffer. (closing seems to be "cached")
  normal 
  call CVSInstallAsPlugin('cvsmenu.vim')
  normal t
  close
  close
endfunction

function! CVSDownloadUpdates()
  call CVSGet('VimTools/cvsmenu.vim',s:cvsmenucvs,'i','')
  call CVSGet('VimTools/cvsmenu.txt',s:cvsmenucvs,'o','')
endfunction

function! CVSInstallAsHelp(...)
  " ask for name to save as (if not given)
  if (a:0 == 0) || (a:1 == '')
    let dest=input('Helpfilename (clear to abort):')
  else
    let dest=a:1
  endif
  " abort if still no filename
  if dest==''
    echo 'Install help:aborted'
  else
    " create directories "~/.vim/doc" if needed
    call CVSAssureLocalDirs()
    " copy to local doc dir
    exec 'w! '.s:localvimdoc.'/'.dest
    " create tags
    exec 'helptags '.s:localvimdoc
  endif
  unlet dest
endfunction

function! CVSInstallAsPlugin(...)
  " ask for name to save as
  if (a:0 == 0) || (a:1 == '')
    let dest=input('Pluginfilename (clear to abort):',a:1)
  else
    let dest=a:1
  endif
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
" user directories / CVSLinks
"-----------------------------------------------------------------------------

function! CVSOpenLinks()
  let links=s:localvim.s:sep.'cvslinks.vim'
  call CVSAssureLocalDirs()
  if !filereadable(links)
    let @z = "\" ~/cvslinks.vim\n"
      \ . "\" move to a command and press <shift-cr> to execute it\n"
      \ . "\" (one-liners only).\n\n"
      \ . "nmap <buffer> <2-leftmouse> :exec getline('.')<cr>\n"
      \ . "nmap <buffer> <s-cr> :exec getline('.')<cr>\n"
      \ . "finish\n\n"
      \ . "\" add modifications below here\n\n"
      \ . "\" look for a new Vim\n"
      \ . "\" login, get latest Vim README.txt, logout\n"
      \ . "call CVSGet('vim/README.txt', ':pserver:anonymous@cvs.vim.org:/cvsroot/vim', 'io', '')\n\n"
      \ . "\" manual cvsmenu update (-> CVS.Settings.Install buffer as...)\n"
      \ . "\" login, get latest version of cvsmenu.vim\n"
      \ . "call CVSGet('VimTools/cvsmenu.vim',':pserver:anonymous@cvs.ezytools.sf.net:/cvsroot/ezytools','i','')\n"
      \ . "\" get latest cvsmenu.txt, logout\n"
      \ . "call CVSGet('VimTools/cvsmenu.vim',':pserver:anonymous@cvs.ezytools.sf.net:/cvsroot/ezytools','o','')\n\n"
      \ . "\" Get some help on this\n"
      \ . "help CVSFunctions"
    exec ':cd '.s:localvim
    new
    normal "zP
    exec ':x '.links
  endif
  if !filereadable(links)
    echo 'CVSLinks: cannot access '.links
    return
  endif
  exec ':sp '.links
  exec ':so %'
  unlet links
endfunction

function! CVSAssureLocalDirs()
  if !isdirectory(s:localvim)
    silent! exec '!mkdir '.s:localvim
  endif
  if !isdirectory(s:localvimdoc)
    silent! exec '!mkdir '.s:localvimdoc
  endif
endfunction

function! CVSGetFolderNames()
  if has("unix")
    " expands to /home/(user)
    let s:localvim=expand('~').s:sep.'.vim'
  else
    " expands to $HOME (must be set)
    if expand('~') == ''
      let s:localvim=$VIMRUNTIME
    else
      let s:localvim=expand('~').s:sep.'vimfiles'
    endif
  endif
  let s:localvimdoc=s:localvim.s:sep.'doc'
endfunction

"-----------------------------------------------------------------------------
" LocalStatus : read from CVS/Entries
"-----------------------------------------------------------------------------

function! CVSLocalStatus()
  " needs to be called from orgbuffer
  let isfile = CVSUsesFile()
  " change to buffers directory
  exec 'cd '.expand('%:p:h')
  if g:CVSforcedirectory>0
    let filename=expand('%:p:h')
  else
    let filename=expand('%:p:t')
  endif
  let regbak=@z
  new
  let @z = CVSCompare(filename)
  normal "zP
  call CVSProcessOutput(isfile, filename, '*localstatus')
  let @z=regbak
  unlet filename
endfunction

" get info from CVS/Entries about given/current buffer/dir
function! CVSCompare(...)
  " return, if no CVS dir
  if !filereadable(s:CVSentries)
    echo 'No '.s:CVSentries.' !'
    return
  endif
  " eval params
  if (a:0 == 1) && (a:1 != '')
    if filereadable(a:1)
      let filename = a:1
      let dirname  = ''
    else
      let filename = ''
      let dirname  = a:1
    endif
  else
    let filename = expand("%:p:t")
    let dirname  = expand("%:p:h")
  endif
  let result = ''
  if filename == ''
    let result = CVSGetLocalDirStatus(dirname)
  else
    let result = CVSGetLocalStatus(filename)
  endif  " filename given
  return result
endfunction

" get info from CVS/Entries about given file/current buffer
function! CVSGetLocalStatus(...)
  if a:0 == 0
    let filename = expand("%:p:t")
  else
    let filename = a:1
  endif
  if filename == ''
    return 'error:no filename'
  endif
  if a:0 > 1
    let entry=a:2
  else
    let entry=CVSGetEntry(filename)
  endif
  if entry == ''
    if isdirectory(filename)
      let result = "unknown:   <DIR>\t".filename"
    else
      let result = 'unknown:   '.filename
    endif
  else
    let entryver  = CVSSubstr(entry,'/',2)
    let entryopt  = CVSSubstr(entry,'/',4)
    let entrytag  = CVSSubstr(entry,'/',5)
    let entrytime = CVStimeToStr(entry)
    let info = filename."\t".entryver." ".entrytime." ".entryopt." ".entrytag
    if !filereadable(filename)
      if isdirectory(filename)
        let result = 'directory: '.filename
      else
	if entry[0] == 'D'
          let result = "missing:   <DIR>\t".filename
	else
          let result = 'missing:   '.info
	endif
      endif
    else
      if entrytime == CVSFiletimeToStr(filename)
	let result = 'unchanged: '.info
      else
	let result = 'modified:  '.info
      endif " time identical
    endif " file exists
  endif  " entry found
  unlet entry
  return result
endfunction

" get info on all files from CVS/Entries and given/current directory
" opens CVS/Entries only once, passing each entryline to CVSGetLocalStatus
function! CVSGetLocalDirStatus(...)
  let zbak = @z
  let ybak = @y
  if a:0 == 0
    let dirname = expand("%:p:h")
  else
    let dirname = a:1
  endif
  exec 'cd '.dirname
  if has("unix")
    let @z = glob("*")
  else
    let @z = glob("*.*")
  endif
  new
  silent! exec 'read '.s:CVSentries
  let entrycount = line("$") - 1
  normal k"zP
  if (line("$") == entrycount) && (getline(entrycount) == '')
    " empty directory
    set nomodified
    return
  endif
  let filecount = line("$") - entrycount
  " collect status of all found files in @y
  let @y = ''
  let curlin = 0
  while (curlin < filecount)
    let curlin = curlin + 1
    let fn=getline(curlin)
    if fn != 'CVS'
      let search=escape(fn,'.')
      let v:errmsg = ''
      " find CVSEntry
      silent! exec '/^D\?\/'.search.'\//'
      if v:errmsg == ''
        let entry = getline(".")
	" clear found file from CVS/Entries
	silent! exec 's/.*//eg'
      else
	let entry = ''
      endif
      " fetch info
      let @y = @y . CVSGetLocalStatus(fn,entry) . "\n"
    endif
  endwhile
  " process files from CVS/Entries
  let curlin = filecount
  while (curlin < line("$"))
    let curlin = curlin + 1
    let entry = getline(curlin)
    let fn=CVSSubstr(entry,'/',1)
    if fn != ''
      let @y = @y . CVSGetLocalStatus(fn,entry) . "\n"
    endif
  endwhile
  set nomodified
  let result = @y
  bdelete
  let @z = zbak
  let @y = ybak
  unlet zbak ybak
  return result
endfunction

" return info about filename from 'CVS/Entries'
function! CVSGetEntry(filename)
  let result = ''
  if a:filename != ''
    silent! exec 'split '.s:CVSentries
    let v:errmsg = ''
    let search=escape(a:filename,'.')
    silent! exec '/^D\?\/'.search.'\//'
    if v:errmsg == ''
      let result = getline(".")
    endif
    set nomodified
    bdelete
  endif
  return result
endfunction

" extract and convert timestamp from CVSEntryItem
function! CVStimeToStr(entry)
  return CVSAsctimeToStr(CVSSubstr(a:entry,'/',3))
endfunction
" get and convert filetime
function! CVSFiletimeToStr(filename)
  return strftime('%Y-%m-%d %H:%M:%S',getftime(a:filename))
endfunction

" entry format : ISO C asctime()
" include local time zone info
function! CVSAsctimeToStr(asctime)
  let mon=strpart(a:asctime, 4,3)
  let DD=CVSLeadZero(strpart(a:asctime, 8,2))
  let hh=CVSLeadZero(strpart(a:asctime, 11,2) + $TIMEOFFSET)
  let nn=CVSLeadZero(strpart(a:asctime, 14,2))
  let ss=CVSLeadZero(strpart(a:asctime, 17,2))
  let YY=strpart(a:asctime, 20,4)
  let MM=CVSMonthIdx(mon)
  let result = YY.'-'.MM.'-'.DD.' '.hh.':'.nn.':'.ss
  unlet YY MM DD hh nn ss mon
  return result
endfunction

" append a leading zero
function! CVSLeadZero(value)
  let nr=substitute(a:value,' ','','g') + 0
  if (nr < 10)
    let nr = '0' . nr
  endif
  return nr
endfunction

" return month (leading zero) from cleartext
function! CVSMonthIdx(month)
  if match(a:month,'Jan') > -1
    return '01'
  elseif match(a:month,'Feb') > -1
    return '02'
  elseif match(a:month,'Mar') > -1
    return '03'
  elseif match(a:month,'Apr') > -1
    return '04'
  elseif match(a:month,'May') > -1
    return '05'
  elseif match(a:month,'Jun') > -1
    return '06'
  elseif match(a:month,'Jul') > -1
    return '07'
  elseif match(a:month,'Aug') > -1
    return '08'
  elseif match(a:month,'Sep') > -1
    return '09'
  elseif match(a:month,'Oct') > -1
    return '10'
  elseif match(a:month,'Nov') > -1
    return '11'
  elseif match(a:month,'Dec') > -1
    return '12'
  else
    return '00'
endfunction

" divide string by sep, return field[index] .start at 0.
function! CVSSubstr(string,separator,index)
  let sub = ''
  let idx = 0
  let bst = 0
  while (bst < strlen(a:string)) && (idx <= a:index)
    if a:string[bst] == a:separator
      let idx = idx + 1
    else
      if (idx == a:index)
        let sub = sub . a:string[bst]
      endif
    endif
    let bst = bst + 1
  endwhile
  unlet idx bst
  return sub
endfunction

"#############################################################################
" Settings (2/2)
"#############################################################################

"-----------------------------------------------------------------------------
" initialization
"-----------------------------------------------------------------------------

call CVSGetFolderNames()
call CVSAddConflictSyntax()
call CVSMakeLeaderMapping()

au BufRead * call CVSAddConflictSyntax()

if !exists("loaded_cvsmenu")
  let loaded_cvsmenu=1
endif
