" Source safe commands
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 1.16

" This sourcesafe integration is based on some shell scripts that I've had
" kicking round for ages to do the same thing.
"
"
" It has been improved by (at least) the contributions of these people:
"   David Eggum (borrowed code)
"   Vikas Agnihotri (fixes & suggestions)
"   Zak Beck (fixes & suggestions)
"
" I have borrowed a number of ideas from the sourcesafe script of David Eggum, with
" the idea of merging the scripts.  These are:
"   * Using a central function to provide all functionality.
"   * Adding ':' commands for all the functions.
"   * The basis for the 'checked out files' browser.
"   * Various option names
"   * The file status stuff
"
" The script relies on an environment variable $SS to be set to point to
" ss.exe, and on a '.project' file specifying which srcsafe database to use.
" I have also added some compatability with David's scripts.
" 
" The scripts require the sourcesafe command-line to be working without
" prompting for a password.  To do this, there are two main methods. The first
" is to set the the %SSDIR% enviornment variable.
" SSDIR=\\server\share\ssdir
"
" The second is to put the following line in your local srcsafe.ini:
"     #include \\server\share\ssdir\srcsafe.ini
"
"
" The script works by scanning for a file  '.project' which should be in the root
" directory of the project. 
" This file should contain the source-safe path
" eg:
" $/MyProj/
" Version 1.6 has improved support for multiple databases. 
" A sourcesafe path (or inifile) can precede this, inclosed with '@' symbols : 
" eg:
" @\\file\srcsafe@$/MyProj/
" This works by setting (& restoring) $SSDIR before calling the
" command-line.
" 
" Note that in the following, you can specify a count for Get, Diff,  (checkout ??)
" This does a get of/diff against the specified version.
" 
" The scripts do their best to allow the user to respond to prompts.  It does
" this by always answering 'no', then parsing the response, and asking the
" user 'yes/no', and then re-executing the command always answering 'yes'.
" It is possible that there are some special cases that need handling.
" 
" Here are the commands availble: (replace \ with <localleader>)
" 
" Command       Map       Count      Description
" --------------------------------------------------
" SDiff         \sf       Version    Diff (Vim Diff)
" SDiffClose    \sF                  Close Diff
" SDiffSS       \sd       Version    Diff (text diff)
" SCheckout     \sk       Version    Checkout
" SLock         \s<c-k>   Version    Checkout (No Get)
" SUncheckout   \s<c-U>              Uncheckout
" SGet          \sg       Version    Get
" SMerge        \sG                  Merge get
" SUpdate       \su                  Update (checkin)
" SStatus       \ss                  Status
"               \sh       Count      History (last n / 3)
" SHistory      \sH                  History window (vertical split - syntax highlighted)
" SAdd          \sA                  Add file to sourcesafe
" SRepeatUpdate \\u                  Update (same comment)
" SUpdateLocked \sU                  Update and keep locked
" SRepeatUpdateLocked \\U            Update (same comment) and keep locked.
" SDeploy                            Deploy the specified directory
"
" SPLocked[!]  [project [username]]
"   View locked files for the given project (or the cwd) - ! specifies recursive.
"   Username can be left blank (for current user) or specified as '*' for all
"   users.
" Commands are similar to the browser
"    <enter> : open file or directory
"    O : open new window for file/directory
"    o : open file in previously visited window
"    p : preview the file
"    f : diff mode
"    u : update file
"    U : repeat update
"    K : checkout
"    L : lock
" Options: Define these in your own .vimrc - defaults in brackets.
" g:ssMenuPath       (&VSS)      : The full menu path prefix for VSS
" g:ssMenuPlace      (40)        : The order specifier prefix for menus
" g:ssDiffVertical   (1)         : Whether diff is vertical or horizontal
" g:ssHistVertical   (1)         : Whether history is vertical or horizontal
" g:ssSetRuler       (1)         : Whether to set the ruler option
" g:ssMaintainStatus (1)         : Maintain the buffers status as possible.
" g:ssShowAllLocks   (1)         : Show all locks on the status line
" g:ssShowExtra      (1)         : Show extra status information
" g:ssUserName       ($USERNAME) : Username for Current user of Sourcesafe
" g:ssExecutable     ($SS)       : May NOT have spaces - use short-filenames.
" g:ssDeployFile     (0)         : Allow deploying of files.
"
" NOTES:
" * If the directory $TEMP/SS  (which is required by the script) does not
"   exist, then create it / warn the user if $TEMP doesn't exist.
" * Allow closing of the diff buffer, returning (as best as I can get) the
"   screen to how it was before the diff.
"  - Also closes the last srcsafe diff before opening the new one.
"  - Clears 'diff' on all other windows, and tries (as best as it can) to
"    restore the 'diff' when the srcsafe diff closes.
" * If you close the temporary diff file -then this also causes the file view to be
"    restored
" * Closing the original file when doing a srcsafe diff causes the diff file
"   to be closed as well.
"
" History:
" 1.7: Added displaying of sourcesafe status (from David E's scripts)
"      Belatedly Acknowledged David Eggum for his stuff.
"      Fixed up the menus to display the correct keystrokes.
"      Added various options from David E's scripts
" 1.8: Fixed up some problems with spaces in paths. Thanks to Vikas Agnihotri
" 1.9: Various suggestions/fixes from Zak Beck:
"      - Fixed Explore/Admin so they work 
"      - Add UpdateLocked and UpdateSimilarLocked
"      - Added Deploy 
"      - Made non-error results use echo rather than confirm()
"      - Added confirm-write for various commands.
"      - Side-effect Text-Diff no longer appears in a dialog (Pointed out by
"      Vikas Agnihotri)
" 1.10:
"      - Fixed a problem with 'Add' preventing it from working.
"      - Removed a few problem points where the project filename wasn't found.
"      - Fix check for ss.exe
"      - Add check for ss.exe in the common directory
" 1.11:
"      - Fix up commands on files in the root directory 
"      - Made sure Status command doesn't show extra messages while being
"        created (Spotted by Z.B.)
"     Fixes from Zak
"      - Added ssHistVertical option.
"      - Fixed menus for checkin similar
"      - Fix checking ssExecutable when using $SS
"      - Allow $SS to have spaces - make script to the hard work.
"      - Set/restore showcmd while doing SPLocked
"      - Fix up SPLocked for filenames with spaces.
" 1.12:
"      - Remove extra cr's when reporting sourcesafe results.
"      - Add '!' for Get to do a recursive get.
"      - Add VDiff to allow a Vertical diff using the smart-diff code
"      - Recognise URL's quickly to prevent loops- reported by Paul Bossi
"      - Recognise scratch buffers 
"      - Allow quick status on directories.
" 1.13:
"      - Add interactive mode for answering multiple questions.
"      - Fix up checking of modified status & automatic reloading of status's
" 1.14:
"      - Fix up system call with $VIMRUNTIME containing spaces. -Zak Beck
"      - Make Get prompt to overwrite a read-only file.
"      - Remove calls to ShortName where possible. (VSS shortname is not
"        always the same as M$oft shortname anyway!)
"      - Don't do Diff against project - causes wacky behaviour.
" 1.15:
"      - Use 'cat' if &shell contains 'sh'
"      - Explain $SSDIR & #include in doco.
" 1.16:
"      Fixes for William D. Bartholomew
"      - Add ssDeployFile for the ability to deploy a file by default.
"      - Fix space in historybuffer temp name.
"      


" Set up defaults (don't override if they already exist)
fun! SSDlet(lhs,rhs)
  if a:lhs !~ '^[gs]:' | let lhs='g:'.a:lhs | else | let lhs=a:lhs | endif
  if !exists(lhs) | exe 'let '.lhs.' = "'.escape(a:rhs,"\\\"").'"' | endif
endfun

call SSDlet('ssMenuPath', '&VSS')   " The full menu path prefix for VSS
call SSDlet('ssMenuPlace', '40')   " The order specifier prefix for menus
call SSDlet('ssDiffVertical', 1)   " Whether diff is vertical or horizontal
call SSDlet('ssHistVertical', 1)   " Whether history is vertical or horizontal
call SSDlet('ssSetRuler', 1)       " Whether to set the ruler option
call SSDlet('ssMaintainStatus', 1) " Maintain the buffers status as possible.
call SSDlet('ssShowAllLocks', 1)   " Show all locks on the status line
call SSDlet('ssShowExtra', 1)      " Show extra status information
call SSDlet('ssUserName', $USERNAME) " Username for Current user of Sourcesafe
call SSDlet('ssDeployFile', 0)    "Allow deployment of files.

delfun SSDlet

" " \sf Diff
" map <localleader>sf :<c-u>call <SID>DoSrcSafe(0, v:count, 'Diff', expand('%'))<cr>
" " \sF Close srcsafe diff
" map <localleader>sF :<c-u>call <SID>ClearDiffMode()<cr>
" " \sk Checkout
" map <localleader>sk :<c-u>call <SID>DoSrcSafe(0, v:count, 'Checkout', expand('%'))<cr>
" " \s<c-U> Uncheckout
" map <localleader>s<C-U> :<c-u>call <SID>DoSrcSafe( 0,v:count, 'UnCheckout', expand('%'))<cr>
" 
" " \sg Get
" map <localleader>sg :<c-u>call <SID>DoSrcSafe(0, v:count, 'Get', expand('%'))<cr>
" 
" " \s<c-k> Checkout (No Get)
" map <localleader>s<C-K> :<c-u>call <SID>DoSrcSafe(0, v:count, 'Lock', expand('%'))<cr>
" " \su Update
" map <localleader>su :<c-u>call <SID>DoSrcSafe(0, v:count,  'Update', expand('%'))<cr>
" " \ss Status
" map <localleader>ss :<c-u>call <SID>DoSrcSafe(0, v:count,  'Status', expand('%'))<cr>
" " \sh History - last n (default 3)
" map <localleader>sh :<c-u>call <SID>SSCmd( 'History -#'.((v:count)?(v:count):3), expand('%'), 0, '')<cr>
" " \sH History window
" map <localleader>sH :<c-u>call <SID>DoHistoryWithSyntax( expand('%'))<cr>
" " \sG Merge get
" map <localleader>sG :<c-u>call <SID>DoSrcSafe(0, v:count, 'Merge', expand('%'))<cr>
" " \sA Add file
" map <localleader>sa :<c-u>call <SID>DoSrcSafe(0, v:count, 'Add',  expand('%'))<cr>
" " \sd normal srcsafe diff
" map <localleader>sd :<c-u>call <SID>DoSrcSafe(0, v:count, 'SSDiff', expand('%'))<cr>
" " \\u Update (same comment)
" map <localleader><localleader>u :call <SID>DoSrcSafe(0, v:count, 'RepeatUpdate', expand('%'))<cr>

fun! s:CheckTemp()
  if !isdirectory($TEMP)
    call confirm("Your $TEMP directory doesn't exist!\n'".$TEMP."'")
  else
    if !isdirectory($TEMP.'\ss')
      call system( 'mkdir "'.$TEMP.'\SS"')
    endif
  endif
endfun

fun! s:Success( msg )
  if a:msg =~ '^\s*$'
    echo 'srcsafe: Success'
  else
    echo 'srcsafe: '.substitute(a:msg,"[\n\r ]*$",'','')
  endif
endfun

fun! s:CheckOverWrite(bufname)
  let destbuf=bufnr(a:bufname)
  if destbuf!=-1
    " make sure we're ok to overwrite a modified file!
    if getbufvar( a:bufname, '&modified' )
      return confirm('File Modified:'. a:bufname."\nContinue?", "&Ok\n&Cancel", 1) == 1
    endif
  endif
  return 1
endfun


fun! s:CheckWrite(bufname)
  " make sure we're ok to write a modified file!
  let destbuf=bufnr(a:bufname)
  if destbuf!=-1
    " make sure we're ok to overwrite a modified file!
    if getbufvar( a:bufname, '&modified' )
      let res=confirm('File Modified:'. a:bufname."\nWrite?", "&Ok\n&Ignore\n&Cancel", 1) 
      if res==3 || res==0
        return 0
      endif
      let cur=winnr()
      let dest=bufwinnr(destbuf)
      exe dest.'winc w'
      if res==1
        write
      elseif res==2
        edit!
      endif
      exe cur.'winc w'
    endif
  endif
  return 1
endfun

fun! s:DoSrcSafe(bang, count, cmd, ... )
  if a:0==0
    let c=1
    let f1=expand('%')
    let is_me=1
  else
    let c=1
    while c <= a:0 
      let f{c}=a:{c}
      let c=c+1
    endwhile
    let c=a:0
    let is_me=0
  endif
    
  let update_status=0
  let i=1
  if a:cmd=~?'\<D\%[iff]\>'               " D^iff
"    while i <= c
    call s:SSDiff(f{i},a:count)
    let i=i+1
"    endwhile
  elseif a:cmd=~?'\<SSD\%[iff]\>'         " SSD^iff
    while i <= c
      if s:CheckWrite(f{i})
        call s:SSCmd('Diff', f{i}, a:count, 'd')
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<C\%[heckout]\>'       " C^heckout
    while i <= c
      if s:CheckOverWrite(f{i})
        call s:SSCmd('Checkout -GWA', f{i}, a:count,  'o')
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<L\%[ock]\>'           " L^ock
    while i <= c
      call s:SSCmd('Checkout -G-', f{i}, a:count,  'o')
      call s:UpdateStatusFor( f{i}, 0 )
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<Unc\%[heckout]\>'     " Unc^heckout
    while i <= c
      if s:CheckWrite(f{i})
        call s:SSCmd('UnCheckout ', f{i}, a:count,  'o')
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<G\%[et]\>'            " G^et
    while i <= c
      if s:CheckOverWrite(f{i})
        if a:bang.'' == '!'
          call s:SSCmd('Get -R ', f{i}, a:count,  'o')
        else
          call s:SSCmd('Get -GWA', f{i}, a:count,  'o')
        endif
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<M\%[erge]\>'          " M^erge
    while i <= c
      if s:CheckWrite(f{i})
        call s:SSCmd('Get -GWM', f{i}, a:count, 'o')
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<U\%[pdate]\>'         " U^pdate
    while i <= c
      if s:CheckWrite(f{i})
        call s:SSUpdate( f{i}, '', '')      
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
    let update_status=1
  elseif a:cmd=~?'\<Re\%[peatUpdate]\>'   " Re^peatUpdate
    while i <= c
      if s:CheckWrite(f{i})
        call s:UpdateSimilar(f{i},'')
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<RepeatUpdateL\%[ocked]\>' " RepeatUpdateL^ocked
    while i <= c
      if s:CheckWrite(f{i})
        call s:UpdateSimilar(f{i},'-K')
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<RawS\%[tatus]\>'          " RawS^tatus
    while i <= c
      call s:SSCmd( 'Status', f{i},0, '')
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<S\%[tatus]\>'         " S^tatus
    while i <= c
      call confirm( s:GetStatus(f{i}, 0, 1, 1)) 
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<Sum\%[mary]\>'        " Sum^mary
    while i <= c
      call s:SSCmd( 'History -#'.((a:count)?(a:count):3), f{i}, 0, '')
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<H\%[istory]\>'        " H^istory
    while i <= c
      call s:DoHistoryWithSyntax( f{i})
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<A\%[dd]\>'            " A^dd
    while i <= c
      if s:CheckWrite(f{i})
        call s:SSCmdAdd( f{i})
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd=~?'\<UpdateL\%[ocked]\>'   " UpdateL^ocked
    while i <= c
      if s:CheckWrite(f{i})
        call s:SSUpdate( f{i}, '', '-K')
        call s:UpdateStatusFor( f{i}, 0 )
      endif
      let i=i+1
    endwhile
  elseif a:cmd =~?'\<Dep\%[loy]\>'      " Dep^loy
    if is_me && !g:ssDeployFile
      let f1=expand('%:h') " Directory, not file
    endif
    while i <= c
      call s:SSCmd('Deploy'.((a:bang.'' =='!')?' -R': ''), f{i}, a:count, '')
      let i=i+1
    endwhile
  elseif a:cmd =~?'\<Dir\%[ectory]\>'   " Dir&etory
    if a:0==0
      let f1=getcwd()
      let c=1
    endif
    while i <= c
      call s:SSCmd('Dir'.((a:bang.'' =='!')?' -R': ''), f{i}, a:count, '')
      let i=i+1
    endwhile
    "let ff=''
    "while i<= c
    "  let ff=ff.' '
    "  if f{i} =~ ' '
    "    let ff=ff.'"'.f{i}.'"'
    "  else
    "    let ff=ff.f{i}
    "  endif
    "  let i=i+1
    "endwhile
    "call input(ff)
    "call s:SSCmd('Dir'.((a:bang=='!')?' -R': ''), ff, a:count, '')
  else
      call confirm('SS: Unknown function :"'.a:cmd.'"')
  endif
endfun
fun! s:UpdateStatusFor( bufname, force )
  if !a:force || !g:ssMaintainStatus  | return | endif
  let cur=winnr()
  let destbuf=bufnr(a:bufname)
  if destbuf!=-1
    let dest=bufwinnr(destbuf)
    if cur!= destbuf | exe destbuf.'winc w' | endif
    call s:UpdateStatus()
    if cur!= destbuf | exe cur.'winc w' | endif
  endif
endfun

" The file where the comment is stored!
let s:commenttmp=$TEMP.'/ss/comment.@@@'
if !has('unix') && &shell !~ 'sh[a-z.]*$'
" && system('cat xxy__yzz') =~ "is not recognized"
    fun! s:Cat( filename )
        return system( 'type "'.substitute(a:filename,'/', '\\', 'g').'"')
    endfun
else
    fun! s:Cat( filename )
        return system( 'cat "'.a:filename.'"')
    endfun
endif

fun! s:UpdateSimilar(filename, extras)
  let comment=''
  if filereadable(s:commenttmp)
     let comment=substitute(substitute(s:Cat(s:commenttmp),'^\s*','',''),'\s*$','','')
  endif
  if comment=='' || comment=='.'
    call confirm('No previous comment!')
    return
  endif

"  let res = confirm("Reuse comment:\n\n".comment,"&Yes\n&No\n&Edit",1)
  let res = confirm("Reuse comment:\n\n".comment,"&Yes\n&No\n&Edit",1)
  if res ==1
    call s:SSUpdate( a:filename, 'r', a:extras)
  elseif res==3
    call s:SSUpdate( a:filename, 'e', a:extras)
  endif
endfun
fun! s:system(cmd,args)
"  echo '+'.a:cmd.'+'.a:args.'+'
  return system('""'.a:cmd.'" '.a:args.'"')
endfun

fun! s:SSUpdate( filename, copyold, extras)
  call s:CheckTemp()
  if filereadable(s:commenttmp)
    call rename(s:commenttmp,s:commenttmp.'.1')
  endif
  if a:copyold=='e'
    " Bring up the old comment to be edited
    call s:system($VIMRUNTIME.'\gvim.exe','-f -u NONE -U NONE -c "set go=aiMr co=30 lines=10"  -c "set nomod" -c "silent e '.s:commenttmp.'.1" -c "silent f %:r" -c "set mod"')
  elseif a:copyold=='r'
    " Reuse the old comment
      call rename(s:commenttmp.'.1', s:commenttmp)
  else
    " Create a new comment
    call s:system($VIMRUNTIME.'\gvim.exe',' -f -u NONE -U NONE -c "set go=aiMr co=30 lines=10" "'.s:commenttmp.'"')
  endif
  if filereadable(s:commenttmp)
    let cmt=s:Cat(s:commenttmp)
    if cmt != ''
      "call input(' update '.s:commenttmp.' '.a:filename)
      call s:SSCmd('Update '.a:extras.' -C@'.s:commenttmp.' ', a:filename,0, 'o')
      return
    endif
  endif
  echo 'Cancelled'
  if filereadable(s:commenttmp.'.1')
      call rename(s:commenttmp.'.1', s:commenttmp)
  endif
endfun


function! s:HistorySyntax()
 syntax region HistoryHeader start="^---" skip="^history" end="^" contains=HistoryFile
 syntax region HistoryFile matchgroup=HistoryFileLeader start="^history " matchgroup=NULL end="$"  contained
 syntax match HistoryVersion /\<[0-9]*\>/ contained
 syntax match HistoryField /\<[a-zA-Z]\+:/  
 syntax region HistoryVersionLine start="^\*\+" end="$" contains=HistoryVersion
" syntax match HistoryCommentLeader "Comment:" contained
" syntax match HistoryCommentTrailer "Comment:" contained
 syntax region HistoryComment matchgroup=HistoryCommentLeader start="^\(Label \)\=[cC]omment:" matchgroup=HistoryCommentTrailer end="^\*"he=s-1 contains=HistoryCommentLeader
 syntax region HistoryAction matchgroup=HistoryActionHead start="^\(Checked in\)\|\(Labeled\)" end="$"
endfunction 
highlight default HistoryVersionLine guifg=Maroon
highlight default HistoryVersion gui=bold guifg=Maroon
highlight default HistoryField gui=bold
highlight default HistoryActionHead gui=bold guifg=Blue 
highlight default link HistoryComment Comment 
highlight default link HistoryCommentLeader HistoryField
highlight default link HistoryCommentTrailer HistoryVersionLine
highlight default link HistoryAction String 
highlight default link HistoryHeader HistoryVersionLine
highlight default link HistoryFile String
highlight default link HistoryFileLeader HistoryActionHead

" Check a directory for the specified file
function! s:CheckDirForFile(directory,file)
    let aborted=0
    let cur=substitute(a:directory,'\\\+','/','g')
    while !filereadable(cur.'/'.a:file) && (aborted==0)
        if ( cur =~ '^\(.:\)\=//\=$') 
            let aborted=1
        elseif ( cur =~ '^//[^/]\+/[^/]\+$' )
            let aborted=2
        else
            let cur=fnamemodify(cur,':h')
        endif
    endwhile
    " Check the two cases we haven't tried
    if (aborted == 1) && filereadable(cur.a:file)
        let aborted=0
    elseif (aborted == 2) && filereadable(cur.'/'.a:file)
        let aborted=0
    endif
    if !(cur =~ '/$')
        let cur = cur.'/'
    endif 
    if aborted
      return ''
    else
      return cur
    endif
endfun


" Return the SS Filename spec.
" Assumes that a file called '.project' exists at the root of the tree.
fun! s:GetSSFile( filename )
  " Check for URLs - which usually get read by plugins.
  if a:filename =~ '^[a-z]\{2,10}:[/\\]\{2}'
    return ''
  endif

  let bufnr=bufnr(a:filename)
  " Speedup - make sure we aren't looking at a special buffer
  if bufnr >= 0 
    let buftype= getbufvar(bufnr, '&buftype')
    if buftype != '' && buftype != 'nowrite' | return '' | endif
  endif
      
  let fname=fnamemodify(a:filename,':p:gs+\\+/+')
  let projdir=s:CheckDirForFile(fnamemodify(fname, ':h'),'.project')
  if projdir==''
    if exists('g:ssLocalTree')
      let localtree=fnamemodify( g:ssLocalTree, ':p:gs+\\+/+')
      if localtree ==? strpart( fname, 0, strlen(localtree) )
        return '$'.strpart( fname, strlen(localtree))
      endif
    endif
    return ''
  endif
  let proj=projdir.'.project'
  let ssfilecont=s:Cat(proj)
  let ssfile=substitute(ssfilecont,"^[  \n\r]*".'\(.\{-}\)'."[  \n\r]*$",'\1','')
  if !exists('g:srcsafe_ssdir_orig')
    let g:srcsafe_ssdir_orig=$SSDIR
  endif

  if ssfile=~'^@'
    let ssproj=substitute(ssfile,'^@\(.*\)@$.*$','\1','')
    let ssfile=substitute(ssfile,'^@.*@\ze\$','','')
    let $SSDIR=substitute(ssproj,'\c[/\\]\k\+\.ini$','','')
  else
    let $SSDIR=g:srcsafe_ssdir_orig
  endif
  if ssfile !~ '[\\/]$'
    let ssfile=ssfile.'/'
  endif
  return '"'.ssfile.strpart(fname,strlen(projdir)).'"'
endfun

" Returns an SS Version string (when using 'count' before a command)
func! s:SSVersion(c)
  return ((a:c==0)?"": ("-V".a:c." "))
endf

fun! s:DiffBufHide( filename)
    if exists('g:srcsafe_diffbuff_difnr')
        let buf=bufnr(a:filename)
        if g:srcsafe_diffbuff_difnr==buf
            call s:ClearDiffMode()
        elseif exists('g:srcsafe_diffbuffnr') && g:srcsafe_diffbuffnr==buf
            let curbuf=winbufnr(winnr())
            if bufexists(buf)
                exe 'buf '.buf
                set diff
                call s:ClearDiffMode()
                exe 'buf '.curbuf
            else
                call s:ClearDiffMode()
            endif
        endif
    endif
endfun

augroup srcsafe
au!
au BufHidden,BufDelete,BufUnload * call <SID>DiffBufHide(expand('<afile>'))
au BufEnter * call <SID>ReclearDiffMode()
if g:ssMaintainStatus
  au BufRead * call <SID>UpdateStatus()
endif
aug END

fun! s:ReclearDiffMode()
  if exists('b:srcsafe_diffmode') && !b:srcsafe_diffmode && exists('b:srcsafe_diffbuffvars')
        exe b:srcsafe_diffbuffvars
        unlet b:srcsafe_diffbuffvars
  endif
endfun

" 
fun! s:ClearDiffMode()
    if exists('g:srcsafe_diffbuffnr')
        let diffbuffnr=g:srcsafe_diffbuffnr
        unlet g:srcsafe_diffbuffnr
        let win=bufwinnr(diffbuffnr)
        while winbufnr(win) != -1 && !getwinvar(win,'&diff')
            let win=win+1
            while winbufnr(win) != -1 && winbufnr(win) != diffbuffnr
                let win=win+1
            endwhile
        endwhile
        let curwin=winnr()
        if winbufnr(win)!= -1
            exe win.'winc w'
            exe b:srcsafe_diffbuffvars
            if &fdc==0
                norm zn
            endif
            exe curwin.'winc w'
        endif
        exe 'bdel '.g:srcsafe_diffbuff_difnr

    let b:srcsafe_diffmode=0
        "unlet b:srcsafe_diffbuffvars
        unlet g:srcsafe_diffbuff_difnr
    endif
endfun

fun! s:SetDiffSplit( ssfile )
    call s:ClearDiffMode()
    let diffs=''
    let win=1
    while winbufnr(win) != -1
        if getwinvar(win, '&diff')
            call setwinvar(win, '&diff',0)
            let diffs='|call setbufvar('.winbufnr(win).',"&diff",1)'
        endif
        let win=win+1
    endwhile
  let b:srcsafe_diffmode=1
    let b:srcsafe_diffbuffvars='set nodiff '. (&scb?'': 'no').'scb sbo='.&sbo.' '.(&wrap?'': 'no' ).'wrap fdm='.&fdm.' fdc='.&fdc.' '.(&fen?'': 'no').'fen '.(&scb?'': 'no').'scb fdl='.&fdl
    let g:srcsafe_diffbuffnr=winbufnr(winnr())
    exe (g:ssDiffVertical?'vert': '').' diffsplit '.a:ssfile
    let g:srcsafe_diffbuff_difnr=winbufnr(winnr())
endfun


" Do a SS difference using vim diff.
fun! s:SSDiff(filename, ssVer)
  "TODO: Handle diff on directories.
  if isdirectory(a:filename) || a:filename=='' | return | endif

  if !s:CheckSS() | return | endif
  call s:CheckTemp()
  let ssfile=$TEMP."\\ss\\". fnamemodify(a:filename, ':t')
  let prjfile=s:GetSSFile(a:filename)
  if prjfile.'' == ''
     echoerr 'Invalid project'
     return
  endif
  let result=s:system(g:ssExecutable,' Get -I-Y -GF- -GL'.$TEMP.'\ss '.s:SSVersion(a:ssVer).prjfile)
  if filereadable(ssfile) && result !~ "is not an existing" 
    
    call s:SetDiffSplit(ssfile)
    exe "norm \<C-W>\<C-X>"
  else
    echo confirm("Could not get file:\n".result,"OK",0)
  endif
endfun

fun! s:CheckSS()
  if !exists("g:ssExecutable")
    if !filereadable($SS)
      let SS='c:\Program Files\Microsoft Visual Studio\vss\win32\ss.exe'
      if !filereadable(SS)
        let SS='c:\Program Files\Microsoft Visual Studio\Common\vss\win32\ss.exe'
      endif
      if filereadable(SS)
        let g:ssExecutable=SS
      else
        echoe 'Set g:ssExecutable or $SS Variable correctly to point to SS.EXE'
        return 0
      endif
    else
        let g:ssExecutable=$SS
    endif
  endif
  return 1
endfun

fun! s:SSCmdAdd( filename)
  "Check for setup.
  if !s:CheckSS() | return | endif

  if !filereadable( a:filename )
    call confirm( "'".a:filename."' does not exist!")
    return
  endif

  let newproj=s:GetSSFile(fnamemodify(a:filename,':p:h'))
  if newproj.'' == ''
     echoerr 'Invalid project'
     return
  endif
  " Change project - so the file gets added to the correct one!
  let result=s:system( g:ssExecutable,' CP '.newproj )
  call s:Success( result)

  let result=s:system( g:ssExecutable,' Add -I-N '.a:filename)

  " Clear all the answered y/n questions for the prompt.
  let res=substitute(result,'(Y/N)N', '', 'g')
  if res ==result
    " Successful - finish.
    call s:Success(res)
    return
  else
    " Answer the questions
    if confirm(res, "&Yes\n&No", 1) != 1
        " Don't continue!
        return 
    endif
  endif

  let result=s:system(g:ssExecutable,' '.a:cmd.' -I-Y '.cmdargs)

  " Strip out all the questions answered:
  let result=substitute(substitute(result,"[^\n]*(Y/N)Y", '', 'g'),"\n\\+", "\n", 'g')
  call s:Success(result)
    
endfun

" Execute a sourcesafe command.
"  cmd - the command plus args.
"  filename - the local filename
"  ssVer  - the version in sourcesafe (or 0)
"  opts
"       f - to answer 'y' to all questions.
"       o - specify output directory
"       d - diff mode (specify standard filename as well)
fun! s:SSCmd(cmd, filename, ssVer, opts)
" force, hasoutput)
  let force=(a:opts=~'f')
  let hasoutput=(a:opts=~'o')
  let isdiff=(a:opts=~'d')

  "Check for setup.
  if !s:CheckSS() | return 0 | endif

  " make sure we're ok to discard a modified file!
  "if &modified && confirm('File Modified - discard?', "&Ok\n&Cancel", 1) != 1
  "  return
  "endif

  " Make sure the file gets output in the correct spot.
  let outpath=a:filename
  if !isdirectory(outpath)
    let outpath=fnamemodify(outpath, ':p:h')
  endif
    
  let ssfile = s:GetSSFile(a:filename)
  if ssfile.'' == ''
     return 0
  endif
  " There is some weirdness that causes the final quote in -GL"s:\" to be escaped!?
  " Make sure this doesn't happen
  let outpath=substitute(outpath,'\\$','\\\\','')
  " Build up the basic arguments.
  let cmdargs=(hasoutput?'-GL"'.outpath.'" ' : '').s:SSVersion(a:ssVer).ssfile

  if isdiff
    let cmdargs=cmdargs.' "'.a:filename.'"'
  endif

  let autoread=&autoread
  if !force
      " Try and do it once - answering 'no' to all questions.

      " Autoread the file
      setlocal autoread

      echo a:cmd
      " Perform the operation
      let result=s:system(g:ssExecutable,' '.a:cmd.' -I-N '.cmdargs)
      " Make sure we know it has been modified.
      checktime
      if !autoread
          setlocal noautoread
      endif

      " Clear all the answered y/n questions for the prompt.
      let res=substitute(result,'(Y/N)N', '', 'g')
      if res ==result
        " Successful - finish.
        "echo a:cmd.' -I-N '.cmdargs
        call s:Success(res)
        return 1
      else
        " Answer the questions
        if confirm(res, "&Yes\n&No", 1) != 1
            " Don't continue!
            return 0
        endif
      endif
  endif 
  " Try again / force 'yes' to questions.
  setlocal autoread
  let result=s:system(g:ssExecutable,' '.a:cmd.' -I-Y '.cmdargs)

  " Strip out all the questions answered:
  let result=substitute(substitute(result,"[^\n]*(Y/N)Y", '', 'g'),"\n\\+", "\n", 'g')
  "echo a:cmd.' -I-Y '.cmdargs
  call s:Success(result)
  checktime
  if !autoread
      setlocal noautoread
  endif
  return 1
endfun

fun! s:DoHistoryWithSyntax(filename)

  if !s:CheckSS() | return | endif

  let prjfile=s:GetSSFile(a:filename)
  if prjfile.'' == ''
     echoerr 'Invalid project'
     return
  endif

  let bname='History!'.fnamemodify(a:filename,':t:gs/ \./_/')
  let hbufnr=bufnr(bname)
  let hwinnr=bufwinnr(hbufnr)
  if hwinnr==-1
    exe 'rightbelow ' . (g:ssHistVertical ? 'v' : '') . 'new'
    if hbufnr==-1
        set buftype=nofile

        exec 'f '.bname
    else
        exec 'b '.hbufnr
    endif
  else
    exe hwinnr.'winc w'
  endif
  setlocal modifiable
  1,$d

  exec '.r !""'.g:ssExecutable.'" History "'.prjfile.'""'
  set nomodified
  setlocal nomodifiable
  call s:HistorySyntax()
  1
endfun


fun! s:DoCheckedOutFiles(bang, ...)
    let bang=a:bang.'' =='!'
    if a:0 == 0
        call s:CheckedOutFiles( getcwd(), '', bang)
    elseif a:0 == 1
        call s:CheckedOutFiles( a:1, '', bang)
    elseif a:0 == 2
        call s:CheckedOutFiles( a:1, a:2, bang)
    else
        echoerr 'SPLocked: Too many arguments!'
    endif

endfun

fun! s:CheckedOutFiles( project, user, recursive)
  if !s:CheckSS() | return | endif
  let tempfile=s:ShortName(tempname())
  let ssproj=s:GetSSFile(a:project)
  if ssproj.'' == ''
     echoerr 'Invalid project'
  endif
  let res=s:system( g:ssExecutable,' Status -I-N -NS -O'.tempfile.' '.(a:recursive?'-R ': '').((a:user=='*')?'':'-U'.a:user.' ').s:ShortName(ssproj))
  if !filereadable(tempfile)
    echoerr res
  endif
  new
  setlocal modifiable
  let showcmd_save=&showcmd
  let report_save=&report
  set noshowcmd
  set report=9999
  exe 'r '.tempfile
  call delete(tempfile)
  let l=1
  let currprj='$/'
  let thefirst=1
  if line('$')==1
    return
  endif
  while l<= line('$')
    exe l
    let txt=getline('.')
    if txt=~'^\s*$'
      d
      let l=l-1
    elseif txt =~ '^$.*'
      let currprj=substitute(txt,':$','','')
      d
      let l=l-1
      let rootlen=strlen(ssproj)
      if strpart(currprj,0,rootlen-1).'/' ==? ssproj
          let currprj=strpart(currprj,rootlen)
      endif
      let thefirst=0
    else
        s/^\([^ ]*\) \+\<\(\k\+\)\> \+\%(\(v[0-9]\+\) \+\)\=[0-9\/]* \+[0-9:pa]*  \+\(.*\)$/\=
\s:CreateInfoLine(currprj,submatch(4).'\'.submatch(1),submatch(2),submatch(3),a:user,a:recursive)/
    endif

    let l=l+1
  endwhile
  call append(0,ssproj.':')
  call append(1,'')
  let ssprojn=substitute(substitute(ssproj, '^"\(.*\)"$', '\1', ''),'[^a-zA-Z0-9]', '_', 'g')
  let bufname='srcsafe:'.ssprojn
  let idx=1
  while bufnr(bufname) != -1
    if bufwinnr(bufnr(bufname))==-1
        exe 'bdel '.bufname
        break
    endif
    let bufname='srcsafe'.idx.':'.ssprojn
    let idx=idx+1
    if idx >10 
        let bufname='??'.winbufnr(winnr())
    endif
  endwhile
  silent exe 'f '.bufname
  let &showcmd=showcmd_save
  let &report=report_save
  set bt=nofile nomodified
  setlocal nomodifiable
  call s:ExplorerMode()
endfun


fun! s:CreateInfoLine( currentproj, filename, user, fversion, which_user, recurse)
    let fname=s:ExpandFile( a:filename)
    let brief=fnamemodify(fname, ':t')
    let briefcol=30
    if a:recurse
        if a:currentproj== '' || a:currentproj =~ '/$'
            let brief=a:currentproj.brief
        else
            let brief=a:currentproj.'/'.brief
        endif
        let briefcol=50
    endif
    if a:which_user == '*'
        let b:filenamecolumn=(briefcol+5+10)
        return escape(substitute(TabCollumns(briefcol,brief, 'r5',a:fversion,10,a:user, 0,fname),'\s*$','',''),'\&')
    else
        let b:filenamecolumn=(briefcol+5)
        return escape(substitute(TabCollumns( briefcol,brief, 'r5', a:fversion,0,fname ),'\s*$','',''),'\&')
    endif
endfun


fun! s:ExpandFile( filename)
    let fname = expand(a:filename)
    if fname==''
        return a:filename
    endif
    return fname
endfun

fun! s:Spaces( n)
    let n=a:n
    let ret=''
    while n>0
        let ret=ret.' '
        let n=n-1
    endwhile
    return ret
endfun

fun! TabCollumns( ...)
    let c=1
    let ret=''
    while c+1 <= a:0
        let txt=a:{c+1}
        let txtlen=strlen(txt)
        let rjust=a:{c}[0] ==?'r'
        if rjust
            let width=strpart(a:{c},1)
        elseif a:{c} == 0
            let ret=ret.a:{c+1}
            let c=c+2
            continue
        else
            let width=a:{c}
        endif
        if txtlen>=width
            if width>5
                let txt='..'.strpart(txt,(txtlen-width)+3)
                let txtlen=width-1
            elseif width <= 2
                let txt=strpart(txt,0,width)
                let txtlen=width
            else
                let txt=strpart('.....',0,width-1)
                let txtlen=width-1
            endif
        endif
        let diff=width-txtlen
        let prespace =s:Spaces( rjust?((width>2)?(diff-1):(diff)):0)
        let postspace=s:Spaces( rjust?((width>2)?1:0):(diff))

        let ret=ret.prespace.txt.postspace
        let c=c+2
    endwhile
    return ret
endfun

fun! s:ExplorerMode()
  nnoremap <script> <buffer> <CR> :call <SID>OpenFile('')<CR>
  nnoremap <script> <buffer> o :call <SID>OpenFile('o')<CR>
  nnoremap <script> <buffer> O :call <SID>OpenFile('O')<CR>
  nnoremap <script> <buffer> p :call <SID>OpenFile('p')<CR>
  nnoremap <script> <buffer> f :call <SID>OpenFile('f')<CR>
  nnoremap <script> <buffer> <localleader>sf :call <SID>OpenFile('f')<CR>
  nnoremap <script> <buffer> K :call <SID>OpenFile('K')<CR>
  nnoremap <script> <buffer> <localleader>sk :call <SID>OpenFile('K')<CR>
  nnoremap <script> <buffer> L :call <SID>OpenFile('L')<CR>
  nnoremap <script> <buffer> <localleader>s<c-k> :call <SID>OpenFile('L')<CR>
  nnoremap <script> <buffer> <localleader>sl :call <SID>OpenFile('L')<CR>
  nnoremap <script> <buffer> u :call <SID>OpenFile('u')<CR>
  nnoremap <script> <buffer> <localleader>su :call <SID>OpenFile('u')<CR>
  nnoremap <script> <buffer> <localleader>sU :call <SID>OpenFile('ul')<CR>
  nnoremap <script> <buffer> U :call <SID>OpenFile('U')<CR>
  nnoremap <script> <buffer> <localleader><localleader>u :call <SID>OpenFile('U')<CR>
  nnoremap <script> <buffer> <localleader><localleader>U :call <SID>OpenFile('Ul')<CR>
endfun

"
fun! s:OpenFile( openmode )
  if !exists('b:filenamecolumn')
    call confirm('No column width specified!??')
    return
  endif
  let fname=strpart(getline('.'),b:filenamecolumn)
  if !filereadable(fname)
    call confirm("Can't find: '".fname."'")
    return
  endif
  if a:openmode==''
    exe 'e '.fname
  elseif a:openmode==#'O'
    exe 'new '.fname
  elseif a:openmode==#'o'
    let win=bufwinnr(bufnr(fname)) 
    if win != -1
      exe win.'winc w'
    else
      exe 'new '.fname
    endif
  elseif a:openmode==#'p'
    exe 'pedit '.fname
  elseif a:openmode==#'f'
    let win=bufwinnr(bufnr(fname)) 
    if win != -1
      exe win.'winc w'
    else
      exe 'new '.fname
    endif
    call s:DoSrcSafe('',v:count, 'Diff' )
  elseif a:openmode==#'K'
    call s:DoSrcSafe('',v:count, 'Checkout', fname )
  elseif a:openmode==#'L'
    call s:DoSrcSafe('',v:count, 'Lock', fname )
  elseif a:openmode==#'u'
    call s:DoSrcSafe('',v:count, 'Update', fname )
  elseif a:openmode==#'U'
    call s:DoSrcSafe('',v:count, 'RepeatUpdate', fname )
  elseif a:openmode==#'ul'
    call s:DoSrcSafe('',v:count, 'UpdateLocked', fname )
  elseif a:openmode==#'Ul'
    call s:DoSrcSafe('',v:count, 'RepeatUpdateLocked', fname )
  endif
endfun

" Coppied from the scripts of David Eggum.
function! SSGetLockStatus()
   if exists("b:checked_out_status")
      return b:checked_out_status
   else
      return ""
   endif
endfunction
" Coppied from the scripts of David Eggum.
function! SSLockStat()
   if exists("b:checked_out_status_brief")
      if b:checked_out_status_brief != ''
        return '<'.b:checked_out_status_brief.'>'
      else
        return b:checked_out_status_brief
      endif
   else
      return ""
   endif
endfunction

if (strlen(&rulerformat) == 0) && (ssSetRuler == 1)
   set rulerformat=%60(%=%{SSGetLockStatus()}%)\ %4l,%-3c\ %3p%%
endif 

function! s:GetStatus(filename, include_brief, ShowExtra, ShowAllLocks)
  if !s:CheckSS() | return "No VSS\n#" | endif
  let ssname = s:GetSSFile(a:filename)

  if ssname.'' == '' 
    return "Not in VSS!".((a:include_brief)?"\n": '')
  endif

  let sBrief=''

  let sCmd = "Status -I-N ".ssname

  let sFull = s:system(g:ssExecutable, sCmd)
  let sLine = sFull
  if (match(sFull,"No checked out files found.") == 0)
    return "Not Locked".((a:include_brief)?"\n@":'')
  elseif (match(sFull,"is not valid SourceSafe syntax") != -1 || 
\          match(sFull,"is not an existing filename or project") != -1 ||
\          match(sFull,"has been deleted") != -1)
    return "Not in VSS".((a:include_brief)?"\n":'')
  elseif (strlen(sFull) == 0)
    return ((a:include_brief)?"\n": "")
  endif

   "Slightly different mode for directory
   let is_dir=isdirectory( a:filename)
   let sep = (is_dir?"\n":', ')
   " Quirk: VSS truncates files over 19 characters long
   let file = (is_dir?'':(strpart(expand("%:t"),0,19)))
   let sUsers = ""
   let sStatus = ""
   let sBrief = ''
   let nCount = 0
   let bMyLock =0
   while (strlen(sLine) != 0)
      let sMatch = matchstr(sLine,".\\{-1,}\n")
      if match(sMatch,'\c^srcsafe:')!=-1 || match(sMatch, '^\s') != -1 || match(sMatch,'^\$') != -1
        "
      elseif is_dir || match(sMatch,'\c^'.file) == 0
        if is_dir 
          let file = substitute(matchstr(sMatch, '^.\{18}'),'\s*$','','') 
        endif
        let sUser = matchstr(sMatch, ' \@<=\w\+')
        let bExclusive = match(sMatch,'\w\+\s\+\w\+\s\+Exc') > -1
        let bIsMe = sUser ==? g:ssUserName
        let sOld = matchstr(sMatch,'\(\w\+\s\+\w\+\s\+v\)\@<=\d\+')
        let bOld = (sOld!='')
        let sExtras = ''

        if bExclusive
          let sBrief=(bIsMe? 'X' : 'x')
        else
          let sBrief=sBrief.(bIsMe? (bOld? 'O' : 'L') : 'l')
        endif

        if a:ShowExtra
          let sExtras = (bExclusive?' (exc)': '').(bOld? ('('.(bIsMe?sOld : 'old').')'): '')
        endif
        if is_dir
          let sUser = file.":\<tab>".sUser
        endif
        if a:ShowAllLocks == 1 || is_dir
           let sUsers = sUsers.((nCount==0)?'':sep).sUser.sExtras
        else
          let bMyLock = bMyLock || bIsMe
          if nCount==0
            let sUsers = sUser.sExtras
          endif
        endif
        let nCount=nCount+1
        if bExclusive | break | endif
      endif

      let iLen = strlen(sMatch)
      let sLine = strpart(sLine,iLen,strlen(sLine)-iLen)
   endwhile

   if a:ShowAllLocks || is_dir
     if strlen(sUsers) > 0
        let sStatus=(is_dir?"Locks:\n": 'Locked by ').sUsers
     endif
   else
     let sStatus=(bMyLock?"Locked":("Locked by ".sUsers))
     let sStatus=sStatus.((nCount>1)?'..':'')
   endif

   if nCount == 0
     echom "VSS plugin: Unrecognised output:" sFull
   endif

   return sStatus.((a:include_brief)?("\n".sBrief): '')
endfun

" get the current lock status from VSS and place it in b:checked_out_status
function! s:UpdateStatus()
  let result=s:GetStatus(@%,1,g:ssShowExtra, g:ssShowAllLocks)
  let b:checked_out_status=matchstr(result,"^[^\n]*")
  let b:checked_out_status_brief=matchstr(result,"\n\\@<=.*$")
endfunction

fun! s:pushLeader()
    let restore=''
    if exists("g:ssMapLeader")
      if exists("g:maplocalleader")
        let restore='let g:maplocalleader="'.escape(maplocalleader,"\\".'"'
      else
        let restore='unlet maplocalleader'
      endif
      let maplocalleader=g:ssMapLeader
    endif
    return restore
endfun

" Get the short-form-name of a filename (the whole directory path)
fun! s:ShortName( filename )
	let fn=a:filename
	if !isdirectory(fn) && ! filereadable(fn)
		return fn
	endif

	let base=fnamemodify(fn,':h')
	let file=fnamemodify(fn,':t')
	if base==fn
		return base
	endif
	let base=s:ShortName( base )
	if base !~ '[/\\]$'
		let base=base.'\'
	endif
	if isdirectory(base)
		if file=~' ' || file =~ '\..*\.' || file =~ '\.[^.]\{4,}$' || file=~'^[^.]\{9}'

			let short=substitute(file,' ', '', 'g')
			let ext=matchstr(short,'\.[^\.]\{3}\([^\.]*$\)\@=')
			let short=substitute(short,'\..*$','','')
			
			let orig=expand(base.file,':p')
			if orig != ''
				let c=1
				while c<20
					let file=strpart(short,0,(7-strlen(c))).'~'.c.ext
					if expand( base.file,':p' ) == orig
						break
					endif
					let c=c+1
				endwhile
			endif
		endif
	endif

	return base.file
endfun

fun! s:addMenuMapping( item, desc, keys, mapping) 
    if !exists('s:ssMenuWhere') | let s:ssMenuWhere = 0 | endif
    let s:ssMenuWhere=s:ssMenuWhere+5
    let menu_desc=g:ssMenuPlace.'.'.s:ssMenuWhere.' '.escape(g:ssMenuPath.'.'.a:item,'\   ')
    if a:item=~ '^-.*-$'
      if has('menu') | exe 'menu '.menu_desc.' <nul>' | endif
      return
    endif
    if a:keys==''
      " menu only mapping
      if has('menu') | exe 'menu '.menu_desc.' '.a:mapping | endif
      return
    endif 
    let restore=s:pushLeader()
    if exists('g:maplocalleader')
      let leaderdesc=g:maplocalleader
    else
      let leaderdesc='\'
    endif
    let keys='<localleader>'.a:keys
    let keysdesc=escape(substitute(keys,'<localleader>',escape(leaderdesc,"\\"), 'g'),"\\")

    if !hasmapto( a:mapping)
      exe 'nnoremap '.keys.' '.a:mapping
    endif
    if has('menu') | exe 'amenu '.menu_desc.'<tab>'.a:desc.keysdesc.' '.keys | endif

    exe restore
endfun

command! -complete=file -bang -nargs=+ -count=0 SS call s:DoSrcSafe(<q-bang>,<count>,<f-args>)
command! -complete=dir -bang -nargs=* -count=0 SDeploy call s:DoSrcSafe(<q-bang>,<count>, 'Deploy',<f-args>)
command! -complete=dir -bang -nargs=* -count=0 SPLocked call s:DoCheckedOutFiles(<q-bang>, <f-args>)

command! -complete=file -nargs=1 -count=0 VDiff call s:SetDiffSplit(<f-args>)


let s:ssMenuWhere=0
"  call s:addMenuMapping('Chec&kout', '{count}', 'sk', )
"
call s:addMenuMapping('Check&in', '', 'su', ':<c-u>call <SID>DoSrcSafe(0, v:count,  "Update", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SUpdate call s:DoSrcSafe(<q-bang>,<count>, 'Update',<f-args>)

call s:addMenuMapping('Checkin Simila&r', '', '<localleader>u', ':<c-u>call <SID>DoSrcSafe(0, v:count, "RepeatUpdate", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SRepeatUpdate call s:DoSrcSafe(<q-bang>,<count>, 'RepeatUpdate',<f-args>)


call s:addMenuMapping('Checkin &Locked', '', 'sU', ':<c-u>call <SID>DoSrcSafe(0, v:count,  "UpdateLocked", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SUpdateLocked call s:DoSrcSafe(<q-bang>,<count>, 'UpdateLocked',<f-args>)
call s:addMenuMapping('Checkin Similar Locked', '', '<localleader>U', ':<c-u>call <SID>DoSrcSafe(0, v:count, "RepeatUpdateLocked", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SRepeatUpdateLocked call s:DoSrcSafe(<q-bang>,<count>, 'RepeatUpdateLocked',<f-args>)

call s:addMenuMapping('&Uncheckout', '', 's<c-u>',
\  ':<c-u>call <SID>DoSrcSafe( 0,v:count, "UnCheckout", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SUncheckout call s:DoSrcSafe(<q-bang>,<count>, 'Uncheckout',<f-args>)

call s:addMenuMapping('-s5-', '', '', '')
call s:addMenuMapping('Directory', '', 's.', 
\  ':<c-u>call <SID>DoSrcSafe(0, v:count, "Directory", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SDirectory call s:DoSrcSafe(<q-bang>,<count>, 'Directory',<f-args>)

call s:addMenuMapping('S&tatus', '', 'SS',
\  ':<c-u>call <SID>DoSrcSafe(0, v:count,  "RawStatus", expand("%"))<cr>')

call s:addMenuMapping('&Show Status','', 'ss', ':<c-u>call <SID>DoSrcSafe(0, v:count, "Status", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SStatus call s:DoSrcSafe(<q-bang>,<count>, 'Status',<f-args>)

call s:addMenuMapping('&History', '{count}', 'sh', ':<c-u>call <SID>SSCmd( "History -#".((v:count)?(v:count):3), expand("%"), 0, "")<cr>')
command! -complete=file -bang -nargs=* -count=0 SSummary call s:DoSrcSafe(<q-bang>,<count>, 'Summary',<f-args>)

call s:addMenuMapping('&View history', '{count}', 'sH', ':<c-u>call <SID>DoHistoryWithSyntax( expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SHistory call s:DoSrcSafe(<q-bang>,<count>, 'History',<f-args>)

call s:addMenuMapping('-s4-', '', '', '')

call s:addMenuMapping('&Text Diff', '{count}', 'sd', ':<c-u>call <SID>DoSrcSafe(0, v:count, "SSDiff", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SDiffSS call s:DoSrcSafe(<q-bang>,<count>, 'SSDiff',<f-args>)

call s:addMenuMapping('&Diff', '{count}', 'sf', ':<c-u>call <SID>DoSrcSafe(0, v:count, "Diff", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SDiff call s:DoSrcSafe(<q-bang>,<count>, 'Diff',<f-args>)

call s:addMenuMapping('C&lose', '', 'sF', ':<c-u>call <SID>ClearDiffMode()<cr>')
command! -nargs=0 SDiffClose call s:ClearDiffMode()

call s:addMenuMapping('-s3-', '', '', '')

call s:addMenuMapping('&Get', '{count}', 'sg', ':<c-u>call <SID>DoSrcSafe(0, v:count, "Get", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SGet call s:DoSrcSafe(<q-bang>,<count>, 'Get',<f-args>)

call s:addMenuMapping('&Merge', '', 'sG', ':<c-u>call <SID>DoSrcSafe(0, v:count, "Merge", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SMerge call s:DoSrcSafe(<q-bang>,<count>, 'Merge',<f-args>)

call s:addMenuMapping('-s2-', '', '', '')

call s:addMenuMapping('&Add', '', 'sa', ':<c-u>call <SID>DoSrcSafe(0, v:count, "Add",  expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SAdd call s:DoSrcSafe(<q-bang>,<count>, 'Add',<f-args>)

call s:addMenuMapping('Check&out', '{count}', 'sk', ':<c-u>call <SID>DoSrcSafe(0, v:count, "Checkout", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SCheckout call s:DoSrcSafe(<q-bang>,<count>, 'Checkout',<f-args>)

call s:addMenuMapping('Checkout-&No get', '', 's<c-k>', ':<c-u>call <SID>DoSrcSafe( 0,v:count, "Lock", expand("%"))<cr>')
command! -complete=file -bang -nargs=* -count=0 SLock call s:DoSrcSafe(<q-bang>,<count>, 'Lock',<f-args>)

call s:addMenuMapping('-s1-', '', '', '')

call s:addMenuMapping('&Explore', '','',':call <SID>SSRun("ssexp.exe")<CR>')
call s:addMenuMapping('Admin', '','',':call <SID>SSRun("ssadmin.exe")<CR>')

fun! s:SSRun( prog)
  if !s:CheckSS() | return | endif
  exe '!start "'.fnamemodify(g:ssExecutable,':h').'\'.a:prog
endfun

" vim: ts=2 et sw=2
