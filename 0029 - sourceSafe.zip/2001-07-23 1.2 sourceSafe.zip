" Author: David Eggum <davide@simutech.com>
" Last Update: Jul 23 2001
" Version: 1.2
" 
" sourceSafe.vim - Interfaces with the MS VSS command line.  This script works
" on the current file opened and is meant to only perform frequently used
" tasks such as checkin, checkout, get current file, check differences and so
" on.  The current lock status can also be displayed in the vim window;
" examples of placing it in the ruler are provided below.
" 
" Setup: Two settings under the "CONFIGURATION" section below must first be
" customized for your environment.  The rest of the settings are cosmetic.
"
" Usage: See the "COMMANDS" section below for how to use this script.  
"
" Updates:
" 1.2 - Diffing the file with the latest version in VSS now takes advantage of
"       Vim's built-in diff feature.
"     - Added new lock status mode. (Daniel Einspanjer)
"     - Added extra lock information (old) and (exc). (Daniel Einspanjer)
"     - Added "!" (bang) switch to all commands for raw interaction with VSS.
" 1.1a- Fixed hang with Undocheckout when the current file has changed.
" 1.1 - The current lock status is now available from the VSSGetStatus()
"       function.  It looks really good in the ruler!  Samples are provided
"       below.  Also see :help rulerformat.
"     - Command responses are now echo'ed to the screen.
"     - Added silent flag (Daniel Einspanjer)
" 1.0 - Initial release

" Todo:
"  - show lock status in Select Buffer list
"
" Send any improvements you would like to have included!

" --- CONFIGURATION ---

" Change this environment variable to the path of your VSS database.  The path to your
" database can be found in VSS under File->Open SourceSafe Database.
let $SSDIR="//gamma/swdev/src/jrave"


" Files are referenced in VSS like this:
"
" $/the/path/to/your/file
"
" So in order to match the path of the local file to the format above, the top-level
" directory must be changed to "$".
" For example, if the current file is
"
"   C:/dev/project/main.c
"
" then to check the status of the currently opened file the following VSS
" command would need to be sent
"
"   ss Status $/project/main.c
"
" The following substitue pattern will change to "$" for these commands, so
" change this setting to the top level of your local tree.
" let vssTree="C:\\\\dev"
"  or simply:
" let vssTree="C:.dev"
" 
" This also works well:
let vssTree=".*dev[0-9.]*"
" 
" NOTE: These do not work:
" let vssTree="C:\dev"
" let vssTree="C:\\dev"
" let vssTree="C:/dev"

" The following flag will change the behavior of VSSGetStatus() to show
" all users who have the file checked out, or only your status.
let ssShowAllLocks=1

" Change this to your username.  Note that VSS is case sensitive!  This
" setting is used when ssShowAllLocks=0
let ssUserName="Davide"

" set to 1 to display outdated (old) or exclusive (exc) lock status.
let ssShowExtra=0

" When using the built-in diff feature, set this to 1 to split the windows
" vertically, set to 0 to split horizontally.
let ssDiffVertical=1

" The current lock status is available from the VSSGetStatus() command.  A
" really useful place to put this info is in the ruler, like so: 
"
" set rulerformat=%{VSSGetStatus()}
"
" The current lock status is updated when the file opens and after every :SS
" command (see COMMANDS section below)
"
" The following works well; change it to your liking.
set rulerformat=%60(%=%{VSSGetStatus()}%)\ %4l,%-3c\ %3p%%

" --- END CONFIGURATION ---

au! BufRead * SSStatus

let b:checked_out_status = ""

function s:GetSSName(filename)
   let ssfile = substitute(a:filename,g:vssTree,"\$","")
   return substitute(ssfile,"\\","/","g")
endfunction

function VSSGetStatus()
   if exists("b:checked_out_status")
      return b:checked_out_status
   else
      return ""
   endif
endfunction

let g:vss_debug = 0

" get the current lock status from VSS and place it in b:checked_out_status
function s:UpdateStatus(bang,cmd_args)
   let sCmd = "ss Status ".a:cmd_args." ".s:GetSSName(expand("%:p"))
   if a:bang == "!" " Raw VSS interaction
      exec "!".sCmd
      return
   endif

   let sFull = system(sCmd)
   let sLine = sFull
   if (match(sFull,"No checked out files found.") == 0)
      let b:checked_out_status = "Not Locked"
      return b:checked_out_status
   elseif (match(sFull,"is not valid SourceSafe syntax") != -1 || match(sFull,"is not an existing filename or project") != -1)
      " may occur when a file is checked that is not in the top
      " level of the development tree
      let b:checked_out_status = "Not in VSS"
      return b:checked_out_status
   elseif (strlen(sFull) == 0)
      let b:checked_out_status = ""
      return ""
   endif

   " Quirk: VSS truncates files over 19 characters long
   let file = strpart(expand("%:t"),0,19)
   call s:printd("file: ".file."\n")
   let sUsers = ""
   let sStatus = ""
   while (strlen(sLine) != 0)
      call s:printd("Line len: ".strlen(sLine))
      let sMatch = matchstr(sLine,".\\{-1,}\n")
      call s:printd("match: ".sMatch."\n")
      if match(sMatch,file) == 0
         if g:ssShowAllLocks == 1
            if strlen(sUsers) > 0
               let sUsers = sUsers.','
            endif
            let sUsers = sUsers.matchstr(sMatch,' \w\+')
            " If this checkout is exclusive, append it and break.
            if g:ssShowExtra
               if match(sMatch,'\w\+\s\+\w\+\s\+Esc') > -1
                  let sUsers = sUsers.' (exc)'
                  break
               " If this checkout is old, append it.
               elseif match(sMatch,'\w\+\s\+\w\+\s\+v') > -1
                  let sUsers = sUsers.' (old)'
               endif
            endif
            call s:printd("users: ".sUsers."\n")
         else
            call s:printd("checking match: ".sMatch." w/ ".g:ssUserName)
            " Get the index of where ssUserName ends.
            let iMatchedAt = matchend(sMatch,g:ssUserName)
            call s:printd("iMatchedAt: ".iMatchedAt."\n")
            " If *I* have it checked out...
            if iMatchedAt > -1
               let sStatus = "Locked"
               " If this checkout is exclusive, append it and break.
               if g:ssShowExtra
                  if match(sMatch,'\s\+Exc', iMatchedAt) > -1
                     let sStatus = sStatus." (exc)"
                  " If this checkout is old, append it and break.
                  elseif match(sMatch,'\s\+v', iMatchedAt) > -1
                     let sStatus = sStatus." (old)"
                  endif
               endif
               break
            " ElseIf someone else has it exclusively checked out,
            " Notify and break.
            elseif match(sMatch,'\w\+\s\+\w\+\s\+Esc') > -1
               let sStatus = sStatus."Locked by".matchstr(sMatch,' \w\+')
               break
            " Else I don't care about any other status.
            else
               let sStatus = "Not Locked"
            endif
         endif
      endif

      let iLen = strlen(sMatch)
      let sLine = strpart(sLine,iLen,strlen(sLine)-iLen)
      call s:printd("new line: ".sLine."\n")
   endwhile

   if strlen(sUsers) > 0
      let b:checked_out_status = "Locked by".sUsers
   elseif strlen(sStatus) > 0
      let b:checked_out_status = sStatus
   else
      echo "VSS plugin: Unrecoginzed output: ".sFull
   endif

   return b:checked_out_status
endfunction

" execute the SS command and echo the results to the vim window.
function s:Generic(bang,cmd_args)
   let sCmd = "ss ".a:cmd_args." ".s:GetSSName(expand("%:p"))." -GL".expand("%:p:h")
   if a:bang == "!" " Raw VSS interaction
      exec "!".sCmd
      e
      SSStatus
      return
   endif

   let sFull = system(sCmd)

   let sMatch = matchstr(sFull,".* is already checked out, continue.\\{-1,}\n")
   let iLen = strlen(sMatch)
   if (iLen == 0)
      let sMatch = matchstr(sFull,".* has changed. Undo check out and lose changes.\\{-1,}\n")
      let iLen = strlen(sMatch)
   endif
   if (iLen > 0)
      let sLine = strpart(sFull,iLen,strlen(sFull)-iLen)
   else
      let sLine = sFull
   endif

   SSStatus

   let v:errmsg = ""
   silent! e
   if v:errmsg != ""
      echo "reopen failed: ".v:errmsg
      return
   endif

   " echo command response, useful if there's an error
   echo sLine
endfunction

" NOTE: The "Editor for viewing files" field in VSS->Tools->Options->General
" must be blank in VSS for the built-in diff to work.  Otherwise the file will
" be opened in the listed editor rather than redirected to a temporary file. 
" TODO: Is there a way to dynamically set/restore this setting?
"  - make vert configurable
function s:Diff(bang,cmd_args)
   if a:bang == "!" " Raw VSS interaction
      exec "!ss Diff ".a:cmd_args." ".s:GetSSName(expand("%:p"))." ".expand("%:p")
      return
   endif

   let sFile = tempname().".".expand("%:e") " append same extention for syntax highlighting
   let sFull = system("ss View ".a:cmd_args." -O".sFile." ".s:GetSSName(expand("%:p")))

   if &diffexpr == ""
      let diff = "diff"
   else
      let diff = &diffexpr
   endif

   let sFull = system(diff." -q ".sFile." ".expand("%"))
   if (strlen(sFull) > 0)
      " the files differ
      if g:ssDiffVertical
         exec "vert diffsplit ".sFile
      else
         exec "diffsplit ".sFile
      endif
   else
      echo "No differences"
   endif
endfunction

function s:printd(message)
   if g:vss_debug
      echo a:message
   endif
endfunction

command! -bang -nargs=+ SS call s:Generic(<q-bang>,<q-args>)
command! -bang -nargs=* SSDiff call s:Diff(<q-bang>,<q-args>)
command! -bang -nargs=* SSStatus call s:UpdateStatus(<q-bang>,<q-args>)


" --- COMMANDS ---
"
" Some of the more common commands have been mapped below.  Any VSS command
" (in theory, anyway) can be called from the command line.
" This script really likes to do things quietly, but if you want more
" interaction from VSS, then use the "!" (bang) modifier.
" -I-Y means answer yes to all questions.
" See: VSS help for all available commands and command switches.


" Check out (and lock) current file from VSS
nmap ,cO :SS! Checkout<cr>
nmap ,co :SS Checkout -I-Y<cr>

" Check in current file
nmap ,cI :SS! Checkin<cr>
nmap ,ci :SS Checkin -I-Y<cr>

" Guess.
nmap ,cU :SS! Undocheckout<cr>
nmap ,cu :SS Undocheckout -I-Y<cr>

" Get the latest version of this file.  Does not lock file
nmap ,cG :SS! Get<cr>
nmap ,cg :SS Get<cr>

" Updates the locked status of this file.
nmap ,cS :SSStatus!<cr>
nmap ,cs :SSStatus<cr>

" compares differences, unix-like
nmap ,cD :SSDiff! -DU<cr>
nmap ,cd :SSDiff<cr>

" echo current DB.  Useful when working with several databases.  You can dynamically
" change which database to interface with by changing the $SSDIR env variable, like so:
" nmap ,do :let $SSDIR = "//<path>/<to>/<alternate>/<database>"
nmap ,db :echo "using VSS DB ".$SSDIR<cr>


" Extra utilities useful for working with VSS
"
" sets current file to writeable
nmap ,w :silent !attrib -r %<cr>:e<cr>

" sets current file to readonly
nmap ,r :silent !attrib +r %<cr>:e<cr>
