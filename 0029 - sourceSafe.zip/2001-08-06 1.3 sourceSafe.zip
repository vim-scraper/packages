" Author: David Eggum <davide@simutech.com>
" Last Update: Aug 03 2001
" Version: 1.3
" 
" sourceSafe.vim - Interfaces with the MS VSS command line.  This script
" provides a shortcut to the most frequently used operations such as checkin,
" checkout, get current file, check differences, and so on; it is *not* meant
" to be a full replacement of the VSS GUI.
"
" Setup:
" The first three settings under the "CONFIGURATION" section below must first be
" customized for your environment.  The rest of the settings are based on user
" preference.
"
" Usage:
" Interaction with VSS is done from the VIM command line.  Each command works
" in the context of the current buffer.  Most of the commands look like this:
"
" :SS <action>
"
" Where <action> is substituted with something like "Checkout" or "Checkin
" -I-Y".  Whatever is entered for <action> is passed directly to VSS.
" See the "COMMANDS" section below for a mapping of the most common (and
" useful) operations.
"
" To diff the working file with the one checked into VSS, use the ":SSDiff"
" command.  The differences are displayed using the built-in VIM diff utility.
" Change the ssDiffVertical setting to display the difference vertically or
" horizontally.
"
" The current lock status is available from the SSGetStatus() function and is
" updated whenever a file is opened and after any "SS" command.  This function
" is useful for placing the file status somewhere in the vim window.  The
" status can be updated manually with the ":SSStatus" command.  Examples of
" placing it in the ruler are provided in the "CONFIGURATION" section below.
"
" A summary of all the files checked out by the current user will be listed
" with the ":SSShowAll" command.  Simple VSS operations can be performed on
" the listed files, both in normal and visual mode.
" 
" This script really likes to do things quietly, but if you want direct
" interaction from VSS, then use the bang (!) modifier with any command. For
" example:
"
" :SS! Undocheckout
"
" This will allow you to see the actual command sent to VSS, and give you a
" chance to answer any questions that VSS may have, such as "This file has
" changed, Undo check out and lose changes?(Y/N)".
"
" Updates:  The latest version is available at vim.sourceforge.net
" 1.3 - Added SSShowAll function that lists all checked out files.  
"     - Added some important VSS configuration information.  See SSDiff.
"     - Added mapleader.
" 1.2 - Diffing the file with the latest version in VSS now takes advantage of
"       Vim's built-in diff feature.
"     - Added new lock status mode. (Daniel Einspanjer)
"     - Added extra lock information (old) and (exc). (Daniel Einspanjer)
"     - Added "!" (bang) switch to all commands for direct interaction with VSS.
" 1.1a- Fixed hang with Undocheckout when the current file has changed.
" 1.1 - The current lock status is now available from the SSGetStatus()
"       function.  It looks really good in the ruler!  Samples are provided
"       below.  Also see :help rulerformat.
"     - Command responses are now echo'ed to the screen.
"     - Added silent flag (Daniel Einspanjer)
" 1.0 - Initial release

" Todo:
" - ...
"
" Send any suggestions or improvements you would like to have included!

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

" Change this to your username.  Note that VSS is case sensitive!
let ssUserName="Davide"

" The following flag will change the behavior of SSGetStatus() to show
" all users who have the file checked out, or only your status.
let ssShowAllLocks=1

" set to 1 to display outdated (old) or exclusive (exc) lock status.
let ssShowExtra=0

" When using the built-in diff feature, set this to 1 to split the windows
" vertically, set to 0 to split horizontally.
let ssDiffVertical=1

" In the SSShowAll window, this is the default interaction setting.
let ssQuietMode=1

" The current lock status is available from the SSGetStatus() command.  A
" really useful place to put this info is in the ruler, like so: 
"
" set rulerformat=%{SSGetStatus()}
"
" The following works well; change it to your liking.
set rulerformat=%60(%=%{SSGetStatus()}%)\ %4l,%-3c\ %3p%%


" --- COMMANDS ---
"
" Note: "-I-Y" means automatically answer yes to all questions.  If you do not
" like the default command mappings, i.e. ",co" for "Check Out", then change
" the mapleader setting to your liking. See :help mapleader.
" See: VSS help for all available commands and command switches.

let mapleader = ",c"

" Check out (and lock) current file from VSS
nmap <Leader>O :SS! Checkout<cr>
nmap <Leader>o :SS Checkout -I-Y<cr>

" Check in current file
nmap <Leader>I :SS! Checkin<cr>
nmap <Leader>i :SS Checkin -I-Y<cr>

" Guess.
nmap <Leader>U :SS! Undocheckout<cr>
nmap <Leader>u :SS Undocheckout -I-Y<cr>

" Get the latest version of this file.  Does not lock the file.
nmap <Leader>G :SS! Get<cr>
nmap <Leader>g :SS Get<cr>

" Updates the locked status of this file.
nmap <Leader>S :SSStatus!<cr>
nmap <Leader>s :SSStatus<cr>

" NOTE: The "Editor for viewing files" field in VSS->Tools->Options->General
" must be blank in VSS for the VIM diff utility to work.  Otherwise the file
" will be opened in the listed editor rather than redirected to a temporary
" file.  Also, so that comments can be added when checking in code, be sure
" that an editor is listed in VSS->Tools->Options->Command Line Options, and
" the "Use editor to prompt for comments" option is selected,

" compares differences, unix-like
nmap <Leader>D :SSDiff! -DU<cr>
nmap <Leader>d :SSDiff<cr>

nmap <Leader>A :SSShowAll!<cr>
nmap <Leader>a :SSShowAll<cr>

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


" --- END OF COMMANDS ---

au! BufRead * SSStatus

let b:checked_out_status = ""

function s:GetSSName(filename)
   let ssfile = substitute(a:filename,g:vssTree,"\$","")
   return substitute(ssfile,"\\","/","g")
endfunction

function SSGetStatus()
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
   let sUsers = ""
   let sStatus = ""
   while (strlen(sLine) != 0)
      let sMatch = matchstr(sLine,".\\{-1,}\n")
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
         else
            " Get the index of where ssUserName ends.
            let iMatchedAt = matchend(sMatch,g:ssUserName)
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
function s:Generic(bang,cmd_args,filename,bExternal)
   let sCmd = "ss ".a:cmd_args." ".s:GetSSName(a:filename)." -GL".fnamemodify(a:filename,":h")
   if a:bang == "!" " Raw VSS interaction
      exec "!".sCmd
      if a:bExternal == 0
         e
         SSStatus
      endif
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

   if a:bExternal == 0
      SSStatus

      let v:errmsg = ""
      silent! e
      if v:errmsg != ""
         echo "reopen failed: ".v:errmsg
         return
      endif
   endif

   " echo command response, useful if there's an error
   echo sLine
endfunction

function s:Diff(bang,cmd_args,filename,bSummary)
   if a:bang == "!" " Raw VSS interaction
      exec "!ss Diff ".a:cmd_args." ".s:GetSSName(a:filename)." ".a:filename
      return
   endif

   let sFile = tempname().".".fnamemodify(a:filename,":e") " append same extention for syntax highlighting
   let sFull = system("ss View ".a:cmd_args." -O".sFile." ".s:GetSSName(a:filename))

   if &diffexpr == ""
      let diff = "diff"
   else
      let diff = &diffexpr
   endif

   let sFull = system(diff." -q ".sFile." ".a:filename)
   if (strlen(sFull) > 0)
      " the files differ
      if a:bSummary
         return 1
      endif

      if g:ssDiffVertical
         exec "vert diffsplit ".sFile
      else
         exec "diffsplit ".sFile
      endif
   else
      if a:bSummary
         return 0
      endif

      echo "No differences"
   endif
endfunction

" show all files locked by you
function s:showAll(bang)
   let sCmd = "ss Status $/ -R -U"
   if a:bang == "!"
      exec "!".sCmd
      return
   endif

   echo "Please wait..."

   " sent to a temporary file to prevent VSS from splitting long lines
   let sFile = tempname()
   let sFull = system(sCmd." -O".sFile)
   exec "sp ".sFile
   set nobuflisted
   silent g/:$/d
   silent g/^$/d
   silent exec '%s/'.g:ssUserName.'\s\+//e'

   1
   normal mv

   let @v = "\" Use a visual block (V) to operate on multiple files.\n"
          \."\" (e)dit, check(i)n, (u)ndocheckout, (d)iff, (q)uit\n"
          \."\" (!) change interaction mode. Current mode is quiet.\n"
          \."\" (D)iff summary.\n"
          \."\" Files checked out by ".g:ssUserName.":\n"
   put! v
   if g:ssQuietMode == 0
      silent %s/quiet/interactive/
   endif
   w

   setlocal nomodifiable

   if winheight(".") > line("$")
      exec "resize ".line("$")
   endif

   1
   normal 'v

   syn keyword String contained quiet interactive
   syn match Comment "^\".*$" contains=Special,String,Directory
   syn match Directory "^\f\+" contains=Special
   syn match String "^\" Files.*$"
   syn match Special "(\zs.\ze)" contained
   syn match Title "No files found.*$"
   syn match Special "No changes" contained
   syn match Directory "\zschanges\ze." contained

   nmap <buffer> <silent> !      :call SSChangeMode()<cr>
   nmap <buffer> <silent> D      :call SSDiffSummary()<cr>
   nmap <buffer> <silent> e      :call SSEditFile()<cr>
   nmap <buffer> <silent> i      :call SSGeneric("Checkin",0)<cr>
   vmap <buffer> <silent> i      :call SSGeneric("Checkin",1)<cr>
   nmap <buffer> <silent> u      :call SSGeneric("Undocheckout",0)<cr>
   vmap <buffer> <silent> u      :call SSGeneric("Undocheckout",1)<cr>
   nmap <buffer> <silent> d      :call SSDiffl()<cr>
   vmap <buffer> <silent> d      :call SSDiffl()<cr>
   nmap <buffer> <esc>  :q!<cr>
   nmap <buffer> q      :q!<cr>
endfunction

function SSDiffSummary()
   let iLastLine = line("$")
   let i = 1
   while i <= iLastLine
      let sFile = s:getFile(i)
      let i = i + 1
      if strlen(sFile) == 0
         continue
      endif

      if s:Diff("","",sFile,1) == 0
         let sFile = strpart(fnamemodify(sFile,":t"),0,19)
         exec "syn match Special '".sFile."' contained"
      endif
   endwhile

   normal mv
   setlocal modifiable
   silent! %s/\zsiff summary\ze\./&; No changes, changes/
   noh
   setlocal nomodifiable
   set nomodified
   normal 'v
endfunction

function SSChangeMode()
   setlocal modifiable
   normal mv
   if g:ssQuietMode == 1
      let g:ssQuietMode = 0
      silent %s/quiet/interactive/
   else
      let g:ssQuietMode = 1
      silent %s/interactive/quiet/
   endif
   normal 'v
   setlocal nomodifiable
   set nomodified
endfunction

function s:getFile(spot)
   let sLine = getline(a:spot)
   if match(sLine,"^\"") != -1
      return ""
   elseif match(sLine,"^No files found.*") != -1
      return ""
   endif
   let sFile = matchstr(sLine,"\\f\\+")
   let sFull = matchstr(sLine,"\\f\\+$")."\\".sFile
   if (strlen(sFile) == 19) " VSS probably truncated the filename
      return glob(sFull."*") " try to restore it
   endif

   return sFull
endfunction

function SSEditFile()
   let sFile = s:getFile(".")
   if strlen(sFile) == 0
      return
   endif
   q!
   exec "e ".sFile
endfunction

function SSGeneric(cmd,bVisual)
   let sFile = s:getFile(".")
   if strlen(sFile) == 0
      return
   endif
   if g:ssQuietMode == 1
      call s:Generic("",a:cmd." -I-Y",sFile,1)
   else
      call s:Generic("!",a:cmd,sFile,1)
   endif

   " Note: can't use mode(), it always returns "n"
   if a:bVisual == 0
      setlocal modifiable
      d
      set nomodified
      setlocal nomodifiable
   elseif line("'>") == line(".")
      " we are operating on the last line in the visual block so delete the
      " entire block
      setlocal modifiable
      silent '<,'>d
      set nomodified
      setlocal nomodifiable
   endif
endfunction

function SSDiffl()
   let sFile = s:getFile(".")
   if strlen(sFile) == 0
      return
   endif
   call s:Diff("!","-DU",sFile,0)
endfunction

command! -bang -nargs=+ SS call s:Generic(<q-bang>,<q-args>,expand("%:p"),0)
command! -bang -nargs=* SSDiff call s:Diff(<q-bang>,<q-args>,expand("%:p"),0)
command! -bang -nargs=* SSStatus call s:UpdateStatus(<q-bang>,<q-args>)
command! -bang -nargs=* SSShowAll call s:showAll(<q-bang>)


" vim: sw=3
