" Author: David Eggum <davide@simutech.com>
" Last Update: Jul 17 2001
" Version: 1.1a
" 
" sourceSafe.vim - A simple script that interfaces with the MS VSS command
" line.  This script works on the current file opened and is meant to only
" perform frequently used tasks such as checkin, checkout, get current file,
" check differences and so on.  The current lock status can also be displayed
" in the vim window; examples of placing it in the ruler are provided below.
" 
" Setup: Two settings under the "CONFIGURATION" section below must first be
" customized for your environment.  The third setting (rulerformat) is optional.
"
" Usage: See the "COMMANDS" section below for how to use this script.  
"
" Updates:
" 1.1 - The current lock status is now available from the VSSGetStatus()
"       function.  It looks really good in the ruler!  Samples are provided
"       below.  Also see :help rulerformat.
"     - Command responses are now echo'ed to the screen.
"     - Added silent flag (Daniel Einspanjer)
" 1.0 - Initial release

" Todo:
"  - Take advantage of the built-in diff util!  ...Although VSS's "View" command
"    doesn't obey the -O switch... suggestions anyone on how to get around
"    this?
"
" Please send me any improvements you make so they can be included!

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


" The current lock status is available from the VSSGetStatus() command.  A
" really good place to put this info is in the ruler, like so: 
"
" set rulerformat=%{VSSGetStatus()}
"
" The current lock status is updated when the file opens and after every :SS
" command (see COMMANDS section below)
"
" The following works well; change it to your liking.
set rulerformat=%60(%=%{VSSGetStatus()}%)\ %5l,%-4c\ %3p%%

" --- END CONFIGURATION ---

au! BufRead * call VSSUpdateStatus()

let b:checked_out_status = ""

function! SSName(filename)
   let ssfile = substitute(a:filename,g:vssTree,"\$","")
   return substitute(ssfile,"\\","/","g")
endfunction

function! VSSGetStatus()
   if exists("b:checked_out_status")
      return b:checked_out_status
   else
      return ""
   endif
endfunction

let g:vss_debug = 0

" get the current lock status from VSS and place it in b:checked_out_status
function! VSSUpdateStatus()
   let sFull = system("ss Status ".SSName(expand("%:p")))
   let file = expand("%:t")
   if g:vss_debug
      echo "returned: ".sFull
   endif
   let sLine = sFull
   let iStart = 0
   let sUsers = ""
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
   let file = strpart(file,0,19)
   if g:vss_debug
      echo "file: ".file."\n"
   endif
   while (strlen(sLine) != 0)
      if g:vss_debug
         echo "Line len: ".strlen(sLine)
      endif
      let sMatch = matchstr(sLine,".\\{-1,}\n")
      if g:vss_debug
         echo "match: ".sMatch."\n"
      endif
      if match(sMatch,file) == 0
         if strlen(sUsers) == 0
            let sUsers = sUsers.matchstr(sMatch,' \w\+')
         else
            let sUsers = sUsers.",".matchstr(sMatch,' \w\+')
            if g:vss_debug
               echo "users: ".sUsers."\n"
            endif
         endif
      endif

      let iLen = strlen(sMatch)
      let sLine = strpart(sLine,iLen,strlen(sLine)-iLen)
      if g:vss_debug
         echo "new line: ".sLine."\n"
      endif
   endwhile

   if strlen(sUsers) > 0
      let b:checked_out_status = "Locked by".sUsers
   else
      echo "VSS plugin: Unrecoginzed output: ".sFull
   endif

   return b:checked_out_status
endfunction

" execute the SS command, and echo the results to the vim window.
function! VSSDo(cmd_args)
   " exec "!ss ".a:cmd_args." ".SSName(expand("%:p"))." -GL".expand("%:p:h")
   let sFull = system("ss ".a:cmd_args." ".SSName(expand("%:p"))." -GL".expand("%:p:h"))

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

   call VSSUpdateStatus()

   let v:errmsg = ""
   silent! e
   if v:errmsg != ""
      echo "reopen failed: ".v:errmsg
      return
   endif

   " echo command response, useful if there's an error
   echo sLine
endfunction

command! -nargs=+ SS call VSSDo(<q-args>)
command! -nargs=+ SSDiff exec "!ss Diff" <q-args> SSName(expand("%:p")) expand("%:p")


" --- COMMANDS ---
"
" See: VSS help for all available commands and command switches


" Check out (and lock) current file from VSS
nmap ,co :SS Checkout -I-Y<cr>

" Check in current file
nmap ,ci :SS Checkin<cr>

" Guess.  Be careful, you will lose any changes you've made!
nmap ,cu :SS Undocheckout -I-Y<cr>

" Get the latest version of this file.  Does not lock file
nmap ,cg :SS Get<cr>

" Updates the locked status of this file.
nmap ,cs :call VSSUpdateStatus()<cr><C-L>

" compares differences, unix-like
nmap ,cd :SSDiff -DU<cr>

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
