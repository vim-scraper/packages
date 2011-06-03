" Author: David Eggum <davide@simutech.com>
" Last Update: Apr 16 2001
" Version: 1.0
" 
" See: VSS help for all available commands and command switches
"
" Please email me any improvements you make
" 
" sourceSafe.vim - A simple script that interfaces with MS VSS from the command
" line.  This script works on the current file opened and is meant to only
" perform frequently used tasks such as check in, check out, get current file,
" check differences and so on.  The settings under the "CONFIGURATION" section
" below must be customized for your environment.

" --- CONFIGURATION ---

" Change this environment variable to the path of your VSS database.  The path to your
" database can be found in VSS under File->Open SourceSafe Database.
let $SSDIR="//gamma/swdev/src"

" Files are referenced in VSS like this:
" $/the/path/to/your/file
" So in order to match the path to the local file to the format above, the top-level
" directory must be changed to "$".
" For example, to check the status of the currently opened file, say
"   C:/dev/project/main.c
" Then this VSS command will to be sent
"   ss Status $/project/main.c
" Change this substitue pattern to the top level of your local tree.
let vssTree="C:.dev"

" This also works well:
" let vssTree=".*dev[0-9.]*"

" NOTE: These do not work:
" let vssTree="C:\dev"
" let vssTree="C:\\dev"
" let vssTree="C:/dev"



function! SSName(filename)
    let ssfile = substitute(a:filename,g:vssTree,"\$","")
    return substitute(ssfile,"\\","/","g")
endfunction

command! -nargs=+ SS exec "!ss" <q-args> SSName(expand("%:p"))
command! -nargs=+ SSl exec "!ss" <q-args> SSName(expand("%:p")) "-GL".expand("%:p:h") | e
command! -nargs=+ SSDiff exec "!ss Diff" <q-args> SSName(expand("%:p")) expand("%:p")


" --- COMMANDS ---

" Check out (and lock) current file from VSS
nmap ,co :SSl Checkout<cr>

" Check in current file
nmap ,ci :SSl Checkin<cr>

" Guess
nmap ,cu :SSl Undocheckout<cr>

" Get the latest version of this file.  Does not lock file
nmap ,cg :SSl Get<cr>

" Reports who has this file checked out
nmap ,cs :SS Status<cr>

" compares differences, unix-like
nmap ,cd :SSDiff -DU<cr>

" echo current DB.  Useful when working with several databases.  You can dynamically
" change which database to interface with by changing the $SSDIR env variable, like so:
" nmap ,do :let $SSDIR = "//<path>/<to>/<alternate>/<database>"
nmap ,db :echo "using VSS DB ".$SSDIR<cr>


" Extra utilities useful for VSS
" sets current file to writeable
nmap ,w :!attrib -r %<cr>:e<cr>

" sets current file to readonly
nmap ,r :!attrib +r %<cr>:e<cr>
