
" =============================================================================
"          Name Of File: sccs-menu.vim
"           Description: Creates a SCCS menu.
"                Author: Pradeep Unde (pradeep_unde@yahoo.com)
"               Version: 1.9
"                   URL: 
"                  Date: August 8, 2001
"     Last Modification: "Tue, 15 Jan 2002"
"             Copyright: None.
"                 Usage: This menu displays useful Source Code Control System
"                        (SCCS) functions.
"         Configuration: Your sccs executables must be in your path.
"          Requirements: Only runs in GUI mode.
" =============================================================================

" Setting SCCS menu
if has("gui")
    amenu  S&CCS.&Get\ latest\ version\ of\ open\ file<Tab>sccs\ get\ (F3)	:call GetSCCSLatestVersion(expand("%:t"))<CR>
    amenu  S&CCS.Get\ a\ &version\ of\ open\ file<Tab>sccs\ get\ -rxx	:let rev = input("Enter revision number:")<Bar>call GetRevision(expand("%:t"), rev)<CR>
    amenu  S&CCS.Get\ latest\ version\ of\ &all\ files<Tab>sccs\ get\ SCCS	:!sccs get SCCS<CR>
    amenu  S&CCS.-SEP1-			:
    amenu  S&CCS.Check\ &in\ open\ file<Tab>sccs\ delta\ (F5)	 :let comm = input("Enter comment:")<Bar>call CheckIn(expand("%:t"), comm)<BAR>:e!<CR>
    amenu  S&CCS.Check\ &out\ open\ file<Tab>sccs\ edit\ (F4)	:!sccs edit %:t<CR>:e!<CR>
    amenu  S&CCS.&Unedit\ open\ file<Tab>sccs\ unedit	:!sccs unedit %:t<CR>:e!<CR>
    amenu  S&CCS.&Uncheckout\ open\ file<Tab>sccs\ unget	:!sccs unget %:t<CR>:e!<CR>
    amenu  S&CCS.&Revert\ back<Tab>sccs\ unget/get\ (F10)	:!sccs unget %:t<CR>:!sccs get %:t<CR>:e!<CR>
    amenu  S&CCS.-SEP2-			:
    amenu  S&CCS.&Add\ file\ in\ SCCS<Tab>sccs\ create	:!sccs create %:t<CR>:e!<CR>
    amenu  S&CCS.-SEP3-			:
    "amenu  S&CCS.Get\ &log\ of\ open\ file\ in\ SCCS<Tab>sccs\ prt\ (F6)	:!sccs prt %<CR>
    amenu  S&CCS.Get\ &log\ of\ open\ file\ in\ SCCS<Tab>sccs\ prt\ (F6)	:call SCCSShowLog("sccs-log", "sccs prt")<CR>
    amenu  S&CCS.Diff\ with\ prev\ version<Tab>(F7)	:call ShowSCCSDiff(expand("%:t"))<CR>
    amenu  S&CCS.Diff\ with\ two\ versions<Tab>	:let rev1 = input("Enter the first revision number to diff:")<Bar>:let rev2 = input("Enter the second revision number to diff:")<Bar>:call ShowSCCSVersionDiff(expand("%:t"), rev1, rev2)<CR>
endif

" SCCS Mappings
if(v:version >= 600)
    "Map F3 to get latest version of open file
    map <F3> :call GetSCCSLatestVersion(expand("%:t"))<CR>
    " Map F4 to check-out
    map <F4> :!sccs edit %:t<CR>:e!<CR>
    " Map F5 to check-in
    map <F5> :let comm = input("Enter comment:")<Bar>call CheckIn(expand("%:t"), comm)<BAR>:e!<CR>
    " Map F6 to get sccs log
    "map <F6> :!sccs prt %<CR>
    map <F6> :call SCCSShowLog("sccs-log", "sccs prt")<CR>
    " Map F7 to get diff of previous sccs version of open file 
    map <F7> :call ShowSCCSDiff(expand("%:t"))<CR>
    " Map F10 to Revert back current file
    map <F10> :!sccs unget %:t<CR>:!sccs get %:t<CR>:e!<CR>
endif

" Gets a required revision of a file from SCCS
function GetRevision(filename, revision)
    silent execute ":!sccs get -r" . a:revision . " " . a:filename
    call SCCSUpdateVersion()
endfunction

" Checks in a file with a comment in SCCS
function CheckIn(filename, comment)
    let quote = "\""
    silent execute ":!sccs delget -y" . quote a:comment . quote a:filename
endfunction

" Shows diff of opened file against the previous version. Uses vertical diff
" facility in Vim 6.0
function ShowSCCSDiff(filename)
    " Get the filetype of the file to be diffed. Used later on to set the
    " filtype of the diff buffers
    let s:fileType = &ft
    silent execute ":!sccs get -p " . a:filename . " > tempfile.java"
    " Check if 1st buffer for diff exists
    if bufexists("diff")
        execute "bd! diff"
    endif
    " create a new buffer
    execute "vnew diff"
    " Get the required version of the file from SCCS and put it in diff
    let s:cmdName = "sccs get -p -s " . a:filename
    silent execute "0r!" . s:cmdName
    execute "set filetype=" . s:fileType
    set nomodified
    execute "diffthis"
    " Do some cursor movement for diff to work!
    execute "wincmd l"
    execute "diffthis"
    execute "wincmd h"
    " Go to the beginning of the buffer
    execute "normal 1G"
endfunction

" Shows diff of 2 different versions opened file. Uses vertical diff
" facility in Vim 6.0
function ShowSCCSVersionDiff(filename, rev1, rev2)
    " Get the filetype of the file to be diffed. Used later on to set the
    " filtype of the diff buffers
    let s:fileType = &ft
    execute "set nodiff"

    " Check if we are diffing against the same versions
    if(a:rev1 == a:rev2)
        execute ":redraw"
        echo "Nothing to show diff"
        return
    endif

    " Get the vesion of the opened file
    let s:curr_ver = SCCSUpdateVersion()

    " Check we need to split the screen horizontally
    let s:rev = ""
    if(match(s:curr_ver, a:rev1) != -1)
        let s:rev = a:rev2
    elseif(match(s:curr_ver, a:rev2) != -1)
        let s:rev = a:rev1
    endif
    if(s:rev != "")
        " Check if 1st buffer for diff exists
        if bufexists("diff1")
            execute "bd! diff1"
        endif
        " create a new buffer
        execute "vnew diff1"
        " Get the required version of the file from SCCS and put it in diff
        let s:cmdName = "sccs get -p -s " . a:filename . " -r " . s:rev
        silent execute "0r!" . s:cmdName
        execute "set filetype=" . s:fileType
        set nomodified
        execute "diffthis"
        " Do some cursor movement for diff to work!
        execute "wincmd l"
        execute "diffthis"
        execute "wincmd h"
        " Go to the beginning of the buffer
        execute "normal 1G"
        return
    endif
    
    " Check if 1st buffer for diff exists
    if bufexists("diff1")
        execute "bd! diff1"
    endif

    " create a new buffer
    execute "new diff1"

    " Get the required version of the file from SCCS and put it in diff
    let s:cmdName = "sccs get -p -s " . a:filename . " -r " . a:rev1
    silent execute "0r!" . s:cmdName
    execute "set filetype=" . s:fileType
    set nomodified
    execute "diffthis"

    " Check if 2nd buffer for diff exists
    if bufexists("diff2")
        execute "bd! diff2"
    endif
    execute "vnew diff2"
    
    " Get the other required version of the file from SCCS
    let s:cmdName = "sccs get -p -s " . a:filename . " -r " . a:rev2
    silent execute "0r!" . s:cmdName
    execute "set filetype=" . s:fileType
    set nomodified
    execute "diffthis"
    
    " Do some cursor movement for diff to work!
    execute "wincmd l"
    execute "diffthis"
    execute "wincmd h"
    
    " Go to the beginning of the buffer
    execute "normal 1G"
endfunction

" -----------------------------------------------------------------------------
" ReadCommandBuffer
" - bufferName is the name which the new buffer with the command results
"   should have
" - cmdName is the command to execute
" -----------------------------------------------------------------------------
function! ReadCommandBuffer(bufferName, cmdName)
  " modify the shortmess option:
  " A  don't give the "ATTENTION" message when an existing swap file is
  "    found.
  set shortmess+=A

  " get the name of the current buffer
  let currentBuffer = bufname("%")

  " if a buffer with the input buffer name exists, delete it
  if bufexists(a:bufferName)
    execute 'bd! ' a:bufferName
  endif

  " create a new buffer
  execute 'new ' a:bufferName

  " execute the rlog command
  execute 'r!' a:cmdName ' ' currentBuffer

  set nomodified

  " go to the beginning of the buffer
  execute "normal 1G"

  " restore the shortmess option
  set shortmess-=A
endfunction

" -----------------------------------------------------------------------------
" ShowLog
" show the log results of the current file with SCCS
" -----------------------------------------------------------------------------
function! SCCSShowLog(bufferName, cmdName)
    call ReadCommandBuffer(a:bufferName, a:cmdName)
endfunction

let b:sccs_version = ""

function SCCSGetVersion()
   if exists("b:sccs_version")
      return b:sccs_version
   else
      return ""
   endif
endfunction

" Get the current version from SCCS and place it in b:sccs_version
function SCCSUpdateVersion()
   let s:filename = expand("%:t")
   if(s:filename  == "")
       let b:sccs_version = ""
       return ""
   endif

   " First check whether the file exists in SCCS
   let s:cmdName="sccs prt " . s:filename 

   let s:version = system(s:cmdName)
   if(strpart(s:version, 0, 1) == "")
       let b:sccs_version = " "
       return b:sccs_version
   elseif(match(s:version, "nonexistent") != -1)
       let b:sccs_version = "Not in SCCS"
       return b:sccs_version
   elseif(match(s:version, "%") != -1)
       let b:sccs_version = "Not in SCCS"
       return b:sccs_version
   endif

   " Now check if it is locked by the user
   let s:grpCmd = "sccs check -U | grep " . s:filename
   let s:grpRes = system(s:grpCmd)
   if(!v:shell_error)
       let b:sccs_version = "Checked out(Locked)"
       return b:sccs_version
   endif

   " Now get the actual version
   let s:cmdName="sccs prt -y " . s:filename . " | awk '{getline;print $3;}' "
   let s:version = system(s:cmdName)
   let b:sccs_version = strpart(s:version, 0, strlen(s:version)-1)
   return b:sccs_version
endfunction

function GetSCCSLatestVersion(filename)
    silent execute ":!sccs get " . a:filename
endfunction

" Misc settings
set laststatus=2    "Always have a status line to show SCCS version
"set rulerformat=%60(SCCS-%{SCCSGetVersion()}%)%=%l,%c%V\ %3P%*
set   statusline=%1*[%02n]%*\ %2*%f%*\ %(\[%M%R%H]%)%=SCCS-%{SCCSGetVersion()}\ %4l,%02c%2V\ %P%*

" Change to the directory the file in your current buffer is in
autocmd BufEnter * :cd %:p:h 
" Update the SCCS version of file when we open it
autocmd BufEnter * call SCCSUpdateVersion()
