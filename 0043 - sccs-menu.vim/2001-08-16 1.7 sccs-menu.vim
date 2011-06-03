
" =============================================================================
"          Name Of File: sccs-menu.vim
"           Description: Creates a SCCS menu.
"                Author: Pradeep Unde
"                   URL: 
"                  Date: August 8, 2001
"     Last Modification: "Wed, 18 Aug 2001"
"             Copyright: None.
"                 Usage: This menu displays useful Source Code Control System
"                        (SCCS) functions.
"         Configuration: Your sccs executables must be in your path.
"          Requirements: Only runs in GUI mode.
" =============================================================================

" Setting SCCS menu
if has("gui")
    amenu  S&CCS.&Get\ latest\ version\ of\ open\ file<Tab>sccs\ get\ (F3)	:call GetSCCSLatestVersion(expand("%"))<CR>
    amenu  S&CCS.Get\ a\ &version\ of\ open\ file<Tab>sccs\ get\ -rxx	:let rev = input("Enter revision number:")<Bar>call GetRevision(expand("%"), rev)<CR>
    amenu  S&CCS.Get\ latest\ version\ of\ &all\ files<Tab>sccs\ get\ SCCS	:!sccs get SCCS<CR>
    amenu  S&CCS.-SEP1-			:
    amenu  S&CCS.Check\ &in\ open\ file<Tab>sccs\ delta\ (F5)	 :let comm = input("Enter comment:")<Bar>call CheckIn(expand("%"), comm)<BAR>:e!<CR>
    amenu  S&CCS.Check\ &out\ open\ file<Tab>sccs\ edit\ (F4)	:!sccs edit %<CR>:e!<CR>
    amenu  S&CCS.&Uncheckout\ open\ file<Tab>sccs\ unget	:!sccs unget %<CR>:e!<CR>
    amenu  S&CCS.&Revert\ back<Tab>sccs\ unget/get\ (F10)	:!sccs unget %<CR>:!sccs get %<BAR>:e!<CR>
    amenu  S&CCS.-SEP2-			:
    amenu  S&CCS.&Add\ file\ in\ SCCS<Tab>sccs\ create	:!sccs create %<CR>:e!<CR>
    amenu  S&CCS.-SEP3-			:
    "amenu  S&CCS.Get\ &log\ of\ open\ file\ in\ SCCS<Tab>sccs\ prt\ (F6)	:!sccs prt %<CR>
    amenu  S&CCS.Get\ &log\ of\ open\ file\ in\ SCCS<Tab>sccs\ prt\ (F6)	:call SCCSShowLog("sccs-log", "sccs prt")<CR>
    amenu  S&CCS.Diff\ with\ prev\ version<Tab>(F7)	:call ShowSCCSDiff(expand("%"))<CR>
    amenu  S&CCS.Diff\ with\ two\ versions<Tab>	:let rev1 = input("Enter the first revision number to diff:")<Bar>:let rev2 = input("Enter the second revision number to diff:")<Bar>:call ShowSCCSVersionDiff(expand("%"), rev1, rev2)<CR>
endif

" SCCS Mappings
if(v:version >= 600)
    "Map F3 to get latest version of open file
    map <F3> :call GetSCCSLatestVersion(expand("%"))<CR>
    " Map F4 to check-out
    map <F4> :!sccs edit %<CR>:e!<CR>
    " Map F5 to check-in
    map <F5> :let comm = input("Enter comment:")<Bar>call CheckIn(expand("%"), comm)<BAR>:e!<CR>
    " Map F6 to get sccs log
    "map <F6> :!sccs prt %<CR>
    map <F6> :call SCCSShowLog("sccs-log", "sccs prt")<CR>
    " Map F7 to get diff of previous sccs version of open file 
    map <F7> :call ShowSCCSDiff(expand("%"))<CR>
    " Map F10 to Revert back current file
    map <F10> :!sccs unget %<CR>:!sccs get %<CR>:e!<CR>
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
    silent execute ":!rm tempfile.java"
    silent execute ":!sccs get -p " . a:filename . " > tempfile.java"
    silent execute ":vert diffs tempfile.java"
endfunction

" Shows diff of 2 different versions opened file. Uses vertical diff
" facility in Vim 6.0
function ShowSCCSVersionDiff(filename, rev1, rev2)
    if(a:rev1 == a:rev2)
        execute ":redraw"
        echo "Nothing to show diff"
        return
    endif
    let s:curr_ver = SCCSUpdateVersion()

    " Check we need to split the screen horizontally
    let s:rev = ""
    if(match(s:curr_ver, a:rev1) != -1)
        let s:rev = a:rev2
    elseif(match(s:curr_ver, a:rev2) != -1)
        let s:rev = a:rev1
    endif
    if(s:rev != "")
        silent execute ":!sccs get -p " . a:filename . " -r " . s:rev . " > tempfile.java"
        silent execute ":vert diffs tempfile.java"
        return
    endif
    
    
    silent execute ":!sccs get -p " . a:filename . " -r " . a:rev1 . " > tempfile1.java"
    silent execute ":!sccs get -p " . a:filename . " -r " . a:rev2 . " > tempfile2.java"
    
    if bufexists("diff")
        execute "bd! diff"
    endif

    " create a new buffer
    execute "new diff"

    " execute the rlog command
    execute ":0read tempfile1.java"
    set nomodified
    
    silent execute ":vert diffs tempfile2.java"


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
   let s:filename = expand("%")
   if(s:filename  == "")
       let b:sccs_version = ""
       return ""
   endif
   
   " First check whether the file exists in SCCS
   let s:cmdName="sccs prt " . s:filename 

   let b:version = system(s:cmdName)
   if(strpart(b:version, 0, 1) == "")
       let b:sccs_version = " "
       return b:sccs_version
   elseif(match(b:version, "nonexistent") != -1)
       let b:sccs_version = "Not in SCCS"
       return b:sccs_version
   elseif(match(b:version, "%") != -1)
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
   let s:cmdName="sccs what " . s:filename . " | tr -d \"\n\" | cut -f2 -d\":\" | tr -d \"\t\" | cut -f1 -d\",\" | cut -f2,3 -d\" \" | cut -f2 -d\" \""
   let b:version = system(s:cmdName)
   let b:sccs_version = strpart(b:version, 0, strlen(b:version)-1)
   return b:sccs_version
endfunction

function GetSCCSLatestVersion(filename)
    silent execute ":!sccs get " . a:filename
endfunction

" Misc settings
set laststatus=2    "Always have a status line to show SCCS version
"set rulerformat=%60(SCCS-%{SCCSGetVersion()}%)%=%l,%c%V\ %3P%*
set   statusline=%1*[%02n]%*\ %2*%f%*\ %(\[%M%R%H]%)%=SCCS-%{SCCSGetVersion()}\ %4l,%02c%2V\ %P%*

" Update the SCCS version of file when we open it
au! BufRead * call SCCSUpdateVersion()
