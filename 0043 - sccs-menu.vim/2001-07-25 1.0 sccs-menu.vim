
" =============================================================================
"          Name Of File: sccs-menu.vim
"           Description: Creates a SCCS menu.
"                Author: Pradeep Unde
"                   URL: 
"                  Date: October 16, 2000
"     Last Modification: "Tue, 18 Jul 2000 14:25:48 Eastern Daylight Time ()"
"             Copyright: None.
"                 Usage: This menu displays useful Source Code Control System
"                        (SCCS) functions.
"         Configuration: Your sccs executables must be in your path.
"          Requirements: Only runs in GUI mode.
" =============================================================================

" Setting SCCS menu
if has("gui")
    amenu  S&CCS.&Get\ latest\ version\ of\ open\ file<Tab>sccs\ get\ (F3)	:!sccs get %<CR>
    amenu  S&CCS.Get\ a\ &version\ of\ open\ file<Tab>sccs\ get\ -rxx	:let rev = input("Enter revision number:")<Bar>call GetRevision(expand("%"), rev)<CR>
    amenu  S&CCS.Get\ latest\ version\ of\ &all\ files<Tab>sccs\ get\ SCCS	:!sccs get SCCS<CR>
    amenu  S&CCS.-SEP1-			:
    amenu  S&CCS.Check\ &in\ open\ file<Tab>sccs\ delta\ (F5)	 :let comm = input("Enter comment:")<Bar>call CheckIn(expand("%"), comm)<CR>
    amenu  S&CCS.Check\ &out\ open\ file<Tab>sccs\ edit\ (F4)	:!sccs edit %<CR>
    amenu  S&CCS.&Uncheckout\ open\ file<Tab>sccs\ unget	:!sccs unget %<CR>
    amenu  S&CCS.&Revert\ back<Tab>sccs\ unget/get\ (F10)	:!sccs unget %<CR>:!sccs get %<CR>
    amenu  S&CCS.-SEP2-			:
    amenu  S&CCS.&Add\ file\ in\ SCCS<Tab>sccs\ create	:!sccs create %<CR>
    amenu  S&CCS.-SEP3-			:
    amenu  S&CCS.Get\ &log\ of\ open\ file\ in\ SCCS<Tab>sccs\ prt\ (F6)	:!sccs prt %<CR>
    amenu  S&CCS.Diff\ with\ prev\ version<Tab>sccs\ diffs\ (F7)	:call ShowSCCSDiff(expand("%"))<CR>
endif

" SCCS Mappings
if(v:version >= 600)
    "Map F3 to get latest version of open file
    map <F3> :!sccs get %<CR>
    " Map F4 to check-out
    map <F4> :!sccs edit %<CR>
    " Map F5 to check-in
    map <F5> :let comm = input("Enter comment:")<Bar>call CheckIn(expand("%"), comm)<CR>
    " Map F6 to get sccs log
    map <F6> :!sccs prt %<CR>
    " Map F7 to get diff of previous sccs version of open file 
    map <F7> :call ShowSCCSDiff(expand("%"))<CR>
    " Map F10 to Revert back current file
    map <F10> :!sccs unget %<CR>:!sccs get %<CR>
endif

"function CheckIn(filename, comment)
"let quote = "\""
"execute ":!sccs delget -y" . quote a:comment . quote a:filename
"endfunction

function GetRevision(filename, revision)
execute ":!sccs get -r" . a:revision a:filename
endfunction

function CheckIn(filename, comment)
let quote = "\""
execute ":!sccs delget -y" . quote a:comment . quote a:filename
endfunction

function ShowSCCSDiff(filename)
execute ":!rm tempfile.java"
execute ":!sccs get -p " . a:filename . " > tempfile.java"
execute ":vert diffs tempfile.java"
endfunction
