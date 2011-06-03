" File:		cmvc.vim (global plugin)
" Last Change:	Thu, 14 Feb 2002 11:59:57 Eastern Standard Time
" Maintainer:	Dan Sharp <dwsharp@hotmail.com>
" Version:	1.0
"
" NOTE:		This script currently requires Vim 6.  If there is interest
"		in making it 5.x compatible, let me know and I will see what 
"		I can do.
"
" Installation:	Just drop this file into your plugin directory, and it will
"		automatically be loaded when you start Vim.  Otherwise, just
"		put it in a directory of your choice and :source it as needed.
" 
" This script integrates using the IBM Configuration Management Version
" Control (CMVC) application from within Vim.  It provides keyboard shortcuts
" as well as a menu to access common functions such as checking files in and
" out, viewing and responding to defects, etc.
"
" Currently, only the functions I use regularly are implemented.  If you want
" to add others, the API should be fairly straightforward to do so.  Of
" course, I would appreciate receiving any changes you make, so that I can
" incorporate it into the next version (and avoid duplicating the work
" myself).
"
" TODO:	    Here are some other things I want to add when I get around to it:
"	    -	Implement more functions, like Report, to get a list of
"		available defects / files / etc., which you can then view or
"		checkout.
"	    -	Make the script more "plugin-ized", like allowing users to
"		override the default mappings and use their own.
"	    -	add a syntax file for the -view output from various commands
"	    -	create a formal help file
"	    -	Keep a history of releases, components, and families used,
"		then present them in a "confirm" dialog so the user can just
"		pick one instead of retyping it.  Inlcude an "other" button to
"		bring up an "inputdialog" so the user can still enter an
"		unlisted / new value.
"	    -	Add commands to let the functions be used in Command mode.

if exists("loaded_cmvc") || &cp
    finish
endif
let loaded_cmvc = 1

let s:save_cpo = &cpo
set cpo&vim

" Initialization:	    {{{
" Setup initial variables.  If the user has set a global variable, use it.
" Otherwise, check for an environment variable.  If neither are found, set 
" the variable to blank and the user will be prompted for it when needed. 
" ===================================================================
if exists("g:cmvcRelease")
    let s:cmvcRelease = g:cmvcRelease
elseif exists("CMVC_RELEASE")
    let s:cmvcRelease = $CMVC_RELEASE
else
    let s:cmvcRelease = ""
endif

if exists("g:cmvcFamily")
    let s:cmvcFamily = g:cmvcFamily
elseif exists("CMVC_FAMILY")
    let s:cmvcFamily = $CMVC_FAMILY
else
    let s:cmvcFamily = ""
endif

if exists("g:cmvcComponent")
    let s:cmvcComponent = g:cmvcComponent
elseif exists("CMVC_COMPONENT")
    let s:cmvcComponent = $CMVC_COMPONENT
else
    let s:cmvcComponent = ""
endif

if exists("g:cmvcTop")
    let s:cmvcTop = g:cmvcTop
elseif exists("CMVC_TOP")
    let s:cmvcTop = $CMVC_TOP
else
    let s:cmvcTop = ""
endif

if !exists("g:cmvcTracking")
    let g:cmvcTracking = "false"
endif

if !exists("g:cmvcVerbose")
    let g:cmvcVerbose = "false"
endif

"}}}

" Utility functions:		{{{
" Generally "getters" for various options passed to the
" CMVC commands.  The "getters" are usually paired, one to get the value, and
" another to create the parameter string that is passed to the executable.
" ===================================================================

" Family	    {{{
function! s:getFamily()
    if s:cmvcFamily == ""
	if exists("b:cmvcFamily")
	    let s:cmvcFamily = b:cmvcFamily
	else
	    call s:SetFamily()
	endif
    endif
    return s:cmvcFamily
endfunction

function! s:getFamilyParam()
    return " -family " . s:getFamily()
endfunction
" }}}

" Release	    {{{
function! s:getRelease()
    if s:cmvcRelease == ""
	if exists("b:cmvcRelease")
	    let s:cmvcRelease = b:cmvcRelease
	else
	    call s:SetRelease()
	endif
    endif
    return s:cmvcRelease
endfunction

function! s:getReleaseParam()
    return " -release " . s:getRelease()
endfunction
" }}}

" Component	    {{{
function! s:getComponent()
    if s:cmvcComponent == ""
	if exists("b:cmvcComponent")
	    let s:cmvcComponent = b:cmvcComponent
	else
	    call s:SetComponent()
	endif
    endif
    return s:cmvcComponent
endfunction

function! s:getComponentParam()
    return " -component " . s:getComponent()
endfunction
" }}}

" Convenience method.  Most File operations require Family and Release
" parameters.  This method combines them.  Just saves typeing later.
function! s:getFileCheckingParams()
    return s:getFamilyParam() . s:getReleaseParam()
endfunction

" Owner		{{{
function! s:getOwner()
    return inputdialog("Who should be the owner?")
endfunction

function! s:getOwnerParam()
    return " -owner " . s:getOwner()
endfunction
" }}}

" Defect	    {{{
function! s:getDefectNumber()
    if !exists("b:defectNum")
	let defectNum = inputdialog("What defect do you want?")
    else
	let defectNum = b:defectNum
    endif
    return defectNum
endfunction

function! s:getDefectParam()
    return " -defect " . s:getDefectNumber()
endfunction
" }}}

" Directory (a.k.a., Top)	    {{{
function! s:getDirectory()
    if !exists("b:top")
	let top = inputdialog("What directory do you want?")
    else
	let top = b:top
    endif
    return top
endfunction

function! s:getDirectoryParam()
    return " -top " . s:getDirectory()
endfunction
" }}}

" Remarks	    {{{
function! s:getRemarks()
    let remarks = ""
    normal gg
    let lineNum = 1
    while lineNum <= line("$")
	let remarks = remarks . getline(lineNum) . " "
	let lineNum = lineNum + 1
    endwhile
    return escape(remarks, '"')
endfunction

function! s:getRemarksParam()
    return " -remarks \"" . getRemarks() . "\""
endfunction
" }}}

" Abstract	    {{{
function! s:getAbstract()
    return inputdialog("Enter the abstract for this defect:")
endfunction

function! s:getAbstractParam()
    return " -abstract " . s:getAbstract()
endfunction
" }}}

" Filename	    {{{
function! s:getFileName()
    if exists("b:fileName")
	let fileName = b:fileName
    else
	let fileName = inputdialog("What file do you want?")
    endif
    return fileName
endfunction
" }}}

" Most commands send data to the server and expect no reply.  When specifying
" the -view option to a command, though, we want to display the output in a
" new buffer.  Open a scratch buffer and read in the data returned by the view
" command.
function s:view(object, target, extraParams)
    let bufName = "{" . a:object . "-" . a:target . "}"
    if !bufexists(bufName)
	execute "edit " . bufName
    else
	" If the buffer already exists, switch to it and delete the current
	" contents to refresh the display.
	execute "buffer " . bufName
	normal ggdG
    endif
    set buftype=nofile
    set bufhidden=hide
    set noswapfile
    execute "silent 0r !" . a:object . " -view " . a:target . a:extraParams . " -long"
endfunction

" Many operations allow the user to add remarks about the action they are
" performing.  For these operations, split open a scratch buffer where the
" user can enter these comments.  When the window is closed, execute the 
" specified command, passing the data entered in this buffer, then remove
" the buffer.
function! s:openCommentsWindow(object, action, params)
    new 
    set buftype=nofile
    set bufhidden=delete
    set noswapfile
    execute "map <silent> <buffer> :wq :call <SID>execute('" . a:object . "', '"
	\ . a:action . "', '" . a:params . "' . <SID>getRemarksParam() ) <bar> bw<CR>"
endfunction

" All commands except the -view have the same execution syntax.  This is just
" a central routine to easily allow global modification of the final command
" (like adding the -verbose flag, for example).
function! s:execute(object, action, params)
    let commandLine = "silent !" . a:object . " -" . a:action . " " . a:params
    if g:cmvcVerbose == "1"
	let commandLine = commandLine . " -verbose"
    endif
    execute commandLine
endfunction

" }}}

" Main operation functions: the "external API"	    {{{
" ===================================================================

" Work with files in the repository.
function! s:FileCommand(action)
    let fileName = s:getFileName()
    if fileName == ""
	return
    endif

    if a:action == "view"
	call s:view( "File", fileName, s:getFileCheckingParams() )
	let b:fileName = fileName
    elseif a:action == "checkin" || a:action == "create"
	let params =  expand("%") . s:getFileCheckingParams()
	if g:cmvcTracking == "true"
	    let params = params . s:getDefectParam()
	endif
	if a:action == "create"
	    let params = params . s:getComponentParam()
	endif
	call s:openCommentsWindow("File", a:action, params )
    elseif a:action == "checkout" || a:action == "extract" || a:action == "unlock" || a:action == "lock"
	call s:execute("File", a:action, fileName . s:getFileCheckingParams())
	execute "edit " . fnamemodify(fileName, ":t")
    endif
endfunction

" Work with defects in the repository
function! s:DefectCommand(action)
    let defectNum = s:getDefectNumber()
    if defectNum == ""
	return
    endif
    if a:action == "view"
	call s:view("Defect", defectNum, s:getFamilyParam())
	let b:defectNum = defectNum
    else
	let params = defectNum . s:getFamilyParam()
	if a:action == "assign"
	    let params = params . s:getOwnerParam()
	elseif a:action == "open"
	    let params = params . s:getComponentParam() . s:getAbstractParam()
	endif
	call s:openCommentsWindow("Defect", a:action, params)
    endif
endfunction

" Work with tracks in the repository
function! s:TrackCommand(action)
    let defectNum = s:getDefectNumber()
    if defectNum == ""
	return
    endif

    if a:action == "view"
	call s:view("Track", s:getDefectParam(), s:getFileCheckingParams())
	let b:defectNum = defectNum
    elseif a:action == "create" || a:action == "integrate" || a:action == "cancel" ||
        \  a:action == "review" || a:action == "complete"  || a:action == "test" ||
	\  a:action == "commit" || a:action == "consider"  || a:action == "fix"
	call s:execute( "Track", a:action, s:getDefectParam() . s:getFileCheckingParams())
    endif
endfunction

" Related to defects, allows you to work with verification records.
function! s:VerifyCommand(action)
    let defectNum = s:getDefectNumber()
    if defectNum == ""
	return
    endif

    let params = s:getDefectParam() . s:getFamilyParam()
    if a:action == "assign"
	let params = params . s:getOwnerParam()
	call s:execute( "VerifyCm", a:action, params)
    else
	call s:openCommentsWindow("VerifyCm", a:action, params)
    endif
endfunction

" Related to working with files, indicates whether a defect number must be
" associated with a command to modify a file.
function! s:EnableTracking(toggle)
    let g:cmvcTracking = a:toggle
endfunction

" A few quik methods to let users change the predefined values.
function! s:SetFamily()
    let s:cmvcFamily = inputdialog("What family should be used?", s:cmvcFamily)
endfunction

function! s:SetRelease()
    let s:cmvcRelease = inputdialog("What release should be used?", s:cmvcRelease)
endfunction

function! s:SetComponent()
    let s:cmvcComponent = inputdialog("What component should be used?", s:cmvcComponent)
endfunction

function! s:SetVerbose()
    let g:cmvcVerbose = confirm("Do you want verbose output?", "&Yes\n&No")
endfunction

" }}}

" Mappings and Menus		{{{
" ===================================================================

" Defects	    {{{
amenu <silent> <script> CMVC.&Defects.&View<TAB>\\dvi :call <SID>DefectCommand("view")<CR>
amenu <silent> <script> CMVC.&Defects.&Open<TAB>\\dop :call <SID>DefectCommand("open")<CR>
amenu <silent> <script> CMVC.&Defects.A&ssign<TAB>\\das  :call <SID>DefectCommand("assign")<CR>
amenu <silent> <script> CMVC.&Defects.A&ccept<TAB>\\dac  :call <SID>DefectCommand("accept")<CR>
amenu <silent> <script> CMVC.&Defects.&Verify<TAB>\\dve  :call <SID>DefectCommand("verify")<CR>
amenu <silent> <script> CMVC.&Defects.&Cancel<TAB>\\dca  :call <SID>DefectCommand("cancel")<CR>
amenu <silent> <script> CMVC.&Defects.&Reopen<TAB>\\dre  :call <SID>DefectCommand("reopen")<CR>
amenu <silent> <script> CMVC.&Defects.Add\ &Note<TAB>\\dan  :call <SID>DefectCommand("note")<CR>

map <silent> <Leader>dvi :call <SID>DefectCommand("view")<CR>
map <silent> <Leader>dop :call <SID>DefectCommand("open")<CR>
map <silent> <Leader>das :call <SID>DefectCommand("assign")<CR>
map <silent> <Leader>dac :call <SID>DefectCommand("accept")<CR>
map <silent> <Leader>dve :call <SID>DefectCommand("verify")<CR>
map <silent> <Leader>dca :call <SID>DefectCommand("cancel")<CR>
map <silent> <Leader>dre :call <SID>DefectCommand("reopen")<CR>
map <silent> <Leader>dan :call <SID>DefectCommand("note")<CR>
" }}}

" Files		{{{
amenu <silent> <script> CMVC.&Files.Check\ &In<TAB>\\fci  :call <SID>FileCommand("checkin")<CR>
amenu <silent> <script> CMVC.&Files.Check\ &Out<TAB>\\fco :call <SID>FileCommand("checkout")<CR>
amenu <silent> <script> CMVC.&Files.Create<TAB>\\fcr :call <SID>FileCommand("create")<CR>
amenu <silent> <script> CMVC.&Files.&Extract<TAB>\\fex :call <SID>FileCommand("extract")<CR>
amenu <silent> <script> CMVC.&Files.&View<TAB>\\fvi :call <SID>FileCommand("view")<CR>
amenu <silent> <script> CMVC.&Files.&Lock<TAB>\\flo :call <SID>FileCommand("lock")<CR>
amenu <silent> <script> CMVC.&Files.&Unlock<TAB>\\ful :call <SID>FileCommand("unlock")<CR>
amenu <silent> <script> CMVC.&Files.&Undo<TAB>\\fud :call <SID>FileCommand("undo")<CR>

map <silent> <Leader>fci :call <SID>FileCommand("checkin")<CR>
map <silent> <Leader>fco :call <SID>FileCommand("checkout")<CR>
map <silent> <Leader>fcr :call <SID>FileCommand("create")<CR>
map <silent> <Leader>fex :call <SID>FileCommand("extract")<CR>
map <silent> <Leader>fvi :call <SID>FileCommand("view")<CR>
map <silent> <Leader>flo :call <SID>FileCommand("lock")<CR>
map <silent> <Leader>ful :call <SID>FileCommand("unlock")<CR>
map <silent> <Leader>fud :call <SID>FileCommand("undo")<CR>
" }}}

" Tracks	    {{{
amenu <silent> <script> CMVC.&Tracks.&Create<TAB>\\tcr :call <SID>TrackCommand("create")<CR>
amenu <silent> <script> CMVC.&Tracks.&View<TAB>\\tvi :call <SID>TrackCommand("view")<CR>
amenu <silent> <script> CMVC.&Tracks.&Integrate<TAB>\\tin :call <SID>TrackCommand("integrate")<CR>
amenu <silent> <script> CMVC.&Tracks.&Cancel<TAB>\\tca :call <SID>TrackCommand("cancel")<CR>
amenu <silent> <script> CMVC.&Tracks.&Review<TAB>\\tre :call <SID>TrackCommand("review")<CR>
amenu <silent> <script> CMVC.&Tracks.Complete<TAB>\\tcp :call <SID>TrackCommand("complete")<CR>
amenu <silent> <script> CMVC.&Tracks.&Test<TAB>\\tte :call <SID>TrackCommand("test")<CR>
amenu <silent> <script> CMVC.&Tracks.Commit<TAB>\\tcm :call <SID>TrackCommand("commit")<CR>
amenu <silent> <script> CMVC.&Tracks.Consider<TAB>\\tcn :call <SID>TrackCommand("consider")<CR>
amenu <silent> <script> CMVC.&Tracks.&Fix<TAB>\\tfi :call <SID>TrackCommand("fix")<CR>

map <silent> <Leader>tcr :call <SID>TrackCommand("create")<CR>
map <silent> <Leader>tvi :call <SID>TrackCommand("view")<CR>
map <silent> <Leader>tin :call <SID>TrackCommand("integrate")<CR>
map <silent> <Leader>tca :call <SID>TrackCommand("cancel")<CR>
map <silent> <Leader>tre :call <SID>TrackCommand("review")<CR>
map <silent> <Leader>tcp :call <SID>TrackCommand("complete")<CR>
map <silent> <Leader>tte :call <SID>TrackCommand("test")<CR>
map <silent> <Leader>tcm :call <SID>TrackCommand("commit")<CR>
map <silent> <Leader>tcn :call <SID>TrackCommand("consider")<CR>
map <silent> <Leader>tfi :call <SID>TrackCommand("fix")<CR>
" }}}

" Verify Commands	    {{{
amenu <silent> <script> CMVC.&Verify.A&bstain<TAB>\\vab	:call <SID>VerifyCommand("abstain")<CR>
amenu <silent> <script> CMVC.&Verify.A&ccept<TAB>\\vac	:call <SID>VerifyCommand("accept")<CR>
amenu <silent> <script> CMVC.&Verify.A&ssign<TAB>\\vas	:call <SID>VerifyCommand("assign")<CR>
amenu <silent> <script> CMVC.&Verify.&Reject<TAB>\\vre	:call <SID>VerifyCommand("reject")<CR>

map <silent> <Leader>vab    :call <SID>VerifyCommand("abstain")<CR>
map <silent> <Leader>vac    :call <SID>VerifyCommand("accept")<CR>
map <silent> <Leader>vas    :call <SID>VerifyCommand("assign")<CR>
map <silent> <Leader>vre    :call <SID>VerifyCommand("reject")<CR>
" }}}

" General settings	    {{{
amenu <silent> <script> CMVC.&Enable\ Tracking<TAB>\\ten  :call <SID>EnableTracking("true")<CR>
amenu <silent> <script> CMVC.&Disable\ Tracking<TAB>\\tdi :call <SID>EnableTracking("false")<CR>
amenu <silent> <script> CMVC.Set\ &Family<TAB>\\sfa	  :call <SID>SetFamily()<CR>
amenu <silent> <script> CMVC.Set\ &Release<TAB>\\sre	  :call <SID>SetRelease()<CR>
amenu <silent> <script> CMVC.Set\ &Component<TAB>\\sco	  :call <SID>SetComponent()<CR>
amenu <silent> <script> CMVC.Set\ &Verbose<TAB>\\sve	  :call <SID>SetVerbose()<CR>

map <silent> <Leader>ten :call <SID>EnableTracking("true")<CR>
map <silent> <Leader>tdi :call <SID>EnableTracking("false")<CR>
map <silent> <Leader>sfa :call <SID>SetFamily()<CR>
map <silent> <Leader>sre :call <SID>SetRelease()<CR>
map <silent> <Leader>sco :call <SID>SetComponent()<CR>
map <silent> <Leader>sve :call <SID>SetVerbose()<CR>
" }}}

" }}}

let &cpo = s:save_cpo
unlet s:save_cpo
