" vim: set ts=4 sw=4 tw=78 ai nowrap foldmethod=marker
"
" File header {{{1
" --------------------------------------------------------------
"  FILE:            vim_lxtrace.vim
"  AUTHOR:          Staale Flock, Staale -- lexholm .. no
"  VERSION:         0.1
"  LASTMODIFIED:    10 Nov 2004
"
"  PURPOSE:
"		To provide vim scripts with a simple tracing utility.
"		
"  WHY:
" 		I have been working on some vim-scripts and read a lot of them
" 		the last two weeks and I thought I needed a Tracing utility to
" 		ease analysis of how some scripts works.
" 		
"  REQUIREMENT:
" 		None, this script works as a standalone plugin. But I think you
" 		would appreciate the provided winmanager integration. To use it
" 		you would need a working version of the winmanager script.
" 		
"  INSTALLATION:
"  		Place this file in your plugin directory (~/.vim/plugin/)
"
"  		When you start vim again and open a vim file for editing you should
"  		get a message that lxTrace has installed it's documentation.
"  		To get you started you could try :h lxTrace
"
"  		If you have installed winmanager you should add LxTrace to the
"  		g:winManagerWindowLayout in your vimrc file. My line looks like
"  		this 
"			let g:winManagerWindowLayout = 'FileExplorer,TagList|BufExplorer,LxTrace'
"  		
"  TIPS:
"  		Documentation (when written..:o) is found at the bottom of this file. 
"  		Thanks to code from vimspell.vim it should be self-installing the first
"  		time this module is loaded. When installed you could try 
"  			:help lxTrace
" 
"  NOTE:
"		10 Nov 2004 (v 0.1) This is my initial upload. The module probably 
"		misses functionality and has undetected bugs. So feel free to
"		suggest improvements.
"		
"
"  Best Regards
"  Staale Flock, (staale -- lexholm .. no)
"  Norway
"
" ---------------------------------------------------------------------------
"
" Load check {{{1
if exists("LxTraceLoaded") || &cp
	finish
endif
let LxTraceLoaded = 1

" winmanager integration Variables {{{1
let g:LxTrace_title = "[lxTrace]"
"NOTE: TODO: Could probably change back to only g:LxTrace_title after
"bug in FocusWindow is verified gone
let s:LxTrace_title = g:LxTrace_title

let loaded_LxTrace = 1
" Used to make sure that only one LxTrace buffer is open at a time.
if !exists("g:lxTraceRunning")
  let g:lxTraceRunning = 0
endif

" When opening a new window, split the new windows below or above the
" current window?  1 = below, 0 = above.
if !exists("g:lxTraceSplitBelow")
  let g:lxTraceSplitBelow = &splitbelow		"splitbelow is a VIM flag
endif

" When opening a new window, split the new window horizontally or vertically?
" '' = Horizontal, 'v' = Vertical.
if !exists("g:lxTraceSplitType")
  let g:lxTraceSplitType = ""
endif

" When selected buffer is opened, open in current window or open a separate
" one. 1 = use current, 0 = use new.
if !exists("g:lxTraceOpenMode")
  let g:lxTraceOpenMode = 0
endif


"Used to control resizing
if !exists("g:lxTraceResize")
  let g:lxTraceResize = 1
endif

" Handles limits of dynamic resizing.
if !exists("g:lxTraceMaxHeight")
  let g:lxTraceMaxHeight = 15
endif


" Default behavior is a stand-alone script
" This will be changed if LxxTrace_Start is trigged by winmanager.
" TODO: If the user start this script before winmanager we might be in
" trouble
if !exists("s:displayMode")
	let s:displayMode=""
endif

" Script variables {{{1
" Turn on DebugMsg calls in this module
"dbgERROR = 0, dbgMSG = 1, dbgSUCCESS = 2, dbgTRYING = 3, dbgWARNING =1,
" dbgTRACE=1 
" Should normally be -1 to get ride of all messages.
if !exists("g:lxTraceDebugSelf")
	let g:lxTraceDebugSelf = -1
endif

"Trace internal messages to bufferwindow or echomsg
"1='echomsg', 0=>bufferwindow
"NOTE: 0 could result in circular calls!
if !exists("g:lxTraceDebugToEchomsg")
	let g:lxTraceDebugToEchomsg = 1
endif

"Default state when initiated by winmanager
"1='on' 0='off'
if !exists("g:lxTraceWMDefaultState")
	let g:lxTraceWMDefaultState = 1
endif

"Use a string variable as a buffer so we don't have to locate
"The trace window on each call.
if !exists('g:lxTraceUseMsgBuffer')
	let g:lxTraceUseMsgBuffer = 1
endif

"How many line should we buffer before we empty it to
"the LxTrace window.
if !exists('g:lxTraceMsgBufferLines')
	let g:lxTraceMsgBufferLines = 25
endif

if !exists('g:lxTraceSelfTest')
	let g:lxTraceSelfTest = 0	
endif

if !exists("s:traceStatus")
	let s:traceStatus = 'off'
endif

if !exists("s:LastMsgBufferError")
	let s:LastMsgBufferError = ""
endif

if !exists('s:msgBuffer')
	let s:msgBuffer = ""
	let s:msgBufferLineCount = 0	
endif
" winmanager integration Interface {{{1
" LxTrace_IsValid {{{2
" -------------------------------------------------------------------
"  Called by winmanger at each BufEnter to check if YourPlugin has a 
"  valide display (If YourPlugin is in a visible window). If it 
"  isn't then they will be redrawn by calling the next function 
"  :help winmanager-hook-isvalid
" -------------------------------------------------------------------
function! LxTrace_IsValid()
	call <SID>DebugSelf('dbgTRACE', "LxTrace_IsValide")
	return 1
endfunction
" LxTrace_IsPossible {{{2
" -------------------------------------------------------------------
"  Should return 0 if YourPluging is not able to display stuf at the
"  moment.If the interfaces not implemented winmanager assumes
"  YourPlugin is always able to display something.
" -------------------------------------------------------------------
function! LxTrace_IsPossible()
	call <SID>DebugSelf('dbgTRACE', "LxTrace_IsPossible")
	return 1
endfunction

" LxTrace_Start {{{2
" -------------------------------------------------------------------
" This is the function which winmanager calls the first time this plugin is
" displayed. Again, the rule for the name of this function is:
" <ExplorerName>_Start()
" -------------------------------------------------------------------
function! LxTrace_Start()
	call <SID>DebugSelf('dbgTRACE', "LxTrace_Start")
	"We have been called by winmanager so let it handle window
	"management.
	let s:displayMode='winmanager'
	
	call <SID>StartLxTrace(0)
	"Do the user want us to start tracing atonce?	
	if g:lxTraceWMDefaultState == 1
		let s:traceStatus = 'on'
	endif	

endfunction

" LxTrace_ReSize {{{2
" -------------------------------------------------------------------
"  Called after YourPlugin_Refresh() (Which is flagged as obsolete in
"  winmanager documentation?)
"  Supposedly called when winmanager allowes resizing.
"  Study code in bufexplorer.
"  :help winamanger-hook-resize
"
"  NOTE: your plugin may (will) be part of a window that is resized
"  by another plugin, or you might want to use only the nescesary
"  screen space to let other plugins have an easier life. You
"  could let the user decide how much space your plugin should claim
"  when sharing screen space with other plugins.
"  SOURCE: bufexplorer.vim
" -------------------------------------------------------------------
function! LxTrace_ReSize()
	call <SID>DebugSelf('dbgTRACE', "LxTrace_ReSize")
	"Check if we are the reason _ReSize is called.
	if !g:lxTraceResize
		return
	endif

	"If we have buffered content not in the window
	call LxTraceMsgBufferDump()
	
	"How many lines in our buffer?
	let nlines = line("$")
	"Adjust according to max values
	if nlines > g:lxTraceMaxHeight
		let nlines = g:lxTraceMaxHeight
	endif
	
	"resize window height
	exe nlines." wincmd _"
	
endfunction

" LxTrace_WrapUp {{{2
" -------------------------------------------------------------------
"  This interface is undocumented, but called from
"  GotoNextExplorerInGroup in winmanager. 
" -------------------------------------------------------------------
function! LxTrace_WrapUp()
"NOTE: Is this part of the winmanager interface?
	call <SID>DebugSelf('dbgTRACE', "LxTrace_WrapUp")
endfunction
"End winmanager integration }}}1
"
function! <SID>SetDefultBufferOptions()	"{{{1
	"NOTE: Make sure we have the right buffer?
	setlocal bufhidden=delete
	setlocal buftype=nofile
	setlocal highlight
	setlocal noswapfile
	setlocal nowrap
	setlocal nobuflisted
	setlocal noshowcmd		"Do not show command in status-line
endfunction


function! <SID>SetupMaps()	"{{{1
"  TODO: Sample code, remove and use your own
	"Will only work when buffer has focus (cursor inside window)!
	"nnoremap <buffer> <silent> <F12> :call <SID>LxWinbufAppend("F12 hitt")<cr>
	map <buffer> <silent> <F12> :call <SID>LxWinbufAppend("F12 hitt")<cr>
	map <silent> <F11> :call LxTrace("F11 hitt")<cr>
	"Listen (subscribe) to BufEnter event
	"autocmd BufEnter * call <SID>EventBufEnter()
endfunction


function! <SID>SetupSyntaxHighlighting()	"{{{1
"TODO: Not realy tested or tweeked
	if has("syntax") && !has("syntax_items") && exists("g:syntax_on")
		syn match LxTraceWinmanagerIterface	'.*\(_Start\|_ReSize\)'
		syn match LxTraceMSG '^MSG.*'
		syn match LxTraceERROR '^ERROR.*'
		syn match LxTraceDEGUG '^DEBUG.*'
		syn match LxTraceTagName	'^LxTrace.*' 
		syn match LxTraceVariable '.*\ [aglsw]:.*'
		syn match LxTraceIgnore '"$'
		"Special,String,Error,Comment,Ignore
		hi def link LxTraceWinmanagerIterface Special
		hi def link LxTraceMSG String
		hi def link LxTraceERROR Error
		hi def link LxTraceDEBUG Comment
		hi def link LxTraceTagName String
		hi def link LxTraceVariable Comment
		hi def link LxTraceIgnore Ignore
	endif		
endfunction




function! <SID>FocusWindow(bufRef)  "{{{1
" --------------------------------------------------------------{{{2
" This improves vimtip 357 which fails under certain conditions
" If a buffer with the name bufRef exists this function will make sure
" the bufRef window has focus and return a reference to the buffer in
" the current window.
" 
" The reason we need this function is that we can not trust 
" a command like: execute bufwinnr("MyBuf") 'wincmd w' to set focus on
" the right window. Mishaps are typical if you have windwows containing
" buffers with the nolist, nomodifiable set.
" Neither can we thrust bufname to work as documented in :help bufname
" On my system I get
"
" :echo bufname("[Buf List]") 	-->/mnt/hda5/home/staale/.vim/vimtips.txt
" :echo bufname("[Buf]") 		-->""
" :echo bufname("[Buf\ List]") 	-->/mnt/hda5/home/staale/.vim/vimtips.txt
" :echo bufname("[\Buf\ List\]") 	-->/mnt/hda5/home/staale/.vim/vimtips.txt
" :echo bufname("Buf List") 	-->[Buf List]
" :echo bufname("List") 		-->[Buf List]
" :echo bufname("File List") 	-->""
" :echo bufname("File") 		-->""
" :echo bufname("[File List]") 	-->"lxTrace.vim"
" :echo bufname("[File\ List]") 	-->"lxTrace.vim"
" :echo bufname("\[File\ List\]") 	-->"lxTrace.vim"
" 
" :echo bufname("vimtips") -->/mnt/hda5/home/staale/.vim/vimtips.txt
" :echo bufname(".vimrc") -->/mnt/hda5/home/staale/.vimrc
" :echo bufname("lxTrace") -->lxTrace.vim
" :
"
" When I have 6 windows open ([File List] (hidden) and [Buf List] from
" winmanager. 1 help file, vimtips.txt my .vimrc and this file. Focus is
" in this file during the tests
"
" Also why dows this return 3
" :echo match("\[File\ List\]","\[lxTrace\]")
"
" ----------------------------------------------------------------}}}2
	"Save current buffer information so we can get back
	let nCurBuf = bufnr("%")
	let sCurBufName = bufname(nCurBuf)	"Returns a complete? name
	let nCurWin = bufwinnr(nCurBuf)

	
	let nBuffers = bufnr("$")
	let i = 0
	let nnFailed = 0
	let done = 0
	if type(a:bufRef) == 1
		"Reference by name
		while i <= nBuffers && done == 0
			let i = i + 1
			let winnr = bufwinnr(i)
			if winnr > 0 
				execute winnr "wincmd w"
				let sFoo = bufname("%")
				"TODO: This might fail if we have two equaly named buffers
				"call <SID>DebugSelf(1, "TRYING: |".<SID>EscapeString(sFoo)."| AGAINST |".<SID>EscapeString(a:bufRef)."| match returns ".match(<SID>EscapeString(sFoo),<SID>EscapeString(a:bufRef)))
				if (match(<SID>EscapeString(sFoo),<SID>EscapeString(a:bufRef)) > -1) || (sFoo == a:bufRef)
					" we have the wanted window, so return reference to the
					" previous window
					let nnFailed = 1
					"call <SID>DebugSelf(1, "SUCCESS: FocusWindow: Focus set to ".sFoo." (bufRef: ".a:bufRef.")")
					return nCurBuf				
				endif
			endif
		endwhile
	else
		"Reference by bufnr
		let winnr = bufwinnr(a:bufRef)
		execute winnr "wincmd w"
		let nFoo = bufnr("%")
		if nFoo == a:bufRef
			let sFoo = bufname(nFoo)
			let nnFailed = 1
			call <SID>DebugSelf(1, "SUCCESS: FocusWindow: Focus set to bufnr ".nFoo." named: ".sFoo)
			return nCurBuf
		endif
	endif
	"If we reach this point we did not find the questioned buffer so we
	"have to go back to the original buffer and return a error msg 'kind
	"of'
	if nnFailed == 0 
		"call <SID>DebugSelf(1, "ERROR:FocusWindow: Focus could not be set to bufRef:".a:bufRef." Name found: ".sBufName." nNumRef:".nNumRef)
		call <SID>DebugSelf(0, "ERROR:FocusWindow: Focus could not be set to bufRef:".a:bufRef)
		execute nCurWin "wincmd w"
		return -1
	endif
endfunction

function! <SID>LxWinbufAppend(text)	"{{{1
	call <SID>WinbufAppend(s:LxTrace_title,a:text)
endfunction


function! <SID>WinbufAppend(title,text)	"{{{1
"NOTE: This function is (should be) generic and moved out to a library
"file.
"TODO: This function should be cleand up.
let nFocusBuf = <SID>FocusWindow(a:title)
if nFocusBuf > -1 
" This test should be cowered by FocusWindow
"	if ( match(<SID>EscapeString(bufname("%")),<SID>EscapeString(s:LxTrace_title)) > -1 ) || (bufname("%") == s:LxTrace_title)
		set modifiable	
		"call append("$", a:text)

		execute "normal G\<End>"
		"We have to use put rather than append("$",a:text) to preserve embeded
		"newline's
		put =''.a:text
		execute "normal G"
		
		"call <SID>DebugSelf('dbgMSG', "MSG: Appended text to : bufnr(".bufnr("%").") Named: ".bufname("%")." Return to: (".nFocusBuf.") named: ".bufname(nFocusBuf))
		"set nomodified
		"set nomodifiable
		let nFoo = <SID>FocusWindow(nFocusBuf) 
		if  nFoo == -1
			"Error msg
			"This can create circular calls
			"call <SID>DebugSelf('dbgERROR', "ERROR: lxTrace.vim MsgBuffer could not restore focus to buffer nr: ".nFocusBuf)
			let sMsg = "ERROR: lxTrace.vim MsgBuffer could not restore focus to buffer nr: ".nFocusBuf
			if !exists('s:LastMsgBufferError') || (s:LastMsgBufferError != sMsg)
				let s:Ls:LastMsgBufferError = sMsg
			else
				echomsg "ERROR: lxTrace.vim MsgBuffer could not restore focus to buffer nr: ".nFocusBuf
			endif
		endif
"	endif
else
	"Error msg
	call <SID>DebugSelf('dbgERROR', "ERROR: lxTrace.vim MsgBuffer Could not set focus to ".s:LxTrace_title." nFocusBuf: ".nFocusBuf)
	"echomsg "ERROR: lxTrace.vim MsgBuffer Could not set focus to ".s:LxTrace_title." nFocusBuf: ".nFocusBuf
endif
endfunction


function! <SID>MsgBufferAdd(text)	"{{{1
" Add a:text to a string buffer or directly to the
" LxTrace windowbuffer.
	if exists('g:lxTraceUseMsgBuffer') && g:lxTraceUseMsgBuffer == 1
		" buffer messages and view them later
		if strlen(a:text) > 0 
			if s:msgBufferLineCount == 0
				"let s:msgBuffer =s:msgBufferLineCount."\t".a:text
				let s:msgBuffer =a:text
			else
				let sFoo = s:msgBuffer
				"let s:msgBuffer = sFoo."\n".s:msgBufferLineCount."\t".a:text
				let s:msgBuffer = sFoo."\n".a:text
			endif
			let s:msgBufferLineCount = s:msgBufferLineCount +1
			if s:msgBufferLineCount >= g:lxTraceMsgBufferLines
				call LxTraceMsgBufferDump()
			endif
		endif
	else
		call <SID>LxWinbufAppend(a:text)
	endif
endfunction

function! LxTraceMsgBufferDump()	"{{{1
	if s:msgBufferLineCount > 0 
		"Must come first as it prevent rentrant calls while vim updates
		"buffers
		let s:msgBufferLineCount = 0
		call <SID>LxWinbufAppend(s:msgBuffer)
		"NOTE: It is not clear to me if this will free memory better than
		"just using let s:msgBuffer = ''
		unlet s:msgBuffer
		let s:msgBuffer = ""
	endif
endfunction
 

function! <SID>EscapeString(text)	"{{{1
" --------------------------------------------------------------- {{{2
"  Used to replace special regexp chrs in strings with escaped ones.
"  ex: :echo match("[File List]","[Buf List]") will return something
"  else than the expected -1
" ----------------------------------------------------------------}}}2	
	return substitute(substitute(a:text,'[','\\[',''),']','\\]','')
endfunction

function! LxTrace (text)	"{{{1
  if s:traceStatus == 'on' 
	call <SID>MsgBufferAdd(a:text)
  endif	
endfunction

function! LxTraceOn ()	"{{{1
  	"let l:oldnr = bufwinnr("%")
	"TODO: Should we check against winmanager and let winmanager set
	"focus to us if we should be handeled by winmanager?
	if !exists("s:displayMode")
		let s:displayMode = 'standalone'
	elseif s:displayMode != "winmanager"
		call <SID>StartLxTrace(2)
	endif
  	let s:traceStatus = 'on'
endfunction


function! LxTraceOff ()	"{{{1
  	let s:traceStatus = 'off'
endfunction

function! LxTraceStatus()	"{{{1
	return s:traceStatus
endfunction

function! LxTraceToggle() "{{{1
	if s:traceStatus = 'on'
		call LxTraceOff()
	elseif s:traceStatus = 'off'
		call LxTraceOn()
	else
		echomsg "ERROR: In lxTrace.TraceToggle: Unexpected trace state: ".s:traceStatus
	endif
	return s:traceStatus
endfunction

" TODO: Finish and cleanup regex code	{{{1
"let regexReplaceStart = '%s/'
"let regexReplaceEnd = '/g'
"let regexFunction = '\(^\s*fu\%[nction]\%[!]\ \%[<SID>]\(.*\)(.*\)'
"let regexLxTraceCall = '\s*call\ LxTrace('
"let regexLxTraceCallLine = regexLxTraceCall.'.*)'
"let regexLxTraceCallInsert= '\1\r\tcall\ LxTrace('.
"let regexLxTraceCallInsertIN = regexLxTraceCallInsert. "'IN\ ".'\2\'."')"

function! LxTraceInsertFunctionIn()	"{{{1
	let sFoo = '%s/\(^\s*fu\%[nction]\%[!]\ \%[<SID>]\(.*\)(.*\)/\1\r\tcall\ LxTrace('."'IN\ ".'\2\'."')".'/g'
	exe "".sFoo
endfunction

function! LxTraceRemoveFunctionIn()	"{{{1
	let sSearchReplaceStart = '%s/'
	let sSearchReplaceEnd = '/g'	
	let sFunction = '\(^\s*fu\%[nction]\%[!]\ \%[<SID>]\(.*\)(.*\)'
	let sTrace = '\s*call\ LxTrace(.*)'
	exe sSearchReplaceStart.'\('.sFunction.'\)'.'\n'.sTrace.'/'.'\1'.sSearchReplaceEnd
endfunction

" StartLxTrace {{{1
function! <SID>StartLxTrace(split) 
	let _splitbelow = &splitbelow		":help splitbelow
"TODO: make shure there is only one LxTrace Window
  let g:lxTraceSplitType = ""

  if a:split == 2
	let g:lxTraceSplitType = "v"
  endif

  " Save current and alternate buffer numbers for later.
"  let s:curBufNbr = <SID>MRUGet(1)
"  let s:altBufNbr = <SID>MRUGet(2)

  let &splitbelow = g:lxTraceSplitBelow
  call <SID>DebugSelf('dbgMSG', "MSG: s:displayMode: ".s:displayMode)
  if !exists("s:displayMode") || s:displayMode != "winmanager"
	  
	if a:split || (&modified && &hidden == 0)
	  if has("win32")
		exe "silent! ".g:lxTraceSplitType."sp ".s:LxTrace_title
	  else
		exe "silent! ".g:lxTraceSplitType."sp ".substitute(substitute(s:LxTrace_title,'[','\\[',''),']','\\]','')
	  endif

	  let s:lxTraceSplitWindow = 1
	else
	  if has("win32")
		exe "silent! e ".s:LxTrace_title
	  else
"		exe "silent! e ".s:LxTrace_title
		exe "silent! e ".substitute(substitute(s:LxTrace_title,'[','\\[',''),']','\\]','')
	  endif

	  let s:lxTraceSplitWindow = 0
	endif
	  
  endif
	" Just to make shure we are in the right window
	if <SID>FocusWindow(s:LxTrace_title) != -1
		call <SID>SetDefultBufferOptions()
		call <SID>SetupMaps()
		" set up the maps.
		call <SID>SetupSyntaxHighlighting()
		"Initiate the buffer
		let lModifiable = &modifiable
		let lModified = &modified
		setlocal modifiable
		call append("$",'Started: '.localtime())
		let &modifiable = lModifiable
		let &modified = lModified
	else
		echomsg 'ERROR: In StartLxTrace: FocusWindow('.s:LxTrace_title.' failed'
	endif  


"	put='started:'.localtime()
	let &splitbelow = _splitbelow
	let _showcmd = &showcmd	

endfunction

function! <SID>DebugSelf(dbgLevel,text)	"{{{1
	if !exists("s:DebugSelfCircularCall")
		let s:DebugSelfCircularCall = 0
	endif
if s:DebugSelfCircularCall == 0
try
		let s:DebugSelfCircularCall = 1
	"dbgERROR = 0, dbgMSG = 1, dbgSUCCESS = 2, dbgTRYING = 3, dbgWARNING =
	"1, dbgTRACE=1
 	"   Check if user has som opinions {{{2
	"   Convert level flag {{{2
	if type(a:dbgLevel) == 1
		if a:dbgLevel == 'dbgERROR'	
			let nLevel = 0 
		elseif a:dbgLevel == 'dbgMSG'	
			let nLevel = 1 
		elseif a:dbgLevel == 'dbgSUCCESS'	
			let nLevel = 2 
		elseif a:dbgLevel == 'dbgTRYING'	
			let nLevel = 3 
		elseif a:dbgLevel == 'dbgWARNING'	
			let nLevel = 1 
		elseif a:dbgLevel == 'dbgTRACE'
			let nLevel = 1
		else
			let nLevel = 0
			echomsg "ERROR: undefined dbgLevel:=".a:dbgLevel
		endif
	else
		let nLevel = a:dbgLevel
	endif "}}}2
	"   Determine output sink {{{2
	if  nLevel <= g:lxTraceDebugSelf
		if g:lxTraceDebugToEchomsg == 1
			echomsg "DEBUGSELF:[lxTrace.vim]".a:text
		else
			call <SID>LxWinbufAppend("DEBUGSELF:[lxTrace.vim]". a:text)
		endif
	endif	"}}}2
catch
	echomsg "ERROR: In lxTrace.DebugSelf: v:exception=".v:exception. " Occurred at: ".v:throwpoint
finally
	let s:DebugSelfCircularCall = 0
endtry
endif
endfunction




" Command and Menu items {{{1
command! -nargs=0 LxTraceOn call LxTraceOn()
command! -nargs=0 LxTraceOff call LxTraceOff()
command! -nargs=0 LxTraceToggle call LxTraceToggle()

" add/remove custom menu
" VIM Menu

menu &Plugin.&Trace.Trace&On :TraceOn<CR>
menu &Plugin.&Trace.TraceO&ff :TraceOff<CR>
menu &Plugin.&Trace.Trace\ &Status :echo LxTraceStatus()<CR>
imenu &Plugin.&Trace.&Insert\ LxTrace\ Text call LxTrace(" = " .  )9<Left>a


" NOTE Commented code, obsolete or unfinished {{{1
" ---------------------------------------------------------------------
"  Plugin specific functions
" ---------------------------------------------------------------------
"
""if exists("loaded_lsTrace")
""	finish
""endif
""let loaded_lsTrace = 1
""" Used to make sure that only one lxTrace buffer is open at a time.
""if !exists("g:lxTraceRunning")
""  let g:lxTraceRunning = 0
""endif
""
""" When opening a new window, split the new windows below or above the
""" current window?  1 = below, 0 = above.
""if !exists("g:lxTraceSplitBelow")
""  let g:lxTraceSplitBelow = &splitbelow		"splitbelow is a VIM flag
""endif
""
""" When opening a new window, split the new window horzontally or vertically?
""" '' = Horizontal, 'v' = Vertical.
""if !exists("g:lxTraceSplitType")
""  let g:lxTraceSplitType = ""
""endif
""
""" When selected buffer is opened, open in current window or open a separate
""" one. 1 = use current, 0 = use new.
""if !exists("g:lxTraceOpenMode")
""  let g:lxTraceOpenMode = 0
""endif
""
""
"""Variables requiered by winmanager plugin
""let s:LxTrace_title = "[lxTrace]"
""
""if !exists("g:lxTraceResize")
""  let g:lxTraceResize = 1
""endif
""
"""Variabes locale to lxTrace
""let s:traceStatus = 'off'
""let s:traceStatusWindowName = s:LxTrace_title "OBS! Do a %s///g if you change LxTrace_Window
""let s:displayMode=""
""
""" -------------------------------------------------------------------
""" Interfac methodes requiered by the winmanager plugin.
""" FORMAT <pluginName>_[IsPossible|Start|Isvalide|Refresh|ReSize]
""" -------------------------------------------------------------------
""function! LxTrace_Start()
""	let s:displayMode="winmanager"
""	let s:lxTraceSplitWindow = 0
""	call <SID>StartLxTrace(0)
""endfunction
""
""function! LxTrace_IsValid()
"""	call s:traceStatus("LxTrace_IsValide")
""	return 0
""endfunction
""
""function! LxTrace_IsPossible()
""	return 1
""endfunction
""
""function! LxTrace_Refresh()
""	let s:displayMode = "wnmanager"
""	call <SID>StartLxTrace(0)
""endfunction
""
""" Handles dynamic resizing of the window.
""if !exists("g:lxTraceMaxWidth")
""  let g:lxTraceMaxWidth = 25
""endif
""
""if !exists("g:lxTraceMaxHeight")
""	let g:lxTraceMaxHeight = 25
""endif
""
""function! LxTrace_ReSize()
""  if !g:lxTraceResize
""	return
""  end
""
""  let nlines = line("$")
""
""  if nlines > g:lxTraceMaxHeight
""	let nlines = g:lxTraceMaxHeight
""  endif
""
""  exe nlines." wincmd _"
""
""  " The following lines restore the layout so that the last file line is also
""  " the last window line. sometimes, when a line is deleted, although the
""  " window size is exactly equal to the number of lines in the file, some of
""  " the lines are pushed up and we see some lagging '~'s.
""  let presRow = line(".")
""  let presCol = virtcol(".")
""  exe $
""  let _scr = &scrolloff
""  let &scrolloff = 0
""  normal! z-
""  let &scrolloff = _scr
""  exe presRow
""  exe "normal! ".presCol."|"
""endfunction
""
""function! LxTrace_WrapUp()
""	"hmm, why??
""endfunction


""function! <SID>SetSyntaxHighlighting()
""	" set up some _really_ elementary syntax highlighting.
""	if has("syntax") && !has("syntax_items") && exists("g:syntax_on")
""		"\S non-whitespace character
""		syn match TagsLxTraceFunctionName	"^<SID>.*"
""		syn match TagsLxTraceFunctionName	"s\:.*"
""		syn match TagsLxTraceFunctionName	"g\:.*"
""		syn match TagsLxTraceFunctionName	"^IN.*"
""		syn match TagsLxTraceFunctionName	"^OUT.*"
""		"syn match TagsLxTraceTagName	/.*_Start|.*_IsPossible|.*_IsValid|.*_WrapUp|.*_title/
""		syn match TagsLxTraceTagName	".*_Start"
""		syn match TagsLxTraceError   '^"\s\+Error:'
""		syn match TagsLxTraceVariable 'g:TagsExplorerSkipError'
""		syn match TagsLxTraceIgnore '"$'
""
""		hi def link TagsLxTraceFunctionName String
""		hi def link TagsLxTraceTagName Special
""		hi def link TagsLxTraceError Error
""		hi def link TagsLxTraceVariable Comment
""		hi def link TagsLxTracerIgnore Ignore
""	end
""endfunction
""
""function! <SID>SetDefaultBufferOptions()
""	setlocal bufhidden=delete
""	setlocal buftype=nofile
""	setlocal nomodifiable
""	setlocal noswapfile
""	setlocal nowrap
""	setlocal nobuflisted
""	set noshowcmd
""endfunction
""
""
""function! <SID>CloseTraceWindow()
""	try
""		bunload lxTrace
""	endtry
""endfunction

""
"""au bufEnter * :call LxTrace("*** Event bufEnter *************")
"""au bufLeave * :call LxTrace("*** Event bufLeave *************")
""
"""def CT_Trace(text):
"""	try:
"""		vim.command('call LxTrace("' + text + '")')
"""	except: pass
"""
"""
""function! <SID>DeleteBuffer()
""  if getline('.') =~ '^"'
""	return
""  endif
""
""  setlocal modifiable
""
""  let _bufNbr = <SID>ExtractBufferNbr(getline('.'))
""
""  " These commands are to temporarily suspend the activity of winmanager.
""  if exists("s:displayMode") && s:displayMode == "winmanager"
""	call WinManagerSuspendAUs()
""  end
""
""  exe "silent! bd "._bufNbr
""  d _
""
""  " Reactivate winmanager autocommand activity.
""  if exists("s:displayMode") && s:displayMode == "winmanager"
""	call WinManagerForceReSize("BufExplorer")
""	call WinManagerResumeAUs()
""  end
""
""  setlocal nomodifiable
""endfunction
""
"" " FocusBuf {{{2
"" SOURCE Vimtip 357
"" function! <SID>FocusBuf(nameref)
"" move the focus to the buffer nameref -- create 
"" it if it doesn't exist
"" return the number of the currently focused buffer 
"function! <SID>FocusBuf(nameref)
"  let l:oldnr = bufwinnr("%")
"  let l:win_nu = bufwinnr(a:nameref)
"  if l:win_nu > 0
"	execute l:win_nu "wincmd w"
"  else
"	if bufexists(a:nameref)
"	  execute "sbuffer" a:nameref
"	else
"	  execute "new" a:nameref
"	endif
"  endif
"  return l:oldnr
"endfunction
"
"" AppendBuf {{{2
"" SOURCE Vimtip 357
"" function! <SID>AppendBuf(nameref, stuff)
"" append stuff to buffer nameref
"" 
"function! <SID>AppendBuf(nameref, stuff)
"   let l:oldnr = <SID>FocusBuf(a:nameref)
"   execute "normal G\<End>"
"   call append("$", a:stuff)
"   execute "normal G\<End>"
"	execute l:oldnr "wincmd w"
"endfunction
"
"}}}1

" Help (Documentation) installation {{{1
"
" Install-Documentation {{{2
" ---------------------------------------------------------------------
" Function: <SID>InstallDocumentation(full_name, revision)   
"   Install help documentation.
" Arguments:
"   full_name: Full name of this vim plugin script, including path name.
"   revision:  Revision of the vim script. #version# mark in the document file
"              will be replaced with this string with 'v' prefix.
" Return:
"   1 if new document installed, 0 otherwise.
" Note: Cleaned and generalized by guo-peng Wen
" 
" Source: vimspell.vim s:SpellInstallDocumentation 
"         http://www.vim.org/scripts/script.php?script_id=465  
" ---------------------------------------------------------------------
function! <SID>InstallDocumentation(full_name, revision)
    " Name of the document path based on the system we use:
    if (has("unix"))
        " On UNIX like system, using forward slash:
        let l:slash_char = '/'
        let l:mkdir_cmd  = ':silent !mkdir -p '
    else
        " On M$ system, use backslash. Also mkdir syntax is different.
        " This should only work on W2K and up.
        let l:slash_char = '\'
        let l:mkdir_cmd  = ':silent !mkdir '
    endif

    let l:doc_path = l:slash_char . 'doc'
    let l:doc_home = l:slash_char . '.vim' . l:slash_char . 'doc'

    " Figure out document path based on full name of this script:
    let l:vim_plugin_path = fnamemodify(a:full_name, ':h')
    let l:vim_doc_path    = fnamemodify(a:full_name, ':h:h') . l:doc_path
    if (!(filewritable(l:vim_doc_path) == 2))
        echomsg "Doc path: " . l:vim_doc_path
        execute l:mkdir_cmd . l:vim_doc_path
        if (!(filewritable(l:vim_doc_path) == 2))
            " Try a default configuration in user home:
            let l:vim_doc_path = expand("~") . l:doc_home
            if (!(filewritable(l:vim_doc_path) == 2))
                execute l:mkdir_cmd . l:vim_doc_path
                if (!(filewritable(l:vim_doc_path) == 2))
                    " Put a warning:
                    echomsg "Unable to open documentation directory"
                    echomsg " type :help add-local-help for more informations."
                    return 0
                endif
            endif
        endif
    endif

    " Exit if we have problem to access the document directory:
    if (!isdirectory(l:vim_plugin_path)
        \ || !isdirectory(l:vim_doc_path)
        \ || filewritable(l:vim_doc_path) != 2)
        return 0
    endif

    " Full name of script and documentation file:
    let l:script_name = fnamemodify(a:full_name, ':t')
    let l:doc_name    = fnamemodify(a:full_name, ':t:r') . '.txt'
    let l:plugin_file = l:vim_plugin_path . l:slash_char . l:script_name
    let l:doc_file    = l:vim_doc_path    . l:slash_char . l:doc_name

    " Bail out if document file is still up to date:
    if (filereadable(l:doc_file)  &&
        \ getftime(l:plugin_file) < getftime(l:doc_file))
        return 0
    endif

    " Prepare window position restoring command:
    if (strlen(@%))
        let l:go_back = 'b ' . bufnr("%")
    else
        let l:go_back = 'enew!'
    endif

    " Create a new buffer & read in the pluggin file (me):
    setl nomodeline
    exe 'enew!'
    exe 'r ' . l:plugin_file

    setl modeline
    let l:buf = bufnr("%")
    setl noswapfile modifiable

    norm zR
    norm gg

    " Delete from first line to a line starts with
    " === START_DOC
    1,/^=\{3,}\s\+START_DOC\C/ d

    " Delete from a line starts with
    " === END_DOC
    " to the end of the documents:
    /^=\{3,}\s\+END_DOC\C/,$ d

    " Remove fold marks:
    % s/{\{3}[1-9]/    /

    " Add modeline for help doc: the modeline string is mangled intentionally
    " to avoid it be recognized by VIM:
    call append(line('$'), '')
    call append(line('$'), ' v' . 'im:tw=78:ts=4:ft=help:norl:')

    " Replace revision:
    exe "normal :1s/#version#/ v" . a:revision . "/\<CR>"

    " Save the help document:
    exe 'w! ' . l:doc_file
    exe l:go_back
    exe 'bw ' . l:buf

    " Build help tags:
    exe 'helptags ' . l:vim_doc_path

    return 1
endfunction

" Autmoatically install documentation when script runs {{{2
" This code will check file (self) and install/update documentation included
" at the bottom.
" SOURCE: vimspell.vim, function! <SID>InstallDocumentation
" 		   http://www.vim.org/scripts/script.php?script_id=465
  let s:revision=
	\ substitute("$Revision: 0.1 $",'\$\S*: \([.0-9]\+\) \$','\1','')
  silent! let s:help_install_status =
      \ <SID>InstallDocumentation(expand('<sfile>:p'), s:revision)
  if (s:help_install_status == 1) 
      echom expand("<sfile>:t:r") . ' v' . s:revision .
		\ ': Help-documentation installed.'
  endif

	if (g:lxTraceSelfTest == 1)
	  call VURunnerRunTest('TestSuiteVimUnitSelfTest')
	endif	

" Stop sourceing this file, no code after this.
finish

" Documentation {{{1
" Help header {{{2
=== START_DOC
*lxTrace.txt*    A vim plugin to ease usage of trace statements      #version#


	lxTrace: Tracing code flow and developer messages in vim

==============================================================================
CONTENT  {{{2                                                *lxTrace-contents*
                                                                *Trace* *trace*
	Installation        : |lxTrace-installation|        *debug* *debugging*
	Configuration       : |lxTrace-configuration|
	lxTrace intro       : |lxTrace|
	Requirements        : |lxTrace-requirements|
	lxTrace commands    : |lxTrace-commands|
	Bugs                : |lxTrace-bugs|
	Tips                : |lxTrace-tips|
	Todo list           : |lxTrace-todo|
	Change log          : |lxTrace-cahnge-log|

==============================================================================
1. lxTrace Installation {{{2                            *lxTrace-installation*
                                                             |lxTrace-content|
	

	TODO: Write documentation, Installation
==============================================================================
1.1 lxTrace Configuration {{{2                         *lxTrace-configuration*
	
	The following settings can be modified in your vimrc

	let g:lxTraceSelfTest = 0
		This module should have some selftest functionality. It requieres 
		the vimUnit module to function.
		TODO: At the moment there is no selftest functionality. This modules 
		codebase was created before vimUnit.
	
	let g:lxTraceDebugself = -1
		Unless you do changes to lxTrace you would normaly not change this. 
		The values and assosiated words used in the script 
		dbgERROR = 0, dbgMSG = 1, dbgSUCCESS = 2, dbgTRYING = 3, dbgWARNING =1,
		dbgTRACE=1 
		The normal value -1 is used to supress all Debugself and many 
		handeled error messages.
		
	let g:lxTraceDebugToEchomsg = 1
		Change this to 0 if you want Debug information to be logged in the
		lxTrace windowbuffer rather than using echomsg.
		
	let g:lxTraceWMDefaultState = 1
		When winmanager controls the lxTrace windowbuffer this flag controls
		if tracing is started emediately after winmager has initiates 
		lxTrace.
			
	let g:lxTraceUseMsgBuffer = 0
		If you use lots of LxTrace() call's you should let lxTrace use a 
		internal buffer to store each call rather than updating the lxTrace
		windowbuffer each time.
		
	let g:lxTraceMsgBufferLines = 25
		This variable controls how many call's that are stored in the 
		internal buffer before it's content are moved to the lxTrace 
		windowbuffer.
		The internal buffer is also moved when lxTrace gets a ReSize message
		from winmanager or when a LxTraceMsgBufferDump call is done.
		
==============================================================================
1.1 lxTrace Requirements {{{2                           *lxTrace-requirements*
                                                             |lxTrace-content|
	TODO: Write documentation, Requirements
	
	Just a working vim environment
	
==============================================================================
2. lxTrace Intro {{{2                                 *VU* *VimUnit* *lxTrace*
                                                             |lxTrace-content|
	TODO: Write documentation, Intro
==============================================================================
3. lxTrace Commands {{{2                                    *lxTrace-commands*
                                                             |lxTrace-content|
	TODO: Write documentation, Commands

	LxTraceOn()                                                  *LxTraceOn()*
		Turn tracing on. If the windowbuffer is hidden make it visible.
		
	LxTraceOff()                                                *LxTraceOff()*
		Turn tracing off.
		
	LxTraceToggle()                                            *LxTraceToggle*		
		Toggel tracing. 
		If it is off a call to |LxTraceOn()| is done. 
		If tracing is on a call to |LxTraceOff()| is done.
		
	LxTraceInsertFunctionIn()                      *LxTraceInsertFunctionIn()*
		Insert a call to LxTrace() after each function declaration.
		
	LxTraceRemoveFunctionIn()                      *LxTraceRemoveFunctionIn()*
		Remove code inserted with LxTraceInsertFunctionIn()
		
	LxTraceMsgBufferDump()                            *LxTraceMsgBufferDump()*
		Transfer content, and empty, the internal buffer to the lxTrace 
		windowbuffer. This function is called internaly when 
		the g:msgBufferLineCount treshold is reached or lxTrace gets a ReSize 
		message from winmanager

==============================================================================
4. lxTrace Bugs {{{2                                            *lxTrace-bugs*
                                                             |lxTrace-content|
	TODO: Write documentation, Bugs
	
	Bugs, what bugs..;o)
	
	Seriously, When you think you have found on you have to describe the steps
	you do from you start vim to you encounter the bug. If you are not able to
	describe those steps I will not likely be able to recreate the bug will I?
	
	Your report should be mailed to me staale .. lexholm -- no.
	
	Known bugs or problems:
	1: Something is corumpting my modeline ? I want ts=4 not 8 and so on.
==============================================================================
5. lxTrace Tips {{{2                                            *lxTrace-tips*
                                                             |lxTrace-content|
	TODO: Write documentation, Tips
	
	You should use a internal Msg function to redirect and disable echo and 
	echomsg calls rather than using those calls dircetly. vim (gvim) has showen
	me some hickups regaridng gui update, in gvim, when I did to many gui update 
	calls and those calls was suspended by echo and/or echomsg calls.

==============================================================================
6. lxTrace Todo list {{{2                                       *lxTrace-todo*   
                                                             |lxTrace-content|
	TODO: Write more documentation

	Figure out why vimspell does not like this file and then use it.
	Automatic insertion of trace code in branches (if...else)
	Automatic insertion of trace code before return
	Add code In LxTraceInsert* to handel passed parameters.
	
==============================================================================
7. lxTrace Change log  {{{2                               *lxTrace-change-log*
                                                             |lxTrace-content|
Developer reference: (breake up mail address)
---------------------------------------------
SF = Staale Flock, staale -- lexholm .. no

------------------------------------------------------------------------------
By	Date		Description, if version nr changes place it first.
------------------------------------------------------------------------------
SF	10 Nov 2004	0.1	Initial uppload
==============================================================================

=== END_DOC

" vim: set ts=4 sw=4 tw=78 
