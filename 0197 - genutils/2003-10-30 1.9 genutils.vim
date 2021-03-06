"
" Useful buffer, file and window related functions.
"
" Author: Hari Krishna Dara <hari_vim at yahoo dot com>
" Last Change: 28-Oct-2003 @ 09:43
" Requires: Vim-6.0 (preferably 6.2), multvals.vim(3.3)
" Version: 1.9.4
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"     http://www.vim.org/script.php?script_id=197
" Description:
"   - Functions MakeArgumentString(), MakeArgumentList() and CreateArgString()
"     to work with and pass variable number of arguments to other functions.
"     There is also an ExtractFuncListing() function that is used by the above
"     functions to create snippets (see also breakpts.vim, ntservices.vim and
"     ntprocesses.vim for interesting ideas on how to use this function).
"	  fu! s:IF(...)
"	    exec MakeArgumentString('myArgString')
"   	    exec "call Impl(1, " . myArgString . ")"
"   	  endfu
"	or	
"	  fu! s:IF(...)
"	    exec MakeArgumentList('myArgList', ';')
"   	    exec "call Impl(1, " . CreateArgString(myArgList, ';') . ")"
"   	  endfu
"   - Misc. window/buffer related functions, NumberOfWindows(),
"     FindWindowForBuffer(), FindBufferForName(), MoveCursorToWindow(),
"     MoveCurLineToWinLine(), SetupScratchBuffer(), MapAppendCascaded()
"   - Save/Restore all the window height/width settings to be restored later.
"   - Save/Restore position in the buffer to be restored later. Works like the
"     built-in marks feature, but has more to it.
"   - NotifyWindowClose() to get notifications *after* a window with the
"     specified buffer has been closed or the buffer is unloaded. The built-in
"     autocommands can only notify you *before* the window is closed. You can
"     use this with the Save/Restore window settings feature to restore the
"     user windows, after your window is closed. I have used this utility in
"     selectbuf.vim to restore window dimensions after the browser window is
"     closed. To add your function to be notified when a window is closed, use
"     the function:
"
"         function! AddNotifyWindowClose(windowTitle, functionName)
"
"     There is also a test function called RunNotifyWindowCloseTest() that
"     demos the usage.
"   - ShowLinesWithSyntax() function to echo lines with syntax coloring.
"   - ShiftWordInSpace(), CenterWordInSpace() and
"     AlignWordWithWordInPreviousLine() utility functions to move words in the
"     space without changing the width of the field. A GetSpacer() function to
"     return a spacer of specified width.
"   - A quick-sort functions QSort() that can sort a buffer contents by range
"     and QSort2() that can sort any arbitrary data and utility compare
"     methods.  Binary search functions BinSearchForInsert() and
"     BinSearchForInsert2() to find the location for a newline to be inserted
"     in an already sorted buffer or arbitrary data.
"   - ExecMap function has now been separated as a plugin called execmap.vim.
"   - A sample function to extract the scriptId of a script.
"   - New CommonPath() function to extract the common part of two paths, and
"     RelPathFromFile() and RelPathFromDir() to find relative paths (useful
"     HTML href's). A side effect is the CommonString() function to find the
"     common string of two strings.
"   - UnEscape() and DeEscape() functions to reverse and Escape() to compliment
"     what built-in escape() does.
"   - Utility functions CurLineHasSign() and ClearAllSigns() to fill in the
"     gaps left by Vim.
"   - GetVimCmdOutput() function to capture the output of Vim built-in
"     commands, in a safe manner. This function is available only for Vim
"     version 6.2 or higher.
"   - OptClearBuffer() function to clear the contents and undo history of the
"     current buffer in an optimal manner. Ideal to be used when plugins need
"     to refresh their windows and don't care about preserving the current
"     contents (which is the most usual case).
"   - GetPreviewWinnr() function (only on Vim6.2).
"   - Functions to have persistent data, PutPersistentVar() and
"     GetPersistentVar(). You don't need to worry about saving in files and
"     reading them back. To disable, set g:genutilsNoPersist in your vimrc.
"
"     Example: Put the following in a file called t.vim in your plugin
"     directory and watch the magic. You can set new value using SetVar() and
"     see that it returns the same value across session when GetVar() is
"     called.
"     >>>>t.vim<<<<
"	au VimEnter * call LoadSettings()
"	au VimLeavePre * call SaveSettings()
"	
"	function! LoadSettings()
"	  let s:tVar = GetPersistentVar("T", "tVar", "defVal")
"	endfunction
"	
"	function! SaveSettings()
"	  call PutPersistentVar("T", "tVar", s:tVar)
"	endfunction
"	
"	function! SetVar(val)
"	  let s:tVar = a:val
"	endfunction
"	
"	function! GetVar()
"	  return s:tVar
"	endfunction
"     <<<<t.vim>>>>
"
"   - A function to emulate the default Vim behavior for |timestamp| changes.
"     It also provides hooks to get call backs before and after handling the
"     default FileChangedShell autocommand (effectively splitting it into a
"     Pre and a Post event). Suggested usage is to use AddToFCShellPre() or
"     AddToFCShell() and either install a default event handling mechanism for
"     all files by calling DefFCShellInstall() or create your own autocommand on
"     a matching pattern to call DefFileChangedShell() function. See
"     perforce.vim for usage examples.
"   - Place the following in your vimrc if you find them useful:
"
"       command! DiffOff :call CleanDiffOptions()
"       
"       command! -nargs=0 -range=% SortByLength <line1>,<line2>call QSort(
"           \ 'CmpByLengthNname', 1)
"       command! -nargs=0 -range=% RSortByLength <line1>,<line2>call QSort(
"           \ 'CmpByLineLengthNname', -1)
"       command! -nargs=0 -range=% SortJavaImports <line1>,<line2>call QSort(
"           \ 'CmpJavaImports', 1)
"
"	nnoremap <silent> <C-Space> :call ShiftWordInSpace(1)<CR>
"	nnoremap <silent> <C-BS> :call ShiftWordInSpace(-1)<CR>
"	nnoremap <silent> \cw :call CenterWordInSpace()<CR>
"
"	nnoremap <silent> \va :call AlignWordWithWordInPreviousLine()<CR>
" Deprecations:
"   - The g:makeArgumentString and g:makeArgumentList are obsolete and are
"     deprecated, please use MakeArgumentString() and MakeArgumentList()
"     instead.
"   - FindWindowForBuffer() function is now deprecated, as the corresponding
"     Vim bugs are fixed. Use the below expr instead:
"	bufwinnr(FindBufferForName(fileName))
" TODO:
"   - fnamemodify() on Unix doesn't expand to full name if the filename doesn't
"     really exist on the filesystem.
"   - Support specifying arguments (with spaces) enclosed in "" or '' for
"     makeArgumentString. Just combine the arguments that are between "" or ''
"     and strip the quotes off.
"

if exists("loaded_genutils")
  finish
endif
let loaded_genutils = 1

" Make sure line-continuations won't cause any problem. This will be restored
"   at the end
let s:save_cpo = &cpo
set cpo&vim


let g:makeArgumentString = 'exec MakeArgumentString()'
let g:makeArgumentList = 'exec MakeArgumentList()'

" Execute the function return value to make a string containing all your
"   variable number of arguments that are passed to your function, which can
"   be used to pass the variable number of arguments further down to another
"   function. This by default generates a local variable named
"   'argumentString', which can be changed by passing a different name as the
"   first argument to this function.
" Uses __argCounter and __nextArg local variables, so make sure you don't have
"   variables with the same name in your function.
" Ex:
"   fu! s:IF(...)
"     exec MakeArgumentString()
"     exec "call Impl(1, " . argumentString . ")"
"   endfu
let s:makeArgumentString = ''
function! MakeArgumentString(...)
  if s:makeArgumentString == ''
    let s:makeArgumentString = ExtractFuncListing(s:myScriptId.
	  \ '_makeArgumentString', 0, 0)
  endif
  if a:0 > 0 && a:1 != ''
    return substitute(s:makeArgumentString, '\<argumentString\>', a:1, 'g')
  else
    return s:makeArgumentString
  endif
endfunction


" Execute the function return value to make a comma separated list of all the
"   variable number of arugments that were passed to your function. This by
"   default generates a local variable with the name 'argumentList', which can
"   be changed by passing a different name as the first argument to this
"   function. You can manipulate this variable and pass the string to
"   CreateArgString() function below to make an argument string which can be
"   used as mentioned above in "exec MakeArgumentString()". You can also use
"   the scripts that let you handle arrays to manipulate this string (such as
"   remove/insert args) before passing them on.
" Uses __argCounter and __argSeparator local variables, so make sure you don't
"   have variables with the same name in your function. You can also pass in a
"   second optional argument which is used as the argument separator instead
"   of the default ','. You need to make sure that the separator string itself
"   can't occur as part of arguments, or use a sequence of characters that is
"   hard to occur as separator.
" Ex: 
"   fu! s:IF(...)
"     exec MakeArgumentList()
"     exec "call Impl(1, " . CreateArgString(argumentList, ',') . ")"
"   endfu
let s:makeArgumentList = ''
function! MakeArgumentList(...)
  if s:makeArgumentList == ''
    let s:makeArgumentList = ExtractFuncListing(s:myScriptId.
	  \ '_makeArgumentList', 0, 0)
  endif
  if a:0 > 0 && a:1 != ''
    let mkArgLst = substitute(s:makeArgumentList, '\<argumentList\>', a:1, 'g')
    if a:0 > 1 && a:2 != ''
      let mkArgLst = substitute(s:makeArgumentList,
	    \ '\(\s\+let __argSeparator = \)[^'."\n".']*', "\\1'".a:2."'", '')
    endif
    return mkArgLst
  else
    return s:makeArgumentList
  endif
endfunction

" This function returns the body of the specified function ( the name should be
"   complete, including any scriptid prefix in case of a script local
"   function), without the function header and tail. You can also pass in the
"   number of additional lines to be removed from the head and or tail of the
"   function.
function! ExtractFuncListing(funcName, hLines, tLines)
  let listing = GetVimCmdOutput('func '.a:funcName)
  let listing = substitute(listing,
	\ '^\%(\s\|'."\n".'\)*function '.a:funcName.'([^)]*)'."\n", '', '')
  "let listing = substitute(listing, '\%(\s\|'."\n".'\)*endfunction\%(\s\|'."\n".'\)*$', '', '')
  " Leave the last newline character.
  let listing = substitute(listing, '\%('."\n".'\)\@<=\s*endfunction\s*$', '', '')
  let listing = substitute(listing, '\(\%(^\|'."\n".'\)\s*\)\@<=\d\+',
	\ '', 'g')
  if a:hLines > 0
    let listing = substitute(listing, '^\%([^'."\n".']*'."\n".'\)\{'.
	  \ a:hLines.'}', '', '')
  endif
  if a:tLines > 0
    let listing = substitute(listing, '\%([^'."\n".']*'."\n".'\)\{'.
	  \ a:tLines.'}$', '', '')
  endif
  return listing
endfunction

" Useful to collect arguments into a soft-array (see multvals on vim.sf.net)
"   and then pass them to a function later.
" You should make sure that the separator itself doesn't exist in the
"   arguments. You can use the return value same as the way argumentString
"   created by the "exec g:makeArgumentString" above is used. If the separator
"   is a pattern, you should pass in an optional additional argument, which
"   is an arbitrary string that is guaranteed to match the pattern, as a
"   sample separator (see multvals.vim for details).
" Usage:
"     let args = 'a b c' 
"     exec "call F(" . CreateArgString(args, ' ') . ")"
function! CreateArgString(argList, sep, ...)
  let sep = (a:0 == 0) ? a:sep : a:1
  call MvIterCreate(a:argList, a:sep, 'CreateArgString', sep)
  let argString = "'"
  while MvIterHasNext('CreateArgString')
    let nextArg = MvIterNext('CreateArgString')
    if a:sep != "'"
      let nextArg = substitute(nextArg, "'", "' . \"'\" . '", 'g')
    endif
    let argString = argString . nextArg . "', '"
  endwhile
  let argString = strpart(argString, 0, strlen(argString) - 3)
  call MvIterDestroy('CreateArgString')
  return argString
endfunction


" {{{
function! s:_makeArgumentString()
  let __argCounter = 1
  let argumentString = ''
  while __argCounter <= a:0
    let __nextArg = substitute(a:{__argCounter}, "'", "' . \"'\" . '", "g")
    let argumentString = argumentString . "'" . __nextArg . "'" .
	  \ ((__argCounter == a:0) ? '' : ', ')
    let __argCounter = __argCounter + 1
  endwhile
  unlet __argCounter
  if exists('__nextArg')
    unlet __nextArg
  endif
endfunction

function! s:_makeArgumentList()
  let __argCounter = 1
  let __argSeparator = ','
  let argumentList = ''
  while __argCounter <= a:0
    let argumentList = argumentList . a:{__argCounter}
    if __argCounter != a:0
      let argumentList = argumentList . __argSeparator
    endif
    let __argCounter = __argCounter + 1
  endwhile
  unlet __argCounter
  unlet __argSeparator
endfunction
" }}}


" Useful function to debug passing arguments to functions. See exactly what
"   you receive on the other side.
" Ex: :exec 'call DebugShowArgs('. CreateArgString("a 'b' c", ' ') . ')' 
function! DebugShowArgs(...)
  let i = 0
  let argString = ''
  while i < a:0
    let argString = argString . a:{i + 1} . ', '
    let i = i + 1
  endwhile
  let argString = strpart(argString, 0, strlen(argString) - 2)
  call input("Args: " . argString)
endfunction


"
" Return the number of windows open currently.
"
function! NumberOfWindows()
  let i = 1
  while winbufnr(i) != -1
    let i = i+1
  endwhile
  return i - 1
endfunction

" Find the window number for the buffer passed.
" The fileName argument is treated literally, unlike the bufnr() which treats
"   the argument as a regex pattern.
function! FindWindowForBuffer(bufferName, checkUnlisted)
  return bufwinnr(FindBufferForName(a:bufferName))
endfunction

" Returns the buffer number of the given fileName if it is already loaded.
" The fileName argument is treated literally, unlike the bufnr() which treats
"   the argument as a filename-pattern, however it first removes one level of
"   back-slashes from the bufferName.
function! FindBufferForName(fileName)
  let fileName = a:fileName
  " The name could be having extra backslashes to protect certain chars (such
  "   as '#' and '%'), so first expand them.
  let fileName = DeEscape(fileName)
  " Protect the regex characters that are not already protected.
  let fileName = substitute(fileName, '\\\@<!\%(\\\\\)*\([[,{\\]\)', '\\\1',
	\ 'g')
  let _isf = &isfname
  if v:version >= 602
    try
      set isfname-=\
      set isfname-=[
      let i = bufnr('^' . fileName . '$')
    finally
      let &isfname = _isf
    endtry
  else
    set isfname-=\
    set isfname-=[
    let i = bufnr('^' . fileName . '$')
    let &isfname = _isf
  endif
  return i
endfunction


" Given the window number, moves the cursor to that window.
function! MoveCursorToWindow(winno)
  if NumberOfWindows() != 1
    execute a:winno . " wincmd w"
  endif
endfunction


" Moves the current line such that it is going to be the nth line in the window
"   without changing the column position.
function! MoveCurLineToWinLine(n)
  normal! zt
  if a:n == 1
    return
  endif
  let _wrap = &l:wrap
  setl nowrap
  let n = a:n
  if n >= winheight(0)
    let n = winheight(0)
  endif
  let n = n - 1
  execute "normal! " . n . "\<C-Y>"
  let &l:wrap = _wrap
endfunction


" Turn on some buffer settings that make it suitable to be a scratch buffer.
function! SetupScratchBuffer()
  setlocal nobuflisted
  setlocal noswapfile
  setlocal buftype=nofile
  " Just in case, this will make sure we are always hidden.
  setlocal bufhidden=delete
endfunction


" Turns off those options that are set by diff to the current window.
"   Also removes the 'hor' option from scrollopt (which is a global option).
" Better alternative would be to close the window and reopen the buffer in a
"   new window. 
function! CleanDiffOptions()
  setlocal nodiff
  setlocal noscrollbind
  setlocal scrollopt-=hor
  setlocal wrap
  setlocal foldmethod=manual
  setlocal foldcolumn=0
endfunction


" This function is an alternative to exists() function, for those odd array
"   index names for which the built-in function fails. The var should be
"   accessible to this functions, so it should be a global variable.
"     if ArrayVarExists("array", id)
"       let val = array{id}
"     endif
if v:version >= 602
function! ArrayVarExists(varName, index)
  try
    exec "let test = " . a:varName . "{a:index}"
  catch /^Vim\%((\a\+)\)\=:E121/
    return 0
  endtry
  return 1
endfunction
else
function! ArrayVarExists(varName, index)
  let v:errmsg = ""
  silent! exec "let test = " . a:varName . "{a:index}"
  if !exists("test") || v:errmsg != ""
    let v:errmsg = ""
    return 0
  endif
  return 1
endfunction
endif


" Works like the built-in escape(), except that it escapes the passed in
"   characters only if they are not already escaped, so something like
"   'a\bc\\bd' would become 'a\bc\\\bd'. The chars value directly goes into
"   the [] collection, so it can be anything that is accepted in [].
function! Escape(str, chars)
  return substitute(a:str, '\\\@<!\(\\\\\)*\([' . a:chars .']\)', '\1\\\2', 'g')
endfunction


" Works like the reverse of the builtin escape() function. Un-escapes only the
"   specified characters. The chars value directly goes into the []
"   collection, so it can be anything that is accepted in [].
function! UnEscape(str, chars)
  return substitute(a:str, '\\\@<!\(\\\\\)*\\\([' . a:chars . ']\)',
	\ '\1\2', 'g')
endfunction


" Works like the reverse of the built-in escape() function. De-escapes all the
"   escaped characters. Essentially removes one level of escaping from the
"   string, so something like: 'a\b\\\\c\\d' would become 'ab\\c\d'.
function! DeEscape(str)
  let str = a:str
  let str = substitute(str, '\\\(\\\|[^\\]\)', '\1', 'g')
  return str
endfunction


" Expands the string for the special characters. The return value should
"   essentially be would you see if it was a string constant with
"   double-quotes.
function! s:ExpandStr(str)
  let str = substitute(a:str, '"', '\\"', 'g')
  exec "let str = \"" . str . "\"" 
endfunction


" Returns 1 if preview window is open or 0 if not.
if v:version >= 602
function! GetPreviewWinnr()
  let _eventignore = &eventignore
  let curWinNr = winnr()
  let winnr = -1
  try
    set eventignore+=WinLeave,WinEnter
    exec "wincmd P"
    let winnr = winnr()
  catch /^Vim\%((\a\+)\)\=:E441/
    " Ignore, winnr is already set to -1.
  finally
    if winnr() != curWinNr
      exec curWinNr.'wincmd w'
    endif
    let &eventignore = _eventignore
  endtry
  return winnr
endfunction
endif


"
" Saves the heights and widths of the currently open windows for restoring
"   later.
"
function! SaveWindowSettings()
  call SaveWindowSettings2(s:myScriptId, 1)
endfunction


"
" Restores the heights of the windows from the information that is saved by
"  SaveWindowSettings().
"
function! RestoreWindowSettings()
  call RestoreWindowSettings2(s:myScriptId)
endfunction


function! ResetWindowSettings()
  call ResetWindowSettings2(s:myScriptId)
endfunction


" Same as SaveWindowSettings, but uses the passed in scriptid to create a
"   private copy for the calling script. Pass in a unique scriptid to avoid
"   conflicting with other callers. If overwrite is zero and if the settings
"   are already stored for the passed in sid, it will overwriting previously
"   saved settings.
function! SaveWindowSettings2(sid, overwrite)
  if ArrayVarExists("s:winSettings", a:sid) && ! a:overwrite
    return
  endif

  let s:winSettings{a:sid} = ""
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",",
          \ NumberOfWindows())
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",", &lines)
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",", &columns)
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",", winnr())
  let i = 1
  while winbufnr(i) != -1
    let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",",
            \ winheight(i))
    let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",",
            \ winwidth(i))
    let i = i + 1
  endwhile
  "let g:savedWindowSettings = s:winSettings{a:sid} " Debug.
endfunction


" Same as RestoreWindowSettings, but uses the passed in scriptid to get the
"   settings.
function! RestoreWindowSettings2(sid)
  "if ! exists("s:winSettings" . a:sid)
  if ! ArrayVarExists("s:winSettings", a:sid)
    return
  endif

  call MvIterCreate(s:winSettings{a:sid}, ",", "savedWindowSettings")
  let nWindows = MvIterNext("savedWindowSettings")
  if nWindows != NumberOfWindows()
    unlet s:winSettings{a:sid}
    call MvIterDestroy("savedWindowSettings")
    return
  endif
  let orgLines = MvIterNext("savedWindowSettings")
  let orgCols = MvIterNext("savedWindowSettings")
  let activeWindow = MvIterNext("savedWindowSettings")
  
  let winNo = 1
  while MvIterHasNext("savedWindowSettings")
    let height = MvIterNext("savedWindowSettings")
    let width = MvIterNext("savedWindowSettings")
    call MoveCursorToWindow(winNo)
    exec 'resize ' . ((&lines * height + (orgLines / 2)) / orgLines)
    exec 'vert resize ' . ((&columns * width + (orgCols / 2)) / orgCols)
    let winNo = winNo + 1
  endwhile
  
  " Restore the current window.
  call MoveCursorToWindow(activeWindow)
  call ResetWindowSettings2(a:sid)
  "unlet g:savedWindowSettings " Debug.
  call MvIterDestroy("savedWindowSettings")
endfunction


function! ResetWindowSettings2(sid)
  if ArrayVarExists("s:winSettings", a:sid)
    unlet s:winSettings{a:sid}
  endif
endfunction


" Cleanup file name such that two *cleaned up* file names are easy to be
"   compared. This probably works only on windows and unix platforms.
function! CleanupFileName(fileName)
  let fileName = a:fileName

  " If filename starts with an ~.
  " The below case takes care of this also.
  "if match(fileName, '^\~') == 0
  "  let fileName = substitute(fileName, '^\~', escape($HOME, '\'), '')
  "endif

  " Expand relative paths and paths containing relative components.
  if ! PathIsAbsolute(fileName)
    let fileName = fnamemodify(fileName, ":p")
  endif

  " Remove multiple path separators.
  if has("win32")
    let fileName=substitute(fileName, '\\', '/', "g")
  elseif OnMS()
    let fileName=substitute(fileName, '\\\{2,}', '\', "g")
  endif
  let fileName=substitute(fileName, '/\{2,}', '/', "g")

  " Remove ending extra path separators.
  let fileName=substitute(fileName, '/$', '', "")
  let fileName=substitute(fileName, '\$', '', "")

  if OnMS()
    let fileName=substitute(fileName, '^[A-Z]:', '\L&', "")

    " Add drive letter if missing (just in case).
    if match(fileName, '^/') == 0
      let curDrive = substitute(getcwd(), '^\([a-zA-Z]:\).*$', '\L\1', "")
      let fileName = curDrive . fileName
    endif
  endif
  return fileName
endfunction
"echo CleanupFileName('\\a///b/c\')
"echo CleanupFileName('C:\a/b/c\d')
"echo CleanupFileName('a/b/c\d')
"echo CleanupFileName('~/a/b/c\d')
"echo CleanupFileName('~/a/b/../c\d')


function! OnMS()
  return has("win32") || has("dos32") || has("win16") || has("dos16") ||
       \ has("win95")
endfunction


function! PathIsAbsolute(path)
  let absolute=0
  if has("unix") || OnMS()
    if match(a:path, "^/") == 0
      let absolute=1
    endif
  endif
  if (! absolute) && OnMS()
    if match(a:path, "^\\") == 0
      let absolute=1
    endif
  endif
  if (! absolute) && OnMS()
    if match(a:path, "^[A-Za-z]:") == 0
      let absolute=1
    endif
  endif
  return absolute
endfunction


function! PathIsFileNameOnly(path)
  return (match(a:path, "\\") < 0) && (match(a:path, "/") < 0)
endfunction


" Copy this method into your script and rename it to find the script id of the
"   current script.
function! s:MyScriptId()
  map <SID>xx <SID>xx
  let s:sid = maparg("<SID>xx")
  unmap <SID>xx
  return substitute(s:sid, "xx$", "", "")
endfunction
let s:myScriptId = s:MyScriptId()
delfunc s:MyScriptId " Not required any more.


"" --- START save/restore position. {{{

" characters that must be escaped for a regular expression
let s:escregexp = '[/*^$.~\'

" This method tries to save the position along with the line context if
"   possible. This is like the vim builtin marker. Pass in a unique scriptid
"   to avoid conflicting with other callers.
function! SaveSoftPosition(scriptid)
  let s:startline_{a:scriptid} = getline(".")
  call SaveHardPosition(a:scriptid)
endfunction

function! RestoreSoftPosition(scriptid)
  0
  if search('\m^'.escape(s:startline_{a:scriptid},s:escregexp),'W') <= 0
    call RestoreHardPosition(a:scriptid)
  else
    execute "normal!" s:col_{a:scriptid} . "|"
    call MoveCurLineToWinLine(s:winline_{a:scriptid})
  endif
endfunction

function! ResetSoftPosition(scriptid)
  unlet s:startline_{a:scriptid}
endfunction

" A synonym for SaveSoftPosition.
function! SaveHardPositionWithContext(scriptid)
  call SaveSoftPosition(a:scriptid)
endfunction

" A synonym for RestoreSoftPosition.
function! RestoreHardPositionWithContext(scriptid)
  call RestoreSoftPosition(a:scriptid)
endfunction

" A synonym for ResetSoftPosition.
function! ResetHardPositionWithContext(scriptid)
  call ResetSoftPosition(a:scriptid)
endfunction

" Useful when you want to go to the exact (line, col), but marking will not
"   work, or if you simply don't want to disturb the marks. Pass in a unique
"   scriptid.
function! SaveHardPosition(scriptid)
  let s:col_{a:scriptid} = virtcol(".")
  let s:lin_{a:scriptid} = line(".")
  let s:winline_{a:scriptid} = winline()
endfunction

function! RestoreHardPosition(scriptid)
  " This doesn't take virtual column.
  "call cursor(s:lin_{a:scriptid}, s:col_{a:scriptid})
  execute s:lin_{a:scriptid}
  execute "normal!" s:col_{a:scriptid} . "|"
  call MoveCurLineToWinLine(s:winline_{a:scriptid})
endfunction

" Return the line number of the previously saved position for the scriptid.
function! GetLinePosition(scriptid)
  return s:lin_{a:scriptid}
endfunction

" Return the column number of the previously saved position for the scriptid.
function! GetColPosition(scriptid)
  return s:col_{a:scriptid}
endfunction

function! ResetHardPosition(scriptid)
  unlet s:col_{a:scriptid}
  unlet s:lin_{a:scriptid}
  unlet s:winline_{a:scriptid}
endfunction

function! IsPositionSet(scriptid)
  return exists('s:col_' . a:scriptid)
endfunction

"" --- END save/restore position. }}}



""
"" --- START: Notify window close --
""


"
" When the window with the title windowTitle is closed, the global function
"   functionName is called with the title as an argument, and the entries are
"   removed, so if you need future notifications, then you need to register
"   again. The windowTitle here is nothing but the buffer with the name that
"   you are interested in.
"
function! AddNotifyWindowClose(windowTitle, functionName)
  " The window name could be having extra backslashes to protect certain
  " chars, so first expand them.
  let bufName = DeEscape(a:windowTitle)
  call s:AddNotifyWindowClose(bufName, a:functionName)
endfunction

function! s:AddNotifyWindowClose(windowTitle, functionName)
  let bufName = a:windowTitle

  " Make sure there is only one entry per window title.
  if exists("s:notifyWindowTitles") && s:notifyWindowTitles != ""
    call s:RemoveNotifyWindowClose(bufName)
  endif

  if !exists("s:notifyWindowTitles")
    " Both separated by :.
    let s:notifyWindowTitles = ""
    let s:notifyWindowFunctions = ""
  endif

  let s:notifyWindowTitles = MvAddElement(s:notifyWindowTitles, ";", bufName)
  let s:notifyWindowFunctions = MvAddElement(s:notifyWindowFunctions, ";",
          \ a:functionName)

  let g:notifyWindowTitles = s:notifyWindowTitles " Debug.
  let g:notifyWindowFunctions = s:notifyWindowFunctions " Debug.

  " Start listening to events.
  aug NotifyWindowClose
    au!
    au WinEnter * :call CheckWindowClose()
  aug END
endfunction


function! RemoveNotifyWindowClose(windowTitle)
  " The window name could be having extra backslashes to protect certain
  " chars, so first expand them.
  let bufName = DeEscape(a:windowTitle)
  call s:RemoveNotifyWindowClose(bufName)
endfunction

function! s:RemoveNotifyWindowClose(windowTitle)
  let bufName = a:windowTitle

  if !exists("s:notifyWindowTitles")
    return
  endif

  if MvContainsElement(s:notifyWindowTitles, ";", bufName)
    let index = MvIndexOfElement(s:notifyWindowTitles, ";", bufName)
    let s:notifyWindowTitles = MvRemoveElementAt(s:notifyWindowTitles, ";",
            \ index)
    let s:notifyWindowFunctions = MvRemoveElementAt(s:notifyWindowFunctions,
            \ ";", index)

    if s:notifyWindowTitles == ""
      unlet s:notifyWindowTitles
      unlet s:notifyWindowFunctions
      unlet g:notifyWindowTitles " Debug.
      unlet g:notifyWindowFunctions " Debug.
  
      aug NotifyWindowClose
        au!
      aug END
    endif
  endif
endfunction


function! CheckWindowClose()
  if !exists("s:notifyWindowTitles") || s:notifyWindowTitles == ""
    return
  endif

  " First make an array with all the existing window titles.
  "let i = 1
  "let currentWindows = ""
  "while winbufnr(i) != -1
  "  let bufname = bufname(winbufnr(i))
  "  if bufname != ""
  "    " For performance reasons.
  "    "let currentWindows = MvAddElement(currentWindows, ";", bufname)
  "    let currentWindows = currentWindows . bufname . ";"
  "  endif
  "  let i = i+1
  "endwhile
  "call input("currentWindows: " . currentWindows)

  " Now iterate over all the registered window titles and see which one's are
  "   closed.
  let i = 0 " To track the element index.
  " Take a copy and modify these if needed, as we are not supposed to modify
  "   the main arrays while iterating over them.
  let processedElements = ""
  call MvIterCreate(s:notifyWindowTitles, ";", "NotifyWindowClose")
  while MvIterHasNext("NotifyWindowClose")
    let nextWin = MvIterNext("NotifyWindowClose")
    if bufwinnr(nextWin) == -1
    "if ! MvContainsElement(currentWindows, ";", nextWin)
      let funcName = MvElementAt(s:notifyWindowFunctions, ";", i)
      let cmd = "call " . funcName . "(\"" . nextWin . "\")"
      "call input("cmd: " . cmd)
      exec cmd

      " Remove these entries as these are already processed.
      let processedElements = MvAddElement(processedElements, ";", nextWin)
    endif
  endwhile
  call MvIterDestroy("NotifyWindowClose")

  call MvIterCreate(processedElements, ";", "NotifyWindowClose")
  while MvIterHasNext("NotifyWindowClose")
    let nextWin = MvIterNext("NotifyWindowClose")
    call RemoveNotifyWindowClose(nextWin)
  endwhile
  call MvIterDestroy("NotifyWindowClose")
endfunction


"function! NotifyWindowCloseF(title)
"  call input(a:title . " closed")
"endfunction
"
"function! RunNotifyWindowCloseTest()
"  split ABC
"  split X Y Z
"  call AddNotifyWindowClose("ABC", "NotifyWindowCloseF")
"  call AddNotifyWindowClose("X Y Z", "NotifyWindowCloseF")
"  call input("notifyWindowTitles: " . s:notifyWindowTitles)
"  call input("notifyWindowFunctions: " . s:notifyWindowFunctions)
"  au WinEnter
"  split b
"  call input("Starting the tests, you should see two notifications:")
"  quit
"  quit
"  quit
"endfunction


""
"" --- END: Notify window close --
""



"
" TODO: For large ranges, the cmd can become too big, so make it one cmd per
"       line.
" Displays the given line(s) from the current file in the command area (i.e.,
" echo), using that line's syntax highlighting (i.e., WYSIWYG).
"
" If no line number is given, display the current line.
"
" Originally,
" From: Gary Holloway "gary at castandcrew dot com"
" Date: Wed, 16 Jan 2002 14:31:56 -0800
"
function! ShowLinesWithSyntax() range
  " This makes sure we start (subsequent) echo's on the first line in the
  " command-line area.
  "
  echo ''

  let cmd        = ''
  let prev_group = ' x '     " Something that won't match any syntax group.

  let show_line = a:firstline
  let isMultiLine = ((a:lastline - a:firstline) > 1)
  while show_line <= a:lastline
    let cmd = ''
    let length = strlen(getline(show_line))
    let column = 1

    while column <= length
      let group = synIDattr(synID(show_line, column, 1), 'name')
      if group != prev_group
        if cmd != ''
          let cmd = cmd . "'|"
        endif
        let cmd = cmd . 'echohl ' . (group == '' ? 'NONE' : group) . "|echon '"
        let prev_group = group
      endif
      let char = strpart(getline(show_line), column - 1, 1)
      if char == "'"
        let char = "'."'".'"
      endif
      let cmd = cmd . char
      let column = column + 1
    endwhile

    try
      exec cmd."\n'"
    catch
      echo ''
    endtry
    let show_line = show_line + 1
  endwhile
  echohl NONE
endfunction


function! AlignWordWithWordInPreviousLine()
  "First go to the first col in the word.
  if getline('.')[col('.') - 1] =~ '\s'
    normal! w
  else
    normal! "_yiw
  endif
  let orgVcol = virtcol('.')
  let prevLnum = prevnonblank(line('.') - 1)
  if prevLnum == -1
    return
  endif
  let prevLine = getline(prevLnum)

  " First get the column to align with.
  if prevLine[orgVcol - 1] =~ '\s'
    " column starts from 1 where as index starts from 0.
    let nonSpaceStrInd = orgVcol " column starts from 1 where as index starts from 0.
    while prevLine[nonSpaceStrInd] =~ '\s'
      let nonSpaceStrInd = nonSpaceStrInd + 1
    endwhile
  else
    if strlen(prevLine) < orgVcol
      let nonSpaceStrInd = strlen(prevLine) - 1
    else
      let nonSpaceStrInd = orgVcol - 1
    endif

    while prevLine[nonSpaceStrInd - 1] !~ '\s' && nonSpaceStrInd > 0
      let nonSpaceStrInd = nonSpaceStrInd - 1
    endwhile
  endif
  let newVcol = nonSpaceStrInd + 1 " Convert to column number.

  if orgVcol > newVcol " We need to reduce the spacing.
    let sub = strpart(getline('.'), newVcol - 1, (orgVcol - newVcol))
    if sub =~ '^\s\+$'
      " Remove the excess space.
      exec 'normal! ' . newVcol . '|'
      exec 'normal! ' . (orgVcol - newVcol) . 'x'
    endif
  elseif orgVcol < newVcol " We need to insert spacing.
    exec 'normal! ' . orgVcol . '|'
    exec 'normal! ' . (newVcol - orgVcol) . 'i '
  endif
endfunction


" This function shifts the word in the space without moving the following words.
"   Doesn't work for tabs.
function! ShiftWordInSpace(dir)
  if a:dir == 1 " forward.
    " If currently on <Space>...
    if getline(".")[col(".") - 1] == " "
      let move1 = 'wf '
    else
      " If next col is a 
      "if getline(".")[col(".") + 1]
      let move1 = 'f '
    endif
    let removeCommand = "x"
    let pasteCommand = "bi "
    let move2 = 'w'
    let offset = 0
  else " backward.
    " If currently on <Space>...
    if getline(".")[col(".") - 1] == " "
      let move1 = 'w'
    else
      let move1 = '"_yiW'
    endif
    let removeCommand = "hx"
    let pasteCommand = 'h"_yiwEa '
    let move2 = 'b'
    let offset = -3
  endif

  let savedCol = col(".")
  exec "normal!" move1
  let curCol = col(".")
  let possible = 0
  " Check if there is a space at the end.
  if col("$") == (curCol + 1) " Works only for forward case, as expected.
    let possible = 1
  elseif getline(".")[curCol + offset] == " "
    " Remove the space from here.
    exec "normal!" removeCommand
    let possible = 1
  endif

  " Move back into the word.
  "exec "normal!" savedCol . "|"
  if possible == 1
    exec "normal!" pasteCommand
    exec "normal!" move2
  else
    " Move to the original location.
    exec "normal!" savedCol . "|"
  endif
endfunction


function! CenterWordInSpace()
  let line = getline('.')
  let orgCol = col('.')
  " If currently on <Space>...
  if line[orgCol - 1] == " "
    let matchExpr = ' *\%'. orgCol . 'c *\w\+ \+'
  else
    let matchExpr = ' \+\(\w*\%' . orgCol . 'c\w*\) \+'
  endif
  let matchInd = match(line, matchExpr)
  if matchInd == -1
    return
  endif
  let matchStr = matchstr(line,  matchExpr)
  let nSpaces = strlen(substitute(matchStr, '[^ ]', '', 'g'))
  let word = substitute(matchStr, ' ', '', 'g')
  let middle = nSpaces / 2
  let left = nSpaces - middle
  let newStr = ''
  while middle > 0
    let newStr = newStr . ' '
    let middle = middle - 1
  endwhile
  let newStr = newStr . word
  while left > 0
    let newStr = newStr . ' '
    let left = left - 1
  endwhile

  let newLine = strpart(line, 0, matchInd)
  let newLine = newLine . newStr
  let newLine = newLine . strpart (line, matchInd + strlen(matchStr))
  call setline(line('.'), newLine)
endfunction


" If lhs is already mapped, this function makes sure rhs is appended to it
"   instead of overwriting it. If you are rhs has any script local functions,
"   make sure you use the <SNR>\d\+_ prefix instead of the <SID> prefix (or the
"   <SID> will be replaced by the SNR number of genutils script, instead of
"   yours).
" mapMode is used to prefix to "oremap" and used as the map command. E.g., if
"   mapMode is 'n', then the function call results in the execution of noremap
"   command.
function! MapAppendCascaded(lhs, rhs, mapMode)

  " Determine the map mode from the map command.
  let mapChar = strpart(a:mapMode, 0, 1)

  " Check if this is already mapped.
  let oldrhs = maparg(a:lhs, mapChar)
  if oldrhs != ""
    let self = oldrhs
  else
    let self = a:lhs
  endif
  "echomsg a:mapMode . "oremap" . " " . a:lhs . " " . self . a:rhs
  exec a:mapMode . "oremap" a:lhs self . a:rhs
endfunction


" Just a convenience function. This returns the output of the command as a
" string, without corrupting any registers. Returns empty string on errors.
" Check for v:errmsg after calling this function for any error messages.
if v:version >= 602
function! GetVimCmdOutput(cmd)
  let v:errmsg = ''
  let output = ''
  let _z = @z
  try
    redir @z
    silent exec a:cmd
  catch /.*/
    let v:errmsg = substitute(v:exception, '^[^:]\+:', '', '')
  finally
    redir END
    if v:errmsg == ''
      let output = @z
    endif
    let @z = _z
  endtry
  return output
endfunction
endif


" Clear the contents of the current buffer in an optimum manner.
function! OptClearBuffer()
  " Go as far as possible in the undo history to conserve Vim resources.
  let _modifiable = &l:modifiable
  let _undolevels = &undolevels
  try
    setl modifiable
    set undolevels=-1
    silent! 0,$delete _
  finally
    let &undolevels = _undolevels
    let &l:modifiable = _modifiable
  endtry
endfunction


"" START: Sorting support. {{{
""

"
" Comapare functions.
"

function! s:CmpByLineLengthNname(line1, line2, direction)
  return CmpByLineLengthNname(a:line1, a:line2, a:direction)
endfunction

function! CmpByLineLengthNname(line1, line2, direction)
  let cmp = CmpByLength(a:line1, a:line2, a:direction)
  if cmp == 0
    let cmp = CmpByString(a:line1, a:line2, a:direction)
  endif
  return cmp
endfunction

function! s:CmpByLength(line1, line2, direction)
  return CmpByLength(a:line1, a:line2, a:direction)
endfunction


function! CmpByLength(line1, line2, direction)
  let len1 = strlen(a:line1)
  let len2 = strlen(a:line2)
  return a:direction * (len1 - len2)
endfunction

function! s:CmpJavaImports(line1, line2, direction)
  return CmpJavaImports(a:line1, a:line2, a:direction)
endfunction

" Compare first by name and then by length.
" Useful for sorting Java imports.
function! CmpJavaImports(line1, line2, direction)
  " FIXME: Simplify this.
  if stridx(a:line1, '.') == -1
    let pkg1 = ''
    let cls1 = substitute(a:line1, '.* \(^[ ]\+\)', '\1', '')
  else
    let pkg1 = substitute(a:line1, '^\(.*\.\)[^. ;]\+.*$', '\1', '')
    let cls1 = substitute(a:line1, '^.*\.\([^. ;]\+\).*$', '\1', '')
  endif
  if stridx(a:line2, '.') == -1
    let pkg2 = ''
    let cls2 = substitute(a:line2, '.* \(^[ ]\+\)', '\1', '')
  else
    let pkg2 = substitute(a:line2, '^\(.*\.\)[^. ;]\+.*$', '\1', '')
    let cls2 = substitute(a:line2, '^.*\.\([^. ;]\+\).*$', '\1', '')
  endif

  let cmp = CmpByString(pkg1, pkg2, a:direction)
  if cmp == 0
    let cmp = CmpByLength(cls1, cls2, a:direction)
  endif
  return cmp
endfunction

function! s:CmpByString(line1, line2, direction)
  return CmpByString(a:line1, a:line2, a:direction)
endfunction

function! CmpByString(line1, line2, direction)
  if a:line1 < a:line2
    return -a:direction
  elseif a:line1 > a:line2
    return a:direction
  else
    return 0
  endif
endfunction

function! CmpByStringIgnoreCase(line1, line2, direction)
  let line1 = substitute(a:line1, '.', '\u&', 'g')
  let line2 = substitute(a:line2, '.', '\u&', 'g')
  if line1 < line2
    return -a:direction
  elseif line1 > line2
    return a:direction
  else
    return 0
  endif
endfunction

function! s:CmpByNumber(line1, line2, direction)
  return CmpByNumber(a:line1, a:line2, a:direction)
endfunction

function! CmpByNumber(line1, line2, direction)
  let num1 = a:line1 + 0
  let num2 = a:line2 + 0

  if num1 < num2
    return -a:direction
  elseif num1 > num2
    return a:direction
  else
    return 0
  endif
endfunction

"
" To Sort a range of lines, pass the range to QSort() along with the name of a
" function that will compare two lines.
"
function! QSort(cmp, direction) range
  call s:QSortR(a:firstline, a:lastline, a:cmp, a:direction,
	\ 's:BufLineAccessor', 's:BufLineSwapper', '')
endfunction

" A more generic sort routine, that will let you provide your own accessor and
"   swapper, so that you can extend the sorting to something beyond the default
"   buffer lines.
function! QSort2(start, end, cmp, direction, accessor, swapper, context)
  call s:QSortR(a:start, a:end, a:cmp, a:direction, a:accessor, a:swapper,
	\ a:context)
endfunction

" The default swapper that swaps lines in the current buffer.
function! s:BufLineSwapper(line1, line2, context)
  let str2 = getline(a:line1)
  call setline(a:line1, getline(a:line2))
  call setline(a:line2, str2)
endfunction

" The default accessor that returns lines from the current buffer.
function! s:BufLineAccessor(line, context)
  return getline(a:line)
endfunction

"
" Sort lines.  QSortR() is called recursively.
"
function! s:QSortR(start, end, cmp, direction, accessor, swapper, context)
  if a:end > a:start
    let low = a:start
    let high = a:end

    " Arbitrarily establish partition element at the midpoint of the data.
    exec "let midStr = " . a:accessor . "(" . ((a:start + a:end) / 2) .
	  \ ", a:context)"

    " Loop through the data until indices cross.
    while low <= high

      " Find the first element that is greater than or equal to the partition
      "   element starting from the left Index.
      while low < a:end
	exec "let str = " . a:accessor . "(" . low .  ", a:context)"
        exec "let result = " . a:cmp . "(str, midStr, " . a:direction . ")"
        if result < 0
          let low = low + 1
        else
          break
        endif
      endwhile

      " Find an element that is smaller than or equal to the partition element
      "   starting from the right Index.
      while high > a:start
	exec "let str = " . a:accessor . "(" . high .  ", a:context)"
        exec "let result = " . a:cmp . "(str, midStr, " . a:direction . ")"
        if result > 0
          let high = high - 1
        else
          break
        endif
      endwhile

      " If the indexes have not crossed, swap.
      if low <= high
        " Swap lines low and high.
	exec "call " . a:swapper . "(" . high . ", " . low . ", a:context)"
        let low = low + 1
        let high = high - 1
      endif
    endwhile

    " If the right index has not reached the left side of data must now sort
    "   the left partition.
    if a:start < high
      call s:QSortR(a:start, high, a:cmp, a:direction, a:accessor, a:swapper,
	    \ a:context)
    endif

    " If the left index has not reached the right side of data must now sort
    "   the right partition.
    if low < a:end
      call s:QSortR(low, a:end, a:cmp, a:direction, a:accessor, a:swapper,
	    \ a:context)
    endif
  endif
endfunction

" Return the line number where given line can be inserted in the current
"   buffer. This can also be interpreted as the line in the current buffer
"   after which the new line should go.
" Assumes that the lines are already sorted in the given direction using the
"   given comparator.
function! BinSearchForInsert(start, end, line, cmp, direction)
  return BinSearchForInsert2(a:start, a:end, a:line, a:cmp, a:direction,
	\ 's:BufLineAccessor', '')
endfunction

" A more generic implementation which doesn't restrict the search to a buffer.
function! BinSearchForInsert2(start, end, line, cmp, direction, accessor,
      \ context)
  let start = a:start - 1
  let end = a:end
  while start < end
    let middle = (start + end + 1) / 2
    exec "let str = " . a:accessor . "(" . middle .  ", a:context)"
    exec "let result = " . a:cmp . "(str, a:line, " . a:direction . ")"
    if result < 0
      let start = middle
    else
      let end = middle - 1
    endif
  endwhile
  return start
endfunction

""" END: Sorting support. }}}


" Eats character if it matches the given pattern.
"
" Originally,
" From: Benji Fisher <fisherbb@bc.edu>
" Date: Mon, 25 Mar 2002 15:05:14 -0500
"
" Based on Bram's idea of eating a character while type <Space> to expand an
"   abbreviation. This solves the problem with abbreviations, where we are
"   left with an extra space after the expansion.
" Ex:
"   inoreabbr \stdout\ System.out.println("");<Left><Left><Left><C-R>=EatChar('\s')<CR>
function! EatChar(pat)
   let c = nr2char(getchar())
   return (c =~ a:pat) ? '' : c
endfun


" Can return a spacer from 0 to 80 characters width.
let s:spacer= "                                                               ".
      \ "                           "
function! GetSpacer(width)
  return strpart(s:spacer, 0, a:width)
endfunction


" Convert Roman numerals to decimal. Doesn't detect format errors.
"
" Originally,
" From: "Preben Peppe Guldberg" <c928400@student.dtu.dk>
" Date: Fri, 10 May 2002 14:28:19 +0200
"
" START: Roman2Decimal {{{
let s:I = 1
let s:V = 5
let s:X = 10
let s:L = 50
let s:C = 100
let s:D = 500
let s:M = 1000

fun! s:Char2Num(c)
    " A bit of magic on empty strings
    if a:c == ""
        return 0
    endif
    exec 'let n = s:' . toupper(a:c)
    return n
endfun

fun! Roman2Decimal(str)
    if a:str !~? '^[IVXLCDM]\+$'
        return a:str
    endif
    let sum = 0
    let i = 0
    let n0 = s:Char2Num(a:str[i])
    let len = strlen(a:str)
    while i < len
        let i = i + 1
        let n1 = s:Char2Num(a:str[i])
        " Magic: n1=0 when i exceeds len
        if n1 > n0
            let sum = sum - n0
        else
            let sum = sum + n0
        endif
        let n0 = n1
    endwhile
    return sum
endfun
" END: Roman2Decimal }}}


" BEGIN: Relative path {{{
" Find common path component of two filenames, based on the tread, "computing
" relative path".
" Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
function! CommonPath(path1, path2)
  let path1 = CleanupFileName(a:path1)
  let path2 = CleanupFileName(a:path2)
  return CommonString(path1, path2)
endfunction


function! CommonString(str1, str2)
  let str1 = CleanupFileName(a:str1)
  let str2 = CleanupFileName(a:str2)
  if str1 == str2
    return str1
  endif
  let n = 0
  while str1[n] == str2[n]
    let n = n+1
  endwhile
  return strpart(str1, 0, n)
endfunction


function! RelPathFromFile(srcFile, tgtFile)
  return RelPathFromDir(fnamemodify(a:srcFile, ':r'), a:tgtFile)
endfunction


function! RelPathFromDir(srcDir, tgtFile)
  let cleanDir = CleanupFileName(a:srcDir)
  let cleanFile = CleanupFileName(a:tgtFile)
  let cmnPath = CommonPath(cleanDir, cleanFile)
  let shortDir = strpart(cleanDir, strlen(cmnPath))
  let shortFile = strpart(cleanFile, strlen(cmnPath))
  let relPath = substitute(substitute(shortDir, '[^/]\+$', '', ''),
	\ '[^/]\+', '..', 'g')
  return relPath . shortFile
endfunction

" END: Relative path }}}


" BEGIN: Persistent settings {{{
if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
  " Make sure the '!' option to store global variables that are upper cased are
  "   stored in viminfo file. Make sure it is the first option, so that it will
  "   not interfere with the 'n' option ("Todd J. Cosgrove"
  "     <todd dot cosgrove at softechnics dot com>).
  set viminfo^=!
endif

" The pluginName and persistentVar have to be unique and are case insensitive.
" Should be called from VimLeavePre. This simply creates a global variable which
"   will be persisted by Vim through viminfo. The variable can be read back in
"   the next session by the plugin using GetPersistentVar() function. The
"   pluginName is to provide a name space for different plugins, and avoid
"   conflicts in using the same persistentVar name.
" This feature uses the '!' option of viminfo, to avoid storing all the
"   temporary and other plugin specific global variables getting saved.
function! PutPersistentVar(pluginName, persistentVar, value)
  if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
    let globalVarName = s:PersistentVarName(a:pluginName, a:persistentVar)
    exec 'let ' . globalVarName . " = '" . a:value . "'"
  endif
endfunction

" Should be called from VimEnter. Simply reads the gloval variable for the
"   value and returns it. Removed the variable from global space before
"   returning the value, so should be called only once.
function! GetPersistentVar(pluginName, persistentVar, default)
  if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
    let globalVarName = s:PersistentVarName(a:pluginName, a:persistentVar)
    if (exists(globalVarName))
      exec 'let value = ' . globalVarName
      exec 'unlet ' . globalVarName
    else
      let value = a:default
    endif
    return value
  else
    return default
  endif
endfunction

function! s:PersistentVarName(pluginName, persistentVar)
  return 'g:GU_' . toupper(a:pluginName) . '_' . toupper(a:persistentVar)
endfunction
" END: Persistent settings }}}


" FileChangedShell handling {{{
let s:fcShellPreFuncs = ''
let s:fcShellFuncs = ''

" The function can return -1 to mean use default autoread and 0 to mean
" noautoread and 1 to mean autoread. The return value of all functions is ORed
" for the effective autoread.
function! AddToFCShellPre(funcName)
  let s:fcShellPreFuncs = MvAddElement(s:fcShellPreFuncs, ',', a:funcName)
endfunction

function! RemoveFromFCShellPre(funcName)
  let s:fcShellPreFuncs = MvRemoveElement(s:fcShellPreFuncs, ',', a:funcName)
endfunction

function! AddToFCShell(funcName)
  let s:fcShellFuncs = MvAddElement(s:fcShellFuncs, ',', a:funcName)
endfunction

function! RemoveFromFCShell(funcName)
  let s:fcShellFuncs = MvRemoveElement(s:fcShellFuncs, ',', a:funcName)
endfunction

let s:defFCShellInstalled = 0
function! DefFCShellInstall()
  if ! s:defFCShellInstalled
    aug DefFCShell
    au!
    au FileChangedShell * nested call DefFileChangedShell()
    aug END
  endif
  let s:defFCShellInstalled = s:defFCShellInstalled + 1
endfunction

function! DefFCShellUninstall()
  let s:defFCShellInstalled = s:defFCShellInstalled - 1
  if ! s:defFCShellInstalled
    aug DefFCShell
    au!
    aug END
  endif
endfunction

" This function emulates the Vim's default behavior when a |timestamp| change
"   is detected. Use the above hooks to have your customized operations done
"   during this event. From your callback methods, return 1 to mean autoread,
"   0 to mean noautoread and -1 to mean system default. The return value of
"   this method is 1 if the file was reloaded and 0 otherwise.
function! DefFileChangedShell()
  let autoread = s:InvokeFuncs(s:fcShellPreFuncs)
  let bufNo = expand("<abuf>") + 0
  if getbufvar(bufNo, '&modified')
    let option = confirm("W12: Warning: File \"" . expand("<afile>") .
          \ "\" has changed and the buffer was changed in Vim as well",
          \ "&OK\n&Load File", 1, "Question")
  elseif ! autoread
    let option = confirm("W11: Warning: File \"" . expand("<afile>") .
          \ "\" has changed since editing started", "&OK\n&Load File", 1,
          \ "Question")
  else " if autoread
    let option = 2
  endif
  if option == 2
    let alternativeFile = 0
    let orgWin = winnr()
    if bufNo != winbufnr(0)
      let win = bufwinnr(bufNo)
      if win != -1
	exec win "wincmd w"
      else
	let alternativeFile = 1
      endif
    endif
    if alternativeFile
      exec "edit! #" . expand('<abuf>')
    endif
    call SaveHardPosition('DefFileChangedShell')
    edit!
    call RestoreHardPosition('DefFileChangedShell')
    if orgWin != winnr()
      wincmd p
    endif
    return 1
  endif
  call s:InvokeFuncs(s:fcShellFuncs)
  return 0
endfunction

function! s:InvokeFuncs(funcList)
  let autoread = &autoread
  if a:funcList != ''
    call MvIterCreate(a:funcList, ',', 'InvokeFuncs')
    while MvIterHasNext('InvokeFuncs') && ! autoread
      exec "let result = " . MvIterNext('InvokeFuncs') . '()'
      if result != -1
	let autoread = autoread || result
      endif
    endwhile
  endif
  return autoread
endfunction
" FileChangedShell handling }}}


" Sign related utilities {{{
" Returns 
function! CurLineHasSign()
  let signs = s:GetVimCmdOutput('sign place buffer=' . bufnr('%'), 1)
  return (match(signs,
	\ 'line=' . line('.') . '\s\+id=\d\+\s\+name=VimBreakPt') != -1)
endfunction

" Clears all signs in the current buffer.
function! ClearAllSigns()
  let signs = GetVimCmdOutput('sign place buffer=' . bufnr('%'), 1)
  let curIdx = 0
  let pat = 'line=\d\+\s\+id=\zs\d\+\ze\s\+name=VimBreakPt'
  let id = 0
  while curIdx != -1 && curIdx < strlen(signs)
    let id = matchstr(signs, pat, curIdx)
    if id != ''
      exec 'sign unplace ' . id . ' buffer=' . bufnr('%')
    endif
    let curIdx = matchend(signs, pat, curIdx)
  endwhile
endfunction
" }}}

" Restore cpo.
let &cpo = s:save_cpo
unlet s:save_cpo

" vim6:fdm=marker
