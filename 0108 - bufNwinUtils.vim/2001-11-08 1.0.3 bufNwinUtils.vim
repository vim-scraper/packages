"
" Useful buffer, file and window related functions.
"
" Author: Hari <hari_vim@yahoo.com>
" Last Modified: 05-Nov-2001 @ 16:43
" Version: 1.0.3
"

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

"
" Find the window number for the buffer passed.
"
function! FindWindowForBuffer(bufferName, checkUnlisted)
  let bufno = bufnr(a:bufferName)
  " bufnr() will not find unlisted buffers.
  if bufno == -1 && a:checkUnlisted
    " Iterate over all the open windows for 
    let i = 1
    while winbufnr(i) != -1
      if bufname(winbufnr(i)) == a:bufferName
        return i;
      endif
      let i = i + 1
    endwhile
  endif
  return bufwinnr(bufno)
endfunction

" Returns the buffer number of the given fileName if it is already loaded.
function! FindBufferForName(fileName)
  let i = 1
  while i <= bufnr("$")
    if bufexists(i) && (match(bufname(i), a:fileName) != -1)
      break
    endif
    let i = i + 1
  endwhile
  if i <= bufnr("$")
    return i
  else
    return -1
  endif
endfunction


" Given the window number, moves the cursor to that window.
function! MoveCursorToWindow(winno)
  " Since we can't move to this window directly.
  let winmax = NumberOfWindows()
  if a:winno > winmax
    return " No use.
  endif

  while 1
      if winnr() == a:winno
          break
      endif
      wincmd w
  endwhile
endfunction


"
" Saves the heights of the currently open windows for restoring later.
"
function! SaveWindowSettings()
  let i = 2
  let g:savedCurrentWindowNumber = winnr()
  let g:savedWindowSettings = winheight(1)
  while winbufnr(i) != -1
    let g:savedWindowSettings = g:savedWindowSettings . "," . winheight(i)
    let i = i + 1
  endwhile
endfunction

"
" Restores the heights of the windows from the information that is saved by
"  SaveWindowSettings().
"
function! RestoreWindowSettings()
  if ! exists("g:savedWindowSettings")
    return
  endif
  
  let prevIndex = 0
  let winNo = 1
  if NumberOfWindows() > 1
    while prevIndex != -1
      let nextWinHeight = NextElement(g:savedWindowSettings, ",", prevIndex)
      exec "normal " . winNo . "\<C-W>w"
      exec "resize " . nextWinHeight
      let winNo = winNo + 1
      let prevIndex = NextIndex(g:savedWindowSettings, ",", prevIndex + 1)
    endwhile
  
    " Restore the current window.
    exec "normal " . g:savedCurrentWindowNumber . "\<C-W>w"
  endif
  unlet g:savedWindowSettings
  unlet g:savedCurrentWindowNumber
endfunction

" Cleanup file name such that two *cleaned up* file names are easy to be
"   compared.
function! CleanupFileName(file)
  let fileName=a:file
  " Remove multiple path separators.
  if has("win32")
    let fileName=substitute(fileName, "\\", "/", "g")
  elseif has("win16") || has("dos16") || has("dos32")
    let fileName=substitute(fileName, "\\\\{2,}", "\\", "g")
  endif
  let fileName=substitute(fileName, "/\\{2,}", "/", "g")

  " Remove ending extra path separators.
  let fileName=substitute(fileName, "/$", "", "")
  let fileName=substitute(fileName, "\\$", "", "")

  if has("win32") || has("dos32") || has("win16") || has("dos16")
    let fileName=substitute(fileName, "^[A-Z]:", "\\L&", "")
  endif
  return fileName
endfunction

function! PathIsAbsolute(path)
  let absolute=0
  if has("unix") || has("win32")
    if match(a:path, "/") == 0
      let absolute=1
    endif
  endif
  if (! absolute) && has("win32")
    if match(a:path, "\\") == 0
      let absolute=1
    endif
  endif
  if (! absolute) && (has("win32") || has("dos32") || has("win16") || has("dos16"))
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
function! SampleScriptIdFunction()
  map <SID>xx <SID>xx
  let s:sid = maparg("<SID>xx")
  unmap <SID>xx
  return substitute(s:sid, "xx$", "", "")
endfunction


" characters that must be escaped for a regular expression
let s:escregexp = '/*^$.~\'


" This method tries to save the position along with the line context if
"   possible. This is like the vim builtin marker. Pass in a unique scriptid.
function! SaveHardPositionWithContext(scriptid)
  let s:startline_{a:scriptid} = getline(".")
  call SaveHardPosition(a:scriptid)
endfunction


function! RestoreHardPositionWithContext(scriptid)
  0
  if search('\m^'.escape(s:startline_{a:scriptid},s:escregexp),'W') <= 0
    call RestoreHardPosition(a:scriptid)
  else
    execute "normal!" s:col_{a:scriptid} . "|"
  endif
  "unlet s:startline_{a:scriptid}
endfunction


" Useful when you want to go to the exact (line, col), but marking will not
"   work. Pass in a unique scriptid.
function! SaveHardPosition(scriptid)
  let s:col_{a:scriptid} = virtcol(".")
  let s:lin_{a:scriptid} = line(".")
endfunction


function! RestoreHardPosition(scriptid)
  execute s:lin_{a:scriptid}
  execute "normal!" s:col_{a:scriptid} . "|"
  "unlet s:col_{a:scriptid}
  "unlet s:lin_{a:scriptid}
endfunction
