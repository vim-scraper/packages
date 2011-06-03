" ntprocesses.vim
" Author: Hari Krishna <hari_vim at yahoo dot com>
" Last Change: 14-Feb-2003 @ 18:12
" Created: 21-Jan-2003
" Requires: Vim-6.0, multvals.vim(3.0)
" Depends On: genutils.vim(1.4), Align.vim(17), winmanager.vim
" Version: 1.1.1
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
" Description:
"   - This plugin generates a list of NT processes that are running on the
"     local NT/W2K/XP machine. You can kill any by processing K on the
"     corresponding entry.
"   - You can open the processlist window through WinManager (as described in
"     the installation section below) or by assigning a hot key. You can use
"     the same hot key to open/close the window. Alternatively, you can also
"     use the :NTProcesses command to open/close the processes window.
"   - You can choose which fields that you want to see by using the
"     NTprocFields command. You can select the sort fields by pressing s
"     consecutively and r for reversing the sort direction.
"   - For the sake of efficiency, the list of processes is cached. To see the
"     latest set of processes and their states at any time, refresh the window
"     by pressing 'R'.
"   - It depends on other plugins, but except multvals, it is not absolutely
"     necessary to intall others. If you do, you may have a better
"     experience (as described in Installation section below).
" Installation:
"   - Place the plugin in a plugin diretory under runtimepath and configure
"     WinManager according to your taste. E.g:
"
"	let g:winManagerWindowLayout = 'FileExplorer,NTProcesses'
"
"     You can then switch between FileExplorer and NTProcesses by pressing ^N
"     and ^P.
"   - If you don't want to use WinManager, you can still use the :NTProcesses
"     comamnd or assign a hotkey by placing the following in your vimrc:
"
"	nmap <silent> <F5> <Plug>NTProcesses
"
"     You can substitute any key or sequnce of keys for <F5> in the above map.
"   - Requires multvals.vim to be installed. Download from:
"	http://www.vim.org/script.php?script_id=171
"   - If genutils.vim is installed, it is used to sort the process list.
"   - If Align.vim is installed, it is used to format the output.
"   - Requires cscript.exe to be in the path.
"   - Use g:ntprocFields, g:ntprocSortFieldIndex, g:ntprocSortDirection to
"     specify field names, default sort field and the sort direction
"     respectively. Use NTprocFields command to see the list of field names
"     possible.
" TODO: 

if exists('loaded_ntprocesses')
  finish
endif
let loaded_ntprocesses = 1

" Make sure line-continuations won't cause any problem. This will be restored
"   at the end
let s:save_cpo = &cpo
set cpo&vim

" Initialization {{{

nnoremap <script> <silent> <Plug>NTProcesses :silent call <SID>ListProcesses()<cr>
command! -nargs=0 NTProcesses :call <SID>ListProcesses()
command! -nargs=0 NTprocFields :call <SID>SelectFields()

let g:NTProcesses_title = "[NT Processes]"
let s:myBufNum = -1
let s:vbscript = "
      \ Option Explicit\n
      \ Dim strComputer, CPUTime_expr, Owner_expr, Text, Prop, expr, result\n
      \ Dim objWMIService, colProcessList, objProcess\n
      \ \n
      \ strComputer = \".\"\n
      \ \n
      \ CPUTime_expr = \" (CSng(objProcess.KernelModeTime) + \" & _\n
      \     \"CSng(objProcess.UserModeTime)) / 10000000\"\n
      \ Owner_expr = \"objProcess.GetOwner(strNameOfUser,strUserDomain)\" & _\n
      \     vbCrLf & \"result = strUserDomain & \"\"\\\"\" & strNameOfUser\"\n
      \ Set objWMIService = GetObject(\"winmgmts:\" _\n
      \     & \"{impersonationLevel=impersonate}!\\\\\" & strComputer _\n
      \     & \"\\root\\cimv2\")\n
      \ If Wscript.Arguments(0) = \"-k\" Then\n
      \     Set colProcessList = objWMIService.ExecQuery _\n
      \         (\"Select * from Win32_Process Where ProcessId = \" & _\n
      \             Wscript.Arguments(1))\n
      \     For Each objProcess in colProcessList\n
      \         objProcess.Terminate()\n
      \     Next\n
      \ Else\n
      \     Set colProcessList = objWMIService.ExecQuery _\n
      \         (\"Select * from Win32_Process\")\n
      \     For Each objProcess in colProcessList\n
      \         Text = \"\"\n
      \         For Each Prop in Wscript.Arguments\n
      \             Execute(\"expr = \" & Prop & \"_expr\")\n
      \             If expr = Empty Then\n
      \                 expr = \"objProcess.\" & Prop\n
      \             End If\n
      \             Execute(\"result = \" & expr)\n
      \             Text = Text & result & vbTab\n
      \         Next\n
      \         Wscript.Echo Text\n
      \     Next\n
      \ End If\n
      \ "
let s:tempFile = ""
let s:opMode = ""
let s:allFields = "CPUTime,Description,ExecutablePath,ExecutionState,Handle," .
      \ "HandleCount,InstallDate,KernelModeTime,MaximumWorkingSetSize," .
      \ "MinimumWorkingSetSize,Name,Owner,PageFaults,PageFileUsage," .
      \ "ParentProcessId,PeakPageFileUsage,PeakVirtualSize," .
      \ "PeakWorkingSetSize,Priority,PrivatePageCount,ProcessId," .
      \ "QuotaNonPagedPoolUsage,QuotaPagedPoolUsage," .
      \ "QuotaPeakNonPagedPoolUsage,QuotaPeakPagedPoolUsage," .
      \ "ReadOperationCount,ReadTransferCount,SessionId,Status," .
      \ "TerminationDate,ThreadCount,UserModeTime,VirtualSize,WorkingSetSize," .
      \ "WriteOperationCount,WriteTransferCount"

if exists("g:ntprocFields")
  let s:fields = g:ntprocFields
  unlet g:ntprocFields
elseif !exists("s:fields")
  let s:fields = "Name ProcessId ParentProcessId"
endif

" Index into the s:fields.
if exists("g:ntprocSortFieldIndex")
  let s:sortFieldIndex = g:ntprocSortFieldIndex
  unlet g:ntprocSortFieldIndex
elseif !exists("s:sortFieldIndex")
  let s:sortFieldIndex = 0
endif

if exists("g:ntprocSortDirection")
  let s:sortdirection = g:ntprocSortDirection
  unlet g:ntprocSortDirection
elseif !exists("s:sortdirection")
  let s:sortdirection = 1
endif

" Space bank.
let s:spacer = '                                                               '

function! s:MyScriptId()
  map <SID>xx <SID>xx
  let s:sid = maparg("<SID>xx")
  unmap <SID>xx
  return substitute(s:sid, "xx$", "", "")
endfunction
let s:myScriptId = s:MyScriptId()
delfunction s:MyScriptId

" Initialization }}}


function! <SID>ListProcesses()
  let s:opMode = 'user'
  if s:myBufNum == -1
    " Temporarily modify isfname to avoid treating the name as a pattern.
    let _isf = &isfname
    set isfname-=\
    set isfname-=[
    exec "sp \\". g:NTProcesses_title
    let &isfname = _isf
    let s:myBufNum = bufnr('%')
  else
    let buffer_win = bufwinnr(s:myBufNum)
    if buffer_win == -1
      exec 'sb '. s:myBufNum
    else
      exec buffer_win . 'wincmd w'
    endif
  endif

  call s:UpdateBuffer(0)
endfunction


function! s:UpdateBuffer(force)
  if a:force || getline(1) == ''
    setlocal modifiable
    if exists('*SaveSoftPosition')
      call SaveSoftPosition("NTProcesses")
    endif

    let _report = &report
    set report=99999

    " Go as far as possible in the undo history to conserve Vim resources.
    let i = 0
    while line('$') != 1 && i < &undolevels
      silent! undo
      let i = i + 1
    endwhile
    " Delete the contents if there are still any.
    silent! 0,$delete _

    if s:tempFile == ""
      let tempDir = substitute(tempname(), '[^/\\]\+$', '', '')
      if ! isdirectory(tempDir)
	call confirm('Invalid temp directory: ' . tempDir, 'OK', 1, 'Error')
	return
      endif
      let s:tempFile = tempDir . '\\ntprocesses.vbs'
      silent! $put! =s:vbscript
      let v:errmsg = ""
      silent! exec 'w! ' . s:tempFile
      silent! undo
      if v:errmsg != ""
	call confirm('Error creating temp file: ' . s:tempFile . "\n" .
	      \ v:errmsg, 'OK', 1, 'Error')
      endif
    endif

    let procList = system('cscript.exe //E:vbscript //Nologo ' . s:tempFile .
	  \ ' ' . s:fields)
    if v:shell_error == -1
      call confirm("Error executing cscript.exe, are you sure it is in the " .
	    \ "path?\n" . procList, 'OK', 1, 'Error')
      return
    elseif v:shell_error != 0
      call confirm("There was an error executing cscript.exe, couldn't " .
	    \ "generate process list.\n" . procList, 'OK', 1, 'Error')
      return
    endif

    silent! $put =procList
    silent! 1delete _

    let hdr = substitute(s:fields, ' ', "\t", 'g')
    let marker = substitute(hdr, '[^\t]', '-', 'g')
    silent! call append(0, hdr)
    if exists('*QSort')
      call s:SortListing()
    endif
    if exists('*Align')
      setlocal modifiable
      silent! %call Align("\t")
    endif
    if exists('*QSort')
      call s:ShowSortMarker()
    endif

    let &report = _report

    if exists('*RestoreSoftPosition')
      call RestoreSoftPosition("NTProcesses")
    endif
    setlocal nomodifiable

    call s:SetupBuf()
  endif
endfunction


function! s:SelectFields()
  let response = ''
  let oldResponse = ''
  while 1
    let response = input('Fields selected: ' . s:fields . "\n" .
	  \ "Select action to perform (a:add,d:delete,q:quit): ")
    echo "\n"
    if oldResponse != 'd' || response != 'a'
      let selField = -1
    endif
    if response == 'q'
      break
    endif
    if response != 'd' && response != 'a'
      echo "Invalid selection"
      continue
    endif
    let selField = MvPromptForElement2(s:allFields, ',', selField,
	  \ "Select the field: ", -1, 0, 2)
    if selField != ''
      if response == 'd'
	let s:fields = MvRemoveElement(s:fields, ' ', selField)
      else
	let s:fields = MvAddElement(s:fields, ' ', selField)
      endif
    endif
    let oldResponse = response
  endwhile
endfunction


function! s:DoAction()
  if line('.') < 3
    return
  endif
  let pid = s:GetField('ProcessId')
  let proc  = s:GetField('Name')
  if pid != ''
    setlocal modifiable
    let answer = confirm('Would you like to "Terminate" the process: ' .
	  \ proc . '(' . pid . ')', "&Yes\n&No", 2, "Question")
    if answer == 1
      let result = system('cscript.exe //E:vbscript //Nologo ' . s:tempFile .
	    \ ' -k ' . pid)
      if v:shell_error != 0
	call confirm(result, "OK", 1, "Error")
      else
	silent! delete _
      endif
    endif
    setlocal nomodifiable
  else
    call confirm('There is no PID found for this process, did you ' .
	  \ 'disable ProcessId field?', "&OK", 1, 'Error')
  endif
endfunction


function! s:Trim(val)
  return substitute(substitute(a:val, '^\s\+', '', ''), '\s\+$', '', '')
endfunction


function! s:GetField(field)
  return s:Trim(s:GetFieldNoTrim(a:field))
endfunction


function! s:GetFieldNoTrim(field)
  let fieldIdx = MvIndexOfPattern(getline(1), "\t", " *" . a:field . " *")
  if fieldIdx != -1
    return MvElementAt(getline('.'), "\t", fieldIdx)
  else
    return ""
  endif
endfunction


function! s:SetField(field, val)
  let fieldIdx = MvIndexOfPattern(getline(1), "\t", " *" . a:field . " *")
  if fieldIdx != -1
    call setline('.', MvReplaceElementAt(getline('.'), "\t", a:val, fieldIdx))
  endif
endfunction


" Set field preserving the existing white-space, as much as possible (if the
" new field is larger than the field width, then no gaurantees).
" There should not be any tabs *in* the field.
function! s:SetFieldNoTrim(field, val)
  let oldField = s:GetFieldNoTrim('State')
  let frontSpace = matchstr(oldField, '^ \+')
  let backSpace = matchstr(oldField, ' \+$')
  let oldWidth = strlen(oldField) - strlen(frontSpace) - strlen(backSpace)
  let diffWidth = oldWidth - strlen(a:val)
  let newField = frontSpace . a:val . strpart(s:spacer, 0, strlen(backSpace) +
	\ diffWidth)
  call s:SetField('State', newField)
endfunction


function! s:SetupBuf()
  setlocal nowrap
  setlocal ts=1
  setlocal bufhidden=hide
  setlocal buftype=nofile
  setlocal nobuflisted
  nnoremap <silent> <buffer> K :call <SID>DoAction()<CR>
  nnoremap <silent> <buffer> R :call <SID>UpdateBuffer(1)<CR>

  " Invert these to mean close instead open.
  command! -buffer -nargs=0 NTProcesses :call s:Quit()
  nnoremap <buffer> <silent> <Plug>NTProcesses :call s:Quit()<CR>
  " Map the sort keys only if the QSort is available.
  if exists('*QSort')
    nnoremap <silent> <buffer> s :call <SID>SortSelect(1)<CR>
    nnoremap <silent> <buffer> r :call <SID>SortReverse()<CR>
  endif
endfunction


function! s:Quit()
  if s:opMode != 'WinManager'
    quit
  endif
endfunction


"" Sort support {{{

function! s:ShowSortMarker()
  setlocal modifiable
  let marker = substitute(getline(1), '\a', '-', 'g')
  let marker = MvReplaceElementAt(marker, "\t",
	\ substitute(MvElementAt(getline(1), "\t", s:sortFieldIndex), '\a',
	\   s:sortdirection == 1 ? 'v' : '^', 'g'), s:sortFieldIndex)
  silent! call setline(2, marker)
  setlocal nomodifiable
endfunction


function! s:GetCurrentSortFieldName()
  return MvElementAt(s:fields, ' ', s:sortFieldIndex)
endfunction


function! s:CmpByCurrentSortField(line1, line2, direction)
  let field1 = MvElementAt(a:line1, "\t", s:sortFieldIndex)
  let field2 = MvElementAt(a:line2, "\t", s:sortFieldIndex)
  if field1 =~ '^\s*\d\+\s*$'
    return CmpByNumber(s:Trim(field1), s:Trim(field2), a:direction)
  else
    return CmpByStringIgnoreCase(field1, field2, a:direction)
  endif
endfunction


" Reverse the current sort order
function! s:SortReverse()
  if exists("s:sortdirection") && s:sortdirection == -1
    let s:sortdirection = 1
  else
    let s:sortdirection = -1
  endif
  call s:SortListing()
  call s:ShowSortMarker()
endfunction

" Toggle through the different sort orders
function! s:SortSelect(inc)
  " Select the next sort option
  let s:sortFieldIndex = s:sortFieldIndex + a:inc

  " Wrap the sort type.
  let max = MvNumberOfElements(s:fields, ' ')
  if s:sortFieldIndex >= max
    let s:sortFieldIndex = 0
  elseif s:sortFieldIndex < 0
    let s:sortFieldIndex = max
  endif

  call s:SortListing()
  call s:ShowSortMarker()
endfunction

" Sort the file listing
function! s:SortListing()
    " Save the line we start on so we can go back there when done
    " sorting
    call SaveSoftPosition('SortListing')

    " Allow modification
    setlocal modifiable
    " Do the sort
    "3,$call QSort(s:myScriptId . 'CmpByCurrentSortField',
    silent! 3,$call QSort(s:myScriptId . 'CmpByCurrentSortField',
	  \ s:sortdirection)
    " Disallow modification
    setlocal nomodifiable

    " Return to the position we started on
    call RestoreSoftPosition('SortListing')
endfunction
"" Sort support }}}


" WinManager call backs {{{
function! NTProcesses_Start()
  let s:opMode = 'WinManager'
  call s:UpdateBuffer(0)
endfunction

function! NTProcesses_Refresh()
  call NTProcesses_Start()
endfunction

function! NTProcesses_IsValid()
  return 1
endfunction

function! NTProcesses_ReSize()
endfunction
" WinManager call backs }}}

" Restore cpo.
let &cpo = s:save_cpo
unlet s:save_cpo

" vim6:fdm=marker sw=2
