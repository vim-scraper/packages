" ntprocesses.vim
" Author: Hari Krishna <hari_vim at yahoo dot com>
" Last Change: 06-Oct-2003 @ 09:37
" Created: 21-Jan-2003
" Requires: Vim-6.2, multvals.vim(3.4), genutils.vim(1.10)
" Depends On: Align.vim(17), winmanager.vim
" Version: 1.2.7
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"     http://www.vim.org/script.php?script_id=568 
" Description:
"   - This plugin generates a list of NT processes that are running on the
"     local NT/W2K/XP machine. You can kill any by processing K on the
"     corresponding entry.
"   - You can open the processlist window through WinManager (as described in
"     the installation section below) or by assigning a hot key. You can use
"     the same hot key to open/close the window. Alternatively, you can also
"     use the :NTProcesses command to open/close the processes window.
"   - You can choose which fields that you want to see by using the
"     NtpFields command. You can select the sort fields by pressing s
"     consecutively and r for reversing the sort direction.
"   - For the sake of speed, the list of processes is cached. To see the
"     latest set of processes and their states at any time, refresh the window
"     by pressing 'R'.
"   - It requires multvals and genutils plugins to be always installed, but
"     others are required only depending on your usage/setting (for a better
"     experience and formatting).
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
"	nmap <silent> <F6> <Plug>NTProcesses
"
"     You can substitute any key or sequnce of keys for <F6> in the above map.
"   - Requires multvals.vim to be installed. Download from:
"	http://www.vim.org/script.php?script_id=171
"   - Requires genutils.vim to be installed. Download from:
"	http://www.vim.org/script.php?script_id=197
"   - If Align.vim is installed, it is used to format the output.
"   - Requires cscript.exe to be in the path.
"   - Use g:ntprocFields, g:ntprocSortFieldIndex, g:ntprocSortDirection to
"     specify field names, default sort field and the sort direction
"     respectively. Use NtpFields command to see the list of field names
"     possible.
" TODO: 

if exists('loaded_ntprocesses')
  finish
endif
if v:version < 602
  echomsg "You need Vim 6.2 to run this version of ntprocesses.vim."
  finish
endif
if !exists("loaded_multvals")
  runtime plugin/multvals.vim
endif
if !exists("loaded_multvals") || loaded_multvals < 304
  echomsg "ntprocesses: You need to have multvals version 3.4 or higher"
  finish
endif
if !exists("loaded_genutils")
  runtime plugin/genutils.vim
endif
if !exists("loaded_genutils") || loaded_genutils < 110
  echomsg "ntprocesses: You need to have genutils version 1.10 or higher"
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
command! -nargs=0 NtpFields :call <SID>SelectFields()

let g:NTProcesses_title = "[NT Processes]"

if !exists("g:ntprocFields")
  let g:ntprocFields = "Name ProcessId ParentProcessId"
endif
" Index into the g:ntprocFields.
if !exists("g:ntprocSortFieldIndex")
  let g:ntprocSortFieldIndex = 0
endif
if !exists("g:ntprocSortDirection")
  let g:ntprocSortDirection = 1
endif
if !exists("g:ntprocHostName")
  let g:ntprocHostName = "."
endif
if !exists('s:myBufNum')
let s:myBufNum = -1
let s:opMode = ""
endif
let s:tempFile = ""
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

" Space reservoir.
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
    let _cpo = &cpo
    try
      set isfname-=\
      set isfname-=[
      set cpo-=A
      if exists('+shellslash')
	exec "sp \\\\". g:NTProcesses_title
      else
	exec "sp \\". g:NTProcesses_title
      endif
    finally
      let &isfname = _isf
      let &cpo = _cpo
    endtry
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
    call OptClearBuffer()

    if s:tempFile == ""
      if ! s:InitVBS()
	return
      endif
    endif

    let procList = system('cscript.exe //E:vbscript //Nologo ' .
	  \ s:tempFile . ' ' . g:ntprocFields)
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

    let hdr = substitute(g:ntprocFields, ' ', "\t", 'g')
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
      call ResetSoftPosition("NTProcesses")
    endif
    setlocal nomodifiable

    call s:SetupBuf()
  endif
endfunction


" This is a dummy function that is used to just contain vbscript for the ease
"   of modifying it later.
function! s:_vbScript()
  if exists("some_non_existing_var")
    Option Explicit
    Dim strComputer, CPUTime_expr, Owner_expr, Text, Prop, expr, result
    Dim objWMIService, colProcessList, objProcess

    strComputer = "."

    CPUTime_expr = " (CSng(objProcess.KernelModeTime) + " & _
        "CSng(objProcess.UserModeTime)) / 10000000"
    Owner_expr = "objProcess.GetOwner(strNameOfUser,strUserDomain)" & _
        vbCrLf & "result = strUserDomain & ""\"" & strNameOfUser"
    Set objWMIService = GetObject("winmgmts:" _
        & "{impersonationLevel=impersonate}!\\" & strComputer & "\root\cimv2")
    If Wscript.Arguments(0) = "-k" Then
        Set colProcessList = objWMIService.ExecQuery _
            ("Select * from Win32_Process Where ProcessId = " & _
                Wscript.Arguments(1))
        For Each objProcess in colProcessList
            objProcess.Terminate()
        Next
    Else
        Set colProcessList = objWMIService.ExecQuery _
            ("Select * from Win32_Process")
        For Each objProcess in colProcessList
            Text = ""
            For Each Prop in Wscript.Arguments
                Execute("expr = " & Prop & "_expr")
                If expr = Empty Then
                    expr = "objProcess." & Prop
                End If
                Execute("result = " & expr)
                Text = Text & result & vbTab
            Next
            Wscript.Echo Text
        Next
    End If
  endif
endfunction


let s:vbscript = ''
function! s:InitVBS()
  if s:vbscript == ''
    let s:vbscript = ExtractFuncListing(s:myScriptId.'_vbScript', 1, 1)
  endif
  let tempDir = substitute(tempname(), '[^/\\]\+$', '', '')
  if ! isdirectory(tempDir)
    call confirm('Invalid temp directory: ' . tempDir, 'OK', 1, 'Error')
    return 0
  endif
  let s:tempFile = tempDir . '\\ntprocesses.vbs'
  silent! $put! =s:vbscript
  let v:errmsg = ""
  let _cpo = &cpo
  try
    set cpo-=A
    silent! exec 'w! ' . s:tempFile
  finally
    let &cpo = _cpo
  endtry
  silent! undo
  if v:errmsg != ""
    call confirm('Error creating temp file: ' . s:tempFile . "\n" .
	  \ v:errmsg, 'OK', 1, 'Error')
    return 0
  endif
  return 1
endfunction


function! s:SetHost(...)
  if a:0 == 0
    echo "Current remote host: " . g:ntservHostName
  else
    let g:ntprocHostName = a:1
    let s:tempFile = ''
    call s:UpdateBuffer(1)
  endif
endfunction


function! s:SelectFields()
  let response = ''
  let oldResponse = ''
  while 1
    let response = input('Fields selected: ' . g:ntprocFields . "\n" .
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
    let selField = MvPromptForElement2(
	  \ (response == 'a') ? s:allFields : g:ntprocFields,
	  \ (response == 'a') ? ',' : ' ', selField,
	  \ "Select the field: ", -1, 0, 2)
    if selField != ''
      if response == 'd'
	let g:ntprocFields = MvRemoveElement(g:ntprocFields, ' ', selField)
      else
	let g:ntprocFields = MvAddElement(g:ntprocFields, ' ', selField)
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
  setlocal nobuflisted
  setlocal nowrap
  setlocal noreadonly
  setlocal ts=1
  setlocal bufhidden=hide
  setlocal buftype=nofile
  setlocal foldcolumn=0
  command! -buffer -nargs=0 NTP :NTProcesses
  command! -buffer -nargs=? NTPsetHost :call <SID>SetHost(<f-args>)
  nnoremap <silent> <buffer> K :call <SID>DoAction()<CR>
  nnoremap <silent> <buffer> R :call <SID>UpdateBuffer(1)<CR>
  nnoremap <silent> <buffer> q :NTProcesses<CR>

  " Invert these to mean close instead of open.
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
    if NumberOfWindows() == 1
      redraw | echohl WarningMsg | echo "Can't quit the last window" |
	    \ echohl NONE
    else
      quit
    endif
  endif
endfunction


"" Sort support {{{

function! s:ShowSortMarker()
  setlocal modifiable
  let marker = substitute(getline(1), '\a', '-', 'g')
  let marker = MvReplaceElementAt(marker, "\t",
	\ substitute(MvElementAt(getline(1), "\t", g:ntprocSortFieldIndex), '\a',
	\   g:ntprocSortDirection == 1 ? 'v' : '^', 'g'), g:ntprocSortFieldIndex)
  silent! call setline(2, marker)
  setlocal nomodifiable
endfunction


function! s:GetCurrentSortFieldName()
  return MvElementAt(g:ntprocFields, ' ', g:ntprocSortFieldIndex)
endfunction


function! s:CmpByCurrentSortField(line1, line2, direction)
  let field1 = MvElementAt(a:line1, "\t", g:ntprocSortFieldIndex)
  let field2 = MvElementAt(a:line2, "\t", g:ntprocSortFieldIndex)
  if field1 =~ '^\s*\d\+\s*$'
    return CmpByNumber(s:Trim(field1), s:Trim(field2), a:direction)
  else
    return CmpByStringIgnoreCase(field1, field2, a:direction)
  endif
endfunction


" Reverse the current sort order
function! s:SortReverse()
  if exists("g:ntprocSortDirection") && g:ntprocSortDirection == -1
    let g:ntprocSortDirection = 1
  else
    let g:ntprocSortDirection = -1
  endif
  call s:SortListing()
  call s:ShowSortMarker()
endfunction

" Toggle through the different sort orders
function! s:SortSelect(inc)
  " Select the next sort option
  let g:ntprocSortFieldIndex = g:ntprocSortFieldIndex + a:inc

  " Wrap the sort type.
  let max = MvNumberOfElements(g:ntprocFields, ' ')
  if g:ntprocSortFieldIndex >= max
    let g:ntprocSortFieldIndex = 0
  elseif g:ntprocSortFieldIndex < 0
    let g:ntprocSortFieldIndex = max
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
	  \ g:ntprocSortDirection)
    " Disallow modification
    setlocal nomodifiable

    " Return to the position we started on
    call RestoreSoftPosition('SortListing')
    call ResetSoftPosition('SortListing')
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
