" ntservices.vim
" Author: Hari Krishna <hari_vim at yahoo dot com>
" Last Change: 26-Feb-2004 @ 19:26
" Created: 16-Jan-2003
" Requires: Vim-6.2, multvals.vim(3.5), genutils.vim(1.10)
" Depends On: Align.vim(17), winmanager.vim
" Version: 1.7.1
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"     http://www.vim.org/script.php?script_id=533
" Description:
"   - This plugin generates a list of NT services that are installed on the
"     local NT/W2K/XP machine with the current started status. You can
"     start/stop the service by pressing S or pause/continue the service by
"     pressing P on the corresponding entry.
"   - You can open the servicelist window through WinManager (as described in
"     the installation section below) or by assigning a hot key. You can use
"     the same hot key to open/close the window. Alternatively, you can also
"     use the :NTServices command to open/close the services window.
"   - You can choose which fields that you want to see by using the
"     NtsFields command. You can select the sort fields by pressing s
"     consecutively and r for reversing the sort direction.
"   - If you have permissions, you can view the service list in a remote m/c
"     by using the NtsSetHost command. With no arguments, it prints the
"     current remote host name. To switch back to the local m/c, use "." for
"     the host name.
"   - For the sake of speed, the list of services is cached. To see the latest
"     set of services and their states at any time, refresh the window by
"     pressing 'R'.
"   - It requires multvals and genutils plugins to be always installed, but
"     others are required only depending on your usage/setting (for a better
"     experience and formatting).
" Installation:
"   - Place the plugin in a plugin diretory under runtimepath and configure
"     WinManager according to your taste. E.g:
"
"	let g:winManagerWindowLayout = 'FileExplorer,NTServices'
"
"     You can then switch between FileExplorer and NTServices by pressing ^N
"     and ^P.
"   - If you don't want to use WinManager, you can still use the :NTServices
"     comamnd or assign a hotkey by placing the following in your vimrc:
"
"	nmap <silent> <F5> <Plug>NTServices
"
"     You can substitute any key or sequnce of keys for <F5> in the above map.
"   - Requires multvals.vim to be installed. Download from:
"	http://www.vim.org/script.php?script_id=171
"   - Requires genutils.vim to be installed. Download from:
"	http://www.vim.org/script.php?script_id=197
"   - If Align.vim is installed, it is used to format the output.
"   - Requires cscript.exe and net.exe to be in the path.
"   - Use g:ntservFields, g:ntservSortFieldIndex, g:ntservSortDirection to
"     specify field names, default sort field and the sort direction
"     respectively. Use NtsFields command to see the list of field names
"     possible.
" TODO: 
"   - When controlling remote services, I sometimes get a weird error message.

if exists('loaded_ntservices')
  finish
endif
if v:version < 602
  echomsg "You need Vim 6.2 to run this version of ntservices.vim."
  finish
endif
if !exists("loaded_multvals")
  runtime plugin/multvals.vim
endif
if !exists("loaded_multvals") || loaded_multvals < 305
  echomsg "ntservices: You need a newer version of multvals.vim plugin"
  finish
endif
if !exists("loaded_genutils")
  runtime plugin/genutils.vim
endif
if !exists("loaded_genutils") || loaded_genutils < 110
  echomsg "ntservices: You need a newer version of genutils.vim plugin"
  finish
endif
let loaded_ntservices = 1

if ! OnMS()
  finish
endif

" Make sure line-continuations won't cause any problem. This will be restored
"   at the end
let s:save_cpo = &cpo
set cpo&vim

" Initialization {{{

nnoremap <script> <silent> <Plug>NTServices :silent call <SID>ListServices()<cr>
command! -nargs=0 NTServices :call <SID>ListServices()
command! -nargs=0 NtsFields :call <SID>SelectFields()
command! -nargs=? NtsSetHost :call <SID>SetHost(<f-args>)

let g:NTServices_title = "[NT Services]"

if ! exists("g:ntservFields")
  let g:ntservFields = "DisplayName Name State"
endif

" Index into the g:ntservFields.
if ! exists("g:ntservSortFieldIndex")
  let g:ntservSortFieldIndex = 0
endif

if ! exists("g:ntservSortDirection")
  let g:ntservSortDirection = 1
endif

if ! exists("g:ntservHostName")
  let g:ntservHostName = '.'
endif

if !exists('s:myBufNum')
let s:myBufNum = -1
let s:opMode = ""
endif
let s:tempFile = ""

let s:allFields = "AcceptPause,AcceptStop,Caption,CheckPoint," .
      \ "CreationClassName,Description,DesktopInteract,DisplayName," .
      \ "ErrorControl,ExitCode,InstallDate,Name,PathName,ProcessId," .
      \ "ServiceSpecificExitCode,ServiceType,Started,StartMode,StartName," .
      \ "State,Status,SystemCreationClassName,SystemName,TagId,WaitHint"

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


function! <SID>ListServices()
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
	exec "sp \\\\". g:NTServices_title
      else
	exec "sp \\". g:NTServices_title
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
      call SaveSoftPosition("NTServices")
    endif

    let _report = &report
    set report=99999
    call OptClearBuffer()

    if s:tempFile == ""
      if ! s:InitVBS()
	return
      endif
    endif

    let servList = system('cscript.exe //E:vbscript //Nologo ' .
	  \ s:tempFile . ' ' . g:ntservFields)
    if v:shell_error == -1
      call confirm("Error executing cscript.exe, are you sure it is in the " .
	    \ "path?\n" . servList, 'OK', 1, 'Error')
      return
    elseif v:shell_error != 0
      call confirm("There was an error executing cscript.exe, couldn't " .
	    \ "generate service list.\n" . servList, 'OK', 1, 'Error')
      return
    endif

    silent! $put =servList

    let hdr = substitute(g:ntservFields, ' ', "\t", 'g')
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
      call RestoreSoftPosition("NTServices")
      call ResetSoftPosition("NTServices")
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
    Dim strComputer, Text, Prop, expr, result
    Dim objWMIService, colServices, objService

    strComputer = "."

    Set objWMIService = GetObject("winmgmts:" _
        & "{impersonationLevel=impersonate}!\\" & strComputer & "\root\cimv2")
    Set colServices = objWMIService.ExecQuery _
        ("Select * from Win32_Service")
    For Each objService in colServices 
        Text = ""
        For Each Prop in Wscript.Arguments
            Execute("expr = " & Prop & "_expr")
            If expr = Empty Then
                expr = "objService." & Prop
            End If
            Execute("result = " & expr)
            Text = Text & result & vbTab
        Next
        Wscript.Echo Text
    Next
  endif
endfunction


let s:vbscript = ''
function! s:InitVBS()
  if s:vbscript == ''
    let s:vbscript = ExtractFuncListing(s:myScriptId.'_vbScript', 1, 1)
  endif
  let s:vbscript = substitute(s:vbscript, "\n".'\s*strComputer = "[^'."\n".']*'.
	\ "\n", "\n strComputer = \"".g:ntservHostName."\"\n", '')
  let tempDir = substitute(tempname(), '[^/\\]\+$', '', '')
  if ! isdirectory(tempDir)
    call confirm('Invalid temp directory: ' . tempDir, 'OK', 1, 'Error')
    return 0
  endif
  let s:tempFile = tempDir . '\\ntservices.vbs'
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
    let g:ntservHostName = a:1
    let s:tempFile = ''
    if bufnr('%') == s:myBufNum
      call s:UpdateBuffer(1)
    endif
  endif
endfunction


function! s:SelectFields()
  let response = ''
  let oldResponse = ''
  while 1
    let response = input('Fields selected: ' . g:ntservFields . "\n" .
	  \ "Select action to perform (a:add,d:delete,q:quit): ")
    echo "\n"
    if oldResponse !=# 'd' || response !=# 'a'
      let selField = -1
    endif
    if response ==# 'q'
      break
    endif
    if response !=# 'd' && response !=# 'a'
      echo "Invalid selection"
      continue
    endif
    let selField = MvPromptForElement2(
	  \ (response ==# 'a') ? s:allFields : g:ntservFields,
	  \ (response ==# 'a') ? ',' : ' ', selField,
	  \ "Select the field: ", -1, 0, 2)
    if selField != ''
      if response ==# 'd'
	let g:ntservFields = MvRemoveElement(g:ntservFields, ' ', selField)
      else
	let g:ntservFields = MvAddElement(g:ntservFields, ' ', selField)
      endif
    endif
    let oldResponse = response
  endwhile
endfunction


function! s:DoAction(axn)
  if line('.') < 3
    return
  endif
  let servName = s:GetField('DisplayName')
  let servShortName = s:GetField('Name')
  let state = s:GetField('State')
  if servShortName != ''
    setlocal modifiable
    if a:axn ==# 'toggle'
      if state ==# 'Stopped'
	let reqAxn = 'start'
	let newTag = 'Running'
      elseif state ==# 'Running'
	let reqAxn = 'stop'
	let newTag = 'Stopped'
      elseif state ==# 'Paused'
	let reqAxn = 'continue'
	let newTag = 'Running'
      else
	return
      endif
    elseif a:axn ==# 'pause'
      if state ==# 'Running'
	let reqAxn = 'pause'
	let newTag = 'Paused'
      elseif state ==# 'Paused'
	let reqAxn = 'continue'
	let newTag = 'Running'
      else
	return
      endif
    endif
    let answer = confirm('Would you like to "' . reqAxn . '" the service: ' .
	  \ servName . '(' . servShortName . ')', "&Yes\n&No", 2, "Question")
    if answer == 1
      let cmd = 'echo N | net ' . reqAxn . ' ' . servShortName
      let result = system(cmd)
      if result =~ 'The following services are dependent on the '
	let result = substitute(result, 'Do you want to continue \_.*', '', 'g')
	let response = confirm(result, "&Yes\n&No", 1, 'Question')
	if response == 2
	  return
	else
	  let result = system(cmd . ' /y')
	endif
      endif
      if v:shell_error == 0 || result =~ ' service has already been started\.'
	    \ || result =~ ' service is not started\.'
	    \ || result =~ ' service could not be stopped\.'
	call confirm(result, "OK", 1, "Info")
	call s:SetFieldNoTrim('State', newTag)
      else
	call confirm(result, "OK", 1, "Error")
      endif
    endif
    setlocal nomodifiable
  else
    call confirm('There is no Name found for this service, did you ' .
	  \ 'disable Name field?', "&OK", 1, 'Error')
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
  call SetupScratchBuffer()
  setlocal nowrap
  setlocal ts=1
  setlocal bufhidden=hide
  command! -buffer -nargs=0 NTS :NTServices
  command! -buffer -nargs=? NTSsetHost :NtsSetHost <args>
  command! -buffer -nargs=0 NtsRefresh :call <SID>UpdateBuffer(1)
  command! -buffer -nargs=0 NtsToggle :call <SID>DoAction('toggle')
  command! -buffer -nargs=0 NtsPause :call <SID>DoAction('pause')
  nnoremap <silent> <buffer> S :NtsToggle<CR>
  nnoremap <silent> <buffer> P :NtsPause<CR>
  nnoremap <silent> <buffer> R :NtsRefresh<CR>
  nnoremap <silent> <buffer> q :NTServices<CR>

  " Invert these to mean close instead of open.
  command! -buffer -nargs=0 NTServices :call s:Quit()
  nnoremap <buffer> <silent> <Plug>NTServices :call s:Quit()<CR>
  " Map the sort keys only if the QSort is available.
  if exists('*QSort')
    command! -buffer -nargs=0 NtsSortSel :call <SID>SortSelect(1)
    command! -buffer -nargs=0 NtsSortRev :call <SID>SortReverse()
    nnoremap <silent> <buffer> s :NtsSortSel<CR>
    nnoremap <silent> <buffer> r :NtsSortRev<CR>
  endif

  syn match NTServiceStopped "\%(^.*\t.*\|^\) *Stopped *\t.*$"
  syn match NTServiceRunning "\%(^.*\t.*\|^\) *Running *\t.*$"
  syn match NTServicePaused  "\%(^.*\t.*\|^\) *Paused *\t.*$"
  hi NTServiceStopped guifg=red ctermfg=red
  hi NTServiceRunning guifg=green ctermfg=green
  hi NTServicePaused guifg=yellow ctermfg=yellow
endfunction


function! s:Quit()
  if s:opMode !=# 'WinManager'
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
	\ substitute(MvElementAt(getline(1), "\t", g:ntservSortFieldIndex), '\a',
	\   g:ntservSortDirection == 1 ? 'v' : '^', 'g'), g:ntservSortFieldIndex)
  silent! call setline(2, marker)
  setlocal nomodifiable
endfunction


function! s:GetCurrentSortFieldName()
  return MvElementAt(g:ntservFields, ' ', g:ntservSortFieldIndex)
endfunction


function! s:CmpByCurrentSortField(line1, line2, direction)
  let field1 = MvElementAt(a:line1, "\t", g:ntservSortFieldIndex)
  let field2 = MvElementAt(a:line2, "\t", g:ntservSortFieldIndex)
  if field1 =~ '^\s*\d\+\s*$'
    return CmpByNumber(s:Trim(field1), s:Trim(field2), a:direction)
  else
    return CmpByStringIgnoreCase(field1, field2, a:direction)
  endif
endfunction


" Reverse the current sort order
function! s:SortReverse()
  if exists("g:ntservSortDirection") && g:ntservSortDirection == -1
    let g:ntservSortDirection = 1
  else
    let g:ntservSortDirection = -1
  endif
  call s:SortListing()
  call s:ShowSortMarker()
endfunction

" Toggle through the different sort orders
function! s:SortSelect(inc)
  " Select the next sort option
  let g:ntservSortFieldIndex = g:ntservSortFieldIndex + a:inc

  " Wrap the sort type.
  let max = MvNumberOfElements(g:ntservFields, ' ')
  if g:ntservSortFieldIndex >= max
    let g:ntservSortFieldIndex = 0
  elseif g:ntservSortFieldIndex < 0
    let g:ntservSortFieldIndex = max
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
	  \ g:ntservSortDirection)
    " Disallow modification
    setlocal nomodifiable

    " Return to the position we started on
    call RestoreSoftPosition('SortListing')
    call ResetSoftPosition('SortListing')
endfunction
"" Sort support }}}


" WinManager call backs {{{
function! NTServices_Start()
  let s:opMode = 'WinManager'
  call s:UpdateBuffer(0)
endfunction

function! NTServices_Refresh()
  call NTServices_Start()
endfunction

function! NTServices_IsValid()
  return 1
endfunction

function! NTServices_ReSize()
endfunction
" WinManager call backs }}}

" Restore cpo.
let &cpo = s:save_cpo
unlet s:save_cpo

" vim6:fdm=marker sw=2
