" ntservices.vim
" Author: Hari Krishna <hari_vim at yahoo dot com>
" Last Change: 18-Jan-2003 @ 12:31
" Created: 16-Jan-2003
" Requires: Vim-6.0, winmanager.vim
" Requires: genutils.vim(1.4), Align.vim(17)
" Version: 1.1.0
" Download From:
"     http://www.vim.org/script.php?script_id=533
" Description:
"   - This plugin generates a list of NT services that are installed on the
"     local NT/W2K/XP machine with the current started status. You can
"     start/stop the service by pressing S or pause/continue the service by
"     pressing P on the corresponding entry.
"   - For the sake of efficiency, the list of services is cached. To see the
"     latest set of services and their states at any time, refresh the window
"     by pressing 'R'.
" Installation:
"   - Place the plugin in a plugin diretory under runtimepath and configure
"     WinManager according to your taste. E.g:
"
"	let g:winManagerWindowLayout = 'FileExplorer,NTServices'
"
"   - This script is designed to be a WinManager plugin, so you can use it only
"     from WinManager.
"   - If genutils.vim is installed, it is used to sort the service list.
"   - If Align.vim is installed, it is used to format the output.
"   - Requires cscript.exe and net.exe to be in the path.
" TODO: 

if exists('loaded_ntservices')
  finish
endif
let loaded_ntservices = 1

let g:NTServices_title = "[NT Services]"
let s:vbscript = "
      \ strComputer = \".\"\n
      \ Set objWMIService = GetObject(\"winmgmts:\" _\n
      \     & \"{impersonationLevel=impersonate}!\\\\\" & strComputer _\n
      \	    & \"\\root\\cimv2\")\n
      \ Set colRunningServices = objWMIService.ExecQuery _\n
      \     (\"Select * from Win32_Service\")\n
      \ For Each objService in colRunningServices \n
      \     Wscript.Echo objService.DisplayName & VbTab _\n
      \		& objService.Name & VbTab & objService.State\n
      \ Next\n
      \ "
let s:tempFile = ""


function! s:ListServices(force)
  if a:force || getline(1) == ''
    setlocal modifiable
    call SaveSoftPosition("NTServices")

    " Go as far as possible in the undo history.
    while line('$') != 1
      silent! undo
    endwhile

    if s:tempFile == ""
      let s:tempFile = tempname() . '.vbs'
      $put! =s:vbscript
      exec 'w! ' . s:tempFile
      undo
    endif

    let servList = system('cscript.exe //E:vbscript //Nologo ' . s:tempFile)
    if v:shell_error == -1
      call confirm('Error executing cscript.exe, are you sure it is in the ' .
	    \ 'path?', 'OK', 1, 'Error')
      return
    elseif v:shell_error != 0
      call confirm("There was an error executing cscript.exe, couldn't " .
	    \ "generate service list", 'OK', 1, 'Error')
      return
    endif

    silent! $put =servList
    silent! 1delete _

    if exists('*QSort')
      silent! %call QSort('CmpByStringIgnoreCase', 1)
    endif
    silent! call append(0, "DisplayName\tName\tState")
    silent! call append(1, "-----------\t----\t-----")
    if exists('*Align')
      silent! %call Align("\t")
    endif

    call RestoreSoftPosition("NTServices")
    setlocal nomodifiable
  endif
  call s:SetupBuf()
endfunction


function! s:DoAction(axn)
  if line('.') < 3
    +
    return
  endif
  let curService = substitute(matchstr(getline('.'), '^[^\t]\+'),
	\ '\s\+$', '', '')
  if curService != ''
    setlocal modifiable
    if a:axn == 'toggle'
      if getline('.') =~ 'Stopped$'
	let reqAxn = 'start'
	let newTag = 'Running'
      elseif getline('.') =~ 'Running$'
	let reqAxn = 'stop'
	let newTag = 'Stopped'
      elseif getline('.') =~ 'Paused$'
	let reqAxn = 'continue'
	let newTag = 'Running'
      else
	return
      endif
    elseif a:axn == 'pause'
      if getline('.') =~ 'Running$'
	let reqAxn = 'pause'
	let newTag = 'Paused'
      elseif getline('.') =~ 'Paused$'
	let reqAxn = 'continue'
	let newTag = 'Running'
      else
	return
      endif
    endif
    let answer = confirm('Would you like to "' . reqAxn . '" the service: ' .
	  \ curService, "&Yes\n&No", 2, "Question")
    if answer == 1
      let cmd = 'echo N | net ' . reqAxn . ' \"' . curService . '\"'
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
      call confirm(result, "OK", 1, (v:shell_error != 0) ? "Error" : "Info")
      if v:shell_error == 0 || result =~ ' service has already been started\.'
	    \ || result =~ ' service is not started\.'
	call setline('.', substitute(getline('.'), '[^\t]\+$', newTag, ''))
      endif
    endif
    setlocal nomodifiable
  endif
endfunction


function! s:SetupBuf()
  setlocal nowrap
  setlocal ts=1
  setlocal bufhidden=hide
  nnoremap <silent> <buffer> S :call <SID>DoAction('toggle')<CR>
  nnoremap <silent> <buffer> P :call <SID>DoAction('pause')<CR>
  nnoremap <silent> <buffer> R :call <SID>ListServices(1)<CR>

  syn match NTServiceStopped ".*Stopped$"
  syn match NTServiceRunning ".*Running$"
  syn match NTServicePaused ".*Paused$"
  hi NTServiceStopped guifg=red ctermfg=red
  hi NTServiceRunning guifg=green ctermfg=green
  hi NTServicePaused guifg=lightblue ctermfg=lightblue
endfunction


" WinManager call backs {{{
function! NTServices_Start()
  call s:ListServices(0)
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

" vim6:fdm=marker sw=2
