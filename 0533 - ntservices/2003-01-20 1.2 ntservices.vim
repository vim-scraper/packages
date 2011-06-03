" ntservices.vim
" Author: Hari Krishna <hari_vim at yahoo dot com>
" Last Change: 20-Jan-2003 @ 16:45
" Created: 16-Jan-2003
" Requires: Vim-6.0
" Depends On: genutils.vim(1.4), Align.vim(17), winmanager.vim
" Version: 1.2.1
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
"   - For the sake of efficiency, the list of services is cached. To see the
"     latest set of services and their states at any time, refresh the window
"     by pressing 'R'.
"   - It depends on other plugins, but it is not absolutely necessary to
"     intall them along with this plugin. If you do, you may have a better
"     experience (as described in Installation section below).
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
"   - If genutils.vim is installed, it is used to sort the service list.
"   - If Align.vim is installed, it is used to format the output.
"   - Requires cscript.exe and net.exe to be in the path.
" TODO: 

if exists('loaded_ntservices')
  finish
endif
let loaded_ntservices = 1

" Initialization {{{

nnoremap <script> <silent> <Plug>NTServices :silent call <SID>ListServices()<cr>
command! -nargs=0 NTServices :call <SID>ListServices()

let g:NTServices_title = "[NT Services]"
let s:myBufNum = -1
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
let s:opMode = ""

" Initialization }}}


function! <SID>ListServices()
  let s:opMode = 'user'
  if s:myBufNum == -1
    " Temporarily modify isfname to avoid treating the name as a pattern.
    let _isf = &isfname
    set isfname-=\
    set isfname-=[
    exec "sp \\". g:NTServices_title
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
      call SaveSoftPosition("NTServices")
    endif

    " Go as far as possible in the undo history.
    while line('$') != 1
      silent! undo
    endwhile

    if s:tempFile == ""
      let s:tempFile = tempname() . '.vbs'
      silent! $put! =s:vbscript
      silent! exec 'w! ' . s:tempFile
      silent! undo
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

    if exists('*RestoreSoftPosition')
      call RestoreSoftPosition("NTServices")
    endif
    setlocal nomodifiable

    call s:SetupBuf()
  endif
endfunction


function! s:DoAction(axn)
  if line('.') < 3
    return
  endif
  let servName = matchstr(getline('.'), '^\@<=[^\t]\{-}\%( *\t\)\@=')
  let servShortName = matchstr(getline('.'), '\%(\t *\)\@<=[^\t]\{-}\%( *\t\)\@=')
  if servName != ''
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
      call confirm(result, "OK", 1, (v:shell_error != 0) ? "Error" : "Info")
      if v:shell_error == 0 || result =~ ' service has already been started\.'
	    \ || result =~ ' service is not started\.'
	call setline('.', substitute(getline('.'), '[^\t ]\+$', newTag, ''))
      endif
    endif
    setlocal nomodifiable
  endif
endfunction


function! s:SetupBuf()
  setlocal nowrap
  setlocal ts=1
  setlocal bufhidden=hide
  setlocal buftype=nofile
  setlocal nobuflisted
  nnoremap <silent> <buffer> S :call <SID>DoAction('toggle')<CR>
  nnoremap <silent> <buffer> P :call <SID>DoAction('pause')<CR>
  nnoremap <silent> <buffer> R :call <SID>UpdateBuffer(1)<CR>

  " Invert these to mean close instead open.
  command! -buffer -nargs=0 NTServices :call Quit()
  nnoremap <buffer> <silent> <Plug>NTServices :call Quit()<CR>

  syn match NTServiceStopped ".*Stopped$"
  syn match NTServiceRunning ".*Running$"
  syn match NTServicePaused ".*Paused$"
  hi NTServiceStopped guifg=red ctermfg=red
  hi NTServiceRunning guifg=green ctermfg=green
  hi NTServicePaused guifg=lightblue ctermfg=lightblue
endfunction


function! s:Quit()
  if s:opMode != 'WinManager'
    quit
  endif
endfunction


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

" vim6:fdm=marker sw=2
