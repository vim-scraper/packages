"
" remcmd.vim -- lets you execute commands remotely at another vim instance.
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Change: 14-Feb-2002 @ 12:52
" Created:     17-Nov-2001 @ 15:15
" Requires: Vim-6.0, multvals.vim(2.0.5)
" Version: 1.0.7
" Environment:
"   Adds
"       RCmdInitialize, RCmdPresetServer, RCmdRegStartSharing,
"       RCmdRegStopSharing, RCmdRegServerList, RCmdRegister, RCmdShareRegister,
"       RCmdSyncRegisters
"     commands.
"   Adds
"       <Leader>ta, <Leader>ts, <Leader>ro, <Leader>re, <Leader>sf, <Leader>sb,
"       <Leader>sr
"     normal mode mappings, if not disabled.
"       y, d
"     operator pending mode mapping if register sharing is enabled.
"       Y, D
"     normal mode mapping if register sharing is enabled.
"   Adds
"       Remcmd
"     menu and popup groups if enabled.
"   Adds
"       RemCmdServerList(), RemCmdSelectServer(), RemCmdSendRemoteCommand()
"     global functions.
"   Depends on 
"       g:remcmdAddToMenu, g:remcmdAddToPopupMenu, g:remcmdUseDialogs,
"         g:remcmdNoMappings
"     global variables
"
" Usage:
"   <Leader>ro - Opens the current file in a remote Vim session.
"   <Leader>re - Executes a command in the remote Vim session.
"   <Leader>ta - Opens the current tag in the remote Vim session.
"   <Leader>ts - Selects the current tag in the remote Vim session.
"   <Leader>sf - Search forward for the current word in the remote Vim session.
"   <Leader>sb - Search backward for the current word in the remote Vim session.
"   <Leader>sr - Force a synchronize on the remote registers with the local
"                registers.
"
" Limitations:
"   The script can only capture those yanks or deletes that end with y, Y, d or
"     D. So something yanked by using the command: y'a is not shared. Use
"     RCmdSyncRegisters or the SyncRegisters menu (if menu is enabled) to force
"     a sync.
"

if exists("loaded_remcmd")
  finish
endif
let loaded_remcmd=1

" Call this any time to reconfigure the environment. This re-performs the same
"   initializations that the script does during the vim startup.
command! -nargs=0 RCmdInitialize :call <SID>Initialize()

function! s:Initialize()
  function! s:CreateMenu(sub)
    exec 'amenu <silent> ' . a:sub . '&Remcmd.&OpenCurrentFile :call <SID>RemoteOpenCurrentFile()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Remcmd.&SelectTag :call <SID>RemoteTagSelect()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Remcmd.&JumpToTag :call <SID>RemoteTagOpen()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Remcmd.&EnterRemoteCommand :call <SID>ExecRemoteCommand()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Remcmd.Search&Forward :call <SID>RemoteSearchForward()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Remcmd.Search&Backward :call <SID>RemoteSearchBackward()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Remcmd.-Sep- :'
    exec 'amenu <silent> ' . a:sub . '&Remcmd.Sync&Registers :call <SID>RemoteSyncRegisters()<CR>'
  endfunction
  
  "
  " Add menu entries if user wants.
  "
  if exists("g:remcmdAddToMenu")
    call s:CreateMenu('')
    unlet g:remcmdAddToMenu
  endif
  
  if exists("g:remcmdAddToPopupMenu")
    call s:CreateMenu('PopUp.')
    unlet g:remcmdAddToPopupMenu
  endif
  
  if exists("g:remcmdUseDialogs")
    let s:useDialogs = 1
    unlet g:remcmdUseDialogs
  else
    let s:useDialogs = 0
  endif

  if exists("g:remcmdPresetServer")
    let s:presetServer = g:remcmdPresetServer
    unlet g:remcmdPresetServer
  else
    let s:presetServer = ""
  endif

  if exists("g:remcmdRegSharingEnabled")
    let s:regSharingEnabled = 1
    unlet g:remcmdRegSharingEnabled
  else
    let s:regSharingEnabled = 0
  endif

  if exists("g:remcmdRegShareOnlyInNormalMode")
    let s:regShareOnlyInNormalMode = g:remcmdRegShareOnlyInNormalMode
    unlet g:remcmdRegShareOnlyInNormalMode
  else
    let s:regShareOnlyInNormalMode = 1
  endif

  " Registers to keep in sync with the remote vim sessions.
  if exists("g:remcmdRegisters")
    let s:registerNames = g:remcmdRegisters
    unlet g:remcmdRegisters
  else
    let s:registerNames = '"'
  endif

  " Remote register names to keep the local registers in sync with. If empty,
  "   the same register as the local register is used.
  if exists("g:remcmdRemoteRegisters")
    let s:remoteRegisterNames = g:remcmdRemoteRegisters
    unlet g:remcmdRemoteRegisters
  else
    let s:remoteRegisterNames = ''
  endif

  if exists("g:remcmdRegServerList")
    let s:regShareServerList = g:remcmdRegServerList
    unlet g:remcmdRegServerList
  else
    let s:regShareServerList = ""
  endif

  " Pass a servername as preset to be used, instead of prompting user everytime.
  " To clear the preset, just pass an empty string.
  command! -nargs=1 RCmdPresetServer :call RemCmdPresetServer(<f-args>)

  command! -nargs=0 RCmdRegStartSharing :call <SID>SetRegisterSharing(1)
  command! -nargs=0 RCmdRegStopSharing :call <SID>SetRegisterSharing(0)
  command! -nargs=1 RCmdRegServerList :call <SID>SetShareRegServerList(<f-args>)
  command! -nargs=1 RCmdRegister :call <SID>SetSharedRegisterName(<f-args>)
  command! -nargs=+ RCmdShareRegister :call <SID>RemoteShareRegister(<f-args>)
  command! -nargs=0 RCmdSyncRegisters :call <SID>RemoteSyncRegisters()


  " If this variable is set, don't define mappings.
  if ! exists("g:remcmdNoMappings")
    nnoremap <silent> <Leader>ro :call <SID>RemoteOpenCurrentFile()<cr>
    nnoremap <silent> <Leader>re :call <SID>ExecRemoteCommand()<cr>
    nnoremap <silent> <Leader>ta :call <SID>RemoteTagOpen()<cr>
    nnoremap <silent> <Leader>ts :call <SID>RemoteTagSelect()<cr>
    nnoremap <silent> <Leader>sf :call <SID>RemoteSearchForward()<CR>
    nnoremap <silent> <Leader>sb :call <SID>RemoteSearchBackward()<CR>
    nnoremap <silent> <Leader>sr :call <SID>RemoteSyncRegisters()<CR>
  endif
  
  " Initialize script variables.
  let s:selectedServerName = ""
  let s:prevRegVal = ""

  " Cleanup some unneeded functions.
  delfunction s:CreateMenu
endfunction

call s:Initialize()


"
" Returns the list of server names as comma separated values. You can use this
"   with the multvals.vim library available from vim.sf.net to iterate over the
"   server names as if it is an array.

" Returns:
"   the comma separated server list.
"
function! RemCmdServerList()
  let list = substitute(serverlist(), "\n", ",", "g")
  return list
endfunction


"
" Prompts the user for a server and sends the arbitrary command cmd.
"
" Params:
"   cmd - the arbitrary command that should be passed to the remote Vim.
"
function! RemCmdSendRemoteCommand(cmd)
  if a:cmd != ""
    let serverName = RemCmdSelectServer()
    if serverName != ""
      call s:RemoteSendCommand(serverName, a:cmd)
    endif
  endif
endfunction

"
" Prompts the user for a server name and returns it.
"
" Returns:
"   the selected server name.
"
function! RemCmdSelectServer()
  if s:presetServer != ""
    return s:presetServer
  endif

  let l:list = RemCmdServerList()

  let serverName = MvPromptForElement(l:list, ',', s:selectedServerName,
          \ "Enter the server number or name: ", v:servername, s:useDialogs)
  if serverName != ""
    let s:selectedServerName = serverName
  endif
  return serverName
endfunction


function! RemCmdPresetServer(serverName)
  let s:presetServer = a:serverName
endfunction


function! s:RemoteSendCommand(serverName, cmd)
  if a:serverName != ""
    call remote_send(a:serverName, a:cmd)
  endif
endfunction


function! s:ExecRemoteTagCmd(tagcmd)
  let serverName = RemCmdSelectServer()

  call s:RemoteSendCommand(serverName, ":" . a:tagcmd . " " .
          \ expand("<cword>") . "\<cr>")
endfunction


function! s:RemoteOpenCurrentFile()
  let serverName = RemCmdSelectServer()

  call s:RemoteSendCommand(serverName, ":e " . "+" . line(".") . " " .
          \ expand("%:p") . "\<cr>")
endfunction


function! s:RemoteTagOpen()
  call s:ExecRemoteTagCmd("ta")
endfunction


function! s:RemoteTagSelect()
  call s:ExecRemoteTagCmd("ts")
endfunction


function! s:ExecRemoteCommand()
  if s:useDialogs
    let cmd = inputdialog("Enter command:")
  else
    let cmd = input("Enter command:")
  endif

  call RemCmdSendRemoteCommand(cmd)
endfunction


function! s:RemoteSearchForward()
  call RemCmdSendRemoteCommand("/" . expand("<cword>") . "\<CR>")
endfunction

function! s:RemoteSearchBackward()
  call RemCmdSendRemoteCommand("?" . expand("<cword>") . "\<CR>")
endfunction


function! s:RemoteSyncRegisters()
  if s:registerNames != ''
    call MvIterCreate(s:registerNames, ',', "RemCmdLocalRegisters")
    call MvIterCreate(s:remoteRegisterNames, ',', "RemCmdRemoteRegisters")
    let nextLocalReg = ''
    let nextRemReg = ''
    while MvIterHasNext("RemCmdLocalRegisters")
      let nextLocalReg = substitute(MvIterNext("RemCmdLocalRegisters"),
            \ '^\s*$', '', '')
      " Should happen only as a typo, as user may have typed in an extra , or
      "   may have set one or more spaces as the value of s:registerNames.
      if nextLocalReg == ''
        continue
      endif
      if MvIterHasNext("RemCmdRemoteRegisters")
        let nextRemReg = substitute(MvIterNext("RemCmdRemoteRegisters"),
            \ '^\s*$', '', '')
      else
        let nextRemReg = ''
      endif

      call s:RemoteShareRegister(nextLocalReg, nextRemReg, s:regShareServerList)
    endwhile
    call MvIterDestroy("RemCmdLocalRegisters")
    call MvIterDestroy("RemCmdRemoteRegisters")
  endif
endfunction

function! s:RemoteShareRegister(regName, remRegName, serverList)
  exec "let regValue = @" . a:regName
  if !exists("s:prevRegVal".char2nr(a:regName)) ||
        \ s:prevRegVal{char2nr(a:regName)} != regValue
    let s:prevRegVal{char2nr(a:regName)} = regValue
    let serverList = a:serverList
    if serverList == ""
      let serverList = RemCmdServerList()
    endif

    call MvIterCreate(serverList, ",", "RemCmd")
    let cmd = ":let @" . ((a:remRegName != '') ? a:remRegName : a:regName) .
            \ " = remote_expr('" . v:servername . "', '@" . a:regName . "')" .
            \ "\<CR>"
    "call input("cmd=" .cmd)
    while MvIterHasNext("RemCmd")
      let nextServer = MvIterNext("RemCmd")
      if nextServer != v:servername
        " First determine the mode.
        let remMode = remote_expr(nextServer, "mode()")
        if remMode != "n" && s:regShareOnlyInNormalMode
          continue
        endif

        call s:RemoteSendCommand(nextServer, s:GetCommandForMode(cmd, remMode))
      endif
    endwhile
  endif
endfunction


" On the lines of amenu. See :help amenu.
function! s:GetCommandForMode(cmd, mode)
  if a:mode == "n" || s:regShareOnlyInNormalMode
    return a:cmd . ":\<C-H>" 
  elseif a:mode == "v" || a:mode == "V" || a:mode == "\<C-V>" ||
       \ a:mode == "s" || a:mode == "S" || a:mode == "\<C-S>"
    return "\<ESC>" . a:cmd . ":\<C-H>" 
  elseif a:mode == "i" || a:mode == "R"
    return "\<C-O>" . a:cmd
    "return "" . a:cmd
  elseif a:mode == "r"
    return "\<CR>" . a:cmd . ":\<C-H>" 
  elseif a:mode == "c"
    return "\<C-C>" . a:cmd . ":\<C-H>" 
  " What about operator pending? Unfortunately, we can't determine the
  "   operator pending mode by calling mode().
  endif
endfunction


function! s:SetSharedRegisterName(name)
  let s:registerNames = a:name
  call s:SetRegisterSharing(1)
endfunction


function! s:SetRegisterSharing(share)
  if s:regSharingEnabled == a:share
    return
  endif

  let s:regSharingEnabled = a:share
  if a:share
    onoremap <silent> y y:call <SID>RemoteSyncRegisters()<CR>
    onoremap <silent> d d:call <SID>RemoteSyncRegisters()<CR>
    nnoremap <silent> Y Y:call <SID>RemoteSyncRegisters()<CR>
    nnoremap <silent> D D:call <SID>RemoteSyncRegisters()<CR>
  else
    if hasmapto("y", "o")
      ounmap y
    endif
    if hasmapto("d", "o")
      ounmap d
    endif
    if hasmapto("Y", "n")
      nunmap Y
    endif
    if hasmapto("D", "n")
      nunmap D
    endif
  endif
endfunction


function! s:SetShareRegServerList(list)
  let s:regShareServerList = a:list
  call s:SetRegisterSharing(1)
endfunction


" This is required to avoid duplication of code and to work-around the problem
"   with the vim script code ordering.
let s:tmp = s:regSharingEnabled
let s:regSharingEnabled = ! s:tmp
" Ideally, this should be called from s:Initialize() function, but the
"   function to be called is not defined yet, so it won't work.
call s:SetRegisterSharing(s:tmp)
unlet s:tmp
