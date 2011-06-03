"
" remcmd.vim -- lets you execute commands remotely at another vim instance.
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Change: 31-Jan-2002 @ 14:35
" Created: 17-Nov-2001 @ 15:15
" Requires: Vim-6.0, multvals.vim(2.0.5)
" Version: 1.0.3
" Environment:
"   Adds
"       RCmdInitialize, RCmdPresetServer
"     command.
"   Adds
"       <Leader>ta, <Leader>ts, <Leader>ro, <Leader>re
"     normal mode commands, if not disabled.
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
"

if exists("loaded_remcmd")
  finish
endif
let loaded_remcmd=1

" Call this any time to reconfigure the environment. This re-performs the same
"   initializations that the script does during the vim startup.
command! -nargs=0 RCmdInitialize :call <SID>Initialize()

" Pass a servername as preset to be used, instead of prompting user everytime.
" To clear the preset, just pass an empty string.
command! -nargs=1 RCmdPresetServer :call RemCmdPresetServer(<f-args>)


function! s:Initialize()
  function! s:CreateMenu(sub)
    exec 'amenu ' . a:sub . '&Remcmd.&OpenCurrentFile :call <SID>RemoteOpenCurrentFile()<CR>'
    exec 'amenu ' . a:sub . '&Remcmd.&SelectTag :call <SID>RemoteTagSelect()<CR>'
    exec 'amenu ' . a:sub . '&Remcmd.&JumpToTag :call <SID>RemoteTagOpen()<CR>'
    exec 'amenu ' . a:sub . '&Remcmd.&EnterRemoteCommand :call <SID>ExecRemoteCommand()<CR>'
    exec 'amenu ' . a:sub . '&Remcmd.&SearchForward :call <SID>RemoteSearchForward()<CR>'
    exec 'amenu ' . a:sub . '&Remcmd.&SearchBackward :call <SID>RemoteSearchBackward()<CR>'
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

  " If this variable is set, don't define mappings.
  if ! exists("g:remcmdNoMappings")
    nnoremap <silent> <Leader>ro :call <SID>RemoteOpenCurrentFile()<cr>
    nnoremap <silent> <Leader>re :call <SID>ExecRemoteCommand()<cr>
    nnoremap <silent> <Leader>ta :call <SID>RemoteTagOpen()<cr>
    nnoremap <silent> <Leader>ts :call <SID>RemoteTagSelect()<cr>
    nnoremap <silent> <Leader>sf :call <SID>RemoteSearchForward()<CR>
    nnoremap <silent> <Leader>sb :call <SID>RemoteSearchBackward()<CR>
  endif
  
  " Initialize script variables.
  let s:selectedServerName = ""
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
  if exists("s:presetServer") && s:presetServer != ""
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
  if s:presetServer == ""
    unlet s:presetServer
  endif
endfunction


function! s:RemoteSendCommand(serverName, cmd)
  if a:serverName != ""
    call remote_send(a:serverName, a:cmd)
  endif
endfunction


function! s:ExecRemoteTagCmd(tagcmd)
  let serverName = RemCmdSelectServer()

  call s:RemoteSendCommand(serverName, ":" . a:tagcmd . " " . expand("<cword>") .
          \ "\<cr>")
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
