" curcmdmode.vim: extends the notion of Vim's mode() function
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Change: 14-Jul-2003 @ 20:00
" Created: 08-Jul-2003
" Requires: Vim-6.0, genutils.vim (1.7)
" Version: 1.0.1
" Description:
"   This plugin provides a small library of functions and mappings and a
"   framework to extend Vim's command-line mode in a way that is not possible
"   by using Vim's built-in features alone.
"
"   - Maps "/", ":" and "?" in normal mode to keep track of the current
"     command mode. It also remaps <Plug>CCMCCM to point to the current
"     command mode, to be used in your mappings.
"   - Defines the following plugin mappings in all the modes to the
"     corresponding key shown. Use these in your mappings whenever you want to
"     by pass any mappings for these keys and directly access the
"     corresponding key from your mapping. E.g., if you need to include ":" in
"     your mapping and want to bypass the mapping provided by the plugin
"     itself (say you are only temporarily switching to ":" to come back to
"     the current mode through <Plug>CCMCCM), then you should use <Plug>CCM:
"     instead of ":" in your mapping, to avoid any side effects (events in
"     this case).
"	    <Plug>CCM:	    => :
"	    <Plug>CCM/	    => /
"	    <Plug>CCM?      => ?
"	    <Plug>CCMC-U    => <C-U>
"	    <Plug>CCMC-C    => <C-C>
"	    <Plug>CCMC-R    => <C-R>
"	    <Plug>CCMC-O    => <C-O>
"	    <Plug>CCMEsc    => <Esc>
"	    <Plug>CCMBS     => <BS>
"	    <Plug>CCMCR     => <CR>
"   - The built-in function mode() is useful to know that it is a command
"     mode, but you can't know if you are on a ":" or "/" prompt etc. To know
"     the exact mode, use the CCMGetCCM() function. Use CCMSetCCM() function
"     to change the mode that is stored internally by this plugin (not the
"     actual command mode in Vim).  Useful if you are bypassing the plugin by
"     using one of the <Plug>CCM[:/?] mappings to change the mode.
"   - Defines functions CCMAddCmdModeListener(), CCMRemoveCmdModeListener() to
"     register and deregister call back listener functions that will be called
"     whenever the plugin is aware of the changes in the command-mode. There
"     can be multiple such functions registered at anytime and the function
"     should accept exactly one declared argument, which is passed a value of
"     g:CCM_START, g:CCM_RESET, g:CCM_ABORT or g:CCM_END. Only the g:CCM_START
"     events are notified by default. To start/stop listening to the rest of
"     the events, you need to call the CCMStMon()/CCMEnMon() functions. The
"     CCMStMon() function needs to be called everytime a new mode is started.
"   - For additional help and usage ideas, see the following plugins:
"     chcmdmode.vim: http://www.vim.org/script.php?script_id=144
"     cmdalias.vim: http://www.vim.org/script.php?script_id=746
"     smartctrl-w.vim:
"	  http://mywebpage.netscape.com/haridara/vim/smartctrl-w.vim
" Limitations:
"   - Implementation of CCMStMon()/CCMEnMon() is incomplete. The events may
"     not be captured in all the instances and there could be some
"     interference with maps executed through :normal command without
"     suffixing it with a '!'.
" TODO:
"   - It should be possible to map the <BS> in expr mode to <C-C> followed by an
"     examination into the 'expr' history to know if it is going to be the
"     last <BS> and automatically EndExprMode(). I should also be able to
"     position the cursor correctly, following the way cmdalias does it.
"   - I need to capture <C-J> the same way as <CR>
"   - I need to take care of the commands that continue in the : mode such as
"     multi-statement commands (function, if etc.) and gQ.
"

if exists("loaded_curcmdmode")
  finish
endif
let loaded_curcmdmode = 1

" Make sure genutils.vim is available for initialization.
if !exists("loaded_genutils")
  runtime plugin/genutils.vim
endif

" Make sure line-continuations won't cause any problem. This will be restored
"   at the end
let s:save_cpo = &cpo
set cpo&vim

function! s:MyScriptId()
  map <SID>xx <SID>xx
  let s:sid = maparg("<SID>xx")
  unmap <SID>xx
  return substitute(s:sid, "xx$", "", "")
endfunction
let s:myScriptId = s:MyScriptId()
delfunction s:MyScriptId " This is not needed anymore.

" Use this to avoid disturbing the command modes.
nnoremap <Plug>CCM: :
nnoremap <Plug>CCM/ /
nnoremap <Plug>CCM? ?
noremap <Plug>CCMC-U <C-U>
noremap! <Plug>CCMC-U <C-U>
noremap <Plug>CCMC-C <C-C>
noremap! <Plug>CCMC-C <C-C>
noremap <Plug>CCMC-R <C-R>
noremap! <Plug>CCMC-R <C-R>
noremap <Plug>CCMC-O <C-O>
noremap! <Plug>CCMC-O <C-O>
noremap <Plug>CCMEsc <Esc>
noremap! <Plug>CCMEsc <Esc>
noremap <Plug>CCMBS <BS>
noremap! <Plug>CCMBS <BS>
noremap <Plug>CCMCR <CR>
noremap! <Plug>CCMCR <CR>
 
let g:CCM_START = 'start'
let g:CCM_RESET = 'reset'
let g:CCM_ABORT = 'abort'
let g:CCM_END = 'end'

" Save the started mode by mapping :, / and ?.
" These may already be mapped (e.g., I have hlsearch related mappings), so use
"   MapAppendCascaded to avoid overwriting them.
call MapAppendCascaded(":", "<C-R>=".s:myScriptId.
      \ "CCMSetCCM(\":\", 1)<CR>", "nn")
call MapAppendCascaded("/", "<C-R>=".s:myScriptId.
      \ "CCMSetCCM(\"/\", 1)<CR>", "nn")
call MapAppendCascaded("?", "<C-R>=".s:myScriptId.
      \ "CCMSetCCM(\"?\", 1)<CR>", "nn")

" Initialize script variables.
let s:curCmdMode = ":"
let s:originLineNo = 0
let s:monitoring = 0
aug CCMEnMon | aug END

" Listeners that will be called when one of the command modes
"   starts/resets/ends.
let s:cmdModeListeners = ''

function! CCMGetCCM()
  return s:curCmdMode
endfunction

function! CCMSetCCM(cmdMode)
  return s:CCMSetCCM(a:cmdMode, 0)
endfunction

function! CCMStMon()
  if !s:monitoring
    let s:monitoring = 1

    " NOTE: Observe the use of <Plug> versions of the BS, CR etc. to avoid
    "	getting recursively mapped.
    " NOTE: Observe the use of :<C-R>=Func()<CR><BS> instead of :call Func()<CR>
    "   to avoid the call showing up (and the hit return prompt thereafter).
    "
    " Normally, simplified versions such as this would suffice, but for
    "   applications such as chcmdmode.vim, which needs to reset the current
    "   line, the callback needs to be run from : mode.
    "cmap <C-U> <Plug>CCMC-U<C-R>=<SID>ResetLine()<Plug>CCMCR
    cmap <C-U> <Plug>CCMC-U<Plug>CCMBS<Plug>CCM:<Plug>CCMC-R=<SID>ResetLine()<Plug>CCMCR<Plug>CCMBS<Plug>CCMCCM
    cmap <silent> <C-C> <Plug>CCMC-C<Plug>CCM: call <SID>AbortCmdMode()<Plug>CCMCR
    cmap <silent> <Esc> <Plug>CCMEsc<Plug>CCM: call <SID>EscCmdMode()<Plug>CCMCR
    cmap <CR> <Plug>CCMCR<Plug>CCM:<Plug>CCMC-R=<SID>EndCmdMode()<Plug>CCMCR<Plug>CCMBS
    cmap <C-R>= <Plug>CCMC-R=<SID>StartExprCmdMode()<Plug>CCMCR<Plug>CCMC-R=

    " This is a trick that works quite nicely. For the last <BS>, the
    "   <Plug>CCMHdlrBS gets executed in the normal or insert mode. For the rest
    "   of the <BS>'s, it is executed in the command mode and is ignored.
    cmap <BS> <Plug>CCMBS<Plug>CCMHdlrBS
    "cmap <Plug>CCMHdlrBS <Nop> " This doesn't update selection for some reason.
    cnoremap <Plug>CCMHdlrBS a<BS>
    nmap <Plug>CCMHdlrBS <Plug>CCM:<Plug>CCMC-R=<SID>ResetLine()<Plug>CCMCR<Plug>CCMBS
    imap <Plug>CCMHdlrBS <Plug>CCMC-O<Plug>CCM:<Plug>CCMC-R=<SID>ResetLine()<Plug>CCMCR<Plug>CCMBS
    au CCMEnMon CursorHold * :call CCMEnMon()
  endif
endfunction

function CCMEnMon()
  if s:monitoring
    silent! cunmap <C-U>
    silent! cunmap <C-C>
    silent! cunmap <Esc>
    silent! cunmap <CR>
    silent! cunmap <C-R>=
    silent! cunmap <BS>
    silent! cunmap <Plug>CCMHdlrBS
    silent! nunmap <Plug>CCMHdlrBS
    silent! iunmap <Plug>CCMHdlrBS
    au! CCMEnMon
    let s:monitoring = 0
  endif
endfunction

" The Expr mode started from the command mode.
function! s:StartExprCmdMode()
  cmap <buffer> <C-U> <Plug>CCMC-U
  cmap <buffer> <silent> <C-C> <Plug>CCMC-C<Plug>CCM: call <SID>EndExprCmdMode()<Plug>CCMCR
  cmap <buffer> <silent> <Esc> <Plug>CCMEsc<Plug>CCM: call <SID>EndExprCmdMode()<Plug>CCMCR
  cmap <buffer> <CR> <Plug>CCMCR<Plug>CCMC-R=<SID>EndExprCmdMode()<Plug>CCMCR
  cmap <buffer> <C-R>= <Plug>CCMC-R=
  " FIXME: I need to somehow capture the last <BS> also.
  return ""
endfunction

" Return back to the command mode from expr mode.
function! s:EndExprCmdMode()
  cunmap <buffer> <C-U>
  cunmap <buffer> <C-C>
  cunmap <buffer> <Esc>
  cunmap <buffer> <CR>
  cunmap <buffer> <C-R>=
  return ""
endfunction

function! s:CCMSetCCM(cmdMode, start)
  let s:curCmdMode = a:cmdMode
"  Decho "CCMSetCCM: curCmdMode = " . CCMGetCCM() . " start = " . a:start . " line = " . CCMOriginLineNo()
  if a:start
    call s:StartCmdMode()
  endif

  exec "noremap! <Plug>CCMCCM" s:curCmdMode
  exec "noremap <Plug>CCMCCM" s:curCmdMode

  return ""
endfunction

function! s:StartCmdMode()
  call CCMEnMon() " In case we missed it last time.
  let s:originLineNo = line(".")
  call s:FireCmdModeListeners(g:CCM_START)
  return ""
endfunction

function! s:EndCmdMode()
  call CCMEnMon()
  call s:FireCmdModeListeners(g:CCM_END)
  return ""
endfunction

function! s:ResetLine()
  call s:FireCmdModeListeners(g:CCM_RESET)
  return ""
endfunction

function! s:AbortCmdMode()
  call CCMEnMon()
  call s:FireCmdModeListeners(g:CCM_ABORT)
  return ""
endfunction

function! s:EscCmdMode()
  " If the <Esc> was entered through a map, then it would result in an
  " g:CCM_END instead of an g:CCM_ABORT, but there is no way to distinguish
  " it.
  if &cpoptions =~ '.*x.*'
    call s:EndCmdMode()
  else
    call CCMEnMon()
    call s:FireCmdModeListeners(g:CCM_ABORT)
  endif
  return ""
endfunction

function! CCMOriginLineNo()
  return s:originLineNo
endfunction

function! CCMAddCmdModeListener(listener)
  let s:cmdModeListeners = s:cmdModeListeners . "\ncall ".a:listener.'(<arg>)'
endfunction

function! CCMRemoveCmdModeListener(listener)
  let s:cmdModeListeners = substitute(s:cmdModeListeners,
	\ "\ncall ".a:listener.'(<arg>)', '', '')
endfunction

function! s:FireCmdModeListeners(action)
  let listenerCalls = substitute(s:cmdModeListeners, '<arg>', "'".a:action."'",
	\ 'g')
  exec listenerCalls
endfunction

" Restore cpo.
let &cpo = s:save_cpo
unlet s:save_cpo

" vim6:fdm=marker sw=2
