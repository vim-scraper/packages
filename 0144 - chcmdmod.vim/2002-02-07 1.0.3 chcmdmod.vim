"
" chcmdmod.vim, a very useful mappings that can switch the current command
"   mode between ':', '/' and '?'.
"   When you are in one of the :, / or ? modes, switch to a different mode by
"   typing ^X/ or ^X: or ^X?.
"
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Change: 06-Feb-2002 @ 16:15
" Requires: Vim-6.0
" Version: 1.0.3
" Environment:
"   Adds
"       /, : and ?
"     normal mode mappings.
"   Adds
"       ^X/, ^X?, ^X:
"     command mode mappings to switch between different modes.
"   Adds
"       CCMInitialize
"     command. Call this to reconfigure the script anytime.
"   Adds
"       GetCurrentCmdMode() and SetCurrentCmdMode()
"     functions for use by your script to know what is the current command
"     mode. The built-in function mode() is useful to know that it is a command
"     mode, but yyou can't know if you are on a ":" or "/" prompt etc.
"

if exists("loaded_chcmdmod")
  finish
endif
let loaded_chcmdmod = 1

function! s:Initialize()

" Supports only one level of cascaded mapping. Good enough for me.
function! MapSelfWithCaution(lhs, rhs, mapMode)

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


"function! s:MapWithCaution(lhs, rhs, mapMode)
"
"  " Determine the map mode from the map command.
"  let mapChar = strpart(a:mapMode, 0, 1)
"  if mapChar == "m"
"    let mapChar = ""
"  endif
"
"  " Check if this is already mapped.
"  let oldrhs = maparg(a:lhs, mapChar)
"  exec a:mapMode . "map" a:lhs a:rhs
"
"  " Avoid infinite recursion in mappings by using oremap.
"  let safeMapCmd = strpart(a:mapMode, 0, 1) . "oremap" . strpart(a:mapMode, 1)
"  exec safeMapCmd a:lhs a:rhs
"endfunction


" Save the started mode.
" Though we use nmap instead of noremap, this still overwrites the previous
"   mappings, don't know why.
"nmap : :<C-R>=SetCurrentCmdMode(":")<CR>
"nmap / /<C-R>=SetCurrentCmdMode("/")<CR>
"nmap ? ?<C-R>=SetCurrentCmdMode("?")<CR>

" If these are already mapped (I have hlsearch related mappings), then the
"   above overwrites them, so the following works-around the problem by doing
"   both in a single go.
call MapSelfWithCaution(":", "<C-R>=SetCurrentCmdMode(\":\")<CR>", "nn")
call MapSelfWithCaution("/", "<C-R>=SetCurrentCmdMode(\"/\")<CR>", "nn")
call MapSelfWithCaution("?", "<C-R>=SetCurrentCmdMode(\"?\")<CR>", "nn")

" Use the <Plug> equivalents to avoid the s:curCmdMode getting currupted.
"cnoremap <C-X>/ <C-C>/<C-R>=<SID>LastCommandLine()<CR>
"cnoremap <C-X>? <C-C>?<C-R>=<SID>LastCommandLine()<CR>
"cnoremap <C-X>: <C-C>:<C-R>=<SID>LastCommandLine()<CR>
cmap <C-X>/ <Plug>CCMControlC<Plug>CCMFwdSearchCmd<C-R><C-R>=<SID>LastCommandLine()<CR><C-R>=SetCurrentCmdMode("/")<CR>
cmap <C-X>? <Plug>CCMControlC<Plug>CCMBwdSearchCmd<C-R><C-R>=<SID>LastCommandLine()<CR><C-R>=SetCurrentCmdMode("?")<CR>
cmap <C-X>: <Plug>CCMControlC<Plug>CCMColonCmd<C-R><C-R>=<SID>LastCommandLine()<CR><C-R>=SetCurrentCmdMode(":")<CR>

" Use this to avoid disturbing the command modes.
nnoremap <Plug>CCMColonCmd :
nnoremap <Plug>CCMFwdSearchCmd /
nnoremap <Plug>CCMBwdSearchCmd ?
noremap <Plug>CCMControlU <C-U>
noremap! <Plug>CCMControlU <C-U>
noremap <Plug>CCMControlC <C-C>
noremap! <Plug>CCMControlC <C-C>
noremap <Plug>CCMEsc <Esc>
noremap! <Plug>CCMEsc <Esc>


cmap <C-S> <C-R>=<SID>SaveLineNo()<CR><Plug>CCMControlC<Plug>CCMColonCmd call <SID>AdvanceLine()<CR><Plug>CurCmdMode<C-R>=<SID>LastCommandLine()<CR>

" Initialize script variables.
let s:curCmdMode = ":"

" This function is not needed, so delete it.
delfunction MapSelfWithCaution

endfunction " s:Initialize

call s:Initialize()

" Call this any time to reconfigure the environment. This re-performs the same
"   initializations that the script does during the vim startup.
command! -nargs=0 CCMInitialize :call <SID>Initialize()

function! GetCurrentCmdMode()
  return s:curCmdMode
endfunction

function! SetCurrentCmdMode(cmdMode)
  let s:curCmdMode = a:cmdMode
  "if !exists("s:originalLineNo")
    let s:originalLineNo = line(".")
  "endif
  "echomsg "SetCurrentCmdMode: curCmdMode = " . s:curCmdMode . " line = " . s:originalLineNo
  exec "noremap! <Plug>CurCmdMode" s:curCmdMode
  exec "noremap <Plug>CurCmdMode" s:curCmdMode
  return ""
endfunction

function! s:LastCommandLine()
  "echomsg "LastCommandLine: curCmdMode = " . s:curCmdMode
  let histList = ""
  if s:curCmdMode == ":"
    let histList = "cmd"
  elseif s:curCmdMode == "/" || s:curCmdMode == "?"
    let histList = "search"
  endif
  let lastCommand = histget(histList, -1)
  "echomsg "LastCommandLine: lastCommand from " . histList . " = " . lastCommand
  call histdel(histList, -1)

  " Work around for the <C-C> not inserting the command in the history from
  " the second try onwards. But it now leaves a copy in the history for the
  " last used command mode, even if you cancel the command.
  "echomsg "LastCommandLine: Added lastCommand to " . histList
  call histadd(histList, lastCommand)
  return lastCommand
endfunction

function! s:Reset()
  if exists("s:originalLineNo")
    unlet s:originalLineNo
  endif
endfunction

function! s:SaveLineNo()
  let w:savedLineNo = line(".")
  "if !exists("w:originalLineNo")
    "let w:originalLineNo = w:savedLineNo
  "endif
  return ""
endfunction

function! s:GetSavedLineNo()
  return w:savedLineNo
endfunction

function! s:ResetPosition()
    "echomsg "ResetPosition"
  if exists("s:originalLineNo")
    exec s:originalLineNo
    unlet s:originalLineNo
    unlet w:savedLineNo
  endif
  if exists("w:ccm_search_inited")
    cunmap <C-U>
    cunmap <C-C>
    cunmap <CCMEsc>
    unlet w:ccm_search_inited
  endif
endfunction

function! s:AdvanceLine()
  "echomsg "AdvanceLine: curCmdMode = " . s:curCmdMode
  if s:curCmdMode == "/"
    let lineoffset = 1
    let colpos = 0
  elseif s:curCmdMode == "?"
    let lineoffset = -1
    let colpos = "$"
  else
    return
  endif

  exec (s:GetSavedLineNo() + lineoffset)
  exec "normal" colpos

  if !exists("w:ccm_search_inited")
    cmap <C-U> <Plug>CCMControlC<Plug>CCMColonCmd call <SID>ResetPosition()<CR><Plug>CurCmdMode<C-R>=SetCurrentCmdMode(GetCurrentCmdMode())<CR>
    cmap <C-C> <Plug>CCMControlC<Plug>CCMColonCmd call <SID>ResetPosition()<CR>
    "cnoremap <C-C> <C-R>=<SID>ResetPosition()<CR><C-C>
    cmap <CCMEsc> <Plug>CCMEsc<Plug>CCMColonCmd call <SID>ResetPosition()<CR>
    let w:ccm_search_inited = 1
  endif
endfunction
