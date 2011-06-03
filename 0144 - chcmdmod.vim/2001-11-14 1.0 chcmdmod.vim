"
" chcmdmod.vim, a very useful mappings that can switch the current command
"   mode between ':', '/' and '?'.
"   When you are in one of the :, / or ? modes, switch to a different mode by
"   typing ^X/ or ^X: or ^X?.
"
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Change: 14-Nov-2001 @ 14:55
" Requires: Vim-6.0
" Version: 1.0
"

if exists("loaded_chcmdmod")
  finish
endif
let loaded_chcmdmod = 1

function! s:LastCommandLine()
  " echo "prevCmdMode " . s:prevCmdMode
  let bufferName = ""
  if s:prevCmdMode == ":"
    let bufferName = "cmd"
  elseif s:prevCmdMode == "/" || s:prevCmdMode == "?"
    let bufferName = "search"
  endif
  let lastCommand = histget(bufferName, -1)
  call histdel(bufferName, -1)

  " Work around for the <C-C> not inserting the command in the history from
  " the second try onwards. But it now leaves a copy in the history for the
  " last used command mode, even if you cancel the command.
  if s:curCmdMode == ":"
    let bufferName = "cmd"
  elseif s:curCmdMode == "/" || s:curCmdMode == "?"
    let bufferName = "search"
  endif
  call histadd(bufferName, lastCommand)
  return lastCommand
endfunction

let s:curCmdMode = ":"

function! s:SetCurrentCmdMode(cmdMode)
  let s:prevCmdMode = s:curCmdMode
  let s:curCmdMode = a:cmdMode
  return ""
endfunction

" Save the started mode.
nnoremap : :<C-R>=<SID>SetCurrentCmdMode(":")<CR>
nnoremap / /<C-R>=<SID>SetCurrentCmdMode("/")<CR>
nnoremap ? ?<C-R>=<SID>SetCurrentCmdMode("?")<CR>

cnoremap <C-X>/ <C-C>/<C-R>=<SID>LastCommandLine()<CR>
cnoremap <C-X>? <C-C>?<C-R>=<SID>LastCommandLine()<CR>
cnoremap <C-X>: <C-C>:<C-R>=<SID>LastCommandLine()<CR>
