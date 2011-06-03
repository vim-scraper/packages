let g:isrecording=0
nnoremap <F3> :call RecordMacro() <CR>
nnoremap <F4> :call PerformMacro() <CR>
function RecordMacro()
  let g:isrecording=1
  :exec "normal qa"
endfunction
function PerformMacro()
  if(g:isrecording==1)
    "Stop recording
    let g:isrecording=0
    :exec "normal q"
    let @a = strpart(@a, 0, strlen(@a) - 3)
  else
    "Play recorded macro
    :exec "normal @a"
  endif
endfunction

