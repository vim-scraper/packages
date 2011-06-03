" pyhelp.vim - Pyhelp
" Maintainer:   Travis Jeffery
" [15 Nov 2008 03:53:45 Travis Jeffery (eatsleepgolf@gmail.com)]

" Exit quickly when:
" - this plugin was already loaded (or disabled)
" - when 'compatible' is set
if (exists("g:loaded_pyhelp") && g:loaded_pyhelp) || &cp
    finish
endif
let g:loaded_pyhelp = 1

let s:cpo_save = &cpo
set cpo&vim

function! VisualText()
  let temp = @a
  norm! gv"ay
  let visual = @a
  let temp   = @a
  return temp
endfunction

function! ShowPydoc(module, ...)
    let fPath = "/tmp/pyHelp_" . a:module . ".pydoc"
    execute ":!pydoc " . a:module . " > " . fPath
    execute ":sp ".fPath
    set ft=help
endfunction

function! PyHelp()
    let module = VisualText()
    let fPath  = "/tmp/pyHelp_" . module . ".pydoc"
    execute ":!pydoc " . module . " > " . fPath
    execute ":sp " .fPath
    set ft=help
endfunction

" Commands and Maps {{{1

command! -range VisualText call VisualText()
command! -nargs=+ Pyhelp :call ShowPydoc("<args>")
command! -range PyHelp call PyHelp()
vnoremap ph :PyHelp<CR>
" 1}}}

let &cpo = s:cpo_save

" vim:set ft=vim ts=8 sw=4 sts=4:
