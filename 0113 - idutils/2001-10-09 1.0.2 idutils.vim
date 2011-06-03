""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" For id-utils
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Modified: 09-Oct-2001 @ 12:01
" Requires: Vim-6.0 or higher.
" Version: 1.0.2
"
" Drop the file in your plugin directory or source it from your vimrc.
"
" Usage:
"  :IDGrep <id>
"

if exists("loaded_idutils")
  finish
endif
let loaded_idutils=1

if ! exists("IGlidcmd")
  let IGlidcmd="lid -R grep"
endif

" Add the "lid -R grep" format to grep formats.
set gfm+="%f:%l:%m"

command! -nargs=+ -complete=tag IDGrep call <SID>IDGrep(0, <f-args>)
command! -nargs=+ -complete=tag IDGrepAdd call <SID>IDGrep(1, <f-args>)
command! -nargs=0 IDGrepReset call <SID>IDGrepReset(1)

" Pass an optional filter pattern as a second argument.
function! s:IDGrep(grepAdd, keyword, ...)
  if a:0 > 1
    echo "Too many arguments... " . a:0
    return
  endif
  "  We need to check for non-null string because Vim passes a null string if
  "    this is called from a command.
  if a:0 == 1 && a:1 != ""
    call s:IDGrepSet(a:1)
  else
    call s:IDGrepSet()
  endif
  if a:grepAdd == 1
    exec "grepadd " . a:keyword
  else
    exec "grep " . a:keyword
  endif
  call s:IDGrepReset(0)
endfunction

" You can pass an optional filter.
function! s:IDGrepSet(...)
  let s:IGsavedGrepprg = &grepprg
  if exists("IGlidcmd") && IGlidcmd != ""
    let &grepprg=IGlidcmd
  else
    " The ultimate default.
    let &grepprg="lid -R grep"
  endif
  "  We need to check for non-null string because Vim passes a null string if
  "    this is called from a command.
  let s:IGsavedShellpipe=&shellpipe
  let &shellpipe=''
  if a:0 == 1 && a:1 != ""
    let &shellpipe= &shellpipe . '| grep ' . a:1
  endif
  let &shellpipe= &shellpipe . s:IGsavedShellpipe
endfunction

" You call this method separately in case you need to reverse what IDGrep
"   command did (if you Ctrol-Ced the execution, eg.).
function! s:IDGrepReset(interactive)
  if exists("s:IGsavedGrepprg")
    let &grepprg=s:IGsavedGrepprg
    unlet s:IGsavedGrepprg
  endif
  if exists("s:IGsavedShellpipe")
    let &shellpipe=s:IGsavedShellpipe
    unlet s:IGsavedShellpipe
  endif
  if (a:interactive)
    echomsg "Done resetting your IDGrep settings."
  endif
endfunction
