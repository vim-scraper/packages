" File Case Enforcer -  Make sure the vim buffer's case matches the filesystem case.
"
" This is for case insensitive but persistent systems like windows, and makes
" sure that the case of the file isn't changed by how you invoke vim.
"
" This was created for Git to function correctly since it IS sensitive to case
" changes.
"
" Author: Michael Geddes <vimmer at frog dot wheelycreek dot net>
" Last Modified: 07 Feb 2011
" Copyright: 2011 Michael Geddes
" Version: 0.1
" Feel free to share and modify as long as you give credit.

aug FileCaseEnforcer
  au!
  au BufRead,BufNewFile * nested call <SID>EnforceFilenameCase()
  au SwapExists * call <SID>CheckSwapExists()
aug END

" This prevents the swap dialog from happening twice when opening a file with
" the wrong case specified.
fun! s:CheckSwapExists()
  let filen=bufname('%')
  if filen!=''
    let filecase=s:filesystem_case(filen)
    if filecase !=# filen
      let v:swapchoice='o'
    endif
  endif
endfun

" Gets the filesystem case for a filename.
fun! s:filesystem_case(bufname)
  " Match drive/UNC paths

  let mp=((!exists('+shellslash')||&shellslash)?'/' : '\\')
  let start=matchstr(a:bufname,'^\([^:]:\|'.mp.mp.'[^'.mp.']*'.mp.'[^'.mp.']*\)'.mp.'\=')
  let pathname=strpart(a:bufname, strlen(start))
  let realpart=''
  let newreal=pathname
  while newreal != realpart
    let realpart=newreal
    " just need one [aA] in each section for glob to work its magic.
    let globval=glob(start.substitute(realpart,'\v\c([A-Z])([^A-Z]{-}(\\|$))','[\u\1\l\1]\2','g'))
    if globval != ''
      " Found something, copy the rest on.
      return globval.strpart(pathname, strlen(realpart))
    endif
    let newreal=fnamemodify(realpart,':h')
    let newreal=((newreal=='.')?'':newreal)
  endwhile
  " Give up just return the original filename
  return a:bufname
endfun

" Check the filesystem case matches, and enforce it if it doesn't match.
fun! s:EnforceFilenameCase()
  let filen=bufname('%')
  if filen==''
    return
  endif
  let filecase=s:filesystem_case(filen)
  if filecase !=# filen
    try
      " Need to make this 'unnamed' first to maintain the window.
      enew
      " now wipe out the old name
	  " (use bufnr to prevent issues with spaces in the filename)
      exe bufnr(filen).'bwipeout'
      " And edit the new file (to avoid 'File Exists' message)
      silent e `=filecase`
      " let b:OrigCase=filen.'-->'.filecase
    catch /^Vim\%((\a\+)\)\=:E325:/
      " Prevent swap exists message from throwing an error.
      let v:errmsg=''
    endtry
  endif
endfun
