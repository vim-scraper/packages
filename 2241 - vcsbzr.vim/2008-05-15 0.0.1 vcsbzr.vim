" BZR extension for VCSCommand.
"
" Version:       VCS development
" Maintainers:   Neil Martinsen-Burrell <neilmartinsenburrell@gmail.com>
"                Sabin Iacob <iacobs@m0n5t3r.info>
" License:
" Copyright (c) 2007 Bob Hiestand
"
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to
" deal in the Software without restriction, including without limitation the
" rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
" sell copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
"
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
" FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
" IN THE SOFTWARE.
"
"  Installation
"  ============
"
"  This file is Bazaar (http://bazaar-vcs.org) backend plugin for the 
"  VCSCommand plugin for ViM.  In order to use it, you need to install
"  VCSCommand from 
"       http://www.vim.org/scripts/script.php?script_id=90 or
"       http://code.google.com/p/vcscommand/
"  You should follow the installation instructions for that package in the
"  doc/vcscommand.txt file.
"
"  Having installed VCSCommand, you should put this file in the same place
"  that you installed the vcscommand.vim file.
"
"  Usage Notes
"  ===========
"
"  The version control commands are the same as they are for other version
"  control systems as described in ``:help vcscommand``.  Due to the nature of
"  Bazaar, VCSUnlock and VCSLock do nothing.
"
"  Contact
"  =======
"
"  This file is developed and distributed at http://launchpad.net/bzr-vim.
"  Go there to request features or report bugs.  Thanks!
"
" Section: Documentation {{{1
"
" Options documentation: {{{2
"
" VCSCommandBZRExec
"   This variable specifies the BZR executable.  If not set, it defaults to
"   'bzr' executed from the user's executable path.

" Section: Plugin header {{{1

if v:version < 700
  echohl WarningMsg|echomsg 'VCSCommand requires at least VIM 7.0'|echohl None
  finish
endif

let s:save_cpo=&cpo
set cpo&vim

runtime plugin/vcscommand.vim

if !executable(VCSCommandGetOption('VCSCommandBZRExec', 'bzr'))
  " BZR is not installed
  finish
endif

" Section: Variable initialization {{{1

let s:bzrFunctions = {}

" Section: Utility functions {{{1

" Function: s:DoCommand(cmd, cmdName, statusText) {{{2
" Wrapper to VCSCommandDoCommand to add the name of the BZR executable to the
" command argument.
function! s:DoCommand(cmd, cmdName, statusText)
  if VCSCommandGetVCSType(expand('%')) == 'BZR'
    let fullCmd = VCSCommandGetOption('VCSCommandBZRExec', 'bzr') . ' ' . a:cmd
    return VCSCommandDoCommand(fullCmd, a:cmdName, a:statusText, {'allowNonZeroExit': 1})
  else
    throw 'BZR VCSCommand plugin called on non-BZR item.'
  endif
endfunction

" Section: VCS function implementations {{{1

" Function: s:bzrFunctions.Identify(buffer) {{{2
function! s:bzrFunctions.Identify(buffer)
  let fileName = resolve(bufname(a:buffer))
  let statusText = system(VCSCommandGetOption('VCSCommandBZRExec', 'bzr') . ' info "' . fileName . '"')
  if(v:shell_error)
    return 0
  else
    return 1
  endif
endfunction

" Function: s:bzrFunctions.Add() {{{2
function! s:bzrFunctions.Add(argList)
  return s:DoCommand(join(['add'] + a:argList, ' '), 'add', join(a:argList, ' '))
endfunction

" Function: s:bzrFunctions.Annotate(argList) {{{2
function! s:bzrFunctions.Annotate(argList)
  if len(a:argList) == 0
    if &filetype == 'BZRAnnotate'
      " Perform annotation of the version indicated by the current line.
      let caption = matchstr(getline('.'),'\v^\s+\zs\d+')
      let options = ' -r' . caption
    else
      let caption = ''
      let options = ''
    endif
  elseif len(a:argList) == 1 && a:argList[0] !~ '^-'
    let caption = a:argList[0]
    let options = ' -r' . caption
  else
    let caption = join(a:argList, ' ')
    let options = ' ' . caption
  endif

  let resultBuffer = s:DoCommand('blame' . options, 'annotate', caption) 
  if resultBuffer > 0
    normal 1G2dd
    set filetype=BZRAnnotate
  endif
  return resultBuffer
endfunction

" Function: s:bzrFunctions.Commit(argList) {{{2
function! s:bzrFunctions.Commit(argList)
  let resultBuffer = s:DoCommand('commit -F "' . a:argList[0] . '"', 'commit', '')
  if resultBuffer == 0
    echomsg 'No commit needed.'
  endif
endfunction

" Function: s:bzrFunctions.Delete() {{{2
function! s:bzrFunctions.Delete(argList)
  return s:DoCommand(join(['rm'] + a:argList, ' '), 'rm', join(a:argList, ' '))
endfunction

" Function: s:bzrFunctions.Diff(argList) {{{2
function! s:bzrFunctions.Diff(argList)
  if len(a:argList) == 0
    let revOptions = [] 
    let caption = ''
  elseif len(a:argList) <= 2 && match(a:argList, '^-') == -1
    let revOptions = ['-r' . join(a:argList, '..')]
    let caption = '(' . a:argList[0] . ' : ' . get(a:argList, 1, 'current') . ')'
  else
    " Pass-through
    let caption = join(a:argList, ' ')
    let revOptions = a:argList
  endif

  let resultBuffer = s:DoCommand(join(['diff'] + revOptions), 'diff', caption)
  if resultBuffer > 0
    set filetype=diff
  else
    echomsg 'No differences found'
  endif
  return resultBuffer
endfunction

" Function: s:bzrFunctions.GetBufferInfo() {{{2
" Provides version control details for the current file.  Current version
" number and current repository version number are required to be returned by
" the vcscommand plugin.
" Returns: List of results:  [revision, repository]

function! s:bzrFunctions.GetBufferInfo()
  let originalBuffer = VCSCommandGetOriginalBuffer(bufnr('%'))
  let fileName = resolve(bufname(originalBuffer))
  let statusText = system(VCSCommandGetOption('VCSCommandBZRExec', 'bzr') . ' status -S "' . fileName . '"')
  let revision = system(VCSCommandGetOption('VCSCommandBZRExec', 'bzr') . ' revno "' . fileName . '"')
  if(v:shell_error)
    return []
  endif

  " File not under BZR control.
  if statusText =~ '^?'
    return ['Unknown']
  endif

  let [flags, repository] = matchlist(statusText, '^\(.\{3}\)\s\+\(\S\+\)')[1:2]
  if revision == ''
    " Error
    return ['Unknown']
  elseif flags =~ '^A'
    return ['New', 'New']
  else
    return [revision, repository]
  endif
endfunction

" Function: s:bzrFunctions.Info(argList) {{{2
function! s:bzrFunctions.Info(argList)
  return s:DoCommand(join(['version-info'] + a:argList, ' '), 'version-info', join(a:argList, ' '))
endfunction

" Function: s:bzrFunctions.Lock(argList) {{{2
function! s:bzrFunctions.Lock(argList)
  echomsg 'bzr lock is not necessary'
endfunction

" Function: s:bzrFunctions.Log() {{{2
function! s:bzrFunctions.Log(argList)
  if len(a:argList) == 0
    let options = []
    let caption = ''
  elseif len(a:argList) <= 2 && match(a:argList, '^-') == -1
    let options = ['-r' . join(a:argList, ':')]
    let caption = options[0]
  else
    " Pass-through
    let options = a:argList
    let caption = join(a:argList, ' ')
  endif

  let resultBuffer = s:DoCommand(join(['log', '-v'] + options), 'log', caption)
  return resultBuffer
endfunction

" Function: s:bzrFunctions.Revert(argList) {{{2
function! s:bzrFunctions.Revert(argList)
  return s:DoCommand('revert', 'revert', '')
endfunction

" Function: s:bzrFunctions.Review(argList) {{{2
function! s:bzrFunctions.Review(argList)
  if len(a:argList) == 0
    let versiontag = '(current)'
    let versionOption = ''
  else
    let versiontag = a:argList[0]
    let versionOption = ' -r ' . versiontag . ' '
  endif

  let resultBuffer = s:DoCommand('cat' . versionOption, 'review', versiontag)
  if resultBuffer > 0
    let &filetype=getbufvar(b:VCSCommandOriginalBuffer, '&filetype')
  endif
  return resultBuffer
endfunction

" Function: s:bzrFunctions.Status(argList) {{{2
function! s:bzrFunctions.Status(argList)
  let options = ['-S']
  if len(a:argList) == 0
    let options = a:argList
  endif
  return s:DoCommand(join(['status'] + options, ' '), 'status', join(options, ' '))
endfunction

" Function: s:bzrFunctions.Unlock(argList) {{{2
function! s:bzrFunctions.Unlock(argList)
  echomsg 'bzr unlock is not necessary'
endfunction
" Function: s:bzrFunctions.Update(argList) {{{2
function! s:bzrFunctions.Update(argList)
  return s:DoCommand('update', 'update', '')
endfunction

" Section: Plugin Registration {{{1
call VCSCommandRegisterModule('BZR', expand('<sfile>'), s:bzrFunctions, [])

let &cpo = s:save_cpo

" vim600: set foldmethod=marker:
