"=============================================================================
"    Copyright: Copyright (C) 2003 Dr. Johann Pfefferl
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               quicksession.vim is provided *as is* and comes with no
"               warranty of any kind, either expressed or implied. In no
"               event will the copyright holder be liable for any damages
"               resulting from the use of this software.
" Name Of File: quicksession.vim
"  Description: Session Loader Vim Plugin
"   Maintainer: Dr. Johann Pfefferl (johann.pfefferl at agfa dot com)
"  Last Change: $Date: 2003/08/12 10:23:13 $
"      Version: $Id: quicksession.vim,v 1.6 2003/08/12 10:23:13 pfefferl Exp $
"        Usage: Normally, this file should reside in the plugins
"               directory and be automatically sourced. If not, you must
"               manually source this file using ':source quicksession.vim'.
"
"               You may use the default keymappings of
"
"                 <Leader>qq  - Opens QuickSession
"
"               Or you can use
"
"                 ":QuickSession" - Opens QuickSession
"
"               Before using it you have to create a file which contains a
"               list of entries. As default this plugin uses the file
"               `~/.vim_sessions`. But you can redefine this by setting the
"               global variable "g:quickSessionFile" in your .vimrc file.
"               The contains of the file must look like the following vim
"               regular expression:
"               /^\w\+\s\+\S\+$/
"               The line starts at the beginning with a label, then
"               at least one space follows and after that the name of the
"               session file follows. For example
"
"               +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
"               canas      ~/projects/canbus/SiE/canas/clients/Session.vim
"               twm        /usr/local/share/lib/twm/Session.vim
"               CVSROOT    ~/projects/cvs/Session.vim
"               vips_drv   ~/projects/dws/vtk/lxdrv/Session.vim
"               +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
"
"               defines 4 sessions which could be loaded quickly by this
"               plugin.
"
"=============================================================================

" Exit quickly when QuickSession has already been loaded or when 'compatible'
" is set.
if exists("loaded_quicksession") || &cp
  finish
endif

let loaded_quicksession = 1

" Setup the autocommands that handle the enter/leave buffer stuff
augroup quicksession
  autocmd!
  autocmd BufEnter __QuickSession__ silent call <SID>Initialize()
  autocmd BufLeave __QuickSession__ silent call <SID>Cleanup()
augroup End

" Create commands
if !exists(":QuickSession")
  command QuickSession :call <SID>StartQuickSession()
endif

" Configuration variables {{{1
" Session file containing the list of sessions
if !exists("g:quickSessionFile")
  let g:quickSessionFile = '~/.vim_sessions'
endif

" Flag if all existing buffers should be deleted before sourcing
" the new session file
if !exists("g:quickSessionBufDel")
  let g:quickSessionBufDel = 1
endif

" Keymappings {{{1
nmap <silent> <unique> <Leader>qq :QuickSession<CR>

" Internal global variables {{{1
" Used to make sure that only one QuickSession is open at a time.
if !exists("g:quickSessionRunning")
  let g:quickSessionRunning = 0
endif

" Used to remember the last position
if !exists("g:quickSessionLastPos")
  let g:quickSessionLastPos = 1
endif

" Initialize {{{1
function! <SID>Initialize()
  let s:_insertmode = &insertmode
  set noinsertmode

  let s:_showcmd = &showcmd
  set noshowcmd

  let s:_cpo = &cpo
  set cpo&vim

  let s:_report = &report
  let &report = 10000

  setlocal nonumber
  setlocal foldcolumn=0

  let g:quickSessionRunning = 1
endfunction

" Cleanup {{{1
function! <SID>Cleanup()
  let &insertmode = s:_insertmode
  let &showcmd = s:_showcmd
  let &cpo = s:_cpo
  let &report = s:_report

  let g:quickSessionRunning = 0
  let g:quickSessionLastPos = line('.')
endfunction

" StartQuickSession  {{{1
function! <SID>StartQuickSession()
  " Make sure there is only one explorer open at a time.
  if g:quickSessionRunning == 1
    return
  endif

  let _splitbelow = &splitbelow

  if filereadable(glob(g:quickSessionFile))
    exe 'silent! topleft sp __QuickSession__'
    call <SID>DisplaySessions()
  else
    echoerr 'No session file '.g:quickSessionFile.' found'
  endif
  let &splitbelow = _splitbelow
endfunction

" DisplaySessions  {{{1
function! <SID>DisplaySessions()
  setlocal bufhidden=delete
  setlocal buftype=nofile
  setlocal modifiable
  setlocal noswapfile
  setlocal nowrap

  if has("syntax")
    call <SID>SetupSyntax()
  endif

  exe '0read '.g:quickSessionFile
  normal gg
  let header = "\" Keymappings:\n"
  let header = header . "\"\t'o' or Mouse-Double-Click: Open the session under the cursor\n"
  let header = header . "\"\t'e': Edit the quicksession config file `".g:quickSessionFile."`\n\n"
  silent! put! =header
  exe g:quickSessionLastPos
  nnoremap <buffer> <silent> o :call <SID>SelectSession()<cr>
  nnoremap <buffer> <silent> <2-leftmouse> :call <SID>SelectSession()<cr>
  nnoremap <buffer> <silent> e :call <SID>EditSessionFile()<cr>

  setlocal nomodifiable
endfunction

" EditSessionFile {{{1
function! <SID>EditSessionFile()
	exe 'bd|topleft sn '.g:quickSessionFile
	setlocal bufhidden=delete
	exe 'normal '.g:quickSessionLastPos.'G'
endfunction

" SelectSession {{{1
function! <SID>SelectSession()
  let _sessionline = getline('.')
  let _sessionfn = substitute(_sessionline, '^\s*\S\+\s\+', '', '')
  if filereadable(glob(_sessionfn))
    if g:quickSessionBufDel > 0
	    %bdelete
    endif
    exe 'source '._sessionfn
  elseif strlen(_sessionfn)
    echoerr 'Session file `'._sessionfn.'` not found'
  endif
endfunction

" SetupSyntax {{{1
if has("syntax")
  function! <SID>SetupSyntax()
    syn match sessionComment     /^".*$/
    syn match sessionLabel     /^\w\+/
    syn match sessionFile     /\S\+$/

    if !exists("g:did_quicksession_syntax_hints")
      let g:did_quicksession_syntax_hints = 1
      hi def link sessionComment Comment
      hi def link sessionLabel Identifier
      hi def link sessionFile String
    endif
  endfunction
endif "}}}1

" vim600:ft=vim foldmethod=marker
