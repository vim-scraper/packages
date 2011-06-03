" mruex.vim - Explore your MRU (Most Recently Used) files {{{
" Author: Ajit J. Thakkar (ajit AT unb DOT ca)
" Last Change:	2003 Apr. 03
" Version: 1.3
" Credits: Ivan Tarasov - saving and restoring numbered registers in MruUpdate

" mruex.vim maintains a list (in a file called mrulist) of your most recently
" used files. mruex.vim presents an explorer-like interface to mrulist so that
" you can easily open your MRU files. Duplicates and files that are no longer
" readable are pruned from mrulist whenever an entry is added to mrulist. If
" you have several instances of Vim running simultaneously, each update to the
" mrulist becomes available immediately in all the Vim instances.

" Usage:
" A mrulist window is opened by the command :RSplit or <Leader>r where <Leader>
" is \ by default. Five commands are available in the mrulist window:
" Key		Action:
" h		Toggle help
" Enter		Open file
" o		Open file in new window
" d		Delete entry (not file)
" q		Close mrulist window

" Customization:
" Variable		Default		Meaning:
" mruex_key		'<Leader>r'	Key to open mrulist window
" mruex_win_height	6		Height of mrulist window
" mruex_max_entries	20		Maximum number of MRU file names to
" 					keep in mrulist excluding duplicates
" mruex_type		'w'		Add file to mrulist whenever it is
" 					written (saved) to disk. The other
" 					possibility is 'e' to add file to
" 					mrulist whenever it is entered.
" mruex_filter		''		Vim regular expression defining files
" 					to be excluded from the mrulist
" Examples:
" 	let mruex_filter='d:\\temp\\'
" in your vimrc excludes files in the d:\temp\ directory from the mrulist.
" 	let mruex_key='<f3>'
" in your vimrc sets F3 as the key that opens the mrulist window.

" Installation: Just place this file in $HOME\vimfiles\plugin for MSWindows or
" $HOME/.vim/plugin for Unix. The mrulist file will be created in
" $HOME\vimfiles or $HOME/.vim respectively. mruex.vim requires Vim 6.0 (or
" later) run with "set nocompatible" in the vimrc.
"}}}

if exists('loaded_mruex') || version < 600 || &cp
  finish
endif
let loaded_mruex=1

" MruInit: initialize variables and setup maps and autocommands {{{
fun! s:MruInit()
  let u_lz=&lz
  set lz
  call s:MruList()
  " set type of mru
  if !exists('g:mruex_type')
    let g:mruex_type='w'
  elseif g:mruex_type !=? 'w' && g:mruex_type !=? 'e'
    let g:mruex_type='w'
  endif
  " set mruex_max_entries
  if exists('g:mruex_max_entries')
    let s:max_lines=g:mruex_max_entries+5
  else
    let s:max_lines=25
  endif
  " set mruex_win_height
  if exists('g:mruex_win_height')
    let s:win_height=g:mruex_win_height
  else
    let s:win_height=6
  endif
  " Map to open mrulist window
  if !exists('g:mruex_key')
    let g:mruex_key='<Leader>r'
  endif
  com RSplit call s:MruExplore()
  exe 'nnoremap <silent> '. g:mruex_key .' :RSplit<cr>'
  " Autocommand to update mrulist
  if g:mruex_type ==? 'w'
    if version > 601 || (version == 601 && has("patch316"))
      autocmd BufWritePost * call s:MruUpdate(0)
    else
      autocmd BufWritePost * call s:MruUpdate(1)
    endif
  elseif g:mruex_type ==? 'e'
    autocmd BufEnter * call s:MruUpdate(0)
  endif
  let &lz=u_lz
endfun
"}}}
" MruList: find mrulist file; create it if necessary {{{
fun! s:MruList()
  silent! 1sp
  let upath=&path
  if has('win32')
    let temp=substitute(&rtp,'"','','g')
    let &path=substitute(temp,' ','\\\ ','g')
  else
    let &path=&rtp
  endif
  let v:errmsg=""
  silent! find mrulist
  if v:errmsg != ""
    silent! find plugin/mruex.vim
    let s:mrulist=substitute(expand('%:p:h'),"plugin$", 'mrulist', '')
    silent! bwipeout
    silent! 1sp
    exe "silent! e ".s:mrulist
    call setline(1,'" h : toggle help')
    1put='\" <Enter> : open file'
    2put='\" o : open file in new window'
    3put='\" d : delete entry'
    4put='\" q : close this window'
    silent! w
  else
    let s:mrulist=expand("%:p")
    call s:MruPrune()
    silent! w
  endif
  let &path=upath
  unlet! upath temp
  let s:altbufnr=-1
  call s:MruWinSet()
  call s:MruClose()
endfun
"}}}
" MruExplore: go to new or existing mruex window {{{
fun! s:MruExplore()
  let mruwin=bufwinnr('mrulist')
  if mruwin != -1
    exe mruwin.'wincmd w'
    return
  endif
  call s:MruGetWindow()
  if line("$") == 5
    echohl WarningMsg | echo "mrulist is empty" | echohl None
  else
    6
  endif
endfun
"}}}
" MruGetWindow: Get a mrulist window {{{
fun! s:MruGetWindow()
  let s:curwinnr=winnr()
  let s:curbufnr=bufnr('%')
  let s:altbufnr=bufnr('#')
  let s:mrubuf=bufnr('mrulist')
  exe 'silent! botright'.s:win_height.'sp'
  if s:mrubuf != -1
    exe 'silent! b '.s:mrubuf
  else
    exe 'silent! e '.s:mrulist
    call s:MruWinSet()
  endif
  if version > 601 || (version == 601 && has("patch195"))
    setlocal wfh
  endif
endfun
"}}}
" MruWinSet: settings for mrulist window {{{
fun! s:MruWinSet()
  setlocal noswapfile nowrap nomodifiable fdm=manual autoread bufhidden=hide
  setlocal nobuflisted
  if has('syntax')
    syn match Names '[^\/]\+$'
    hi def link Names Type
    syn match HelpLines '^".*'
    hi def link HelpLines Comment
  endif
  let b:mruhelp=1
  call s:MruHelp()
  " Maps to open a file
  com! -buffer -nargs=1 ROpen call s:MruOpenFile(<args>)
  " Open in new window
  nnoremap <silent> <buffer> o :ROpen(2)<cr>
  " Open in mruex window
  nnoremap <silent> <buffer> <cr> :ROpen(0)<cr>
  " Map to delete mruex entry
  com! -buffer RRemove call s:MruRemove()
  nnoremap <silent> <buffer> d :RRemove<cr>
  " Map to close mruex window
  com! -buffer RClose call s:MruClose()
  nnoremap <silent> <buffer> q :RClose<cr>
  " Map to toggle help
  com! -buffer RHelp call s:MruHelp()
  nnoremap <silent> <buffer> h :RHelp<cr>
endfun
"}}}
" MruOpenFile: edit file {{{
fun! s:MruOpenFile(newwin)
  " in mruex window (newwin=0) or new window (newwin > 0)
  let thisline=getline(".")
  if thisline =~ '^"' || thisline =~ '^\s*$'
    return
  endif
  call s:MruClose()
  if a:newwin > 0
    split
  endif
  exe "e ".thisline
endfun
"}}}
" MruRemove: delete entry from mrulist {{{
fun! s:MruRemove()
  if getline(".") =~? '^"'
    return
  endif
  setlocal modifiable
  .d
  silent! w
  setlocal nomodifiable
endfun
"}}}
" MruHelp: toggle help for mruex window {{{
fun! s:MruHelp()
  if has('folding')
    if b:mruhelp == 0
      let b:mruhelp=1
      1,5foldopen
      1
    else
      let b:mruhelp=0
      1,5fold
    endif
  endif
endfun
"}}}
" MruUpdate: update mrulist {{{
fun! s:MruUpdate(wipe)
  let regq=@"
  let reg0=@0
  let reg1=@1
  let reg2=@2
  let reg3=@3
  let reg4=@4
  let reg5=@5
  let reg6=@6
  let reg7=@7
  let reg8=@8
  let reg9=@9
  let name=expand("%:t")
  if name == "mrulist" || name == ""
    return
  endif
  if exists('g:mruex_filter')
    if expand("%:p") =~? g:mruex_filter
      return
    endif
  endif
  let name=expand("%:p:~")
  " Move to existing mruex window or open one
  let u_lz=&lz
  set lz
  call s:MruGetWindow()
  let dhead=search('close this',"w")
  if dhead == 0
    echohl WarningMsg | echo "Corrupt mrulist" | echohl None
    return
  endif
  " Remove duplicates
  setlocal modifiable
  let ename=escape(name,'.~\/[*')
  silent! exe "g/".ename.'/d'
  " Remove files that are not readable
  call s:MruPrune()
  " Add entry
  exe dhead.'put=name'
  if line("$") > s:max_lines
    silent exe s:max_lines."+1,$d"
  endif
  silent! w
  setlocal nomodifiable
  if a:wipe > 0
    silent! bwipeout
  else
    call s:MruClose()
  endif
  unlet! name dhead ename
  let &lz=u_lz
  " restore registers possibly corrupted
  let @"=regq
  let @1=reg1
  let @2=reg2
  let @3=reg3
  let @4=reg4
  let @5=reg5
  let @6=reg6
  let @7=reg7
  let @8=reg8
  let @9=reg9
endfun
"}}}
" MruClose: close mrulist {{{
fun! s:MruClose()
  let u_lz=&lz
  set lz
  hide
  if exists('s:curwinnr')
    exe s:curwinnr.'wincmd w'
  endif
  if s:altbufnr != -1
    exe "b".s:altbufnr
    exe "b".s:curbufnr
  endif
  let &lz=u_lz
endfun
"}}}
" MruPrune: Remove unreadable files from list {{{
fun! s:MruPrune()
  5
  while line(".") < line('$')
    norm! j
    while line('.') > 5
      let filen=substitute(getline('.'),'\~',escape(expand($HOME),' \/[*'),"")
      if !filereadable(filen)
	.d
      else
	break
      endif
    endwhile
  endwhile
endfun
"}}}

call s:MruInit()

" vim: fdm=marker:
