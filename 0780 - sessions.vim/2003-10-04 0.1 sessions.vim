" vim: ts=2 sw=2 et fdm=marker
" Plugin: sessions.vim --- named shortcuts to groups of files
" Version: 0.1
" $Id: sessions.vim,v 1.7 2003/10/04 20:11:58 andrew Exp andrew $
"
" Author: Andrew Rodionoff <arnost AT mail DOT ru>
"
" Description: This plugin introduces following additions to regular session
" management (see help on viminfo, mkview, mksession):
" - short commands for session saving, loading, clearing and merging:
"   :SS <name> --- save session <name>
"   :SL <name> --- load session <name>
"   :SN <name> --- clear buffer list and set new current session name.
"   :SS without parameter saves current session loaded with :SL or created
"       with SN.
"   :SM <name> --- merge session <name> into current one.
"
" - All sessions are stored and loaded out of specific directory named
"   'sessions' in &runtimepath. 
"   Note: it is nessessary to create this directory manually before using
"   plugin. E.g. 'mkdir -p ~/.vim/sessions' will do on Unix-like systems.
"
" - Session name argument does not need to have path prefix or extension. It's
"   also may be completed from already saved session names.
"
" - Major difference to built-in session mechanism is that saved session by
"   default does not contain any mapping, window layout, line number or
"   unlisted buffer information whatsoever. There's just file list and current
"   working directory/file. This is done intentionally, with robustness and
"   loading speed in mind (see help on viminfo to learn how to store file
"   marks across sessions). To enable old-stype session files, set variable
"   g:Sessions_Old_style = 1 in your .vimrc.
"
" TODO:
" - MRU session list.
"
" Set g:Sessions_Old_style to 1 if you want traditional session files
if !exists('g:Sessions_Old_style')
  let g:Sessions_Old_style = 0
endif

fun! s:Head(str)
  return substitute(a:str, '\([^\n]*\)\(\n.*\|$\)', '\1', '')
endfun

fun! s:Tail(str)
  return substitute(a:str, '[^\n]*\n\?\(.*\)', '\1', '')
endfun

fun! s:Zap()
  let l:bnr = 1
  while l:bnr <= bufnr('$')
    if buflisted(l:bnr) && bufname(l:bnr) != ""
      silent! exec 'bw! ' . l:bnr
    endif
    let l:bnr = l:bnr + 1
  endwhile
endfun

fun! s:WeedEmpty()
  let l:bnr = 1
  while l:bnr <= bufnr('$')
    if buflisted(l:bnr) && bufname(l:bnr) == ""
      silent! exec 'bw! ' . l:bnr
    endif
    let l:bnr = l:bnr + 1
  endwhile
endfun

fun! s:Source(name)
  let l:sesspath = globpath(&rtp, 'sessions/' . escape(a:name, ' ') . '.vim')
  if l:sesspath != ""
    exe 'so ' . s:Head(l:sesspath)
  endif
endfun

fun! s:Load(name)
  wall!
  call s:Zap()
  call s:Source(a:name)
  call s:WeedEmpty()
endfun

fun! s:Merge(name)
  let l:old_ts = v:this_session
  call s:Source(a:name)
  let v:this_session = l:old_ts
endfun

fun! s:Make(fname)
  if g:Sessions_Old_style
    exec "mksession! " . a:fname
    return
  endif
  exec "redir! > " . a:fname
  silent echo 'cd ' . getcwd()
  let l:bnr = 1
  while l:bnr <= bufnr('$')
    if buflisted(l:bnr) && bufname(l:bnr) != ""
      silent echo 'badd ' . escape(fnamemodify(bufname(l:bnr), '%:~:.'), '" ')
    endif
    let l:bnr = l:bnr + 1
  endwhile
  silent echo 'edit ' . expand('%:~:.')
  silent echo 'let v:this_session = "' . escape(a:fname, '"\') . '"'
  redir END
  let v:this_session = a:fname
endfun

fun! s:Save(...)
  wall
  if a:0 == 0
    if v:this_session != ""
      call s:Make(v:this_session)
    else
      echo "No current session to write"
    endif
  else
    let l:sessdirs = globpath(&rtp, 'sessions')
    if l:sessdirs == ""
      echoerr "Sessions directory not found."
      return
    endif
    while l:sessdirs != ""
      let l:sessdest = s:Head(l:sessdirs)
      if filewritable(l:sessdest) == 2
        call s:Make(l:sessdest . '/' . a:1 . '.vim')
        break
      endif
      let l:sessdirs = s:Tail(l:sessdirs)
    endwhile
  endif
endfun

fun! s:New(...)
  wall
  if a:0 == 0
    let v:this_session = ''
  else
    let v:this_session = a:1
  endif
  call s:Zap()
endfun

fun! SessionComplete(A,L,P)
  return substitute(globpath(&rtp, 'sessions/*.vim'), '.\{-}\([^/\n]\+\).vim\(\n\|$\)', '\1\2', 'g')
endfun

command -complete=custom,SessionComplete -nargs=1 SL call <SID>Load(<f-args>)
command -complete=custom,SessionComplete -nargs=1 SM call <SID>Merge(<f-args>)
command -complete=custom,SessionComplete -nargs=? SS call <SID>Save(<f-args>)
command -complete=custom,SessionComplete -nargs=? SN call <SID>New(<f-args>)
