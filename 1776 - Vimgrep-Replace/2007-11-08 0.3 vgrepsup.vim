" Author: Michael Geddes ( vimmer at frog.wheelycreek.net )
"
" Created for zimnyx on IRC
" Version:0.3
"
" Gsub Do a global search replace on a directory.
" Bsub Do a buffer search replace.
" 
"
" Copyright: Copyright me. Feel free to use, share, modify & distribute the
" script but acknowledge me please.
"
" vim: ts=2 sw=2 et
fun! GlobSearchReplace( fileglob, sub, rep, flag)
  let v:errmsg=''
  exe 'vimgrep /'.escape(a:sub,'/').'/ '.a:fileglob
  if v:errmsg != ''  | return 0 | endif
  let countup=0
  let more=1
  while 1
    silent exe 's/'.escape(a:sub, '/').'/'.escape(a:rep,'/').'/'.a:flag
    let countup+=1
    try
      silent cnext
    catch
      if v:exception !~ 'E553:' | echoerr v:exception | endif
      break
    endtry
  endwhile
  return countup
endfun

fun! BufSearchReplace( fileglob, sub, rep, flag)
  if a:fileglob != ''
    echoerr 'Filter not supported'
  else
    exe 'bufdo %s/'.escape(a:sub, '/').'/'.escape(a:rep,'/').'/'.a:flag
  end
endfun

fun! s:CallGlobReplace(func, str)
  if strlen(a:str) == 0 
    echoerr 'Usage: /sub/rep/flags files'
    return 0
  endif
  let firstch= a:str[0]
  let argidx=0
  let arg0=''
  let arg1=''
  let argflags=''
  let argfileglob=''
  let idx=1
  let str=a:str
  while idx < strlen(str)
    let ch=str[idx]
    if ch=='\' && idx+1 < strlen(str) && str[idx+1] == firstch
      let str=str[0:idx].str[idx+1:]
    elseif ch==firstch
      let arg{argidx}=str[1:idx-1]
      let argidx+=1
      let str=str[idx : ]
      let idx=0
      if argidx==2
        break
      endif
    endif
    let idx+=1
  endwhile
  if argidx == 2
    "echo ' argidx=2'
    let idx+=1
    while idx < strlen(str)
      if str[idx]=~'\s'
        let idx+=1
        break
       endif
      let argflags=argflags.str[idx]
      let idx+=1
    endwhile
    let argfileglob=str[idx : ]
  endif
  exe 'call '.a:func.'( argfileglob, arg0, arg1, argflags)'
  " call GlobSearchReplace( argfileglob, arg0, arg1, argflags)
endfun

com! -nargs=1  Gsub  :call s:CallGlobReplace('GlobSearchReplace', <q-args>)
com! -nargs=1  Bsub  :call s:CallGlobReplace('BufSearchReplace', <q-args>)

