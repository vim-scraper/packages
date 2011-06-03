" Author: Michael Geddes ( vimmer@frog.wheelycreek.net )
"
" Created for zimnyx on IRC
"
" Do a global search replace on a directory.
"
" Copyright: Copyright me. Feel free to use, share, modify & distribute the
" script but acknowledge me please.
"
" vim: ts=2 sw=2 et
fun!  GlobSearchReplace( fileglob, sub, rep, flag)
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
