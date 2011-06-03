" broadcast.vim - Don Yang (http://omoikane.cjb.net)
"
" Bcast keys = broadcast to every window
" Bcast0 keys = broadcast to every window except current
"
" 05/03/02


" Get server name
function! s:ServerName(...)
   let sname = a:2
   let i = 0

   while sname != ''
      let j = stridx(sname, nr2char(10))
      if i == a:1
         if j > 0
            let sname = strpart(sname, 0, j)
         endif
         break
      endif

      let sname = strpart(sname, j + 1)
      let i = i + 1
   endwhile
   return sname
endfunction

" Broadcast to all VIM windows
function! Broadcast(...)
   let i = 0
   let slist = serverlist()

   " Send keystrokes to all other windows first
   while 1
      let sname = s:ServerName(i, slist)
      if sname == ''
         break
      endif
      if sname != v:servername
         call remote_send(sname, a:2)
      endif
      let i = i + 1
   endwhile

   " Send keystrokes to current window
   if a:1 == 1
      call remote_send(v:servername, a:2)
   endif
endfunction


" Register functions
if has('clientserver')
   command! -nargs=1 Bcast call Broadcast(1, <q-args>)
   command! -nargs=1 Bcast0 call Broadcast(0, <q-args>)
else
   echo 'VIM not compiled with +clientserver'
endif
