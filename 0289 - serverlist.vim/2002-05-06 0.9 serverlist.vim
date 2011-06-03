" serverlist.vim - Don Yang (http://omoikane.cjb.net)
"
" Create key mappings to switch between vim windows.
" After :so serverlist.vim, each window will be accessible using
" some key mapping in normal mode, e.g. '\a' for first window.
"
" remote_foreground() is used to bring window to foreground.
" Whether or not it actually comes to foreground and receive focus
" will also depend on your window manager.
"
" 05/06/02


" Get server name
function! s:ServerName()
   let s:sname = s:slist
   let s:k = 0

   " Tokenize server list until token index matches
   while s:sname != ''
      let s:j = stridx(s:sname, nr2char(10))
      if s:k == s:i
         if s:j > 0
            let s:sname = strpart(s:sname, 0, s:j)
         endif
         break
      endif

      let s:sname = strpart(s:sname, s:j + 1)
      let s:k = s:k + 1
   endwhile
endfunction

" Unmap keys
function! s:UnmapKeys()
   let s:i = 0

   while s:i < strlen(s:kdict)
      " Clear key mapping
      let s:klist = s:klist . ':sil! nunmap \'.strpart(s:kdict, s:i, 1).'<CR>'

      " Remove last command (clear key mapping) from history
      let s:klist = s:klist . ':sil! call histdel(":", -2)<CR>'

      let s:i = s:i + 1
   endwhile
endfunction

" Map server keys
function! s:CreateKeyMaps()
   let s:i = 0

   while s:i < strlen(s:kdict)
      call s:ServerName()
      if s:sname == ''
         " No more windows
         break
      endif

      " Create key mapping to switch window
      let s:klist = s:klist . ':nmap \' . strpart(s:kdict, s:i, 1) . ' '
      let s:klist = s:klist . ':sil! call remote_foreground("' . s:sname . '")'
      let s:klist = s:klist . nr2char(22) . nr2char(22)
      let s:klist = s:klist . nr2char(22) . nr2char(13) . '<CR>'

      " Remove last command (create key mapping) from history
      let s:klist = s:klist . ':sil! call histdel(":", -2)<CR>'

      let s:i = s:i + 1
   endwhile
endfunction

" Broadcast key mappings to each VIM window
function! s:Broadcast()
   " Generate client keystrokes
   let s:klist = '<C-\><C-N>'
   call s:UnmapKeys()
   call s:CreateKeyMaps()
   let s:klist = s:klist .  ':sil! call histdel(":", "histdel")<CR>'

   " Broadcast to everyone
   let s:i = 0
   while s:i < strlen(s:kdict)
      call s:ServerName()
      if s:sname == ''
         break
      endif
      call remote_send(s:sname, s:klist)
      let s:i = s:i + 1
   endwhile
endfunction


" Script entry point

if !has('clientserver')
   echo 'VIM not compiled with +clientserver'
else
   " Setup keys
   let s:slist = serverlist()
   let s:kdict = 'abcdefghijklmnopqrstuvwxyz'
   call s:Broadcast()

   " Release storage
   unlet s:slist s:sname s:klist s:kdict s:i s:j s:k
endif
