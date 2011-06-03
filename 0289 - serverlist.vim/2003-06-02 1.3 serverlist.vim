" serverlist.vim - Don Yang (uguu.org)
"
" Create key mappings to switch between vim windows.
" Mappings created:
"
"  \a    bring window 1 to foreground.
"  \b    bring window 2 to foreground.
"  ...
"  \z    bring window 26 to foreground.
"
"  \.    show mapping for current window.
"  \,    refresh mappings for all windows.
"  \\    cycle to next window (only works if loaded as plugin).
"  \/    cycle to previous window (only works if loaded as plugin).
"
" remote_foreground() is used to bring window to foreground.
" Whether or not it actually comes to foreground and receive focus
" will also depend on your window manager.
"
" If one of the windows exit, mappings in other windows are not
" updated.  Use \, (refresh key mappings) in that case.
"
" Window cycling only works if script is loaded as a plugin.
" This is to ensure that the next window also has the window cycling
" function defined (didn't seem like a good idea to define functions
" remotely).
"
" 05/06/02: 0.9 - initial release
" 06/26/02: 1.0 - add mapping to display mapping
" 06/27/02: 1.1 - preserve current mapping -- thanks to Salman Halim!
" 09/28/02: 1.2 - cycle to next window -- thanks to Eric Arnold!
" 09/30/02: 1.21 - fix to get around cmap <C-V> issue
" 11/26/02: 1.22 - cycle to previous window
" 06/02/03: 1.3 - changes for Vim 6.2


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
      let s:klist = s:klist . ':sil! nunmap \'.strpart(s:kdict, s:i, 1).'<CR>'
      let s:i = s:i + 1
   endwhile
endfunction

" Add mapping to show key mapping
function! s:ShowKeyMapping()
   let s:j = stridx(s:klist, 'remote_foreground("' . s:sname . '")')
   let s:k = strpart(s:klist, s:j - 13, 1)
   let s:j = ':nn \. :echo "' . s:sname . ': \\' . s:k . '"'
   let s:j = ":exec '" . s:j . "'.nr2char(13)<CR>"

   let s:k = s:klist . s:j
endfunction

" Create key mapping for one client
function! s:SwitchWindow()
   let s:klist = s:klist . ":exec ':nn \\" . s:k . " "
   let s:klist = s:klist . ':sil! call remote_foreground("' . s:sname . '")'
   let s:klist = s:klist . "'.nr2char(13)<CR>"
endfunction

" Create key mappings for all clients
function! s:CreateKeyMaps()
   " Get key mappings for clients that already have them
   let s:i = 0
   while strlen(s:kdict) > 0
      call s:ServerName()
      if s:sname == ''
         " No more windows
         break
      endif

      " Get key mapping
      let s:k = remote_expr(s:sname, "maparg('\\.', 'n')")
      let s:j = stridx(s:k, s:sname . ': \')
      if s:j > 0
         " Client already has shortcut key defined.
         " Get current key
         let s:k = strpart(s:k, s:j + strlen(s:sname) + 4, 1)

         " Remove key from dictionary
         let s:j = stridx(s:kdict, s:k)
         let s:kdict = strpart(s:kdict, 0, s:j) . strpart(s:kdict, s:j + 1)
         call s:SwitchWindow()
      endif

      let s:i = s:i + 1
   endwhile

   " Assign keys to clients that are not yet assigned
   let s:i = 0
   while strlen(s:kdict) > 0
      call s:ServerName()
      if s:sname == ''
         break
      endif

      " Get key mapping
      let s:k = remote_expr(s:sname, "maparg('\\.', 'n')")
      let s:j = stridx(s:k, s:sname . ': \')
      if s:j < 0
         " Client do not have shortcut key defined yet.
         " Use first key available from dictionary
         let s:k = strpart(s:kdict, 0, 1)
         let s:kdict = strpart(s:kdict, 1)
         call s:SwitchWindow()
      endif

      let s:i = s:i + 1
   endwhile
endfunction

" Send keystrokes to a VIM window
function! s:SendKeys()
   if v:version < 602
      " Before Vim 6.2, remote_send commands would flood the history,
      " so remove commands for everything sent
      let s:k = substitute(s:k, "<CR>", "<CR>:call histdel(':', -2)<CR>", "g")
      call remote_send(s:sname, s:k . '\.')
   else
      " As of Vim 6.2, remote_send is no longer recorded in the history
      call remote_send(s:sname, s:k . ':normal \.<CR>')
   endif
endfunction

" Broadcast key mappings to each VIM window
function! s:Broadcast()
   " Generate common client keystrokes
   let s:klist = '<C-\><C-N>'
   call s:UnmapKeys()
   call s:CreateKeyMaps()

   " Broadcast to everyone
   let s:i = 0
   while 1
      call s:ServerName()
      if s:sname == ''
         break
      endif
      call s:ShowKeyMapping()
      call s:SendKeys()
      let s:i = s:i + 1
   endwhile
endfunction


" Window cycling functions
function! s:CycleWindow()
   " Get key mapping for current window
   let s:k = maparg('\.', 'n')
   let s:j = stridx(s:k, ': \\')
   if s:j < 1
      " No mapping defined for current window, maybe user undefined it?
      unlet s:j s:k
      return
   endif
   let s:k = strpart(s:k, s:j + 4, 1)

   " Set dictionary for cycle order.
   " Windows are cycled by alphabetical order of key mapping assigned to them.
   " Because key mappings are consistent across windows, this guarantees
   " that the cycle order will also be consistent.
   let s:kdict = strpart(s:kdict, stridx(s:kdict, s:k) + 1) . s:kdict

   " Cycle through key mappings until a good window is found
   let s:i = ''
   let s:cmd = ''
   while s:kdict != ''
      let s:cmd = maparg("\\" . strpart(s:kdict, 0, 1), 'n')
      if s:cmd != ''
         " Found good mapping, check if next window still exists
         let s:i = strpart(s:cmd, stridx(s:cmd, '"') + 1)
         let s:i = strpart(s:i, 0, stridx(s:i, '"'))
         silent! let s:i = remote_expr(s:i, "maparg('\\.', 'n')")
         if s:i != ''
            " Good window found, bring to foreground (later)
            break
         endif
         let s:cmd = ''
      endif
      let s:kdict = strpart(s:kdict, 1)
   endwhile

   " Release storage
   unlet s:i s:j s:k s:kdict
endfunction

function! CycleNextWindow()
   let s:kdict = 'abcdefghijklmnopqrstuvwxyz'
   call s:CycleWindow()

   " Bring next window to foreground now.
   " This must be the last command, otherwise original window regains focus.
   exec s:cmd
endfunction

function! CyclePreviousWindow()
   let s:kdict = 'zyxwvutsrqponmlkjihgfedcba'
   call s:CycleWindow()
   exec s:cmd
endfunction


" Build/broadcast key mappings
function! MapAllWindows()
   " Delayed load
   if exists('s:reload')
      " Clear trigger
      unlet s:reload
      augroup ServerList
         autocmd!
      augroup END
      augroup! ServerList

      " Try getting list of windows again
      let s:slist = serverlist()
      if s:slist == ''
         echomsg 'Can not get list of VIM windows'
         unlet s:slist
         return
      endif
   else
      let s:slist = serverlist()
   endif

   " Setup keys
   if s:slist == ''
      " List of windows not available
      if has('autocmd')
         " If VIM was built with autocommands, try mapping key names later
         " Function is tied to BufWinEnter, not VimEnter or GUIEnter, etc.
         " Tests show that not all server names are available at earlier times.
         augroup ServerList
            autocmd!
            autocmd BufWinEnter * call MapAllWindows()
         augroup END
         let s:reload = 1
      else
         " VIM was not built with autocommands, give up
         echomsg 'Can not get list of VIM windows'
      endif
   else
      " Windows enumerated okay, proceed to setup keys
      let s:kdict = 'abcdefghijklmnopqrstuvwxyz'
      call s:Broadcast()

      " Release storage
      unlet s:i s:j s:k s:slist s:sname s:klist s:kdict
   endif
endfunction


" Script entry point
if !has('clientserver')
   echoerr 'VIM was not compiled with +clientserver'
else
   exec 'nnoremap \, :call MapAllWindows()' . nr2char(13)
   exec 'nnoremap \\ :call CycleNextWindow()' . nr2char(13)
   exec 'nnoremap \/ :call CyclePreviousWindow()' . nr2char(13)
   call MapAllWindows()
endif
