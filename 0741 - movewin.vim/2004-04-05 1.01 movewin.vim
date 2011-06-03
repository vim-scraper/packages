" movewin.vim - Don Yang (uguu.org)
"
" Move GVIM window using the keyboard.
" After loading the plugin, use :MoveWin
"
"              k/K move up
"  h/H = move left      l/L = move right
"              j/J move down
"
" All other keys = done
"
" 08/22/03 - initial version
" 04/05/04 - redraw title when lazyredraw is not set



function! MoveWin()
   let s:d1 = 4
   let s:d2 = 16

   let s:t = &titlestring
   let s:x = getwinposx()
   let s:y = getwinposy()
   let s:k = 'k'

   if s:x == -1 || s:y == -1
      echoerr 'Can not get window position'
   else
      while stridx('hjklHJKL', s:k) >= 0
         let &titlestring = 'Moving window: (' . s:x . ', ' . s:y . ')'
         if ! &lazyredraw
            redraw
         endif

         let s:k = nr2char(getchar())
         if s:k ==? 'h'
            let s:x = s:x - s:d1
            if s:k == 'h'
               let s:x = s:x - s:d2
            endif
         endif
         if s:k ==? 'j'
            let s:y = s:y + s:d1
            if s:k == 'j'
               let s:y = s:y + s:d2
            endif
         endif
         if s:k ==? 'k'
            let s:y = s:y - s:d1
            if s:k == 'k'
               let s:y = s:y - s:d2
            endif
         endif
         if s:k ==? 'l'
            let s:x = s:x + s:d1
            if s:k == 'l'
               let s:x = s:x + s:d2
            endif
         endif
         exec ':winpos ' . s:x . ' ' . s:y

      endwhile
   endif

   let &titlestring = s:t
   unlet s:k s:x s:y s:d1 s:d2 s:t
endfunction

command! MoveWin call MoveWin()
