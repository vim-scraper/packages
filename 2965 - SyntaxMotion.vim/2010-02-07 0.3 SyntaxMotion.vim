" SyntaxMotion:     Cursor motion by syntax highlighting group
" Maintainer:       Dominique Pellé <dominique.pelle@gmail.com>
" Last Change:      2010/02/07
" Version:          0.3
"
" Long Description: This plugin provides cursor motion by syntax highlighting
"                   groups in normal and visual mode. Press \<right> (actually
"                   <Leader><right>) or \<left> (actually <Leader><left>) to
"                   move the cursor forward or backward until the end or the
"                   beginning of the text which has the same syntax
"                   highlighting group as syntax highlighting group where
"                   cursor is originally located. For example, if cursor is
"                   currently inside a comment, pressing \<left> will move the
"                   cursor to the beginning of the comment. If cursor is in a
"                   string, pressing \<right> will move the cursor to the end
"                   of the string, etc.  Repeating the motion multiple times
"                   will move to the next syntax highlighting group (forward
"                   or backward). A repeat count number can be given before
"                   typing \<left> or \<right>. Motion actually moves to the
"                   start or end of the text which have the same color (rather
"                   than same syntax group) as the color of the text where
"                   cursor is originally located. So if your color scheme
"                   defines the same colors for multiple syntax groups, motion
"                   may actually span multiple consecutive syntax groups if
"                   they have the same colors.  Motion is based on syntax
"                   highlighting colors rather than syntax highlighting groups
"                   to make behavior more intuitive (what you see is what you
"                   get) since users see colors on the screen but may not be
"                   aware of different syntax highlight groups.
"
" License:          The VIM LICENSE applies to SyntaxMotion.vim
"                   (see ":help copyright" except use "SyntaxMotion.vim" 
"                   instead of "Vim").
" 
function! SyntaxMotion(dir, mode, count)
  if a:mode == 'v'
    normal gv
  endif

  let l:count = a:count
  while l:count > 0
    " Move at least by one character to be able to move to next syntax group
    " when repeating motion multiple times.
    call search('.', (a:dir == 'f' ? 'bW' : 'W'))
    let l:synID = synIDtrans(synID(line("."),col("."), 1))
    let l:fg1   = synIDattr(l:synID, 'fg')
    let l:bg1   = synIDattr(l:synID, 'bg')
    while 1
      let l:save_cursor = getpos(".")
      call search('.', (a:dir == 'f' ? 'bW' : 'W'))
      let l:synID = synIDtrans(synID(line("."),col("."), 1))
      let l:fg2   = synIDattr(l:synID, 'fg')
      let l:bg2   = synIDattr(l:synID, 'bg')
      if  l:fg1 != l:fg2 || l:bg1 != l:bg2 || getpos(".") == l:save_cursor
        call setpos('.', l:save_cursor)
        break
      endif
    endwhile
    let l:count = l:count - 1
  endwhile

  if a:mode == 'v'
    exe "normal \<esc>"
    normal gv
  endif
endfunction

nnoremap <silent> <Leader><left>  :<c-u>call SyntaxMotion('f', 'n', v:count1)<cr>
vnoremap <silent> <Leader><left>  :<c-u>call SyntaxMotion('f', 'v', v:count1)<cr>
nnoremap <silent> <Leader><right> :<c-u>call SyntaxMotion('b', 'n', v:count1)<cr>
vnoremap <silent> <Leader><right> :<c-u>call SyntaxMotion('b', 'v', v:count1)<cr>
