" SyntaxMotion: Cursor motion & visual selection by syntax highlighting group
" Maintainer:   Dominique Pellé <dominique.pelle@gmail.com>
" Last Change:  2010/02/11
" Version:      0.5
"
" Long Description:
"
" This plugin provides cursor motion by syntax highlighting groups in normal
" and visual mode:
"
" - Press  \<right>  (actually <Leader><right>) to move the cursor forward to 
"   the end of the syntax group where cursor is located.
"
" - Press  \<left>  (actually <Leader><right>) to move the cursor forward to 
"   the beginning of the syntax group where cursor is located.
"
" For example, when inside a string, pressing  \<right>  moves to the end of 
" the string.  When inside a comment, pressing  \<left>  moves to the 
" beginning of the comment.  You can extrapolate this behavior for any other
" of syntax group.
"
" Repeating the motion multiple times will move to the next syntax
" highlighting group (forward or backward). A repeat count number can also be
" given before typing  \<left>  or  \<right>.  For example, typing  3\<left>
" moves the cursor to the left by 3 syntax elements.
"
" Strictly speaking, motion actually moves to the start or end of the text
" which have the same "color" (rather than same "syntax group") as the color
" of the text where cursor is originally located. So if your color scheme
" defines the same colors for multiple syntax groups, motion may actually span
" multiple consecutive syntax groups if they have the same colors.  Motion is
" based on syntax highlighting colors rather than syntax highlighting groups
" to make behavior more intuitive (what you see is what you get) since users
" see colors on the screen but may not be aware of different syntax highlight
" groups.
"
" Plugin also provides a way to select text visually around the position
" of the cursor with the same syntax group as where cursor is located:
"
" - type  va<right>  to visually select the text with the same syntax as 
"   where cursor is located, and move the cursor to the end of the selected
"   text.
"
" - type  va<left>  to visually select the text with the same syntax as 
"   where cursor is located, and move the cursor to the beginning of the 
"   selected text.
"
" Syntax highlighting must be enabled for the plugin to work.
"
" License: The VIM LICENSE applies to SyntaxMotion.vim (see ":help copyright"
" except use "SyntaxMotion.vim" instead of "Vim").
" 
" ToDo:
" - write a proper help page
" - provide mapping in operator pending mode


" This function moves by cursor by syntax.
" - a:dir is 'f' or 'b' to move cursor (f)orward or (b)ackward to the
"   end or beginning of current syntax block, 'F' or 'B' to move
"   cursor (F)ordward or (B)ackward & to the next syntax block when at 
"   the end of beginning of current syntax block (this allows to move 
"   through multiple syntax blocks when calling SyntaxMotion('F', ...) 
"   or SyntaxMotion('B', ...) multiple times.
" - a:mode is either 'v' when in (v)isual mode or 'n' when in (n)ormal mode.
" - a:count is the repeat count.
function! SyntaxMotion(dir, mode, count)
  if a:mode == 'v'
    normal gv
  endif

  let l:count = a:count
  while l:count > 0
    if a:dir ==# 'B' || a:dir ==# 'F'
      " Move at least by one character to be able to move to next syntax group
      " when repeating motion multiple times.
      call search('.', (a:dir ==# 'B' ? 'bW' : 'W'))
    endif
    let l:synID = synIDtrans(synID(line("."),col("."), 1))
    let l:fg1   = synIDattr(l:synID, 'fg')
    let l:bg1   = synIDattr(l:synID, 'bg')
    while 1
      let l:save_cursor = getpos(".")
      call search('.', (a:dir ==? 'b' ? 'bW' : 'W'))
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

" This function selects text visually by syntax.
" - a:dir is 'f' if cursor should be moved to the end of the visually
"   selected text, or 'b' to move the cursor to the beginning of the
"   visually selected text.
function! SyntaxVisualSelect(dir)
  let l:save_cursor = getpos(".")
  call SyntaxMotion(a:dir == 'f' ? 'b' : 'f', 'n', 1)
  exe "normal \<esc>"
  normal v
  " Using setpos() is not strictly required, but it speeds up moving to the
  " other side of the text.
  call setpos('.', l:save_cursor) 
  call SyntaxMotion(a:dir, 'n', 1)
endfunction

" Motion by syntax (in normal & visual mode):
" - use  \<left>  to move the end of text with same syntax as
"   where cursor is located.
" - use  \<right>  to move the end of text with same syntax as
"   where cursor is located.
nnoremap <silent> <Leader><left>  :<c-u>call SyntaxMotion('B', 'n', v:count1)<cr>
vnoremap <silent> <Leader><left>  :<c-u>call SyntaxMotion('B', 'v', v:count1)<cr>
nnoremap <silent> <Leader><right> :<c-u>call SyntaxMotion('F', 'n', v:count1)<cr>
vnoremap <silent> <Leader><right> :<c-u>call SyntaxMotion('F', 'v', v:count1)<cr>

" Visual selection by syntax:  
" - use  va<left>  to make a visual selection by syntax, and move the
"   cursor to the end of the selection.
" - use  va<right>  to make visual selection by syntax, and move the
"   cursor to the beginning of the selection.
vnoremap <silent> a<left>  :<c-u>call SyntaxVisualSelect('b')<cr>
vnoremap <silent> a<right> :<c-u>call SyntaxVisualSelect('f')<cr>
