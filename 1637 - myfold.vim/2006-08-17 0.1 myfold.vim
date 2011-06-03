"  File: myfold.vim
"  Author: Taku Maeda ( Japan )
"  Version: 0.1 Beta
"  Last Modified: August 18, 2006
" 
"  This plugin provide functionality to enable or disalbe
"  fold more easier.
"  Parameters are based on my favorite setting( syntax base ), so please modify
"  these values directly in function if you need.
"
"  -----------------------------
"  Installation
"  -----------------------------
"  1. put this file to $HOME/.vim/plugin folder or other.
"  2. start vim and edit an source code.
"  3. now, you can type two commands, from vim command mode.
"
"  :MyFoldEnable
"  :MyFoldDisable
"
"  thats all.

"  -----------------------------
"  My private hilight and keymap in ~/.vimrc
"  -----------------------------
"  ## you can copy this if you need
"  # I don't like default fold hilight( I always use vim via SSH cterm ).
"  highlight Folded ctermfg=6 ctermbg=0
"  highlight FoldColumn ctermfg=6 ctermbg=0
"
"  # set keyboard shortcut.
"  nnoremap <silent> <F4> :MyFoldEnable<CR>
"  nnoremap <silent> <F5> :MyFoldDisable<CR>
"
"  -----------------------------
"  Short Tutorial
"  -----------------------------
"  1. edit program file( I like ruby ).
"  vim proglam.rb
"
"  2. type <F4> key in normal mode.
"  you can see folded lines( by syntax )and foldcolumn.
"
"  3. type <F4> key in normal mode.
"  hide foldcolumn. not change open/close state each fold.
"
"  4. type <F5> key in normal mode.
"  open all fold and hide foldcolumn.
"
function! MyFoldEnable()
  " midify 'foldcolumn' if you what
  if &foldcolumn == 4
    set foldcolumn=0
  else
    set foldcolumn=4
  endif

  if &foldmethod !=# 'syntax'
    set foldenable
    set foldmethod=syntax
    " foldnextmax must be 'foldcolumn' - 1
    set foldnestmax=3
    set foldlevel=1
  endif
endfunction

function! MyFoldDisable()
  if &foldmethod ==# 'syntax'
    set foldcolumn=0
    set foldmethod=manual
    set nofoldenable
  endif
endfunction

command! MyFoldEnable :call MyFoldEnable()
command! MyFoldDisable :call MyFoldDisable()
