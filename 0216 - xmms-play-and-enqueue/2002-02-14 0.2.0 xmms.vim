" Play or enqueue files using xmms.
" Each file must be present on its own line.
"
" algorithm:
" 1. Copy the entire line into register 5.
" 2. Paste the register on the line below.
" 3. Substitute single-quotes with the string '\'' (to denote a shell single
"    quote inside a single-quoted surrounded filename)
" 4. Copy this entire line.
" 5. Execute xmms or xmms -e on the filename.
" 6. Delete the line that was added.
map <F3> 0"5y$ji<CR><ESC>ki<C-R>5<ESC>:s/'/'\\''/ge<CR>0"5y$:!xmms -e '<C-R>5'<CR><CR>ddk
map <S-F3> 0"5y$ji<CR><ESC>ki<C-R>5<ESC>:s/'/'\\''/ge<CR>0"5y$:!xmms '<C-R>5'<CR><CR>ddk

