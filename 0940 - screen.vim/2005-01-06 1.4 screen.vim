" screen(1)-like mappings for buffers
" Ricardo Signes <rjbs-vim@public.manxome.org>

" there should contain a ^A between the quotes, below
" the next revision will have a nicer system for loading this plugin with a
" different meta/leader character
let mapleader = "" 

nnoremap <Leader><Space> :bnext<CR>
nnoremap <Leader>n       :bnext<CR>
nnoremap <Leader><C-n>   :bnext<CR>
nnoremap <Leader>p       :bprev<CR>
nnoremap <Leader><C-p>   :bprev<CR>
nnoremap <Leader>c       :new\|only<CR>
nnoremap <Leader><C-c>   :new\|onlyCR>
nnoremap <Leader>K       :bdel<CR>
nnoremap <Leader>S       :split<CR>
nnoremap <Leader><Tab>   :wincmd w<CR>
nnoremap <Leader>Q       :only<CR>
nnoremap <Leader>Q       :only<CR>
nnoremap <Leader>w       :ls<CR>
nnoremap <Leader><C-w>   :ls<CR>
nnoremap <Leader>a       :e #<CR>
nnoremap <Leader><C-a>   :e #<CR>
nnoremap <Leader>"       :BufExp<CR>
