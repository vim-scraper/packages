" screen(1)-like mappings for buffers
" Ricardo Signes <rjbs-vim@public.manxome.org>

" there should contain a ^A between the quotes, below
" the next revision will have a nicer system for loading this plugin with a
" different meta/leader character
let mapleader = "" 

nnoremap <Leader><Space> :bnext<CR>			" space -> next buffer
nnoremap <Leader>n       :bnext<CR>			" n     -> next buffer
nnoremap <Leader><C-n>   :bnext<CR>			" <C-n> -> next buffer
nnoremap <Leader>p       :bprev<CR>			" p     -> prev buffer
nnoremap <Leader><C-p>   :bprev<CR>			" <C-p> -> prev buffer
nnoremap <Leader>c       :new<CR>           " <C-c> -> new buffer
nnoremap <Leader><C-c>   :new<CR>           " <C-c> -> new buffer
nnoremap <Leader>K       :bdel<CR>          " K     -> new buffer
nnoremap <Leader>S       :split<CR>         " K     -> split
nnoremap <Leader><Tab>   :wincmd w<CR>      " <Tab> -> next window
nnoremap <Leader>Q       :only<CR>          " Q     -> only
nnoremap <Leader>Q       :only<CR>          " Q     -> only
nnoremap <Leader>w       :ls<CR>            " w     -> list buffers
nnoremap <Leader><C-w>   :ls<CR>            " <C-w> -> list buffers
nnoremap <Leader>a       :e #<CR>           " a     -> previous buffer
nnoremap <Leader><C-a>   :e #<CR>           " <C-a> -> previous buffer
