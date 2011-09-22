" Summary:     Quick run single c source file in vim
" Description:
"           This is a smart utility for quick run single c source file in vim.
"
"           It has follow features:
"               1. Use gcc compiler.
"               2. Compile and run single c source file just one command.
"               3. Program's output is showing below the source file in a new window.
"               4. You can hide the output window easily.
"               5. When there are some errors, quick window will automatically appears.
"               6. When there is no error, quick window hides automatically and the program is running.
"               7. When you compile it, source code is saved by the script, so you need not to save it.
"               8. Only three commands, easily to remember, easily to use.
"
"           The most cool things are:
"                1. Run source file just one command.
"                2. Quick window shows and hides automatically.
"                3. Everything happened in vim, no ugly command window.
"
" Screenshot:
"           1. Has bugs, quick window appears
"               http://vimer.1063083.n5.nabble.com/Quickfix-window-td4791439.html
"           2. Output in vim, no command window
"               http://vimer.1063083.n5.nabble.com/Output-window-td4791441.html 
"
" Author:   Tian Huixiong: <nedzqbear@gmail.com>
"           I'm very glad to receive your feedback.

" Version:  1.5
" Update:   2011-09-14
" Licence:  This script is released under the Vim License.
"
" Install:
"     Put this file in ~/.vim/plugin on *nix.
"     Or put it in $vim/vimfiles/plugin on Windows.

" Tutorial:
"     compile and run:      ,g   (g means gcc)
"     run the program:      ,r   (r means run)
"     hide the output:      ,h   (h means hide)
"
"     If you don't like these mapping, just modify the source code.


function! Gcc()
    let filename = expand('%')
    let root = expand('%:r')
    let makeprg  = 'set makeprg=gcc\ -Wall\ -o' . root . '\ ' . filename
    silent! exe makeprg
    silent! exe ':update'

    silent! exe 'make'

    call HideOutput()
    call Quickfix()
endfunction

function! Quickfix()
    let list = getqflist()
    let bugs = len(list)

    if bugs == 0
        echo 'Compile success!'
        " Hide the quickfix window
        silent! exe 'cw'
        call Run()
    else
        echo 'Fix bugs first.'
        " Show the quickfix window
        silent! exe 'cw ' . string((bugs + 1) > 9 ? 9 : (bugs + 1))
    endif
endfunction

function! HideOutput()
    let output_file = string(getpid()) . '.output'
    let bufnumlist  = tabpagebuflist()

    for bufnum in bufnumlist
        let file = bufname(bufnum)
        if file =~# output_file 
            silent! exe string(bufnum) . ' bwipe!' 
            break
        endif
    endfor

    silent! exe ':setlocal laststatus=2'
endfunction

function! Run()
    call HideOutput()
    
    let src_winnr   = winnr()
    let output_file = string(getpid()) . '.output'
    let bin_file    = expand('%:p:r')

    if executable(bin_file) != 1
        " Program not exist
        echo bin_file . '.exe not exist.'
        return
    endif

    redir => sb_message
    silent! set sb?
    redir END
    let sb_message = 'set '. substitute(sb_message, '^\W\s*', '', '')

    :set splitbelow
    silent! exe '6split ' . output_file 
    silent! exe '%!' . bin_file

    "silent! exe ':update'
    silent! exe ':set filetype=tmp'
    silent! exe ':setlocal laststatus=0'
    :set nosplitbelow

    silent! set sb_message
    silent! exe src_winnr . 'wincmd w'
endfunction

autocmd FileType c,cpp  nnoremap <buffer><silent> ,g  :call Gcc()<cr>
autocmd FileType c,cpp  nnoremap <buffer><silent> ,r  :call Run()<cr>
autocmd FileType c,cpp  nnoremap <buffer><silent> ,h  :call HideOutput()<cr>
autocmd FileType tmp    nnoremap <buffer><silent> ,h  :q!<cr> 

