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
"               1. Everything happened in vim, no ugly command window.
"               2. Quick window shows and hides automatically.
"               3. Run the source file just one command.
" Screenshot:
"           1. Has bugs, quick window appears
"               http://vimer.1063083.n5.nabble.com/Quickfix-window-td4791439.html
"           2. Output in vim, no command window
"               http://vimer.1063083.n5.nabble.com/Output-window-td4791441.html 
"
" Author:   Tian Huixiong: <nedzqbear@gmail.com>
"           I'm very glad to receive your feedback.

" Version:  1.1
" Update:   2011-09-13
" Licence:  This script is released under the Vim License.
"
" Install:
"     Put this file in ~/.vim/plugin on *nix.
"     Or put it in $vim/vimfiles/plugin on Windows.

" Tutorial:
"     Add these to your vimrc file:
"     "-----gccsingle.vim-----: Quick run single c source file in vim
"     autocmd FileType c,cpp  nnoremap <buffer><silent> ,g  :call Gcc()<cr><c-w>w
"     autocmd FileType c,cpp  nnoremap <buffer><silent> ,r  :call RunWin()<cr>
"     autocmd FileType c,cpp  nnoremap <buffer><silent> ,h  :call HideOutputWin()<cr>
"     autocmd FileType tmp    nnoremap <buffer><silent> ,h  :q!<cr>
"     
"     compile and run:      ,g   (g means gcc)
"     run the program:      ,r   (r means run)
"     hide the output:      ,h   (h means hide)


function! Gcc()
    let filename = expand('%')
    let root = expand('%:r')
    let makeprg  = 'set makeprg=gcc\ -Wall\ -o' . root . '\ ' . filename
    silent! exe makeprg
    silent! exe ':update'

    silent! exe 'make'

    call HideOutputWin()
    call ShowQuickfix()
endfunction

function! ShowQuickfix()
    let list = getqflist()
    let bugs = len(list)

    if bugs == 0
        echo ' Compile success!'
        " Hide the quickfix window
        silent! exe 'cw'
        " Run it
        call RunWin()
    else
        echo ' Fix bugs first.'
        " Show the quickfix window
        silent! exe 'cw ' . string((bugs + 1) > 9 ? 9 : (bugs + 1))
    endif

endfunction

function! HideOutputWin()
    let output_file = string(getpid()) . '.output'
    let bufnumlist  = tabpagebuflist()

    for bufnum in bufnumlist
        let file = bufname(bufnum)
        if file =~# output_file 
            silent! exe string(bufnum) . ' bwipe!' 
            break
        endif
    endfor

endfunction

function! RunWin()
    call HideOutputWin()
    
    let root        = expand('%:r')
    let output_file = string(getpid()) . '.output'
    let bin_file    = root

    redir => sb_message
    :silent! set sb?
    redir END
    let sb_message = 'set '. substitute(sb_message, '^\W\s*', '', '')

    :set splitbelow
    silent! exe '6split ' . output_file 
    silent! exe '%!' . bin_file

    silent! exe ':update'
    silent! exe ':set filetype=tmp'
    :set nosplitbelow

    :silent! set sb_message
endfunction
