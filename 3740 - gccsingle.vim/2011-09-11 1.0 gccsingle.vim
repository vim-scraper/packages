" Summary:     Quick run single c source file in vim
" Description:
"           This is a smart utility for quick run single c source file in vim.
"
"           It has follow features:
"               1. Use gcc compiler.
"               2. Compile single c source file.
"               3. Run binary, its output is showing below the source file in a new window.
"               4. You can hide the output window easily.
"               5. When there are some errors, quick window will automatically appears.
"               6. When there is no error, quick window hides automatically.
"               7. When you compile it, source code is saved by the script, so you need not to save it.
"               8. Help tip is showing in command line.
"               9. Only three commands, easy to remember, easy to use.
"
"           The most cool things are:
"               1. Everything happened in vim, no ugly command window.
"               2. Quick window shows and hides automatically.
" Screenshot:
"           1. Has bugs, quick window appears
"               http://vimer.1063083.n5.nabble.com/Quickfix-window-td4791439.html
"           2. Output in vim, no command window
"               http://vimer.1063083.n5.nabble.com/Output-window-td4791441.html 
"
" Author:   Tian Huixiong: <nedzqbear@gmail.com>
"           I'm very glad to receive your feedback.

" Version:  1.0
" Update:   2011-09-11
" Licence:  This script is released under the Vim License.
"
" Install:
"     Put this file in ~/.vim/plugin on *nix.
"     Or put it in $vim/vimfiles/plugin on Windows.

" Tutorial:
"     Add these to your vimrc file:
"     "-----gccsingle.vim-----: Quick run single c source file in vim
"         autocmd FileType c,cpp  nnoremap <buffer><silent> ,g  :call Gcc()<cr>:call GccQuickfix()<cr>
"         autocmd FileType c,cpp  nnoremap <buffer><silent> ,r  :call RunWin()<cr><C-w>x<Esc>:call RunWinHelp()<cr>
"         autocmd FileType c,cpp  nnoremap <buffer><silent> ,h  :call HideOutputWin()<cr>:call HideOutputWinHelp()<cr>
"         autocmd FileType tmp    nnoremap <buffer><silent> ,h  :q!<cr>
"     
"     compile source file:  ,g   (g means gcc)
"     run the binary:       ,r   (r means run)
"     hide the output win:  ,h   (h means hide)


function! Gcc()
    let filename = expand('%')
    let root = expand('%:r')
    let makeprg  = 'set makeprg=gcc\ -Wall\ -o' . root . '\ ' . filename
    exe makeprg
    exe ':update'

    " redir to avoid pressing <Enter> when has compile errors
    redir => message
    silent execute ':make' 
    redir END
    "exe 'make'

    call HideOutputWin()
    "call GccQuickfix()
endfunction

function! GccQuickfix()
    let list = getqflist()
    let bugs = len(list)

    if bugs == 0
        echo ' Compile success!    Press ,r to run.'
    else
        echo ' Fix bugs first.'
    endif

    " Show the quickfix window
    exe 'cw ' . string((bugs + 1) > 9 ? 9 : (bugs + 1))
endfunction

function! Run()
    let root = expand('%:r')
    let cmd = '!' . root
    exe cmd
endfunction

function! HideOutputWin()
    let output_file = string(getpid()) . '.output'
    let bufnumlist  = tabpagebuflist()

    for bufnum in bufnumlist
        let file = bufname(bufnum)
        if file =~# output_file 
            exe string(bufnum) . ' bwipe!' 
            break
        endif
    endfor
endfunction

function! HideOutputWinHelp()
    echo ' Press ,g to compile;    Press ,r to run.'
endfunction
    
function! RunWin()
    call HideOutputWin()

    let root        = expand('%:r')
    let output_file = string(getpid()) . '.output'
    let bin_file    = root

    exe '6split ' . output_file 
    exe '%!' . bin_file

    exe ':update'
    exe ':set filetype=tmp'
    "normal! <c-w>x
    "echo ' Press ,h to hide output window.'
endfunction

function! RunWinHelp()
    echo ' Press ,h to hide output window.'
endfunction
