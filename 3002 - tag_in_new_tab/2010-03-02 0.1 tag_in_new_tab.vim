" tag_in_new_tab 0.1
" open tag in new tab
" Miroslav Tynovsky

function! TagInNewTab()
    let word = expand("<cword>")
    redir => tagsfiles
    silent execute 'set tags'
    redir END
    tabe
    execute 'setlocal' . strpart(tagsfiles, 2)
    execute 'tag ' . word
endfunction

nmap    <S-Enter>    :call TagInNewTab()<CR>

