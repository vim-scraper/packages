function Xmms_Play_Mp3(xmms_opts)
    let line = getline(".")
    let repl = substitute(line, "'", "'\\\\''", "ge")
    let repl = substitute(repl, "!", "\\\\!", "g")
    execute "silent !xmms " . a:xmms_opts . " '" . repl . "'"
endfunction

map <F3> :call Xmms_Play_Mp3("-e")<CR>
map <S-F3> :call Xmms_Play_Mp3("")<CR>

