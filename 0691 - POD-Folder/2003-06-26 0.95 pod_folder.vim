set foldmethod=manual
set foldcolumn=4
set nowrapscan

normal zE                                   " clear any existing folds

normal gg                                   " start at the top
while (line(".") != line("$"))              " loop to the end
    let @/ = "\\(=head3\\|\\%$\\)"          " look for ^=head3 or the end of the file
    normal nv                               " find the next and go to visual mode
    if (line(".") == line("$"))             " exit if we have the last line
        break
    endif
    let @/ = "\\(=head[123]\\|\\%$\\)"      " next look for =head1, =head2, =head3 or the end
    normal nkkzf                            " after finding one go up 2 lines and make a fold
    if (line(".") == line("$"))             " exit if we have the last line
        break
    endif
endwhile
normal v

normal gg
while (line(".") != line("$"))
    let @/ = "\\(=head2\\|\\%$\\)"          " same as above, but with =head2
    normal nv
    if (line(".") == line("$"))
        break
    endif
    let @/ = "\\(=head[12]\\|\\%$\\)"
    normal nkkzf
    if (line(".") == line("$"))
        break
    endif
endwhile
normal v

normal G
while (line(".") > 1)                       " this one goes backwards
    let @/ = "=head1"                       " and picks up the =head1 tags
    if (line(".") == 1)
        break
    endif
    normal vNzf
    if (line(".") == 1)
        break
    endif
    normal kk
endwhile

set wrapscan
