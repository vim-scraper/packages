" janitor.vim
"
" plugin created just for remove extra whitespaces at
" end of each line
"
"                                         by Aleksandr Koss (http://nocorp.me)

" Remove all spaces from end in each line
fun! RemoveSpaces()
  if &bin | return | endif
  if search('\s\+$', 'n')
    let line = line('.')
    let col = col('.')
    sil %s/\s\+$//ge
    call cursor(line, col)
  endif
endf

" Bind RemoveSpaces to autocommand
autocmd BufWritePre * call RemoveSpaces()
