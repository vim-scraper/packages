" autoinclude.vim
" Maintener: pyrho
" 18 June 2008

if has("autocmd")
  aug coding
    au BufEnter *.hh noremap ;; :call AutoIncludeHH()<CR>
    au BufEnter *.cc noremap ;; :call AutoIncludeCC()<CR>
  aug END

  function AutoIncludeHH()
    let class = expand("<cword>")
    let old_line_num = line(".")
    let x = substitute(class, '\([A-Z]\)\(\w\+\)\([A-Z]\)\(\w\+\)', 'class \1\2\3\4;', "g")
    if (class == x)
      let x = substitute(class, '\([A-Z]\)\(\w\+\)', 'class \1\2;', "g")
    endif
    if (exists('g:AutoIncludeLine'))
      let line = g:AutoIncludeLine
    else
      line = 5
    endif
    execute '' . line
    normal O
    execute 's/$/\=x/'
    normal ==
    execute '' . old_line_num + 1
  endfun

  function AutoIncludeCC()
    let class = expand("<cword>")
    let old_line_num = line(".")
    let x = substitute(class, '\([A-Z]\)\(\w\+\)\([A-Z]\)\(\w\+\)', '#include "\l\1\2-\l\3\4.hh"', "g")
    if (class == x)
      let x = substitute(x, '\([A-Z]\)\(\w\+\)', '#include "\l\1\2.hh"', "g")
    endif
    if (exists('g:AutoIncludeLine'))
      let line = g:AutoIncludeLine
    else
      line = 5
    endif
    execute '' . line
    normal O
    execute 's/$/\=x/'
    normal ==
    execute '' . old_line_num + 1
  endfun
endif
