function! Delete_key(...)

  let line=getline (".")
  if line=~'^\s*$'
    execute "normal dd"
    return
  endif

  let column = col(".")
  let line_len = strlen (line)
  let first_or_end=0

  if column == 1
    let first_or_end=1
  else
    if column == line_len
      let first_or_end=1
  endif
  endif

  execute "normal i\<DEL>\<ESC>"

  if first_or_end == 0
     execute "normal l" 
  endif

endfunction


function! BS_key(...)

  let column = col(".")
  "call Decho ("colum: " . column)

  execute "normal i\<BS>\<ESC>"

    if column == 1
      let column2 = col (".")
      if column2 > 1
          execute "normal l"
      endif
    else
      if column > 2
        execute "normal l" 
      endif
    endif

endfunction       


function! TAB_key (...)

  "call Decho ("TAB_key")

  let start_pos = col(".")

  execute "normal i\<TAB>"

  let end_pos = col(".")
  let diff = end_pos - start_pos
  let counter = 0
  

  "ugly :)
  while 1==1
    execute "normal l"
    let counter= counter + 1
    if counter >= diff
      break
    endif
  endwhile

  execute "normal \<ESC>"
       
endfunction



nnoremap <silent> <DEL> :call Delete_key()<CR>
nnoremap <silent> <CR> i<CR><ESC>
nnoremap <silent> <SPACE> i<SPACE><ESC>l
nnoremap <silent> <TAB> :call TAB_key()<CR>
nnoremap <silent> <BS> :call BS_key()<CR>
