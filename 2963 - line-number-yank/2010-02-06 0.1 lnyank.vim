function PasteWithLineNumber() range
"only for file less than 1000 lines
  let @" = ''
  let s:head_blank = ""
  for s:i in range(a:firstline, a:lastline)
    if s:i < 10
      let s:head_blank = "  "
    elseif  s:i < 100
      let s:head_blank = " "
    endif
    let  @" .=  s:head_blank . s:i . "  " . getline(s:i) . "\n"
  endfor
endfunction
map \ny :call PasteWithLineNumber()<cr>
