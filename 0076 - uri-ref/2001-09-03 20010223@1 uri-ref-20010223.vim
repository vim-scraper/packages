" -------------------------------------------
" Commands for quoting URIs in emails
" Copyright (c) 2001 Hugo Haas
" 23 Feb 2001 version
" I hereby put this code in the public domain.
" Documentation at: http://larve.net/people/hugo/2001/02/email-uri-refs/
" --------------------------------------------

" Insert a reference at the cursor position
function InsertRef()
  let ref = input("Reference: ")
  call AskNumber()
  if (col(".") == 1)
    execute "normal i[\<C-R>r]\<ESC>"
  else
    execute "normal a[\<C-R>r]\<ESC>"
  endif
  normal G
  set paste
  ?^-- $
  execute "normal O  " . @r . ". " . ref . "\<ESC>`rf]"
  set nopaste
  let a = col(".")
  normal $
  let b = col(".")
  if ( a == b )
    startinsert!
  else
    normal `rf]l
    startinsert
  endif
endfuntion

" Convert <http://example.com> into a reference
function ConvertToRef()
  call AskNumber()
  execute "normal cf>[\<C-R>r]\<ESC>G"
  ?^-- $
  execute "normal O\<ESC>"
  set paste
  execute "normal px0x>>i\<C-R>r. \<ESC>`r"
  set nopaste
endfunction

" Ask a reference number
function AskNumber()
  if (@r == "")
    call FindRefHiNumber()
  endif
  let @r = @r + 1
  let number = input("Reference number (" . @r . "): ")
  if ( number != "" )
    let @r = number
  endif
endfunction

" Find the highest number in the text
function FindRefHiNumber()
  normal 1G
  /^$
  let body = line(".")
  let cur = body + 1
  let found = 0
  let @/="\\[[0-9]\\+\\]"
  while ( cur <= line("$") )
    if (match(getline(cur), @/) != -1)
      let found = 1
      break
    endif
    let cur = cur + 1
  endwhile
  if ( found == 0 )
    let @r = 0
    normal `r
    return
  endif
  " Find the highest number
  normal n
  let l = line(".")
  let c = col(".")
  while (1)
    if ( line(".") > body )
      normal f]mqF[ly`qf]
      if ((@0 + 0) > @r)
        let @r = @0
      endif
      normal n
    endif
    if ( (line(".") < body) || ((l == line(".")) && (c == col("."))) ) 
      break
    endif
  endwhile
  normal `r
endfunction

map! <F5> <ESC>mr:call InsertRef()<CR>
map <F6> F<mr:call ConvertToRef()<CR>
