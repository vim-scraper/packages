" Just another justifier
" Version:    0.1
" Maintainer: Kontra, Gergely <kgergely@mcl.hu>
fu! <SID>Justify(...) range
  if a:0|let tw=&tw|let &tw=a:1|endif
  exe a:firstline
  exe 'norm! V'.a:lastline.'Ggq'
  let lastline=line('.')
  let s=@/|exe a:firstline.','.lastline.'s/\s\+/ /ge'|let @/=s
  let i=a:firstline
  while i<=lastline "NOT a:lastline!!!
    exe i
    let i=i+1
    if getline('.') !~ '\w\s'
      continue
    endif
    while strlen(getline('.'))<&tw
      norm E
      if strpart(getline('.'),col('.'))=~'^\s*$'
	norm ^E
      endif
      exe "norm! a \<Esc>"
    endw
  endw
  if a:0
    let &tw=tw
  endif
endf
com! -nargs=? -range Justify <line1>,<line2>call <SID>Justify(<args>)

