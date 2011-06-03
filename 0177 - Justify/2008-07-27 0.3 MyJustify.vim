" Just another justifier
" Version:    0.3
" Maintainer: Kontra, Gergely <kgergely@mcl.hu>
" GetLatestVimScripts: 177 2 :AutoInstall: MyJustify.vim
" Improved by Charles E. Campbell, Jr.
" Multibyte patch suggested by Teemu Likonen
fun! <SID>Justify(...) range
  if a:0|let tw=&tw|let &tw=a:1|endif
  exe a:firstline
  exe 'norm! V'.a:lastline.'Ggq'
  let lastline=line('.')
  let s=@/|exe 'silent '.a:firstline.','.lastline.'s/\s\+/ /ge'|let @/=s
  let i=a:firstline
  while i<=lastline "NOT a:lastline!!!
    exe i
    let i=i+1
    if getline('.') !~ '\w\s'
      continue
    endif
    while strlen(substitute(getline('.'),'.','x','g'))<&tw
      silent! norm! E
      if strpart(getline('.'),col('.'))=~'^\s*$'
        norm! ^E
      endif
      exe "norm! a \<Esc>"
    endw
  endw
  if a:0
    let &tw=tw
  endif
endfun
com! -nargs=? -range Justify <line1>,<line2>call <SID>Justify(<args>)

