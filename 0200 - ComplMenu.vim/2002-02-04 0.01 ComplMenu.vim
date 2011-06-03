" ComplMenu.vim
" Version: 0.01
" Another completion function by Gregory Kontra 
" The minimal mappings used are:
" <C-L>  to get a list of matches
" <Tab>   to go to the next match
" <S-Tab>   to go to the previous match
" if g:AutoCompl is defined, the list is updated, as you type.
" (it can be slow!) OK, the number of the completion is limited to 32
" BUGS:
" * No unload
" * and more...

if !exists('complType') "Integration with other copmletion functions...
  let complType="\<C-p>"
  im <buffer> <C-X> <C-r>=CtrlXPP()<CR>
  im <buffer> <Tab> <C-P>
  im <buffer> <S-Tab> <C-N>
  im <buffer> <C-P> <C-r>=<SID>Compl(-1)<CR>
  im <buffer> <C-N> <C-r>=<SID>Compl(1)<CR>
  fu! CtrlXPP()
    ec''|ec '-- ^X++ mode (/^E/^Y/^L/^]/^F/^I/^K/^D/^V/^N/^P/n/p)'
    let complType=nr2char(getchar())
    if stridx(
	  \"\<C-E>\<C-Y>\<C-L>\<C-]>\<C-F>\<C-I>\<C-K>\<C-D>\<C-V>\<C-N>\<C-P>np",
	  \complType)!=-1
      if complType!="n" && complType!="p"
	let g:complType="\<C-x>".complType
      el
	let g:complType=nr2char(char2nr(complType)-96)  " char2nr('n')-char2nr("\<C-n")
      en
      if g:complType=="\<C-p>" && g:complType=='p'
	iun <buffer> <Tab>
	iun <buffer> <S-Tab>
	im <buffer> <Tab> <C-p>
	im <buffer> <S-Tab> <C-n>
      el
	iun <buffer> <Tab>
	iun <buffer> <S-Tab>
	im <buffer> <Tab> <C-n>
	im <buffer> <S-Tab> <C-p>
      en
      return g:complType
    el
      echohl "Unknown mode"
      return complType
    en
  endf
en

fu! s:SwitchToBuf(bufnr)
  wh bufnr('%') != a:bufnr
    norm! w
  endw
endf

fu! <SID>ComplBufInit()
  let returnWin=bufnr('%')
  exe 'vnew [Completion_'.returnWin.']'
  let b:choice=1
  let b:returnWin=returnWin
  setl bt=nofile bh=delete
  norm! 20|
  let complWin=bufnr('%')
  norm! p
  let b:complWin=complWin
  if exists('g:AutoCompl')
    let i=65 "azAZ
    wh i<91
      exe "im <silent> <buffer> <Char-".i."> <Char-".i."><C-l>"
      let i=i+1
    endw
    let i=97 "azAZ
    wh i<123
      exe "im <silent> <buffer> <Char-".i."> <Char-".i."><C-l>"
      let i=i+1
    endw
    im <silent> <buffer> _ _<C-l>
    im <silent> <buffer> <BS> <BS><C-l>
  en
  im <silent> <buffer> <C-l> <Esc>:echo ComplAll()<CR>a
  exe 'autocmd BufWinLeave '.expand('%:p').' bdel '.complWin
endf

fu! ComplAll()
  if !buflisted(b:complWin)
    retu ''
  en
  let lz=&lz
  set lz
  let i=1
  let l=line('.')
  let v=virtcol('.')
  norm! H
  let h=line('.')
  exe "norm!" l."G".v."|"
  let word0=expand('<cword>')
  let word1=''
  let compl=g:complType
  wh (i<3) || ((word0 != word{i-1}) && word{i-1} != word{i-2})
    silent exe "norm! oa\<C-u>".word0.compl."\<Esc>"
    let word{i}=getline('.')
    move .-2
    d
    let compl=compl.g:complType
    if i>32
      break
    en
    let i=i+1
  endw
  let c=1
  exe "norm!" h."Gzt".l."G".v."|"
  cal s:SwitchToBuf(b:complWin)
  let b:choice=1
  let b:wordToCompl=word0
  mat Search /a\@=b/
  1,$d
  wh c<i
    exe "norm! o".word{c}."\<Esc>"
    let c=c+1
  endw
  1d
  %s/\s\+$//e
  " Don't show, what we have typed
  silent exe 'g/\V\^'.escape(word0,'\').'\$/d'
  " Just at top
  exe "norm! 1GO".word0."\<Esc>"
  " Don't show empty lines
  g/^$/d
  "if strlen(getline(1)) && getline(1)==getline(2)
  "  2d
  "en
  let &lz=lz
  match IncSearch /\%1l.*.\ze$/
  cal s:SwitchToBuf(b:returnWin)
  retu ''
endf

fu! <SID>Compl(i)
  cal s:SwitchToBuf(b:complWin)
  let numChoices=line('$')
  let b:choice=(b:choice-1+a:i)%numChoices+1
  let line=getline(b:choice)
  if strlen(line)
    exe 'mat Search /\%'.b:choice.'l.*.$/'
    cal s:SwitchToBuf(b:returnWin)
    retu "\<Right>\<C-w>".line"\<Right>"
  el
    cal s:SwitchToBuf(b:returnWin)
    retu "\<Right>"
  en
endf

"fu! <SID>EscCompl()
"  if !exists('b:complWin') || expand('<cword>')
"    retu "\<Esc>"
"  en
"  cal s:SwitchToBuf(b:complWin)
"  1,d$
"  cal s:SwitchToBuf(b:returnWin)
"  retu "\<Right>\<C-w>".getbufvar(b:complWin,'choice')."\<Right>"
"endf

nmap <Leader>cw :cal <SID>ComplBufInit()<CR>
