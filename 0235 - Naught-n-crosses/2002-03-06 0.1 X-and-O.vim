"====================================================================={{{
" File:		X-and-O.vim
" Author:	Gergely Kontra
" Version:	0.1
" Created:	2002.02.28. 00:28:36
" Last Update:	
"------------------------------------------------------------------------
" Description:	Naught and Crosses against computer
"------------------------------------------------------------------------
" Installation:	Drop it to your plugin direcotry
" 		Maybe your 5-years-old nephew will loose one game...
" History:	0.1:	* Initial release
" TODO:		Intelligence ;)
"========================================================================
"
" Avoid reinclusion

if exists("g:loaded_X_and_O_vim") | finish | endif
let g:loaded_X_and_O_vim = 1
"---------------------------------------------------------------------}}}

fu s:Move(dx,dy) "{{{ for cursor movements
  let x=(virtcol('.')-1)/2+a:dx
  let y=line('.')+a:dy
  if x>=0 || x<15 || y>=0 || y<15
    exe 'norm! '.y.'G'.(2*x+1).'|'
  en
endf "}}}

fu s:PutSign() "{{{ put user's sign (with check)
  if getline('.')[col('.')-1]!='·'|echoerr 'Cheater!'|retu|en
  setl ma
  norm rX
  redr|let won=s:CheckWon()|if won!=''|cal confirm(won." Won")|retu|en
  cal s:Compute()
  redr|let won=s:CheckWon()|if won!=''|cal confirm(won." Won")|retu|en
endf "}}}

"{{{ check if anybody won
fu s:CheckPos(y,x) "{{{ helper for CheckWon
  let what=getline(1+a:y)[2*a:x] "str index start with 0!
  if what==''
    echoerr a:y.' row '.a:x.' col'
  en
  if what=='·'
    let s:_X_=0|let s:_O_=0
  el
    let s:_{what}_=s:_{what}_+1
    let other=(what=='X')?'O':'X'
    let s:_{other}_=0
  en
  retu (s:_X_>=5)? 'X':((s:_O_>=5)?'O':'')
endf "}}}
fu s:CheckWon() "{{{ Just check if someone won, and return sign
  let s:_X_=0|let s:_O_=0
  let y=0|wh y<15
    let x=0|wh x<15
      let won=s:CheckPos(y,x)
      if strlen(won)|retu won|en
      let x=x+1
    endw
    let y=y+1
  endw

  let s:_X_=0|let s:_O_=0
  let x=0|wh x<15
    let y=0|wh y<15
      let won=s:CheckPos(y,x)
      if strlen(won)|retu won|en
      let y=y+1
    endw
    let x=x+1
  endw

  let s:_X_=0|let s:_O_=0
  let x=0|wh x<15
    let y=0|wh y<15
      let won=s:CheckPos(y,x)
      if strlen(won)|retu won|en
      let x=(x+1)%15
      if x==0|let s:_X_=0|let x:_O_=0|en
      let y=y+1
    endw
    let x=x+1
  endw

  let s:_X_=0|let s:_O_=0
  let y=0|wh y<15
    let x=0|wh x<15
      let won=s:CheckPos(y,x)
      if strlen(won)|retu won|en
      let y=(y+14)%15
      if y==14
	let s:_X_=0|let s:_O_=0
      en
      let x=x+1
    endw
    let y=y+1
  endw
endf "}}}
"}}}

fu s:ScoreDir(y,x,dy,dx,sign) "{{{ score at y,x in dir dy, dx
  let other=(a:sign=='O'?'X':'O')
  if getline(1+a:y)[2*a:x]!='·'|retu -1|en " not free place -> return
  "echo getline(1+a:y)[2*a:x].' found'
  exe 'norm! '.(1+a:y).'G'.(2*a:x+1).'|r'.a:sign
  " make it our sign
  let closed1=1|let closed2=1
  let dx=a:dx|let dy=a:dy
  let x=a:x+dx|let y=a:y+dy
  wh x>=0 && x<15 && y>=0 && y<15
    let w=getline(1+y)[2*x]
    if w!=a:sign
      if w=='·'|let closed1=0|en
      let x=x-dx|let y=y-dy
      brea
    en
    let x=x+dx|let y=y+dy
  endw
  let dx=-dx|let dy=-dy " Go back
  let x=x+dx|let y=y+dy

  let sign=0|let hole=0|let place=0|let temphole=0

  wh x>=0 && x<15 && y>=0 && y<15
    let w=getline(1+y)[2*x]
    if w==a:sign
      let sign=sign+1
      let x=x+dx|let y=y+dy
    else
      if w=='·'|let closed2=0|en
      brea
    en
  endw
  "echo 'Sum at row '.a:y.' col '.a:x.' signs:' sign.' holes:'.hole
  exe 'norm! '.(1+a:y).'G'.(2*a:x+1).'|r·'

  if (closed1 && closed2 && (sign < 5))
    retu 0
  en
  if sign>=5
    retu 10000
  en
  let res=sign*sign*(sign-closed1-closed2)
  if res<0|res=0|en
  retu res
endf "}}}

fu s:Score(y,x) "{{{ score at pos y,x
  let score=0
  let dy=0|let dx=1
  wh dy<2
    let score=score+7*s:ScoreDir(a:y,a:x,dy,dx,'O')+6*s:ScoreDir(a:y,a:x,dy,dx,'X')
    let dx=dx+1
    if dx>1|let dx=-1|let dy=dy+1|en
  endw
  "echo 'Sum at row '.a:y.' col '.a:x.' :'.score
  retu score
endf "}}}

fu s:Compute() "{{{ main method for computer's calculatoins
  setl ma
  let maxscore=-100000
  let x=0|wh x<15
    let y=0|wh y<15
      let score=s:Score(y,x)
      if score>maxscore
	" echo 'New maxscore '.score.' at row '.y.' col '.x
	let maxx=x|let maxy=y
	let maxscore=score
      en
      let y=y+1
    endw
    let x=x+1
  endw
  exe 'norm! '.(1+maxy).'G'.(2*maxx+1).'|rO'
  setl noma
endf" }}}

fu s:Game() " maps, and the game area {{{
  vs Naught-n-Crosses
  se nobl ma buftype=nowrite bufhidden=delete
  res 15|vert res 29
  1,$d
  "norm! 15i· xY14p7G11|rO
  norm! 15i· xY14p8G15|
  setl noma " default is local, I hope...
  nmap <buffer> <Up>    k
  nmap <buffer> <Down>  j
  nmap <buffer> <Left>  h
  nmap <buffer> <Right> l
  nmap <buffer> h :cal <SID>Move(-1, 0)<CR>
  nmap <buffer> l :cal <SID>Move( 1, 0)<CR>
  nmap <buffer> j :cal <SID>Move( 0, 1)<CR>
  nmap <buffer> k :cal <SID>Move( 0,-1)<CR>
  nmap <buffer> <Space> :cal <SID>PutSign()<CR>
endf
map <Leader>xo :cal <SID>Game()<CR>
"}}}
" vim:fdm=marker:commentstring=\ "%s:
