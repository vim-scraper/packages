" MicroMarks {{{
" Marks and Jumps function inspired by the editor AlphaTK
" Later I found a vim implementation by Stephen Riehm and Luc Herminette but I
" was unable to download / set it up correctly, and there were dependency
" problems :(
" (The function required an other script, that required another...)
"
" Minimal setup:
" --------------
" imap <C-J> ¡jump!
"
" Commands defined:
" -----------------
" MarkMap {lhs} {rhs}
" MarkMapB {lhs} {rhs}
"
" For defining insert mode abbreviations and maps.
" Special signs in the maps:
" «» - the first one is the cursor position, the others are jump points
" ¶  - when applying a macro in visual mode the selected area will be expanded
"      here (added newline after expanding)
" ·  - Same as ¶, but trailing newline will be removed
" «¶»- mark OR jump point
" «¡»- mark in only insert mode mapping (??? will be removed)
"
" Functions defined:
" ------------------
" ClearMarks() Clears marks, and those lines, which contains just marks
"
" Examples:
" MarkMap if if «»<CR>¶end«»
" The cursor is placed at the first «» sign is in the macro.
" So, after typing 'if' and pressing <C-Space> it will expand to:
" if <Cursor here>
" en«»
" It can be used in visual mode.
" Select a line, eg:
" fini
" Press <C-Space>
" Type if<Enter>
" You will get
" if <Cursor here>
"   fini
" end
" 
" I suggest putting im ¡jump! <C-j> in your .vimrc, or in this file.
" Again press ga over the ¡ sign to figure out what to type to get it.
" (It is <M-!> on my Windows98)
" Another vim maps
if 0
  MarkMapB ¡wh! wh «»<CR>¶endw«¡»
  MarkMapB ¡wh<Space>i! let i=«»<CR>wh i<«»<CR>¶let i=i+1<CR>endw«¡»
  MarkMapB ¡fu! fu «»<CR>¶endf«¡»
  MarkMapB ¡fu!! fu! «»<CR>¶endf«¡»
en
"
" BUGS:
" If the cursor position is at the very first column, the cursor will position
" AFTER the first character
"}}}

fu! Jumpfunc() "{{{
  if !search('«.\{-}»','W') "no more marks
    retu "\<CR>"
  el
    if getline('.')[col('.')]=="»"
      retu "\<Del>\<Del>"
    el
      retu "\<Esc>lvf»\<C-g>"
    en
  en
endf "}}}

fu! ClearMarks() "{{{
  exe 'g/^\s*\(«.\{-}'.».'\)\+$/d'
  exe '%s/«.\{-}»//g'
endf "}}}

imap ¡jump! <c-r>=Jumpfunc()<CR>
vmap ¡jump! <c-r>=Jumpfunc()<CR>

fu! MarkMap(b, left, ... ) "{{{
  if a:0==0|echoerr 'Right side need'|en
  let right=a:1
  let i=2
  wh i<=a:0
    let right=right.'<Space>'.a:{i}
    let i=i+1
  endw
  " INSERT MODE MAP: no buffer expand
  let r=substitute(substitute(right,'[¶·]','','g'),'«¡','«','g')
  exe 'ima '. (a:b?'<buffer>':'') .
    \ a:left.' '.r."<Esc>`[:cal search('«»','W')<CR>2s"

  " VISUAL MAP: ignore insert-only mappings
  let r=substitute(right,'«¡[^»]*»','','g')
  " peel · and ¶ signs inside marks
  let r= substitute(r,'«\([¶·]\)[^»]\{-}»','\1','g')
  "visuals must expand · and ¶ (just one!)
  " let r=substitute(r,'¶','<C-R>=substitute(@","[^\\n]$","\\n","")<CR>','')
  " ^ doesn't work, but why?
  " in ¶ mode add extra newline after inserted text, if needed
  let r=substitute(r,'¶','<C-R>=char2nr(@"[strlen(@")-1])==10?@":@"."\\n"<CR>','')
  " in · mode remove extra newline after inserted text
  let r=substitute(r,'·','<C-R>=char2nr(@"[strlen(@")-1])==10?strpart(@",0,strlen(@")-1):@"<CR>','')
  exe 'vma '.a:left." c".r."<Esc>`<:cal search('«»','W')<CR>2s"
endf "}}}

com! -nargs=+ MarkMap cal MarkMap(0,<f-args>)
com! -nargs=+ MarkMapB cal MarkMap(1,<f-args>)

fu! s:Expand() "{{{
  let c=col('.')
  let l=strpart(getline('.'),0,c)
  let i=0
  wh i<c
    let t=strpart(l,i,c)
    if strlen(maparg('¡'.t.'!','i'))
      if c-i-1
	exe 'norm '.(c-i-1).'h'
      en
      exe 'norm '.(c-i).'xa¡'.t."!\<Esc>"
      retu
    en
    let i=i+1
  endw
endf "}}}

fu! s:VExpand() "{{{
  let n=input('Template name? ')
  sil exe 'norm gv¡'.n.'!'
  if col('.')==strlen(getline('.'))
    star!
  el
    star
  en
endf "}}}
if !hasmapto('<Plug>MarkExpandI')
  imap <unique> <C-Space> <Plug>MarkExpand
en
ino <unique> <script> <Plug>MarkExpand <Esc>:sil cal <SID>Expand()<CR>a
if !hasmapto('<Plug>MarkExpandV')
  vmap <unique> <C-Space> <Plug>MarkExpandV
en
vno <unique> <script> <Plug>MarkExpandV :<C-U> cal <SID>VExpand()<CR>

aug MicroMarks
  au!
  au BufNewFile,BufRead * syn match IncSearch /«.\{-}»/ containedin=ALL
aug END

MarkMap ¡(! <C-v>(«·»)«»
MarkMap ¡[! <C-v>[«·»]«»
MarkMap ¡{! <C-v>{«·»}«»
MarkMap ¡}! {<CR>«¶»<CR>}«»
MarkMap ¡<! <lt>«·»>«»
MarkMap ¡"! <C-v>"«·»<C-v>"«»
MarkMap ¡'! <C-v>'«·»<C-v>'«»
MarkMap ¡$! <C-v>$«·»<C-v>$«»
MarkMap ¡date! ·<C-R>=strftime('%Y. %m. %d.')<CR>«»

"vim:sw=2:sts=2:fdm=marker:cms=\"\ %s:
