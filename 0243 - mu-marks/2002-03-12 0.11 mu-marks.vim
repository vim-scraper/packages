" I need somebody, who just simplifies this script, and tells me why my
" versions are so complicated...
" Press zv on a fold to open it!
"
" MicroMarks {{{ (description)
" Author:	Gergely Kontra <kgergely@mcl.hu>
" Version:	0.11
" Description:
" Marks and Jumps and Bracketing (template expanding) functions inspired by
" the editor AlphaTK Later I found vim implementations by Stephen Riehm and
" Luc Hermitte, but I have problems obtaining it or making it working, and I
" have new ideas, so here is my version...
"
" THIS SCRIPT WANTS TO BE A STANDALONE ONE (doesn't depends on anything[really?])
"
" Minimal Setup:
" imap <C-J> ¡jump!
" Can be anything, but I think <C-J> is good :)
"
" Commands Defined:
" MarkMap {lhs} {rhs}
" MarkMapB {lhs} {rhs}
"
" Special signs in the maps:
" «» - the first one is the cursor position, the others are jump points
" ¶  - when applying a macro in visual mode the selected area will be expanded
"      here (added newline after expanding)
" ·  - Same as ¶, but trailing newline will be removed
" «¶»- mark OR jump point
" «¡»- mark in only insert mode mapping (??? will be removed)
" Note, that <Space> on lhs should be written as space, but on rhs you can
" write if ()<CR>.
"
" Function Defined:
" ClearMarks() Clears marks, and those lines, which contains just marks
"
" Examples:
" MarkMap if if «»<CR>¶end«¡»
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
" Note, that the «¡» mark didn't appeared in the visual mapping
"
" Another vim maps
if 0
  MarkMapB wh wh «»<CR>¶endw«¡»
  " for-like loop
  MarkMapB wh<Space>i let i=«»<CR>wh i<«»<CR>¶let i=i+1<CR>endw«¡»
  MarkMapB fu fu «»<CR>¶endf«¡»
  MarkMapB fu! fu! «»<CR>¶endf«¡»
en
"
" HISTORY:
" 0.1	* Initial release
" 0.11	* Many bugfixes
"}}}

fu! Jumpfunc() "{{{ function, and mappings to it
  if !search('«.\{-}»','W') "no more marks
    retu "\<CR>"
  el
    if getline('.')[col('.')]=="»"
      retu "\<Del>\<Del>"
    el
      retu "\<Esc>".(col('.')==1?'':'l')."vf»\<C-g>"
    en
  en
endf
ima ¡jump! <c-r>=Jumpfunc()<CR>
vma ¡jump! <c-r>=Jumpfunc()<CR>
"}}}

fu! ClearMarks() "{{{
  exe '%s/«.\{-}»//g'
endf "}}}

fu! MarkMap(b, left, ... ) "{{{ and the related MarkMap{B} commands
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
    \ '¡'.a:left.'! '.r."<Esc>`[:cal search('«[^»]*»','W')<CR>"

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
  exe 'vma ¡'.a:left."! c".r."<Esc>`<:cal search('«[^»]*»','W')<CR>"
endf

com! -nargs=+ MarkMap cal MarkMap(0,<f-args>)
com! -nargs=+ MarkMapB cal MarkMap(1,<f-args>)
"}}}

fu! s:DamnedEndOfLine() "{{{
  if getline('.')[col('.')]=='»'
    if col('.')+1==strlen(getline('.'))
      norm 2x
      star!
    el
      norm 2x
      star
    en
  el
    norm `[¡jump!
  en
endf "}}}

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
      exe 'norm '.(c-i).'x'
      exe 'norm a¡'.t."!"

      retu s:DamnedEndOfLine()
    en
    let i=i+1
  endw
  retu ""
endf "}}}

fu! s:VExpand() "{{{
  let n=input('Template name? ')
  if !strlen(maparg('¡'.n.'!','v'))
    echoerr 'Unknown template'
  en
  exe 'norm gv¡'.n.'!'
  cal s:DamnedEndOfLine()
endf "}}}

" Plugin-mappings {{{
if !hasmapto('<Plug>MarkExpandI')
  imap <unique> <C-Space> <Plug>MarkExpand
en
ino <script> <Plug>MarkExpand <Esc>:cal <SID>Expand()<CR>

if !hasmapto('<Plug>MarkExpandV')
  vmap <unique> <C-Space> <Plug>MarkExpandV
en
vno <script> <Plug>MarkExpandV :<C-U> cal <SID>VExpand()<CR>
" }}}

aug MicroMarks "{{{
  au!
  au BufNewFile,BufRead * syn match IncSearch /«.\{-}»/ containedin=ALL
  au BufWritePre * if search('«[^»]*»','w') && confirm('File contain MicroMarks («» chars). Delete them?',"&Yes\n&No")==1|cal ClearMarks()|en
aug END "}}}

" common brackets and date {{{
MarkMap ( <C-v>(«·»)«»
MarkMap [ <C-v>[«·»]«»
MarkMap { <C-v>{«·»}«»
MarkMap } {<CR>«¡»·<CR>}«»
MarkMap < <lt>«·»>«»
MarkMap " <C-v>"«·»<C-v>"«»
MarkMap ' <C-v>'«·»<C-v>'«»
MarkMap $ <C-v>$«·»<C-v>$«»
MarkMap date ·<C-R>=strftime('%Y. %m. %d.')<CR>«»
" }}}
"vim:sw=2:sts=2:fdm=marker:cms=\"\ %s:fcl=all
