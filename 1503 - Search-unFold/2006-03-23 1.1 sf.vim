"=============================================================================
" What Is This: Search Fold
" Author: Paul Wei
" File: sf.vim
" Usage:
" :SFS  - search fold start   - initial search fold(same as SF but 
"                               deletes all previous folds first)
" :SF   - search fold         - add additional folds
" :SR   - search reset        - reset
" :SC   - search count        - count number of search terms found
" Description:
" This function folds all lines except the current search term. Additional
" search terms can be unfolded by repeated application. This is useful for
" parsing log files to display only lines of interest.
"
" ChangeLog:
"     1.0  initial version modified from 'f.vim by Antonio Colombo
"     1.1  use only 1 level folds and can unfold additional search
"          terms - Paul Wei

"*****************************************************************
"* find commands
"*****************************************************************
command -nargs=0 SFS call SFS()<CR>
command -nargs=0 SR  call SR()<CR>
command -nargs=0 SF  call SF() <CR>
command -nargs=0 SC  call SC()<CR>

" e - expand fold
" 9/6/2001 02:51 initial by Max Ischenko
" 6/2/2003 23:09 simplification to 1 line by ppa-nsu@mail.ru
nnoremap <silent> e :exe 'silent! normal! za'.(foldlevel('.')?'':'l')<cr> 

"set nocp vb noerrorbells ic incsearch
"set foldcolumn=2 
"set ruler hls showmatch nowrap shiftwidth=2
"map t :b#<C-M>

"*****************************************************************
"* FoldR : fold range
"*----------------------------------------------------------------
"*   from : fold start
"*   to   : fold end
"*****************************************************************
function! s:FoldR(from,to) range
if (a:from <= a:to)
  :exe a:from ","  a:to "fold"
endif
endf

"*****************************************************************
"* FoldR : fold file
"*----------------------------------------------------------------
"* from : fold start
"* to   : fold end
"*****************************************************************
" larg: 
"  search expression
" md: 
"  0 - start fresh, 
"  1 - append, top level
"  2 - append, one level down
function! SF2(larg, md) range
let arg=a:larg
silent exec "normal /" . arg . "/"
":nohl
" for use inside :help
set vb t_vb=
set foldenable
set foldmethod=manual
set foldminlines=0
set foldtext=
let num=1
let nFound=0
let fold_st=0
let fold_ed=0
let fromline=1
let toline=0
let lastline= line("$")
norm G$
if a:md==0
  norm zE
else
  norm zR
endif
let num=search(arg)
while num<=lastline && num>0
  if ( num>0 )
    let nFound=nFound+1
    let toline=num-1
    if (a:md >= 1 && foldlevel(".") > 0)
      norm mzzO
      norm [z
      let fold_st=line(".")
      norm ]z
      let fold_ed=line(".")
      "echo num fold_st fold_ed
      norm 'z
      if (a:md==1)
        norm zD
      endif
      call s:FoldR(fold_st, toline)
      call s:FoldR(num+1, fold_ed)
    else
      call s:FoldR(fromline,toline)
      let fromline=num+1
    endif
    norm $
  endif
  let num=search(arg,"W")
endwhile
if a:md==0 && fromline<=lastline && fromline!=1
  let toline=lastline
  call s:FoldR(fromline,toline)
endif
norm 1G
if (nFound>0)
  call histadd("/",arg)
  silent exec "normal" "/" . arg . "/"
  silent exec "match Search /\\c" . arg . "/"
  norm zM
  echo "Matches: " nFound
"  return ("/" . arg . "/")
else
  echo "Expression not found:" arg
"  return ("")
endif
set t_vb="<Esc>|f"
:endf

""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search Start, with fold
function! SFS() range
call SF2(@/, 0)
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search, with fold
function! SF() range
call SF2(@/, 1)
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search Count, no folding
function! SC() range
let arg=@/
norm mzG
let nFound=0
let num=0
let num=search(arg)
while num>0
  let nFound=nFound+1
  let num=search(arg,"W")
endwhile
echo "Matches: " nFound
norm 'z
endfunction

function! SR()
set foldtext=foldtext()
set foldmethod=marker
norm zM
endfunction
