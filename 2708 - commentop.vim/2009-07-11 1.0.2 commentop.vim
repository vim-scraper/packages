" =============================================================================
" commentop.vim - commands and operators to comment/uncomment lines
"=============================================================================
"
" Author:  Takahiro SUZUKI <takahiro.suzuki.ja@gmDELETEMEail.com>
" Version: 1.0.2 (Vim 7.1)
" Licence: MIT Licence
" URL:     http://www.vim.org/scripts/script.php?script_id=2708
"
"=============================================================================
" Document: {{{1
"
"-----------------------------------------------------------------------------
" Description:
"   This plugin provides a set of commands and operators  to comment or
"   uncomment lines. Linewise comment token (such as double quote in vim
"   script) is detected automatically by looking up filetype of the file.
"   Here are the preset filetypes:
"     vim, cpp, php, python, ruby, perl, sh, haskell, tex, matlab
"
"   You can also easily define your own comment token for filetype. Add below
"   in your .vimrc:
"     CommentopSetCommentType FILETYPE REMOVEPATTERN INSERTSTRING
"
"   plugin keymaps:
"     <Plug>CommentopNormaltoggle    " (n) toggle comment [count] lines
"     <Plug>CommentopNormalappend    " (n) comment out [count] lines
"     <Plug>CommentopNormalremove    " (n) uncomment [count] lines
"     
"     <Plug>CommentopVisualtoggle    " (v) toggle comment selected lines
"     <Plug>CommentopVisualappend    " (v) comment out selected lines
"     <Plug>CommentopVisualremove    " (v) uncomment selected lines
"     
"     <Plug>CommentopOperatortoggle  " (n op) toggle comment {motion}
"     <Plug>CommentopOperatorappend  " (n op) comment out {motion}
"     <Plug>CommentopOperatorremove  " (n op) uncomment {motion}
"
"   default mapping:
"     co       <Plug>CommentopNormaltoggle
"     cO       <Plug>CommentopNormalappend
"     c<C-O>   <Plug>CommentopNormalremove
"
"     co       <Plug>CommentopVisualtoggle
"     cO       <Plug>CommentopVisualappend
"     c<C-O>   <Plug>CommentopVisualremove
"
"     go       <Plug>CommentopOperatortoggle
"     gO       <Plug>CommentopOperatorappend
"     g<C-O>   <Plug>CommentopOperatorremove
"
"-----------------------------------------------------------------------------
" Installation:
"   Place this file in /usr/share/vim/vim*/plugin or ~/.vim/plugin/
"
"-----------------------------------------------------------------------------
" Examples:
"   in normal mode:
"      co          " toggle comment for this line
"      3cO         " comment out 3 lines
"
"   in normal mode (operator):
"      goip        " toggle comment for this paragraph
"      gOi{        " comment out this {} block
"
"   in visual mode:
"      c<C-O>      " remove comments in visual selection
"
"-----------------------------------------------------------------------------
" ChangeLog:
"   1.0.2:
"     - bug fix (wrong comment string with ft=vim)
"   1.0.1:
"     - bug fix (gO was mapped to comment out operator)
"   1.0:
"     - Initial release
"
" }}}1
"=============================================================================

function! s:CountHeadSpace()
  let p = getpos('.')
  if p[2]==1 | return 0 | endif
  let line = getline('.')[0:p[2]-2]
  let len = 0
  while len < strlen(line)
    if line[len]=~"[ \<TAB>]" | let len += 1 | else | break | endif
  endwhile
  return len
endfunction

" visual mode
function! s:VisualLinewiseComment(mode)
  let b = getpos("'<")
  let e = getpos("'>")
  call s:Comment(a:mode, e[1]-b[1]+1)
endfunction

" normal mode with seved count
function! s:LinewiseComment(mode)
  call s:Comment(a:mode, s:count1)
endfunction

"mode 0:off 1:on 2:toggle
function! s:Comment(mode, count)
  " determine the comment type
  if has_key(s:comment_types, &ft)
    let commentmatch = s:comment_types[&ft]['match']
    let commentinsertstr = s:comment_types[&ft]['insert']
  else
    return
  endif

  let c = a:count
  " count head spaces
  normal! ^
  let p = getpos('.')
  let mh = -1
  while c>0
    let hs = s:CountHeadSpace()
    if hs<mh || mh==-1 | let mh = hs | endif
    normal! j
    let c -= 1
  endwhile
  " insert/remove comments
  call setpos('.', p)
  let c = a:count
  while c>0
    let iscomment = getline('.')[mh :] =~ commentmatch
    let prevstr = (mh>0) ? getline('.')[0:mh-1] : ''
    if a:mode==0 || (a:mode==2 && iscomment)
      " remove
      call setline('.', prevstr . substitute(getline('.')[mh :], commentmatch, '', ''))
    else
      " insert
     call setline('.', prevstr . commentinsertstr . getline('.')[mh :])
    endif
    normal! j
    let c -= 1
  endwhile
  call setpos('.', p)
endfunction

" comment in/out operator
function! s:LinewiseCommentInOperator(type)
  call s:LinewiseCommentOperator(0, a:type)
endfunction
function! s:LinewiseCommentOutOperator(type)
  call s:LinewiseCommentOperator(1, a:type)
endfunction
function! s:LinewiseCommentToggleOperator(type)
  call s:LinewiseCommentOperator(2, a:type)
endfunction

function! s:LinewiseCommentOperator(mode, type)
  exe 'normal! `['
  let b = getpos('.')
  exe 'normal! `]'
  let e = getpos('.')
  exe 'normal! `['
  call s:Comment(a:mode, e[1]-b[1]+1)
endfunction

function! s:SaveCnt()
  let s:count1 = v:count1
endfunction

" function and command to set the comment type from file type
let s:comment_types = {}
function! s:SetCommentType(...)
  " 1:filetype, 2:match, 3:insert
  let s:comment_types[a:1] = {'match': a:2, 'insert': a:3}
endfunction

command! -nargs=* CommentopSetCommentType :call s:SetCommentType(<f-args>)

" preset comment types
CommentopSetCommentType vim       ^\"[\ <TAB>]\\{,1}   "\ 
CommentopSetCommentType sh        ^#[\ <TAB>]\\{,1}    #\ 
CommentopSetCommentType perl      ^#[\ <TAB>]\\{,1}    #\ 
CommentopSetCommentType python    ^#[\ <TAB>]\\{,1}    #\ 
CommentopSetCommentType ruby      ^#[\ <TAB>]\\{,1}    #\ 
CommentopSetCommentType haskell   ^--[\ <TAB>]\\{,1}   --\ 
CommentopSetCommentType cpp       ^//[\ <TAB>]\\{,1}   //\ 
CommentopSetCommentType php       ^//[\ <TAB>]\\{,1}   //\ 
CommentopSetCommentType tex       ^%[\ <TAB>]\\{,1}    %\ 
CommentopSetCommentType matlab    ^%[\ <TAB>]\\{,1}    %\ 

" default keymaps
nmap co      <Plug>CommentopNormaltoggle
nmap cO      <Plug>CommentopNormalappend
nmap c<C-O>  <Plug>CommentopNormalremove

vmap co      <Plug>CommentopVisualtoggle
vmap cO      <Plug>CommentopVisualappend
vmap c<C-O>  <Plug>CommentopVisualremove

nmap go      <Plug>CommentopOperatortoggle
nmap gO      <Plug>CommentopOperatorappend
nmap g<C-O>  <Plug>CommentopOperatorremove

" plugin keymaps
nnoremap <silent> <Plug>CommentopNormaltoggle   :<C-U>call <SID>SaveCnt()<CR>:call <SID>LinewiseComment(2)<CR>
nnoremap <silent> <Plug>CommentopNormalappend   :<C-U>call <SID>SaveCnt()<CR>:call <SID>LinewiseComment(1)<CR>
nnoremap <silent> <Plug>CommentopNormalremove   :<C-U>call <SID>SaveCnt()<CR>:call <SID>LinewiseComment(0)<CR>

vnoremap <silent> <Plug>CommentopVisualtoggle   :<C-U>call <SID>SaveCnt()<CR>:call <SID>VisualLinewiseComment(2)<CR>
vnoremap <silent> <Plug>CommentopVisualappend   :<C-U>call <SID>SaveCnt()<CR>:call <SID>VisualLinewiseComment(1)<CR>
vnoremap <silent> <Plug>CommentopVisualremove   :<C-U>call <SID>SaveCnt()<CR>:call <SID>VisualLinewiseComment(0)<CR>

nnoremap <silent> <Plug>CommentopOperatortoggle :<C-U>call <SID>SaveCnt()<CR><ESC>:set opfunc=<SID>LinewiseCommentToggleOperator<CR>g@
nnoremap <silent> <Plug>CommentopOperatorappend :<C-U>call <SID>SaveCnt()<CR><ESC>:set opfunc=<SID>LinewiseCommentOutOperator<CR>g@
nnoremap <silent> <Plug>CommentopOperatorremove :<C-U>call <SID>SaveCnt()<CR><ESC>:set opfunc=<SID>LinewiseCommentInOperator<CR>g@

