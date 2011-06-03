" str2numchar.vim - String convert to Numeric Character Reference
" Author:       secondlife <hotchpotch@NOSPAM@gmail.com>
" Last Change:  2006 Sep 02
" Version: 0.1, for Vim 7.0
"
" DESCRIPTION:
"  This plugin is String convert to Numeric Character Reference
"  and String convert to Hex Literal
"
" for example:
" :echo Str2HexLiteral('abcd')
" \x61\x62\x63\x64
" :echo Str2NumChar('abcd')
" &#97;&#98;&#99;&#100;
"
" ==================== file str2numchar.vimrc ====================
" vmap <silent> sn :Str2NumChar<CR>
" vmap <silent> sh :Str2HexLiteral<CR>
" ==================== end: str2numchar.vimrc ====================

if v:version < 700 || (exists('g:str2numchar') && g:str2numchar || &cp)
  finish
endif
let g:str2numchar = 1

function! s:nr2Hex(nr)
  let n = a:nr
  let r = ""
  while n
    let r = '0123456789ABCDEF'[n % 16] . r
    let n = n / 16
  endwhile
  return r
endfunc

function! s:range2HexLiteral() range
"    call s:setVisualReasionString(String2HexLiteral(s:getVisualReasionString()))
    silent execute "normal! gv:s/\\%V./\\= String2HexLiteral(submatch(0)) /g\<CR>"
endfunction

function! s:range2NumChar() range
    "call s:setVisualReasionString(String2NumChar(s:getVisualReasionString()))
    silent execute "normal! gv:s/\\%V./\\= '&#' . char2nr(submatch(0)) . ';'/g\<CR>"
endfunction

" global
function! String2HexLiteral(str)
  let out = ''
  let ix = 0
  while ix < strlen(a:str)
    let out = out . '\x' . s:nr2Hex(char2nr(a:str[ix]))
    let ix = ix + 1
  endwhile
  return out
endfunction

" global
function! String2NumChar(str)
  return substitute(a:str, '.', '\= "&#" . char2nr(submatch(0)) . ";"', 'g')
endfunction

" for range command
command! -range Str2HexLiteral :<line1>,<line2>call s:range2HexLiteral()
command! -range Str2NumChar :<line1>,<line2>call s:range2NumChar()

"function! s:getVisualReasionString()
"  silent normal! gv"zy
"  return @z
"endfunction
"
"function! s:setVisualReasionString(str)
"  let @z = a:str
"  let paste_save = &paste
"  set paste
"  silent execute "normal! gv\"_d"
"  if col('.') == strlen(getline('.'))
"    silent execute "normal! a\<C-R>z\<ESC>"
"  else
"    silent execute "normal! i\<C-R>z\<ESC>"
"  end
"  let &paste = paste_save
"endfunction
"
"function! s:mbStrlen(str)
"  return strlen(substitute(a:str, '.', 'x', 'g'))
"end

