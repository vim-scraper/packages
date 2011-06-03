" Vim syntax file
" Language:	brew
" Maintainer:	Will Gray <graywh@gmail.com>
"
" This is heavily based on Tim Pope's eruby.vim.

if exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'brew'
endif

if !exists("g:brew_default_subtype")
  let g:brew_default_subtype = "html"
endif

if !exists("b:brew_subtype") && main_syntax == 'brew'
  let s:lines = getline(1)."\n".getline(2)."\n".getline(3)."\n".getline(4)."\n".getline(5)."\n".getline("$")
  let b:brew_subtype = matchstr(s:lines,'brew_subtype=\zs\w\+')
  if b:brew_subtype == ''
    let b:brew_subtype = matchstr(&filetype,'^brew\.\zs\w\+')
  endif
  if b:brew_subtype == ''
    let b:brew_subtype = matchstr(substitute(expand("%:t"), '\c\%\(\.brew\)\+$','',''),'\.\zs\w\+$')
  endif
  if b:brew_subtype == 'latex'
    let b:brew_subtype = 'tex'
  elseif b:brew_subtype == 'txt'
    let b:brew_subtype = 'text'
  elseif b:brew_subtype == ''
    let b:brew_subtype = g:brew_default_subtype
  endif
endif

if !exists("b:brew_nest_level")
  let b:brew_nest_level = strlen(substitute(substitute(substitute(expand("%:t"),'@','','g'),'\c\.\%(brew\)\>','@','g'),'[^@]','','g'))
endif
if !b:brew_nest_level
  let b:brew_nest_level = 1
endif

if exists("b:brew_subtype") && b:brew_subtype != ''
  exe "runtime! syntax/".b:brew_subtype.".vim"
  unlet! b:current_syntax
endif
syn include @rTop syntax/r.vim

syn cluster brewRegions contains=brewOneLiner,brewBlock,brewExpression,brewComment

exe 'syn region brewOneLiner   matchgroup=brewDelimiter start="^%\{1,'.b:brew_nest_level.'\}%\@!"    end="$" contains=@rTop containedin=ALLBUT,@brwRegions keepend oneline'
exe 'syn region brewBlock      matchgroup=brewDelimiter start="<%\{1,'.b:brew_nest_level.'\}%\@!-\=" end="-\=%\@<!%\{1,'.b:brew_nest_level.'\}>" contains=@rTop containedin=ALLBUT,@brwRegions keepend'
exe 'syn region brewExpression matchgroup=brewDelimiter start="<%\{1,'.b:brew_nest_level.'\}="       end="-\=%\@<!%\{1,'.b:brew_nest_level.'\}>" contains=@rTop containedin=ALLBUT,@brwRegions keepend'
exe 'syn region brewComment    matchgroup=brewDelimiter start="<%\{1,'.b:brew_nest_level.'\}#"       end="-\=%\@<!%\{1,'.b:brew_nest_level.'\}>" contains=rTodo,@Spell containedin=ALLBUT,@brwRegions keepend'

highlight default link brewDelimiter    PreProc
highlight default link brewComment      Comment

let b:current_syntax = 'brew'

if main_syntax == 'brew'
  unlet main_syntax
endif
