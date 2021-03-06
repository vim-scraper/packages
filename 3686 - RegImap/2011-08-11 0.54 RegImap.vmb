" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/RegImap.vim	[[[1
309
" File:          RegImap.vim
" Author:        Artem Shcherbina
" Last Updated:  Aug 6, 2011
" Version:       0.5
" Description:   RegImap.vim  Plugin for using regular expression substitutes in insert mode
"
"                For more help see RegImap.txt; you can do this by using:
"                :helptags ~/.vim/doc
"                :h RegImap.txt

if exists("g:RegImap_inited") || &cp || version < 700
  finish
endif

let RegImap_inited = 1

if v:version > 703 || v:version == 703 && has("patch196")
  autocmd CursorMovedI * call TriggerRegImap()
  autocmd InsertCharPre * let s:useTrigger = 1
else
  autocmd CursorMovedI * let s:useTrigger = 1 | call TriggerRegImap()
endif

autocmd BufEnter * call ReadRegImaps()
autocmd FileType * call ReadRegImaps()

highlight PlaceHolder ctermbg=0 ctermfg=4 guifg=#4CFF4C guibg=#3A3A3A
call matchadd('PlaceHolder', '<' . '+.\{-}+>')

if !exists("g:RegImap_baseDir")
  let g:RegImap_baseDir = &runtimepath
endif

if !exists("g:RegImap_useNextPH")
  let g:RegImap_useNextPH = '<Tab>'
endif

if !exists("g:RegImap_selectNextPH")
  let g:RegImap_selectNextPH = '<S-Tab>'
endif

if !exists("g:RegImap_exitSelectMode")
  let g:RegImap_exitSelectMode = ';'
endif

if !exists("g:RegImap_clearSelectedText")
  let g:RegImap_clearSelectedText = '<C-j>'
endif

let g:whiteStart='^\(\s*\)\zs'
let g:cursor = '\' . '%#'
let g:isPH = '<' . '+.*+>'
let g:isCursorPH = '<' . '+@.*@+>'

let g:notDQ = '\%(\\\\\|\\[^\\]\|[^\"]\)'
let g:DQstr = '"' . notDQ . '*"'
let g:notSQ = "[^']"
let g:SQstr = "'" . notSQ . "*'"
let g:notSQDQ = "[^\"']"
let g:closedQuotedText = '\%(' . notSQDQ . '*\%(' . SQstr . '\|' . DQstr . '\)\)*' . notSQDQ . '*'


if g:RegImap_useNextPH != ''
  exec 'inoremap ' . g:RegImap_useNextPH . ' <C-r>=NextPH()'
  exec 'nnoremap ' . g:RegImap_useNextPH . ' a<C-r>=NextPH("", escape)'
  exec 'snoremap ' . g:RegImap_useNextPH . ' <Esc>a<C-r>=CheckSelected()'
endif

if g:RegImap_selectNextPH != ''
  exec 'nnoremap ' . g:RegImap_selectNextPH . ' :call SelectPH()'
  exec 'snoremap ' . g:RegImap_selectNextPH . ' <Esc>:call SelectPH()'
  exec 'inoremap ' . g:RegImap_selectNextPH . ' <Esc>:call SelectPH()'
endif

if g:RegImap_exitSelectMode != ''
  exec 'snoremap ' . g:RegImap_exitSelectMode . ' <Esc>'
endif

if g:RegImap_clearSelectedText != ''
  exec 'snoremap ' . g:RegImap_clearSelectedText . ' i<BS><C-r>=ClearSelection()'
endif


let escape = "\<Esc>" 
let s:useTrigger = 0
let s:defaults = {
      \'filetype' : 'common',
      \'condition' : '',
      \'feedkeys' : '',
      \}


let s:parameters = s:defaults
let s:sourcedFiles = {}
let s:regImaps = {}


fun! PH(...)
  return '<' . '+' . join(a:000) . '+>'
endfun

fun! CursorPH(...)
  return '<' . '+@' . join(a:000) . '@+>'
endfun

"fun! NotInString(...)
"  return '^' . g:closedQuotedText . '\zs' . join(a:000) . '\ze' . g:closedQuotedText . '$'
"endfun

function! SetParameters(...)
  if a:0 == 0
    let s:parameters = s:defaults
  else
    call extend(s:parameters, a:1)
  endif
endfunction

fun! RegImap(pattern, substitute, ...)
  if a:pattern == ''
    echohl WarningMsg
    echomsg 'RegImap: Empty pattern for ' . a:ft
    echohl None
  endif
  if a:pattern !~ '\\%#'
    let pattern = '\%(' . a:pattern . '\)\%#'
  else
    let pattern = a:pattern
  endif
  " Replace  by special string
  let substitute = substitute(a:substitute, '', '?c' . 'r?', 'g')
  if (substitute[0:1] == '\=') && substitute !~ g:isPH
    let substitute .= '."' . PH() . '"'
  endif
  
  if (substitute[0:1] != '\=') && substitute !~ g:isPH
    let substitute .= PH()
  endif
  
  if substitute !~ g:isCursorPH
    let substitute = substitute(substitute, '<' . '+\zs.\{-}\ze+>', '@&@', '')
  endif
  
  if a:0 > 0
    let parameters = GetParameters(a:1)
  else
    let parameters = s:parameters
  endif
  
  for ft in split(parameters.filetype, '\.')
    if !has_key(s:regImaps, ft)
      let s:regImaps[ft] = []
    endif
    let defined=0
    for RegImap in s:regImaps[ft]
      if RegImap.pattern == pattern
        echohl WarningMsg
        echomsg 'RegImap: Redefining ' . a:pattern . ' for ' . ft
        echohl None
        let defined=1
      endif
    endfor
    if !defined
      " singleline if not \n
      call add(s:regImaps[ft], extend({'pattern' : pattern, 'substitute' : substitute, 'singleLine' : (pattern !~ '\\n')}, parameters))
    endif
  endfor
endfun

fun! ReloadRegImaps()
  let s:regImaps = {}
  for RegImapsFile in keys(s:sourcedFiles)
    if (filereadable(RegImapsFile))
      exec 'source ' . RegImapsFile
    endif
  endfor
endf

function! ClearSelection()
  return (search('^\s*\n', 'nbc', line('.')) ? "\<Esc>dd" : '') . "a\<C-R>=NextPH()\<CR>"
endfunction

function! CheckSelected()
  if search('<' . g:cursor . '+.*+>', 'bce')
    let save_cursor = getpos(".")
    s/<[+]\(.\{-}\)+\%#>/\1/
    call cursor(line('.'), save_cursor[2]-3)
    return TriggerRegImap(1)
  endif
  let s:useTrigger = 1
  return "\<Esc>a"
endfunction


function! GetParameters(input)
  let parameters = extend(copy(s:parameters), a:input)
  if parameters.condition !~ g:cursor
    let parameters.condition = '\%(' . parameters.condition . '\)\%#'
  endif
  if parameters.filetype == ''
    let parameters.filetype = 'common'
  endif
  return parameters
endfunction

fun! ReadRegImaps()
  if &ft != ''
    call ReadFile(&ft)
  endif
  call ReadFile('common')
endf

fun! ReadFile(filetype)
  for RegImapsFile in split(globpath(g:RegImap_baseDir, 'RegImap/' . a:filetype . '.vim')) + split(globpath(g:RegImap_baseDir, 'MyRegImap/' . a:filetype . '.vim'))
    if !has_key(s:sourcedFiles, RegImapsFile) && filereadable(RegImapsFile) " If file was not sourced yet
      let s:sourcedFiles[RegImapsFile] = 1
      exec 'source ' . RegImapsFile
    endif
  endfor
endf


function! SelectPH()
  if search(g:isPH, 'w') " PlaceHolder found
    let save_cursor = getpos(".")
    let width = searchpos('+>', 'en')[1] - save_cursor[2]
    call feedkeys("\<Esc>v" . (width) . "lo\<C-g>", 'n')
  endif
endfunction


function! NextPH(...)
  " Use tab for autocomplete?
  if pumvisible()
    return "\<C-y>"
  endif
  
  if search(g:isCursorPH, 'cw') || search(g:isPH, 'cw') " PlaceHolder found
    let save_cursor = getpos(".")
    
    if search('\%#' . g:isCursorPH, 'cw')
      let width = searchpos('+>', 'en')[1] - save_cursor[2] - 2
      s/\%#<[+]@\(.\{-}\)@+>/\1/
    else
      let width = searchpos('+>', 'en')[1] - save_cursor[2]
      s/\%#<[+]\(.\{-}\)+>/\1/
    endif
    
    call setpos('.', save_cursor)
    
    if width > 4
      " PlaceHolder with text
      call feedkeys("\<Esc>lv" . (width-4) . "l\<C-g>", 'n')
    elseif width == 4
      " PlaceHolder with one char
      call feedkeys("\<Esc>lv\<C-g>", 'n')
    endif
    if a:0 > 0
      call feedkeys(a:1)
    endif
  else
    if a:0 > 1
      call feedkeys(a:2)
    endif
  endif
  return ''
endfunction

fun! TriggerRegImap(...)
  if !s:useTrigger
    return ''
  endif
  let s:useTrigger = 0
  let lineNum = line('.')
  for ft in split(&ft, '\.') + ['common']
    if exists('s:regImaps["'.ft.'"]')
      for mapping in s:regImaps[ft]
        if mapping.singleLine
          if (mapping.condition == '' || search(mapping.condition, 'cnb', lineNum)) && search(mapping.pattern, 'cnb', lineNum)
"            call feedkeys("<C-g>u")
            exec 'normal! :s/' . mapping.pattern . '/' . mapping.substitute . "\<CR>"
            s/?[c]r?//ge " Put  defined in RegImap back
            return NextPH(mapping.feedkeys)
          endif
        else
          if (mapping.condition == '' || search(mapping.condition, 'cnb')) && search(mapping.pattern, 'cnb')
            exec 'normal! :%s/' . mapping.pattern . '/' . mapping.substitute . "\<CR>"
            %s/?[c]r?//ge " Put  defined in RegImap back
            return NextPH(mapping.feedkeys)
          endif
        endif
      endfor
    endif
  endfor
  return ''
endfun

" For debug
fun! PrintRegImaps()
  for ft in split(&ft, '\.') + ['common']
    if exists('s:regImaps["'.ft.'"]')
      exec 'normal! o' . ft
      for mapping in s:regImaps[ft]
        exec 'normal! o' . mapping.pattern . ' -> ' 
              \. mapping.substitute . ' / ' 
              \. mapping.condition . ' / ' . mapping.feedkeys
      endfor
    endif
  endfor
endfun
doc/RegImap.txt	[[[1
375
*RegImap.txt*  Plugin for using regular expression substitutes in insert mode

RegImap                                                     *RegImap-contents*
Last Change: Aug 6, 2011

|RegImap-usage|       Usage
|RegImap-commands|    Commands
|RegImap-variables|   Variables
|RegImap-options|     Options
|RegImap-patterns|    Patterns
|RegImap-features|    Features
|RegImap-contact|     Contact
|RegImap-changelog|   Changelog

For Vim version 7.0 or later.
This plugin only works if 'compatible' is not set.
{Vi does not have any of these features.}


==============================================================================
USAGE                                                         *RegImap-usage*

Note If you want to improve performance of this plugin then update to vim
version 7.3 with patch 196, for example from here
http://www.vim.org/mercurial.php

RegImap plugin allows you to type text more efficiently by defining your
mappings, that will be used, while you typing text. For example, in vim you
often type nnoremap. To speed up this job, you can define the following
mapping:
>
 call RegImap('^nno', 'nnoremap ', {'filetype' : 'vim'})
<

First argument is a {pattern}, that defines when the substitute will done.
Second argument is a {substitute} pattern, that will be inserted instead of
the {pattern}. Third argument defines filetype, when this substitute will
work. Now you need to type only three characters at the start of the line to
get full word and a space. Note that this substitute will not performed in any
other place of the text.

But, for example, you have to type also inoremap, vnoremap and others. Of
course, you can call separate |RegImap| function for each word, but there is
more efficient way:
>
 call RegImap('^\([nisxvolc]\)no', '\1noremap ', {'filetype' : 'vim'})
<

Now, regular expression is used for the first argument. Only one character out
of "nisxvolc" will match, and it is passed to the second argument.

More formally, |RegImap| works like executing :s/{pattern}/{substitute}/ every
time you enter a character or move the cursor in insert mode.

In your {substitute} pattern you can use |PlaceHolders|. They are defined be |PH|. 
You can jump to the next placeholder by pressing <tab> button, or cicle
through available |PlaceHolders| by <S-Tab>. Look at |RegImap-options| fom
more details.

New patterns are defined by |RegImap| function. You can use it directly at you
vim file at ftplugin directory. More convenient way is to keep them all at the
RegImap folder with names ft.vim for filetype ft. They are automatically
sourced, when you enter a buffer.

If, when you typed a key in insert mode multiple {pattern}'s match, the one
that was defined first will be used.

Look at |RegImap-patterns| for more examples.


==============================================================================
COMMANDS                                                   *RegImap-commands*

                                                                    *RegImap*
RegImap({pattern}, {substitute}, [, {parameters}])

RegImap adds a new regular mapping. Essentially RegImap defines a |CursorMovedI|
autocommand that performs substitute 's/{pattern}/{substitute}/' in the insert
mode at the cursor position. {pattern} and {substitute} are regular
expressions. You can use there |RegImap-variables| and |PlaceHolder|. Simple
example:
>
 call RegImap('^\s\+r ', 'return ')
<

Every time you typing 'r' and <space> at the start of the line you will
automatically get 'return '.

You can also use vim script in {substitute} field, see |sub-replace-expression|.
For example:
>
 call RegImap('\<cdate', '\=strftime("%Y-%m-%d")')
<

{parameters} are additional options in the form
>
 {'option1' : value1, 'option2' : value2, ... }
<

Possible options are:
  - {filetype}, default 'common'. Mapping will work only in specified
    filetype. 'common' means that this pattern will work in all files.
  - {condition}, default ''. Additional pattern that is checked before 
    {pattern}. If it is not satisfied, substitute is not performed. Added for
    efficiency, when using complex or multiline {pattern}.
  - {feedkeys}, default ''. Keys, that are send to vim after successful
    substitute. Default values of these parameters can be changed by
    |RegImap-SetParameters|.

Note If {pattern} does not contain atom '\%#' (matches at cursor position),
     then it is added to the end. If {substitute} does not contain
     |PlaceHolder| calls, one |PlaceHolder| is added to the end.

After substitute 's/{pattern}/{substitute}/' is done, cursor is moved to the
next |PlaceHolder|.

After that |feedkeys| is called with {feedkeys} argument.


SetParameters([{parameters}])                         *RegImap-SetParameters*

With this function you can change default values of {parameters}, specified in
|RegImap|. If you call it without arguments, {parameters} are reset to
defaults.


PH([{string}])                                     *PlaceHolder*   *RegImap-PH*

Return a Placeholder with possible default value. Basically for {substitute}
fields.


CursorPH([{string}])                                       *RegImap-CursorPH*

Same as |PH|, but it is selected before other Placeholders.



ReadRegImaps()                                         *RegImap-ReadRegImaps*
Sources {filetype}.vim files from directories, specified by |RegImap_basedir|.
This files usually contain vim scripts, |RegImap| calls. This function is
called automatically when you enter a buffer.


ReloadRegImaps()                                     *RegImap-ReloadRegImaps*
Cleares all defined mappings and sourced again all files, used before by
|RegImap-ReadRegImaps|.



==============================================================================
VARIABLES                                                 *RegImap-variables*

Useful variables to use in {pattern} fields.

>
 -----------------------------------------------------------------------------
 | variable         | value       | matches                                  |
 -----------------------------------------------------------------------------
 | whiteStart       | ^\(\s*\)\zs | spaces of tabs at the start of the line, |
 |                  |             | they are not included into the match     |
 |                  |             | (see h \zs). You can access them via \1  |
 | cursor           | \%#         | cursor position                          |
 | notDQ            | *           | not double quote character               |
 | DQstr            | *           | double quoted string                     |
 | notSQ            | "[^']"      | not a single quote                       |
 | SQstr            | *           | Single quoted string                     |
 | notSQDQ          | "[^\"']"    | not any quote                            |
 | closedQuotedText | *           | arbitrary number of single or double     |
 |                  |             | quoted strings or usual text             |
 -----------------------------------------------------------------------------

 * = look at RegMap.vim
<

==============================================================================
OPTIONS                                                     *RegImap-options*

You can change values of this variables in your .vimrc file.

                                                            *RegImap_basedir*  >
  let g:RegImap_baseDir = &runtimepath
<

Mappings are by default looked for any RegImap directory in your
'runtimepath'. Typically, it is located at '~/.vim/RegImap/' on *nix or
'$HOME\vimfiles\RegImap\' on Windows. To change that location or add another
one, change the g:baseDir variable in your |.vimrc| to your preferred
directory. This will be used by the |globpath()| function, and so accepts the
same syntax as it (e.g., comma-separated paths).

                                                          *RegImap_useNextPH*  >
  let g:RegImap_useNextPH = '<Tab>'
<

If *popupmenu-completion* is visible (it is patterned by <C-X>, omni
completion or other), then <C-E> key is send, leading to selection of
current item. Else cursor is moved to the next |PlaceHolder|. It works in
insert, normal or select modes. |PlaceHolder| is removed and if it has default
value it is inserted.

                                                       *RegImap_selectNextPH*  >
  let g:RegImap_selectNextPH = '<S-Tab>'
<

Move cursor to the next |PlaceHolder|, without expanding them. They are just
selected, allowing you to delete it by pressing any key or using
|RegImap_clearSelection| key. You also can expand it by pressing <Tab>. If
there is no |PlaceHolder| in file, nothing happens.

                                                     *RegImap_exitSelectMode*  >
  let g:RegImap_exitSelectMode = ';'
<

Key to exit from select mode to normal mode. Selected text is left unchanged.

                                                  *RegImap_clearSelectedText*  >
  let g:RegImap_clearSelectedText = '<C-j>'
<

Key to clear selected text and go to insert mode. If it results in empty
string, it is deleted. Then move to next |PlaceHolder| if possible.


==============================================================================
PATTERNS FOR SPECIFIC FILETYPES                            *RegImap-patterns*

Here are described several mapping for different filetypes. You can find more
in your |RegImap_baseDir| directory. All of them are for insert mode, cursor
position is indicated by | character.


VIM                                                             *RegImap-vim*

If you are not inside string by typing semicolon you will exit insert mode.

>
 -----------------------------------------------------------------------------
 | sample        | key | result              | comment                       |
 -----------------------------------------------------------------------------
 | let a = 2|    |  ;  | let a = 2|          | normal mode                   |
 | let a = |     |  [  | let a = [|]         |                               |
 | let a = '|    |  [  | let a = '[|         | don't work inside strings     |
 | let a = |     |  {  | let a = {|}         |                               |
 | let a = |     |  '  | let a = '|'         |                               |
 | call cursor   |  '  | call cursor(|)      | after character you bet braces|
 | '^\s*' . p|   |  h  | '^\s*' . PH(|)      |                               |
 | if|           |space| if |                | at the empty line             |
 |               |     |                     |                               |
 |               |     | endif               |                               |
 | if a:0 > 0|   |<tab>| if a:0 > 0          |                               |
 |   <++>        |     |   |                 |                               |
 | endif         |     | endif               |                               |
 | for|          |space| for | in <++>       | at the empty line             |
 |               |     |   <++>              |                               |
 |               |     | endfor              |                               |
 | let v = [|]   |   | let v = [           |                               |
 |               |     |        \|           |                               |
 |               |     |        \]           |                               |
 | r|            |space| call RegImap('|',   | at the start of the line      |
 |               |     |    '<++>''<+,+>')   |                               |
 | r|            |space| return |            | after several spaces          |
 | if a ma|      |space| if a =~ |           | also available nm eq ne le ge |
 | get           |  l  | getline(|)          |                               |
 | getline(|)    |  l  | getline(line('.'))  |                               |
 -----------------------------------------------------------------------------
<


TEX                                                            *RegImap-tex*

By typing semicolon you will exit insert mode.

>
 -----------------------------------------------------------------------------
 | sample        | key | result              | comment                       |
 -----------------------------------------------------------------------------
 | b|            |space| \begin{|}           | at the start of the line      |
 |               |     |   <++>              |                               |
 |               |     | \end{}              |                               |
 | \begin{|}     |  e  | \begin{e|}          | begin and end fields are sin- |
 |   E = mc^2    |     |   E = mc^2          | chronized even if you return  |
 | \end{}        |     | \end{e}             | to them later                 |
 | s|            |space| \section{|}         |                               |
 | ss|           |space| \subsection{|}      |                               |
 | |             |  [  | \[                  | at the start of the line      |
 |               |     |   |                 |                               |
 |               |     | \]                  |                               |
 | |             |  {  | {                   | at the start of the line      |
 |               |     |   |                 |                               |
 |               |     | }                   |                               |
 | \foo|         |  [  | \foo[|]             | not at the start of the line  |
 | i             |space| \item |             | at the start of the line      |
 | ith           |space| $i$--th |           |                               |
 | jth           |space| $j$--th |           |                               |
 | text |        |  '  | text $|$<++>        | in text replaced to inline    |
 |               |     |                     | equation                      |
 | $f|$          |  '  | $f(|)<++>$          | in math replaced to braces    |
 -----------------------------------------------------------------------------
<


CPP                                                            *RegImap-cpp*

>
 -----------------------------------------------------------------------------
 | sample        | key | result              | comment                       |
 -----------------------------------------------------------------------------
 | if|           |space| if(|)               | at the empty line             |
 |               |     |     <++>            |                               |
 | sw|           |space| switch(|)           |                               |
 |               |     | {                   |                               |
 |               |     |     <+case+>        |                               |
 |               |     |     <+default: +>   |                               |
 |               |     | }                   |                               |
 | case 1: |     |<tab>| case 1:             | you can get substitute of     |
 | <+case +>     |     | case |: <+code+>;   | your PlaceHolder, making      |
 |               |     |         <+break;+>  | recursive substitutes         |
 |               |     | <+case +>           |                               |
 | |             |  [  | {                   | at the start of the line      |
 |               |     |     |               |                               |
 |               |     | }                   |                               |
 | argc|         |  [  | args[|]             | not at the start of the line  |
 -----------------------------------------------------------------------------
<


==============================================================================
FEATURES                                                   *RegImap-features*

RegImap plugin has the following features among others:
  - The syntax of |RegImap| allows to use TextMate's or other snippets. But
    maybe you will want to improve them with the power of regular expressions.
  - Dynamic update of linked variables is possible by defining appropriate
    mapping. See for loop for CPP and begin-end statemnts for TEX
  - PlaceHolders within placeholders are possible, look at 'case ' pattern for
    cpp.


==============================================================================
CONTACT                                      *RegImap-contact* *RegImap-author*

Script homepage http://www.vim.org/scripts/script.php?script_id=3686

If you have any suggestions, improvements or want to share with others your
patterns, please post them here http://github.com/artshcherbina/vim-RegImap
or send at my email: artshcherbina <at> gmail <dot> com


==============================================================================
CHANGELOG                                                *RegImap-changelog*

0.54
 - Added PlaceHolder highlighting
 - Added CursorPH
 - Fixed small bugs

0.53
 - Added use of InsertCharPre autocommand if you have patch196
 - Refactored code, fixed small bugs

0.52
 - Fixed sourcing algorithm (thanks to Alexey Radkov)
 - Added ReloadRegImaps function
 - Added g:RegImap_ to settings names
 - Updated help

0.51
 - Fixed path detection 
  
0.5~
 - First public version

==============================================================================
vim:tw=78:ts=8:ft=help:norl:
RegImap/common.vim	[[[1
3
call SetParameters({'filetype' : 'common'})

call RegImap('\<cdate', '\=strftime("%Y-%m-%d")')
RegImap/vim.vim	[[[1
77
call SetParameters({'filetype' : 'vim'})

call RegImap('^' . closedQuotedText . "('\\zs" . cursor . "\\ze)" . closedQuotedText . '$', PH() . "'", {'condition' : "'"})
call RegImap('^' . closedQuotedText . '\zs;' . cursor . '\ze' . closedQuotedText . '$', '', {'feedkeys' : "\<Esc>"})
call RegImap('^' . closedQuotedText . '[[]\zs' . cursor . '\ze\([]]\@!.\)*$', PH() . ']')
call RegImap('^' . closedQuotedText . '{\zs' . cursor . '\ze[^}]*$', PH() . '}')

let whiteStartCommands = [
      \['\s\zsr ', 'return '],
      \['if ', 'if ' . PH() . '\1  ' . PH() . '\1endif'],
      \['  e ', 'else\1  ' . PH()],
      \['for \zs', PH('key') . ' in ' . CursorPH('list') . '\1  ' . PH() . '\1endfor'],
      \['ex ', "exec '" . PH('normal! ') . "' . " . PH(), 'e'],
      \['.*[[]\s*\n\zs' . cursor . '\ze[]]', '\1      \\' . PH() . '\1      \\'],
      \]

for key in whiteStartCommands
  call RegImap(whiteStart . key[0], key[1])
endfor

call RegImap(whiteStart . '\\.*\n\zs\s*' . cursor, '\1\\', {'condition' : ' \|\n'})

let startCommands = [
      \['fu ', 'function! ' . PH() . '()  ' . PH() . 'endfunction'],
      \['fi ', 'finish'],
      \['[invsx]no', '&remap '],
      \['\(\w*\) def ', 'if !exists("g:\1")  let \1 = ' . PH('default') . 'endif'],
      \['r ', "call RegImap('". PH() . "', '". PH() . "')"],
      \['call RegImap(' . closedQuotedText . ',' . closedQuotedText . ',\zs\ze' . cursor . ')$', '', "{}\<Left>:"],
      \]

call RegImap('^' . closedQuotedText . '{\zs:' . cursor . '\ze}', PH() . " : " . PH())


for key in startCommands
  if exists("key[2]")
    call RegImap('^' . key[0], key[1], {'feedkeys' : key[2]})
  else
    call RegImap('^' . key[0], key[1])
  endif
endfor


let spaceCommands = [
      \['ma', '=\~'],
      \['nm', '!\~'],
      \['eq', '=='], 
      \['ne', '!='], 
      \['le', '<='], 
      \['ge', '>='], 
      \['an', '\&\&'], 
      \['or', '||'], 
      \]
      
for key in spaceCommands
  call RegImap('^' . closedQuotedText . '\zs\<' . key[0] . ' ', key[1] . ' ')
endfor

call RegImap('\<ph ', 'PH(' . PH() . ')' . PH())

let functions = [
      \['getl', 'getline(' . PH() . ')'],
      \['getline(l\zs' . cursor . '\ze)', "ine('" . PH('.') . "')"],
      \]
     
for key in functions
  call RegImap('\<' . key[0], key[1])
endfor

" Other commands
call RegImap('\<line(\zs\.' . cursor . '\ze)', "'.'")

" Single quote to braces after char
call RegImap('^' . closedQuotedText . '\w\zs' . "'\\ze" . cursor . closedQuotedText . '$', '(' . PH() . ')', {'condition' : "'"})
" Single quote to two single quotes
call RegImap('^' . closedQuotedText . "\\%('\\@!\\W\\|^\\)" . '\zs' . "'\\ze" . cursor . closedQuotedText . '$', "'" . PH() . "'", {'condition' : "'"})

RegImap/cpp.vim	[[[1
50
call SetParameters({'filetype' : 'cpp'})


call RegImap(whiteStart . '\zssw ', 'switch(' . PH() . ')\1{\1    ' . PH("case ") . '\1    ' . PH("default: ") . '\1}')
call RegImap(whiteStart . 'case \zs', PH("value") . ': ' . PH("code") . '; ' . PH('break;') . '\1' . PH("case "))

call RegImap('^\s*\zsp\s', 'printf("' . PH('Done') . '\\n");')
call RegImap('^\s*\zspi\s', 'printf("%i\\n", ' . PH() . ');')
call RegImap('^\s*\zspii\s', 'printf("%i, %i\\n", ' . PH() . ');')
call RegImap('^\s*\zsps\s', 'printf("%s\\n", ' . PH() . ');')

let whiteStartCommands = [
      \['r ', 'return '],
      \['\(else\|}\|\)\s*if\zs ', '(' . PH() . ')\1    ' . PH()],
      \['e ', 'else\1    ' . PH()],
      \['c ',  'const '],
      \['d ',  'double '],
      \['b ',  'bool '],
      \['v ',  'void '],
      \['ci ', 'const int '],
      \['for ', 'for(int ' . PH() . ' = 0; \2 < ' . PH() . '; \2++)\1    ' . PH()],
      \['const\s*\(int\|double\)\s*\(\w*\)\zs\w', '\U&'],
      \]                

for key in whiteStartCommands
  call RegImap(whiteStart . key[0], key[1])
endfor
  
call RegImap(whiteStart . 'for\s*(.\{-}\s\(\k\+' . cursor . '\k*\)\zs\(\s*=.\{-};\s\{-}\)\s\k*\(.*;\s*\)\k*\ze.*)', CursorPH() . '\3 \2\4\2')

let spaceCommands = [
      \['ma', '=\~'],
      \['nm', '!\~'],
      \['eq', '=='], 
      \['ne', '!='], 
      \['le', '<='], 
      \['ge', '>='], 
      \['an', '\&\&'], 
      \['or', '||'], 
      \]
      
for key in spaceCommands
  call RegImap('^' . closedQuotedText . '\zs\<' . key[0] . ' ', key[1] . ' ')
endfor

" Single quote to braces after char
call RegImap('^' . closedQuotedText . notSQDQ . '\{-}\w\zs' . "'\\ze" . cursor . closedQuotedText . notSQDQ . '*$', '(' . PH() . ')', {'condition' : "'"})
" Single quote to two single quotes
call RegImap('^' . closedQuotedText . notSQDQ . '\{-}' . "'\\@!\\W" . '\zs' . "'\\ze" . cursor . closedQuotedText . notSQDQ . '*$', "'" . PH() . "'", {'condition' : "'"})

RegImap/tex.vim	[[[1
47
call SetParameters({'filetype' : 'tex'})

let whiteStartCommands = [
      \['b ', '\\begin{' . PH() . '}' . PH('\\label{') . '\1  ' . PH() . '\1\\end{}'],
      \['s ', '\\section{' . PH() . '}'],
      \['ss ', '\\subsection{' . PH() . '}'],
      \['[[]', '\\[\1  ' . PH() . '\1\\]'],
      \['{', '{' . PH() . '\1}'],
      \['i ', '\\item '],
      \]

for key in whiteStartCommands
  if exists("key[2]")
    call RegImap(whiteStart . key[0], key[1], {'feedkeys' : key[2]})
  else
    call RegImap(whiteStart . key[0], key[1])
  endif
endfor

call RegImap('[[]\zs' . cursor . '\ze\([]]\@!.\)*$', PH() . ']')
call RegImap('^\%([^$]*\$[^$]*\$\)*\zs\(\w\)' . cursor, ' \1')

let commands = [
      \['{\zs', PH() . '\1}' . PH() ],
      \]

for key in commands
  if exists("key[2]")
    call RegImap(key[0], key[1], {'feedkeys' : key[2]})
  else
    call RegImap(key[0], key[1])
  endif
endfor

" Text patterns
call RegImap('\<ith ', '$i$-th')
call RegImap('\<jth ', '$i$-th')
call RegImap('\<\([dD]\)ont ', "\1on't ")
call RegImap('\<its ', "it's ")

" Sinchronize \begin{} and \end{} based on indent
call RegImap(whiteStart . '\\begin{\zs\([^}]*\)\%#\([^}]*\)\(}.*\n' . '\(\1\(\s.*\)\?\n\)\{-}' . '\1\\end{\)[^}]*\ze}', '\2\3' . PH() . '\4\2\3')
call RegImap(whiteStart . '\\begin{\zs[^}]*\(}.*\n' . '\(\1\(\s.*\)\?\n\)\{-}' . '\1\\end{\)\([^}]*\)\%#\([^}]*\)\ze}', '\5\6\2\5\6' . PH())

" Single quote to braces after char
"call RegImap("'\\ze" . cursor, '(' . PH() . ')' . PH() )

