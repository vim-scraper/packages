" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/RegImap.vim	[[[1
251
" File:          regImap.vim
" Author:        Artem Shcherbina
" Last Updated:  Aug 6, 2011
" Version:       0.5
" Description:   regImap.vim  Plugin for using regular expression substitutes in insert mode
"
"                For more help see regImap.txt; you can do this by using:
"                :helptags ~/.vim/doc
"                :h regImap.txt

"plugin/RegImap.vim
"doc/RegImap.txt
"RegImap/common.vim
"RegImap/vim.vim
"RegImap/cpp.vim
"RegImap/tex.vim



if exists("g:regImap_inited") || &cp || version < 700
  finish
endif

let regImap_inited=1

autocmd CursorMovedI * call TriggerRegImap()
autocmd BufEnter * call ReadRegImaps()

if !exists("g:baseDir")
  let baseDir = &runtimepath
endif

if !exists("g:useNextPH")
  let useNextPH = '<Tab>'
endif

if !exists("g:selectNextPH")
  let selectNextPH = '<S-Tab>'
endif

if !exists("g:exitSelectMode")
  let exitSelectMode = ';'
endif

if !exists("g:clearSelectedText")
  let clearSelectedText = '<C-j>'
endif

exec 'inoremap ' . useNextPH . ' <C-r>=NextPH()'
exec 'nnoremap ' . useNextPH . ' a<C-r>=NextPH("") . Escape()'
exec 'snoremap ' . useNextPH . ' <Esc>a<C-r>=CheckSelected()'
exec 'nnoremap ' . selectNextPH . ' :call SelectPH()'
exec 'snoremap ' . selectNextPH . ' <Esc>:call SelectPH()'
exec 'inoremap ' . selectNextPH . ' <Esc>:call SelectPH()'
exec 'snoremap ' . exitSelectMode . ' <Esc>'
exec 'snoremap ' . clearSelectedText . ' i<BS><C-r>=ClearSelection()'

function! Escape()
  return "\<Esc>"
endfunction

function! ClearSelection()
  let save_cursor = getpos(".")
  if search('^\s*\n', 'bc', line('.'))
    call cursor(line('.'), save_cursor[2])
    return "\<Esc>dd"
  endif
  return ''
endfunction

function! CheckSelected()
  if search(g:isPH . g:cursor, 'bce')
    let save_cursor = getpos(".")
    s/<[+]\(.\{-}\)+\%#>/\1/
    call cursor(line('.'), save_cursor[2]-3)
    return TriggerRegImap(1)
  endif
  return "\<Esc>a"
endfunction


let s:regImaps = {}
let whiteStart='^\(\s*\)\zs'
let cursor = '\' . '%#'
let isPH = '<' . '+.*+>'
let isAutoPH = '<' . '+|.*|+>'

let notDQ = '\%(\\\\\|\\[^\\]\|[^\"]\)'
let DQstr = '"' . notDQ . '*"'
let notSQ = "[^']"
let SQstr = "'" . notSQ . "*'"
let notSQDQ = "[^\"']"
let closedQuotedText = '\%(' . notSQDQ . '*\%(' . SQstr . '\|' . DQstr . '\)\)*' . notSQDQ . '*'

fun! PH(...)
  return '<' . '+' . join(a:000) . '+>'
endfun

let defaults = {
      \'filetype' : 'common',
      \'condition' : '',
      \'feedkeys' : '',
      \}

let parameters = defaults

function! SetParameters(...)
  if a:0 == 0
    let g:parameters = g:defaults
  else
    call extend(g:parameters, a:1)
  endif
endfunction

function! GetParameters(input)
  let parameters = extend(copy(g:parameters), a:input)
  if parameters.condition !~ g:cursor
    let parameters.condition .= '\%#'
  endif
  if parameters.filetype == ''
    let parameters.filetype = 'common'
  endif
  return parameters
endfunction

fun! RegImap(pattern, substitute, ...)
  if a:pattern == ''
    echohl WarningMsg
    echomsg 'RegImap: Empty pattern for ' . a:ft
    echohl None
  endif
  let pattern = a:pattern . (a:pattern =~ '\\%#' ? '' : '\%#')
  
  " Replace  by special string
  let substitute = substitute(a:substitute, '', '?c' . 'r?', 'g')
  if (substitute[0:1] != '\=') && substitute !~ g:isPH
    let substitute .= PH()
  endif

  if a:0 > 0
    let parameters = GetParameters(a:1)
  else
    let parameters = g:parameters
  endif
  
  for ft in split(parameters.filetype, '\.')
    if !has_key(s:regImaps, ft)
      let s:regImaps[ft] = []
    endif
    let defined=0
    for regImap in s:regImaps[ft]
      if regImap.pattern == pattern
        echohl WarningMsg
        echomsg 'RegImap: Redefining ' . a:pattern . ' for ' . ft
        echohl None
        let defined=1
      endif
    endfor
    if !defined
      " Singleline if not \n
      call add(s:regImaps[ft], extend({'pattern' : pattern, 'substitute' : substitute, 'singleLine' : (pattern !~ '\\n')}, parameters))
    endif
  endfor
endfun

fun! ReadRegImaps()
  let s:regImaps = {}
  
  for regImapsFile in split(globpath(g:baseDir, 'regImap/common.vim')) + split(globpath(&rtp, 'regImap/' . &ft . '.vim'))
    exec 'normal! :so ' . regImapsFile . ''
  endfor
endf

function! SelectPH()
  if search(g:isPH, 'w') " PlaseHolder found
    let save_cursor = getpos(".")
    let width = searchpos('+>', 'en')[1] - save_cursor[2]
    call feedkeys("\<Esc>v" . (width) . "lo\<C-g>", 'n')
  endif
endfunction


function! NextPH(...)
"      return ''
  if pumvisible()
    return "\<C-y>"
  endif
  if search(g:isPH, 'cw') " PlaseHolder found
    let save_cursor = getpos(".")
    let width = searchpos('+>', 'en')[1] - save_cursor[2]
  
    s/\%#<[+]\(.\{-}\)+>/\1/
    call setpos('.', save_cursor)
    
    if width > 4
      " PlaseHolder with text
      call feedkeys("\<Esc>lv" . (width-4) . "l\<C-g>", 'n')
    elseif width == 4
      " PlaseHolder with one char
      call feedkeys("\<Esc>lv\<C-g>", 'n')
    endif
    if a:0 > 0
      call feedkeys(a:1)
    endif
  endif
  return ''
endfunction

fun! TriggerRegImap(...)
  for ft in split(&ft, '\.') + ['common']
    if exists('s:regImaps["'.ft.'"]')
      for regImap in s:regImaps[ft]
        if regImap.singleLine
          let lineNum = line('.')
          if (regImap.condition == '' || search(regImap.condition, 'cnb', lineNum)) && search(regImap.pattern, 'cnb', lineNum)
            exec 'normal! :s/' . regImap.pattern . '/' . regImap.substitute . "\<CR>"
            " Put  defined in RegImap back
            s/?[c]r?//ge
            call setpos('.', [bufnr("%"), lineNum, 0, 0])
            call NextPH(regImap.feedkeys)
            return ''
          endif
        else
          let lineNum = line('.')
          if (regImap.condition == '' || search(regImap.condition, 'cnb')) && search(regImap.pattern, 'cnb')
            exec 'normal! :%s/' . regImap.pattern . '/' . regImap.substitute . "\<CR>"
            " Put  defined in RegImap back
            %s/?[c]r?//ge
            call setpos('.', [bufnr("%"), lineNum, 0, 0])
            call NextPH(regImap.feedkeys)
            return ''
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
      for regImap in s:regImaps[ft]
        exec 'normal! o' . regImap.pattern . ' -> ' . regImap.substitute . ' / ' . regImap.condition . ' / ' . regImap.feedkeys
      endfor
    endif
  endfor
endfun

doc/RegImap.txt	[[[1
339
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

For Vim version 7.0 or later.
This plugin only works if 'compatible' is not set.
{Vi does not have any of these features.}


==============================================================================
USAGE                                                         *RegImap-usage*

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

More formally, |RegImap| works like executing s/{pattern}/{substitute}/ every
time you enter a character or move the cursor in insert mode.

In your {substitute} pattern you can use |PlaceHolders|. They are defined be |PH|. 
You can jump to the next placeholder by pressing <tab> button, or cicle
through available |PlaceHolders| by <S-Tab>. Look at |RegImap-options| fom
more detailes.

New patterns are defined by |RegImap| function. You can use it directly at you
vim file at ftplugin directory. More convinent way is to keep them all at the
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
    {pattern}. If it is not sutisfied, substitute is not performed. Added for
    efficiency, when using complex or multiline {pattern}.
  - {feedkeys}, default ''. Keys, that are send to vim after successfull
    substitute. Default values of these parameters can be changed by
    |RegImap-SetParameters|.

Note If {pattern} does not contain atom '\%#' (matches at cursor position),
     then it is added to the end. If {substitute} does not contain
     |PlaceHolder| calls, one |PlaceHolder| is added to the end.

After substitute 's/{pattern}/{substitute}/' is done, cursor is moved to the
next |PlaceHolder|.

After that |feedkeys| is called fith {feedkeys} argument.


SetParameters([{parameters}])                         *RegImap-SetParameters*

With this function you can change default values of {parameters}, specified in
|RegImap|. If you call it without arguments, {parameters} are reset to
defaults.


PH([{string}])                           *PlaceHolder*   *RegImap-PlaceHolders*

Return a Placeholder with possible default value after placeholder selection.
Basicall for {substitute} fields.


ReadRegImaps()                                         *RegImap-ReadRegImaps*

Sourses .vim files from directories, specified by |RegImap-basedir|. This files
usually contain vim scripts, |RegImap| calls.


==============================================================================
VARIABLES                                                 *RegImap-variables*

Usefull variables to use in {pattern} fields.

>
 -----------------------------------------------------------------------------
 | variable         | value       | matches                                  |
 -----------------------------------------------------------------------------
 | whiteStart       | ^\(\s*\)\zs | spaces of tabs at the start of the line, |
 |                  |             | they are not includet into the match     |
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

                                                            *RegImap-basedir*  >
  let g:baseDir = &runtimepath
<

Mappings are by default looked for any RegImap directory in your
'runtimepath'. Typically, it is located at '~/.vim/RegImap/' on *nix or
'$HOME\vimfiles\RegImap\' on Windows. To change that location or add another
one, change the g:baseDir variable in your |.vimrc| to your preferred
directory. This will be used by the |globpath()| function, and so accepts the
same syntax as it (e.g., comma-separated paths).

                                                          *RegImap-useNextPH*  >
  let useNextPH = '<Tab>'
<

If *popupmenu-completion* is visible (it is patterned by <C-X>, omni
complition or other), then <C-E> key is send, leading to selection of
current item. Else cursor is moved to the next |PlaceHolder|. It works in
insert, normal or select modes. |PlaceHolder| is removed and if it has default
value it is inserted.

                                                       *RegImap-selectNextPH*  >
  let selectNextPH = '<S-Tab>'
<

Move cursor to the next |PlaceHolder|, without expanding them. They are just
selected, allowing you to delete it by pressing any key or using
|RegImap-clearSelection| key. You also can expand it by pressing <Tab>. If
there is no |PlaceHolder| in file, nothing happens.

                                                     *RegImap-exitSelectMode*  >
  let exitSelectMode = ';'
<

Key to exit from select mode to normal mode. Selected text is left unchanged.

                                                  *RegImap-clearSelectedText*  >
  let clearSelectedText = '<C-j>'
<

Key to clear selected text and go to insert mode. If it results in empty
string, it is deleted.


==============================================================================
PATTERNS FOR SPECIFIC FILETYPES                            *RegImap-patterns*

Here are described several mapping for different filetypes. You can find more
in your |RegImap-baseDir| directory. All of them are for insert mode, cursor
position is indicated by | character.


VIM                                                             *RegImap-vim*

If you are not inside string by typing semicolon you will exit insert mode.
This is much more usefull than <Esc> of <C-[>.

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
 | i|            |  f  | if |                | at the empty line             |
 |               |     |                     |                               |
 |               |     | endif               |                               |
 | if a:0 > 0|   |<tab>| if a:0 > 0          |                               |
 |   <++>        |     |   |                 |                               |
 | endif         |     | endif               |                               |
 | fo|           |  r  | for | in <++>       | at the empty line             |
 |               |     |   <++>              |                               |
 |               |     | endfor              |                               |
 | let v = [|]   |   | let v = [           |                               |
 |               |     |        \|           |                               |
 |               |     |        \]           |                               |
 | imap cl|      |space| imap <C-l>|         | any character instead of l    |
 | imap <C-l> r| |space| imap <C-l> <Right>| | also available <BS> <Down>    |
 | imap f5|      |space| imap <F5>|          | <Esc> <Left> <Right> <Space>  |
 |               |     |                     | <Tab> <Up>                    |
 | r|            |space| call RegImap('|',   | at the start of the line      |
 |               |     |    '<++>', '<++>')  |                               |
 | r|            |space| return |            | inside function               |
 | if a ma|      |space| if a =~ |           | also available nm eq ne le ge |
 | get           |  l  | getline(|)          |                               |
 | getline(|)    |  l  | getline(line('.'))  |                               |
 -----------------------------------------------------------------------------
<


TEX                                                            *RegImap-tex*

If you are not inside string by typing semicolon you will exit insert mode.

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
 -----------------------------------------------------------------------------
<


CPP                                                            *RegImap-cpp*

By typing semicolon you will exit insert mode. If you are at the end of
line semicolon is left in the text.

>
 -----------------------------------------------------------------------------
 | sample        | key | result              | comment                       |
 -----------------------------------------------------------------------------
 | i|            |  f  | if(|)               | at the emply line             |
 |               |     |     <++>            |                               |
 | for|          |space| for | in <++>       | at the emply line             |
 |               |     |   <++>              |                               |
 |               |     | endfor              |                               |
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
  - Dinamic update of linked variables is possible by defining appropriate
    mapping. See |for| loop for CPP and |begin-end| for TEX
  - PlaceHolders within placeholders are possible, look at 'case ' pattern for
    cpp.


==============================================================================
CONTACT                                      *RegImap-contact* *RegImap-author*

If you have any suggestions, improvements or want to share with others your
patterns, please email: artshcherbina <at> gmail <dot> com


==============================================================================
vim:tw=78:ts=8:ft=help:norl:
RegImap/common.vim	[[[1
4
call SetParameters({'filetype' : 'common'})
call RegImap('\<ph ', '|' . PH() . '|' . PH())

call RegImap('\<cdate', '\=strftime("%Y-%m-%d")')
RegImap/vim.vim	[[[1
126
call SetParameters({'filetype' : 'vim'})

call RegImap('^' . closedQuotedText . '\zs;', '', {'feedkeys' : "\<Esc>"})
call RegImap('^' . closedQuotedText . '[[]\zs' . cursor . '\ze\([]]\@!.\)*$', PH() . ']')
call RegImap('^' . closedQuotedText . '{\zs' . cursor . '\ze[^}]*$', PH() . '}')
call RegImap('^call RegImap' . closedQuotedText . '{\zs:' . cursor . '\ze}', "'" . PH() . "' : '" . PH() . "'")

let whiteStartCommands = [
      \['r ', 'return '],
      \['if', 'if ' . PH() . '\1  ' . PH() . '\1endif'],
      \['  el', 'else\1  ' . PH()],
      \['for', 'for ' . PH('key') . ' in ' . PH() . '\1  ' . PH() . '\1endfor'],
      \['exe', "exec '" . PH('normal! ') . "' . " . PH(), 'e'],
      \['.*[[]\s*\n\zs' . cursor . '\ze[]]', '\1      \\' . PH() . '\1      \\'],
      \]
"      \['\\.*[[]\s*\n' . cursor, "exec '" . PH('normal! ') . "' . " . PH(), 'e'],

if !exists("g:baseDir")
  let baseDir = &rtp
endif

" Auto let 

for key in whiteStartCommands
  call RegImap(whiteStart . key[0], key[1])
endfor

call RegImap(whiteStart . '\\.*\n\zs', '\1\\', {'condition' : '\n'})

"let speedCalls = [
"      \]
"for key in speedCalls
"  call RegImap(whiteStart . 'call \zs' . key[0] . ' ', key[1])
"endfor

let startCommands = [
      \['fu', 'function! ' . PH() . '()  ' . PH() . 'endfunction'],
      \['fi', 'finish'],
      \['[invsx]no', '&remap '],
      \['\(\w*\) def ', 'if !exists("g:\1")  let \1 = ' . PH('default') . 'endif'],
      \['r ', "call RegImap('". PH() . "', '". PH() . "'" . PH(', ') . ")"],
      \]
      
for key in startCommands
  call RegImap('^' . key[0], key[1])
endfor

let vimKeyCodes =[
      \['b', '<BS>'],
      \['c', '<CR>'],
      \['c\(.\)', '<C-\1>'],
      \['d', '<Down>'],
      \['e', '<Esc>'],
      \['f\(\d\)', '<F\1>'],
      \['f\(1\d\)', '<F\1>'],
      \['l', '<Left>'],
      \['r', '<Right>'],
      \['s', '<Space>'],
      \['t', '<Tab>'],
      \['u', '<Up>']]


for key in vimKeyCodes
  call RegImap('\\\zs' . key[0] . ' ', key[1], {'condition' : ' '})
  call RegImap('^\%(' . notDQ . '*' . DQstr . '\)*' . notDQ . '*"' . notDQ . '*\zs\<' . key[0] . ' ', '\\' . key[1], {'condition' : ' '})
  call RegImap('^\%(' . notDQ . '*' . DQstr . '\)*' . notDQ . '*\zs\<' . key[0] . ' ', key[1], {'condition' : ' '})
"  call RegImap('\zs\<' . key[0] . ' ', key[1], {'condition' : ' '})
endfor


let spaceCommands = [
      \['ma', '=\~'],
      \['nm', '!\~'],
      \['eq', '=='], 
      \['ne', '!='], 
      \['le', '<='], 
      \['ge', '>='], 
      \]
      
for key in spaceCommands
  call RegImap('\<' . key[0] . ' ', key[1] . ' ')
endfor

let functions = [
      \['getl', 'getline(' . PH() . ')'],
      \['getline(l\zs' . cursor . '\ze)', "ine('" . PH('.') . "')"],
      \['fee', 'feedkeys' . PH()],
      \]
     
for key in functions
  call RegImap('\<' . key[0], key[1])
endfor

" Other commands
call RegImap('\<line(\zs\.' . cursor . '\ze)', "'.'")

" Single quote to braces after char
call RegImap('^' . closedQuotedText . '\w\zs' . "'\\ze" . cursor . closedQuotedText . '$', '(' . PH() . ')', {'condition' : "'"})
" Single quote to two single quotes
call RegImap('^' . closedQuotedText . "'\\@!\\W" . '\zs' . "'\\ze" . cursor . closedQuotedText . '$', "'" . PH() . "'", {'condition' : "'"})


let doubleOperators = [
      \['\s*\([!-=+><]\)\s*=', ' \1= ', '='],
      \['\s*\([=!]\)\s*\~', ' \1\~ ', '\~'],
      \]
      
for key in doubleOperators
  call RegImap('^' . closedQuotedText . '\zs' . key[0], key[1], {'condition' : key[2]})
endfor

" Insert spaces
"let operators = [
"      \['=', '='],
"      \['-', '-'],
"      \['+', '+'],
"      \['>', '>'],
"      \['<', '<'],
"      \]

"for key in operators
"  call RegImap('^' . closedQuotedText . '\S\zs' . key[0], ' ' . key[1] . ' ', key[0])
"  call RegImap(notSQDQ . '*\zs' . key[0] . '\s*=', key[1] . '= ', '')
"  call RegImap(closedQuotedText . '\S\zs' . key[0] . '\s*=', ' ' . key[1] . '= ', '')
"endfor

RegImap/cpp.vim	[[[1
30
call SetParameters({'filetype' : 'cpp'})

call RegImap(whiteStart . '\zssw ', 'switch(' . PH() . ')\1{\1    ' . PH("case ") . '\1    ' . PH("default: ") . '\1}')
call RegImap(whiteStart . 'case \zs', PH("value") . ': ' . PH("code") . '; ' . PH('break;') . '\1' . PH("case "))

call RegImap('\<\l*F\zs', 'rame')
call RegImap(';' .cursor . '\s*$', ';', {'feedkeys' : "\<Esc>"})
call RegImap(';', '', {'feedkeys' : "\<Esc>"})

call RegImap('^\s*\zsp\s', 'printf("' . PH() . '\\n");')
call RegImap('^\s*\zspi\s', 'printf("%i\\n", ' . PH() . ');')
call RegImap('^\s*\zsps\s', 'printf("%s\\n", ' . PH() . ');')

let whiteStartCommands = [
      \['re', 'return '],
      \['\(else\|}\)\s*if\zs', '(' . PH() . ')\1    ' . PH()],
      \['el', 'else\1  ' . cursor],
      \['for ', 'for ' . PH() . ' in ' . PH() . '\1  ' . PH() . '\1endfor']
      \]

for key in whiteStartCommands
  call RegImap(whiteStart . key[0], key[1])
endfor


" Single quote to braces after char
call RegImap('^' . closedQuotedText . notSQDQ . '\{-}\w\zs' . "'\\ze" . cursor . closedQuotedText . notSQDQ . '*$', '(' . PH() . ')', {'condition' : "'"})
" Single quote to two single quotes
call RegImap('^' . closedQuotedText . notSQDQ . '\{-}' . "'\\@!\\W" . '\zs' . "'\\ze" . cursor . closedQuotedText . notSQDQ . '*$', "'" . PH() . "'", {'condition' : "'"})

RegImap/tex.vim	[[[1
39
call SetParameters({'filetype' : 'tex'})

let whiteStartCommands = [
      \['b ', '\1\\begin{' . PH() . '}\1  ' . PH() . '\1\\end{}'],
      \['s ', '\1\\section{' . PH() . '}'],
      \['ss ', '\1\\subsection{' . PH() . '}'],
      \['\zs[[]', '\\[' . PH() . '\1\\]'],
      \['\zs{', '{' . PH() . '\1}'],
      \['\zsi ', '\\item '],
      \]

for key in whiteStartCommands
  call RegImap(whiteStart . key[0], key[1])
endfor


" Math patterns
"call RegImap('\<h ', '\hat')
"call RegImap('\<b ', '\bar')
"call RegImap('\<s ', '\sub')

" Text patterns
call RegImap('\<ith ', '$i$-th')
call RegImap('\<jth ', '$i$-th')
"call RegImap('\<x ', '$X$')
"call RegImap('\<k ', '$K$')
call RegImap('\<\([dD]\)ont ', "\1on't ")
"call RegImap('\<sub', "subpopulation")


" Sinchronize \begin{} and \end{} based on indent
call RegImap(whiteStart . '\\begin{\zs\([^}]*\)\%#\([^}]*\)\(}.*\n' . '\(\1\(\s.*\)\?\n\)\{-}' . '\1\\end{\)[^}]*\ze}', '\2\3' . PH() . '\4\2\3')
call RegImap(whiteStart . '\\begin{\zs[^}]*\(}.*\n' . '\(\1\(\s.*\)\?\n\)\{-}' . '\1\\end{\)\([^}]*\)\%#\([^}]*\)\ze}', '\5\6\2\5\6' . PH())

" Other
"call RegImap('[[]', '[\%#]')
"call RegImap('{', '{\%#}')
"call RegImap("'", '(\%#)')
"call RegImap('\S\zs\s*\([,.]\)', '\1 ')
