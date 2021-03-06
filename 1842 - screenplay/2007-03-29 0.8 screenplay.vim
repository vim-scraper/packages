" Vim plugin to assist in writing well formatted screenplays
" Last Change:  2007-03-24
" Maintainer:   Alex Lance, alla at cyber.com.au
" License:      This file is placed in the public domain.
"
"
" The definition of a well formatted screenplay (as I understand it) goes
" something like:
"
" <15 spaces>SCENE/ACTION/DESCRIPTIONS (max 54 chars before wrapping)
" <37 spaces>CHARACTER NAME
" <25 spaces>DIALOG (max 35 chars before wrapping)
"
" It might look like:
"
"
"              EXT. CITY STREET - DAY
"
"              Rain pissing down. Newspaper KID on street. A MAN stops
"              to listen.
"
"                                    KID
"                              (shouting)
"                        Extra! Extra! Child labour used to
"                        sell newspapers!
"
" Features
" ========
"
"  * All text is just a tab away from being indented correctly.
"    The tab button and the backspace button have been modified to go
"    backwards and forwards in helpful chunks 
"    Action  == 16 spaces == 1 tab
"    Dialog  == 26 spaces == 2 tabs
"    Speaker == 38 spamce == 3 tabs
"
"  * The textwidth (tw) variable switches from 68 to 60, when moving from
"    ACTION to DIALOG. This ensures good margins.
"
"  * Control-p will format a paragraph from under the cursor
"    downwards, relative to the (textwidth) margin
"
"  * When you tab three times to type in a character name for DIALOG, and you
"    type the first letter of the characters name and then tab again, then you
"    will be presented with a choice to autocomplete that characters name.
"    (provided you've typed that characters name above a DIALOG at least once
"    before).
"  
"  * Hitting enter after DIALOG should align for a new character name 
"
"  * Hitting enter twice after DIALOG should align for new ACTION block
"       
" TODO: 
"  - provide indentation and textwidth settings for ACTOR DIRECTIONS (31
"    spaces and then ACTOR DIRECTION in brackets)
"  - make vim style help for this plugin
"  - syntax highlighting
"
" HOW TO INSTALL
"
"  * Drop this file in your ${VIMRUNTIME}/ftplugin/ directory
"  * ensure your instance of vim has these options enabled:
"    :filetype on
"    :filetype plugin on
"    :au BufRead,BufNewFile *.screenplay    set filetype=screenplay
"  * Ensure the suffix the file you are editing is .screenplay and away you
"    go!
"
"
"


" Avoid loading this twice
if exists("loaded_screenplay")
  finish
endif
let loaded_screenplay = 1
let g:counter = []

" Three listeners: Enter, Tab and Backspace
imap <CR> <C-R>=ScreenplayEnterPressed()<CR>
imap <TAB> <C-R>=ScreenplayTabPressed()<CR>
imap <BS> <C-R>=ScreenplayBackspacePressed()<CR>
imap  <C-R>=ScreenplayBackspacePressed()<CR>

" Reformat paragraph with Ctrl-P in insert and normal mode
imap <C-P> <C-R>=ScreenplayCtrlPPressed()<CR>
map <C-P> i<C-R>=ScreenplayCtrlPPressed()<CR>

" map ctrl-d to clean up all the whitespace so that ctrl-p work correctly
"imap <C-D> !A!<Esc>:%s/^[ ]\{1,}$//g<CR>?!A!<CR>df!i

set tw=68         " Set text width to 68
set expandtab     " Change tabs into spaces
set softtabstop=0 " softtabstop variable can break my custom backspacing
set autoindent    " Set auto indent
set ff=unix       " use unix fileformat


function! ScreenplayEnterPressed()
  let [lnum, col] = searchpos('[^ ].*', 'bnc', line("."))

  let len = len(g:counter)
  if len > 0
    let len = len -1
  endif
    
  let prev_col = get(g:counter,len)
  call add(g:counter, printf("%d",col))
  let rtn = "\<CR>"
  set tw=68
 
  " Name -> Dialog
  if col == 38 && !pumvisible()
    set tw=60
    let rtn = "\<CR>\<Esc>I".repeat(' ', 25)

  " Dialog -> New Name
  elseif col == 26
    set tw=60
    let rtn = "\<CR>\<CR>\<Esc>I".repeat(' ', 37)

  " Dialog -> Action
  elseif prev_col == 26 && col == 0
    set tw=68
    let rtn = "\<Esc>I".repeat("\<BS>", 22)
  endif

  "return rtn .prev_col." - ".col
  return rtn 
endfunction


function! ScreenplayTabPressed()
  let s:x = 4
  let s:extra = ""
  let s:coord = col(".")
  set tw=68
  
  if s:coord < 16
    let s:x = 16 - s:coord
  elseif s:coord < 26
    set tw=60
    let s:x = 26 - s:coord
  elseif s:coord < 38
    set tw=60
    let s:x = 38 - s:coord
  elseif s:coord >= 38
    let s:x = 0
    let s:extra = "\<C-X>\<C-U>"
  endif
  
  return repeat(' ', s:x) . s:extra
endfunction


function! ScreenplayBackspacePressed()
  let [lnum, col] = searchpos('[^ ].*', 'bn', line("."))
  let s:x = 1
  
  if col == 0
    let s:coord = col(".")

    if s:coord > 38
      let s:x = s:coord - 38
    elseif s:coord > 26
      let s:x = s:coord - 26
    elseif s:coord > 16
      let s:x = s:coord - 16
    elseif s:coord == 1
      let s:x = s:coord - 0
    elseif s:coord > 0
      let s:x = s:coord - 1
    endif
  endif

  return repeat("\<BS>", s:x)
endfunction


function! ScreenplayCtrlPPressed()
  let [lnum, col] = searchpos('[^ ].*', 'bnc', line("."))

  if col == 26
    set tw=60
    return "\<Esc>gq}i"
  elseif col == 16
    set tw=68
    return "\<Esc>gq}i"
  endif
  return "\<Esc>gq}i"
endfunction

" This function allows a dropdown list to 
" appear for character names at the top of DIALOG
function! ScreenplayCompleteCharacterName(findstart, base)
  if a:findstart
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ '\a'
      let start -= 1
    endwhile
    return start
  else
    let last_line = str2nr(line("$"))
    let line_num = 1
    let pattern = "^".repeat(" ",37)."[A-Za-z0-9 ']*$"
    "let pattern = 'combination'
    let matches = {}
    let names = []
    let res = []
    
    " need the call to cursos to start the search from the start of doc
    call cursor(1, 1) 

    " loop through all the line
    while line_num <= last_line
      if search(pattern,"cn",line_num) > 0
        let k = substitute(getline(line_num),"^[ ]*","","")
        let k = substitute(k,"[ ]*$","","")
        if !has_key(matches,k) && strlen(k) > 0
          let matches[k] = 1
        endif
      endif
      let line_num = line_num + 1
      call cursor(line_num, 1) 
    endwhile
   
    for key in sort(keys(matches))
      call add(names, key)
    endfor

    for n in names
      if n =~ '^' . a:base
        call add(res, n)
      endif
    endfor
    return res
  endif
endfun
set completefunc=ScreenplayCompleteCharacterName





