"""""""""""""""""""""""""""""""
"-----------------------------"
" File: buffers_search.vim
" Author: Munteanu Alexandru Ionut (io_alex_2002 [ AT ] yahoo.fr)
" Description: The "Buffers Search" plugin searches the buffers
" for a pattern, prints the results into a new buffer and lets you
" jump in the buffers, at the position of a result
" Creation Date: 06.03.2007
" Last Modified: 07.03.2007
"
" Type zR if you use vim and don't understand what this file contains
"
" {{{ License : GNU GPL
" License:
"
" "Buffers Search" plugin searches the buffers for a pattern, and
" prints the results into a new buffer
"
" Copyright (C) 2007  Munteanu Alexandru Ionut
"
" This program is free software; you can redistribute it and/or
" modify it under the terms of the GNU General Public License
" as published by the Free Software Foundation; either version 2
" of the License, or (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
" 02110-1301, USA.
"
" }}}
"
" {{{ Documentation
" Documentation:
"
" {{{ Installation :
" Installation:
"
" Download the file "buffers_search.vim" and put it into the plugins
" directory; on a unix-like system, the user plugin directory could be
" "~/.vim/plugin"
"
" }}}
" {{{ Configuration
" Configuration:
"
" Most of the time, you won't need to change anything. However, you
" can modify this script to suit your needs.
"
" What you may want to modify :
"   -the function "s:Bs_define_user_commands" defines the :Bs command;
"    you may want to change Bs to something else
"   -the function "s:Bs_keys_mapping" defines the mappings inside the
"    buffer that contains the search results (the keys that you use
"    to navigate easily)
"   -the function "s:Bs_syntax_highlight" contains the syntax highlight
"    inside the buffer with the search results; you could modifye its
"    content as you prefer
"
" }}}
" {{{ Utilisation :
" Utilisation:
" 
"O_____________:
" User Commands:
"O_____________:
"
" The only command available is :
"   ":Bs <search_regex>"
"
"   Example:
"     :Bs function test
"
" After typing this command followed by Enter (<CR>), two things
" could happend :
"   -if there is no result, there is no much change; nothing appears
"   -if there is at least one result, a new buffer appears at the
"   bottom at the screen, containing the results of the buffers; the
"   focus is then transferred to the buffer with the search results,
"   on the first result of the first printed buffer
"
"O____________________________:
" The Buffer With The Results:
"O____________________________:
" The buffer with the results contains the results for each buffer.
"
" Syntax of the buffer with the results :
" ---------------------------------------
"   The results for one buffer are between the "-buffer_name" line, and
" the next similar line that starts the next buffer or the end of the
" buffer.
"   Each result is written on one line : on the first part of the line,
" until the first ':' we have the line number of each result, and
" after the ':' we have the line containing the matched string from
" the buffer.
"   The matched string is highlighted like a search result, in all the
" results buffer.
"   Some basic syntax highlight is available, like matching numbers,
" strings, some ponctuation signs and paranthesis.
"
" Usage of the buffer with the results :
" --------------------------------------
"   Like every regular buffer, most of the tasks that you can do
" usually are available inside.
"
"  Default Mappings And Options:
"  -----------------------------
"   -the :number option is off
"   -the buffer is marked as nomodifiable
"   -the buffer is marked as a scratch; everything inside the buffer
"    will be lost, if not saved by the user
"   -some special keymaps are available inside the buffer :
"
"   Special Keymaps In The Buffer With The Results:
"   -----------------------------------------------
"   *  "Space" : moves the cursor on the first result of the next buffer
"              (below the "-buffer_name" line)
"   *  "Enter" or "Control-j" :
"         -if the cursor is on a result line (which starts with a
"           line number) the first window displayed will move to the
"           buffer that corresponds to this result (the buffer name is
"           on first line searching backwards, that starts with "-"),
"           and put the cursor to the position of the first character 
"           of the matched search
"         -if the cursor is not a result line, then it's quite the
"           same result as before, except that the cursor position
"           will be at the top of the file : line 1, column 1
"   *  "q" : the q key deletes the buffer with the results
"
" }}}
" {{{ Contributions, Support, Bugs, ...
"
" Contributions Support Bugs:
"
" You can send any support question, bug reports, patches, ... to
" Munteanu Alexandru Ionut <io_alex_2002 [ AT ] yahoo.fr>
"
" }}}
" }}}

"""""""""""""""""""""""""""""""
" User settings
"""""""""""""""""""""""""""""""
"{{{ User commands
" define user commands
function! s:Bs_define_user_commands()
  if !exists(':Bs')
    command! -nargs=1 Bs call s:Bs_search_buffers(<q-args>)
  endif
endfunction

"}}}
"{{{ Keys mapping in the buffer with the results
function! s:Bs_keys_mapping(search)
  
  let g:Bs_search = a:search

  "<CR> and <C-j> have the same result
  nnoremap <buffer> <silent> <C-j> :let Bsline=split(getline('.'),':')[0]<CR> :let Bsbuf_name=strpart(getline(search("^-.*$","nb")),1) <CR> :let [Bsline_number, Bscolumn]=searchpos(g:Bs_search,"n") <CR> :exe 'BsJb '.Bsbuf_name.' '.Bsline.' '.Bscolumn <CR>
  nnoremap <buffer> <silent> <CR> :let Bsline=split(getline('.'),':')[0]<CR> :let Bsbuf_name=strpart(getline(search("^-.*$","nb")),1) <CR> :let [Bsline_number, Bscolumn]=searchpos(g:Bs_search,"n") <CR> :exe 'BsJb '.Bsbuf_name.' '.Bsline.' '.Bscolumn <CR>

  "space goes to the next buffer result
  nnoremap <buffer> <silent> <Space> :let Bsline=search("^-.*$","n") <CR> :call cursor(Bsline+1,7) <CR>
  "q quits the buffer
  nnoremap <buffer> <silent> q :silent! bdel!<CR>
endfunction

"}}}
"{{{ Syntax highlight for the buffer with the results
function! s:Bs_syntax_highlight(search)
  "syntax the search
  exe 'silent! mat Search /'.a:search.'/'
  
  "simple number highlight
  sy match buffers_nums "\d\+"
  hi def link buffers_nums Number

  "simple keywords
  ""sy keyword Clike if then else case break void char int long
  ""hi def link Clike Keyword

  "simple string highlight
  sy match ponctuation "\.\|?\|!\|\,\|;\|<\|>\|\~\|&\|="
  hi def link ponctuation Type
  
  "simple paranthesis highlight
  sy match paranthesis "(\|)\|\[\|\]\|{\|}"
  hi def link paranthesis NonText

  "simple string highlight
  sy region buffers_doubleq start='"' end='"\|$'
  sy region buffers_singleq start="'" end="'\|$"
  sy region buffers_backticks start="`" end="`\|$"
  hi def link buffers_doubleq String
  hi def link buffers_singleq String
  hi def link buffers_backticks String

  "left numbers
  sy match line_numbers "^\d\+"
  hi def link line_numbers Question 

  "buffer titles
  sy match buffers_title "^\-.*$"
  hi def link buffers_title Title
 
endfunction

"}}}

"name of the results buffer
let g:Bs_results_buffer_name = "Buffers_search_result"
"the search that we entered
let g:Bs_search = ""
"-----------------------------"
"""""""""""""""""""""""""""""""

"{{{ Main program (defining user commands)
"avoid loading it twice
if exists("buffers_search")
  finish
else
  call s:Bs_define_user_commands()
endif
let buffers_search = 1
"}}}

"""""""""""""""""""""""""""""""
" Internal functions
"""""""""""""""""""""""""""""""
"Main functions
"{{{ s:Bs_search_buffers(search)
"search through the buffers and calls show_results
"after that, with the results found in the buffers
"
"results structure :
"  [ { 'buffer1_name' : [ { 'line_number' : 'line_content' }, { "  'line_number2' : 'line_content2' } ]
"    { 'buffer2_name' : [ { 'line_number' : 'line_content' }, { "  'line_number2' : 'line_content2' } ] ]
function! s:Bs_search_buffers(search)

  "we save the initial cursor position
  let l:initial_position = getpos(".")
  
  "we keep the results of the search here
  let l:results = []

  "we iterate over the buffers
  "get the number of the last buffer
  let l:last_buffer_number = bufnr('$')
  let l:buffer_number = 1
  "we only search in the buffers once
  let l:searched_buffers = []
  "the name of the buffer at the start
  let l:start_buffer_name = bufname('%')
  
  while (l:buffer_number <= l:last_buffer_number)

    "if the buffer is listed
    if buflisted(l:buffer_number)

      "go to the buffer
      exe 'b '.l:buffer_number

      "we get the current name of the buffer
      let l:buffer_name = bufname('%')

      "skip the buffer search result buffer
      if l:buffer_name =~ "Buffers_search_result"
        call add(l:searched_buffers,l:buffer_name)
        let l:buffer_number = l:buffer_number + 1
        if buflisted(l:buffer_number) 
          exe 'b '.l:buffer_number
        endif
      endif
      let l:buffer_name = bufname('%')
  
      "move at the top
      normal gg
  
      "if we didn't already read this buffer
      if !s:Bs_is_in_list(l:searched_buffers,l:buffer_name)
  
        "we put the buffer in the list
        call add(l:searched_buffers,l:buffer_name)
  
        "we search over the current buffer
        let l:old_line_number = 0
        let l:buffer_result = []
        while search(a:search,"e")
          "we get the current line number
          let l:line_number = line('.')
          "just if we search forward
          if l:line_number > l:old_line_number
            "we get the line content
            let l:line_content = getline(l:line_number)
            call add(l:buffer_result,{l:line_number : l:line_content})
          else
            "get out of the current buffer
            break
          endif
          let l:old_line_number = l:line_number
        endwhile "end search on the current buffer
        
        "if we have results,
        if (len(l:buffer_result) > 0)
          "we store the result for this buffer
          call add(l:results, {'-'.l:buffer_name : l:buffer_result})
        endif 
    
      endif "end is_in_list

    endif "end isbuflisted

    "increment our counter
    let l:buffer_number = l:buffer_number + 1

  endwhile "end iteration over the buffers

  "go to the buffer from start
  exe 'b '.l:start_buffer_name
  "we restore the initial cursor position
  call setpos('.', l:initial_position)
  
  "if we have results,
  if (len(l:results) > 0)
    "we show the results
    call s:Bs_show_results(a:search,l:results)
  else
    "if we have the Buffer search window, delete this buffer 
    let l:results_buffer_number = s:Bs_get_go_buffer(g:Bs_results_buffer_name)
    "change to the search results buffer
    exe 'b '.l:results_buffer_number
    "we verify if the current buffer is the right one
    if bufname('%') =~ g:Bs_results_buffer_name
      "we delete it
      bdel!
    endif
    "center the cursor in the old buffer
    normal z.
  endif

endfunction

"}}}
"{{{ s:Bs_process_results(search,results) : returns string_result
"processes the results and returns a String
"with the representation of the results
function! s:Bs_process_results(search,results)

  ""let l:string_result = "**".a:search."**\n"
  let l:string_result = ""

  "for each buffer, we show the results
  let l:number_of_buffers = len(a:results)
  for l:buffer_number in range(0,l:number_of_buffers-1)
    "we get the results of this buffer
    let l:buffer_results = a:results[l:buffer_number]
    "we get the name of the buffer
    let l:b_name = keys(l:buffer_results)
    let l:buffer_name = l:b_name[0]
    if l:buffer_number != 0
      let l:string_result = l:string_result."\n"
    endif
    "we print the buffer name
    let l:string_result = l:string_result.l:buffer_name."\n"
    let l:buffer_real_results = l:buffer_results[l:buffer_name]
    "we get the number of results of the buffer
    let l:buffer_number_of_results = len(l:buffer_real_results)
    "we iterate over the results of this buffer
    for l:result_number in range(0,l:buffer_number_of_results-1)
      let l:result = l:buffer_real_results[l:result_number]
      "we get the line number and the line_content
      let l:res_line_num = keys(l:result)
      let l:result_line_number = l:res_line_num[0]
      let l:result_line_content = l:result[l:result_line_number]
      "we print line_number and line_content
      let l:line_number_content = printf('%-4s : %s',l:result_line_number,l:result_line_content)
      let l:string_result = l:string_result.l:line_number_content."\n"
    endfor
  endfor
  
  return l:string_result

endfunction

"}}}
"{{{ s:Bs_show_results(search,results)
"shows the results of the search
function! s:Bs_show_results(search, results)
  
  "if the search results buffer already exists
  let l:results_buffer_number = s:Bs_get_go_buffer(g:Bs_results_buffer_name)
  "we should be on the results buffer now
  
  "we verify that the current buffer is the correct one
  if bufname('%') =~ g:Bs_results_buffer_name
    
    "set the buffer modifiable
    setlocal modifiable
  
    "we erase the buffer
    silent! 1,$ d
    "we process the results
    let l:buffer_string_result = s:Bs_process_results(a:search,a:results)
    "we put the results
    silent! put! =l:buffer_string_result 
    "erase the last line
    $ d d
  
    "go at the start
    call cursor(2,8)
    
    "no number at left
    setlocal nonumber
    "don't ask for saving buffer at exit, etc..
    setlocal buftype=nofile
  
    "set the buffer as non modifiable
    setlocal nomodifiable
      
    call s:Bs_keys_mapping(a:search)
    call s:Bs_syntax_highlight(a:search)

  endif "end verify if it's the currect buffer

endfunction

"}}}
"{{{ s:Bs_jump_buffer(...)
"jumps to a line into a buffer
function! s:Bs_jump_buffer(...)
  let l:buffer_name = a:1
  let l:buffer_line = str2nr(a:2)
  let l:buffer_column = str2nr(a:3)
  
  if l:buffer_line == 0
    let l:buffer_line = 1
  endif
  
  "don't count the ^\d+\s+: at the end of the line
  let l:buffer_column=l:buffer_column - 7
  if l:buffer_column == 0
    let l:buffer_column = 1
  endif

  "change the window - go to the first window
  exe '1 wincmd w'
  "change the buffer
  exe 'b '.l:buffer_name

  "go to the line
  call cursor(l:buffer_line,l:buffer_column)
  "open eventual folding
  exe 'silent! foldopen'

  "match the search in the new buffer
  exe 'mat Search /'.g:Bs_search.'/'
  redraw

  "blink the result
  sleep 300m
  exe 'mat Search /sdsdsdjldlfzheazehaozdsdqshoidf/'

  "optional
  "redraw
  "sleep 250m
  "exe 'mat Search /'.g:Bs_search.'/'
  "redraw
  "sleep 250m
  "exe 'mat Search /sdsdsdjldlfzheazehaozdsdqshoidf/'
  "redraw

endfunction

"}}}

"Utils functions
"{{{ s:Bs_get_go_buffer(buffer_name) : returns buffer_number
"checks if the results buffer already exists
"if it does not exists, it creates one
"returns the number of the buffer
"-we also go on that buffer
function! s:Bs_get_go_buffer(buffer_name)

  let l:buffer_number = bufnr(a:buffer_name)
  "if the buffer does not exists
  if l:buffer_number == -1
    "we create it
    let l:buffer_number = bufnr(a:buffer_name,1)
    "we split and go on it
    exe 'bo sp '.a:buffer_name
    exe 'resize 15'
  else
    "if the buffer is not visible, show it
    if bufwinnr(l:buffer_number) == -1
      exe 'bo sp '.a:buffer_name
      exe 'resize 15'
    endif
  endif
  let l:window_number = bufwinnr(l:buffer_number)
  exe l:window_number.' wincmd w'

  return l:buffer_number

endfunction

"}}}
"{{{ s:Bs_is_in_list(list,name)
"checks if name is in list
function! s:Bs_is_in_list(list,name)
  
  let l:list_length = len(a:list)

  if l:list_length > 0
    for pos in range(0,l:list_length-1)
      if a:list[pos] =~ a:name
        return 1
      endif
    endfor
  endif

  return 0

endfunction

"}}}

""""""""""""""""""""""""""""""""
" Internal commands
"""""""""""""""""""""""""""""""
"{{{ Internal commands
"this command is not intended for end-use, it's used when calling Bs
command! -nargs=* BsJb call s:Bs_jump_buffer(<f-args>)
"}}}

" vim:ft=vim:fdm=marker:ff=unix:nowrap:tabstop=2:shiftwidth=2
