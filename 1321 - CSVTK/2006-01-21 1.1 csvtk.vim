" CSVTK: a Comma Separated Values Tool Kit
" Version: 1.1
" Date: 1/21/2006
" Description:
"   CSVTK Provides functions and mappings to help working with CSV files or
"   any files where a separator (e.g. a comma) is used between values and
"   where values can be enclosed between delimiters (e.g. double quotes).
"   Separators characters inside delimiters are not considered as separators.

"   Field numbering starts at 1.

"   Mappings allow user to:
"     * Jump between highlighted fields,
"     * Jump to a specific field,
"     * Display the field number of the field under the cursor,
"     * Return the value of field (given the field number) on current line or
"     specified line,
"     * Toggle highlighting for each field (in one or several colors)
"     * jump between highlighted fields
"
"   The highlight will show in the current line and be updated when user 
"   changes line (the highlight is updated thanks to an autocomand on the 
"   CursorHold event, therefore it is not updated when in insert mode). The 
"   separator following the field is highlighted as part of the field.
"
"   Also, a few examples are provided to show how to display the current field
"   number in the status line (so you always know what field you're in) and
"   how to fold lines depending on field content.
"
" Installation:
"   Just copy the script in your plugin directory. If you want the status line 
"   to show current field, change your vimrc file as proposed at the end of 
"   this file.
"
" Customization:
"   By default, highlighting is done with a single color (the TODO color). 
"   (When two adjoining fields are highlighted, it's hard to see them as 2 
"   fields.)
"   When the script called MultipleSearch is installed, highlighting can be
"   done in several colors instead. To do so, just change the global variable
"   g:CSV_mono to 0. Do not change this variable while fields are highlighted
"   (remove all highlights first).
" 
" Usage:
"   See end of the file for mappings already defined and examples.
"
" Required_plugins:
"   multvals.vim v3.10, from Hari Krishna Dara
"   genUtils.vim v1.18.3 from Hari Krishna Dara
" 
" Optional_plugins:
"   I recommend MultipleSearch (script #479) from Dan Sharp (not to be 
"   confused with MultipleSearch2 (from belarbi benamar). CSVTK just 
"   does not work with MultipleSearch2

"This script was inspired by script 'CSV delimited field jumper : Map keys to 
"easily jump to fields in CSV files' from J Robertson (Script #309)


let g:CSV_separator = ","
" The initial field separator

let g:CSV_string_delimiter = "\""
" The initial string delimiter

let g:CSV_field_list = ''
" The list of fields to highlight, initially empty

let g:CSV_mono = 0
" Indicates whether or not to use a different color for each highlighted field
"   when 1, use single color highlight -- does not require the multipleSearch plugin
"   when 0, use multi color highlight -- requires the multipleSearch plugin   

function CSV_field_start(field)
" Returns the column number where a:field starts {{{
  let current = 1
  let in_string = 0

  let buf = getline('.')
  let maxbuf = strlen(buf)

  let i = 0
  while (i < maxbuf) && (current != a:field)
    if buf[i] == g:CSV_string_delimiter
      if in_string
        let in_string = 0
      else
        let in_string = 1
      endif
    else
      if (buf[i] == g:CSV_separator) && (! in_string)
        let current = current + 1
      endif
    endif
    let i = i + 1
  endwhile
  return i+1
endfunction " }}}

function CSV_goto_field(...)
" Put cursor at begining of given field # (prompt for a field # if none given) {{{
  if a:0 == 0
    call inputsave()
    let need_field = input('Jump to field? ')
    call inputrestore()
  else
    let need_field = a:1
  endif
  call cursor(0, CSV_field_start(need_field))
endfunction "}}}

function CSV_goto_highlighted(direction)
" Put cursor at begining of the next highlighted field in the given direction {{{
  let temp_field_list = MvQSortElements(g:CSV_field_list , ',', 'CmpByNumber', a:direction)
  let next_field = MvElementAt(temp_field_list, ',', (MvIndexOfElement(temp_field_list, ',', CSV_get_field(0)) + 1) % MvNumberOfElements(temp_field_list, ','))
  call cursor(0, CSV_field_start(next_field))
endfunction "}}} 

function CSV_get_field(verbose)
" Display (if verbose) or just return field number of field under cursor {{{
  let current = 1
  let in_string = 0

  let buf = getline('.')
  let maxbuf = strlen(buf)
  let need_col = col('.')

  let i = 0
  while (i < maxbuf) && (i < need_col)
    if buf[i] == g:CSV_string_delimiter
      if in_string
        let in_string = 0
      else
        let in_string = 1
      endif
    else
      if (buf[i] == g:CSV_separator) && (! in_string)
        if (i + 1 == need_col)
          break
        endif
        let current = current + 1
      endif
          
    endif

    let i = i + 1
  endwhile

  if a:verbose == 1
    echo 'Field: (current separator= ' . g:CSV_separator . ')  '. current
  else
     return current
  endif
endfunction "}}}

function CSV_get_new_sep()
" Prompt for new separator and delimiter {{{
 call inputsave()
 let temp = input('New separator=  (current = ' . g:CSV_separator . ')')
 call inputrestore()
 let g:CSV_separator = (temp != "" ) ? temp : g:CSV_separator

 call inputsave()
 let temp = input('New string delimiter=  (current = ' . g:CSV_string_delimiter. ')')
 call inputrestore()
 let g:CSV_string_delimiter = (temp != "") ? temp : g:CSV_string_delimiter
endfunction "}}}

function s:CSV_single_pattern(n)
" Search pattern for field a:n {{{
  return '\%(\%'.line(".").'l\%'.CSV_field_start(a:n).'c.*\%'.(CSV_field_start(a:n+1)).'c\)'
endfunction " }}}

function CSV_add_field(field)
" Add a:field to the list of fields to highlight and starts highlight if needed {{{
  let g:CSV_field_list = MvAddElement(g:CSV_field_list, ',', a:field) 
  if MvNumberOfElements(g:CSV_field_list, ',') == 1
    call CSV_start()
  endif
endfunction " }}}

function CSV_remove_field(field)
" Remove a:field from the list of fields to highlight and stop highlight if needed {{{
  let g:CSV_field_list = MvRemoveElement(g:CSV_field_list, ',', a:field)
  if MvNumberOfElements(g:CSV_field_list, ',') == 0
    call CSV_stop()
  endif
endfunction " }}}

function CSV_actual_highlight_mono_color()
" Performs the actual highlight in one color only (uses :match with a pattern of the fields to highlight) {{{
 call MvIterCreate(g:CSV_field_list, ',', 'csv_iter')
 let temp= 'match Todo /'
 while MvIterHasNext('csv_iter')
   let p=s:CSV_single_pattern(MvIterNext('csv_iter'))
     let temp=temp.p.'\|'
 endwhile
 call MvIterDestroy('csv_iter')
 let temp=substitute(temp, "\\\\|$", "/", "")
 exe temp
endfunction " }}}

function CSV_actual_highlight()
" Perform the actual highlight in multiple colors (do a multiple search for all fields in the list) {{{
 SearchReset
 SearchReinit
 call MvIterCreate(g:CSV_field_list, ',', 'csv_iter')
 while MvIterHasNext('csv_iter')
   let p=s:CSV_single_pattern(MvIterNext('csv_iter'))
   exe "Search ".p
 endwhile
 call MvIterDestroy('csv_iter')
endfunction " }}}

function CSV_start()
" Save and change the update time, set the autocomand {{{
  let g:CSV_highlight_fieldUpdateTime = &updatetime 
  set updatetime=50 
  if g:CSV_mono == 0
    augroup HiField 
      au! 
      au CursorHold * call CSV_actual_highlight()
    augroup END 
  else
    augroup HiField 
      au! 
      au CursorHold * call CSV_actual_highlight_mono_color()
    augroup END 
  endif
endfunction " }}}

function CSV_stop()
" Remove the autocmd group, reset highlighting, restore update time {{{
  augroup HiField 
    au! 
  augroup END
  if g:CSV_mono == 0
    SearchReset
    SearchReinit
  else
    match none
  endif
  if exists("g:CSV_highlight_fieldUpdateTime")
    exe 'set updatetime=' . expand(g:CSV_highlight_fieldUpdateTime) 
    unlet g:CSV_highlight_fieldUpdateTime 
  endif
endfunction " }}}

function CSV_clear_all()
" Clear the entire list of fields to highlight {{{
  let g:CSV_field_list=''
  call CSV_stop()
endfunction "}}}

function CSV_toggle_field(field)
" If a:field is in the list: remove it. Otherwise, add it to the list {{{
  if MvContainsElement(g:CSV_field_list, ",", a:field)
    call CSV_remove_field(a:field)
  else
    call CSV_add_field(a:field)
  endif
endfunction " }}}

function CSV_value(field)
" Return the value of a:field on cursor line, without the first and last string delimiter {{{
  return CSV_value_line(line("."), a:field)
endfunction " }}}
            
function CSV_value_line(line, field)
" Return the value of a:field in a:line without the first and last string delimiter {{{
  let start = CSV_field_start(a:field)  - 1
  let temp = CSV_field_start(a:field + 1)
  let len = (temp > strlen(getline(a:line))) ? CSV_field_start(a:field + 1) - start - 1 :  CSV_field_start(a:field + 1) - start - 2 
  let temp = strpart(getline(a:line), start, len) 
  return substitute(temp, "\\%(^".g:CSV_string_delimiter."\\)\\|\\%(".g:CSV_string_delimiter."$\\)", "", "g")
endfunction " }}}  

" Prompt for new field separator and string delimiter
map <C-F6> :call CSV_get_new_sep()<CR>

" Jump to a specific field (enter field at prompt)
map <C-F5> :call CSV_goto_field()<CR>

" Jump to the next highlighted field
map <S-F6> :call CSV_goto_highlighted(1)<CR>

" Jump to the previous  highlighted field
map <S-F5> :call CSV_goto_highlighted(-1)<CR>

" Toggle current field highlighting
map <S-F3> :call CSV_toggle_field(CSV_get_field(0))<CR>

" Clear the entire list of fields to highlight
map <A-F3> :call CSV_clear_all()<CR>  

" StatusLine_example: For use in status line, you can add the following group to the status line 
"option:
"%(Sep\ %{g:CSV_separator}\ Field\ %{CSV_get_field(0)}\ %)

"For example, here is my status line (inspired by one of the online vim tips)
"set statusline=%<%f\ %h%m%r%=%(Sep\ %{g:CSV_separator}\ Field\ %{CSV_get_field(0)}\ %)%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%k\ %-14.(%l,%c%V%)\ %P

" CSV_values_and_CSV_value_line_examples:
" These functions can be used for instance in a g command.
"   Example1: copy in register a all lines where field #2 is equal to "interesting":
" (first, empty buffer a)
" :let a="" 
" :%g/^/if CSV_value(1)=="interesting"|y A
"
"   Example2: fold away the lines where field #2 is not "interesting"
" set foldexpr=CSV_value_line(v:lnum,2)=~'interesting'?0:1
" set foldmethod=expr foldlevel=0 fdc=3

" vim:fdm=marker:fdc=3
