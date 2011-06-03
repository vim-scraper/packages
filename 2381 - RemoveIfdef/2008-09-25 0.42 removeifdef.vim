" Section: Introduction                                                     {{{1
"" -*- vim -*-
"" FILE:        removeifdef.vim
"" Description: remove ifdef macro of C/C++ program
"" Version:     0.42
"" Author:      Masashi Watanabe  (watanama@gmail.com)
""
"" Description:
""   This script finds #ifdef/#else/#endif keyword, and removes undefined area.
""   And display the defined macros in your source code after executing.
""
""   When we analysis the source code written by another person, a lot of
""   #ifdef are troublesome very much.  This script helps you to analysis it by
""   removing undefined area.
""   Please see the example. It helps you to understand this script work.
""
"" Instructions:
""   1 - Put in your plugin directory, and open the C/C++ file
""   2 - Type :RemoveIfdef
""   3 - This script finds ifdef macro and remove undefined area
""   If you want to add the macros, add them to this script' argument.
""
"" Feature:
""   - support the comment (// and /* ... */)
""   - decide macro keyword from file extension (support C/C++ and Verilog)
""
"" Argument:
""   <None> : Remove undefined area by the macro defined in just source file.
""   [-n]   : Print code with original source code line. This argument should
""            be specified at first.
""            ex) #define AAA
""                #ifdef AAA
""                // comment
""                #endif
""                  ===>
""                1 #define AAA
""                3 // comment
""   String : Add these strings into macro list
""
"" Note:
""  1. This script changes the C/C++ source code directly. If you want to
""     cancel, please undo (u).
""  2. If some macro are defined in the include files, this script doesn't
""     work exactly.
""
"" Examples:
""   - example source code
""       #define AAA
""       // comment 1
""       #ifdef BBB
""       #ifdef AAA
""       // comment 2
""       #else // AAA
""       // comment 3
""       #endif // AAA
""       #else // BBB
""       #ifdef AAA
""       // comment 4
""       #else // AAA
""       // comment 5
""       #endif // AAA
""       #endif // BBB
""
""   - Type RemoveIfdef
""       -> // comment 1
""          // comment 4
""
""   - Type RemoveIfdef BBB
""       -> // comment 1
""          // comment 2
""
"" Known Bug:
""  - Does not support include
""  - There are some bugs about /* ... */ comment analysis
""
"" History:
""  ver 0.1    2008.09.16 - original version
""  ver 0.2    2008.09.17 - Print defined macro into window
""  ver 0.3    2008.09.17 - Support C comment
""  ver 0.4    2008.09.17 - Support file extension
""   ver 0.41  2008.09.18 - /* - */ comment bug fix
""   ver 0.42  2008.09.19 - change support version from 6.0 to 7.0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Environment check                                                {{{1
" version check
if v:version < 700 | finish | endif

" reload check
if exists("loaded_RemoveIfdef") | finish | endif
let loaded_RemoveIfdef= 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Public variable and command definitions                           {{{1
command! -nargs=* RemoveIfdef call <SID>RemoveIfdef(<f-args>)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: RemoveIfdef                                                      {{{1
function! <SID>RemoveIfdef(...)

  let l:comment_area = 0
  let l:ifdef_level  = 0
  let l:macro_list   = []
  let l:enable_list  = [1]
  let l:arg_index    = 1
  let l:debug_mode   = 0
  let l:macro_init   = "#"

  " debug print mode
  if len(a:000) > 0 && a:1 == "-n"
    let l:debug_mode = 1
    let l:arg_index = l:arg_index + 1
  endif

  " add the marco specified through arguments
  while l:arg_index <= len(a:000)
    call add(l:macro_list, a:{l:arg_index})
    let l:arg_index = l:arg_index + 1
  endwhile

  " check file extension (default : C/C++)
  if expand("%:e") == "c"
  || expand("%:e") == "h"
  || expand("%:e") == "cpp"
  || expand("%:e") == "cc"
  || expand("%:e") == "hpp"
    let l:macro_init = "#"
  elseif expand("%:e") == "v"
    let l:macro_init = "`"
  endif

  " Get each line and check ifdef/ifndef/else/define/undef
  let l:line_num = 1
  let l:org_line = 0
  while l:line_num<=line("$")
    let l:cur_line = getline(l:line_num)
    let l:org_line = l:org_line + 1

    " comment out area check
    if l:cur_line =~ '^[ \t]*\/\/'
      if l:debug_mode == 1
        let l:line_with_num = printf("%5d %s", l:org_line, l:cur_line)
        call setline(l:line_num, l:line_with_num)
      endif
      let l:line_num = l:line_num + 1
      continue
    endif

    if l:comment_area == 0 && l:cur_line =~ '\/\*'
      if l:cur_line =~ '^[ \t]*\/\*[^(\*\/)]*\*\/[ \t]*#'
        let l:cur_line = substitute(l:cur_line, '^[ \t]*\/\*[^(\*\/)]*\*\/[ \t]*#', "#", "")
      else
        let l:comment_area = 1
      endif
    endif

    if l:comment_area == 1
      if l:cur_line =~ '\*\/'
        let l:comment_area = 0
        if l:cur_line =~ '^[^(\*\/)]*\*\/[ \t]*#'
          let l:cur_line = substitute(l:cur_line, '^[^(\*\/)]*\*\/[ \t]*#', "#", "")
        endif
      else
        if l:debug_mode == 1
          let l:line_with_num = printf("%5d %s", l:org_line, l:cur_line)
          call setline(l:line_num, l:line_with_num)
        endif
        let l:line_num = l:line_num + 1
        continue
      endif
    endif

    " 
    " defined area
    " 
    if l:enable_list[-1] == 1
      "
      " define
      "
      if l:cur_line =~ '^[ \t]*' . l:macro_init . 'define'
        let l:new_macro = substitute(l:cur_line, "^[ \t]*" . l:macro_init . "define *\\([^ ]*\\).*$", "\\1", "")

        " find defined macro
        let l:find_or_not = 0
        for target in l:macro_list
          if target == l:new_macro
            let l:find_or_not = 1
            break
          endif
        endfor

        if l:find_or_not == 0
          call add(l:macro_list, l:new_macro)
        endif

        if l:debug_mode == 1
          let l:line_with_num = printf("%5d %s", l:org_line, l:cur_line)
          call setline(l:line_num, l:line_with_num)
        endif

        let l:line_num = l:line_num + 1

      "
      " undef
      "
      elseif l:cur_line =~ '^[ \t]*' . l:macro_init . 'undef'
        call remove(l:macro_list, substitute(l:cur_line, "^[ \t]*" . l:macro_init . "undef *\\([^ ]*\\).*$", "\\1", ""))

        if l:debug_mode == 1
          let l:line_with_num = printf("%5d %s", l:org_line, l:cur_line)
          call setline(l:line_num, l:line_with_num)
        endif

        let l:line_num = l:line_num + 1

      "
      " ifndef
      "
      elseif l:cur_line =~ '^[ \t]*' . l:macro_init . 'ifndef'
        let l:keyword = substitute(l:cur_line, "^[ \t]*" . l:macro_init . "ifndef *\\([^ ]*\\).*$", "\\1", "")

        let l:find_or_not = 0
        if l:keyword != "0"
          for l:target in l:macro_list
            if l:target == l:keyword
              let l:find_or_not = 1
              break
            endif
          endfor
        endif

        if l:find_or_not == 0
          call add(l:enable_list, 1)
        else
          call add(l:enable_list, 0)
        endif
        silent! execute l:line_num . "normal dd"

      "
      " ifdef
      "
      elseif l:cur_line =~ '^[ \t]*' . l:macro_init . 'ifdef'
        let l:keyword = substitute(l:cur_line, "^[ \t]*" . l:macro_init . "ifdef *\\([^ ]*\\).*$", "\\1", "")

        let l:find_or_not = 0
        if l:keyword != "0"
          for l:target in l:macro_list
            if l:target == l:keyword
              let l:find_or_not = 1
              break
            endif
          endfor
        endif

        if l:find_or_not == 1
          call add(l:enable_list, 1)
        else
          call add(l:enable_list, 0)
        endif
        silent! execute l:line_num . "normal dd"

      "
      " else
      "
      elseif l:cur_line =~ '^[ \t]*' . l:macro_init . 'else'
        let l:enable_list[-1] = 0
        silent! execute l:line_num . "normal dd"

      "
      " endif
      "
      elseif l:cur_line =~ '^[ \t]*' . l:macro_init . 'endif'
        call remove(l:enable_list, -1)
        silent! execute l:line_num . "normal dd"

      "
      " normal line
      "
      else
        if l:debug_mode == 1
          let l:line_with_num = printf("%5d %s", l:org_line, l:cur_line)
          call setline(l:line_num, l:line_with_num)
        endif

        let l:line_num = l:line_num + 1
      endif

    " 
    " undefined area
    " 
    elseif l:enable_list[-1] == 0
      "
      " ifdef ifndef
      "
      if l:cur_line =~ '^[ \t]*' . l:macro_init . 'ifdef' || l:cur_line =~ '^[ \t]*' . l:macro_init . 'ifndef'
        let l:ifdef_level = l:ifdef_level + 1

      "
      " else
      "
      elseif l:cur_line =~ '^[ \t]*' . l:macro_init . 'else'
        if l:ifdef_level == 0
          let l:enable_list[-1] = 1
        endif

      "
      " endif
      "
      elseif l:cur_line =~ '^[ \t]*' . l:macro_init . 'endif'
        if l:ifdef_level == 0
        call remove(l:enable_list, -1)
        else
          let l:ifdef_level = l:ifdef_level - 1
        endif
      endif

      " remove undefined area
      silent! execute l:line_num . "normal dd"
    endif

  endwhile

  " print all defined area
  echo l:macro_list
endfunction
"}}}1

" vim700: set foldmethod=marker :
