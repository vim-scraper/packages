" -*- vim -*-
" FILE: python.vim
" LAST MODIFICATION: 2001/07/07
" (C) Copyright 2001 Mikael Berthe <mikael.berthe@efrei.fr>
" Version: 1.0

" USAGE:
"
" Juste source this script when editing Python files.
" Example: au FileType python source ~me/.vim/scripts/python.vim
" You can set the global variable "g:select_heading_comment" to 0
" if you don't want to select comments preceding a declaration
" Example: (in your .vimrc) "let g:select_heading_comment = 0"
" You may want to take a look at the 'shiftwidth' option for the
" shift commands...
"
" REQUIREMENTS:
" vim (>= 600)
"
" Shortcuts:
"   [[      -- Jump to beginning of block
"   ]]      -- Jump to end of block
"   ]v      -- Select (Visual Line Mode) block
"   ]<      -- Shift block to left
"   ]>      -- Shift block to right
"   ]c      -- Select current/previous class
"   ]f      -- Select current/previous function
"   ]<up>   -- Jump to previous line with the same/lower indentation
"   ]<down> -- Jump to next line with the same/lower indentation

map  [[   :PBoB<CR>
vmap [[   :<C-U>PBoB<CR>m'gv``
map  ]]   :PEoB<CR>
vmap ]]   :<C-U>PEoB<CR>m'gv``

map  ]v   [[V]]
map  ]<   [[V]]<
vmap  ]<  <
map  ]>   [[V]]>
vmap  ]>  >

map  ]c   :call PythonSelectObject("class")<CR>
map  ]f   :call PythonSelectObject("function")<CR>

map  ]<up>    :call PythonNextLine(-1)<CR>
map  ]<down>  :call PythonNextLine(1)<CR>
" You may prefer use <s-up> and <s-down>... :-)



" Menu entries
nmenu <silent> &Python.Beginning\ of\ Block<Tab>[[ 
    \[[
nmenu <silent> &Python.End\ of\ Block<Tab>]] 
    \]]
nmenu &Python.-Sep1- :
nmenu <silent> &Python.Shift\ Block\ Left<Tab>]< 
    \]<
vmenu <silent> &Python.Shift\ Block\ Left<Tab>]< 
    \]<
nmenu <silent> &Python.Shift\ Block\ Right<Tab>]> 
    \]>
vmenu <silent> &Python.Shift\ Block\ Right<Tab>]> 
    \]>
nmenu &Python.-Sep2- :
vmenu <silent> &Python.Comment\ Selection 
    \:call PythonCommentSelection()<CR>
nmenu <silent> &Python.Comment\ Selection 
    \:call PythonCommentSelection()<CR>
vmenu <silent> &Python.Uncomment\ Selection 
    \:call PythonUncommentSelection()<CR>
nmenu <silent> &Python.Uncomment\ Selection 
    \:call PythonUncommentSelection()<CR>
nmenu &Python.-Sep3- :
nmenu <silent> &Python.Previous\ Class 
    \:call PythonDec("class", -1)<CR>
nmenu <silent> &Python.Next\ Class 
    \:call PythonDec("class", 1)<CR>
nmenu <silent> &Python.Previous\ Function 
    \:call PythonDec("function", -1)<CR>
nmenu <silent> &Python.Next\ Function 
    \:call PythonDec("function", 1)<CR>
nmenu &Python.-Sep4- :
nmenu <silent> &Python.Select\ Block<Tab>]v 
    \]v
nmenu <silent> &Python.Select\ Function<Tab>]f 
    \]f
nmenu <silent> &Python.Select\ Class<Tab>]c 
    \]c
nmenu &Python.-Sep5- :
nmenu <silent> &Python.Previous\ Line\ wrt\ indent<Tab>]<up> 
    \]<up>
nmenu <silent> &Python.Next\ Line\ wrt\ indent<Tab>]<down> 
    \]<down>


:com! PBoB execute "normal ".PythonBoB(line('.'), -1)."G"
:com! PEoB execute "normal ".PythonBoB(line('.'), 1)."G"



" Go to a block boundary (-1: previous, 1: next)
function! PythonBoB(line, direction)
  let ln = a:line
  let ind = indent(ln)
  let mark = ln
  let indent_valid = strlen(getline(ln))
  let ln = ln + a:direction

  while((ln >= 1) && (ln <= line('$')))
    if (!indent_valid)
      let indent_valid = strlen(getline(ln))
      let ind = indent(ln)
      let mark = ln
    else
      if (strlen(getline(ln)))
        if (indent(ln) < ind)
          break
        endif
        let mark = ln
      endif
    endif
    let ln = ln + a:direction
  endwhile

  return mark
endfunction


" Go to previous (-1) or next (1) class/function definition
function! PythonDec(obj, direction)
  if (a:obj == "class")
    let objregexp = "^\\s*class\\s\\+[a-zA-Z0-9_]\\+"
        \ . "\\s*\\((\\([a-zA-Z0-9_,. \\t\\n]\\)*)\\)\\=\\s*:"
  else
    let objregexp = "^\\s*def\\s\\+[a-zA-Z0-9_]\\+\\s*(\\_[^:#]*)\\s*:"
  endif
  let flag = "W"
  if (a:direction == -1)
    let flag = flag."b"
  endif
  let res = search(objregexp, flag)
endfunction


" Comment out selected lines
" commentString is inserted in non-empty lines, and should be aligned with
" the block
function! PythonCommentSelection()  range
  let commentString = "#"
  let cl = a:firstline
  let ind = 1000    " I hope nobody use so long lines! :)

  " Look for smallest indent
  while (cl <= a:lastline)
    if strlen(getline(cl))
      let cind = indent(cl)
      let ind = ((ind < cind) ? ind : cind)
    endif
    let cl = cl + 1
  endwhile
  if (ind == 1000)
    let ind = 1
  else
    let ind = ind + 1
  endif

  let cl = a:firstline
  execute ":".cl
  " Insert commentString in each non-empty line, in column ind
  while (cl <= a:lastline)
    if strlen(getline(cl))
      execute "normal ".ind."|i".commentString
    endif
    execute "normal j"
    let cl = cl + 1
  endwhile
endfunction

" Uncomment selected lines
function! PythonUncommentSelection()  range
  " commentString could be different than the one from CommentSelection()
  " For example, this could be "# \\="
  let commentString = "#"
  let cl = a:firstline
  while (cl <= a:lastline)
    let ul = substitute(getline(cl),
             \"\\(\\s*\\)".commentString."\\(.*\\)$", "\\1\\2", "")
    call setline(cl, ul)
    let cl = cl + 1
  endwhile
endfunction


" Select an object ("class"/"function")
function! PythonSelectObject(obj)
  " Go to the object declaration
  normal $
  call PythonDec(a:obj, -1)
  let beg = line('.')

  if !exists("g:select_heading_comment") || (g:select_heading_comment)
    let decind = indent(beg)
    let cl = beg
    while (cl>1)
      let cl = cl - 1
      if (indent(cl) == decind) && (getline(cl)[decind] == "#")
        let beg = cl
      else
        break
      endif
    endwhile
  endif

  if (a:obj == "class")
    let eod = "\\(^\\s*class\\s\\+[a-zA-Z0-9_]\\+\\s*"
            \ . "\\((\\([a-zA-Z0-9_,. \\t\\n]\\)*)\\)\\=\\s*\\)\\@<=:"
  else
   let eod = "\\(^\\s*def\\s\\+[a-zA-Z0-9_]\\+\\s*(\\_[^:#]*)\\s*\\)\\@<=:"
  endif
  " Look for the end of the declaration (not always the same line!)
  call search(eod, "")

  " Is it a one-line definition?
  if match(getline('.'), "^\\s*\\(#.*\\)\\=$", col('.')) == -1
    let cl = line('.')
    execute ":".beg
    execute "normal V".cl."G"
  else
    " Select the whole block
    normal j
    let cl = line('.')
    execute ":".beg
    execute "normal V".PythonBoB(cl, 1)."G"
  endif
endfunction


" Jump to the next line with the same (or lower) indentation
" Useful for moving between "if" and "else", for example.
function! PythonNextLine(direction)
  let ln = line('.')
  let ind = indent(ln)
  let indent_valid = strlen(getline(ln))
  let ln = ln + a:direction

  while((ln >= 1) && (ln <= line('$')))
    if (!indent_valid) && strlen(getline(ln)) 
        break
    else
      if (strlen(getline(ln)))
        if (indent(ln) <= ind)
          break
        endif
      endif
    endif
    let ln = ln + a:direction
  endwhile

  execute "normal ".ln."G"
endfunction


" vim:set et sts=2 sw=2:
