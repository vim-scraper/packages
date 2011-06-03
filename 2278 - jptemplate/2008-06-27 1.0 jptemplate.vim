" jptemplate.vim: 
" 
" A simple yet powerful interactive templating system for VIM.
" 
" Version 1.0 (released 2008-06-28).
" 
" Copyright (c) 2008 Jannis Pohlmann <jannis@xfce.org>.
"
" This program is free software; you can redistribute it and/or modify 
" it under the terms of the GNU General Public License as published by 
" the Free Software Foundation; either version 2 of the License, or (at 
" your option) any later version.
"
" This program is distributed in the hope that it will be useful, but 
" WITHOUT ANY WARRANTY; without even the implied warranty of 
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
" General Public License for more details.
"
" You should have received a copy of the GNU General Public License 
" along with this program; if not, write to the Free Software 
" Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
" MA  02111-1307  USA
"
" TODO:
" - Code cleanup
" - Possibility to abort with <Esc> or some other key
" - Support indentation with tabs (not spaces only)
"


" Default template dir used if g:jpTemplateDir is not set
let s:templateDir = $HOME . '/.vim/jptemplate'

" Debug mode
let s:debug = 0


function! jp:GetTemplateInfo ()

  " Prepare info dictionary
  let info = {}

  " Get part of the line before the cursor
  let part = getline  ('.')[0 : getpos ('.')[2]-1]

  " Get start and end position of the template name
  let info['start'] = match    (part, '\(\w*\)$')
  let info['end']   = matchend (part, '\(\w*\)$')

  " Get template name
  let info['name']  = part[info['start'] : info['end']]

  " Throw exception if no template name could be found
  if info['name'] == ''
    throw 'No template name found at cursor'
  endif

  " Calculate line indentation
  let info['indent'] = max ([0, matchend (part, '^\s\+')])

  " Return template name information
  return info

endfunction


function! jp:ReadTemplate (name, filename)
  
  " Try to read the template file and throw exception if that fails
  try
    return readfile (a:filename)
  catch
    throw 'Template "' . a:name . '" could not be found.'
  endtry

endfunction


function! jp:SetCursorPosition (lines)

  for cnt in range (0, a:lines)
    " Search for ${cursor} in the current line
    let str    = getline  (line ('.') + cnt)
    let start  = match    (str, '${cursor}')
    let end    = matchend (str, '${cursor}') 
    let before = strpart  (str, 0, start)
    let after  = strpart  (str, end)

    if start >= 0
      " Remove ${cursor} and move the cursor to the desired position
      call setline (line ('.') + cnt, before . after)
      call cursor (line ('.') + cnt, start+1)

      " We're done
      break
    endif
  endfor

endfunction


function! jp:ProcessTemplate (info, template)
  
  let matchpos  = 0
  let names     = []
  let variables = {}

  let str = join (a:template, ' ')

  " Detect all variable names of the template
  while 1
    " Find next variable start and end position
    let start = match    (str, '${\(\w\|\s\)\+}', matchpos)
    let end   = matchend (str, '${\(\w\|\s\)\+}', matchpos)

    if start < 0
      " Stop search if there is no variable left
      break
    else
      " Extract variable name (remove '${' and '}')
      let name = str[start+2 : end-2]

      if name == 'cursor'
        " Skip the ${cursor} variable
        let matchpos = end
      else
        " Only insert variables on their first occurance
        if !has_key (variables, name)
          " Add variable name to the names list
          call add (names, name)
          
          " Add variable name (without ${}) to the dictionary
          let variables[name] = 0
        endif
    
        " Start next search at the end position of this variable
        let matchpos = end
      endif
    endif
  endwhile

  " Ask the user to enter values for all variables
  for name in names
    let variables[name] = input (name . ": ")
  endfor

  " Expand all variables
  let index = 0
  while index < len (a:template)
    for [name, value] in items (variables)
      let expr = '${' . name . '}'
      let a:template[index] = substitute (a:template[index], expr, value, 'g')
    endfor
    let index = index + 1
  endwhile

  " Backup characters before and after the template name
  let before = strpart (getline ('.'), 0, a:info['start'])
  let after  = strpart (getline ('.'), a:info['end'])

  " Generate indentation
  let indent = repeat (' ', a:info['indent'])

  " Insert template into the code line by line
  for cnt in range (0, len (a:template)-1)
    if cnt == 0 
      call setline (line ('.'), before . a:template[cnt])
    else
      call append (line ('.') + cnt - 1, indent . a:template[cnt])
    endif
    if cnt == len (a:template)-1
      call setline (line ('.') + cnt, getline (line ('.') + cnt) . after)

      " Move cursor to the end of the inserted template. ${cursor} may
      " overwrite this
      call cursor(line ('.'), len (getline (line ('.') + cnt)))
    endif
  endfor

  " Set the cursor position
  call jp:SetCursorPosition (cnt)

  " Return to insert mode
  startinsert!

endfunction


function! jp:InsertTemplate ()

  " Determine the template directory
  let templateDir = exists ('g:jpTemplateDir') ? g:jpTemplateDir : s:templateDir

  " Determine the filetype subdirectory
  let ftDir = &ft == '' ? 'general' : &ft

  try
    " Detect bounds of the template name as well as the name itself
    let info = jp:GetTemplateInfo ()

    " Generate the full template filename
    let templateFile = templateDir .'/'. ftDir . '/' . info['name']

    " Load the template file
    let template = jp:ReadTemplate (info['name'], templateFile)
  
    " Do the hard work: Process the template
    call jp:ProcessTemplate (info, template)
  catch
    " Inform the user about errors
    echo s:debug ? v:exception . " (in " . v:throwpoint . ")" : v:exception
  endtry

endfunction


" Map <Ctrl>+<Tab> to the template system
imap <C-Tab> <Esc>:call jp:InsertTemplate()<CR>
