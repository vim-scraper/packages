" Summary:     Insert command and output
" Description:
"         When you execute a command, its result is showing in the command line.
"         Sometimes you want to yank it, 
"         but unfortunately you can not edit the result in the command line.
"
"         What you need is to insert the command and its result after the current line, 
"         then you can edit them easily.
"
" Maintainer: Tian Huixiong: <nedzqbear@gmail.com>
"             I'm very glad to receive your feedback. 
" Licence:    This script is released under the Vim License.
" Version:    1.3
" Update:     2011-09-14 
" Install:     
"         Put this file in ~/.vim/plugin on *nix.
"         Or put it in $vim/vimfiles/plugin on Windows.
" Tutorial:
"         :Putcmd command
"
"         :Putcmd colo
"         :Putcmd set guifont?
"         :Putcmd set tabstop?


command! -nargs=+ -complete=command Putcmd call PutCmdMessage(<q-args>)

function! PutCmdMessage(cmd)
  redir => message
  silent execute a:cmd
  redir END

  "echo char2nr(message) 
  "call append(line('.'), substitute(message, '^\W*', '', ''))
  "call append(line('.'), substitute(message, '^\S', '', ''))
  "call append(line('.'), message)
  "call append(line('.'), ':' . a:cmd)

  let message = ':' . a:cmd . message
  let lines = split(message, "\n")
  let lines = reverse(lines)
  for line in lines
      call append(line('.'), line)
  endfor
endfunction

"function! s:TestLines()
   ""let result = 'abc\n123\nkkk\n'
   "let result = "abc\n123\nkkk\n"

    "let i = 0
    "let size = strlen(result)
    "let line = ''
    "let lines = []

    "while i < size
      "if result[i] != "\n"
        ""echo result[i]
        "let line .= result[i]
      "else
        "call insert(lines, line)
        "echo line
        "let line = ''
      "endif
      "let i += 1
    "endwhile

    "let line = ''
    "for line in lines
      "call append(line('.'), line)
    "endfor
"endfunction
