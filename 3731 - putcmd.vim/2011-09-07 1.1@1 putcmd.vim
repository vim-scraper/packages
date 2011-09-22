" Summary:     Insert command and output
" Description:
"         When you execute a command, its result is showing in the command line.
"         Unfortunately you can not yank or edit it.
"         By put the command and its result after the current line, then you can edit them easily. 
" Licence:     This script is released under the Vim License.
" Version:     1.1
" Last Change: 2011-09-07 
" Maintainer:  Tian Huixiong: <nedzqbear@gmail.com>
"              I'm very glad to receive your feedback. 
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

  call append(line('.'), substitute(message, '^\W*', '', ''))
  call append(line('.'), ':' . a:cmd)
endfunction
