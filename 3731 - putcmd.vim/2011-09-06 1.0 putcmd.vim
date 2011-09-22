" Filename:    putcmd.vim
" Description: Put the command and its result in the current position.
" Last Change: 2011-09-06 
" Maintainer:  Tian Huixiong: <nedzqbear@gmail.com>
"              I'm very glad to receive your feedback. 
" Licence:     This script is released under the Vim License.
" Version:     1.0
" Install:     
"         Put this file in ~/.vim/plugin on *nix
"         Or put it in $vim/vimfiles/plugin on Windows
" Tutorial:
"         :Putcmd colo
"         :Putcmd set guifont?


command! -nargs=+ -complete=command Putcmd call PutCmdMessage(<q-args>)

function! PutCmdMessage(cmd)
  redir => message
  silent execute a:cmd
  redir END

  call setline(line('.'), ':' . a:cmd)
  call append(line('.'), substitute(message, '^\W*', '', ''))
endfunction
