" AnyPrinter
" Maintainer:	Adrien Pierard <pierarda@umontreal.ca>
" Version:	0.1
" Last Change:	05/05/07

" License:
" BSD

" Description: 
" This is a  simple script to quicky insert a print statement in your code.
" Currently supported languages are : 
"       C
"       Scheme, Lisp , OCaml/Omlet, Haskell
"       Java
" It also indents current line and puts the cursor where you expect it to be, in
" insert mode of course !

" Changelog:
"   0.1
"       File creation

" Install Details:
" Simply drop this file into your $HOME/.vim/plugin directory.

if exists("b:didAnyPrinter")
  norm :echo "already loaded"
  finish
endif

let b:didAnyPrinter = 1

" PrintCommand is the effective command that will be printed
let b:PrintCommandStart = ""
let b:PrintCommandEnd = ""

" Set the variables according to the filetype of the buffer
function! b:SetArgs()
  let fty = &ft

  " Functionnal languages
  if fty =~ '^\(ocaml\|omlet\)$'
    let b:PrintCommandStart = 'Printf.printf "'
    let b:PrintCommandEnd = '";;'
  elseif fty =~ '^scheme$'
    let b:PrintCommandStart = '(display '
    let b:PrintCommandEnd = ')'  
  elseif fty =~ '^lisp$'
    let b:PrintCommandStart = '(print '
    let b:PrintCommandEnd = ')'  
  elseif fty =~ '^\(haskell\)$'
    let b:PrintCommandStart = 'putStrLn "'
    let b:PrintCommandEnd = '";'  

  " Imperative languages
  elseif fty =~ '^c$'
    let b:PrintCommandStart = 'printf("'
    let b:PrintCommandEnd = '");'  

  " OO languages
  elseif fty =~ '^java$'
    let b:PrintCommandStart = 'System.out.println("'
    let b:PrintCommandEnd = '");'  


  " Else
  else
    let b:PrintCommandStart = 'NoSuchPrintfDefined('  
    let b:PrintCommandEnd = ')'
  endif
endfunction

" The function we were all waiting for !
function! PrintAPrint()
  execute 'norm a'. b:PrintCommandStart
  let shifter = indent(line("."))
  if shifter == -1 
    finish
  endif
  let [bb,ll,cc,oo]= getpos(".")
  execute 'norm a'. b:PrintCommandEnd
  norm ==
  let shifter = indent(line(".")) - shifter
  call setpos(".",[bb,ll,cc + shifter+1, oo])
  star
endfunction

" We have to map it, don't we ?
iab pap <Esc>:call PrintAPrint()<Cr>
nmap <Leader>P :call PrintAPrint()<Cr>

autocmd BufWinEnter,BufNewFile  * call b:SetArgs()
