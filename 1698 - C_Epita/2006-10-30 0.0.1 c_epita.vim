" c_epita.vim
"
" (c) Arnaud GODET 10.30.2006
" distribution under the GPL
"
" email : arnaud dot godet at gmail dot com
"

if exists("loaded_C_Epita") || exists("g:Epita_STOP") || &cp
  finish
endif

" Use for spacing
function! <SID>C_Epita_spacing()
  set cindent				" Indentation
  set cinoptions=c0,C1,(0		" Indetation Options
  set comments=s0:/*,mb:**,ex:*/,://	" Comments
  set textwidth=79			" 80 columns
  " Tab defines
  set shiftwidth=2
  set tabstop=8
  set smarttab
  set noexpandtab
  " Set config for the DoxygenToolkit plugin
  let g:DoxygenToolkit_startCommentTag	= "/**"
  let g:DoxygenToolkit_interCommentTag	= "** "
  let g:DoxygenToolkit_briefTag_pre	= "\\brief "
  let g:DoxygenToolkit_paramTag_pre	= "\\param "
  let g:DoxygenToolkit_returnTag	= "\\return "
  let g:DoxygenToolkit_fileTag		= "\\file "
  let g:DoxygenToolkit_authorTag	= "\\author "
  let g:DoxygenToolkit_dateTag		= "\\date "
  let g:DoxygenToolkit_blockTag		= "\\name "
  let g:DoxygenToolkit_classTag		= "\\class "
  let g:DoxygenToolkit_cinoptions	= "c0,C1"
endfunction

function! <SID>C_Epita_Headers_create()
 exec "normal O" . "/*"
  exec "normal o" . expand("%") . " for " . g:Epita_ProjectName . " in " . expand("%:p:h")
  exec "normal o"
  exec "normal o" . "Made by " . g:Epita_ProjectAuthors
  exec "normal o" . "Login   <" . g:Epita_ProjectLogin . "@epita.fr>"
  exec "normal o"
  let saved_language = v:lang
  language C
  exec "normal o" . "Started on  " . strftime("%c") . " " . g:Epita_ProjectAuthors
  exec "normal o" . "Last update " . strftime("%c") . " " . g:Epita_ProjectAuthors
  exec "language " . saved_language
  exec "normal o" . "/"
endfunction

function! <SID>C_Epita_Headers()
  if !exists("g:Epita_ProjectLogin")
    let g:Epita_ProjectLogin = $USER
  endif
  if !exists("g:Epita_ProjectAuthors")
    let g:Epita_ProjectAuthors = system('grep "^$USER:" /etc/passwd | cut -f 5 -d ":"')
  endif
  let saved_line = line('.')
  " find the Header && his lastline
  silent! normal! msHmtgg$%
  let lastline = line('.')
  if lastline != 9
    " Header not found
    if !exists("g:Epita_ProjectName")
      let g:Epita_ProjectName = input("Entrez le nom du PROJET : ")
    endif
    call <SID>C_Epita_Headers_create()
  else
    " Normal header
    silent! normal! k
    d
    let saved_language = v:lang
    language C
    exec "normal O" . "Last update " . strftime("%c") . " " . g:Epita_ProjectAuthors
    exec "language " . saved_language
  endif
  exec saved_line
endfunction

function! C_Epita()
  call <SID>C_Epita_spacing()
  "Redate file headers automatically 
  autocmd BufWritePre * call <SID>C_Epita_Headers()
endfunction

let c_syntax_for_h = 1
autocmd FileType c call C_Epita()
let loaded_C_Epita = 1
