" c_epita.vim
"
" (c) Arnaud GODET 11.1.2006
" distribution under the GPL
"
" email : arnaud dot godet at gmail dot com
" 
" C_Epita v 0.0.2

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
  exec "normal o" . expand("%:t") . " for " . g:Epita_ProjectName . " in " . expand("%:p:h")
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
  set textwidth=0
  if !exists("g:Epita_ProjectLogin")
    let g:Epita_ProjectLogin = $USER
  endif
  if !exists("g:Epita_ProjectAuthors")
    let g:Epita_ProjectAuthors = system('grep "^$USER:" /etc/passwd | cut -f 5 -d ":"')
  endif
  let saved_line = line('.')
  " find the Header && his lastline
  silent! normal! msHmtgg$%
  if line('.') != 9
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
  set textwidth=79
  exec saved_line
endfunction

function! UPPER(str)
  return substitute(a:str,"\\([a-z]\\)","\\U\\1","g")
endfunction

function! <SID>C_Epita_H_File()
  let h_var = substitute(UPPER(expand("%:t")),"[ .-]","_","g") . "_"
  let saved_line = line('.')
  " Find the header end
  silent! normal! msHmtgg$%
  call search(h_var)
  if line('.') == 9
    " Not found
    exec "normal o" . "#ifndef " . h_var
    exec "normal o" . "# define " . h_var
    " Goto the end of file
    silent! normal! L
    exec "normal o" . "#endif /* !" . h_var . " */"
  endif
  exec saved_line
endfunction

function! C_Epita_Count_line_Cur_func(echo)
  let saved_line = line('.')
  call search("^{","bcW")
  let begin = line('.')
  if begin == 1
    " not found
    finish
  endif
  call search("^}","cW")
  let ends = line('.')
  let nb_lines = ends - begin - 1
  if a:echo == 1
    if nb_lines >= 25
      echohl WarningMsg
      echo "Nb lines = " . nb_lines
      echohl None
    else
      echo "Nb lines = " . nb_lines
    endif
  elseif nb_lines > 25
    echohl WarningMsg
    echo "Function(" . begin . ":" . ends . ") have " . nb_lines . " lines"
    echohl None
  endif
  exec saved_line
endfunction

function! <SID>C_Epita_Count_line_funcs()
  let saved_line = line('.')
  silent! normal! msH
  let last = 1
  call search("^{","W")
  while last != line('.')
    call C_Epita_Count_line_Cur_func(0)
    let last = line('.')
    call search("^{","W")
  endwhile
  exec saved_line
endfunction

function! <SID>C_Epita_Trailing_White_Spaces()
  let saved_line = line('.')
  %s/[[:blank:]]*$//g
  exec saved_line
endfunction

function! C_Epita()
  call <SID>C_Epita_spacing()
  " Redate file headers automatically 
  autocmd BufWritePre * call <SID>C_Epita_Headers()
  autocmd BufWritePre * call <SID>C_Epita_Trailing_White_Spaces()
  " Set headers spec
  autocmd BufWritePre *.h call <SID>C_Epita_H_File()
  " Verify lines numbers of function
  autocmd BufWritePost *.c call <SID>C_Epita_Count_line_funcs()
  abbrev count_line_func call C_Epita_Count_line_Cur_func(1)
endfunction

let c_syntax_for_h = 1
autocmd FileType c call C_Epita()
let loaded_C_Epita = 1
