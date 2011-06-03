" SchemeMode
" Maintainer:	Adrien 'Axioplase '<Axioplase@gmail.com>
" Version:	0.577
" Last Change:	07/06/07 

" License:
" BSD
" And please inform me if you fork it.

" Description: 
" This is a simple fileplugin supposed to help you write Scheme code fast.
" Comes with shortcuts for usual patterns such as `if`, `cond`, `case`, as 
" well as `let`s (normal, letrec and named let), `syntax-rules`, and eventually 
" a header generator and shortcuts to inser (print) lilke statements.
" Since many of those patterns have placeholders for code, `f and `s tags are 
" mapped to what can be thought of as 1st place and 2nd place. For example, in 
" an `if`, first place is the #t branch, and second place is the #f branch
"
" Little help with motions is given too. Mapped <C-l> and <C-h> to the next and
" previous Sexp, in depth parsing order.
"
" You are more than suggested to define the bindings as you want them.
" I chose <Leader>sX so that the "s" reminds you scheme, though is slows down
" typing quite a bit. Moreover, you might not want a 3 keyed shortcut to start a
" quoted list...
"
" Also comes with a function that closes all open brackets.

" Changelog:
"   0.5
"       File creation

"   0.57
"       Added Sexp motions

"   0.577
"       Added a part of AnyPrinter.vim as well as ScmCloseParen.vim

" Install Details:
" Simply drop this file into your $HOME/.vim/ftplugin/ directory and add to your
" vimrc file the following line :
" autocmd Filetype scheme source ~/.vim/ftplugin/SchemeMode.vim 
" 
" TODO :
" I don't know... You tell me
" Maybe "if next line begins by a closing parens, then do a J (that is, we
" we're in a nest)"
" Oh, yes ! Add breadth parsing (moving between Sexps).
"
" Bugs : it happens that scmCloseParens closes one more bracket than required

" Your name, for your projects
let s:MaintainerName = "Adrien 'Axioplase'"  
let s:MaintainerEmail = "axioplase@gmail.com"

if exists("g:didSchemeMode")
  "      norm :echo "Scheme mode already loaded"
  finish
endif

let g:didSchemeMode = 1

function! s:SM_MakeHeader(fname,kind)
  let name = a:fname
  let header = []

  if a:kind =~ '^long$'
    let header = [ 
          \ ';; Project:' . "\t" . name ,
          \ ';;',
          \ ';; Maintainer:' . "\t" . s:MaintainerName . " <" . s:MaintainerEmail . ">", 
          \ ';; Version:' . "\t ",
          \ ';; Last Change:' . "\t" . strftime("%d/%m/%y") , 
          \ ';; License:' . "\t" ,
          \ ';;',
          \ ';; Description:' , 
          \ ';;',
          \ ';; Changelog:' , 
          \ ';;',
          \ ';; Install Details:' , 
          \ ';;',
          \ ';; TODO:' ,
          \ ';;',
          \ ';; Known Bugs:' ,
          \ ';;']
  elseif a:kind =~ '^short$'
    let header = [ 
          \ ';; Project:' . "\t" . name ,
          \ ';; Version:' . "\t ",
          \ ';; Description:']
  else
    let header = ["Ho NO !", a:kind]
  endif
  return header
endfunction

" name of project will be set according to the buffer name, minus its extension
function! SM_PrintHeader(kind)
  call setpos(".",[0,1,1,0])
  let hdr = s:SM_MakeHeader(fnamemodify(bufname("%"),":t:r"),a:kind)

  "We don't want vim to add comments by itself
  let b:oldcomments = &comments
  set comments = 
  for sentence in hdr
    execute 'norm i'. sentence . "\n"
  endfor
  let &comments = b:oldcomments
  " set cursor at the right position
  if a:kind =~ '^long$'
    call setpos(".",[0,4,17,0])
  elseif a:kind =~ '^short$'
    call setpos(".",[0,2,17,0])
  else
    call setpos(".",[0,1,1,0])
  endif
  star
endfunction


" I pattern and transform are on two lines so that mark stays after completion
" of first place.
" Closing parens are on new line in order to ease copy/paste in case of multi
" replacement forms. J is your friend
function! SM_DefineSyntax()
  let output = [
        \ "(define-syntax ",
        \ "(syntax-rules()",
        \ "((_ )",
        \ "())",
        \ "))" ]
  for sentence in output
    norm 0
    execute 'norm i'. sentence . "\n"
    norm k==j==
  endfor
  norm kk
  " set marks 
  norm 0f(lmsk0f_llmfkk
  star!
endfunction


function! SM_DefineMacro()
  let output = [
        \ "(define-macro ",
        \ ")" ]
  for sentence in output
    norm 0
    execute 'norm i'. sentence . "\n"
    norm k==j==
  endfor
  norm kk
  " set marks 
  norm 0f)ms%folmf
  star!
endfunction


function! SM_IfThenElse(full)
  if a:full == 1
    execute 'norm i(if ' 
    norm ==
    execute 'norm A'."\n \n"
    norm k$mfj
    execute 'norm i)'."\n"
    norm k==mskk
    star!
  elseif a:full == 0
    execute 'norm i(if ' . "\n"
    execute 'norm i)' 
    norm k==j==msmf
    execute 'norm a' . "\n"
    norm kk
    star!
  else
    norm aerreur
  endif
endfunction

function! SM_CaseOrCond(kind)
  let output=[]

  if a:kind =~ '^case$'
    let output = [
          \ "(case ",
          \ "(() )",
          \ "(else ))"]
  elseif a:kind =~ '^cond$'
    let output = [
          \ "(cond",
          \ "(() )",
          \ "(else ))"]
  else
    let output = ["(error 'neither case nor cond')"]
  endif

  for sentence in output
    execute 'norm i'. sentence . "\n"
    norm k==j==
  endfor
  norm k
  " set marks 
  if a:kind =~ '^case$'
    norm $hmsk0f(llmfk$
    star!
  elseif a:kind =~ '^cond$'
    norm $hmsk0f(llmf
    star
  endif
endfunction

function! SM_QuoteOrList(kind)
  let output=""

  if a:kind =~ '^list$'
    let output = "(list "
  elseif a:kind =~ '^quote$'
    let output = "'("
  elseif a:kind =~ '^quasiquote$'
    let output = "`("
  else
    let output = "(error 'neither quote nor quasiquote nor list')"
  endif

  execute 'norm i'. output ."\n)"
  norm %J
  " set marks 
  norm msmf
  star
endfunction

function! SM_Define()
  let output = [
        \ "(define ",
        \ "(lambda()",
        \ "))" ]
  for sentence in output
    norm 0
    execute 'norm i'. sentence . "\n"
    norm k==j==
  endfor
  norm k
  " set marks 
  norm f)msk$mfk$
  star!
endfunction

function! SM_NamedLet()
  let output = [
        \ "(let ",
        \ "(())",
        \ ")" ]
  for sentence in output
    execute 'norm i'. sentence . "\n"
    norm k==j==
  endfor
  norm k
  " set marks 
  norm $msk0f)mfk
  star!
endfunction

function! SM_Let(rec)
  if a:rec == 0
    let output = [
          \ "(let (())",
          \ ")" ]
  elseif a:rec == 1
    let output = [
          \ "(letrec (())",
          \ ")" ]
  else
    let output = ["Unknown let"]
  endif
  for sentence in output
    execute 'norm i'. sentence . "\n"
    norm k==j==
  endfor
  norm k
  " set marks 
  norm $msmf%f)
  star
endfunction

function! SM_Begin()
  execute 'norm i' . "(begin\n)"
  " set marks 
  norm k==j==msmf
  star
endfunction

" Depth parsing
function! SM_NextSexp()
  call search("(","c")
  norm %%
  call search("(","")
endfunction

function! SM_PreviousSexp()
  call search("(","bc")
  call search("(","b")
endfunction

" Please write breadth parsing !


"printers
function! SM_Printer(kind,output)
  let printer = ""
  let port = ""
  if a:kind =~ '^\(pp\|print\|display\|write\)$' 
    let printer = a:kind . " "
  endif
  if a:output
    let port = " (current-output-port)"
  endif
  execute 'norm i(' . printer . port . ')'
  norm %Ell
  star
endfunction


"from scmCloseParens
function! SetCursorWhereItIsGoodToPutItEh()
  let line = substitute(getline("."), "\\s\\+$", "", "")
  call setline(line("."),line)
  norm $
  let charUnderCursor = strpart(line("."), col(".") - 1, 1)
  norm a)
  call CountAsMuchAsPossible()
endfunction

function! CountAsMuchAsPossible()
  let cpt = 0
  while (CanWeGoOn() > 0)
    let cpt = cpt + 1
    call OhGetBackAndSetAnotherOne()
  endwhile
  let line = substitute(getline("."), ")$", "", "")
  call setline(line("."),line)
  let b:cpt = cpt
endfunction

function! CanWeGoOn()
  return (searchpair('(', '', ')' , 'b' ))
endfunction  

function! OhGetBackAndSetAnotherOne()
  call searchpair('(', '', ')')
  norm a)
endfunction  

fun! RunScmCloseParens()
  call SetCursorWhereItIsGoodToPutItEh()
endfunction

" We have to map it, don't we ?
" This is just a suggestion, you'd better just use the ones you use most, and
" give em shorter mappings.
"
"nmap <Leader>sH :call SM_PrintHeader("long")<Cr>
"nmap <Leader>sh :call SM_PrintHeader("short")<Cr>
"nmap <Leader>sD :call SM_DefineSyntax()<Cr>
"nmap <Leader>sm :call SM_DefineMacro()<Cr>
"nmap <Leader>si :call SM_IfThenElse(1)<Cr>
"nmap <Leader>sI :call SM_IfThenElse(0)<Cr>
"nmap <Leader>sc :call SM_CaseOrCond("case")<Cr>
"nmap <Leader>sC :call SM_CaseOrCond("cond")<Cr>
"nmap <Leader>sq :call SM_QuoteOrList("quote")<Cr>
"nmap <Leader>sQ :call SM_QuoteOrList("quasiquote")<Cr>
"nmap <Leader>sl :call SM_QuoteOrList("list")<Cr>
"nmap <Leader>sd :call SM_Define()<Cr>
"nmap <Leader>sn :call SM_NamedLet()<Cr>
"nmap <Leader>sL :call SM_Let(0)<Cr>
"nmap <Leader>sr :call SM_Let(1)<Cr>
"nmap <Leader>sb :call SM_Begin()<Cr>

"map <C-l> :call SM_NextSexp()<Cr>
"map <C-h> :call SM_PreviousSexp()<Cr>

"nmap <Leader>sp :call SM_Printer("pp",1)<Cr>
"nmap <Leader>sP :call SM_Printer("display",0)<Cr>

"nmap <Leader>sk :call RunScmCloseParens()<Cr>

"Since we use f and s as marks to go to area to be filled in insert mode, why
"not remap the motion directly to motion + insert ?
"nnoremap `f `fi
"nnoremap `s `si
