" Vim filetype plugin
" Language:	LaTeX
" Maintainer: Carl Mueller, cmlr@math.rochester.edu
" Last Change:	July 25, 2002
" Version:  1.6
" Website:  http://www.math.rochester.edu/u/cmlr/vim/syntax/index.html

" Auctex-style macros for Latex typing.
" You will have to customize the functions RunLatex(), Xdvi(), 
" and the maps for inserting template files, on lines 141 - 144.

" Thanks to Peppe Guldberg for important suggestions.

" Switch to the directory of the tex file.  Thanks to Fritz Mehner.
" This is useful for starting xdvi, or going to the next tex error.
" autocmd BufEnter *.tex :cd %:p:h

" The following is necessary for TexFormatLine() and TexFill()
set tw=0
" substitute text width
let b:tw = 79

" Run Latex
noremap <buffer> K :call <SID>RunLatex()<CR><Esc>
noremap <buffer> <C-Tab> :call <SID>RunLatex()<CR><Esc>
inoremap <buffer> <C-Tab> <Esc>:call <SID>RunLatex()<CR><Esc>

" Emacs-type bindings;  feel free to delete these, you vi purists!
inoremap <buffer> <C-A> <Home>
inoremap <buffer> <C-B> <Left>
inoremap <buffer> <C-D> <Del>
inoremap <buffer> <C-E> <End>
inoremap <buffer> <C-F> <Right>
inoremap <buffer> <C-K> <C-R>=EmacsKill()<CR>
inoremap <buffer> <C-L> <C-O>zz
inoremap <buffer> <C-N> <Down>
inoremap <buffer> <C-P> <Up>
inoremap <buffer> <C-Y> <C-R>"

function! EmacsKill()
    if col(".") == strlen(getline(line(".")))+1
	let @" = "\<CR>"
	return "\<Del>"
    else
	return "\<C-O>D"
    endif
endfunction

" Due to Ralf Aarons
" In normal mode, gw formats the paragraph (without splitting dollar signs).
function! s:TeX_par()
    if (getline(".") != "")
        let par_begin = '^$\|^\s*\\end{\|^\s*\\\]'
        let par_end = '^$\|^\s*\\begin{\|^\s*\\\['
        call search(par_begin, 'bW')
        "call searchpair(par_begin, '', par_end, 'bW')
        +
	let l = line(".")
        normal V
        call search(par_end, 'W')
        "call searchpair(par_begin, '', par_end, 'W')
        -
	if l == line(".")
	    normal 
	endif
	normal Q
    endif
endfun
map <buffer> gw :call <SID>TeX_par()<CR>

function! s:RunLatex()
    update
    ! xterm -bg ivory -fn 7x14 -e latex % &
"    ! latex %
endfunction

" Find Latex Errors
" To find the tex error, first run Latex (see the 2 previous maps).
" If there is an error, press "x" or "r" to stop the Tex processing.
" Then press Shift-Tab to go to the position of the error.
noremap <buffer> <S-Tab> :call <SID>NextTexError()<CR><Space>
inoremap <buffer> <S-Tab> <Esc>:call <SID>NextTexError()<CR><Space>

" Stop warnings, since the log file is modified externally and then 
" read again.
au BufRead *.log    set bufhidden=unload

function! s:NextTexError()
    only
    edit +1 %<.log
    if search('^l\.\d') == 0
	edit #
	redraw
	call input("\nNo (More) Errors Found\n\nPress 'enter' to go on.")
    else
	let linenumber = matchstr(getline('.'), '\d\+')
	let errorposition = col("$") - strlen(linenumber) - 4
	    "Put a space in the .log file so that you can see where you were,
	    "and move on to the next latex error.
	s/^/ /
	write
	split #
	exe "normal " . linenumber . "G" . errorposition . "lzz\<C-W>wzz\<C-W>w"
    endif
endfunction

noremap <buffer> <F9> <C-W>o
inoremap <buffer> <F9> <C-O><C-W>o

" Run xdvi
noremap <buffer> <M-Tab> :call <SID>Xdvi()<CR><Space>
inoremap <buffer> <M-Tab> <Esc>:call <SID>Xdvi()<CR><Space>
function! s:Xdvi()
    update
    ! xterm -bg ivory -fn 7x14 -e latex %
    ! xterm -bg ivory -fn 7x14 -e latex %
"    ! latex %
"    ! latex %
    ! xdvi -expert -s 6 -margins 2cm -geometry 750x950 %< &
"    ! xdvi %< &
endfunction

" Run Ispell (Thanks the Charles Campbell)
" The first set is for vim, the second set for gvim.

"noremap <buffer> <S-Insert> :w<CR>:!ispell %<CR><Space>:e %<CR><Space>
"inoremap <buffer> <S-Insert> <Esc>:w<CR>:!ispell %<CR><Space>:e %<CR><Space>
"vnoremap <buffer> <S-Insert> <C-C>'<c'><Esc>:e ispell.tmp<CR>p:w<CR>:!ispell %<CR><CR>:e %<CR><CR>ggddyG:bwipeout!<CR>:!rm ispell.tmp*<CR><Esc>pkdd
"vnoremap <buffer> <S-Insert> <C-C>`<v`>s<Space><Esc>mq:e ispell.tmp<CR>i<C-R>"<Esc>:w<CR>:!ispell %<CR><CR>:e %<CR><CR>ggVG<Esc>`<v`>s<Esc>:bwipeout!<CR>:!rm ispell.tmp*<CR>`q"_s<C-R>"<Esc>

noremap <S-Insert> :w<CR>:! xterm -bg ivory -fn 10x20 -e ispell %<CR><Space>:e %<CR><Space>
inoremap <S-Insert> <Esc>:w<CR>:! xterm -bg ivory -fn 10x20 -e ispell %<CR><Space>:e %<CR><Space>
"vnoremap <S-Insert> <C-C>'<c'><Esc>:e ispell.tmp<CR>p:w<CR>:! xterm -bg ivory -fn 10x20 -e ispell %<CR><CR>:e %<CR><CR>ggddyG:bwipeout!<CR>:!rm ispell.tmp*<CR><Esc>pkdd
vnoremap <S-Insert> <C-C>`<v`>s<Space><Esc>mq:e ispell.tmp<CR>i<C-R>"<Esc>:w<CR>:! xterm -bg ivory -fn 10x20 -e ispell %<CR><CR>:e %<CR><CR>ggVG<Esc>`<v`>s<Esc>:bwipeout!<CR>:!rm ispell.tmp*<CR>`q"_s<C-R>"<Esc>

" In normal mode, F1 inserts a latex template.
" F2 inserts a minimal latex template
" F3 inserts a letter template
" F4 inserts an exam template
map <buffer> <F1> :if strpart(getline(1),0,9) != "\\document"<CR>0read ~/.Vim/latex<CR>normal 23j$<CR>endif<Esc><Space>i
map <buffer> <F2> :if strpart(getline(1),0,9) != "\\document"<CR>0read ~/.Vim/min-latex<CR>normal 4j$<CR>endif<Esc><Space>i
map <buffer> <F3> :inoremap <buffer> . .  <CR>:inoremap <buffer> ? ?  <CR>:if strpart(getline(1),0,9) != "\\document"<CR>0read ~/.Vim/letter<CR>normal 9jt}<CR>endif<Esc><Space>i
map <buffer> <F4> :if strpart(getline(1),0,9) != "\\document"<CR>!cp ~/Storage/Latex/urmathexam.sty urmathexam.sty<CR>0read ~/Storage/Latex/exam.tex<CR>endif<Esc><Space>

"       dictionary
" set dictionary+=(put filename and path here)

" Boldface:  (2 alternative macros)
" In the first, \mathbf{} appears.
" In the second, you type the letter, and it capitalizes the letter
" and surrounds it with \mathbf{}
" inoremap <buffer> <M-b> \mathbf{}<Left>
inoremap <buffer> <M-b> <Left>\mathbf{<Right>}<Esc>hvUla
vnoremap <buffer> <M-b> <C-C>`>a}<Esc>`<i\mathbf{<Esc>

" Cal:  (3 alternative macros:  see Boldface)
" The third one also inserts \cite{}, if the previous character is a blank
" inoremap <buffer> <M-c> \mathcal{}<Left>
" inoremap <buffer> <M-c> <Left>\mathcal{<Right>}<Esc>h~a
inoremap <buffer> <M-c> <C-R>=<SID>MathCal()<CR>
function! s:MathCal()
    if getline(line("."))[col(".")-2] =~ "[a-zA-Z0-9]"
	return "\<Left>\\mathcal{\<Right>}\<Esc>hvUla"
    else
	return "\\cite{}\<Left>"
    endif
endfunction
vnoremap <buffer> <M-b> <C-C>`>a}<Esc>`<i\mathcal{<Esc>

"  This macro asks the user for the input to \mathbf{}
"inoremap <buffer> <M-b> <C-R>=<SID>WithBrackets("mathbf")<CR>
"function! s:WithBrackets(string)
"    let user = input(a:string . ": ")
"    return "\\" . a:string . "{" . user . "}"
"endfunction

" This is for _{}^{}.  It moves you to the second parenthesis.
inoremap <buffer> <M-f> <Esc>f{a

" Alt-h inserts \widehat{}
inoremap <buffer> <M-h> \widehat{}<Left>

" Alt-i inserts \item
inoremap <buffer> <M-i> \item

" Alt-m inserts \mbox{}
inoremap <buffer> <M-m> \mbox{}<Left>
vnoremap <buffer> <M-t> <C-C>`>a}<Esc>`<i\mbox{<Esc>

" Alt-o inserts \overline{}
inoremap <buffer> <M-o> \overline{}<Left>

" Alt-r inserts (\ref{})
" There are 2 versions.
" The second one inserts \ref{} if the preceding word is Lemma or the like.
"inoremap <buffer> <M-r> (\ref{})<Left><Left>
inoremap <buffer> <M-r> <C-R>=<SID>TexRef()<CR><Esc>F{a
function! s:TexRef()
    let insert = '(\ref{})'
    let lemma = strpart(getline(line(".")),col(".")-7,6)
    if lemma =~# 'Lemma \|emmas \|eorem \|llary '
	let insert = '\ref{}'
    endif
    return insert
endfunction

" Alt-s inserts \sqrt{}
inoremap <buffer> <M-s> \sqrt{}<Left>

" Textbf
inoremap <buffer> <M-t> \textbf{}<Left>
vnoremap <buffer> <M-t> <C-C>`>a}<Esc>`<i\textbf{<Esc>

" This is for _{}^{}.  It gets rid of the ^{} part
inoremap <buffer> <M-x> <Esc>f^cf}

" Alt-u inserts \underline
inoremap <buffer> <M-u> \underline{}<Left>

" Alt-- (Alt-minus) inserts _{}^{}
inoremap <buffer> <M--> _{}^{}<Esc>3hi

" Alt-; inserts \dot{}
inoremap <buffer> <M-;> \dot{}<Left>

" Alt-<Right> inserts \lim_{}
inoremap <buffer> <M-Right> \lim_{}<Left>

" Smart quotes.  Thanks to Ron Aaron <ron@mossbayeng.com>.
function! s:TexQuotes()
    let insert = "''"
    let left = getline(line("."))[col(".")-2]
    if left =~ '^\(\|\s\)$'
	let insert = '``'
    elseif left == '\'
	let insert = '"'
    endif
    return insert
endfunction
inoremap <buffer> " <C-R>=<SID>TexQuotes()<CR>

" Typing ... results in \dots
"function! s:Dots()
"    let left = strpart(getline(line(".")),col(".")-3,2)
"    if left == ".."
"	return "\<BS>\<BS>\\dots"
"    else
"       return "."
"    endif
"endfunction
" Use this if you want . to result in a period followed by 1 space.
"function! s:Dots()
"    let column = col(".")
"    let currentline = getline(line("."))
"    let previous = currentline[column-2]
"    if strpart(currentline,column-3,2) == ". "
"	return "\<BS>"
"    elseif previous == '.'
"	return "\<BS>\\dots"
"    elseif previous =~ '[\$A-Za-z]' && currentline !~ "@"
"	return ". "
"    else
"	return "."
"    endif
"endfunction
" Use this if you want . to result in a period followed by 2 spaces.
function! s:Dots()
    let column = col(".")
    let currentline = getline(line("."))
    let previous = currentline[column-2]
    if strpart(currentline,column-4,3) == ".  "
	return "\<BS>\<BS>"
    elseif previous == '.'
	return "\<BS>\\dots"
    elseif previous =~ '[\$A-Za-z]' && currentline !~ "@"
	return ".  "
    else
	return "."
    endif
endfunction
inoremap <buffer> . <C-R>=<SID>Dots()<CR>
inoremap <buffer> <M-.> .

" Typing __ results in _{}
function! s:SubBracket()
    let insert = "_"
    let left = getline(line("."))[col(".")-2]
    if left == '_'
	let insert = "{}\<Left>"
    endif
    return insert
endfunction
inoremap <buffer> _ <C-R>=<SID>SubBracket()<CR>

" Typing ^^ results in ^{}
function! s:SuperBracket()
    let insert = "^"
    let left = getline(line("."))[col(".")-2]
    if left == '^'
	let insert = "{}\<Left>"
    endif
    return insert
endfunction
inoremap <buffer> ^ <C-R>=<SID>SuperBracket()<CR>

" Since " is not available, <Alt-'> gives it.
inoremap <buffer> <M-'> "

" Begin-End
" F1 - F5 inserts various environments (insert mode).
" Ctrl-F1 through Ctrl-F5 replaces the current environment
"     with \begin-\end{equation} through \begin-\end{}.
inoremap <buffer> <F1> \begin{equation}<CR>\label{}<CR><CR>\end{equation}<Esc>2k$i
inoremap <buffer> <F3> \begin{eqnarray}<CR>\label{}<CR><CR>\end{eqnarray}<Esc>2k$i
inoremap <buffer> <F4> \begin{eqnarray*}<CR><CR>\end{eqnarray*}<Up>
inoremap <buffer> <F6> \left\{\begin{array}{ll}<CR>&\mbox{$$} \\<CR>&\mbox{}<CR>\end{array}<CR>\right.<Up><Up><Up><Home>
inoremap <buffer> <F7> \noindent<CR>\textbf{Proof.}<CR><CR><CR>\qed<Up><Up>

" The following are for $$..$$ in place of \[..\]
"noremap <buffer> <C-F1> /\\end\\|\$\$<CR>S\end{equation}<Esc>v?\\begin\\|\$\$<CR><Esc>S\begin{equation}<Esc>:'<,'>s/&\\|\\lefteqn{\\|\\nonumber\\|\\\\//e<CR>`<:call <SID>PutInLabel()<CR>a
"inoremap <buffer> <C-F1> <Esc>/\\end\\|\$\$<CR>S\end{equation}<Esc>v?\\begin\\|\$\$<CR><Esc>S\begin{equation}<Esc>:'<,'>s/&\\|\\lefteqn{\\|\\nonumber\\|\\\\//e<CR>`<:call <SID>PutInLabel()<CR>a
"inoremap <buffer> <F2> $$<CR><CR>$$<Up>
"noremap <buffer> <C-F2> /\\end<CR>S$$<Esc>v?\\begin<CR><Esc>S$$<Esc>:'<,'>s/&\\|\\lefteqn{\\|\\nonumber\\|\\\\//e<CR>:'<+1g/\\label/delete<CR>
"inoremap <buffer> <C-F2> <Esc>/\\end<CR>S$$<Esc>v?\\begin<CR><Esc>S$$<Esc>:'<,'>s/&\\|\\lefteqn{\\|\\nonumber\\|\\\\//e<CR>:'<+1g/\\label/delete<CR>
"noremap <buffer> <C-F3> /\\end\\|\$\$<CR>S\end{eqnarray}<Esc>v?\\begin\\|\$\$<CR><Esc>S\begin{eqnarray}<Esc>:call <SID>PutInNonumber ()<CR>`<:call <SID>PutInLabel ()<CR>a
"inoremap <buffer> <C-F3> <Esc>/\\end\\|\$\$<CR>S\end{eqnarray}<Esc>v?\\begin\\|\$\$<CR><Esc>S\begin{eqnarray}<Esc>:call <SID>PutInNonumber ()<CR>`<:call <SID>PutInLabel ()<CR>a
"noremap <buffer> <C-F4> /\\end\\|\$\$<CR>S\end{eqnarray*}<Esc>v?\\begin\\|\$\$<CR><Esc>S\begin{eqnarray*}<Esc>:'<,'>s/\\nonumber//e<CR>:'<+1g/\\label/delete<CR>
"inoremap <buffer> <C-F4> <Esc>/\\end\\|\$\$<CR>S\end{eqnarray*}<Esc>v?\\begin\\|\$\$<CR><Esc>S\begin{eqnarray*}<Esc>:'<,'>s/\\nonumber//e<CR>:'<+1g/\\label/delete<CR>

" Uncomment this function if you use the alternate mappings for <C-F1>, etc.
"function! s:PutInLabel()
"   if (-1 == match(getline(line(".")+1),"\\label"))
"       put ='label{}'
"       normal $h
"   endif
"endfunction

noremap <buffer> <C-F1> :call <SID>Change('equation', 1, '&\\|\\lefteqn{\\|\\nonumber\\|\\\\', 0)<CR>i
inoremap <buffer> <C-F1> <Esc>:call <SID>Change('equation', 1, '&\\|\\lefteqn{\\|\\nonumber\\|\\\\', 0)<CR>i
inoremap <buffer> <F2> \[<CR><CR>\]<Up>
noremap <buffer> <C-F2> :call <SID>Change('[', 0, '&\\|\\lefteqn{\\|\\nonumber\\|\\\\', 0)<CR><Esc>
inoremap <buffer> <C-F2> <Esc>:call <SID>Change('[', 0, '&\\|\\lefteqn{\\|\\nonumber\\|\\\\', 0)<CR><Esc>
noremap <buffer> <C-F3> :call <SID>Change('eqnarray', 1, '', 1)<CR>i
inoremap <buffer> <C-F3> <Esc>:call <SID>Change('eqnarray', 1, '', 1)<CR>i
noremap <buffer> <C-F4> :call <SID>Change('eqnarray*', 0, '\\nonumber', 0)<CR><Esc>
inoremap <buffer> <C-F4> <Esc>:call <SID>Change('eqnarray*', 0, '\\nonumber', 0)<CR><Esc>

function! s:Change(env, label, delete, putInNonumber)
    if a:env == '['
	let first = '\\['
	let second = '\\]'
    else
	let first = '\\begin{' . a:env . '}'
	let second = '\\end{' . a:env . '}'
    endif
    let bottom = searchpair('\\\[\|\\begin{','','\\\]\|\\end{','')
    s/\\\]\|\\end{.\{-}}/\=second/
    let top = searchpair('\\\[\|\\begin{','','\\\]\|\\end{','b')
    s/\\\[\|\\begin{.\{-}}/\=first/
    if a:delete != ''
	exe top . "," . bottom . 's/' . a:delete . '//e'
    endif
    if a:putInNonumber == 1
        exe top
	call search('\\end\|\\\\')
	if line(".") != bottom
	    exe '.+1,' . bottom . 's/\\\\/\\nonumber\\\\/e'
	    exe (bottom-1) . 's/\s*$/  \\nonumber/'
	endif
    endif
    if a:label == 1
	exe top
	if getline(top+1) !~ '.*label.*'
	    put ='\label{}'
	    normal $
	endif
    else
	exe top . ',' . bottom . 'g/\\label/delete'
    endif
endfunction

" The next idea came from a contributed NEdit macro.
" typing the name of the environment followed by <F5> results in 
" \begin{environment} \end{environment}
" But, typing <F5> at the beginning of the line results in a prompt
" for the name of the environment.
inoremap <buffer> <F5> <Esc>:call <SID>DoEnvironment()<CR>

" Due to Ralf Arens <ralf.arens@gmx.net>
"inoremap <buffer> <F5> <C-O>:call <SID>PutEnvironment(input("Environment? "))<CR>
inoremap <buffer> <C-F5> <C-O>:call <SID>ChangeEnvironment(input("Environment? "))<CR>
noremap <buffer> <C-F5> :call <SID>ChangeEnvironment(input("Environment? "))<CR>

function! s:DoEnvironment()
    let l = getline(line("."))
    let env = strpart(l, 0, col("."))
    if env =~ '^\s*$'
	call <SID>PutEnvironment(l, input("Environment? "))
    else
	normal 0D
	call <SID>SetEnvironment(env)
    endif
    startinsert
endfunction

" The following function was improved by Peppe Guldberg and Torsten Wolf.
function! s:SetEnvironment(env)
  let indent = strpart(a:env, 0, match(a:env, '\S'))
  let env = strpart(a:env, strlen(indent))
  put! =indent . '\begin{' . env . '}'
  +put =indent . '\end{' . env . '}'
  -s/^/\=indent/
  norm $
  if env == "array"
    -s/$/{}/
    " The "$hl" magic here places the cursor at the last character
    " and not after it as "$" would.
    norm $hl
  elseif env =~# '^\(theorem\|lemma\|equation\|eqnarray\|align\|multline\)$'
    put!=indent . '\label{}'
    normal f}
  endif
  startinsert
endfunction

" The following function was improved by Peppe Guldberg and Torsten Wolf.
function! s:PutEnvironment(indent, env)
  put! =a:indent . '\begin{' . a:env . '}'
  +put =a:indent . '\end{' . a:env . '}'
  normal k$
  if a:env=="array"
    call <SID>ArgumentsForArray(input("{rlc}? "))
  elseif a:env =~# '^\(theorem\|lemma\|equation\|eqnarray\|align\|multline\)$'
    exe "normal O\\label\<C-V>{" . input("Label? ") . "}\<Esc>j"
  endif
endfunction

function! s:ArgumentsForArray(arg)
    put! = '{' . a:arg . '}'
    normal kgJj
endfunction

" Quote or unquote lines, depending on whether you use $$..$$ or \[..\]
function! s:ChangeEnvironment(env)
    call searchpair('\\\[\|\\begin{','','\\\]\|\\end{','')
    let l = getline(line("."))
    let indent = strpart(l, 0, match(l, '\S'))
    s/\\\]\|\\end{.\{-}}/\='\\end{' . a:env . '}'/
    call searchpair('\\\[\|\\begin{','','\\\]\|\\end{','b')
    s/\\\[\|\\begin{.\{-}}/\='\\begin{' . a:env . '}'/
    +
    if a:env =~# '^\(theorem\|lemma\|equation\|eqnarray\|align\|multline\)$'
	if (-1 == match(getline(line(".")),"\\label"))
	    let label = input("Label? ")
	    if label != ''
		put! = indent . '\label{' . label . '}'
	    endif
	endif
    elseif a:env[strlen(a:env)-1] == '*'
	if (-1 != match(getline(line(".")),"\\label"))
	    delete
	endif
    endif
    echo ''
endfunction

function! s:PutInNonumber()
   call search('\\end\|\\\\')
   if getline(line("."))[col(".")] != "e"
       .+1,'>s/\\\\/\\nonumber\\\\/e
       normal `>k
       s/\s*$/  \\nonumber/
   endif
endfunction

" Left-right (3 alternatives)
" In the first version, you type Alt-L and then the bracket
" In the second version, you type the bracket, and then Alt-L
" The second version matches with the Alt-B and Alt-C macros,
" if you use the uncommented versions.  It also doubles as a command
" for inserting \label{}
" The third version is like the second version.  Use it if you have
" disabled automatic bracket completion.
"inoremap <buffer> <M-l>( \left(\right)<Left><Left><Left><Left><Left><Left><Left>
"inoremap <buffer> <M-l>\| \left\|\right\|<Left><Left><Left><Left><Left><Left><Left>
"inoremap <buffer> <M-l>[ \left[\right]<Left><Left><Left><Left><Left><Left><Left>
"inoremap <buffer> <M-l>{ \left\{\right\}<Left><Left><Left><Left><Left><Left><Left><Left>
"inoremap <buffer> <M-l>< \langle\rangle<Left><Left><Left><Left><Left><Left><Left>
"inoremap <buffer> <M-l>q \lefteqn{
function! s:LeftRight()
    let line = getline(line("."))
    let char = line[col(".")-1]
    let previous = line[col(".")-2]
    if char =~ '(\|\['
        exe "normal i\\left\<Esc>la\\right\<Esc>6h"
    elseif char == '|'
	if previous == '\'
	    exe "normal ileft\\\<Esc>"
	else
	    exe "normal i\\left\<Esc>"
	endif
	exe "normal la\\right\<Esc>6h"
    elseif char == '{'
	if previous == '\'
	    exe "normal ileft\\\<Esc>la\\right\<Esc>6h"
	else
	    exe "normal i\\left\\\<Esc>la\\right\\\<Esc>7h"
	endif
    elseif char == '<'
	exe "normal s\\langle\\rangle\<Esc>7h"
    elseif char == 'q'
	exe "normal s\\lefteqn\<C-V>{\<Esc>"
    else
	exe "normal a\\label\<C-V>{}\<Esc>h"
    endif
endfunction
inoremap <buffer> <M-l> <Esc>:call <SID>LeftRight()<CR>a
noremap <buffer> <M-l> :call <SID>PutLeftRight()<CR>
vnoremap <buffer> <M-l> <C-C>`>a\right<Esc>`<i\left<Esc>
"function! s:LeftRight()
"let char = getline(line("."))[col(".")-1]
"let previous = getline(line("."))[col(".")-2]
"if char == '('
"	exe "normal i\\left\<Esc>la\\right)\<Esc>7h"
"elseif char == '['
"	exe "normal i\\left\<Esc>la\\right]\<Esc>7h"
"elseif char == '|'
"	if previous == '\'
"		exe "normal ileft\\\<Esc>la\\right\\\|\<Esc>8h"
"	else
"		exe "normal i\\left\<Esc>la\\right\|\<Esc>7h"
"	endif
"elseif char == '{'
"	if previous == '\'
"		exe "normal ileft\\\<Esc>la\\right\\}\<Esc>8h"
"	else
"		exe "normal i\\left\\\<Esc>la\\right\\}\<Esc>8h"
"	endif
"elseif char == '<'
"	exe "normal s\\langle\\rangle\<Esc>7h"
"elseif char == 'q'
"	exe "normal s\\lefteqn{\<Esc>lx"
"endif
"endfunction

" Bracket Completion Macros
" Typing the symbol a second time (for example, $$) will result in one
" of the symbole (for instance, $).  With {, typing \{ will result in \{\}.
inoremap <buffer> ( <C-R>=<SID>Double("(",")")<CR>
"inoremap <buffer> [ <C-R>=<SID>Double("[","]")<CR>
inoremap <buffer> [ <C-R>=<SID>CompleteSlash("[","]")<CR>
inoremap <buffer> $ <C-R>=<SID>Double("$","$")<CR>
inoremap <buffer> & <C-R>=<SID>DoubleAmpersands()<CR>
inoremap <buffer> { <C-R>=<SID>CompleteSlash("{","}")<CR>
inoremap <buffer> \| <C-R>=<SID>CompleteSlash("\|","\|")<CR>

" If you would rather insert $$ individually, the following macro by 
" Charles Campbell will make the cursor blink on the previous dollar sign,
" if it is in the same line.
" inoremap $ $<C-O>F$<C-O>:redraw!<CR><C-O>:sleep 500m<CR><C-O>f$<Right>

" For () and $$
function! s:Double(left,right)
    if strpart(getline(line(".")),col(".")-2,2) == a:left . a:right
	return "\<Del>"
    else
	return a:left . a:right . "\<Left>"
    endif
endfunction

" Complete [, \[, {, \{, |, \|
function! s:CompleteSlash(left,right)
    let column = col(".")
    let first = getline(line("."))[column-2]
    let second = getline(line("."))[column-1]
    if first == "\\"
	if a:left == "["
	    return "\[\<CR>\<CR>\\]\<Up>"
	else
	    return a:left . "\\" . a:right . "\<Left>\<Left>"
	endif
    else
	if a:left =~ '\[\|{'
	\ && strpart(getline(line(".")),col(".")-2,2) == a:left . a:right
	    return "\<Del>"
        else
            return a:left . a:right . "\<Left>"
	endif
    endif
endfunction

" Double ampersands, unless you are in an array environment
function! s:DoubleAmpersands()
    let stop = 0
    let currentline = line(".")
    while stop == 0
	let currentline = currentline - 1
	let thisline = getline(currentline)
	if thisline =~ '\\begin' || currentline == 0
	    let stop = 1
	endif
    endwhile
    if thisline =~ '\\begin{array}'
	return "&"
    elseif strpart(getline(line(".")),col(".")-2,2) == "&&"
	return "\<Del>"
    else
	return "&&\<Left>"
    endif
endfunction

" Embrace the visual region with the symbol.
" This assumes that you are using
" set selection=exclusive
" If you are not, replace these macros with ones like this.
"vnoremap <buffer> ( <C-C>`>a)<C-C>`<i(<Esc>
vnoremap <buffer> <M-l> <C-C>`>a\right<Esc>`<i\left<Esc>
vnoremap <buffer> `( <C-C>`>a)<Esc>`<i(<Esc>
vnoremap <buffer> `[ <C-C>`>a]<Esc>`<i[<Esc>
vnoremap <buffer> `{ <C-C>`>a}<Esc>`<i{<Esc>
vnoremap <buffer> & <C-C>`>a&<Esc>`<i&<Esc>
vnoremap <buffer> `$ <C-C>`>a$<Esc>`<i$<Esc>
vnoremap <buffer> <M-4> <C-C>`>a$<Esc>`<i$<Esc>

" No timeout.  Applies to mappings such as `a for \alpha
set notimeout

" Greek letter, AucTex style bindings
inoremap <buffer> `` ``
inoremap <buffer> `a \alpha
inoremap <buffer> `b \beta
inoremap <buffer> `c \chi
inoremap <buffer> `d \delta
inoremap <buffer> `e \varepsilon
inoremap <buffer> `f \varphi
inoremap <buffer> `g \gamma
inoremap <buffer> `h \eta
inoremap <buffer> `i \int_{}^{}<Esc>3hi
	    " Or \iota or \infty or \in
inoremap <buffer> `k \kappa
inoremap <buffer> `l \lambda
inoremap <buffer> `m \mu
inoremap <buffer> `n \nu
inoremap <buffer> `o \omega
inoremap <buffer> `p \pi
inoremap <buffer> `q \theta
inoremap <buffer> `r \rho
inoremap <buffer> `s \sigma
inoremap <buffer> `t \tau
inoremap <buffer> `u \upsilon
inoremap <buffer> `v \vee
inoremap <buffer> `w \wedge
inoremap <buffer> `x \xi
inoremap <buffer> `y \psi
inoremap <buffer> `z \zeta
inoremap <buffer> `D \Delta
inoremap <buffer> `I \int_{}^{}<Esc>3hi
inoremap <buffer> `F \Phi
inoremap <buffer> `G \Gamma
inoremap <buffer> `L \Lambda
inoremap <buffer> `N \nabla
inoremap <buffer> `O \Omega
inoremap <buffer> `Q \Theta
inoremap <buffer> `R \varrho
inoremap <buffer> `S \sum_{}^{}<Esc>3hi
inoremap <buffer> `U \Upsilon
inoremap <buffer> `X \Xi
inoremap <buffer> `Y \Psi
inoremap <buffer> `0 \emptyset
inoremap <buffer> `1 \left
inoremap <buffer> `2 \right
inoremap <buffer> `3 \Big
inoremap <buffer> `6 \partial
inoremap <buffer> `8 \infty
inoremap <buffer> `/ \frac{}{}<Esc>2hi
inoremap <buffer> `% \frac{}{}<Esc>2hi
inoremap <buffer> `@ \circ
inoremap <buffer> `\| \Big\|
inoremap <buffer> `= \equiv
inoremap <buffer> `\ \setminus
inoremap <buffer> `. \cdot
inoremap <buffer> `* \times
inoremap <buffer> `& \wedge
inoremap <buffer> `- \bigcap
inoremap <buffer> `+ \bigcup
inoremap <buffer> `( \subset
inoremap <buffer> `) \supset
inoremap <buffer> `< \le
inoremap <buffer> `> \ge
inoremap <buffer> `, \nonumber
inoremap <buffer> `: \dots
inoremap <buffer> `~ \tilde{}<Left>
inoremap <buffer> `^ \hat{}<Left>
inoremap <buffer> `; \dot{}<Left>
inoremap <buffer> `_ \bar{}<Left>
inoremap <buffer> `<M-c> \cos
inoremap <buffer> `<C-E> \exp\left(\right)<Esc>6hi
inoremap <buffer> `<C-I> \in
inoremap <buffer> `<C-J> \downarrow
inoremap <buffer> `<C-L> \log
inoremap <buffer> `<C-P> \uparrow
inoremap <buffer> `<Up> \uparrow
inoremap <buffer> `<C-N> \downarrow
inoremap <buffer> `<Down> \downarrow
inoremap <buffer> `<C-F> \to
inoremap <buffer> `<Right> \lim_{}<Left>
inoremap <buffer> `<C-S> \sin
inoremap <buffer> `<C-T> \tan
inoremap <buffer> `<M-l> \ell
inoremap <buffer> `<CR> \nonumber\\<CR><HOME>&&<Left>

"  With this map, <Space> will split up a long line, keeping the dollar
"  signs together (see the next function, TexFormatLine).
inoremap <buffer> <Space> <Space><Esc>:call <SID>TexFill(b:tw)<CR>a
function! s:TexFill(width)
    "let tw = ( &tw ? &tw : &columns - &wm )
    if col(".") > a:width
	exe "normal a##\<Esc>"
	call <SID>TexFormatLine(a:width)
	exe "normal ?##\<CR>2s\<Esc>"
    endif
endfunction

function! s:TexFormatLine(width)
    normal $
    let length = col(".")
    let go = 1
    while length > a:width+2 && go
    let between = 0
    let string = strpart(getline(line(".")),0,a:width)
    " Count the dollar signs
    let evendollars = 1
    let counter = 0
    while counter <= a:width-1
	if string[counter] == '$' && string[counter-1] != '\'  " Skip \$.
	   let evendollars = 1 - evendollars
	endif
	let counter = counter + 1
    endwhile
    " Get ready to split the line.
    exe "normal " . (a:width + 1) . "|"
    if evendollars
    " Then you are not between dollars.
       exe "normal ?\\$\\| \<CR>W"
    else
    " Then you are between dollars.
	normal F$
	if col(".") == 1
	   let go = 0
	endif
    endif
    exe "normal i\<CR>\<Esc>$"
    let length = col(".")
    endwhile
endfunction

noremap <buffer> gq :call <SID>TexFormatLine(b:tw)<CR>
noremap <buffer> Q :call <SID>TexFormatLine(b:tw)<CR>
vnoremap <buffer> Q J:call <SID>TexFormatLine(b:tw)<CR>
noremap <buffer> <C-CR> :call <SID>TexFormatLine(b:tw)<CR>
inoremap <buffer> <C-CR> <Esc>:call <SID>TexFormatLine(b:tw)<CR>
vnoremap <buffer> <C-CR> J:call <SID>TexFormatLine(b:tw)<CR>

" Matching Brackets Macros (due to Saul Lubkin).  For normal mode.

" Bindings for the Bracket Macros
noremap <buffer> <Tab>x :call <SID>DeleteBrackets()<CR>
noremap <buffer> <Tab>l :call <SID>PutLeftRight()<CR>
noremap <buffer> <Tab><Del> :call <SID>DeleteBrackets()<CR>
noremap <buffer> <Tab>( :call <SID>ChangeRound()<CR>
noremap <buffer> <Tab>[ :call <SID>ChangeSquare()<CR>
noremap <buffer> <Tab>{ :call <SID>ChangeCurly()<CR>
noremap <buffer> <Tab>c :call <SID>ChangeLeftRightBigg()<CR>
noremap <buffer> <Tab>b :call <SID>PutBigg()<CR>

noremap <buffer> <C-Del> :call <SID>DeleteBrackets()<CR>
inoremap <buffer> <C-BS> <Left><C-O>:call <SID>DeleteBrackets()<CR>

" Delete matching brackets.
function! s:DeleteBrackets()
    let b = getline(line("."))[col(".") - 2]
    let c = getline(line("."))[col(".") - 1]
    if b == '\' && c =~ '{\|}'
	normal X%X%
    endif
    if c =~ '{\|\[\|('
	normal %x``x
    elseif c =~ '}\|\]\|)'
	normal %%x``x``
    endif
endfunction

" Put \left...\right in front of the matched brackets.
function! s:PutLeftRight()
    let previous = getline(line("."))[col(".") - 2]
    let char = getline(line("."))[col(".") - 1]
    if previous == '\'
    if char == '{'
	exe "normal ileft\\\<Esc>l%iright\\\<Esc>l%"
    elseif char == '}'
	exe "normal iright\\\<Esc>l%ileft\\\<Esc>l%"
    endif
    elseif char =~ '\[\|('
	exe "normal i\\left\<Esc>l%i\\right\<Esc>l%"
    elseif char =~ '\]\|)'
	exe "normal i\\right\<Esc>l%i\\left\<Esc>l%"
    endif
endfunction

" Look at the 'string', and return either 'default' or the string.
function! s:StripSlash(string,default)
    let len = strlen(a:string)
    if a:string[0] == '\'
	return strpart(a:string, 1, len-1)
    elseif len == 0
	return a:default
    else
	return '\' . a:string
    endif
endfunction

" Put \bigg (or whatever the reader chooses) in front of the matched brackets.
function! s:PutBigg()
    let in = input("\\Big, \\bigg, or what? (default: bigg):  ")
    let in = <SID>StripSlash(in,"\\bigg")
    let b = getline(line("."))[col(".") - 2]
    let c = getline(line("."))[col(".") - 1]
    if b == '\'
	exe "normal hi" . in . "\<Esc>l%hi" . in . "\<Esc>l%"
    elseif c =~ '{\|\[\|(\|}\|\]\|)'
	exe "normal i" . in . "\<Esc>l%i" . in . "\<Esc>l%"
    endif
endfunction

" Change \left..\right to \bigg, or whatever the user chooses.
function! s:ChangeLeftRightBigg()
    let in = input("\\Big, \\bigg, or what? (default: nothing):  ")
    let in = <SID>StripSlash(in,"")
    let b = getline(line("."))[col(".") - 2]
    let c = getline(line("."))[col(".") - 1]
    if b == '\'
      if c =~ '{\|}'
	exe "normal 2F\\xcw" . in . "\<Esc>2l%2F\\xcw" . in . "\<Esc>2l%"
      endif
    elseif c =~ '\[\|(\|\]\|)'
      exe "normal F\\xcw" . in . "\<Esc>l%F\\xcw" . in . "\<Esc>l%"
    endif
endfunction

" Change the brackets to curly brackets.
function! s:ChangeCurly()
    let c = getline(line("."))[col(".") - 1]
    if c =~ '\[\|('
	exe "normal i\\\<Esc>l%i\\\<Esc>lr}``r{"
    elseif c =~ '\]\|)'
	exe "normal %i\\\<Esc>l%i\\\<Esc>lr}``r{%"
    endif
endfunction

" Change the brackets to round brackets.
function! s:ChangeRound()
    let b = getline(line("."))[col(".") - 2]
    let c = getline(line("."))[col(".") - 1]
    if b == '\'
    if c == '{'
	normal X%Xr)``r(
    elseif c == '}'
	normal %X%Xr)``r(%
    endif
    elseif c == '['
	normal %r)``r(
    elseif c == ']'
	normal %%r)``r(%
    endif
endfunction

" Change the brackets to square brackets.
function! s:ChangeSquare()
    let b = getline(line("."))[col(".") - 2]
    let c = getline(line("."))[col(".") - 1]
    if b == '\'
	if c == '{'
	    normal X%Xr]``r[
	elseif c == '}'
	    normal %X%Xr]``r[%
	endif
    elseif c == '('
	normal %r]``r[
    elseif c == ')'
	normal %%r]``r[%
    endif
endfunction

" Saul's menu items
nnoremenu 40.401 Brackets.delete\ brackets\ \ \ Ctrl+Del,Ctrl+BS,Tab-x :call <SID>DeleteBrackets()<CR>
inoremenu 40.402 Brackets.delete\ brackets\ \ \ Ctrl+Del,Ctrl+BS,Tab-x <Left><C-O>:call <SID>DeleteBrackets()<CR> nnoremenu 40.403 Brackets.change\ to\ () :call <SID>ChangeRound()<CR>
nnoremenu 40.404 Brackets.change\ to\ () :call <SID>ChangeRound()<CR>
inoremenu 40.404 Brackets.change\ to\ () <Esc>:call <SID>ChangeRound()<CR>a
nnoremenu 40.405 Brackets.change\ to\ [] :call <SID>ChangeSquare()<CR>
inoremenu 40.406 Brackets.change\ to\ [] <Esc>:call <SID>ChangeSquare()<CR>a
nnoremenu 40.407 Brackets.change\ to\ \\{\\} :call <SID>ChangeCurly()<CR>
inoremenu 40.408 Brackets.change\ to\ \\{\\} <Esc>:call <SID>ChangeCurly()<CR>a
nnoremenu 40.409 Brackets.insert\ \\left,\\right :call <SID>PutLeftRight()<CR>
inoremenu 40.410 Brackets.insert\ \\left,\\right <Esc>:call <SID>PutLeftRight()<CR>a
nnoremenu 40.410 Brackets.insert\ (default\ \\bigg) :call <SID>PutBigg()<CR>
inoremenu 40.411 Brackets.insert\ (default\ \\bigg) <Esc>:call <SID>PutBigg()<CR>a
nnoremenu 40.412 Brackets.change\ \\left,\\right,\\big,\ etc\ to\ (default\ \\nothing) :call <SID>ChangeLeftRightBigg()<CR>
inoremenu 40.413 Brackets.change\ \\left,\\right\,\\big,\ etc\ to\ (default\ \\nothing) <Esc>:call <SID>ChangeLeftRightBigg()<CR>a

" Menus for running Latex, etc.
nnoremenu 50.401 Latex.run\ latex\ \ \ \ Control-Tab :w<CR>:! xterm -bg ivory -fn 7x14 -e latex % &<CR><Space>
inoremenu 50.401 Latex.run\ latex\ \ \ \ Control-Tab <Esc>:w<CR>:! xterm -bg ivory -fn 7x14 -e latex % &<CR><Space>
nnoremenu 50.402 Latex.next\ error\ \ \ Shift-Tab :call <SID>NextTexError()<CR><Space>
inoremenu 50.402 Latex.next\ error\ \ \ Shift-Tab <Esc>:call <SID>NextTexError()<CR><Space>
nnoremenu 50.403 Latex.run\ xdvi\ \ \ \ \ Alt-Tab :call <SID>Xdvi()<CR><Space>
inoremenu 50.403 Latex.run\ xdvi\ \ \ \ \ Alt-Tab <Esc>:call <SID>Xdvi()<CR><Space>
nnoremenu 50.404 Latex.run\ ispell\ \ \ Shift-Ins :w<CR>:! xterm -bg ivory -fn 10x20 -e ispell %<CR><Space>:e %<CR><Space>
inoremenu 50.404 Latex.run\ ispell\ \ \ Shift-Ins <Esc>:w<CR>:! xterm -bg ivory -fn 10x20 -e ispell %<CR><Space>:e %<CR><Space>
"nnoremenu 50.405 Latex.run\ engspchk :so .Vim/engspchk.vim<CR>
"inoremenu 50.405 Latex.run\ engspchk <C-O>:so .Vim/engspchk.vim<CR>

" Math Abbreviations
iab <buffer> \b \bigskip
iab <buffer> \h \hspace{
iab <buffer> \i \noindent
iab <buffer> \l \newline
iab <buffer> \m \medskip
iab <buffer> \n \nonumber\\
iab <buffer> \p \newpage
iab <buffer> \q \qquad
iab <buffer> \v \vfill

" Personal or Temporary bindings.
inoremap <buffer> ;; ;
inoremap <buffer> ;i \int_{\mathbf{R}^d}
inoremap <buffer> ;I \int_{0}^{\infty}
