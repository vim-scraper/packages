" Vim indent file
" Language:     LaTeX
" Maintainer:   Johannes Tanzler <jtanzler@yline.com>
" Created:      Sat, 16 Feb 2002 16:50:19 +0100
" Last Change:	Sun, 17 Feb 2002 00:09:11 +0100
" Version: 0.01
" URL: comming soon: http://www.unet.univie.ac.at/~a9925098/vim/indent/tex.vim

" --> If you're a Vim guru & and you find something that could be done in a
"     better (perhaps in a more Vim-ish or Vi-ish) way, please let me know! 

" Options: {{{
"
" To set the following options (ok, currently it's just one), add a line like
"   let g:tex_indent_items = 1
" to your ~/.vimrc.
"
" * g:tex_indent_items
"
"   If this variable is set, item-environments are indented like Emacs does
"   it, i.e., continuation lines are indented with a shiftwidth.
"   
"   NOTE: I've already set the variable below; delete the corresponding line
"   if you don't like this behaviour.
"
"   Per default, it is unset.
"   
"              set                                unset
"   ----------------------------------------------------------------
"       \begin{itemize}                      \begin{itemize}  
"         \item blablabla                      \item blablabla
"           bla bla bla                        bla bla bla  
"         \item blablabla                      \item blablabla
"           bla bla bla                        bla bla bla  
"       \end{itemize}                        \end{itemize}    
"
"
"   This option applies to itemize, description, enumerate, and
"   thebibliography.
"
" }}} 

" Delete the next line to avoid the special indention of items
let g:tex_indent_items = 1

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1


setlocal indentexpr=GetTeXIndent()
setlocal nolisp
setlocal nosmartindent
setlocal autoindent
setlocal indentkeys+==\\end,=\\item,=\\bibitem


" Only define the function once
if exists("*GetTeXIndent")
  finish
endif



function GetTeXIndent()

  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " At the start of the file use zero indent.
  if lnum == 0
    return 0
  endif

  let ind = indent(lnum)
  let line = getline(lnum)             " last line
  let cline = getline(v:lnum)          " current line


  " Add a 'shiftwidth' after beginning of environments.
  " Don't add it for \begin{document} and \begin{verbatim}
  if line =~ '^\s*\\begin{\(.*\)}'  && line !~ 'verbatim' 
        \&& line !~ 'document'

    let ind = ind + &sw

    if exists("g:tex_indent_items")
      " Add another sw for item-environments
      if line =~ 'itemize' || line =~ 'description' || line =~ 'enumerate'
            \|| line =~ 'thebibliography'
        let ind = ind + &sw
      endif
    endif
  endif

  
  " Subtract a 'shiftwidth' when an environment ends
  if cline =~ '^\s*\\end' && cline !~ 'verbatim' 
        \&& cline !~ 'document'

    if exists("g:tex_indent_items")
      " Remove another sw for item-environments
      if cline =~ 'itemize' || cline =~ 'description' || cline =~ 'enumerate'
            \|| cline =~ 'thebibliography'
        let ind = ind - &sw
      endif
    endif

    let ind = ind - &sw
  endif

  
  " Special treatment for 'item'
  " ----------------------------
  
  if exists("g:tex_indent_items")

    " '\item' or '\bibitem' itself:
    if cline =~ '^\s*\\item' || cline =~ '^\s*\\bibitem'
      let ind = ind - &sw
    endif

    " lines following to '\item' are intented once again:
    if line =~ '^\s*\\item' || line =~ '^\s*\\bibitem'
      let ind = ind + &sw
    endif

  endif

  return ind
endfunction

