" Vim compiler file
" Compiler: xsltproc
" Maintainer: Diego Pino <diego_pino_garcia@yahoo.es>
" URL:
" Last Change: 2004 Jun 05

" This script assumes you have xsltproc (cygwin) already installed

" If you get an E40 error when you run make command, try to append these line
" to your ~/.vimrc file or mswin.vim file
"
" autocmd FileType * execute "set makeef=" . expand("%:p:r") . ".err"
" set gp=grep\ -nH 
" set sp=2>&1\|\ tee 

if exists("current_compiler")
	finish
endif
let current_compiler="xsltproc"

let s:cpo_save=&cpo
set cpo-=C

" C:/docbook/ is the path to Docbook xsl stylesheet path
" See http://prdownloads.sourceforge.net/docbook/docbook-xsl-1.60.1.zip?download
setlocal makeprg=xsltproc.exe\ --nonet\ C:/docbook/htmlhelp/htmlhelp.xsl\ %

" Simple error format
setlocal errorformat=%A%f:%l:%m,
	\%-Z%p^,
	\%-G%.%#

set commentstring=<!--%s-->

let &cpo = s:cpo_save
unlet s:cpo_save
