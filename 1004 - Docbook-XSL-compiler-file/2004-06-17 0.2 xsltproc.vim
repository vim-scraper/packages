" Vim compiler file
" Compiler: xsltproc
" Maintainer: Diego Pino <diego_pino_garcia@yahoo.es>
" URL:
" Version: 0.2 
" Last Change: 2004 Jun 17 

" V 0.1
" Initial release
"
" V 0.2
" Some bug fixes. Now make command accepts parameters. Useful if you want to
" compile with -xinclude option.
"
" This script assumes you have xsltproc (cygwin) already installed

" If you get an E40 error when you run make command, try to append these line
" to your ~/.vimrc file or mswin.vim file
"
" set gp=grep\ -nH 
" set sp=2>&1\|\ tee 

if exists("current_compiler")
	finish
endif
let current_compiler="xsltproc"

let s:cpo_save=&cpo
set cpo-=C

" See http://prdownloads.sourceforge.net/docbook/docbook-xsl-1.60.1.zip?download
" Set your xsl file. Catalog alias is also allow. 
let xslfile="C:/docbook/htmlhelp/htmlhelp.xsl" 
exe "setlocal makeprg=xsltproc.exe\\ --nonet\\ $*\\ " . xslfile . "\\ %"

" Simple error format
setlocal errorformat=%A%f:%l:%m,
	\%-Z%p^,
	\%-G%.%#

" Set makeef
autocmd FileType xml exe "setlocal makeef=" . expand("%:p:r") . ".err" 

set commentstring=<!--%s-->

let &cpo = s:cpo_save
unlet s:cpo_save
