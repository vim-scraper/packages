" Vim syntax file
" Language:	Nested Cascading Style Sheets (CSS DRYer)
" Maintainer:	Niklas Hofer <niklas+vim@lanpartei.de>
" URL:		http://lanpartei.de/~niklas/vim/syntax/ncss.vim
" Last Change:	2008 Feb 10
" Dependency: syntax/css.vim by Claudio Fleiner <claudio@fleiner.com>

setlocal syntax=css

" simply append cssDefinition to itself to allow nesting
redir => s:olddef
   silent! syn list cssDefinition
redir END
if s:olddef != ''
   let s:b = strridx(s:olddef,'matchgroup')
   if s:b != -1
      exe 'syn region cssDefinition '.strpart(s:olddef,s:b).',cssDefinition'
   endif
endif

