" Vim syntax file
" Language:	epperl
" Maintainer:	Ish-Lev Avshalom <avshalom-vim@ishlev.com>
" Version: 1.0
" Last Change:	2003 May 22
" Derived From: ePmbperl by Lukas Zapletal
"
" USE:
" 
" 	let perl_fold=1		" note enabling this will slow down synchronizing
"		augroup filetypedetect
"		autocmd! BufNewFile,BufRead *.ep,*.nep setf epperl
"		augroup END
"
"		" if you want yellow code like in Interdev:
"		autocmd BufNewFile,BufRead *.ep,*.nep colorscheme epperl_yellow
"
"	CHANGELOG:
"	v1.0 - initial release
"

syn case match
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'epperl'
endif

if version < 600
	source <sfile>:p:h/html.vim
	syn case match
	syn include @EPPerl <sfile>:p:h/perl.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
	syn case match
  syn include @EPPerl syntax/perl.vim
endif

" For some reason, perlFunctionName matched any string that had no other match, so I had to redefine it.
syn clear perlFunctionName
if exists("perl_want_scope_in_variables")
  syn match  perlFunctionName   "\\\=&\$*\(\I\i*\)\=\(\(::\|'\)\I\i*\)*\>" contains=perlPackageRef nextgroup=perlVarMember,perlVarSimpleMember
else
  syn match  perlFunctionName   "\\\=&\$*\(\I\i*\)\=\(\(::\|'\)\I\i*\)*\>" nextgroup=perlVarMember,perlVarSimpleMember
endif



syn cluster htmlPreproc add=EPInsideHtml,EPVarInHTML

syn case ignore


" these is the ep-perl region, which simply contain perl expressions
syn region EPInsideHtml matchgroup=EPTags start="<EP-PERL>" end="</EP-PERL>" contains=@EPPerl

" can't make it to contain EPPerl without complex matching becuase of the $" problem, so I left it for now.
syn region EPInsideHtml matchgroup=EPTags start=+<EP-IF eval="+ end=+">+

" a block with one big comment
syn region EPComment matchgroup=EPTags start="<EP-COMMENT>" end="</EP-COMMENT>"

" Perl Vars inside HTML
syn match EPVarInHTML	"\$[#@]*[a-zA-z]\+\(\(->\$*\)*[a-zA-Z0-9_]\)*\$"

syn case match

hi link EPComment Comment
highlight EPTags         ctermfg=darkcyan
hi link EPVarInHTML EPTags




" syncing
syntax sync minlines=40
syntax sync maxlines=100

let b:current_syntax = "epperl"

if main_syntax == 'epperl'
  unlet main_syntax
endif

" vim: set tabstop=2 shiftwidth=2 noexpandtab:
