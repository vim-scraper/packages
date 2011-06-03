" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftdetect/epl.vim	[[[1
2
au BufRead,BufNewFile *.html.epl set filetype=html.epl
au BufRead,BufNewFile *.html.ep  set filetype=html.epl
syntax/epl.vim	[[[1
52
" html w/ Perl as a preprocessor
" Language:    Perl + html
" Maintainer:  yko <ykorshak@gmail.com>
" version:     0.03
" Last Change: 2010 Dec 26
" Location:    http://github.com/yko/mojo.vim
" Original version: vti <vti@cpan.org>

if exists("perl_fold")
   let b:bfold = perl_fold
   unlet perl_fold
endif

" Clear previous syntax name
unlet! b:current_syntax

" Include Perl syntax intp @Perl cluster
syntax include @Perl syntax/perl.vim

" This groups are broken when included
syn cluster Perl remove=perlFunctionName,perlElseIfError

if exists("b:bfold")
    perl_fold = b:bfold
    unlet b:bfold
endif

" Begin and end of code blocks
syn match MojoStart /<%=\{0,2}/ contained
syn match MojoStart /^\s*%=\{0,2}/  contained
syn match MojoEnd /=\{0,1}%>/ contained

syn cluster Mojo contains=MojoStart,MojoEnd

" Highlight code blocks
syn region PerlInside keepend oneline start=+<%=\{0,2}+hs=s skip=+".*%>.*"+ end=+=\{0,1}%>+ contains=@Mojo,@Perl
syn region PerlInside keepend oneline start=+^\s*%=\{0,2}+hs=s end=+$+ contains=@Mojo,@Perl

" Display code blocks in tag parameters' quoted value like 
" <a href="<%= url_for 'foo' %>'>
syn cluster htmlPreproc add=PerlInside

command -nargs=+ HiLink hi def link <args>

HiLink MojoStart perlType
HiLink MojoEnd perlType
HiLink MojoFileName perlString
HiLink MojoFileNameStart perlSpecial

delcommand HiLink

let b:current_syntax = "html.epl"
after/syntax/perl/MojoliciousLite.vim	[[[1
49
" Vim syntax file (for including only)
" html w/ Perl as a preprocessor in __DATA__
" Language:    Mojo epl templates stored in Perl __DATA__
" Maintainer:  yko <ykorshak@gmail.com>
" Version:     0.03
" Last Change: 2010 Dec 26
" Location:    http://github.com/yko/mojo.vim
"
" Thanks to Viacheslav Tykhanovskyi for simplified region syntax
"
" Possible configuration:
"
"  let mojo_highlight_data = 1
"  let mojo_disable_html = 1
"
" For highlight templates in __DATA__ add following line to your .vimrc:
" let mojo_highlight_data = 1

if !exists("mojo_highlight_data") 
    finish
endif

if !exists("b:current_syntax")
  echoerr "MojolisiousTemplate can only be included in existing syntax"
  finish
endif

" Store current syntax name
let cs = b:current_syntax
unlet b:current_syntax

syntax include @Epl syntax/epl.vim

if !exists("mojo_disable_html")
  unlet! b:current_syntax
  syn include @Html syntax/html.vim
endif

" Set up hl of filename headers
syn match MojoFileNameStart "@@" contained

syn region MojoFileContainer start=/@@/ end=/@@/me=s-1 contains=@Epl,@Html,MojoFileName contained keepend fold
syn region MojoFileName start=/@@/ end="$" keepend contains=MojoFileNameStart contained keepend

" Push Template sections and HTML syntax into @perlDATA cluster
syn cluster perlDATA add=@Html,MojoFileContainer

" Revert current syntax name
let b:current_syntax = cs
